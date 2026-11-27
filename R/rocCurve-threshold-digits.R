#
# Copyright (C) 2026 onetrickpony1
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Threshold digits ----
# JASP shows a number cell with the format "sf:d" in one of two ways (jasp-desktop Desktop/html/js/utils.js,
# formatColumn, upstream development at commit 4e7f434, lines 347-357; JASP 0.99.0, commit 1204d18, has the same
# lines):
# - when |x| >= 10^d or |x| >= 1e6 (utils.js line 250, fixed): Number.toExponential(d - 1). This rounds the
#   exact value of the double. A value exactly halfway goes away from zero (ECMAScript picks the larger n).
#   1234.55, the double 1234.549999999999954..., shows as 1.2345e+3 with d = 5.
# - otherwise, and for 0 (lines 343-345): formatPrecision (lines 52-58), Intl.NumberFormat with d significant
#   digits. ICU rounds the shortest decimal that reads back as the same double, halfway away from zero
#   (rounding mode "halfExpand"). 1234.55 shows as 1234.6 with d = 5, and 1.005 as 1.01 with d = 3.
# The functions below mirror both with decimal digits instead of doubles, so the check matches what users see.
# R's signif() differs: it gives 0.12 for 0.125 with d = 2, where JASP shows 0.13.
# With a fixed number of decimals set in JASP's preferences (dp, line 166; fixDecimals, line 170), JASP shows
# every number with those decimals instead (lines 349 and 357), and this rule does not apply.

# A decimal below is a list of: sign; exp, the exponent of the first significant digit; head, the first 15
# significant digits as a whole number; following, the 16th digit; and tail, whether any digit after the 15th
# is non-zero. Every field is a number, so a decimal costs little memory.

# The decimal fields from sprintf("%.Ne") strings with N >= 15. The first 16 characters ("d.ddd...") hold 15
# digits, read as a number below 10 with an error far below 0.5e-14, so rounding head is exact.
.rocParseDecimal <- function(s, sign) {
  rest <- sub("e.*", "", substring(s, 18))
  list(sign = sign, exp = as.integer(sub(".*e", "", s)), head = round(as.numeric(substr(s, 1, 16)) * 1e14),
       following = as.integer(substr(s, 17, 17)), tail = substr(s, 17, 17) != "0" | grepl("[1-9]", rest))
}

# The exact decimal value of finite doubles.
# sprintf rounds correctly (glibc prints the exact value of a double), so the 40 digits of sprintf("%.39e") are
# the exact digits, except that the 40th is rounded. A carry from that rounding reaches the first 16 digits only
# when digits 17 to 40 come out all zero. In that case the value either has at most 40 significant digits
# (.rocAtMost40Digits), which sprintf prints exactly, or it is printed again with all its digits (a double has
# at most 767 significant decimal digits, so 800 hold them all). Either way the fields are exact.
.rocDecimal <- function(x) {
  a <- abs(x)
  s <- sprintf("%.39e", a)
  check <- which(a != 0 & substr(s, 18, 41) == strrep("0", 24))
  unsure <- check[!.rocAtMost40Digits(a[check], as.integer(substring(s[check], 43)))]
  out <- .rocParseDecimal(s, sign(x))
  if (length(unsure) > 0) {
    full <- .rocParseDecimal(sprintf("%.799e", a[unsure]), sign(x[unsure]))
    for (field in names(out))
      out[[field]][unsure] <- full[[field]]
  }
  out
}

# TRUE where the exact decimal value of a (positive and finite) has at most 40 significant digits for sure.
# `exp` is the exponent sprintf gave: floor(log10(a)), or one more when sprintf rounded up to a power of 10.
# A whole number below 1e40 has at most 40 digits (the double 1e40 lies above 10^40). Any other number is m / 2^k,
# with m odd and k > 0 fractional bits. Then a * 10^k = m * 5^k is a whole number that is not a multiple of 10,
# and its digits are the significant digits of a: floor(log10(a)) + k + 1 of them, never more than exp + k + 1.
# Doubling a is exact, and k doublings make it a whole number.
.rocAtMost40Digits <- function(a, exp) {
  whole <- a == floor(a)
  y <- a
  k <- numeric(length(a))
  frac <- !whole
  while (any(frac)) {
    y[frac] <- 2 * y[frac]
    k[frac] <- k[frac] + 1
    frac <- y != floor(y)
  }
  ifelse(whole, a < 1e40, exp + k + 1 <= 40)
}

# The shortest decimal that reads back as the same double (at most 17 significant digits), as ICU uses it.
# The first number of digits p whose sprintf output reads back gives the shortest length. R's as.numeric is not
# always correctly rounded (as.numeric("9.347302") != JavaScript's), so rarely (13 in 302,730 values) this
# differs from ICU by one digit.
# For a normal double (2^-1022 or more) at most three lengths need a try. When the 15-digit decimal reads back,
# the shortest one has at most 15 digits and, padded with zeros, equals it: it lies within half a unit in the
# last place of the double (at most 1.2e-16 of its size), so closer than half the spacing of 15-digit decimals
# (at least 5e-16 of the size), and no other 15-digit decimal is that close. When it does not read back, no
# shorter decimal does either (by the same argument), so the shortest has 16 or 17 digits. A subnormal double
# has fewer bits, so every length from 1 to 17 is tried in turn.
.rocShortestDecimal <- function(x) {
  a <- abs(x)
  s <- sprintf("%.14e", a)
  for (p in 16:17) {
    redo <- which(as.numeric(s) != a)
    if (length(redo) == 0)
      break
    s[redo] <- sprintf("%.*e", p - 1L, a[redo])
  }
  subnormal <- which(a != 0 & a < 2^-1022)
  if (length(subnormal) > 0) {
    found <- rep(FALSE, length(subnormal))
    for (p in 1:17) {
      candidate <- sprintf("%.*e", p - 1L, a[subnormal])
      new <- !found & as.numeric(candidate) == a[subnormal]
      s[subnormal[new]] <- candidate[new]
      found <- found | new
    }
  }
  # Pad the digits with zeros to 17 ("d." and 16 more), so that .rocParseDecimal finds the 16th digit.
  short <- which(regexpr("e", s, fixed = TRUE) < 19)
  if (length(short) > 0) {
    mantissa <- sub("e.*", "", s[short])
    mantissa <- ifelse(grepl(".", mantissa, fixed = TRUE), mantissa, paste0(mantissa, "."))
    s[short] <- paste0(substr(paste0(mantissa, strrep("0", 16)), 1, 18), sub("^[^e]*", "", s[short]))
  }
  .rocParseDecimal(s, sign(x))
}

# Some elements of a decimal.
.rocDecimalAt <- function(x, i) {
  lapply(x, `[`, i)
}

# The value shown for x (a decimal) with d significant digits (4 to 15): round the decimal, halfway away from
# zero. The first d digits, plus one when the next digit is 5 or more; a carry to 10^d adds a digit. Every number
# here is a whole number below 2^53, so the arithmetic is exact.
.rocRoundDecimal <- function(x, d) {
  if (d < 15) {
    scale <- 10^(15 - d)
    head  <- x$head %/% scale
    nextDigit <- (x$head %/% (scale / 10)) %% 10
  } else {
    head <- x$head
    nextDigit <- x$following
  }
  head  <- head + (nextDigit >= 5)
  carry <- head >= 10^d
  head[carry] <- head[carry] / 10
  list(sign = x$sign, exp = x$exp + carry, head = head * 10^(15 - d), tail = rep(FALSE, length(head)))
}

# Compares two decimals exactly: -1, 0 or 1 for x < y, x = y, x > y.
.rocCompareDecimal <- function(x, y) {
  magnitude <- ifelse(x$exp != y$exp, sign(x$exp - y$exp),
                      ifelse(x$head != y$head, sign(x$head - y$head), as.integer(x$tail) - as.integer(y$tail)))
  ifelse(x$sign != y$sign, sign(x$sign - y$sign), ifelse(x$sign == 0, 0, x$sign * magnitude))
}

# The smallest number of significant digits, from 4 (JASP's default for numbers) to 15, with which every shown
# threshold, after JASP's rounding, lies strictly between the two observed values it separates and differs from
# the other shown thresholds. Then the threshold shown classifies every observed value as the threshold itself
# does. A threshold below 10^d and below 1e6 goes through Intl.NumberFormat; any other goes through
# toExponential. A threshold equal to an observed value (pROC uses one when two values are too close for a
# midpoint) must be shown exactly. Returns list(digits, exact): exact is FALSE when even 15 digits cannot do it
# (15 is used then).
# The digits are tried from low to high, and the first that passes is used. For each, the thresholds with the
# least room to their neighbours (relative to their size) are checked first: a number of digits that fails
# usually fails there, and then the other thresholds need no check.
.rocThresholdDigits <- function(thresholds, values, from = 4L, to = 15L) {
  t <- sort(unique(thresholds[is.finite(thresholds)]))
  n <- length(t)
  if (n == 0)
    return(list(digits = from, exact = TRUE))
  v <- sort(unique(values[is.finite(values)]))
  below <- findInterval(t, v, left.open = TRUE)  # the number of observed values below t
  atOrBelow <- findInterval(t, v)                # the number at or below t
  onValue <- atOrBelow > below
  hasLower <- below > 0
  hasUpper <- atOrBelow < length(v)
  # Only the observed values next to a threshold are converted, each once: for the few thresholds of a cut-off
  # that is a few values, not every observed value. .rocDecimal works value by value, so a subset gives the
  # same fields. A threshold without a lower (or upper) value is compared with an arbitrary one, and the
  # result is not used.
  if (length(v) == 0)
    v <- 0
  lowerAt <- pmax(below, 1)
  upperAt <- pmin(atOrBelow + 1, length(v))
  needed <- unique(c(lowerAt, upperAt))
  observed <- .rocDecimal(v[needed])
  lower <- .rocDecimalAt(observed, match(lowerAt, needed))
  upper <- .rocDecimalAt(observed, match(upperAt, needed))
  exact <- .rocDecimal(t)
  shortest <- .rocShortestDecimal(t)

  shownAt <- function(i, digits) {
    viaIntl <- abs(t[i]) < 10^digits & abs(t[i]) < 1e6
    shown <- .rocRoundDecimal(.rocDecimalAt(exact, i), digits)
    if (any(viaIntl)) {
      intl <- .rocRoundDecimal(.rocDecimalAt(shortest, i[viaIntl]), digits)
      for (field in names(shown))
        shown[[field]][viaIntl] <- intl[[field]]
    }
    shown
  }
  passes <- function(i, shown) {
    between <- (!hasLower[i] | .rocCompareDecimal(shown, .rocDecimalAt(lower, i)) > 0) &
               (!hasUpper[i] | .rocCompareDecimal(shown, .rocDecimalAt(upper, i)) < 0)
    all(ifelse(onValue[i], .rocCompareDecimal(shown, .rocDecimalAt(exact, i)) == 0, between))
  }

  # The room is only a guide to the order of the checks, so its rounding does not matter.
  room <- pmin(ifelse(hasLower, t - v[lowerAt], Inf), ifelse(hasUpper, v[upperAt] - t, Inf)) / abs(t)
  room[onValue] <- -Inf
  hardest <- order(room)[seq_len(min(n, 1000))]
  everything <- seq_len(n)
  for (digits in from:to) {
    if (!passes(hardest, shownAt(hardest, digits)))
      next
    shown <- shownAt(everything, digits)
    if (!passes(everything, shown))
      next
    # Both roundings are monotone in t, and the change from one to the other happens at a power of 10, which
    # both show exactly. So the shown values are sorted, and any equal ones are neighbours.
    if (n == 1 || all(.rocCompareDecimal(.rocDecimalAt(shown, -1), .rocDecimalAt(shown, -n)) != 0))
      return(list(digits = digits, exact = TRUE))
  }
  list(digits = to, exact = FALSE)
}

# The smallest number of significant digits, from 4 to 15, that shows every digit of a value you type (15
# significant digits at most). JASP rounds a value of at most 15 digits back to those digits either way: under
# toExponential (1e6 or more) and under Intl.NumberFormat.
.rocTypedDigits <- function(x, from = 4L, to = 15L) {
  if (!is.finite(x) || x == 0)
    return(from)
  mantissa <- sub("e.*", "", sprintf("%.15g", abs(x)))
  digits <- sub("0+$", "", sub("^0+", "", gsub(".", "", mantissa, fixed = TRUE)))
  as.integer(max(from, min(to, nchar(digits))))
}

# A threshold cell with the given number of significant digits (jaspBase::createMixedRow).
.rocThresholdCell <- function(threshold, digits) {
  jaspBase::createMixedRow(threshold, "number", list(sprintf("sf:%d", digits)))
}

# The cells of .rocThresholdCell for many thresholds with the same digits. jaspBase::createMixedRow checks the
# type and the format, which are the same for every cell, so one cell is made with it and the others copy that
# cell with their own value.
.rocThresholdCells <- function(thresholds, digits) {
  template <- .rocThresholdCell(0, digits)
  attrs <- attributes(template)
  data  <- unclass(template)[[1]]
  lapply(thresholds, function(threshold) {
    data[["value"]] <- threshold
    cell <- list(data)
    attributes(cell) <- attrs
    cell
  })
}

.rocRoundedThresholdsText <- function(marker) {
  gettextf("%s: the thresholds are rounded for display. Even 15 significant digits cannot show each of them between the two observed values it separates.",
           decodeColNames(marker))
}
