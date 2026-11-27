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

# Fits ----
# Fit one marker on the given rows. Returns a list with the fit or a problem message.
.rocFitMarker <- function(x, y, positive, negative, direction, markerLabel) {
  ok       <- !is.na(x) & !is.na(y)
  cases    <- x[ok & y == positive]
  controls <- x[ok & y == negative]
  out <- list(nPositive = length(cases), nNegative = length(controls), nRemoved = sum(!ok),
              cases = cases, controls = controls, problem = NULL, fit = NULL)

  if (length(cases) == 0 || length(controls) == 0) {
    out$problem <- gettextf(
      "No ROC curve for %s: it needs at least one complete case in each class.", markerLabel)
    return(out)
  }
  if (length(unique(c(cases, controls))) == 1) {
    out$problem <- gettextf("No ROC curve for %s: the marker is constant.", markerLabel)
    return(out)
  }

  response  <- c(rep(negative, length(controls)), rep(positive, length(cases)))
  predictor <- c(controls, cases)
  fit <- try(pROC::roc(response = response, predictor = predictor, levels = c(negative, positive),
                       direction = direction, quiet = TRUE), silent = TRUE)
  # The checks above catch every case pROC is known to refuse. Any other failure gets the module's own
  # message, so the footnote is translated and never shows pROC's English text.
  if (isTryError(fit)) {
    out$problem <- gettextf("No ROC curve for %s: the curve could not be computed.", markerLabel)
    return(out)
  }

  out$fit <- fit
  out$auc <- as.numeric(fit$auc)

  # DeLong SE needs at least two cases in each class. pROC's var(), ci.auc() and roc.test() each compute the
  # DeLong placement values again, so they are computed once here and the three formulas are applied to them
  # (.rocDelongVar, .rocDelongCi, .rocPairedDelong). The AUC table explains a zero SE in its own footnote.
  # With two or more in each class, a missing SE means that pROC's check of the placement values failed (or the
  # variance was not a finite number); seFailed then gives the AUC table its own footnote.
  out$se <- NA_real_
  out$seFailed <- FALSE
  if (length(cases) >= 2 && length(controls) >= 2) {
    placements <- try(.rocDelongPlacements(fit), silent = TRUE)
    if (!isTryError(placements)) {
      out$placements <- placements
      v <- .rocDelongVar(placements, length(cases), length(controls))
      if (is.finite(v) && v >= 0)
        out$se <- sqrt(v)
    }
    out$seFailed <- is.na(out$se)
  }
  return(out)
}

# DeLong ----
# pROC 1.19.0.1 computes the DeLong placement values in C++ (delongPlacements in R/delong.R, which also checks
# that their mean equals the AUC). The formulas below are pROC's own, term by term and in the same order, so
# the results are the same numbers as pROC's var(), ci.auc() and roc.test().
# delongPlacements is internal to pROC, so this relies on pROC 1.19.0.1: it is pinned in renv.lock, and
# dev/validation/reference.R compares the module with pROC's var(), ci.auc() and roc.test() on every run.
.rocDelongPlacements <- function(fit) {
  utils::getFromNamespace("delongPlacements", "pROC")(fit)
}

# pROC var.roc (R/var.R), method "delong": m cases, n controls.
.rocDelongVar <- function(V, m, n) {
  stats::var(V$Y) / n + stats::var(V$X) / m
}

# pROC ci_auc_delong (R/delong.R, line 63): lower, median and upper, cut to [0, 1].
.rocDelongCi <- function(V, m, n, confLevel) {
  if (m <= 1 || n <= 1)
    return(rep(NA, 3))
  SX <- sum((V$X - V$theta) * (V$X - V$theta)) / (m - 1)
  SY <- sum((V$Y - V$theta) * (V$Y - V$theta)) / (n - 1)
  S <- SX / m + SY / n
  ci <- stats::qnorm(c(0 + (1 - confLevel) / 2, .5, 1 - (1 - confLevel) / 2), mean = V$theta, sd = sqrt(S))
  ci[ci > 1] <- 1
  ci[ci < 0] <- 0
  ci
}

# pROC roc.test(method = "delong", paired = TRUE, alternative = "two.sided") for two fits on the same rows:
# delong.paired.calculations, delong.paired.test and ci_delong_paired (R/delong.R), and the p value of
# roc.test.roc (R/roc.test.R). Returns the AUC difference, the CI of the difference, z and p.
.rocPairedDelong <- function(a, b, confLevel) {
  n  <- a$nNegative
  m  <- a$nPositive
  VR <- a$placements
  VS <- b$placements

  SX <- matrix(NA, ncol = 2, nrow = 2)
  SX[1, 1] <- sum((VR$X - VR$theta) * (VR$X - VR$theta)) / (m - 1)
  SX[1, 2] <- sum((VR$X - VR$theta) * (VS$X - VS$theta)) / (m - 1)
  SX[2, 1] <- sum((VS$X - VS$theta) * (VR$X - VR$theta)) / (m - 1)
  SX[2, 2] <- sum((VS$X - VS$theta) * (VS$X - VS$theta)) / (m - 1)

  SY <- matrix(NA, ncol = 2, nrow = 2)
  SY[1, 1] <- sum((VR$Y - VR$theta) * (VR$Y - VR$theta)) / (n - 1)
  SY[1, 2] <- sum((VR$Y - VR$theta) * (VS$Y - VS$theta)) / (n - 1)
  SY[2, 1] <- sum((VS$Y - VS$theta) * (VR$Y - VR$theta)) / (n - 1)
  SY[2, 2] <- sum((VS$Y - VS$theta) * (VS$Y - VS$theta)) / (n - 1)

  S <- SX / m + SY / n
  L <- c(1, -1)
  # Rounding can make the variance slightly negative. pROC's sqrt() then gives NaN with a warning; NaN is kept
  # (the table shows empty z, p and CI with a footnote), without the warning.
  sigSq <- (L %*% S %*% L)[[1]]
  sig <- if (!is.na(sigSq) && sigSq < 0) NaN else sqrt(sigSq)
  d <- VR$theta - VS$theta

  z <- d / sig
  # No difference and no spread gives NaN; pROC sets z to 0 then.
  if (is.nan(z) && d == 0 && isTRUE(sig == 0))
    z <- 0
  critZ <- stats::qnorm(1 - ((1 - confLevel) / 2))
  list(difference = a$auc - b$auc, lower = d - critZ * sig, upper = d + critZ * sig, z = z,
       p = 2 * stats::pnorm(-abs(z)))
}


# Counts and cut-offs ----
# Accuracy statistics from counts. Undefined values are NA.
.rocStatsFromCounts <- function(tp, fn, tn, fp, prevalence, ciLevel) {
  nPos <- tp + fn
  nNeg <- tn + fp
  sens <- tp / nPos
  spec <- tn / nNeg
  p    <- if (is.null(prevalence)) nPos / (nPos + nNeg) else prevalence

  sensCi <- stats::binom.test(tp, nPos, conf.level = ciLevel)$conf.int
  specCi <- stats::binom.test(tn, nNeg, conf.level = ciLevel)$conf.int

  ppvDen <- sens * p + (1 - spec) * (1 - p)
  npvDen <- spec * (1 - p) + (1 - sens) * p
  list(
    sensitivity      = sens,
    sensitivityLower = sensCi[1],
    sensitivityUpper = sensCi[2],
    specificity      = spec,
    specificityLower = specCi[1],
    specificityUpper = specCi[2],
    ppv              = if (ppvDen > 0) sens * p / ppvDen else NA_real_,
    npv              = if (npvDen > 0) spec * (1 - p) / npvDen else NA_real_,
    lrPositive       = if (spec < 1) sens / (1 - spec) else NA_real_,
    lrNegative       = if (spec > 0) (1 - sens) / spec else NA_real_,
    accuracy         = (tp + tn) / (nPos + nNeg),
    youden           = sens + spec - 1
  )
}

.rocPrevalence <- function(options) {
  if (options[["prevalence"]] == "custom") options[["prevalenceValue"]] else NULL
}

# Counts at every pROC threshold, in pROC's order.
.rocAllCounts <- function(fitInfo) {
  fit <- fitInfo$fit
  tp  <- round(fit$sensitivities * fitInfo$nPositive)
  tn  <- round(fit$specificities * fitInfo$nNegative)
  data.frame(threshold = fit$thresholds, tp = tp, fn = fitInfo$nPositive - tp,
             tn = tn, fp = fitInfo$nNegative - tn)
}

# Counts at one or more thresholds, with pROC's rule (roc_utils_perfs in pROC/R/roc.utils.R): for "higher" a
# case is positive when x >= t and a control negative when x < t; for "lower", x <= t and x > t. The values are
# sorted once and each threshold is placed with findInterval, which counts the values below a threshold
# (left.open = TRUE) or at or below it, so many thresholds cost little more than one.
.rocCountsAtThreshold <- function(fitInfo, threshold, options) {
  cases    <- sort(fitInfo$cases)
  controls <- sort(fitInfo$controls)
  if (options[["direction"]] == "lower") {
    tp <- findInterval(threshold, cases)
    tn <- length(controls) - findInterval(threshold, controls)
  } else {
    tp <- length(cases) - findInterval(threshold, cases, left.open = TRUE)
    tn <- findInterval(threshold, controls, left.open = TRUE)
  }
  data.frame(threshold = threshold, tp = tp, fn = fitInfo$nPositive - tp,
             tn = tn, fp = fitInfo$nNegative - tn)
}

# Exact comparisons on integer counts. A tolerance on proportions would merge thresholds that are not tied
# when n is large (two Youden values can differ by less than 1e-9).

# The rows of a matrix of base-2^20 digits (most significant first) that equal the smallest number.
.rocLexMin <- function(digits) {
  keep <- rep(TRUE, nrow(digits))
  for (k in seq_len(ncol(digits))) {
    best <- min(digits[keep, k])
    keep <- keep & digits[, k] == best
  }
  keep
}

# a^2 + b^2 for non-negative whole numbers below 2^53, exactly, as base-2^20 digits (most significant first).
# Each number is split into three 20-bit pieces, so every partial product and sum stays below 2^53.
.rocSumOfSquaresDigits <- function(a, b) {
  base <- 2^20
  split3 <- function(x) cbind(x %% base, (x %/% base) %% base, x %/% base^2)
  A <- split3(a)
  B <- split3(b)
  coef <- matrix(0, length(a), 5)
  for (i in 1:3) for (j in 1:3)
    coef[, i + j - 1] <- coef[, i + j - 1] + A[, i] * A[, j] + B[, i] * B[, j]
  digits <- matrix(0, length(a), 6)
  carry <- 0
  for (k in 1:5) {
    total <- coef[, k] + carry
    digits[, k] <- total %% base
    carry <- total %/% base
  }
  digits[, 6] <- carry
  digits[, 6:1, drop = FALSE]
}

# Rows of the chosen cut-off(s) for one marker: a data frame of counts.
.rocSelectCutoff <- function(fitInfo, options) {
  criterion <- options[["cutoffCriterion"]]
  if (criterion == "threshold")
    return(.rocCountsAtThreshold(fitInfo, options[["thresholdValue"]], options))

  counts <- .rocAllCounts(fitInfo)
  # Doubles, so that the products below never overflow R's integers (above 2^31 - 1).
  nPos <- as.numeric(fitInfo$nPositive)
  nNeg <- as.numeric(fitInfo$nNegative)
  tp <- counts$tp
  tn <- counts$tn

  keep <- switch(criterion,
    youden = {
      # J * nPos * nNeg = tp * nNeg + tn * nPos - nPos * nNeg: a whole number, exact below 2^53.
      score <- tp * nNeg + tn * nPos
      score == max(score)
    },
    closestTopLeft = {
      # Squared distance * nPos^2 * nNeg^2 = (fn * nNeg)^2 + (fp * nPos)^2, compared exactly. The exact digits
      # are worked out only for the rows that can be closest. In double precision each sum is within a relative
      # 2.3e-16 of its exact value, so every closest row lies within a relative 5e-16 of the smallest double
      # sum, well inside the 1e-12 kept here. Zero sums are exact in double precision.
      a <- counts$fn * nNeg
      b <- counts$fp * nPos
      approx <- a^2 + b^2
      near <- which(approx <= min(approx) * (1 + 1e-12))
      keep <- rep(FALSE, nrow(counts))
      keep[near] <- .rocLexMin(.rocSumOfSquaresDigits(a[near], b[near]))
      keep
    },
    fixedSensitivity = {
      # Division is correctly rounded and so monotone: tp / nPos >= value holds whenever the count meets the
      # value typed, including equality (7 / 10 >= 0.7). A value below 1 (.rocCheckOptions) always has an
      # eligible threshold: the outer one where every case is positive.
      eligible <- tp / nPos >= options[["fixedSensitivityValue"]]
      if (!any(eligible))
        return(counts[0, , drop = FALSE])
      best <- eligible & tn == max(tn[eligible])
      # Among equal specificity, the highest sensitivity (the other points are dominated).
      best & tp == max(tp[best])
    },
    fixedSpecificity = {
      eligible <- tn / nNeg >= options[["fixedSpecificityValue"]]
      if (!any(eligible))
        return(counts[0, , drop = FALSE])
      best <- eligible & tp == max(tp[eligible])
      best & tn == max(tn[best])
    }
  )
  counts[keep, , drop = FALSE]
}

# AUC confidence interval ----
# Returns the CI (3 numbers: lower, median, upper), or NULL when it cannot be computed. Under the bootstrap,
# bootAucs holds the marker's replicate AUCs (.rocBootstrapState), or an error.
.rocAucCi <- function(info, options, bootAucs = NULL) {
  if (options[["aucCiMethod"]] == "bootstrap") {
    # A replicate AUC that is missing would stop quantile(); the table then gives a footnote instead.
    if (is.null(bootAucs) || isTryError(bootAucs) || anyNA(bootAucs))
      return(NULL)
    return(.rocBootstrapQuantiles(bootAucs, options[["ciLevel"]]))
  }
  as.numeric(.rocDelongCi(info$placements, info$nPositive, info$nNegative, options[["ciLevel"]]))
}

# Progress bar ticks per bootstrapped marker: one per 1% of the replicates.
.rocBootTicks <- 100

# Stratified bootstrap of the AUC, with the same random draws as pROC::ci.auc(method = "bootstrap",
# boot.stratified = TRUE). pROC (R/bootstrap.R, stratified.ci.auc) draws, on each replicate,
# sample(controls, replace = TRUE) and then sample(cases, replace = TRUE), and rebuilds a full ROC curve.
# sample(x, replace = TRUE) is x[sample.int(length(x), replace = TRUE)], so the same sample.int() calls in the
# same order give the same draws. (This needs at least two cases and two controls: with one value, sample()
# draws from 1:x instead. The caller only bootstraps with two or more in each class.)
# Instead of a new ROC curve, each replicate counts how often each control was drawn and computes the
# Mann-Whitney sum with ties counted as one half. With the controls sorted by value and W the running total
# of their draw counts (W[1] = 0), a case with lo controls below it and hi controls at or below it scores
#   W[lo + 1] + 0.5 * (W[hi + 1] - W[lo + 1]) = 0.5 * (W[lo + 1] + W[hi + 1]),
# and AUC ("<": higher values positive) = sum of the drawn cases' scores / (nPos * nNeg). For ">" the AUC is
# 1 minus that. Every term is a multiple of 0.5 below 2^53, so the sum is exact. nPos * nNeg is taken as a double:
# as an integer product it would overflow (NA) above 2^31 - 1, for example with 50,000 cases and 50,000 controls.
# The interval uses pROC's rule: quantile() (type 7) at (1 - level) / 2, 0.5 and 1 - (1 - level) / 2
# (ci_auc_bootstrap in R/bootstrap.R).
.rocBootstrapAucCi <- function(cases, controls, direction, nBoot, ciLevel, tick = FALSE) {
  .rocBootstrapQuantiles(.rocBootstrapAucs(cases, controls, direction, nBoot, tick), ciLevel)
}

# The replicate AUCs of .rocBootstrapAucCi, in the order drawn.
.rocBootstrapAucs <- function(cases, controls, direction, nBoot, tick = FALSE) {
  nPos <- length(cases)
  nNeg <- length(controls)
  ord <- order(controls)
  sortedControls <- controls[ord]
  lo <- findInterval(cases, sortedControls, left.open = TRUE) + 1L
  hi <- findInterval(cases, sortedControls) + 1L

  sums <- numeric(nBoot)
  tickEvery <- max(1, nBoot / .rocBootTicks)
  ticksDone <- 0
  for (b in seq_len(nBoot)) {
    drawn <- tabulate(sample.int(nNeg, replace = TRUE), nNeg)
    W <- c(0, cumsum(drawn[ord]))
    score <- W[lo] + W[hi]
    sums[b] <- sum(score[sample.int(nPos, replace = TRUE)])

    if (tick) {
      due <- min(.rocBootTicks, floor(b / tickEvery))
      while (ticksDone < due) {
        progressbarTick()
        ticksDone <- ticksDone + 1
      }
    }
  }
  if (tick) {
    while (ticksDone < .rocBootTicks) {
      progressbarTick()
      ticksDone <- ticksDone + 1
    }
  }

  aucs <- 0.5 * sums / (as.numeric(nPos) * nNeg)
  if (direction == ">")
    aucs <- 1 - aucs
  aucs
}

# pROC's percentile interval of replicate AUCs (ci_auc_bootstrap in R/bootstrap.R).
.rocBootstrapQuantiles <- function(aucs, ciLevel) {
  alpha <- (1 - ciLevel) / 2
  stats::quantile(aucs, c(alpha, 0.5, 1 - alpha), names = FALSE)
}
