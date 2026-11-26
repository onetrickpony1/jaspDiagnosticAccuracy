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

# Main entry point ----
rocCurve <- function(jaspResults, dataset, options) {
  ready <- options[["outcome"]] != "" && length(options[["tests"]]) > 0

  if (ready)
    .rocComputeFits(jaspResults, dataset, options)

  .rocAucTable(jaspResults, options, ready)
  .rocCutoffTable(jaspResults, options, ready)
  .rocComparisonTable(jaspResults, options, ready)
  .rocCoordinatesTable(jaspResults, options, ready)
  .rocPlot(jaspResults, options, ready)
  .rocThresholdPlot(jaspResults, options, ready)

  return()
}

# Dependencies ----
.rocDataDependencies <- c("outcome", "positiveLevel", "tests", "direction")
.rocCiDependencies   <- c("aucCiMethod", "ciLevel")
.rocCutoffDependencies <- c("cutoffCriterion", "fixedSensitivityValue", "fixedSpecificityValue", "thresholdValue")
.rocPrevalenceDependencies <- c("prevalence", "prevalenceValue")

# The replicates and the seed change the AUC table only under the bootstrap, and the seed only when it is set.
# Under DeLong they are not dependencies, so changing them does not recompute the table. A switch between
# DeLong and the bootstrap changes aucCiMethod, which is always a dependency, so nothing can go stale.
.rocAucTableDependencies <- function(options) {
  deps <- c(.rocDataDependencies, .rocCiDependencies)
  if (options[["aucCiMethod"]] == "bootstrap") {
    deps <- c(deps, "bootstrapReplicates", "setSeed")
    if (isTRUE(options[["setSeed"]]))
      deps <- c(deps, "seed")
  }
  deps
}

# Data preparation ----
.rocOutcomeFactor <- function(y) {
  if (!is.factor(y))
    y <- factor(y)
  droplevels(y)
}

.rocMarkerNumeric <- function(x) {
  # Returns list(values, textLevels). Ordinal markers arrive as factors. Numeric labels keep their values;
  # other labels use their level order (1, 2, ...).
  if (is.factor(x)) {
    lv  <- levels(x)
    num <- suppressWarnings(as.numeric(lv))
    if (!anyNA(num) && !anyDuplicated(num))
      return(list(values = num[as.integer(x)], textLevels = FALSE))
    return(list(values = as.numeric(as.integer(x)), textLevels = TRUE))
  }
  if (is.character(x))
    return(list(values = suppressWarnings(as.numeric(x)), textLevels = FALSE))
  return(list(values = as.numeric(x), textLevels = FALSE))
}

# Lists levels for an error message: at most `max` of them, then "and N more".
.rocLevelList <- function(levels, max = 10) {
  if (length(levels) <= max)
    return(paste(levels, collapse = ", "))
  gettextf("%1$s and %2$d more", paste(levels[seq_len(max)], collapse = ", "), length(levels) - max)
}

.rocPrepareOutcome <- function(dataset, options) {
  outcomeName <- options[["outcome"]]
  y <- .rocOutcomeFactor(dataset[[outcomeName]])
  levelsPresent <- levels(y)

  if (length(levelsPresent) != 2) {
    found <- if (length(levelsPresent) == 0) gettext("none") else .rocLevelList(levelsPresent)
    .quitAnalysis(gettextf(
      "The outcome '%1$s' must have exactly two levels after rows with a missing outcome are removed. Levels found: %2$s.",
      decodeColNames(outcomeName), found))
  }

  positive <- options[["positiveLevel"]]
  if (is.null(positive) || length(positive) != 1 || is.na(positive) || identical(positive, ""))
    positive <- levelsPresent[2]

  if (!positive %in% levelsPresent)
    .quitAnalysis(gettextf(
      "The positive level '%1$s' is not among the levels of the outcome '%2$s' in the data (%3$s). Choose another positive level.",
      positive, decodeColNames(outcomeName), .rocLevelList(levelsPresent)))

  negative <- setdiff(levelsPresent, positive)
  list(y = as.character(y), positive = positive, negative = negative)
}

# pROC direction: "<" means controls < cases, so higher values indicate positive.
.rocProcDirection <- function(options) {
  if (options[["direction"]] == "lower") ">" else "<"
}

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
  if (isTryError(fit)) {
    out$problem <- gettextf("No ROC curve for %1$s: %2$s", markerLabel, .extractErrorMessage(fit))
    return(out)
  }

  out$fit <- fit
  out$auc <- as.numeric(fit$auc)

  # DeLong SE needs at least two cases in each class. When AUC = 1, pROC warns that the variance is always 0
  # (pROC R/var.R, line 56). The AUC table explains a zero SE in its own footnote, so only that warning is muffled.
  out$se <- NA_real_
  if (length(cases) >= 2 && length(controls) >= 2) {
    v <- try(withCallingHandlers(pROC::var(fit, method = "delong"), warning = function(w) {
      if (grepl("AUC == 1 is always 0", conditionMessage(w), fixed = TRUE))
        invokeRestart("muffleWarning")
    }), silent = TRUE)
    if (!isTryError(v) && is.finite(v) && v >= 0)
      out$se <- sqrt(v)
  }
  out$tiesBetweenClasses <- any(cases %in% controls)
  return(out)
}

.rocComputeFits <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["rocFitsState"]]))
    return()

  outcome   <- .rocPrepareOutcome(dataset, options)
  direction <- .rocProcDirection(options)
  markers   <- .rocMarkers(options)

  values <- list()
  fits   <- list()
  textLevels <- character(0)
  for (marker in markers) {
    conv <- .rocMarkerNumeric(dataset[[marker]])
    # Infinite values are treated as missing for this marker only.
    infinite <- is.infinite(conv$values)
    conv$values[infinite] <- NA_real_
    values[[marker]] <- conv$values
    if (conv$textLevels)
      textLevels <- c(textLevels, marker)
    fits[[marker]] <- .rocFitMarker(conv$values, outcome$y, outcome$positive, outcome$negative, direction,
                                    decodeColNames(marker))
    # Report each left-out row once. A row with an infinite value and an outcome counts as infinite only. A row
    # with a missing outcome counts as missing, whatever its marker value.
    infiniteOnly <- sum(infinite & !is.na(outcome$y))
    fits[[marker]]$nInfinite <- infiniteOnly
    fits[[marker]]$nRemoved  <- fits[[marker]]$nRemoved - infiniteOnly
  }

  state <- createJaspState(list(
    outcome    = outcome,
    markers    = markers,
    values     = values,
    fits       = fits,
    textLevels = textLevels,
    nRows      = length(outcome$y)
  ))
  state$dependOn(.rocDataDependencies)
  jaspResults[["rocFitsState"]] <- state
}

# The markers in the order given, each once: a variable listed twice is used once.
.rocMarkers <- function(options) {
  unique(unlist(options[["tests"]]))
}

.rocGetFits <- function(jaspResults) {
  jaspResults[["rocFitsState"]]$object
}

# Helpers ----
.rocCiTitle <- function(options) {
  gettextf("%s%% CI", format(100 * options[["ciLevel"]], digits = 6))
}

.rocDirectionText <- function(results, options) {
  if (options[["direction"]] == "lower")
    gettextf("Positive level: %s. Lower marker values indicate positive.", results$outcome$positive)
  else
    gettextf("Positive level: %s. Higher marker values indicate positive.", results$outcome$positive)
}

.rocRuleText <- function(options) {
  if (options[["direction"]] == "lower")
    gettext("A case is classed as positive when its marker value is at or below the threshold (value ≤ threshold).")
  else
    gettext("A case is classed as positive when its marker value is at or above the threshold (value ≥ threshold).")
}

# p columns use type "pvalue", which jaspBase gives the format "dp:3;p:.001" (R/zzzWrappers.R, addColumnInfo,
# commit 5038e41), so a tiny p shows as "< .001". With exact p values turned on in JASP's preferences, a p that
# is 0 in double precision would show as 0, so it gets this footnote.
.rocPZeroText <- function() {
  gettext("p is below the smallest positive number R can store (about 5 × 10⁻³²⁴), so it is stored as 0.")
}

.rocEmpty <- function(x) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) "" else x
}

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

# Counts at a user threshold, with pROC's rule (roc_utils_perfs in pROC/R/roc.utils.R).
.rocCountsAtThreshold <- function(fitInfo, threshold, options) {
  if (options[["direction"]] == "lower") {
    tp <- sum(fitInfo$cases <= threshold)
    tn <- sum(fitInfo$controls > threshold)
  } else {
    tp <- sum(fitInfo$cases >= threshold)
    tn <- sum(fitInfo$controls < threshold)
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
  nPos <- fitInfo$nPositive
  nNeg <- fitInfo$nNegative
  tp <- counts$tp
  tn <- counts$tn

  keep <- switch(criterion,
    youden = {
      # J * nPos * nNeg = tp * nNeg + tn * nPos - nPos * nNeg: a whole number, exact below 2^53.
      score <- tp * nNeg + tn * nPos
      score == max(score)
    },
    closestTopLeft = {
      # Squared distance * nPos^2 * nNeg^2 = (fn * nNeg)^2 + (fp * nPos)^2, compared exactly.
      .rocLexMin(.rocSumOfSquaresDigits(counts$fn * nNeg, counts$fp * nPos))
    },
    fixedSensitivity = {
      # Division is correctly rounded and so monotone: tp / nPos >= value holds whenever the count meets the
      # value typed, including equality (7 / 10 >= 0.7).
      eligible <- tp / nPos >= options[["fixedSensitivityValue"]]
      best <- eligible & tn == max(tn[eligible])
      # Among equal specificity, the highest sensitivity (the other points are dominated).
      best & tp == max(tp[best])
    },
    fixedSpecificity = {
      eligible <- tn / nNeg >= options[["fixedSpecificityValue"]]
      best <- eligible & tp == max(tp[eligible])
      best & tn == max(tn[best])
    }
  )
  counts[keep, , drop = FALSE]
}

.rocCriterionLabel <- function(options) {
  switch(options[["cutoffCriterion"]],
    youden           = gettext("Youden"),
    closestTopLeft   = gettext("Closest to (0, 1)"),
    fixedSensitivity = gettextf("Sensitivity ≥ %s", format(options[["fixedSensitivityValue"]], digits = 6)),
    fixedSpecificity = gettextf("Specificity ≥ %s", format(options[["fixedSpecificityValue"]], digits = 6)),
    threshold        = gettext("Threshold")
  )
}

.rocAddMarkerFootnotes <- function(table, results, colName = "marker") {
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem))
      table$addFootnote(info$problem, colNames = colName, rowNames = marker)
  }
}

# AUC table ----
.rocAucTable <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["aucTable"]]))
    return()

  table <- createJaspTable(title = gettext("Area Under the ROC Curve"))
  table$dependOn(.rocAucTableDependencies(options))
  table$position <- 1
  ciTitle <- .rocCiTitle(options)
  table$addColumnInfo(name = "marker",    title = gettext("Marker"),     type = "string")
  table$addColumnInfo(name = "nPositive", title = gettext("n Positive"), type = "integer")
  table$addColumnInfo(name = "nNegative", title = gettext("n Negative"), type = "integer")
  table$addColumnInfo(name = "auc",       title = gettext("AUC"),        type = "number")
  table$addColumnInfo(name = "se",        title = gettext("SE"),         type = "number")
  table$addColumnInfo(name = "lower",     title = gettext("Lower"),      type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "upper",     title = gettext("Upper"),      type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "z",         title = gettext("z"),          type = "number")
  table$addColumnInfo(name = "p",         title = gettext("p"),          type = "pvalue")
  jaspResults[["aucTable"]] <- table

  if (!ready) {
    for (marker in .rocMarkers(options))
      table$addRows(list(marker = decodeColNames(marker)), rowNames = marker)
    return()
  }

  results <- .rocGetFits(jaspResults)
  bootstrap <- options[["aucCiMethod"]] == "bootstrap"
  if (bootstrap) {
    nBoot <- sum(vapply(results$fits, function(info) is.null(info$problem) && !is.na(info$se), logical(1)))
    if (nBoot > 0)
      startProgressbar(nBoot * .rocBootTicks, gettext("Bootstrapping the AUC"))
  }

  zeroSe <- FALSE
  tooFew <- FALSE
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    row  <- list(marker = decodeColNames(marker), nPositive = info$nPositive, nNegative = info$nNegative,
                 auc = "", se = "", lower = "", upper = "", z = "", p = "")

    if (is.null(info$problem)) {
      row$auc <- info$auc
      if (is.na(info$se)) {
        tooFew <- TRUE
        table$addFootnote(gettextf("%s: the standard error and the confidence interval need at least two cases in each class.",
                                   decodeColNames(marker)), colNames = "se", rowNames = marker)
      } else {
        row$se <- info$se
        if (info$se > 0) {
          z <- (info$auc - 0.5) / info$se
          row$z <- z
          row$p <- 2 * stats::pnorm(-abs(z))
          if (row$p == 0)
            table$addFootnote(.rocPZeroText(), colNames = "p", rowNames = marker)
        } else {
          zeroSe <- TRUE
        }

        ci <- .rocAucCi(info$fit, options)
        if (is.character(ci)) {
          table$addFootnote(gettextf("%1$s: the confidence interval could not be computed: %2$s", decodeColNames(marker), ci),
                            colNames = "lower", rowNames = marker)
        } else {
          row$lower <- .rocEmpty(ci[1])
          row$upper <- .rocEmpty(ci[3])
        }
      }
      if (isTRUE(info$tiesBetweenClasses))
        table$addFootnote(gettextf("%s has tied values between the classes. Each tie counts as one half in the AUC.",
                                   decodeColNames(marker)), colNames = "auc", rowNames = marker)
    }
    if (isTRUE(info$nInfinite > 0))
      table$addFootnote(sprintf(ngettext(info$nInfinite,
                                         "%1$s: %2$d infinite value was treated as missing.",
                                         "%1$s: %2$d infinite values were treated as missing."),
                                decodeColNames(marker), info$nInfinite),
                        colNames = "marker", rowNames = marker)
    if (info$nRemoved > 0)
      table$addFootnote(sprintf(ngettext(info$nRemoved,
                                         "%1$s: %2$d row with a missing value was left out.",
                                         "%1$s: %2$d rows with a missing value were left out."),
                                decodeColNames(marker), info$nRemoved),
                        colNames = "marker", rowNames = marker)
    table$addRows(row, rowNames = marker)
  }

  .rocAddMarkerFootnotes(table, results)
  for (marker in results$textLevels)
    table$addFootnote(gettextf("%s has text labels. Its level order (1, 2, ...) is used as the marker value.",
                               decodeColNames(marker)), colNames = "marker", rowNames = marker)
  if (zeroSe)
    table$addFootnote(gettext("When the DeLong standard error is zero (the classes do not overlap), z and p are not defined."))

  if (bootstrap)
    table$addFootnote(gettextf("The confidence interval is a percentile interval from %d stratified bootstrap samples.",
                               as.integer(options[["bootstrapReplicates"]])))
  else
    table$addFootnote(gettext("The confidence interval uses the DeLong standard error."))
  table$addFootnote(gettext("z and p test AUC = 0.5: a two-sided Wald test with the DeLong standard error."))
  table$addFootnote(.rocDirectionText(results, options))
}

# Returns the CI (3 numbers: lower, median, upper) or an error message.
.rocAucCi <- function(fit, options) {
  ci <- if (options[["aucCiMethod"]] == "bootstrap") {
    .setSeedJASP(options)
    try(.rocBootstrapAucCi(fit$cases, fit$controls, fit$direction, options[["bootstrapReplicates"]],
                           options[["ciLevel"]], tick = TRUE),
        silent = TRUE)
  } else {
    try(suppressWarnings(pROC::ci.auc(fit, conf.level = options[["ciLevel"]], method = "delong")), silent = TRUE)
  }
  if (isTryError(ci))
    return(.extractErrorMessage(ci))
  as.numeric(ci)
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
# 1 minus that. Every term is a multiple of 0.5 below 2^53, so the sum is exact. The interval uses pROC's
# rule: quantile() (type 7) at (1 - level) / 2, 0.5 and 1 - (1 - level) / 2 (ci_auc_bootstrap in
# R/bootstrap.R).
.rocBootstrapAucCi <- function(cases, controls, direction, nBoot, ciLevel, tick = FALSE) {
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

  aucs <- 0.5 * sums / (nPos * nNeg)
  if (direction == ">")
    aucs <- 1 - aucs
  alpha <- (1 - ciLevel) / 2
  stats::quantile(aucs, c(alpha, 0.5, 1 - alpha), names = FALSE)
}

# Cut-off table ----
.rocCutoffTable <- function(jaspResults, options, ready) {
  if (!isTRUE(options[["cutoffTable"]]))
    return()
  if (!is.null(jaspResults[["cutoffTable"]]))
    return()

  table <- createJaspTable(title = gettext("Cut-off"))
  table$dependOn(c(.rocDataDependencies, .rocCutoffDependencies, .rocPrevalenceDependencies, "ciLevel", "cutoffTable"))
  table$position <- 2
  ciTitle <- .rocCiTitle(options)
  seTitle <- gettextf("Sensitivity %s", ciTitle)
  spTitle <- gettextf("Specificity %s", ciTitle)
  table$addColumnInfo(name = "marker",           title = gettext("Marker"),      type = "string", combine = TRUE)
  table$addColumnInfo(name = "criterion",        title = gettext("Criterion"),   type = "string")
  # The threshold cells are mixed cells, each with the significant digits its marker needs (.rocThresholdDigits).
  table$addColumnInfo(name = "threshold",        title = gettext("Threshold"),   type = "mixed")
  table$addColumnInfo(name = "sensitivity",      title = gettext("Sensitivity"), type = "number")
  table$addColumnInfo(name = "sensitivityLower", title = gettext("Lower"),       type = "number", overtitle = seTitle)
  table$addColumnInfo(name = "sensitivityUpper", title = gettext("Upper"),       type = "number", overtitle = seTitle)
  table$addColumnInfo(name = "specificity",      title = gettext("Specificity"), type = "number")
  table$addColumnInfo(name = "specificityLower", title = gettext("Lower"),       type = "number", overtitle = spTitle)
  table$addColumnInfo(name = "specificityUpper", title = gettext("Upper"),       type = "number", overtitle = spTitle)
  table$addColumnInfo(name = "ppv",              title = gettext("PPV"),         type = "number")
  table$addColumnInfo(name = "npv",              title = gettext("NPV"),         type = "number")
  table$addColumnInfo(name = "lrPositive",       title = gettext("LR+"),         type = "number")
  table$addColumnInfo(name = "lrNegative",       title = gettext("LR−"),    type = "number")
  table$addColumnInfo(name = "accuracy",         title = gettext("Accuracy"),    type = "number")
  table$addColumnInfo(name = "youden",           title = gettext("Youden's J"),  type = "number")
  jaspResults[["cutoffTable"]] <- table

  if (!ready) {
    for (marker in .rocMarkers(options))
      table$addRows(list(marker = decodeColNames(marker)), rowNames = marker)
    return()
  }

  results    <- .rocGetFits(jaspResults)
  prevalence <- .rocPrevalence(options)
  criterion  <- .rocCriterionLabel(options)
  undefined  <- FALSE

  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem)) {
      table$addRows(list(marker = decodeColNames(marker), criterion = criterion,
                         threshold = jaspBase::createMixedRow("", "string")), rowNames = marker)
      next
    }

    chosen <- .rocSelectCutoff(info, options)
    # A threshold you type is shown with all its digits (at least 4). A chosen pROC threshold gets the digits that
    # show it between the observed values it separates.
    if (options[["cutoffCriterion"]] == "threshold") {
      digits <- list(digits = .rocTypedDigits(options[["thresholdValue"]]), exact = TRUE)
    } else {
      digits <- .rocThresholdDigits(chosen$threshold, c(info$cases, info$controls))
      if (!digits$exact)
        table$addFootnote(.rocRoundedThresholdsText(marker), colNames = "threshold", rowNames = marker)
    }
    # The first row of each marker is named after the marker, so marker footnotes can find it.
    for (k in seq_len(nrow(chosen))) {
      stats <- .rocStatsFromCounts(chosen$tp[k], chosen$fn[k], chosen$tn[k], chosen$fp[k], prevalence, options[["ciLevel"]])
      row <- c(list(marker = decodeColNames(marker), criterion = criterion,
                    threshold = .rocThresholdCell(chosen$threshold[k], digits$digits)), stats)
      for (col in c("ppv", "npv", "lrPositive", "lrNegative")) {
        if (is.na(row[[col]])) {
          row[[col]] <- ""
          undefined <- TRUE
        }
      }
      table$addRows(row, rowNames = if (k == 1) marker else paste0(marker, "_", k))
    }
    if (nrow(chosen) > 1)
      table$addFootnote(gettextf("%1$s: %2$d thresholds are equally good under this criterion. Each has its own row.",
                                 decodeColNames(marker), nrow(chosen)), colNames = "threshold", rowNames = marker)
  }

  .rocAddMarkerFootnotes(table, results)
  if (undefined)
    table$addFootnote(gettext("Empty PPV, NPV or likelihood ratio cells are undefined (a division by zero)."))
  table$addFootnote(.rocRuleText(options))
  table$addFootnote(gettext("Sensitivity and specificity intervals are Clopper-Pearson intervals."))
  if (is.null(prevalence))
    table$addFootnote(gettext("PPV and NPV use the sample prevalence of each marker's complete cases."))
  else
    table$addFootnote(gettextf("PPV and NPV use a prevalence of %s.", format(prevalence, digits = 6)))
}

# Comparison table ----
.rocComparisonTable <- function(jaspResults, options, ready) {
  if (!options[["compareMarkers"]] || length(.rocMarkers(options)) < 2)
    return()
  if (!is.null(jaspResults[["comparisonTable"]]))
    return()

  table <- createJaspTable(title = gettext("Paired Comparison of AUCs"))
  table$dependOn(c(.rocDataDependencies, "compareMarkers", "ciLevel"))
  table$position <- 3
  ciTitle <- .rocCiTitle(options)
  table$addColumnInfo(name = "marker1",    title = gettext("Marker 1"),   type = "string")
  table$addColumnInfo(name = "marker2",    title = gettext("Marker 2"),   type = "string")
  table$addColumnInfo(name = "n",          title = gettext("n"),          type = "integer")
  table$addColumnInfo(name = "difference", title = gettext("Difference"), type = "number")
  table$addColumnInfo(name = "lower",      title = gettext("Lower"),      type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "upper",      title = gettext("Upper"),      type = "number", overtitle = ciTitle)
  table$addColumnInfo(name = "z",          title = gettext("z"),          type = "number")
  table$addColumnInfo(name = "p",          title = gettext("p"),          type = "pvalue")
  jaspResults[["comparisonTable"]] <- table

  if (!ready)
    return()

  results   <- .rocGetFits(jaspResults)
  markers   <- results$markers
  direction <- .rocProcDirection(options)
  y         <- results$outcome$y

  complete <- !is.na(y)
  for (marker in markers)
    complete <- complete & !is.na(results$values[[marker]])
  nComplete <- sum(complete)
  nRemoved  <- length(y) - nComplete

  # The listwise rows are shared by every pair. When they hold fewer than two cases in a class, no pair can be
  # compared, and the problem lies with these rows, not with the pair's markers.
  listwiseProblem <- .rocListwiseClassProblem(results, complete)

  listwise <- list()
  if (is.null(listwiseProblem)) {
    for (marker in markers)
      listwise[[marker]] <- .rocFitMarker(results$values[[marker]][complete], y[complete], results$outcome$positive,
                                          results$outcome$negative, direction,
                                          gettextf("%s on the rows used for the comparison", decodeColNames(marker)))
  }

  pairs <- utils::combn(markers, 2, simplify = FALSE)
  for (pair in pairs) {
    a <- listwise[[pair[1]]]
    b <- listwise[[pair[2]]]
    rowName <- paste(pair, collapse = "_vs_")
    row <- list(marker1 = decodeColNames(pair[1]), marker2 = decodeColNames(pair[2]), n = nComplete,
                difference = "", lower = "", upper = "", z = "", p = "")

    problem <- if (is.null(listwiseProblem)) c(a$problem, b$problem) else listwiseProblem

    if (is.null(problem)) {
      test <- try(suppressWarnings(pROC::roc.test(a$fit, b$fit, method = "delong", paired = TRUE,
                                                 conf.level = options[["ciLevel"]])), silent = TRUE)
      if (isTryError(test)) {
        problem <- gettextf("The comparison could not be computed: %s", .extractErrorMessage(test))
      } else {
        row$difference <- unname(test$estimate[1] - test$estimate[2])
        row$lower <- .rocEmpty(unname(test$conf.int[1]))
        row$upper <- .rocEmpty(unname(test$conf.int[2]))
        # The CI is the difference plus or minus a multiple of its SE, so a zero-width CI means SE = 0. pROC
        # then sets z to 0 (p = 1) when the difference is also 0 (delong.paired.test in R/delong.R), which
        # would look like a real test. Leave z and p empty instead.
        zeroSe <- isTRUE(unname(test$conf.int[1]) == unname(test$conf.int[2]))
        if (zeroSe) {
          problem <- gettext("The standard error of the difference is zero (for example, the two markers order the cases in the same way, or both separate the classes perfectly), so z and p are not defined.")
        } else {
          row$z <- .rocEmpty(unname(test$statistic))
          row$p <- .rocEmpty(unname(test$p.value))
          if (identical(row$p, 0))
            table$addFootnote(.rocPZeroText(), colNames = "p", rowNames = rowName)
          if (identical(row$z, ""))
            problem <- gettext("The standard error of the difference is zero, so z and p are not defined.")
        }
      }
    }
    table$addRows(row, rowNames = rowName)
    for (msg in problem)
      table$addFootnote(msg, colNames = "marker1", rowNames = rowName)
  }

  table$addFootnote(gettext("DeLong's test for two paired ROC curves. Difference = AUC of marker 1 - AUC of marker 2."))
  table$addFootnote(sprintf(ngettext(nRemoved,
                                     "Only rows where the outcome and every marker are present are used: %d row was left out.",
                                     "Only rows where the outcome and every marker are present are used: %d rows were left out."),
                            nRemoved))
}

# A message when the rows used for the comparison (outcome and every marker present) hold fewer than two cases in
# a class, or NULL. It names each marker that has fewer than two cases in that class even on its own rows,
# because that marker alone makes the comparison impossible.
.rocListwiseClassProblem <- function(results, complete) {
  y    <- results$outcome$y[complete]
  nPos <- sum(y == results$outcome$positive)
  nNeg <- sum(y == results$outcome$negative)
  if (nPos >= 2 && nNeg >= 2)
    return(NULL)

  msg <- gettextf(
    "The comparison uses only rows where the outcome and every marker are present. These rows hold %1$d positive and %2$d negative cases, and the comparison needs at least two in each class.",
    nPos, nNeg)

  for (marker in results$markers) {
    info  <- results$fits[[marker]]
    label <- decodeColNames(marker)
    if (nPos < 2 && info$nPositive < 2)
      msg <- paste(msg, gettextf("%s has fewer than two positive cases even on its own rows.", label))
    if (nNeg < 2 && info$nNegative < 2)
      msg <- paste(msg, gettextf("%s has fewer than two negative cases even on its own rows.", label))
  }
  msg
}

# Coordinates table ----
.rocCoordinatesTable <- function(jaspResults, options, ready) {
  if (!options[["coordinatesTable"]])
    return()
  if (!is.null(jaspResults[["coordinatesTable"]]))
    return()

  table <- createJaspTable(title = gettext("Coordinates"))
  table$dependOn(c(.rocDataDependencies, .rocPrevalenceDependencies, "coordinatesTable"))
  table$position <- 4
  table$addColumnInfo(name = "marker",      title = gettext("Marker"),      type = "string", combine = TRUE)
  # The threshold cells are mixed cells (jaspBase::createMixedRow), each with the number of significant digits
  # its marker needs (.rocThresholdDigits), as in the cut-off table.
  table$addColumnInfo(name = "threshold",   title = gettext("Threshold"),   type = "mixed")
  table$addColumnInfo(name = "sensitivity", title = gettext("Sensitivity"), type = "number")
  table$addColumnInfo(name = "specificity", title = gettext("Specificity"), type = "number")
  table$addColumnInfo(name = "ppv",         title = gettext("PPV"),         type = "number")
  table$addColumnInfo(name = "npv",         title = gettext("NPV"),         type = "number")
  jaspResults[["coordinatesTable"]] <- table

  if (!ready)
    return()

  results    <- .rocGetFits(jaspResults)
  prevalence <- .rocPrevalence(options)
  undefined  <- FALSE

  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem)) {
      table$addRows(list(marker = decodeColNames(marker), threshold = jaspBase::createMixedRow("", "string")),
                    rowNames = marker)
      next
    }
    counts <- .rocAllCounts(info)
    digits <- .rocThresholdDigits(counts$threshold, c(info$cases, info$controls))
    if (!digits$exact)
      table$addFootnote(.rocRoundedThresholdsText(marker), colNames = "threshold", rowNames = marker)
    se <- counts$tp / info$nPositive
    sp <- counts$tn / info$nNegative
    p  <- if (is.null(prevalence)) info$nPositive / (info$nPositive + info$nNegative) else prevalence
    ppvDen <- se * p + (1 - sp) * (1 - p)
    npvDen <- sp * (1 - p) + (1 - se) * p
    ppv <- ifelse(ppvDen > 0, se * p / ppvDen, NA_real_)
    npv <- ifelse(npvDen > 0, sp * (1 - p) / npvDen, NA_real_)
    undefined <- undefined || anyNA(ppv) || anyNA(npv)

    for (k in seq_len(nrow(counts))) {
      table$addRows(list(marker = decodeColNames(marker),
                         threshold = .rocThresholdCell(counts$threshold[k], digits$digits),
                         sensitivity = se[k], specificity = sp[k],
                         ppv = .rocEmpty(ppv[k]), npv = .rocEmpty(npv[k])),
                    rowNames = if (k == 1) marker else paste0(marker, "_", k))
    }
  }

  .rocAddMarkerFootnotes(table, results)
  if (undefined)
    table$addFootnote(gettext("Empty PPV or NPV cells are undefined (a division by zero)."))
  table$addFootnote(gettext("Thresholds are pROC's: the midpoints between neighbouring observed values, plus -∞ and ∞."))
  table$addFootnote(.rocRuleText(options))
}

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
# A whole number below 1e40 has at most 40 digits (the double 1e40 lies above 10^40). Any other number is m / 2^k, with m odd and k > 0 fractional
# bits. Then a * 10^k = m * 5^k is a whole number that is not a multiple of 10, and its digits are the
# significant digits of a: floor(log10(a)) + k + 1 of them, never more than exp + k + 1. Doubling a is exact,
# and k doublings make it a whole number.
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
# The first number of digits p whose sprintf output reads back gives the shortest length. R's as.numeric is not always correctly
# rounded (as.numeric("9.347302") != JavaScript's), so rarely (13 in 302,730 values) this differs from ICU by one digit.
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
  # Each observed value is converted once. A threshold without a lower (or upper) value is compared with an
  # arbitrary one, and the result is not used.
  if (length(v) == 0)
    v <- 0
  lowerAt <- pmax(below, 1)
  upperAt <- pmin(atOrBelow + 1, length(v))
  observed <- .rocDecimal(v)
  lower <- .rocDecimalAt(observed, lowerAt)
  upper <- .rocDecimalAt(observed, upperAt)
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

.rocRoundedThresholdsText <- function(marker) {
  gettextf("%s: the thresholds are rounded for display. Even 15 significant digits cannot show each of them between the two observed values it separates.",
           decodeColNames(marker))
}

# Plots ----
.rocPlot <- function(jaspResults, options, ready) {
  if (!options[["rocPlot"]])
    return()
  if (!is.null(jaspResults[["rocPlot"]]))
    return()

  plot <- createJaspPlot(title = gettext("ROC Curve"), width = 520, height = 420)
  plot$dependOn(c(.rocDataDependencies, .rocCutoffDependencies, "cutoffTable", "rocPlot"))
  plot$position <- 5
  jaspResults[["rocPlot"]] <- plot

  if (!ready)
    return()

  results <- .rocGetFits(jaspResults)
  curves  <- list()
  points  <- list()
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem))
      next
    label  <- decodeColNames(marker)
    counts <- .rocAllCounts(info)
    se <- counts$tp / info$nPositive
    sp <- counts$tn / info$nNegative
    ord <- order(1 - sp, se)
    curves[[marker]] <- data.frame(marker = label, x = (1 - sp)[ord], y = se[ord])

    # The chosen cut-off is marked only when the cut-off table is on (its criterion is set there).
    if (isTRUE(options[["cutoffTable"]])) {
      chosen <- .rocSelectCutoff(info, options)
      points[[marker]] <- data.frame(marker = label, x = 1 - chosen$tn / info$nNegative,
                                     y = chosen$tp / info$nPositive)
    }
  }

  if (length(curves) == 0) {
    plot$setError(gettext("No marker has an ROC curve. See the footnotes of the AUC table."))
    return()
  }

  curveData <- do.call(rbind, curves)
  labels <- unique(curveData$marker)
  curveData$marker <- factor(curveData$marker, levels = labels)
  breaks <- seq(0, 1, by = 0.2)

  # The chance line is a segment from (0, 0) to (1, 1), so it stays inside the axes.
  p <- ggplot2::ggplot(curveData, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], colour = .data[["marker"]])) +
    ggplot2::annotate("segment", x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed", colour = "grey60") +
    ggplot2::geom_path(linewidth = 1)
  notes <- character(0)
  if (length(points) > 0) {
    pointData <- do.call(rbind, points)
    pointData$marker <- factor(pointData$marker, levels = labels)
    p <- p + ggplot2::geom_point(data = pointData, size = 3, shape = 21, fill = "white", stroke = 1.5)
    notes <- .rocCutoffPointNote(options)
  }
  notes <- c(notes, .rocNoCurveNote(results))
  p <- p +
    jaspGraphs::scale_JASPcolor_discrete(name = gettext("Marker")) +
    jaspGraphs::scale_x_continuous(name = gettext("1 - Specificity"), breaks = breaks, limits = c(0, 1)) +
    jaspGraphs::scale_y_continuous(name = gettext("Sensitivity"), breaks = breaks, limits = c(0, 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = if (length(labels) > 1) "right" else "none")
  if (length(notes) > 0)
    p <- p + .rocPlotNote(notes)

  plot$plotObject <- p
}

# A number as typed: up to 15 significant digits, without trailing zeros (1234.5678 stays 1234.5678).
.rocTypedText <- function(x) {
  sprintf("%.15g", x)
}

# What the circles on the ROC plot mark.
.rocCutoffPointNote <- function(options) {
  if (options[["cutoffCriterion"]] == "threshold")
    return(gettextf("Circles mark each marker's point at the threshold %s.",
                    .rocTypedText(options[["thresholdValue"]])))
  # Only the Youden index and closest to (0, 1) can tie; the fixed criteria break ties (.rocSelectCutoff).
  if (options[["cutoffCriterion"]] %in% c("youden", "closestTopLeft"))
    return(gettextf("Circles mark the cut-off chosen for each marker by the criterion in the cut-off table (%s). Tied cut-offs each get a circle.",
                    .rocCriterionLabel(options)))
  gettextf("Circles mark the cut-off chosen for each marker by the criterion in the cut-off table (%s).",
           .rocCriterionLabel(options))
}

# Step curves of sensitivity and specificity against the threshold, for one marker.
# With pROC's rule (roc_utils_perfs in pROC/R/roc.utils.R), the counts at a threshold t depend only on which
# observed values lie on each side of t, so both curves are flat between neighbouring observed values and
# change only at an observed value. Each flat piece is evaluated at one threshold that gives its counts
# (.rocPieceThresholds: pROC's midpoint thresholds, and -Inf and Inf for the outer pieces). The outer pieces
# run to the ends of the x axis (.rocThresholdAxis).
# Returns a data frame with columns marker, threshold, value and measure, one path per measure. The threshold
# column is on the axis scale: the threshold minus the axis offset. The axis is in attr(, "axis").
.rocThresholdSteps <- function(fitInfo, options, label) {
  v <- sort(unique(c(fitInfo$cases, fitInfo$controls)))
  k <- length(v)
  probes <- .rocPieceThresholds(v, options[["direction"]])
  counts <- do.call(rbind, lapply(probes, function(t) .rocCountsAtThreshold(fitInfo, t, options)))
  se <- counts$tp / fitInfo$nPositive
  sp <- counts$tn / fitInfo$nNegative

  # Path: start of the first piece, then at each observed value the level before and after the jump,
  # then the end of the last piece. The pieces start and end at the axis limits: the first and last axis break,
  # or a value beyond them (.rocThresholdAxis).
  axis <- .rocThresholdAxis(v)
  ends <- axis$limits
  x <- c(ends[1], rep(v - axis$offset, each = 2), ends[2])
  piece <- c(1, as.vector(rbind(seq_len(k), seq_len(k) + 1)), k + 1)
  steps <- rbind(
    data.frame(marker = label, threshold = x, value = se[piece], measure = gettext("Sensitivity")),
    data.frame(marker = label, threshold = x, value = sp[piece], measure = gettext("Specificity"))
  )
  attr(steps, "axis") <- axis
  steps
}

# The x axis of the threshold plot for the sorted unique values v, as in the test characteristics plot of
# jaspLearnBayes (R/LSbinaryclassification.R, .bcFillPlotTestCharacteristics, commit 700b1e7): breaks from
# jaspGraphs::getPrettyAxisBreaks on the range of the data, and the axis runs from the first to the last break.
# When the values span less than 1e-5 of their size, tick labels would need more than about 7 significant
# digits, and ggplot2 would show them alike (1.7e+12 for timestamps) or, below about 1e-13, collapse the axis
# to a line (scales::zero_range). The axis then shows the threshold minus a round offset, and says so.
# Returns list(offset, breaks, limits), on the axis scale (threshold minus offset). The limits are the range
# of the breaks, widened to the values when the cleaned breaks (.rocPrettyBreaks) stop just inside them.
.rocThresholdAxis <- function(v) {
  lim <- range(v)
  offset <- 0
  span <- lim[2] - lim[1]
  size <- max(abs(lim))
  if (is.finite(span) && span > 0 && span < 1e-5 * size) {
    # A multiple of a power of ten at least ten times the span, nearest the middle: the values then lie within
    # a few spans of it. Values this close lie within a factor 2 of each other and of the offset, so the
    # subtraction is exact.
    step <- 10^(ceiling(log10(span)) + 1)
    mid <- lim[1] / 2 + lim[2] / 2
    offset <- round(mid / step) * step
    # Near the largest double that multiple can round past it to Inf. Then round toward zero instead, and if
    # that also fails, use the smallest value.
    if (!is.finite(offset))
      offset <- trunc(mid / step) * step
    if (!is.finite(offset))
      offset <- lim[1]
    # The axis title states the offset with 15 significant digits (.rocOffsetText). The number subtracted is
    # that stated number, so the title is exact. Rounding to 15 digits can pass the largest double; rounding
    # to 14 cannot, because the largest double, 1.7976931348623157e308, rounds down at 14 digits.
    stated <- as.numeric(format(offset, digits = 15))
    offset <- if (is.finite(stated)) stated else as.numeric(format(offset, digits = 14))
  }
  breaks <- .rocPrettyBreaks(lim - offset)
  list(offset = offset, breaks = breaks, limits = range(breaks, lim - offset))
}

# The axis title or panel note for an offset: "Threshold − 1e+15". format() picks the shorter of the fixed and
# the scientific form, so 1.7e12 shows as 1.7e+12.
.rocOffsetText <- function(offset) {
  if (offset > 0)
    gettextf("Threshold − %s", format(offset, digits = 15))
  else
    gettextf("Threshold + %s", format(-offset, digits = 15))
}

# One threshold for each of the k + 1 pieces cut by the sorted unique values v: below v[1], between neighbours,
# above v[k]. Every threshold gives the counts of the open interval between its two neighbouring values.
# This follows pROC's roc_utils_thresholds (R/roc.utils.R): the midpoint, computed as a / 2 + b / 2 when
# (a + b) / 2 is above 1e100 in size or overflows. When two values are a few units in the last place apart, the
# midpoint can round onto one of them. pROC then moves it to the value that gives the same counts as the open
# interval: for "higher" (case positive when x >= t) the upper value, for "lower" (case positive when x <= t)
# the lower value. Here the same fix applies to any midpoint that is not strictly between its neighbours.
.rocPieceThresholds <- function(v, direction) {
  k <- length(v)
  if (k < 2)
    return(c(-Inf, Inf))
  a <- v[-k]
  b <- v[-1]
  mid <- (a + b) / 2
  huge <- !is.finite(mid) | abs(mid) > 1e100
  mid[huge] <- a[huge] / 2 + b[huge] / 2
  off <- !(mid > a & mid < b)
  mid[off] <- if (direction == "lower") a[off] else b[off]
  c(-Inf, mid, Inf)
}

.rocThresholdPlot <- function(jaspResults, options, ready) {
  if (!options[["thresholdPlot"]])
    return()
  if (!is.null(jaspResults[["thresholdPlot"]]))
    return()

  nMarkers <- max(1, length(.rocMarkers(options)))
  plot <- createJaspPlot(title = gettext("Sensitivity and Specificity by Threshold"), width = 520,
                         height = 80 + 300 * nMarkers)
  plot$dependOn(c(.rocDataDependencies, "thresholdPlot"))
  plot$position <- 6
  jaspResults[["thresholdPlot"]] <- plot

  if (!ready)
    return()

  results <- .rocGetFits(jaspResults)
  parts <- list()
  axes  <- list()
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem))
      next
    parts[[marker]] <- .rocThresholdSteps(info, options, decodeColNames(marker))
    axes[[marker]]  <- attr(parts[[marker]], "axis")
  }

  if (length(parts) == 0) {
    plot$setError(gettext("No marker has an ROC curve. See the footnotes of the AUC table."))
    return()
  }

  # With more than one marker each gets a full-width panel, one below the other (free x scales), so every panel
  # has room for the same pretty breaks as a single plot. A panel with an offset says so in its title.
  faceted <- length(parts) > 1
  panelLabels <- vapply(names(parts), function(marker) {
    label <- decodeColNames(marker)
    if (faceted && axes[[marker]]$offset != 0)
      label <- gettextf("%1$s (%2$s)", label, .rocOffsetText(axes[[marker]]$offset))
    label
  }, "")
  for (marker in names(parts))
    parts[[marker]]$marker <- panelLabels[[marker]]
  plotData <- do.call(rbind, parts)
  plotData$marker <- factor(plotData$marker, levels = unname(panelLabels))
  breaksList <- lapply(axes, `[[`, "breaks")

  # Each curve is a path with flat pieces between observed values and vertical jumps at them. The x axis of each
  # panel runs from its first to its last pretty break, or to a value just beyond (.rocThresholdAxis), and the
  # paths end there. The legend sits below the plot with a right margin on each label, and geom_rangeframe and
  # themeJaspRaw follow the test characteristics plot of jaspLearnBayes (R/LSbinaryclassification.R,
  # .bcPlotTestCharacteristics and .bcFillPlotTestCharacteristics, commit 700b1e7). A right plot margin leaves
  # room for half of the last tick label, at the right edge of the panel.
  xTitle <- gettext("Threshold")
  if (!faceted && axes[[1]]$offset != 0)
    xTitle <- .rocOffsetText(axes[[1]]$offset)
  xScale <- if (faceted) {
    # With free x scales, ggplot2 asks for the breaks of each panel by its range, which is the range of its
    # breaks, so .rocBreaksFor finds the panel's own breaks again.
    ggplot2::scale_x_continuous(name = xTitle, breaks = function(lim) .rocBreaksFor(lim, breaksList))
  } else {
    ggplot2::scale_x_continuous(name = xTitle, breaks = breaksList[[1]], limits = axes[[1]]$limits)
  }

  p <- ggplot2::ggplot(plotData, ggplot2::aes(x = .data[["threshold"]], y = .data[["value"]],
                                              colour = .data[["measure"]])) +
    ggplot2::geom_path(linewidth = 1) +
    jaspGraphs::scale_JASPcolor_discrete(name = NULL) +
    xScale +
    jaspGraphs::scale_y_continuous(name = gettext("Proportion"), breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "bottom") +
    ggplot2::theme(legend.text = ggplot2::element_text(margin = ggplot2::margin(r = 0.2, unit = "npc")),
                   plot.margin = ggplot2::margin(r = 0.75, unit = "cm"))

  if (faceted)
    p <- p + ggplot2::facet_wrap(~ marker, ncol = 1, scales = "free_x")
  else
    p <- p + ggplot2::ggtitle(levels(plotData$marker))

  note <- .rocNoCurveNote(results)
  if (length(note) > 0)
    p <- p + .rocPlotNote(note)

  plot$plotObject <- p
}

# Pretty axis breaks (jaspGraphs::getPrettyAxisBreaks, which calls pretty(), with its default n = 5). When the
# tick labels would not fit side by side in a panel 520 pixels wide (about 48 characters, with two characters
# of space per label; values such as 1e300 give labels like -3e+300), fewer breaks are asked for. For a range
# near the largest double, pretty() can give infinite breaks; the ends of the range are used then.
.rocPrettyBreaks <- function(lim) {
  for (n in 5:2) {
    breaks <- try(jaspGraphs::getPrettyAxisBreaks(lim, n = n), silent = TRUE)
    if (isTryError(breaks) || length(breaks) < 2 || !all(is.finite(breaks)))
      return(lim)
    breaks <- .rocCleanBreaks(breaks)
    if (sum(nchar(format(breaks, trim = TRUE))) + 2 * length(breaks) <= 48)
      break
  }
  breaks
}

# pretty() can return breaks with rounding noise: for a range from -1e-14 to 1 it gives -1e-14 and
# 0.19999999999999202, which print as "-1e-14" and "2e-01". A break within a millionth of a step of zero
# becomes 0, and the breaks are rounded to the decimals of the step (0.2: one decimal), so they print as plain
# numbers. Breaks with a step of 1 or more are left as they are, apart from the zero.
.rocCleanBreaks <- function(breaks) {
  step <- stats::median(diff(breaks))
  if (!is.finite(step) || step <= 0)
    return(breaks)
  breaks[abs(breaks) < 1e-6 * step] <- 0
  decimals <- -floor(log10(step))
  if (decimals > 0)
    breaks <- round(breaks, decimals)
  breaks + 0  # turns -0 into 0
}

# The breaks, out of a list of break vectors, whose first and last values lie closest to the ends of lim.
.rocBreaksFor <- function(lim, breaksList) {
  distance <- vapply(breaksList, function(b) abs(b[1] - lim[1]) + abs(b[length(b)] - lim[2]), numeric(1))
  breaksList[[which.min(distance)]]
}

# One sentence naming the markers that have no ROC curve, or character(0).
.rocNoCurveNote <- function(results) {
  missing <- Filter(function(marker) !is.null(results$fits[[marker]]$problem), results$markers)
  if (length(missing) == 0)
    return(character(0))
  labels <- paste(vapply(missing, decodeColNames, ""), collapse = ", ")
  sprintf(ngettext(length(missing),
                   "No curve for %s: see the AUC table.",
                   "No curves for %s: see the AUC table."),
          labels)
}

# A note under a plot, left-aligned, as a ggplot2 caption. A plot has no footnotes, so notes go in the caption,
# as in the raincloud plots of jaspDescriptives (R/raincloudPlots.R, commit e3595b5).
.rocPlotNote <- function(lines) {
  text <- paste(vapply(lines, function(x) paste(strwrap(x, width = 60), collapse = "\n"), ""), collapse = "\n")
  list(ggplot2::labs(caption = text),
       ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0)))
}
