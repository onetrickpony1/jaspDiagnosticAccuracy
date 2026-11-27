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
    gettext("A case is classed as positive when its marker value is at or below the threshold (value \u2264 threshold).")
  else
    gettext("A case is classed as positive when its marker value is at or above the threshold (value \u2265 threshold).")
}

# p columns use type "pvalue", which jaspBase gives the format "dp:3;p:.001" (R/zzzWrappers.R, addColumnInfo,
# commit 5038e41), so a tiny p shows as "< .001". With exact p values turned on in JASP's preferences, a p that
# is 0 in double precision would show as 0, so it gets this footnote.
.rocPZeroText <- function() {
  gettext("p is below the smallest positive number R can store (about 5 \u00d7 10\u207b\u00b3\u00b2\u2074), so it is stored as 0.")
}

.rocEmpty <- function(x) {
  if (length(x) == 0 || is.na(x) || !is.finite(x)) "" else x
}

.rocCriterionLabel <- function(options) {
  switch(options[["cutoffCriterion"]],
    youden           = gettext("Youden"),
    closestTopLeft   = gettext("Closest to (0, 1)"),
    fixedSensitivity = gettextf("Sensitivity \u2265 %s", format(options[["fixedSensitivityValue"]], digits = 6)),
    fixedSpecificity = gettextf("Specificity \u2265 %s", format(options[["fixedSpecificityValue"]], digits = 6)),
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
  bootAucs <- if (bootstrap) .rocBootstrapState(jaspResults, results, options)

  zeroSe <- FALSE
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    row  <- list(marker = decodeColNames(marker), nPositive = info$nPositive, nNegative = info$nNegative,
                 auc = "", se = "", lower = "", upper = "", z = "", p = "")

    if (is.null(info$problem)) {
      row$auc <- info$auc
      if (is.na(info$se)) {
        if (isTRUE(info$seFailed)) {
          table$addFootnote(gettextf("%s: the standard error and the confidence interval could not be computed.",
                                     decodeColNames(marker)), colNames = "se", rowNames = marker)
        } else {
          table$addFootnote(gettextf("%s: the standard error and the confidence interval need at least two cases in each class.",
                                     decodeColNames(marker)), colNames = "se", rowNames = marker)
        }
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

        ci <- .rocAucCi(info, options, bootAucs[[marker]])
        if (is.null(ci)) {
          table$addFootnote(gettextf("%s: the confidence interval could not be computed.", decodeColNames(marker)),
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
                               as.integer(options[["bootstrapSamples"]])))
  else
    table$addFootnote(gettext("The confidence interval uses the DeLong standard error."))
  table$addFootnote(gettext("z and p test AUC = 0.5: a two-sided Wald test with the DeLong standard error."))
  table$addFootnote(.rocDirectionText(results, options))
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
  table$addColumnInfo(name = "lrNegative",       title = gettext("LR\u2212"),    type = "number")
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
    if (nrow(chosen) == 0) {
      table$addRows(list(marker = decodeColNames(marker), criterion = criterion,
                         threshold = jaspBase::createMixedRow("", "string")), rowNames = marker)
      table$addFootnote(gettextf("%s: no threshold meets this criterion.", decodeColNames(marker)),
                        colNames = "threshold", rowNames = marker)
      next
    }
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
      stats <- .rocStatsFromCounts(chosen$tp[k], chosen$fn[k], chosen$tn[k], chosen$fp[k], prevalence,
                                   options[["ciLevel"]])
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
  if (!options[["comparisonTable"]] || length(.rocMarkers(options)) < 2)
    return()
  if (!is.null(jaspResults[["comparisonTable"]]))
    return()

  table <- createJaspTable(title = gettext("Paired Comparison of AUCs"))
  table$dependOn(c(.rocDataDependencies, "comparisonTable", "ciLevel"))
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

  # When no row is left out, the rows of the comparison are each marker's own rows, and a marker with a curve
  # keeps its fit from the AUC table (.rocFitMarker on the same values gives the same fit).
  listwise <- list()
  if (is.null(listwiseProblem)) {
    for (marker in markers) {
      own <- results$fits[[marker]]
      listwise[[marker]] <- if (nRemoved == 0 && is.null(own$problem)) own else
        .rocFitMarker(results$values[[marker]][complete], y[complete], results$outcome$positive,
                      results$outcome$negative, direction,
                      gettextf("%s on the rows used for the comparison", decodeColNames(marker)))
    }
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
      # pROC's paired DeLong test (roc.test), on the placement values of each fit (.rocPairedDelong).
      if (is.null(a$placements) || is.null(b$placements)) {
        problem <- gettext("The comparison could not be computed.")
      } else {
        test <- .rocPairedDelong(a, b, options[["ciLevel"]])
        row$difference <- test$difference
        row$lower <- .rocEmpty(test$lower)
        row$upper <- .rocEmpty(test$upper)
        # The CI is the difference plus or minus a multiple of its SE, so a zero-width CI means SE = 0. pROC
        # then sets z to 0 (p = 1) when the difference is also 0 (delong.paired.test in R/delong.R), which
        # would look like a real test. Leave z and p empty instead.
        zeroSe <- isTRUE(test$lower == test$upper)
        if (zeroSe) {
          problem <- gettext("The standard error of the difference is zero (for example, the two markers order the cases in the same way, or both separate the classes perfectly), so z and p are not defined.")
        } else {
          row$z <- .rocEmpty(test$z)
          row$p <- .rocEmpty(test$p)
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

  # The rows of every marker are built as columns and set in one call: adding rows one at a time costs time in
  # proportion to the rows already in the table, so a table with many thresholds would take minutes.
  parts <- list()
  for (marker in results$markers) {
    info <- results$fits[[marker]]
    if (!is.null(info$problem)) {
      parts[[marker]] <- list(marker = decodeColNames(marker), threshold = list(jaspBase::createMixedRow("", "string")),
                              sensitivity = list(NULL), specificity = list(NULL), ppv = list(NULL), npv = list(NULL),
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

    n <- nrow(counts)
    parts[[marker]] <- list(
      marker      = rep(decodeColNames(marker), n),
      threshold   = .rocThresholdCells(counts$threshold, digits$digits),
      sensitivity = as.list(se),
      specificity = as.list(sp),
      ppv         = lapply(ppv, .rocEmpty),
      npv         = lapply(npv, .rocEmpty),
      # The first row of each marker is named after the marker, so marker footnotes can find it.
      rowNames    = c(marker, paste0(marker, "_", seq_len(n)[-1]))
    )
  }
  .rocSetTableColumns(table, parts)

  .rocAddMarkerFootnotes(table, results)
  if (undefined)
    table$addFootnote(gettext("Empty PPV or NPV cells are undefined (a division by zero)."))
  table$addFootnote(gettext("Thresholds are pROC's: the midpoints between neighbouring observed values, plus -\u221e and \u221e."))
  table$addFootnote(.rocRuleText(options))
}

# Sets the data of a table from a list of parts, each a list of columns plus rowNames. A column is a vector or a
# list with one cell per row (a mixed cell from jaspBase::createMixedRow, a number, "" or NULL). The
# table receives one list of columns (jaspTable$setData), and its rows are named from rowNames.
.rocSetTableColumns <- function(table, parts) {
  if (length(parts) == 0)
    return()
  columns <- setdiff(names(parts[[1]]), "rowNames")
  data <- lapply(columns, function(col) do.call(c, unname(lapply(parts, function(part) as.list(part[[col]])))))
  names(data) <- columns
  rowNames <- unlist(lapply(parts, `[[`, "rowNames"), use.names = FALSE)
  for (col in columns) {
    cells <- data[[col]]
    data[[col]] <- if (inherits(cells[[1]], "mixed")) .rocMixedColumn(cells) else cells
  }
  attr(data, "row.names") <- rowNames
  table$setData(data)
}

# One mixed column from a list of mixed cells (each from jaspBase::createMixedRow).
.rocMixedColumn <- function(cells) {
  values  <- lapply(cells, function(cell) unclass(cell)[[1]][["value"]])
  types   <- vapply(cells, function(cell) unclass(cell)[[1]][["type"]], "")
  formats <- lapply(cells, function(cell) unclass(cell)[[1]][["format"]])
  jaspBase::createMixedColumn(values, types, formats)
}
