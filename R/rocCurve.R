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

  if (ready) {
    .rocCheckOptions(options)
    .rocComputeFits(jaspResults, dataset, options)
  }

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
    deps <- c(deps, "bootstrapSamples", "setSeed")
    if (isTRUE(options[["setSeed"]]))
      deps <- c(deps, "seed")
  }
  deps
}

# Option checks ----
# The form keeps every value within the ranges of dev/interface.md, but options can also come from a saved file
# or from R. A value outside its range stops the analysis with a message that names the field as the form
# shows it. Only the values that the chosen output uses are checked.
.rocIsNumber <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x)
}

.rocInOpenUnit <- function(x) {
  .rocIsNumber(x) && x > 0 && x < 1
}

.rocCheckOptions <- function(options) {
  if (!.rocInOpenUnit(options[["ciLevel"]]))
    .quitAnalysis(gettextf("The confidence level must lie between 0%% and 100%%, not at either end."))

  if (options[["aucCiMethod"]] == "bootstrap") {
    reps <- options[["bootstrapSamples"]]
    if (!.rocIsNumber(reps) || reps != round(reps) || reps < 100 || reps > 100000)
      .quitAnalysis(gettext("The number of bootstrap samples must be a whole number from 100 to 100,000."))
  }

  if (isTRUE(options[["cutoffTable"]])) {
    criterion <- options[["cutoffCriterion"]]
    if (criterion == "fixedSensitivity" && !.rocInOpenUnit(options[["fixedSensitivityValue"]]))
      .quitAnalysis(gettext("The value for fixed sensitivity must lie between 0 and 1, not at either end."))
    if (criterion == "fixedSpecificity" && !.rocInOpenUnit(options[["fixedSpecificityValue"]]))
      .quitAnalysis(gettext("The value for fixed specificity must lie between 0 and 1, not at either end."))
    if (criterion == "threshold" && !.rocIsNumber(options[["thresholdValue"]]))
      .quitAnalysis(gettext("The threshold must be a number."))
  }

  if ((isTRUE(options[["cutoffTable"]]) || isTRUE(options[["coordinatesTable"]])) &&
      options[["prevalence"]] == "custom" && !.rocInOpenUnit(options[["prevalenceValue"]]))
    .quitAnalysis(gettext("The custom prevalence must lie between 0 and 1, not at either end."))
}

# Data preparation ----
# JASP treats an empty cell as missing (jasp-desktop CommonData/emptyvalues.cpp, EmptyValues::isEmptyValue:
# an empty string is always an empty value), so an empty-string outcome level counts as missing here too.
.rocOutcomeFactor <- function(y) {
  if (is.factor(y)) {
    levels(y)[levels(y) == ""] <- NA
  } else {
    if (is.character(y))
      y[y == ""] <- NA
    y <- factor(y)
  }
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

# Bootstrap replicates ----
# The replicate AUCs of every marker that gets a bootstrap interval (a list by marker; an element is an error
# when the bootstrap failed). They are kept in a state, so a new confidence level or a switch to DeLong and back
# does not draw them again. They depend on the data, the replicates and the seed, not on the level. The seed is
# set again before each marker, as before, so the replicates of a marker are those of a run without the state.
.rocBootstrapState <- function(jaspResults, results, options) {
  if (!is.null(jaspResults[["rocBootState"]]))
    return(jaspResults[["rocBootState"]]$object)

  markers <- Filter(function(marker) {
    info <- results$fits[[marker]]
    is.null(info$problem) && !is.na(info$se)
  }, results$markers)
  if (length(markers) > 0)
    startProgressbar(length(markers) * .rocBootTicks, gettext("Bootstrapping the AUC"))

  aucs <- list()
  for (marker in markers) {
    fit <- results$fits[[marker]]$fit
    .setSeedJASP(options)
    aucs[[marker]] <- try(.rocBootstrapAucs(fit$cases, fit$controls, fit$direction, options[["bootstrapSamples"]],
                                            tick = TRUE),
                          silent = TRUE)
  }

  state <- createJaspState(aucs)
  deps <- c(.rocDataDependencies, "bootstrapSamples", "setSeed")
  if (isTRUE(options[["setSeed"]]))
    deps <- c(deps, "seed")
  state$dependOn(deps)
  jaspResults[["rocBootState"]] <- state
  aucs
}
