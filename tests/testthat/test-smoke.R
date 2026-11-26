# Smoke tests: the analysis runs without an error. Reference values live in other test files.

.rocSmokeOptions <- function(...) {
  options <- list(
    outcome = "outcome", positiveLevel = "Poor", tests = list("s100b", "ndka", "wfns"),
    direction = "higher", aucCiMethod = "delong", bootstrapReplicates = 2000, setSeed = FALSE, seed = 1,
    ciLevel = 0.95, cutoffCriterion = "youden", fixedSensitivityValue = 0.9, fixedSpecificityValue = 0.9,
    thresholdValue = 0, prevalence = "sample", prevalenceValue = 0.1, compareMarkers = FALSE,
    coordinatesTable = FALSE, cutoffTable = FALSE, rocPlot = FALSE, thresholdPlot = FALSE
  )
  changes <- list(...)
  options[names(changes)] <- changes
  options
}

.rocSmokeData <- function() {
  aSAH <- NULL
  utils::data("aSAH", package = "pROC", envir = environment())
  data.frame(outcome = factor(as.character(aSAH$outcome)), s100b = aSAH$s100b, ndka = aSAH$ndka,
             wfns = as.numeric(aSAH$wfns))
}

test_that("ROC Curve runs with the default options", {
  results <- jaspTools::runAnalysis("rocCurve", .rocSmokeData(), .rocSmokeOptions(), quiet = TRUE)
  expect_identical(results[["status"]], "complete")
})

test_that("ROC Curve runs with every table and plot turned on", {
  options <- .rocSmokeOptions(direction = "lower", aucCiMethod = "bootstrap", bootstrapReplicates = 200,
                              setSeed = TRUE, cutoffCriterion = "closestTopLeft", prevalence = "custom",
                              compareMarkers = TRUE, coordinatesTable = TRUE, cutoffTable = TRUE,
                              rocPlot = TRUE, thresholdPlot = TRUE)
  results <- jaspTools::runAnalysis("rocCurve", .rocSmokeData(), options, quiet = TRUE)
  expect_identical(results[["status"]], "complete")
})
