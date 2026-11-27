# Smoke tests: the analysis runs without an error. Reference values live in other test files.

# The defaults come from the form (jaspTools reads them from inst/qml/RocCurve.qml), so a new option gets its
# default here without a change to this file.
.rocSmokeOptions <- function(...) {
  options <- jaspTools::analysisOptions("rocCurve")
  options$outcome       <- "outcome"
  options$positiveLevel <- "Poor"
  options$tests         <- list("s100b", "ndka", "wfns")
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
  options <- .rocSmokeOptions(direction = "lower", aucCiMethod = "bootstrap", bootstrapSamples = 200,
                              setSeed = TRUE, cutoffCriterion = "closestTopLeft", prevalence = "custom",
                              comparisonTable = TRUE, coordinatesTable = TRUE, cutoffTable = TRUE,
                              rocPlot = TRUE, thresholdPlot = TRUE)
  results <- jaspTools::runAnalysis("rocCurve", .rocSmokeData(), options, quiet = TRUE)
  expect_identical(results[["status"]], "complete")
})
