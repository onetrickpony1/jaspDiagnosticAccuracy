# Helpers and reference values for test-rocCurve.R. testthat loads helper-*.R files before the tests.
# The reference values come from dev/validation/reference.R; see the note at the top of test-rocCurve.R.

# jaspTools looks up the R function by this name and finds inst/qml/RocCurve.qml through the func entry in
# inst/Description.qml (jaspTools getQMLFile()), so the name is the R function's, not the form's.
analysisName <- "rocCurve"

readTestData <- function(file) {
  df <- utils::read.csv(testthat::test_path(file), stringsAsFactors = FALSE)
  df
}

asahData <- function() {
  df <- readTestData("aSAH.csv")
  df$outcome <- factor(df$outcome, levels = c("Good", "Poor"))
  df$gender  <- factor(df$gender)
  df
}

rocOptions <- function(tests, outcome = "outcome", positiveLevel = "Poor") {
  options <- jaspTools::analysisOptions(analysisName)
  options$outcome       <- outcome
  options$positiveLevel <- positiveLevel
  options$tests         <- tests
  options$cutoffTable   <- TRUE   # off by default (progressive disclosure), but most tests check it
  options$rocPlot       <- FALSE
  options$thresholdPlot <- FALSE
  options
}

# Finds an output element by its name anywhere in the results tree (top level or inside containers,
# where jaspResults names it "<container>_<name>").
findElement <- function(x, name) {
  if (!is.list(x)) return(NULL)
  nms <- names(x)
  if (!is.null(nms)) {
    hit <- which(nms == name | endsWith(nms, paste0("_", name)))
    for (i in hit) if (is.list(x[[i]])) return(x[[i]])
  }
  for (el in x) {
    found <- findElement(el, name)
    if (!is.null(found)) return(found)
  }
  NULL
}

getTableElement <- function(results, name) {
  el <- findElement(results[["results"]], name)
  testthat::expect_false(is.null(el), label = paste("element", name, "exists"))
  el
}

getRows <- function(results, name) getTableElement(results, name)[["data"]]

rowsFor <- function(rows, column, value) Filter(function(r) identical(as.character(r[[column]]), as.character(value)), rows)

rowFor <- function(rows, column, value) {
  hit <- rowsFor(rows, column, value)
  testthat::expect_equal(length(hit), 1L, label = paste0("number of rows with ", column, " = ", value))
  if (length(hit) == 0) return(list())
  hit[[1]]
}

footnoteText <- function(el) {
  fn <- el[["footnotes"]]
  if (is.null(fn) || length(fn) == 0) return("")
  paste(vapply(fn, function(f) paste(as.character(f[["text"]]), collapse = " "), ""), collapse = " | ")
}

# A mixed cell (jaspBase::createMixedRow) arrives as list(value, type, format); a plain cell as its value.
cellValue <- function(x) if (is.list(x) && !is.null(x[["value"]])) x[["value"]] else x

isEmptyCell <- function(x) is.null(x) || length(x) == 0 || identical(x, "") || (is.numeric(x) && is.na(x)) || identical(x, ".")

expectCells <- function(row, expected, tolerance = 1e-6, label = "") {
  for (nm in names(expected)) {
    testthat::expect_false(is.null(row[[nm]]), label = paste(label, nm, "present"))
    if (!is.null(row[[nm]]))
      testthat::expect_equal(as.numeric(cellValue(row[[nm]])), expected[[nm]], tolerance = tolerance, label = paste(label, nm))
  }
}

expectNoError <- function(results) {
  testthat::expect_identical(results[["status"]], "complete", label = "analysis status")
}

expectValidationError <- function(results, pattern = NULL) {
  testthat::expect_true(results[["status"]] %in% c("validationError", "error"), label = "analysis stops with an error")
  if (!is.null(pattern))
    testthat::expect_match(as.character(results[["results"]][["errorMessage"]]), pattern)
}

# Reference rows (dev/validation/reference-values.csv)
refAuc <- list(
  s100b.higher = c(nPositive = 41, nNegative = 72, auc = 0.731368563685637, se = 0.0516592920700373,
                   lower = 0.630118211761618, upper = 0.832618915609656, z = 4.47874050174668, p = 7.50847432494133e-06),
  ndka.higher  = c(nPositive = 41, nNegative = 72, auc = 0.611957994579946, se = 0.0564872600626693,
                   lower = 0.501244999271703, upper = 0.722670989888189, z = 1.98200433965787, p = 0.0474787549469322),
  wfns.higher  = c(nPositive = 41, nNegative = 72, auc = 0.823678861788618, se = 0.0383394667258563,
                   lower = 0.748534887818768, upper = 0.898822835758468, z = 8.44244559015053, p = 3.10766092766436e-17),
  s100b.lower  = c(nPositive = 41, nNegative = 72, auc = 0.268631436314363, se = 0.0516592920700373,
                   lower = 0.167381084390344, upper = 0.369881788238382, z = -4.47874050174668, p = 7.50847432494133e-06),
  ndka.lower   = c(nPositive = 41, nNegative = 72, auc = 0.388042005420054, se = 0.0564872600626693,
                   lower = 0.277329010111811, upper = 0.498755000728297, z = -1.98200433965787, p = 0.0474787549469322),
  wfns.lower   = c(nPositive = 41, nNegative = 72, auc = 0.176321138211382, se = 0.0383394667258563,
                   lower = 0.101177164241532, upper = 0.251465112181232, z = -8.44244559015053, p = 3.10766092766436e-17)
)
