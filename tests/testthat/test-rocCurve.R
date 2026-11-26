context("ROC Curve")

# Reference values come from dev/validation/reference.R (hand-coded DeLong and confusion counts in base R,
# checked against pROC 1.19.0.1, its own test files, ROCR 1.0-11, Hanley & McNeil (1982) and the SAS
# reproduction of the DeLong et al. (1988) example). See dev/validation/README.md for every source.
# Option, element and column names follow dev/interface.md.
#
# Tests marked "ASSUMPTION" depend on behaviour that dev/interface.md does not fix exactly; the note next
# to each says what is assumed.

# ---------------------------------------------------------------------------------------------- helpers

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
  options$cutoffTable   <- TRUE   # opt-in since 2026-11-26 (progressive disclosure); most tests check it
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

# ---------------------------------------------------------------------------------------------- AUC table

test_that("AUC table matches pROC and the hand-coded DeLong (aSAH, higher values positive)", {
  options <- rocOptions(c("s100b", "ndka", "wfns"))
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  rows <- getRows(results, "aucTable")
  expect_equal(length(rows), 3L)
  for (m in c("s100b", "ndka", "wfns"))
    expectCells(rowFor(rows, "marker", m), refAuc[[paste0(m, ".higher")]], label = m)
})

test_that("AUC table with lower values indicating positive gives 1 - AUC and the same SE", {
  options <- rocOptions(c("s100b", "ndka", "wfns"))
  options$direction <- "lower"
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  rows <- getRows(results, "aucTable")
  for (m in c("s100b", "ndka", "wfns"))
    expectCells(rowFor(rows, "marker", m), refAuc[[paste0(m, ".lower")]], label = m)
})

test_that("AUC table footnotes name the direction and the CI method", {
  # ASSUMPTION: the footnote words. dev/interface.md says a footnote names the direction and the CI method;
  # the test only looks for the words "higher"/"lower" and "DeLong".
  options <- rocOptions("s100b")
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  fn <- footnoteText(getTableElement(results, "aucTable"))
  expect_match(fn, "higher", ignore.case = TRUE)
  expect_match(fn, "DeLong", ignore.case = TRUE)

  options$direction <- "lower"
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expect_match(footnoteText(getTableElement(results, "aucTable")), "lower", ignore.case = TRUE)
})

test_that("AUC CI at level 0.90 matches pROC", {
  options <- rocOptions("s100b")
  options$ciLevel <- 0.90
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  row <- rowFor(getRows(results, "aucTable"), "marker", "s100b")
  expectCells(row, c(auc = 0.731368563685637, se = 0.0516592920700373,
                     lower = 0.64639658975857, upper = 0.816340537612704))
})

test_that("Hanley & McNeil (1982) CT ratings: AUC equals their W = 0.893", {
  # Table I (p. 30) counts; W = 2642 / (58 * 51) (Table II, p. 31). Their SE (0.032) uses their own
  # formula; the module reports the DeLong SE (0.0307), checked against pROC.
  df <- readTestData("hanleyMcNeil1982.csv")
  df$status <- factor(df$status, levels = c("abnormal", "normal"))
  options <- rocOptions("rating", outcome = "status", positiveLevel = "abnormal")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  row <- rowFor(getRows(results, "aucTable"), "marker", "rating")
  expectCells(row, c(nPositive = 51, nNegative = 58, auc = 2642 / (58 * 51), se = 0.0307244083794146,
                     lower = 0.832952327657925, upper = 0.953389795398409, z = 12.7967008077653))
  expect_equal(round(as.numeric(row[["auc"]]), 3), 0.893)
})

test_that("DeLong et al. (1988) example via SAS: AUC, SE and CI match SAS to 4 decimals (first level positive)", {
  # popind = 0 is the event (SAS: event='0'), the first level, so this also checks a non-default positive level.
  df <- readTestData("delongOvarian.csv")
  df$popind <- factor(df$popind, levels = c("0", "1"))
  options <- rocOptions(c("alb", "totscore", "tp"), outcome = "popind", positiveLevel = "0")
  options$direction <- "lower"
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  rows <- getRows(results, "aucTable")
  sas <- list(alb = c(0.7366, 0.0927, 0.5549, 0.9182), totscore = c(0.7258, 0.1028, 0.5243, 0.9273),
              tp = c(0.6478, 0.1000, 0.4518, 0.8439))
  for (m in names(sas)) {
    row <- rowFor(rows, "marker", m)
    got <- round(as.numeric(c(row[["auc"]], row[["se"]], row[["lower"]], row[["upper"]])), 4)
    expect_equal(got, sas[[m]], label = paste(m, "vs SAS Output 53.8.9"))
    expectCells(row, c(nPositive = 12, nNegative = 31))
  }
  expectCells(rowFor(rows, "marker", "alb"), c(auc = 0.736559139784946, se = 0.0926943460524027,
                                               z = 2.55203418395349, p = 0.0107096010579456))
})

# ---------------------------------------------------------------------------------------------- comparison table

test_that("Paired DeLong comparison matches roc.test and pROC's own test values (aSAH)", {
  # ASSUMPTION: with markers s100b, ndka, wfns the table has the three pairs in the order given
  # ((1,2), (1,3), (2,3)), difference = first - second.
  options <- rocOptions(c("s100b", "ndka", "wfns"))
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  rows <- getRows(results, "comparisonTable")
  expect_equal(length(rows), 3L)
  pick <- function(a, b) { hit <- Filter(function(r) r$marker1 == a && r$marker2 == b, rows); expect_equal(length(hit), 1L); hit[[1]] }
  expectCells(pick("s100b", "ndka"), c(n = 113, difference = 0.119410569105691, lower = -0.0488706064228094,
                                       upper = 0.2876917446341914, z = 1.39077002573558, p = 0.164295175223054))
  expectCells(pick("s100b", "wfns"), c(n = 113, difference = -0.0923102981029811, lower = -0.174214419249478,
                                       upper = -0.0104061769564846, z = -2.20898359144091, p = 0.0271757822291882))
  expectCells(pick("ndka", "wfns"), c(n = 113, difference = -0.211720867208672, lower = -0.3600405634833566,
                                      upper = -0.0634011709339876, z = -2.79777591868904, p = 0.00514557970691098))
})

test_that("Paired comparison, lower direction, matches the SAS DeLong contrasts (DeLong et al. 1988 data)", {
  df <- readTestData("delongOvarian.csv")
  df$popind <- factor(df$popind, levels = c("0", "1"))
  options <- rocOptions(c("alb", "totscore", "tp"), outcome = "popind", positiveLevel = "0")
  options$direction <- "lower"
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  rows <- getRows(results, "comparisonTable")
  pick <- function(a, b) { hit <- Filter(function(r) r$marker1 == a && r$marker2 == b, rows); expect_equal(length(hit), 1L); hit[[1]] }
  # SAS Output 53.8.12: Albumin - K-G Score = 0.0108 (CI -0.1761, 0.1976), Chi-square 0.0127
  r1 <- pick("alb", "totscore")
  expectCells(r1, c(n = 43, difference = 0.010752688172043, lower = -0.176117368734695, upper = 0.197622745078781,
                    z = 0.112778269044526, p = 0.910206347201035))
  expect_equal(round(as.numeric(c(r1$difference, r1$lower, r1$upper)), 4), c(0.0108, -0.1761, 0.1976))
  # SAS: Total Protein - K-G Score = -0.0780 (CI -0.2830, 0.1271); here the pair is (totscore, tp), so the sign flips
  r2 <- pick("totscore", "tp")
  expectCells(r2, c(difference = 0.0779569892473119, lower = -0.127074037077776, upper = 0.282988015572399,
                    z = 0.745218389657994, p = 0.456139707507473))
  expectCells(pick("alb", "tp"), c(difference = 0.0887096774193548, lower = -0.0206391109204, upper = 0.198058465759,
                                   z = 1.59002925832, p = 0.111828210121), tolerance = 1e-5)
})

test_that("Missing values: per-marker complete cases in the AUC table, listwise in the comparison table", {
  df <- asahData()
  df$ndka[1:5] <- NA; df$s100b[6:8] <- NA; df$outcome[9] <- NA
  options <- rocOptions(c("s100b", "ndka"))
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  rows <- getRows(results, "aucTable")
  expectCells(rowFor(rows, "marker", "s100b"), c(nPositive = 39, nNegative = 70, auc = 0.745787545787546,
                                                 se = 0.0524284631977964, lower = 0.643029646155081, upper = 0.84854544542001))
  expectCells(rowFor(rows, "marker", "ndka"), c(nPositive = 40, nNegative = 67, auc = 0.59589552238806,
                                                se = 0.0582826716286214, lower = 0.481663585073187, upper = 0.710127459702932))
  cmp <- getRows(results, "comparisonTable")
  expect_equal(length(cmp), 1L)
  expectCells(cmp[[1]], c(n = 104, difference = 0.15829346092504, lower = -0.0190855602361505,
                          upper = 0.33567248208623, z = 1.7490765275976, p = 0.0802777919805899))
  # ASSUMPTION: the footnote gives the number of removed cases as a digit (113 - 104 = 9 rows)
  expect_match(footnoteText(getTableElement(results, "comparisonTable")), "9")
})

# ---------------------------------------------------------------------------------------------- cut-off table

cutRef <- list(
  s100b.youden = c(threshold = 0.205, sensitivity = 0.634146341463415, sensitivityLower = 0.469362548032833,
                   sensitivityUpper = 0.778772137938935, specificity = 0.805555555555556, specificityLower = 0.695331066701317,
                   specificityUpper = 0.88941621332151, ppv = 0.65, npv = 0.794520547945206, lrPositive = 3.26132404181185,
                   lrNegative = 0.454163162321278, accuracy = 0.743362831858407, youden = 0.43970189701897),
  wfns.youden  = c(threshold = 3.5, sensitivity = 0.634146341463415, sensitivityLower = 0.469362548032833,
                   sensitivityUpper = 0.778772137938935, specificity = 0.833333333333333, specificityLower = 0.726960721088069,
                   specificityUpper = 0.910803593991874, ppv = 0.684210526315789, npv = 0.8, lrPositive = 3.80487804878049,
                   lrNegative = 0.439024390243902, accuracy = 0.761061946902655, youden = 0.467479674796748),
  ndka.youden  = c(threshold = 11.08, sensitivity = 0.707317073170732, sensitivityLower = 0.544626016834637,
                   sensitivityUpper = 0.838701430285692, specificity = 0.513888888888889, specificityLower = 0.393100046479662,
                   specificityUpper = 0.633499721180767, ppv = 0.453125, npv = 0.755102040816326, lrPositive = 1.45505226480836,
                   lrNegative = 0.569545154911009, accuracy = 0.584070796460177, youden = 0.221205962059621),
  wfns.closestTopLeft = c(threshold = 2.5, sensitivity = 0.658536585365854, sensitivityLower = 0.494052526394267,
                   sensitivityUpper = 0.799165886282918, specificity = 0.791666666666667, specificityLower = 0.679770488870628,
                   specificityUpper = 0.878448797092192, ppv = 0.642857142857143, npv = 0.802816901408451,
                   lrPositive = 3.1609756097561, lrNegative = 0.431322207958922, accuracy = 0.743362831858407, youden = 0.45020325203252),
  s100b.fixedSensitivity = c(threshold = 0.075, sensitivity = 0.902439024390244, sensitivityLower = 0.768685450690539,
                   sensitivityUpper = 0.972772533201639, specificity = 0.222222222222222, specificityLower = 0.132682384787841,
                   specificityUpper = 0.335636627854693, ppv = 0.397849462365591, npv = 0.8, lrPositive = 1.1602787456446,
                   lrNegative = 0.439024390243902, accuracy = 0.469026548672566, youden = 0.124661246612466),
  s100b.fixedSpecificity = c(threshold = 0.435, sensitivity = 0.390243902439024, sensitivityLower = 0.242010967178689,
                   sensitivityUpper = 0.554952176629311, specificity = 0.902777777777778, specificityLower = 0.809889659554441,
                   specificityUpper = 0.960010499066397, ppv = 0.695652173913044, npv = 0.722222222222222,
                   lrPositive = 4.01393728222997, lrNegative = 0.675422138836773, accuracy = 0.716814159292035, youden = 0.293021680216802),
  wfns.threshold4.5 = c(threshold = 4.5, sensitivity = 0.439024390243902, sensitivityLower = 0.284687076807926,
                   sensitivityUpper = 0.602501562619241, specificity = 0.944444444444444, specificityLower = 0.863821350687076,
                   specificityUpper = 0.984657462888869, ppv = 0.818181818181818, npv = 0.747252747252747,
                   lrPositive = 7.90243902439024, lrNegative = 0.593974175035868, accuracy = 0.761061946902655, youden = 0.383468834688347),
  wfns.lower.threshold2 = c(threshold = 2, sensitivity = 0.341463414634146, sensitivityLower = 0.200834113717082,
                   sensitivityUpper = 0.505947473605733, specificity = 0.208333333333333, specificityLower = 0.121551202907808,
                   specificityUpper = 0.320229511129372, ppv = 0.197183098591549, npv = 0.357142857142857,
                   lrPositive = 0.431322207958922, lrNegative = 3.1609756097561, accuracy = 0.256637168141593, youden = -0.45020325203252)
)

test_that("Cut-off table, Youden criterion, matches the confusion counts and binom.test CIs", {
  options <- rocOptions(c("s100b", "ndka", "wfns"))
  options$cutoffCriterion <- "youden"
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  rows <- getRows(results, "cutoffTable")
  expect_equal(length(rows), 3L)
  for (m in c("s100b", "ndka", "wfns"))
    expectCells(rowFor(rows, "marker", m), cutRef[[paste0(m, ".youden")]], label = m)
})

test_that("Cut-off table, closest to (0, 1), differs from Youden for wfns", {
  options <- rocOptions("wfns")
  options$cutoffCriterion <- "closestTopLeft"
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "wfns"), cutRef$wfns.closestTopLeft)
})

test_that("Cut-off table, fixed sensitivity 0.9", {
  options <- rocOptions("s100b")
  options$cutoffCriterion <- "fixedSensitivity"
  options$fixedSensitivityValue <- 0.9
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "s100b"), cutRef$s100b.fixedSensitivity)
})

test_that("Cut-off table, fixed specificity 0.9", {
  options <- rocOptions("s100b")
  options$cutoffCriterion <- "fixedSpecificity"
  options$fixedSpecificityValue <- 0.9
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "s100b"), cutRef$s100b.fixedSpecificity)
})

test_that("Cut-off table, user threshold: 4.5 for wfns (the cut-off in Robin et al. 2011)", {
  options <- rocOptions("wfns")
  options$cutoffCriterion <- "threshold"
  options$thresholdValue <- 4.5
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "wfns"), cutRef$wfns.threshold4.5)
})

test_that("Cut-off table, user threshold equal to an observed value counts that value as positive", {
  # Rule from pROC man/roc.Rd (v1.19.0.1, lines 83-87): positive when >= the threshold for "higher", <= for "lower":
  # wfns >= 4 is the same split as wfns >= 3.5; with "lower", wfns <= 2 is the same split as wfns <= 2.5.
  options <- rocOptions("wfns")
  options$cutoffCriterion <- "threshold"
  options$thresholdValue <- 4
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  ref <- cutRef$wfns.youden; ref[["threshold"]] <- 4
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "wfns"), ref)

  options$direction <- "lower"
  options$thresholdValue <- 2
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "wfns"), cutRef$wfns.lower.threshold2)
})

test_that("Cut-off table with a custom prevalence changes only PPV and NPV", {
  options <- rocOptions("s100b")
  options$prevalence <- "custom"
  options$prevalenceValue <- 0.1
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  ref <- cutRef$s100b.youden
  ref[["ppv"]] <- 0.265984654731458
  ref[["npv"]] <- 0.951961569255404
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "s100b"), ref)
})

test_that("Cut-off table CIs at level 0.90 are Clopper-Pearson at 0.90", {
  options <- rocOptions("s100b")
  options$ciLevel <- 0.90
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "s100b"),
              c(sensitivityLower = 0.493875690387087, sensitivityUpper = 0.759191040250843,
                specificityLower = 0.712844343059353, specificityUpper = 0.878441994872762))
})

test_that("Cut-off table, lower direction, Youden for s100b: LR+ undefined is an empty cell with a footnote", {
  # Specificity is 1 at the best threshold (0.035), so LR+ = se / 0 is undefined (dev/interface.md).
  options <- rocOptions("s100b")
  options$direction <- "lower"
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  el <- getTableElement(results, "cutoffTable")
  row <- rowFor(el[["data"]], "marker", "s100b")
  expectCells(row, c(threshold = 0.035, sensitivity = 0.024390243902439, specificity = 1, ppv = 1,
                     npv = 0.642857142857143, lrNegative = 0.975609756097561, accuracy = 0.646017699115044,
                     youden = 0.024390243902439, sensitivityLower = 0.000617316893286519, sensitivityUpper = 0.128554020384146,
                     specificityLower = 0.950055916294145, specificityUpper = 1))
  expect_true(isEmptyCell(row[["lrPositive"]]), label = "LR+ cell is empty")
  expect_true(nchar(footnoteText(el)) > 0, label = "cut-off table has a footnote")
})

test_that("Tied optimal thresholds give one row each, with a footnote (Youden and closest to (0, 1))", {
  # x = 1, 2, 3, 4 with cases at 2 and 4. Hand counts at pROC's thresholds (higher values positive):
  #   t      -Inf   1.5   2.5   3.5   Inf
  #   se      1     1     0.5   0.5   0
  #   sp      0     0.5   0.5   1     1
  # Youden J = se + sp - 1: 0, 0.5, 0, 0.5, 0, so 1.5 and 3.5 tie at 0.5.
  # Closest to (0, 1), (1 - se)^2 + (1 - sp)^2: 1, 0.25, 0.5, 0.25, 1, so 1.5 and 3.5 tie at 0.25.
  df <- data.frame(outcome = factor(c("neg", "pos", "neg", "pos"), levels = c("neg", "pos")), x = c(1, 2, 3, 4))
  for (criterion in c("youden", "closestTopLeft")) {
    options <- rocOptions("x", positiveLevel = "pos")
    options$cutoffCriterion <- criterion
    results <- jaspTools::runAnalysis(analysisName, df, options)
    expectNoError(results)
    el <- getTableElement(results, "cutoffTable")
    rows <- rowsFor(el[["data"]], "marker", "x")
    expect_equal(length(rows), 2L, label = paste(criterion, "rows"))
    rows <- rows[order(vapply(rows, function(r) as.numeric(cellValue(r[["threshold"]])), 0))]
    expectCells(rows[[1]], c(threshold = 1.5, sensitivity = 1, specificity = 0.5, youden = 0.5), label = criterion)
    expectCells(rows[[2]], c(threshold = 3.5, sensitivity = 0.5, specificity = 1, youden = 0.5), label = criterion)
    expect_match(footnoteText(el), "x: 2 thresholds are equally good under this criterion. Each has its own row.",
                 fixed = TRUE, label = criterion)
  }
})

test_that("Ties at the ends: wfns with lower direction has J = 0 at -Inf and Inf only", {
  # ASSUMPTION: -Inf and Inf are candidate thresholds (dev/interface.md: thresholds include them). Every finite
  # wfns threshold has J < 0 in this direction, so the two infinite ones tie.
  options <- rocOptions("wfns")
  options$direction <- "lower"
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  rows <- getRows(results, "cutoffTable")
  expect_equal(length(rows), 2L)
  se <- sort(vapply(rows, function(r) as.numeric(r[["sensitivity"]]), 0))
  expect_equal(se, c(0, 1))
  for (r in rows) expect_equal(as.numeric(r[["youden"]]), 0)
})

# ---------------------------------------------------------------------------------------------- coordinates table

test_that("Coordinates table lists every pROC threshold with sensitivity, specificity, PPV and NPV (wfns)", {
  options <- rocOptions("wfns")
  options$coordinatesTable <- TRUE
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  rows <- getRows(results, "coordinatesTable")
  # ASSUMPTION: the table includes the -Inf and Inf thresholds (6 rows for wfns: -Inf, 1.5, 2.5, 3.5, 4.5, Inf).
  expect_equal(length(rows), 6L)
  finite <- Filter(function(r) is.finite(suppressWarnings(as.numeric(cellValue(r[["threshold"]])))), rows)
  expect_equal(length(finite), 4L)
  ref <- list(
    "1.5" = c(sensitivity = 0.951219512195122, specificity = 0.513888888888889, ppv = 0.527027027027027, npv = 0.948717948717949),
    "2.5" = c(sensitivity = 0.658536585365854, specificity = 0.791666666666667, ppv = 0.642857142857143, npv = 0.802816901408451),
    "3.5" = c(sensitivity = 0.634146341463415, specificity = 0.833333333333333, ppv = 0.684210526315789, npv = 0.8),
    "4.5" = c(sensitivity = 0.439024390243902, specificity = 0.944444444444444, ppv = 0.818181818181818, npv = 0.747252747252747))
  for (t in names(ref)) {
    hit <- Filter(function(r) isTRUE(all.equal(as.numeric(cellValue(r[["threshold"]])), as.numeric(t))), finite)
    expect_equal(length(hit), 1L, label = paste("row for threshold", t))
    if (length(hit) == 1) expectCells(hit[[1]], ref[[t]], label = paste("threshold", t))
  }
})

# ---------------------------------------------------------------------------------------------- bootstrap

test_that("Bootstrap CI runs, brackets the AUC, keeps the DeLong SE and is reproducible with a seed", {
  options <- rocOptions("s100b")
  options$aucCiMethod <- "bootstrap"
  options$bootstrapReplicates <- 200
  options$setSeed <- TRUE
  options$seed <- 1
  r1 <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(r1)
  row1 <- rowFor(getRows(r1, "aucTable"), "marker", "s100b")
  expectCells(row1, c(auc = 0.731368563685637, se = 0.0516592920700373, z = 4.47874050174668))
  expect_true(as.numeric(row1$lower) < as.numeric(row1$auc) && as.numeric(row1$auc) < as.numeric(row1$upper))
  expect_true(as.numeric(row1$lower) > 0.5 && as.numeric(row1$upper) <= 1)
  expect_match(footnoteText(getTableElement(r1, "aucTable")), "bootstrap", ignore.case = TRUE)
  r2 <- jaspTools::runAnalysis(analysisName, asahData(), options)
  row2 <- rowFor(getRows(r2, "aucTable"), "marker", "s100b")
  expect_equal(as.numeric(c(row2$lower, row2$upper)), as.numeric(c(row1$lower, row1$upper)))
})

# ---------------------------------------------------------------------------------------------- cross-checks

test_that("Fitted probabilities of a one-predictor logistic regression have the marker's AUC", {
  df <- asahData()
  df$fitted <- stats::fitted(stats::glm(outcome ~ s100b, family = stats::binomial, data = df))
  options <- rocOptions(c("s100b", "fitted"))
  results <- jaspTools::runAnalysis(analysisName, df, options)
  rows <- getRows(results, "aucTable")
  expectCells(rowFor(rows, "marker", "fitted"), refAuc$s100b.higher[c("auc", "se", "lower", "upper")])
})

test_that("An ordinal marker gives the same results as its numeric codes", {
  # ASSUMPTION: an ordinal test variable is used through its level order (wfns levels 1 < ... < 5).
  df <- asahData()
  df$wfnsOrd <- factor(df$wfns, levels = 1:5, ordered = TRUE)
  options <- rocOptions("wfnsOrd")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  expectCells(rowFor(getRows(results, "aucTable"), "marker", "wfnsOrd"), refAuc$wfns.higher)
})

test_that("Default positive level is the second level", {
  # ASSUMPTION: the option as analysisOptions() gives it (empty or the second level) leads to "Poor".
  options <- jaspTools::analysisOptions(analysisName)
  options$outcome <- "outcome"
  options$tests <- "s100b"
  options$rocPlot <- FALSE
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  expectCells(rowFor(getRows(results, "aucTable"), "marker", "s100b"), refAuc$s100b.higher)
})

test_that("Plots are made without error", {
  options <- rocOptions(c("s100b", "wfns"))
  options$rocPlot <- TRUE
  options$thresholdPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  for (p in c("rocPlot", "thresholdPlot")) {
    el <- findElement(results[["results"]], p)
    expect_false(is.null(el), label = paste(p, "exists"))
    expect_false(identical(el[["status"]], "error"), label = paste(p, "has no error"))
  }
})

# ---------------------------------------------------------------------------------------------- awkward data

test_that("Constant marker: empty cells and a footnote for that marker, the other marker still runs", {
  df <- asahData()
  df$constant <- 1
  options <- rocOptions(c("s100b", "constant"))
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expectCells(rowFor(el[["data"]], "marker", "s100b"), refAuc$s100b.higher)
  # ASSUMPTION: a constant marker counts as a per-marker problem (dev/interface.md), so its AUC cell is empty
  # rather than 0.5.
  rc <- rowFor(el[["data"]], "marker", "constant")
  expect_true(isEmptyCell(rc[["auc"]]), label = "constant marker AUC is empty")
  expect_true(nchar(footnoteText(el)) > 0)
})

test_that("Marker with no positive cases after removing missing values: per-marker footnote", {
  df <- asahData()
  df$noPositive <- ifelse(df$outcome == "Poor", NA, df$s100b)
  options <- rocOptions(c("s100b", "noPositive"))
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expectCells(rowFor(el[["data"]], "marker", "s100b"), refAuc$s100b.higher)
  expect_true(isEmptyCell(rowFor(el[["data"]], "marker", "noPositive")[["auc"]]))
  expect_true(nchar(footnoteText(el)) > 0)
})

test_that("Marker with every value missing: per-marker footnote, the other marker still runs", {
  # ASSUMPTION: an all-missing marker is a per-marker problem (no cases in either class), not an analysis error.
  df <- asahData()
  df$allMissing <- NA_real_
  options <- rocOptions(c("s100b", "allMissing"))
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expectCells(rowFor(el[["data"]], "marker", "s100b"), refAuc$s100b.higher)
  expect_true(isEmptyCell(rowFor(el[["data"]], "marker", "allMissing")[["auc"]]))
  expect_true(nchar(footnoteText(el)) > 0)
})

test_that("Outcome with one class left after removing missing values stops with an error naming the level", {
  df <- asahData()
  df$outcome[df$outcome == "Poor"] <- NA
  options <- rocOptions("s100b")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectValidationError(results, "Good")
})

test_that("Tied marker values: AUC counts ties as one half and the DeLong SE matches", {
  df <- data.frame(outcome = factor(c("n", "p", "n", "p", "n", "n", "p", "p"), levels = c("n", "p")),
                   x = c(1, 1, 2, 2, 3, 3, 3, 4))
  options <- rocOptions("x", positiveLevel = "p")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  expectCells(rowFor(getRows(results, "aucTable"), "marker", "x"),
              c(nPositive = 4, nNegative = 4, auc = 0.5625, se = 0.228217732293819))
})

test_that("One positive case: the AUC is shown and the analysis does not stop", {
  # The DeLong variance needs at least 2 cases in each class (var() of one placement value is NA).
  # ASSUMPTION: the SE, CI, z and p cells are empty (or NaN) with a footnote rather than an error.
  df <- data.frame(outcome = factor(c("p", "n", "n", "n", "n"), levels = c("n", "p")), x = c(4, 1, 2, 3, 5))
  options <- rocOptions("x", positiveLevel = "p")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  row <- rowFor(el[["data"]], "marker", "x")
  expectCells(row, c(nPositive = 1, nNegative = 4, auc = 0.75))
  expect_true(isEmptyCell(row[["se"]]), label = "SE is empty with one positive case")
  expect_true(nchar(footnoteText(el)) > 0)
})

test_that("Huge values: s100b times 1e12 gives the same AUC, SE and cut-off statistics", {
  df <- asahData()
  df$huge <- df$s100b * 1e12
  options <- rocOptions("huge")
  options$compareMarkers <- FALSE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  expectCells(rowFor(getRows(results, "aucTable"), "marker", "huge"), refAuc$s100b.higher)
  ref <- cutRef$s100b.youden
  ref[["threshold"]] <- 0.205e12
  expectCells(rowFor(getRows(results, "cutoffTable"), "marker", "huge"), ref)
})

test_that("Outcome with 3 levels stops with an error naming the levels", {
  df <- asahData()
  df$three <- factor(c("a1", "b2", "c3")[(seq_len(nrow(df)) %% 3) + 1])
  options <- rocOptions("s100b", outcome = "three", positiveLevel = "b2")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectValidationError(results)
  msg <- as.character(results[["results"]][["errorMessage"]])
  for (l in c("a1", "b2", "c3")) expect_match(msg, l)
})

test_that("Positive level not among the levels present stops with an error", {
  df <- asahData()
  df$outcome3 <- factor(as.character(df$outcome), levels = c("Good", "Poor", "Unknown"))  # "Unknown" unused
  options <- rocOptions("s100b", outcome = "outcome3", positiveLevel = "Unknown")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectValidationError(results)
})

test_that("Empty unused outcome level is dropped: the analysis runs with the two levels present", {
  # ASSUMPTION: levels are counted after dropping unused ones ("exactly 2 levels after removing missing values").
  df <- asahData()
  df$outcome3 <- factor(as.character(df$outcome), levels = c("Good", "Poor", "Unknown"))
  options <- rocOptions("s100b", outcome = "outcome3", positiveLevel = "Poor")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  expectCells(rowFor(getRows(results, "aucTable"), "marker", "s100b"), refAuc$s100b.higher)
})

# ---------------------------------------------------------------------------------------------- trap column names

# In plain jaspTools the column names reach R unencoded, so these tests check that labels are built from the
# option values and that names which collide with JASP's encoded names ("JaspColumn_<n>_Encoded") or with each
# other (m, n, a, code, digits) do not mix up markers. The app's own encoding of column names was checked
# separately, in the running JASP app.

trapRun <- function(map, compare = TRUE) {
  df <- asahData()[, c("outcome", "s100b", "ndka", "wfns")]
  names(df) <- unname(map[names(df)])
  options <- rocOptions(unname(map[c("s100b", "ndka", "wfns")]), outcome = map[["outcome"]])
  options$compareMarkers <- compare
  options$coordinatesTable <- TRUE
  jaspTools::runAnalysis(analysisName, df, options)
}

checkTrap <- function(results, map) {
  expectNoError(results)
  auc <- getRows(results, "aucTable")
  for (m in c("s100b", "ndka", "wfns"))
    expectCells(rowFor(auc, "marker", map[[m]]), refAuc[[paste0(m, ".higher")]], label = paste(m, "as", map[[m]]))
  cut <- getRows(results, "cutoffTable")
  for (m in c("s100b", "ndka", "wfns"))
    expectCells(rowFor(cut, "marker", map[[m]]), cutRef[[paste0(m, ".youden")]], label = paste(m, "as", map[[m]]))
  cmp <- getRows(results, "comparisonTable")
  labels <- sort(unique(unlist(lapply(cmp, function(r) c(r$marker1, r$marker2)))))
  expect_equal(labels, sort(unname(map[c("s100b", "ndka", "wfns")])))
  pick <- function(a, b) Filter(function(r) r$marker1 == a && r$marker2 == b, cmp)[[1]]
  expectCells(pick(map[["s100b"]], map[["wfns"]]), c(difference = -0.0923102981029811, z = -2.20898359144091))
  coordMarkers <- unique(vapply(getRows(results, "coordinatesTable"), function(r) as.character(r$marker), ""))
  coordMarkers <- coordMarkers[nzchar(coordMarkers)]
  expect_true(all(coordMarkers %in% map[c("s100b", "ndka", "wfns")]))
  txt <- paste(unlist(results[["results"]]), collapse = " ")
  expect_false(grepl("JaspColumn_[0-9]+_Encoded", txt), label = "no encoded column name in the output")
}

test_that("Trap names m, n, a and outcome code: labels show the names and markers are not mixed up", {
  map <- c(outcome = "code", s100b = "m", ndka = "n", wfns = "a")
  checkTrap(trapRun(map), map)
})

test_that("Trap names that are prefixes of each other (x, xx, x1)", {
  map <- c(outcome = "x10", s100b = "x", ndka = "xx", wfns = "x1")
  checkTrap(trapRun(map), map)
})

test_that("Digit names (0, 1, 10, 2)", {
  # jaspTools may not handle names that are numbers in plain mode (docs/app-encoding.md, "plainTrap");
  # if this fails inside jaspTools before the analysis runs, it is not a module bug.
  map <- c(outcome = "0", s100b = "1", ndka = "10", wfns = "2")
  checkTrap(trapRun(map), map)
})

# ---------------------------------------------------------------------------------------------- regressions (review, 2026-11-26)

# Every expected value below comes from hand counts, base R or pROC called directly.

test_that("Default options with the variables assigned give the AUC table and nothing else", {
  options <- jaspTools::analysisOptions(analysisName)
  options$outcome       <- "outcome"
  options$positiveLevel <- "Poor"
  options$tests         <- c("s100b", "ndka")
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  outputs <- setdiff(names(results[["results"]]), c(".meta", "title", "errorMessage"))
  outputs <- Filter(function(nm) is.list(results[["results"]][[nm]]), outputs)
  expect_identical(outputs, "aucTable")
})

# Step curve value at x from the path returned by .rocThresholdSteps (flat pieces joined by vertical jumps).
stepValueAt <- function(path, x) {
  i <- which(head(path$threshold, -1) < x & x < tail(path$threshold, -1))
  stopifnot(length(i) == 1)
  path$value[i]
}
stepJumps <- function(path) {
  n <- nrow(path)
  same <- path$threshold[-n] == path$threshold[-1] & path$value[-n] != path$value[-1]
  path$threshold[-n][same]
}

test_that("Threshold plot: steps are flat between observed values and jump exactly at them (uneven spacing)", {
  # x = 1, 2, 10, 11, 30, 31; cases 10, 30, 31; controls 1, 2, 11. pROC's thresholds are 1.5, 6, 10.5, 20.5,
  # 30.5, but the counts only change at observed values. Hand counts, higher values positive
  # (case positive when x >= t, control negative when x < t):
  #   t in (1, 2]: se 3/3, sp 1/3   (2, 10]: 3/3, 2/3   (10, 11]: 2/3, 2/3   (11, 30]: 2/3, 3/3   (30, 31]: 1/3, 3/3
  info <- list(cases = c(10, 30, 31), controls = c(1, 2, 11), nPositive = 3, nNegative = 3)
  steps <- jaspDiagnosticAccuracy:::.rocThresholdSteps(info, list(direction = "higher"), "x")
  se <- steps[steps$measure == "Sensitivity", ]
  sp <- steps[steps$measure == "Specificity", ]
  probes <- c(1.5, 6, 10.5, 20.5, 30.5, 3.75, 8.25, 15.5, 25.5)
  seHand <- c(3, 3, 2, 2, 1, 3, 3, 2, 2) / 3
  spHand <- c(1, 2, 2, 3, 3, 2, 2, 3, 3) / 3
  expect_equal(vapply(probes, stepValueAt, 0, path = se), seHand)
  expect_equal(vapply(probes, stepValueAt, 0, path = sp), spHand)
  expect_equal(sort(stepJumps(se)), c(10, 30, 31))
  expect_equal(sort(stepJumps(sp)), c(1, 2, 11))
  # Outer pieces: below 1 every case is positive and no control negative; above 31 the reverse.
  expect_equal(c(se$value[1], sp$value[1]), c(1, 0))
  expect_equal(c(tail(se$value, 1), tail(sp$value, 1)), c(0, 1))

  # Lower values positive: case positive when x <= t, control negative when x > t.
  #   t in (11, 30): se 1/3 (case 10), sp 0/3.   (2, 10): se 0, sp 1/3 (control 11).
  low <- jaspDiagnosticAccuracy:::.rocThresholdSteps(info, list(direction = "lower"), "x")
  expect_equal(stepValueAt(low[low$measure == "Sensitivity", ], 20), 1 / 3)
  expect_equal(stepValueAt(low[low$measure == "Specificity", ], 20), 0)
  expect_equal(stepValueAt(low[low$measure == "Specificity", ], 5), 1 / 3)
  expect_equal(sort(stepJumps(low[low$measure == "Sensitivity", ])), c(10, 30, 31))
})

test_that("Cut-off selection compares exactly on counts: J values 1e-9 apart are not tied (n = 60001)", {
  # Controls: 29999 at 1 and 1 at 5. Cases: 1 at 5 and 30000 at 10. Hand counts:
  #   threshold 3:   tp 30001, tn 29999: J = 29999 / 30000 - 0     = 1 - 1/30000
  #   threshold 7.5: tp 30000, tn 30000: J = 30000 / 30001 + 1 - 1 = 1 - 1/30001 (higher, by 1.1e-9)
  # Closest to (0, 1): (1/30000)^2 at 3 against (1/30001)^2 at 7.5, so 7.5 again.
  df <- data.frame(outcome = factor(c(rep("n", 30000), rep("p", 30001)), levels = c("n", "p")),
                   x = c(rep(1, 29999), 5, 5, rep(10, 30000)))
  best <- pROC::coords(pROC::roc(df$outcome, df$x, levels = c("n", "p"), direction = "<", quiet = TRUE),
                       "best", best.method = "youden", ret = "threshold", transpose = FALSE)
  expect_equal(nrow(best), 1L)
  expect_equal(best$threshold, 7.5)
  for (criterion in c("youden", "closestTopLeft")) {
    options <- rocOptions("x", positiveLevel = "p")
    options$cutoffCriterion <- criterion
    results <- jaspTools::runAnalysis(analysisName, df, options)
    expectNoError(results)
    rows <- getRows(results, "cutoffTable")
    expect_equal(length(rows), 1L, label = paste(criterion, "number of rows"))
    expectCells(rows[[1]], c(threshold = 7.5, sensitivity = 30000 / 30001, specificity = 1), tolerance = 1e-12,
                label = criterion)
  }
})

test_that("Fixed sensitivity: a sensitivity equal to the value typed counts (7/10 and 0.7)", {
  # Cases 0.5, 0.6, 0.7 and 20 to 26; controls 1 to 10. With sensitivity at least 0.7, the best specificity is
  # 1 at threshold 15 (tp 7, tn 10). Without equality, sensitivity 0.8 would be needed: threshold 0.65, sp 0.
  df <- data.frame(outcome = factor(c(rep("n", 10), rep("p", 10)), levels = c("n", "p")),
                   x = c(1:10, 0.5, 0.6, 0.7, 20:26))
  options <- rocOptions("x", positiveLevel = "p")
  options$cutoffCriterion <- "fixedSensitivity"
  options$fixedSensitivityValue <- 0.7
  results <- jaspTools::runAnalysis(analysisName, df, options)
  rows <- getRows(results, "cutoffTable")
  expect_equal(length(rows), 1L)
  expectCells(rows[[1]], c(threshold = 15, sensitivity = 0.7, specificity = 1))
})

test_that("Fixed sensitivity and fixed specificity break ties, so they give one row", {
  # Controls 1, 2; cases 3, 4, 5; higher positive. Hand counts at pROC's thresholds:
  #   t    -Inf  1.5  2.5  3.5  4.5  Inf
  #   se    1    1    1    2/3  1/3  0
  #   sp    0    1/2  1    1    1    1
  # Sensitivity >= 0.5: -Inf to 3.5 qualify; the top specificity 1 is shared by 2.5 and 3.5; the higher
  # sensitivity among them is at 2.5. Specificity >= 0.5: 1.5 to Inf qualify; the top sensitivity 1 is shared
  # by 1.5 and 2.5; the higher specificity among them is at 2.5. Both give one row at 2.5.
  df <- data.frame(outcome = factor(c("n", "n", "p", "p", "p"), levels = c("n", "p")), x = 1:5)
  for (criterion in c("fixedSensitivity", "fixedSpecificity")) {
    options <- rocOptions("x", positiveLevel = "p")
    options$cutoffCriterion <- criterion
    options$fixedSensitivityValue <- 0.5
    options$fixedSpecificityValue <- 0.5
    results <- jaspTools::runAnalysis(analysisName, df, options)
    expectNoError(results)
    el <- getTableElement(results, "cutoffTable")
    expect_equal(length(el$data), 1L, label = paste(criterion, "number of rows"))
    expectCells(el$data[[1]], c(threshold = 2.5, sensitivity = 1, specificity = 1), label = criterion)
    expect_false(grepl("equally good", footnoteText(el)), label = paste(criterion, "no tie footnote"))
  }
})

test_that("Comparison with a zero SE of the difference: z and p are empty, with a footnote", {
  # a and b are the same values (identical placements); c and d both separate the classes perfectly (AUC 1).
  # pROC's roc.test reports z = 0 and p = 1 for both pairs (delong.paired.test sets z = 0 when d = sig = 0).
  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 6), levels = c("n", "p")),
                   a = c(1, 3, 2, 5, 4, 6, 4, 7, 5, 8, 9, 3))
  df$b <- df$a
  df$c <- c(1:6, 11:16)
  df$d <- c(2, 4, 6, 8, 10, 12, 20, 21, 22, 23, 24, 25)
  rocOf <- function(x) pROC::roc(df$outcome, x, levels = c("n", "p"), direction = "<", quiet = TRUE)
  for (pair in list(c("a", "b"), c("c", "d"))) {
    direct <- suppressWarnings(pROC::roc.test(rocOf(df[[pair[1]]]), rocOf(df[[pair[2]]]), method = "delong", paired = TRUE))
    expect_equal(unname(direct$statistic), 0)

    options <- rocOptions(pair, positiveLevel = "p")
    options$compareMarkers <- TRUE
    results <- jaspTools::runAnalysis(analysisName, df, options)
    expectNoError(results)
    el <- getTableElement(results, "comparisonTable")
    expect_equal(length(el$data), 1L)
    row <- el$data[[1]]
    expect_equal(as.numeric(row$difference), 0)
    expect_true(isEmptyCell(row$z), label = paste(pair[1], pair[2], "z is empty"))
    expect_true(isEmptyCell(row$p), label = paste(pair[1], pair[2], "p is empty"))
    expect_match(footnoteText(el), "standard error of the difference is zero")
  }
})

test_that("Bootstrap CI equals pROC's ci.auc (stratified) with the same seed, to 1e-12", {
  df <- asahData()
  for (marker in c("s100b", "wfns")) for (direction in c("<", ">")) for (level in c(0.95, 0.8)) {
    fit <- pROC::roc(df$outcome, df[[marker]], levels = c("Good", "Poor"), direction = direction, quiet = TRUE)
    set.seed(123)
    expected <- as.numeric(pROC::ci.auc(fit, conf.level = level, method = "bootstrap", boot.n = 300,
                                        boot.stratified = TRUE))
    set.seed(123)
    got <- jaspDiagnosticAccuracy:::.rocBootstrapAucCi(fit$cases, fit$controls, direction, 300, level)
    expect_equal(got, expected, tolerance = 1e-12, label = paste(marker, direction, level))
  }

  # jaspBase runs analyses with RNGkind(sample.kind = "Rounding") unless options(jaspLegacyRngKind = FALSE)
  # (setLegacyRng in runJaspResults), so pROC's reference draws below use the same sampler.
  oldKind <- RNGkind()[3]
  on.exit(suppressWarnings(RNGkind(sample.kind = oldKind)), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  fit <- pROC::roc(df$outcome, df$s100b, levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
  set.seed(5)
  expected <- as.numeric(pROC::ci.auc(fit, method = "bootstrap", boot.n = 300, boot.stratified = TRUE))
  set.seed(5)
  got <- jaspDiagnosticAccuracy:::.rocBootstrapAucCi(fit$cases, fit$controls, "<", 300, 0.95)
  expect_equal(got, expected, tolerance = 1e-12, label = "sample.kind Rounding")

  # Through the analysis, with JASP's seed control (seed 7 is set again before each marker).
  options <- rocOptions(c("s100b", "ndka"))
  options$aucCiMethod <- "bootstrap"
  options$bootstrapReplicates <- 250
  options$setSeed <- TRUE
  options$seed <- 7
  results <- jaspTools::runAnalysis(analysisName, df, options)
  rows <- getRows(results, "aucTable")
  for (marker in c("s100b", "ndka")) {
    fit <- pROC::roc(df$outcome, df[[marker]], levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
    set.seed(7)
    ci <- as.numeric(pROC::ci.auc(fit, method = "bootstrap", boot.n = 250, boot.stratified = TRUE))
    expectCells(rowFor(rows, "marker", marker), c(lower = ci[1], upper = ci[3]), tolerance = 1e-12, label = marker)
  }
})

test_that("Outcome-levels error shows at most 10 levels, then 'and N more'", {
  df <- asahData()
  df$many <- factor(sprintf("L%02d", (seq_len(nrow(df)) %% 15) + 1))
  options <- rocOptions("s100b", outcome = "many", positiveLevel = "L02")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectValidationError(results)
  msg <- as.character(results[["results"]][["errorMessage"]])
  for (l in sprintf("L%02d", 1:10)) expect_match(msg, l)
  for (l in sprintf("L%02d", 11:15)) expect_false(grepl(l, msg), label = paste(l, "not listed"))
  expect_match(msg, "and 5 more")
})

test_that("Infinite values are treated as missing for that marker only, with a footnote", {
  df <- asahData()
  df$withInf <- df$s100b
  df$withInf[c(1, 5, 60)] <- c(Inf, -Inf, Inf)
  finite <- is.finite(df$withInf)
  fit <- pROC::roc(df$outcome[finite], df$withInf[finite], levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
  counts <- table(df$outcome[finite])
  options <- rocOptions(c("withInf", "s100b"))
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expectCells(rowFor(el$data, "marker", "withInf"),
              c(nPositive = counts[["Poor"]], nNegative = counts[["Good"]], auc = as.numeric(fit$auc),
                se = sqrt(pROC::var(fit, method = "delong"))))
  expectCells(rowFor(el$data, "marker", "s100b"), refAuc$s100b.higher)
  expect_match(footnoteText(el), "3 infinite values were treated as missing")
})

test_that("A marker listed twice is used once: no duplicate rows and no self-comparison", {
  options <- rocOptions(c("s100b", "s100b", "wfns"))
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  auc <- getRows(results, "aucTable")
  expect_equal(length(auc), 2L)
  expectCells(rowFor(auc, "marker", "s100b"), refAuc$s100b.higher)
  expectCells(rowFor(auc, "marker", "wfns"), refAuc$wfns.higher)
  expect_equal(length(getRows(results, "cutoffTable")), 2L)
  cmp <- getRows(results, "comparisonTable")
  expect_equal(length(cmp), 1L)
  expectCells(cmp[[1]], c(difference = -0.0923102981029811, z = -2.20898359144091))
})

# ---------------------------------------------------------------------------------------------- review fixes (round 2)

test_that("Infinite values are reported once: not also counted as rows with a missing value", {
  # x = 1:5, Inf, 3:7, -Inf. Hand count: 2 infinite values, no other missing value.
  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 6), levels = c("n", "p")),
                   x = c(1:5, Inf, 3:7, -Inf))
  results <- jaspTools::runAnalysis(analysisName, df, rocOptions("x", positiveLevel = "p"))
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expectCells(rowFor(el$data, "marker", "x"), c(nPositive = 5, nNegative = 5))
  expect_match(footnoteText(el), "x: 2 infinite values were treated as missing")
  expect_false(grepl("missing value", footnoteText(el)), label = "no missing-value footnote")

  # Two more rows: a missing x (outcome present), and Inf with a missing outcome. Hand count: 2 infinite values
  # (rows 6 and 12) and 2 rows left out as missing (rows 13 and 14). Each of the 4 rows is reported once.
  df2 <- data.frame(outcome = factor(c(rep(c("n", "p"), each = 6), "n", NA), levels = c("n", "p")),
                    x = c(1:5, Inf, 3:7, -Inf, NA, Inf))
  results <- jaspTools::runAnalysis(analysisName, df2, rocOptions("x", positiveLevel = "p"))
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expectCells(rowFor(el$data, "marker", "x"), c(nPositive = 5, nNegative = 5))
  expect_match(footnoteText(el), "x: 2 infinite values were treated as missing")
  expect_match(footnoteText(el), "x: 2 rows with a missing value were left out")
})

test_that("Comparison footnote names the listwise rows and the marker that empties a class, not another marker", {
  # x has no positive case of its own (missing for every positive row), so the rows with every marker present
  # hold 0 positive and 6 negative cases. y and z are complete. Every pair fails because of x.
  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 6), levels = c("n", "p")),
                   x = c(1:6, rep(NA, 6)), y = c(1, 3, 2, 5, 4, 6, 4, 7, 5, 8, 9, 3),
                   z = c(2, 1, 4, 3, 6, 5, 8, 7, 3, 9, 10, 6))
  options <- rocOptions(c("x", "y", "z"), positiveLevel = "p")
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "comparisonTable")
  expect_equal(length(el$data), 3L)
  for (row in el$data) {
    expect_equal(as.numeric(row$n), 6)
    expect_true(isEmptyCell(row$difference))
  }
  fn <- footnoteText(el)
  expect_match(fn, "These rows hold 0 positive and 6 negative cases")
  expect_match(fn, "x has fewer than two positive cases even on its own rows")
  expect_false(grepl("No ROC curve for y", fn), label = "y is not blamed")
  expect_false(grepl("No ROC curve for z", fn), label = "z is not blamed")
  expect_false(grepl("y has fewer", fn), label = "y is not named as a cause")

  # No single marker to blame: x is missing on positive rows 7 to 9 and y on 10 to 12. Each has 3 positive
  # cases on its own rows, but none are left when both must be present.
  df2 <- df[, c("outcome", "y", "z")]
  df2$y[10:12] <- NA
  df2$x <- c(1:6, NA, NA, NA, 7, 8, 9)
  options <- rocOptions(c("x", "y"), positiveLevel = "p")
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df2, options)
  expectNoError(results)
  fn <- footnoteText(getTableElement(results, "comparisonTable"))
  expect_match(fn, "These rows hold 0 positive and 6 negative cases")
  expect_false(grepl("even on its own rows", fn), label = "no marker named")
  expect_false(grepl("No ROC curve", fn))
})

test_that("Threshold plot: values one unit in the last place apart keep their own piece (pROC thresholds)", {
  # Cases 1 + 2^-52 and 2, controls 0 and 1, higher values positive. No double lies strictly between 1 and
  # 1 + 2^-52, so (1 + 1 + 2^-52) / 2 rounds onto one of them. Hand counts for t in (1, 1 + 2^-52]:
  # cases >= t: both, so se 1; controls < t: both, so sp 1.
  eps <- 2^-52
  info <- list(cases = c(1 + eps, 2), controls = c(0, 1), nPositive = 2, nNegative = 2)
  fit <- pROC::roc(response = c("n", "n", "p", "p"), predictor = c(0, 1, 1 + eps, 2), levels = c("n", "p"),
                   direction = "<", quiet = TRUE)
  expect_equal(fit$specificities[fit$thresholds == 1 + eps], 1)
  expect_equal(fit$sensitivities[fit$thresholds == 1 + eps], 1)

  steps <- jaspDiagnosticAccuracy:::.rocThresholdSteps(info, list(direction = "higher"), "x")
  se <- steps[steps$measure == "Sensitivity", ]
  sp <- steps[steps$measure == "Specificity", ]
  # Path rows: start, 0, 0, 1, 1, 1 + eps, 1 + eps, 2, 2, end. The piece (1, 1 + eps] runs from row 5 to row 6.
  expect_identical(sp$threshold[5:6], c(1, 1 + eps))
  expect_equal(c(se$value[5:6], sp$value[5:6]), c(1, 1, 1, 1))
  expect_equal(sp$value, c(0, 0, 0.5, 0.5, 1, 1, 1, 1, 1, 1))
  expect_equal(se$value, c(1, 1, 1, 1, 1, 1, 0.5, 0.5, 0, 0))

  # Lower values positive: case positive when x <= t, control negative when x > t. Hand counts for t in
  # [1, 1 + eps): cases <= t: none, se 0; controls > t: none, sp 0.
  low <- jaspDiagnosticAccuracy:::.rocThresholdSteps(info, list(direction = "lower"), "x")
  expect_equal(low$value[low$measure == "Sensitivity"][5:6], c(0, 0))
  expect_equal(low$value[low$measure == "Specificity"][5:6], c(0, 0))

  # Every threshold gives the counts of its open interval: the midpoint where it lies strictly between, else
  # the neighbour with the same counts (pROC's roc_utils_thresholds).
  expect_identical(jaspDiagnosticAccuracy:::.rocPieceThresholds(c(0, 1, 1 + eps, 2), "higher"),
                   c(-Inf, 0.5, 1 + eps, 1.5, Inf))
  expect_identical(jaspDiagnosticAccuracy:::.rocPieceThresholds(c(0, 1, 1 + eps, 2), "lower"),
                   c(-Inf, 0.5, 1, 1.5, Inf))
})

test_that("Threshold plot: values near the largest double keep finite axis limits", {
  big <- .Machine$double.xmax
  info <- list(cases = c(0.9 * big, big), controls = c(-big, -0.9 * big), nPositive = 2, nNegative = 2)
  for (direction in c("higher", "lower")) {
    steps <- jaspDiagnosticAccuracy:::.rocThresholdSteps(info, list(direction = direction), "x")
    expect_true(all(is.finite(steps$threshold)), label = paste(direction, "all path points finite"))
    expect_equal(range(steps$threshold), c(-big, big))
    expect_true(all(is.finite(jaspDiagnosticAccuracy:::.rocPieceThresholds(sort(c(info$cases, info$controls)),
                                                                           direction)[2:4])))
  }
  # Two values 2^14 apart near 1e20: the axis shows the threshold minus the offset 1e20, and the path runs
  # from the first to the last pretty break of 0 to 2^14 = 16384 (base R pretty(): 0, 5000, ..., 20000).
  one <- list(cases = c(1e20 + 2^14), controls = c(1e20), nPositive = 1, nNegative = 1)
  steps <- jaspDiagnosticAccuracy:::.rocThresholdSteps(one, list(direction = "higher"), "x")
  expect_identical(attr(steps, "axis")$offset, 1e20)
  expect_equal(range(steps$threshold), range(pretty(c(0, 2^14))))
  expect_true(all(c(0, 2^14) %in% steps$threshold))

  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 3), levels = c("n", "p")),
                   x = c(-big, -0.9 * big, 0, 0.5 * big, 0.9 * big, big))
  options <- rocOptions("x", positiveLevel = "p")
  options$thresholdPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- findElement(results[["results"]], "thresholdPlot")
  expect_false(is.null(el))
  expect_false(identical(el[["status"]], "error"), label = "threshold plot has no error")
})

test_that("AUC table depends on the replicates and the seed only under the bootstrap", {
  deps <- jaspDiagnosticAccuracy:::.rocAucTableDependencies
  delong <- deps(list(aucCiMethod = "delong", setSeed = TRUE))
  expect_false(any(c("bootstrapReplicates", "setSeed", "seed") %in% delong))
  expect_true(all(c("outcome", "positiveLevel", "tests", "direction", "aucCiMethod", "ciLevel") %in% delong))
  boot <- deps(list(aucCiMethod = "bootstrap", setSeed = TRUE))
  expect_true(all(c("aucCiMethod", "ciLevel", "bootstrapReplicates", "setSeed", "seed") %in% boot))
  bootNoSeed <- deps(list(aucCiMethod = "bootstrap", setSeed = FALSE))
  expect_true(all(c("aucCiMethod", "bootstrapReplicates", "setSeed") %in% bootNoSeed))
  expect_false("seed" %in% bootNoSeed)
})

test_that("Exact ties: three Youden thresholds where pROC coords('best') returns two", {
  # Direction lower: case positive when x <= t, control negative when x > t. 12 cases, 12 controls, so
  # tp * nNeg + tn * nPos = 12 * (tp + tn). Hand counts at every pROC threshold:
  #   t     -Inf 1.5 2.5 3.5 4.5 5.5 6.5 7.5 Inf
  #   tp       0   3   5   7   7   8  10  12  12
  #   tn      12  11   8   7   6   3   3   2   0
  #   tp+tn   12  14  13  14  13  11  13  14  12
  # The best is 14 (score 168) at 1.5, 3.5 and 7.5.
  cases    <- c(1, 3, 3, 5, 7, 1, 2, 6, 2, 7, 1, 6)
  controls <- c(4, 2, 8, 8, 5, 1, 2, 2, 7, 5, 5, 3)
  for (t in c(1.5, 3.5, 7.5))
    expect_equal(sum(cases <= t) + sum(controls > t), 14, label = paste("tp + tn at", t))
  for (t in c(2.5, 4.5, 5.5, 6.5))
    expect_lt(sum(cases <= t) + sum(controls > t), 14)

  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 12), levels = c("n", "p")), x = c(controls, cases))
  fit <- pROC::roc(df$outcome, df$x, levels = c("n", "p"), direction = ">", quiet = TRUE)
  best <- pROC::coords(fit, "best", best.method = "youden", ret = "threshold", transpose = FALSE)
  expect_equal(sort(best$threshold), c(3.5, 7.5))   # pROC misses 1.5 on a floating-point near-tie

  options <- rocOptions("x", positiveLevel = "p")
  options$direction <- "lower"
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  rows <- getRows(results, "cutoffTable")
  expect_equal(length(rows), 3L)
  expect_equal(sort(vapply(rows, function(r) as.numeric(cellValue(r$threshold)), 0)), c(1.5, 3.5, 7.5))
  for (r in rows)
    expect_equal(as.numeric(r$youden), 14 / 12 - 1, tolerance = 1e-12)
})

# ---------------------------------------------------------------------------------------------- display (app check, 2026-11-26)

# Expected values below come from jaspBase's source, hand arithmetic, base R or pROC called directly.

schemaField <- function(results, table, column) {
  fields <- getTableElement(results, table)[["schema"]][["fields"]]
  hit <- Filter(function(f) identical(f[["name"]], column), fields)
  testthat::expect_equal(length(hit), 1L, label = paste(table, column, "in the schema"))
  if (length(hit) == 0) list() else hit[[1]]
}

test_that("Every p column uses JASP's p-value format, so a tiny p shows as < .001", {
  # jaspBase R/zzzWrappers.R (addColumnInfo, commit 5038e41): type "pvalue" gets the format "dp:3;p:.001".
  options <- rocOptions(c("s100b", "wfns"))
  options$compareMarkers <- TRUE
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  expectNoError(results)
  for (table in c("aucTable", "comparisonTable")) {
    field <- schemaField(results, table, "p")
    expect_identical(field[["type"]], "pvalue", label = paste(table, "p type"))
    expect_identical(field[["format"]], "dp:3;p:.001", label = paste(table, "p format"))
  }
})

test_that("A p that underflows to 0 gets a footnote", {
  # 3000 controls 0, 1, ..., 2999 and 3000 cases 2000.5, ..., 4999.5. Case 2000.5 + j is below 999 - j controls
  # (j < 999), so 999 * 1000 / 2 pairs are discordant and AUC = 1 - (999 * 1000 / 2) / 3000^2. With a small
  # DeLong SE, z is far beyond 38.5, where 2 * pnorm(-z) is 0 in double precision.
  controls <- 0:2999
  cases    <- 2000.5 + 0:2999
  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 3000), levels = c("n", "p")), x = c(controls, cases))
  fit <- pROC::roc(df$outcome, df$x, levels = c("n", "p"), direction = "<", quiet = TRUE)
  expect_equal(as.numeric(fit$auc), 1 - (999 * 1000 / 2) / 3000^2)
  z <- (as.numeric(fit$auc) - 0.5) / sqrt(pROC::var(fit, method = "delong"))
  expect_gt(z, 40)
  expect_identical(2 * stats::pnorm(-z), 0)

  options <- rocOptions("x", positiveLevel = "p")
  options$cutoffTable <- FALSE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expect_equal(as.numeric(el[["data"]][[1]][["p"]]), 0)
  expect_match(footnoteText(el), "below the smallest positive number R can store", fixed = TRUE)
})

test_that("Coordinates thresholds get enough significant digits per marker to show them between their values", {
  # Each finite threshold is a midpoint between two neighbouring observed values. The digits are the fewest
  # (4 to 15) with which the rounded threshold lies strictly between those two values. Hand reasoning:
  # fine: values 10.001, 10.003, ..., 10.019, thresholds 10.002, ..., 10.018. With 4 digits 10.002 shows as
  #   10.00, below 10.001; with 5 it shows as 10.002. So sf:5.
  # coarse: values 1, ..., 10, thresholds 1.5, ..., 9.5: 4 digits show them exactly. So sf:4.
  # tiny: values 1 + k * 1e-12, thresholds about 1 + (k + 0.5) * 1e-12. With 13 digits (steps of 1e-12) the
  #   second threshold, the double 1.00000000000150013..., shows as 1.000000000002, above its upper value, the
  #   double 1.00000000000199995... (sprintf("%.25f")). With 14 digits (steps of 1e-13) every threshold shows
  #   as 1.0000000000005, 1.0000000000015 and so on, strictly between its values. So sf:14.
  # tooClose: values 1 + k * 1e-15: the thresholds need 16 digits, so the cap of 15 applies, with a footnote.
  fine     <- 10.001 + 0.002 * (0:9)
  coarse   <- 1:10
  tiny     <- 1 + (0:9) * 1e-12
  tooClose <- 1 + (0:9) * 1e-15
  df <- data.frame(outcome = factor(rep(c("n", "p"), 5), levels = c("n", "p")), fine = fine, coarse = coarse,
                   tiny = tiny, tooClose = tooClose)
  expect_identical(signif(10.002, 4), 10)
  expect_identical(signif(10.002, 5), 10.002)

  options <- rocOptions(c("fine", "coarse", "tiny", "tooClose"), positiveLevel = "p")
  options$coordinatesTable <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  expect_identical(schemaField(results, "coordinatesTable", "threshold")[["type"]], "mixed")
  el <- getTableElement(results, "coordinatesTable")
  rows <- el[["data"]]
  expected <- c(fine = "sf:5", coarse = "sf:4", tiny = "sf:14", tooClose = "sf:15")
  for (m in names(expected)) {
    mine <- rowsFor(rows, "marker", m)
    # 10 values give 9 midpoints plus -Inf and Inf.
    expect_equal(length(mine), 11L, label = paste(m, "rows"))
    for (r in mine) {
      expect_identical(r[["threshold"]][["type"]], "number", label = paste(m, "cell type"))
      expect_identical(r[["threshold"]][["format"]], expected[[m]], label = paste(m, "cell format"))
    }
  }
  fn <- footnoteText(el)
  expect_match(fn, "tooClose: the thresholds are rounded for display", fixed = TRUE)
  for (m in c("fine", "coarse", "tiny"))
    expect_false(grepl(paste0(m, ": the thresholds are rounded"), fn, fixed = TRUE), label = paste(m, "no footnote"))
  # The values are unchanged: the fine marker's finite thresholds are the midpoints 10.002, ..., 10.018.
  fineValues <- vapply(rowsFor(rows, "marker", "fine"), function(r) as.character(cellValue(r[["threshold"]])), "")
  finiteFine <- suppressWarnings(as.numeric(fineValues))
  finiteFine <- sort(finiteFine[is.finite(finiteFine)])
  expect_equal(finiteFine, 10.002 + 0.002 * (0:8), tolerance = 1e-12)
})

test_that("Coordinates thresholds near 1e10 and near timestamps get enough digits", {
  # big: values 1e10, 1e10 + 1, 1e10 + 2, 1e10 + 3; thresholds 1e10 + 0.5 and so on. With 11 digits (steps
  #   of 1) 10000000000.5 is exactly halfway and JASP rounds it away from zero, onto 10000000001, one of its
  #   values. With 12 digits (steps of 0.1) it shows exactly. So sf:12 (the old cap of 10 showed all as 1e10).
  # stamp: values 1.7e12, 1.7e12 + 1000, 1.7e12 + 2000; thresholds 1.7e12 + 500 and + 1500. With 10 digits
  #   (steps of 1000) 1700000000500 is halfway and rounds up onto 1700000001000, a value. With 11 digits (steps
  #   of 100) it shows exactly. So sf:11.
  df <- data.frame(outcome = factor(c("n", "p", "n", "p"), levels = c("n", "p")),
                   big = 1e10 + 0:3, stamp = 1.7e12 + c(0, 1000, 2000, 2000))
  options <- rocOptions(c("big", "stamp"), positiveLevel = "p")
  options$coordinatesTable <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  rows <- getRows(results, "coordinatesTable")
  expected <- c(big = "sf:12", stamp = "sf:11")
  for (m in names(expected)) {
    mine <- rowsFor(rows, "marker", m)
    expect_true(length(mine) > 0, label = paste(m, "rows"))
    for (r in mine)
      expect_identical(r[["threshold"]][["format"]], expected[[m]], label = paste(m, "cell format"))
  }
})

test_that("JASP's rounding is mirrored exactly: halfway goes away from zero on the exact double", {
  # JASP formats sf with Intl.NumberFormat (rounding mode halfExpand) or toExponential, both on the exact value
  # of the double. 0.125 and 2000.5 are exact doubles, so they are exactly halfway: 0.13 and 2001 at 2 and 4
  # digits, and -0.13 for -0.125. R's signif() gives 0.12 for 0.125. 2.675 is the double
  # 2.67499999999999982236431605997495353221893310546875, below halfway, so 2.67 at 3 digits.
  shown <- function(x, d) {
    r <- jaspDiagnosticAccuracy:::.rocRoundDecimal(jaspDiagnosticAccuracy:::.rocDecimal(x), d)
    r$sign * r$head * 10^(r$exp - 14)
  }
  expect_identical(signif(0.125, 2), 0.12)
  expect_equal(shown(0.125, 2), 0.13, tolerance = 1e-15)
  expect_equal(shown(-0.125, 2), -0.13, tolerance = 1e-15)
  expect_equal(shown(2000.5, 4), 2001)
  expect_identical(sprintf("%.50f", 2.675), "2.67499999999999982236431605997495353221893310546875")
  expect_equal(shown(2.675, 3), 2.67, tolerance = 1e-15)
  expect_equal(shown(9999.5, 4), 10000)
  expect_equal(shown(0, 4), 0)

  # Below 10^d JASP uses Intl.NumberFormat, which rounds the shortest decimal that reads back as the same
  # double, halfway away from zero. 1234.55 is the double 1234.549999999999954525..., but its shortest decimal
  # is "1234.55", so 5 digits give 1234.6 (toExponential(4) gives 1.2345e+3). 1.005 (the double
  # 1.00499999999999989...) gives 1.01 at 3 digits; toPrecision(3) gives 1.00.
  shownIntl <- function(x, d) {
    r <- jaspDiagnosticAccuracy:::.rocRoundDecimal(jaspDiagnosticAccuracy:::.rocShortestDecimal(x), d)
    r$sign * r$head * 10^(r$exp - 14)
  }
  expect_identical(sprintf("%.30f", 1234.55), "1234.549999999999954525264911353588")
  expect_equal(shown(1234.55, 5), 1234.5, tolerance = 1e-15)
  expect_equal(shownIntl(1234.55, 5), 1234.6, tolerance = 1e-15)
  expect_equal(shownIntl(-1234.55, 5), -1234.6, tolerance = 1e-15)
  expect_equal(shownIntl(1.005, 3), 1.01, tolerance = 1e-15)
  expect_equal(shown(1.005, 3), 1, tolerance = 1e-15)
  expect_equal(shownIntl(0.1 + 0.2, 15), 0.3, tolerance = 1e-15)  # shortest "0.30000000000000004"
  # The shortest decimals "1234.55", "0.30000000000000004", "5e-324" and "1e+300": the first 15 digits, the 16th
  # digit, and whether any digit after the 15th is non-zero.
  sd <- jaspDiagnosticAccuracy:::.rocShortestDecimal(c(1234.55, 0.1 + 0.2, 5e-324, 1e300))
  expect_identical(sd$head, c(123455000000000, 300000000000000, 500000000000000, 100000000000000))
  expect_identical(sd$following, c(0L, 0L, 0L, 0L))
  expect_identical(sd$tail, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(sd$exp, c(3L, -1L, -324L, 300L))
})

test_that("Threshold digits: a threshold must show between its values under the rounding JASP uses", {
  # 1234.55 between 1234.5 and 1234.6 is below 10^5 and 1e6, so JASP uses Intl.NumberFormat. At 5 digits Intl
  # (shortest "1234.55", halfway, away from zero) gives 1234.6, on the upper value. At 6 digits it gives
  # 1234.55. So 6.
  expect_identical(jaspDiagnosticAccuracy:::.rocThresholdDigits(1234.55, c(1234.5, 1234.6)),
                   list(digits = 6L, exact = TRUE))
  # 1.0045 is the double 1.00449999999999994848... (sprintf("%.25f")), shortest "1.0045". Between 1.0039 and
  # 1.00455, at 4 digits the exact digits would give 1.004 (between), but Intl, which JASP uses here, gives 1.005
  # (above 1.00455). At 5 digits Intl gives 1.0045. So 5: a check with the exact digits would stop at 4.
  expect_identical(sprintf("%.25f", 1.0045), "1.0044999999999999484856517")
  expect_identical(jaspDiagnosticAccuracy:::.rocThresholdDigits(1.0045, c(1.0039, 1.00455)),
                   list(digits = 5L, exact = TRUE))
  # 12345.5 between 12345 and 12346: at 4 digits it is at or above 10^4, so JASP uses toExponential, which gives
  # 1.235e+4, above 12346. At 5 digits it is below 10^5 and 1e6, so Intl: exactly halfway, away from zero, so
  # 12346, a value. At 6 digits it shows as 12345.5. So 6.
  expect_identical(jaspDiagnosticAccuracy:::.rocThresholdDigits(12345.5, c(12345, 12346)),
                   list(digits = 6L, exact = TRUE))
})

test_that("Cut-off thresholds get the digits that show them between their values (ties and a halfway case)", {
  # Controls 1, 1234.5, 1234.6; cases 1234.6, 1234.7, 3000; higher values positive. 3 cases and 3 controls, so
  # Youden ranks thresholds by tp + tn. Hand counts at pROC's thresholds:
  #   t       617.75  1234.55  1234.65  2117.35
  #   tp + tn  3 + 1    3 + 2    2 + 3    1 + 3
  # So 1234.55 and 1234.65 tie. With 4 digits both show as 1235. With 5, 1234.55 shows as 1234.6 in JASP
  # (Intl.NumberFormat rounds the shortest decimal "1234.55" halfway up), one of its values; toExponential
  # would give 1234.5, the other. With 6 both show exactly. So sf:6.
  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 3), levels = c("n", "p")),
                   x = c(1, 1234.5, 1234.6, 1234.6, 1234.7, 3000))
  options <- rocOptions("x", positiveLevel = "p")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  expect_identical(schemaField(results, "cutoffTable", "threshold")[["type"]], "mixed")
  rows <- getRows(results, "cutoffTable")
  expect_equal(length(rows), 2L)
  expect_equal(sort(vapply(rows, function(r) cellValue(r[["threshold"]]), 0)), c(1234.55, 1234.65))
  for (r in rows)
    expect_identical(r[["threshold"]][["format"]], "sf:6")

  # One control at 2000.4, one case at 2000.6: the threshold 2000.5 is exactly halfway at 4 digits and shows as
  # 2001, above the case. With 5 digits it shows as 2000.5. So sf:5.
  df2 <- data.frame(outcome = factor(c("n", "p"), levels = c("n", "p")), x = c(2000.4, 2000.6))
  results <- jaspTools::runAnalysis(analysisName, df2, options)
  expectNoError(results)
  row <- getRows(results, "cutoffTable")[[1]]
  expect_equal(cellValue(row[["threshold"]]), 2000.5)
  expect_identical(row[["threshold"]][["format"]], "sf:5")

  # A threshold you type is shown with all its digits: 1234.5678 has 8 significant digits.
  options$cutoffCriterion <- "threshold"
  options$thresholdValue <- 1234.5678
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  row <- getRows(results, "cutoffTable")[[1]]
  expect_equal(cellValue(row[["threshold"]]), 1234.5678)
  expect_identical(row[["threshold"]][["format"]], "sf:8")
})

test_that("Cut-off table: thresholds that even 15 digits cannot show get a footnote naming the marker", {
  # Values 1 + k * 1e-15 for k = 0, ..., 3, controls k = 0, 2 and cases k = 1, 3: the finite Youden thresholds
  # lie about 0.5e-15 from their values and need 16 significant digits.
  df <- data.frame(outcome = factor(c("n", "p", "n", "p"), levels = c("n", "p")), close = 1 + (0:3) * 1e-15)
  options <- rocOptions("close", positiveLevel = "p")
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "cutoffTable")
  finite <- Filter(function(r) is.finite(as.numeric(cellValue(r[["threshold"]]))), el[["data"]])
  expect_true(length(finite) > 0)
  for (r in el[["data"]])
    expect_identical(r[["threshold"]][["format"]], "sf:15")
  expect_match(footnoteText(el), "close: the thresholds are rounded for display", fixed = TRUE)
})

test_that("Coordinates table: a marker without a curve has an empty threshold cell", {
  df <- asahData()
  df$constant <- 1
  options <- rocOptions(c("wfns", "constant"))
  options$coordinatesTable <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  row <- rowFor(getRows(results, "coordinatesTable"), "marker", "constant")
  expect_true(isEmptyCell(cellValue(row[["threshold"]])))
})

plotObject <- function(results, name) {
  el <- findElement(results[["results"]], name)
  testthat::expect_false(is.null(el), label = paste(name, "exists"))
  testthat::expect_false(identical(el[["status"]], "error"), label = paste(name, "has no error"))
  results[["state"]][["figures"]][[el[["data"]]]][["obj"]]
}

test_that("Plots render and their notes say what the circles mark and which marker has no curve", {
  df <- asahData()
  df$beta <- ifelse(df$outcome == "Poor", NA, df$ndka)
  options <- rocOptions(c("s100b", "beta"))
  options$rocPlot <- TRUE
  options$thresholdPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  for (name in c("rocPlot", "thresholdPlot")) {
    p <- plotObject(results, name)
    expect_true(ggplot2::is_ggplot(p), label = paste(name, "is a ggplot"))
    expect_silent(ggplot2::ggplotGrob(p))
    expect_match(p$labels$caption, "No curve for beta", label = paste(name, "caption"))
  }
  expect_match(gsub("\\s+", " ", plotObject(results, "rocPlot")$labels$caption),
               "Circles mark the cut-off chosen for each marker by the criterion in the cut-off table (Youden). Tied cut-offs each get a circle.",
               fixed = TRUE)

  options$cutoffCriterion <- "threshold"
  options$thresholdValue <- 0.2
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expect_match(gsub("\\s+", " ", plotObject(results, "rocPlot")$labels$caption), "threshold 0.2.", fixed = TRUE)
  # The value as typed, not rounded to 6 digits (1234.57).
  options$thresholdValue <- 1234.5678
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expect_match(gsub("\\s+", " ", plotObject(results, "rocPlot")$labels$caption), "threshold 1234.5678.",
               fixed = TRUE)

  options$cutoffTable <- FALSE
  options$tests <- "s100b"
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expect_null(plotObject(results, "rocPlot")$labels$caption)
})

test_that("ROC plot: tied cut-offs each get a circle", {
  # x = 1, 2, 3, 4 with cases at 2 and 4 (higher positive): by hand, Youden is 0.5 at 1.5 (tp 2, tn 1) and at
  # 3.5 (tp 1, tn 2), below that elsewhere. So two circles: (1 - sp, se) = (0.5, 1) and (0, 0.5).
  df <- data.frame(outcome = factor(c("n", "p", "n", "p"), levels = c("n", "p")), x = 1:4)
  options <- rocOptions("x", positiveLevel = "p")
  options$rocPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  built <- ggplot2::ggplot_build(plotObject(results, "rocPlot"))
  pts <- Filter(function(d) "shape" %in% names(d) && all(d$shape == 21), built$data)
  expect_equal(length(pts), 1L)
  got <- pts[[1]][order(pts[[1]]$x), c("x", "y")]
  expect_equal(unname(as.matrix(got)), rbind(c(0, 0.5), c(0.5, 1)))
})

test_that("ROC plot chance line runs from (0, 0) to (1, 1) only", {
  options <- rocOptions("s100b")
  options$rocPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, asahData(), options)
  built <- ggplot2::ggplot_build(plotObject(results, "rocPlot"))
  segments <- Filter(function(d) all(c("xend", "yend") %in% names(d)), built$data)
  expect_equal(length(segments), 1L)
  expect_equal(unlist(segments[[1]][1, c("x", "y", "xend", "yend")], use.names = FALSE), c(0, 0, 1, 1))
})

test_that("Threshold plot: each panel's x axis runs from the first to the last pretty break of its data", {
  # As jaspLearnBayes .bcFillPlotTestCharacteristics (commit 700b1e7): breaks = pretty() of the data range with
  # the default n, limits = range(breaks), ggplot2's default 5% expansion. The expected breaks come from base
  # R pretty() on the range of each marker's values: s100b 0.03 to 2.07 gives 0, 0.5, ..., 2.5 (before this
  # change the axis ran from -1 to 3).
  df <- asahData()
  expect_equal(pretty(c(0.03, 2.07)), seq(0, 2.5, by = 0.5))
  for (tests in list(c("s100b", "ndka", "wfns"), "ndka")) {
    options <- rocOptions(tests)
    options$cutoffTable <- FALSE
    options$thresholdPlot <- TRUE
    results <- jaspTools::runAnalysis(analysisName, df, options)
    expectNoError(results)
    p <- plotObject(results, "thresholdPlot")
    expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRangeFrame"), TRUE)), label = "rangeframe")
    built <- ggplot2::ggplot_build(p)
    for (k in seq_along(tests)) {
      x <- df[[tests[k]]][!is.na(df$outcome)]
      wanted <- pretty(range(x, na.rm = TRUE))
      pp <- built$layout$panel_params[[k]]
      breaks <- pp$x$breaks
      breaks <- breaks[!is.na(breaks)]
      shown  <- pp$x$continuous_range
      unpadded <- shown + c(1, -1) * diff(shown) * 0.05 / 1.1
      label <- paste(paste(tests, collapse = " "), tests[k])
      expect_equal(breaks, wanted, tolerance = 1e-9, label = paste(label, "breaks"))
      expect_equal(unpadded, range(wanted), tolerance = 1e-9, label = paste(label, "limits"))
    }
  }
})

test_that("Threshold plot: when the tick labels of pretty() would not fit, fewer breaks are asked for", {
  # Values from -3e300 to 4e300: base R pretty() with its default n = 5 gives 8 breaks, -3e+300 to 4e+300,
  # with 50 characters of labels plus 2 per label, more than the 48 that fit. With n = 4 it gives 5 breaks,
  # -4e+300 to 4e+300, 31 characters plus 10.
  lim <- c(-3e300, 4e300)
  expect_equal(length(pretty(lim)), 8L)
  expect_equal(pretty(lim, n = 4), c(-4e300, -2e300, 0, 2e300, 4e300))
  expect_equal(jaspDiagnosticAccuracy:::.rocPrettyBreaks(lim), pretty(lim, n = 4))
  # aSAH ndka (3.01 to 419.19) keeps the default: 0, 100, ..., 500.
  expect_equal(jaspDiagnosticAccuracy:::.rocPrettyBreaks(c(3.01, 419.19)), seq(0, 500, by = 100))
})

test_that("Threshold plot: a tiny spread at a large magnitude is shown against threshold minus an offset", {
  # 1e15 plus whole numbers within about 10: the spread is about 2e-14 of the size, below what ggplot2 can
  # show (scales::zero_range collapses the axis). The axis shows the threshold minus 1e15, with breaks from
  # pretty() of that range. Timestamps 1.7e12 plus whole numbers within about 4000: labels of the raw values
  # would all read 1.7e+12. Both need distinct tick labels.
  set.seed(5)
  z <- c(rnorm(30), rnorm(30, 1))
  y <- factor(rep(c("n", "p"), each = 30), levels = c("n", "p"))
  cluster <- 1e15 + round(z * 3)
  stamp   <- 1.7e12 + round(z * 1000)
  # Negative values: the axis shows the threshold plus a round number (-1e15 subtracted).
  for (case in list(list(x = cluster, offset = 1e15, title = "Threshold − 1e+15"),
                    list(x = stamp, offset = 1.7e12, title = "Threshold − 1.7e+12"),
                    list(x = -cluster, offset = -1e15, title = "Threshold + 1e+15"))) {
    df <- data.frame(outcome = y, x = case$x)
    options <- rocOptions("x", positiveLevel = "p")
    options$cutoffTable <- FALSE
    options$thresholdPlot <- TRUE
    results <- jaspTools::runAnalysis(analysisName, df, options)
    expectNoError(results)
    p <- plotObject(results, "thresholdPlot")
    expect_identical(p$scales$get_scales("x")$name, case$title)
    built <- ggplot2::ggplot_build(p)
    pp <- built$layout$panel_params[[1]]
    wanted <- pretty(range(case$x - case$offset))
    expect_equal(pp$x$breaks[!is.na(pp$x$breaks)], wanted, label = paste(case$title, "breaks"))
    labels <- pp$x$get_labels()
    labels <- labels[!is.na(labels)]
    expect_equal(anyDuplicated(labels), 0L, label = paste(case$title, "distinct labels"))
    expect_silent(ggplot2::ggplotGrob(p))
  }

  # Two panels: the offset goes into the panel title.
  df <- data.frame(outcome = y, x = cluster, z = z)
  options <- rocOptions(c("x", "z"), positiveLevel = "p")
  options$cutoffTable <- FALSE
  options$thresholdPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  p <- plotObject(results, "thresholdPlot")
  expect_identical(levels(p$data$marker), c("x (Threshold − 1e+15)", "z"))
  expect_identical(p$scales$get_scales("x")$name, "Threshold")
})

test_that("An ordinal marker with text labels uses its level order, with a footnote", {
  # Levels low < mid < high are used as 1, 2, 3. Expected AUC and SE from pROC on those codes directly.
  codes <- c(1, 2, 2, 3, 1, 1, 2, 3, 3, 3)
  out   <- factor(rep(c("n", "p"), each = 5), levels = c("n", "p"))
  df <- data.frame(outcome = out, grade = factor(c("low", "mid", "high")[codes], levels = c("low", "mid", "high"),
                                                 ordered = TRUE))
  fit <- pROC::roc(out, codes, levels = c("n", "p"), direction = "<", quiet = TRUE)
  options <- rocOptions("grade", positiveLevel = "p")
  options$cutoffTable <- FALSE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  expectCells(rowFor(el[["data"]], "marker", "grade"),
              c(auc = as.numeric(fit$auc), se = sqrt(pROC::var(fit, method = "delong"))), tolerance = 1e-12)
  expect_match(footnoteText(el), "grade has text labels. Its level order (1, 2, ...) is used as the marker value.",
               fixed = TRUE)
})

test_that("AUC table: a zero DeLong SE leaves z and p empty, with a footnote", {
  # Controls 1, 2, 3 and cases 4, 5, 6 (higher positive): every case is above every control, so every
  # placement value is 1, both DeLong variance terms are 0 and the SE is 0 (hand reasoning). AUC = 1.
  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 3), levels = c("n", "p")), x = 1:6)
  fit <- pROC::roc(df$outcome, df$x, levels = c("n", "p"), direction = "<", quiet = TRUE)
  expect_warning(v <- pROC::var(fit, method = "delong"), "AUC == 1 is always 0", fixed = TRUE)
  expect_equal(v, 0)
  options <- rocOptions("x", positiveLevel = "p")
  options$cutoffTable <- FALSE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  el <- getTableElement(results, "aucTable")
  row <- el$data[[1]]
  expect_equal(as.numeric(row$auc), 1)
  expect_equal(as.numeric(row$se), 0)
  expect_true(isEmptyCell(row$z))
  expect_true(isEmptyCell(row$p))
  expect_match(footnoteText(el), "When the DeLong standard error is zero (the classes do not overlap), z and p are not defined.",
               fixed = TRUE)
})

test_that("Plots: when no marker has a curve, both plots show an error message", {
  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 3), levels = c("n", "p")), k = 1)
  options <- rocOptions("k", positiveLevel = "p")
  options$rocPlot <- TRUE
  options$thresholdPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  for (name in c("rocPlot", "thresholdPlot")) {
    el <- findElement(results[["results"]], name)
    expect_identical(el[["status"]], "error", label = paste(name, "status"))
    expect_match(paste(unlist(el[["error"]]), collapse = " "),
                 "No marker has an ROC curve. See the footnotes of the AUC table.", fixed = TRUE, label = name)
  }
})

# ---------------------------------------------------------------------------------------------- regressions (review, 2026-11-26, second round)

# Every expected value below comes from hand reasoning, base R or pROC called directly.

test_that("Threshold plot: values next to the largest double get a finite offset", {
  # x = max * (1 - k * 1e-12), k = 0, ..., 5, with max the largest double: they span about 9e296, below 1e-5 of
  # their size, so the axis gets an offset. The step is 10^(ceiling(log10(9e296)) + 1) = 1e298, and the middle
  # over the step is 17976931348.58 (max is 1.7976931348623157e308). Rounding gives 17976931349e298, above the
  # largest double, so Inf. Rounding toward zero gives 17976931348e298, which prints as 1.7976931348e+308.
  big <- .Machine$double.xmax
  x <- big * (1 - (0:5) * 1e-12)
  axis <- jaspDiagnosticAccuracy:::.rocThresholdAxis(sort(x))
  expect_true(is.finite(axis$offset))
  expect_identical(format(axis$offset, digits = 15), "1.7976931348e+308")
  expect_true(all(is.finite(axis$breaks)) && length(axis$breaks) >= 2)

  df <- data.frame(outcome = factor(rep(c("n", "p"), each = 3), levels = c("n", "p")), x = x)
  options <- rocOptions("x", positiveLevel = "p")
  options$cutoffTable <- FALSE
  options$thresholdPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  p <- plotObject(results, "thresholdPlot")
  expect_identical(p$scales$get_scales("x")$name, "Threshold − 1.7976931348e+308")
  expect_true(all(is.finite(p$data$threshold)))
  # The values minus the offset span the same 9e296 or so: not one vertical line.
  expect_gt(diff(range(p$data$threshold)), 8e296)
})

test_that("Threshold plot: the offset in the axis title is the number subtracted", {
  # v = 1e-10 + k * 2^-86, k = 0, ..., 5. 2^-34 <= 1e-10 < 2^-33, so one unit in the last place of 1e-10 is
  # 2^-86 and the values are 1e-10 and its next five doubles. They span 5 * 2^-86 (about 6.5e-26), so the step
  # is 1e-24 and the nearest multiple of the step is about 1e14 * 1e-24, which as a double is not 1e-10. The
  # title states "1e-10", so 1e-10 itself is subtracted, and the values become exactly k * 2^-86.
  v <- 1e-10 + (0:5) * 2^-86
  expect_identical(v[1], 1e-10)
  expect_false(identical(1e14 * 1e-24, 1e-10))
  axis <- jaspDiagnosticAccuracy:::.rocThresholdAxis(v)
  expect_identical(axis$offset, 1e-10)
  expect_identical(jaspDiagnosticAccuracy:::.rocOffsetText(axis$offset), "Threshold − 1e-10")
  info <- list(cases = v[c(2, 4, 6)], controls = v[c(1, 3, 5)], nPositive = 3, nNegative = 3)
  steps <- jaspDiagnosticAccuracy:::.rocThresholdSteps(info, list(direction = "higher"), "x")
  path <- steps$threshold[steps$measure == "Sensitivity"]
  expect_identical(path[2:13], rep((0:5) * 2^-86, each = 2))
})

test_that("Threshold plot: a tiny negative minimum still gives plain tick labels", {
  # x = -1e-14, 0.2, ..., 1. Base R pretty() of that range has the step 0.2, but its breaks carry rounding noise
  # (-1e-14 and 0.19999999999999202), which print in exponent form. The cleaned breaks are 0, 0.2, ..., 1, and
  # the axis still reaches the smallest value, -1e-14.
  x <- c(-1e-14, 0.2, 0.4, 0.6, 0.8, 1)
  expect_true(any(grepl("e", format(pretty(range(x))))))
  axis <- jaspDiagnosticAccuracy:::.rocThresholdAxis(x)
  expect_identical(axis$offset, 0)
  expect_identical(axis$breaks, c(0, 0.2, 0.4, 0.6, 0.8, 1))
  expect_identical(axis$limits, c(-1e-14, 1))

  df <- data.frame(outcome = factor(rep(c("n", "p"), 3), levels = c("n", "p")), x = x)
  options <- rocOptions("x", positiveLevel = "p")
  options$cutoffTable <- FALSE
  options$thresholdPlot <- TRUE
  results <- jaspTools::runAnalysis(analysisName, df, options)
  expectNoError(results)
  p <- plotObject(results, "thresholdPlot")
  expect_silent(built <- ggplot2::ggplot_build(p))
  labels <- built$layout$panel_params[[1]]$x$get_labels()
  labels <- labels[!is.na(labels)]
  expect_identical(labels, c("0.0", "0.2", "0.4", "0.6", "0.8", "1.0"))
  # Every point of the paths is kept: none lies outside the axis.
  expect_true(all(c(-1e-14, 1) %in% p$data$threshold))
  expect_silent(ggplot2::ggplotGrob(p))
})

test_that("ROC plot note mentions tied cut-offs only for the criteria that can tie", {
  # Only the Youden index and closest to (0, 1) can give tied rows; the fixed criteria break ties, and a typed
  # threshold is one value.
  note <- function(criterion) {
    jaspDiagnosticAccuracy:::.rocCutoffPointNote(list(cutoffCriterion = criterion, fixedSensitivityValue = 0.9,
                                                      fixedSpecificityValue = 0.9, thresholdValue = 0.2))
  }
  tied <- "Tied cut-offs each get a circle."
  expect_match(note("youden"), tied, fixed = TRUE)
  expect_match(note("closestTopLeft"), tied, fixed = TRUE)
  expect_identical(note("fixedSensitivity"),
                   "Circles mark the cut-off chosen for each marker by the criterion in the cut-off table (Sensitivity ≥ 0.9).")
  expect_identical(note("fixedSpecificity"),
                   "Circles mark the cut-off chosen for each marker by the criterion in the cut-off table (Specificity ≥ 0.9).")
  expect_false(grepl(tied, note("threshold"), fixed = TRUE))
})

test_that("The plots' .data pronoun comes from ggplot2, so the package has no undefined global", {
  ns <- asNamespace("jaspDiagnosticAccuracy")
  expect_true(".data" %in% ls(parent.env(ns), all.names = TRUE))
  expect_identical(get(".data", envir = parent.env(ns)), ggplot2::.data)
})

test_that("An AUC of 1 gives no pROC warning from the module", {
  # Controls 1, 2, 3 and cases 4, 5, 6: AUC = 1, and pROC::var() warns that its variance is always 0 (see the
  # zero SE test above). The module keeps the SE of 0 and muffles only that warning.
  expect_warning(jaspDiagnosticAccuracy:::.rocFitMarker(c(1:6, NA), c(rep("n", 3), rep("p", 3), "p"), "p", "n", "<", "x"),
                 NA)
  fit <- jaspDiagnosticAccuracy:::.rocFitMarker(1:6, rep(c("n", "p"), each = 3), "p", "n", "<", "x")
  expect_equal(fit$auc, 1)
  expect_equal(fit$se, 0)
})

test_that("Exact decimals: the short method gives the fields of the full exact expansion", {
  # The expected fields come from the full exact digits of each double, printed by sprintf with 800 digits (glibc
  # prints the exact value): the first 15 digits, the 16th, whether a later digit is non-zero, and the exponent.
  fromFull <- function(x) {
    s <- sprintf("%.799e", abs(x))
    digits <- sub(".", "", sub("e.*", "", s), fixed = TRUE)
    list(sign = sign(x), exp = as.integer(sub(".*e", "", s)), head = as.numeric(substr(digits, 1, 15)),
         following = as.integer(substr(digits, 16, 16)), tail = grepl("[1-9]", substring(digits, 16)))
  }
  set.seed(3)
  x <- c(0, 5e-324, -5e-324, 2^-1022, .Machine$double.xmax, 1e300, -1e-300, 0.1, 0.1 + 0.2, 1234.55, 2.675, 0.125,
         2000.5, 9999.5, 1e15 + 0.5, 1e40, 2^-57, 2^-60, 2^140, (1:50) + 0.5, rnorm(200),
         rnorm(200) * 10^runif(200, -300, 300), 2^runif(100, -1074, 1023))
  expect_identical(jaspDiagnosticAccuracy:::.rocDecimal(x), fromFull(x))
  # At most 40 significant digits, by hand from the exact expansions: 1234.5 has 5; 0.1 has 55; 2^-30 =
  # 9.31322574615478515625e-10 has 21; 2^-57 = 6.938893903907228377647697925567626953125e-18 has 40; 2^-60 has
  # 42; the double 1e39 is a whole number below 1e40; the double 1e40 is above 10^40, so 41 digits; 2^140 has 43.
  expect_identical(jaspDiagnosticAccuracy:::.rocAtMost40Digits(c(1234.5, 0.1, 2^-30, 2^-57, 2^-60, 1e39, 1e40, 2^140),
                                                               c(3, -1, -10, -18, -19, 39, 40, 42)),
                   c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE))
  # The shortest decimal of a subnormal double: 5e-324 is "5e-324", and 3 * 2^-1074 (1.48e-323) is "1.5e-323".
  sd <- jaspDiagnosticAccuracy:::.rocShortestDecimal(c(5e-324, 3 * 2^-1074))
  expect_identical(as.numeric("1.5e-323"), 3 * 2^-1074)
  expect_identical(as.numeric("1e-323"), 2 * 2^-1074)
  expect_identical(sd$head, c(5e14, 1.5e14))
  expect_identical(sd$exp, c(-324L, -323L))
})

test_that("Threshold digits follow JASP's own choice of rounding: Intl.NumberFormat below 10^d and 1e6, else toExponential", {
  # jasp-desktop utils.js (development at 4e7f434, lines 347-357): toExponential(d - 1) when |x| >= 10^d or
  # |x| >= 1e6, Intl.NumberFormat with d significant digits otherwise.
  # 1234.55 (the double 1234.5499999..., shortest "1234.55") between 1234.5 and 1234.65: below 10^5 and 1e6, so
  # JASP uses Intl.NumberFormat, which gives 1234.6 at 5 digits, strictly between. At 4 digits it gives 1235,
  # above 1234.65. So 5 (toExponential would give 1.2345e+3, on 1234.5, but JASP does not use it here).
  expect_identical(sprintf("%.20f", 1234.55), "1234.54999999999995452526")
  expect_identical(jaspDiagnosticAccuracy:::.rocThresholdDigits(1234.55, c(1234.5, 1234.65)),
                   list(digits = 5L, exact = TRUE))
  # 1000000.45 (the double 1000000.4499999999534..., shortest "1000000.45") between 1000000.35 and 1000000.5: at or
  # above 1e6, so JASP uses toExponential, which gives 1.0000004e+6 at 8 digits, strictly between. At 7 digits it
  # gives 1.000000e+6, below 1000000.35. So 8 (Intl.NumberFormat would give 1000000.5, the upper value, but JASP
  # does not use it here).
  expect_identical(sprintf("%.20f", 1000000.45), "1000000.44999999995343387127")
  expect_identical(jaspDiagnosticAccuracy:::.rocThresholdDigits(1000000.45, c(1000000.35, 1000000.5)),
                   list(digits = 8L, exact = TRUE))
})
