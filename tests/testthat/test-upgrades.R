context("ROC Curve: upgrades of saved files")

# inst/Upgrades.qml renames options of older versions when JASP opens a saved file. JASP applies it (jasp-desktop
# QMLComponents/modules/upgrader); these tests cannot run that code, so they read the ChangeRename entries the
# same way and apply them to the options saved in upgrade-0.1.0-aSAH.jasp (the aSAH example as saved with version 0.1.0).

upgradesFile <- function() testthat::test_path("..", "..", "inst", "Upgrades.qml")
exampleFile  <- function() testthat::test_path("upgrade-0.1.0-aSAH.jasp")

readUpgrade <- function() {
  qml <- paste(readLines(upgradesFile(), warn = FALSE), collapse = "\n")
  field <- function(name) regmatches(qml, regexec(paste0(name, ":\\s*\"([^\"]+)\""), qml))[[1]][2]
  renames <- regmatches(qml, gregexpr("ChangeRename\\s*\\{[^}]*\\}", qml))[[1]]
  list(
    functionName = field("functionName"),
    fromVersion  = field("fromVersion"),
    toVersion    = field("toVersion"),
    from = sub(".*from:\\s*\"([^\"]+)\".*", "\\1", renames),
    to   = sub(".*to:\\s*\"([^\"]+)\".*", "\\1", renames)
  )
}

readSavedAnalysis <- function() {
  dir <- tempfile()
  utils::unzip(exampleFile(), files = "analyses.json", exdir = dir)
  jsonlite::fromJSON(file.path(dir, "analyses.json"), simplifyVector = FALSE)[["analyses"]][[1]]
}

test_that("Upgrades.qml goes from 0.1.0 to the version in DESCRIPTION, for rocCurve", {
  skip_if_not(file.exists(upgradesFile()))
  upgrade <- readUpgrade()
  expect_identical(upgrade$functionName, analysisName)
  expect_identical(upgrade$fromVersion, "0.1.0")
  expect_identical(package_version(upgrade$toVersion), utils::packageVersion("jaspDiagnosticAccuracy"))
  expect_identical(upgrade$from, c("bootstrapReplicates", "compareMarkers"))
  expect_identical(upgrade$to, c("bootstrapSamples", "comparisonTable"))
  formOptions <- names(jaspTools::analysisOptions(analysisName))
  expect_true(all(upgrade$to %in% formOptions))
  expect_false(any(upgrade$from %in% formOptions))
})

test_that("The options of the 0.1.0 example file, renamed as Upgrades.qml says, are the form's and run", {
  skip_if_not(file.exists(upgradesFile()) && file.exists(exampleFile()))
  upgrade  <- readUpgrade()
  analysis <- readSavedAnalysis()
  expect_identical(analysis$dynamicModule$analysisEntry, analysisName)
  # JASP applies the step when the saved version is at most fromVersion (Upgrades::findClosestVersion).
  expect_true(package_version(analysis$dynamicModule$moduleVersion) <= package_version(upgrade$fromVersion))

  options <- analysis$options
  options[[".meta"]] <- NULL
  expect_true(all(upgrade$from %in% names(options)))
  for (i in seq_along(upgrade$from)) {
    options[[upgrade$to[i]]]   <- options[[upgrade$from[i]]]
    options[[upgrade$from[i]]] <- NULL
  }
  expect_true(all(names(jaspTools::analysisOptions(analysisName)) %in% names(options)))
  expect_equal(options$bootstrapSamples, 2000)
  expect_true(isTRUE(options$comparisonTable))

  options$outcome <- options$outcome$value
  options$tests   <- unlist(options$tests$value)
  results <- jaspTools::runAnalysis(analysisName, asahData(), options, quiet = TRUE)
  expectNoError(results)
  # The example file has "Compare markers" on, so the comparison table must be there after the renames.
  expect_equal(length(getRows(results, "comparisonTable")), 3L)
})
