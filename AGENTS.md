# jaspDiagnosticAccuracy

A community JASP module for ROC curves and diagnostic accuracy. It has one analysis, ROC Curve: the AUC with DeLong standard errors, cut-off tables, ROC and threshold plots, and a paired comparison of AUCs. These notes are for anyone who contributes, by hand or with a tool.

## Layout

- `R/`: the analysis code. `rocCurve.R` has the entry function, the option checks, the data preparation, the flow and the state that keeps the bootstrap AUCs (`.rocBootstrapState`); `rocCurve-computations.R` the fits, counts, cut-offs and the bootstrap draws; `rocCurve-tables.R` the tables; `rocCurve-threshold-digits.R` the significant digits of the thresholds; `rocCurve-plots.R` the plots.
- `inst/qml/`: the input form (`RocCurve.qml`). Its `info` texts are the help: JASP builds the help page from them (the Form's `info`, each control's `info`, and the Form's `infoBottom` for the missing values, the output, the references and the R packages). There is no `inst/help/` file; one would replace the built page.
- `inst/Upgrades.qml`: renames options of older versions in saved files.
- `tests/testthat/`: the unit tests and their data sets.
- `dev/validation/`: reference values worked out without the module's code, and the script that checks them against other sources.
- `dev/interface.md`: the option names, the statistics and the output names.
- `examples/`: an example `.jasp` file.

## Running the tests and the validation

Install the packages and the module as described in the README ("Installation"). jaspTools and testthat are not in `renv.lock`, so install them as well. Then, in R from the repository root:

```r
library(jaspTools)
setPkgOption("module.dirs", getwd())
setPkgOption("reinstall.modules", FALSE)
testthat::test_dir("tests/testthat")
```

Reinstall the module after each change to `R/`.

The validation script runs from the repository root, in an R that has pROC 1.19.0.1 and ROCR 1.0-11:

```sh
Rscript dev/validation/reference.R
```

It stops on any disagreement. It rewrites `dev/validation/reference-values.csv` and the data sets in `tests/testthat/`; after a run, `git diff` should show no change to them. See `dev/validation/README.md`.

## Rules

- Option and output names follow `dev/interface.md`. Change that file first, then the code and the tests. A renamed option also needs a `ChangeRename` in `inst/Upgrades.qml` and a higher version in DESCRIPTION.
- Progressive disclosure: by default the analysis shows only the AUC table. Everything else is opt-in.
- Tests check against values computed independently (`dev/validation`), never against the module's own output.
- Every table, footnote and error message uses the decoded column names.
- User-visible changes go in `CHANGELOG.md` under "## Unreleased".
- Writing: British English, no em dashes, short plain sentences.
- Commit messages are one plain line with no trailers.
