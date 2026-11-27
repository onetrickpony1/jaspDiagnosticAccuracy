# jaspDiagnosticAccuracy

A community JASP module for ROC curves and diagnostic accuracy. It has one analysis, ROC Curve: the AUC with DeLong standard errors, cut-off tables, ROC and threshold plots, and a paired comparison of AUCs. These notes are for anyone who contributes, by hand or with a tool.

## Layout

- `R/`: the analysis code. `rocCurve.R` has the entry function, the option checks, the data preparation, the flow and the state that keeps the bootstrap AUCs (`.rocBootstrapState`); `rocCurve-computations.R` the fits, counts, cut-offs and the bootstrap draws; `rocCurve-tables.R` the tables; `rocCurve-threshold-digits.R` the significant digits of the thresholds; `rocCurve-plots.R` the plots.
- `inst/qml/`: the input form (`RocCurve.qml`). Each control's `info` text is its tooltip. The Form's own `info` is not a tooltip: JASP uses it only as the opening text of the help page it builds from the `info` texts. The (i) button shows that built page only when `inst/help/rocCurve.md` is missing or when "Generate markdown files for help" is on in JASP's advanced preferences.
- `inst/help/rocCurve.md`: the help page, hand-written in the format of JASP's own modules. JASP shows it for the (i) button. It is named after the analysis function (`func: "rocCurve"` in `inst/Description.qml`).
- `inst/Upgrades.qml`: renames options of older versions in saved files.
- `tests/testthat/`: the unit tests and their data sets.
- `dev/validation/`: reference values worked out without the module's code, and the script that checks them against other sources.
- `dev/interface.md`: the option names, the statistics and the output names.
- `examples/`: an example `.jasp` file.

## Running the tests and the validation

Install the packages and the module as described in the README ("Installation", "As a development module"). jaspTools and testthat are not in `renv.lock`, so install them as well. Then, in R from the repository root:

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
- Few notes, as in JASP's own modules: a footnote only for the positive level and direction, the paired test, a non-default choice or a problem that applies. Explanations go on the help page. Plots carry no text inside the image. `dev/interface.md` ("Footnotes and plot text") lists them.
- Keep the help page and the `info` tooltips in step: a change to an option, a default or an output changes both.
- User-visible changes go in `CHANGELOG.md` under "## Unreleased".
- Writing: British English, no em dashes, short plain sentences.
- Commit messages are one plain line with no trailers.
