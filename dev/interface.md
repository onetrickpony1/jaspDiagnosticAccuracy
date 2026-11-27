# Interface contract for v1 (ROC Curve)

This file fixes the option names, the statistics and the output names. The R code and the tests both follow it.

## Names

- R package: `jaspDiagnosticAccuracy`. Analysis function: `rocCurve` (exported). QML form: `inst/qml/RocCurve.qml`. Title in the menu: "ROC Curve".

## Options (QML `name`, R type, default)

| Name | Values | Default |
|---|---|---|
| `outcome` | one variable (nominal or ordinal) | none |
| `positiveLevel` | a level of `outcome` (string) | the second level |
| `tests` | one or more variables (scale or ordinal); a variable listed twice is used once | none |
| `direction` | `"higher"` / `"lower"` | `"higher"` |
| `aucCiMethod` | `"delong"` / `"bootstrap"` | `"delong"` |
| `bootstrapSamples` | integer 100 to 100000 | 2000 |
| `setSeed`, `seed` | JASP's standard seed control | `FALSE`, 1 |
| `ciLevel` | 0 < x < 1 | 0.95 |
| `cutoffTable` | logical | `FALSE` |
| `cutoffCriterion` | `"youden"` / `"closestTopLeft"` / `"fixedSensitivity"` / `"fixedSpecificity"` / `"threshold"` | `"youden"` |
| `fixedSensitivityValue` | 0 < x < 1 | 0.9 |
| `fixedSpecificityValue` | 0 < x < 1 | 0.9 |
| `thresholdValue` | any number | 0 |
| `prevalence` | `"sample"` / `"custom"` | `"sample"` |
| `prevalenceValue` | 0 < x < 1 | 0.1 |
| `comparisonTable` | logical (enabled with 2 or more tests) | `FALSE` |
| `coordinatesTable` | logical | `FALSE` |
| `rocPlot` | logical | `FALSE` |
| `thresholdPlot` | logical | `FALSE` |

Renamed options: version 0.1.1 renamed `bootstrapReplicates` to `bootstrapSamples` (the standard name in the JASP option-name guide) and `compareMarkers` to `comparisonTable` (a noun, like `cutoffTable` and `coordinatesTable`). `inst/Upgrades.qml` renames them in files saved with 0.1.0. Any later rename needs a new entry there and a higher version in DESCRIPTION.

## Statistics (what each number means)

- Levels: `positiveLevel` is the case (positive) class; the other level is the control class. pROC call: `levels = c(negative, positive)`.
- Direction: `"higher"` means higher marker values indicate positive: pROC `direction = "<"`. `"lower"` means pROC `direction = ">"`. Never `"auto"`.
- Default output (progressive disclosure): with only the variables filled in, the output is the AUC table and nothing else. The other tables and the plots are absent, not empty, until their checkbox is on. The cut-off criterion and its value fields are enabled only when `cutoffTable` is on; the prevalence fields when `cutoffTable` or `coordinatesTable` is on. The ROC plot marks the chosen cut-off only when `cutoffTable` is on.
- Infinite values (Inf or -Inf) in a marker are treated as missing for that marker, with a footnote in the AUC table giving the count.
- Missing values: AUC and cut-off tables use, per marker, the rows where the outcome and that marker are both present. The comparison table uses rows where the outcome and every marker are present.
- AUC SE, z and p: always the DeLong SE. z = (AUC - 0.5) / SE, p two-sided normal.
- AUC CI: DeLong or bootstrap, at `ciLevel`. The ROC curves, the AUC and the DeLong placement values come from the R package pROC; the DeLong standard error, interval and test use pROC's formulas. The bootstrap is the module's own code with the same random draws and quantile rule as `ci.auc(method = "bootstrap", boot.n = bootstrapSamples, boot.stratified = TRUE)`, but without rebuilding an ROC curve per bootstrap sample. Its AUCs and interval agree with pROC's to rounding (within 1e-12), not bit for bit.
- Thresholds: pROC's own thresholds (midpoints between neighbouring observed values, plus -Inf and Inf).
- Cut-off criteria:
  - `youden`: maximise sensitivity + specificity - 1.
  - `closestTopLeft`: minimise (1 - sensitivity)^2 + (1 - specificity)^2.
  - `fixedSensitivity`: among thresholds with sensitivity >= the value, the one with the highest specificity.
  - `fixedSpecificity`: among thresholds with specificity >= the value, the one with the highest sensitivity.
  - `threshold`: classify with the given value, using the same rule as pROC for that direction.
  - Ties for the best value: one row per tied threshold, with a footnote.
  - Ties are found with exact integer arithmetic on the counts. On floating-point near-ties this can give more tied thresholds than pROC `coords(x = "best")`, which compares floating-point proportions. The module is the correct one there. Example: Youden, direction `"lower"`, cases 1 3 3 5 7 1 2 6 2 7 1 6, controls 4 2 8 8 5 1 2 2 7 5 5 3. TP + TN is 14 (out of 24) at 1.5, 3.5 and 7.5, an exact score of 168 at each, so all three are tied, but pROC returns only 3.5 and 7.5.
- At the chosen threshold: TP, FP, TN, FN counts from the per-marker data.
  - Sensitivity = TP / (TP + FN), CI Clopper-Pearson (`binom.test`) at `ciLevel`. Specificity likewise with TN / (TN + FP).
  - PPV and NPV from sensitivity, specificity and prevalence p (sample prevalence = n positive / n), by Bayes' rule: PPV = se p / (se p + (1 - sp)(1 - p)), NPV = sp (1 - p) / (sp (1 - p) + (1 - se) p).
  - LR+ = se / (1 - sp), LR- = (1 - se) / sp. Undefined (division by zero) is shown as an empty cell with a footnote.
  - Accuracy = (TP + TN) / n (sample accuracy, not prevalence-weighted). Youden's J = se + sp - 1.
- Comparison: `roc.test(method = "delong", paired = TRUE)` on the listwise data; difference = AUC first - AUC second (markers in the order given); CI for the difference at `ciLevel`. When the SE of the difference is 0, z and p are empty with a footnote (pROC would report z = 0, p = 1).
- Coordinates table: every pROC threshold with sensitivity, specificity, PPV, NPV (at the chosen prevalence).

## Output elements (jaspResults names and column names)

- `aucTable`: `marker`, `nPositive`, `nNegative`, `auc`, `se`, `lower`, `upper`, `z`, `p`.
- `cutoffTable`: `marker`, `criterion`, `threshold`, `sensitivity`, `sensitivityLower`, `sensitivityUpper`, `specificity`, `specificityLower`, `specificityUpper`, `ppv`, `npv`, `lrPositive`, `lrNegative`, `accuracy`, `youden`.
- `comparisonTable` (when the `comparisonTable` option is on): `marker1`, `marker2`, `n`, `difference`, `lower`, `upper`, `z`, `p`.
- `coordinatesTable` (when on): `marker`, `threshold`, `sensitivity`, `specificity`, `ppv`, `npv`.
- `rocPlot`, `thresholdPlot`.
- Errors (whole analysis): outcome without exactly 2 levels after removing missing values (message names the levels found); `positiveLevel` not among the levels present; an option outside its range in the table above, checked only when the output uses it (`ciLevel` always; `bootstrapSamples` under the bootstrap; `fixedSensitivityValue`, `fixedSpecificityValue` and `thresholdValue` when `cutoffTable` is on with that criterion; `prevalenceValue` when `prevalence` is `"custom"` and `cutoffTable` or `coordinatesTable` is on). The message names the field as the form shows it.
- Missing outcome: an empty cell (the empty string) counts as missing, as in JASP.
- Per-marker problems (constant marker, no cases in one class): that marker's rows show empty cells with a footnote, the other markers still run.

## Footnotes and plot text

As in JASP's own modules, a table has few notes. A note that always applies is kept only where the table cannot be read without it. Everything else is explained on the help page (`inst/help/rocCurve.md`). The plots carry no text inside the image.

Notes that are always shown:

- `aucTable`: "Positive level: {level}. Higher marker values indicate positive." (or "Lower ...").
- `comparisonTable`: "DeLong's test for two paired ROC curves. Difference = AUC of marker 1 - AUC of marker 2."

Notes for a non-default choice:

- `aucTable`, under the bootstrap: "Percentile bootstrap interval from {n} stratified bootstrap samples.", with a thousands separator (2,000). The DeLong interval has no note.
- `cutoffTable` and `coordinatesTable`, with a custom prevalence: "PPV and NPV use a prevalence of {p}." The sample prevalence has no note.

Notes that flag a problem, each shown only when it applies:

- `aucTable`: a marker without a curve (on its row); the SE and interval need two cases per class, or could not be computed; the interval could not be computed; infinite values treated as missing; rows with a missing value left out; text labels used as the level order; a zero DeLong SE, so z and p are not defined; a p that is stored as 0.
- `cutoffTable`: a marker without a curve (on its row); no threshold meets the criterion; equally good thresholds, one row each; thresholds rounded for display; empty PPV, NPV or likelihood ratio cells (a division by zero).
- `comparisonTable`: rows left out (only when at least one row was); too few cases in a class among the rows used; a marker without a curve on the rows used; a zero SE of the difference, so z and p are not defined; the comparison could not be computed; a p that is stored as 0.
- `coordinatesTable`: a marker without a curve (on its row); thresholds rounded for display; empty PPV or NPV cells. The second applies to almost every marker: at the threshold where every case is positive the NPV is 0 / 0, and where no case is positive the PPV is 0 / 0.

Every note uses the decoded column names.

Not in a note, but on the help page: the DeLong standard error and the two-sided Wald test of AUC = 0.5; ties between the classes count one half in the AUC; the classification rule (a value equal to the threshold counts as positive); the Clopper-Pearson intervals; PPV and NPV at the sample prevalence; pROC's thresholds (midpoints, plus -Inf and Inf); what the circles on the ROC plot mark.

Plots: a marker without a curve is left out of the ROC plot and the threshold plot (the AUC table says why). When no marker has a curve, the plot shows the error "No marker has an ROC curve. See the footnotes of the AUC table." (`plot$setError`).
