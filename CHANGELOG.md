# Changelog

## Unreleased

- Clear error messages when an option is outside its range (confidence level, number of bootstrap samples, fixed sensitivity or specificity, threshold, custom prevalence), instead of a crash or a missing row.
- Empty outcome cells count as missing, as elsewhere in JASP.
- Footnotes use the module's own messages when pROC cannot compute a curve, interval or comparison.
- Faster coordinates table for large data sets. Its time now grows in line with the number of rows, not with its square.
- Faster sensitivity and specificity by threshold plot for large data sets.
- DESCRIPTION has a `Website` field. JASP takes the module's website from it, for example in the message shown for a saved file whose module is not installed.
- Version 0.1.1. Two options have new names: `bootstrapReplicates` is now `bootstrapSamples`, and `compareMarkers` is now `comparisonTable`. This matters for R syntax and scripts that set options by name. Files saved with 0.1.0 keep their settings: `inst/Upgrades.qml` renames the two options when JASP opens them.
- The help page is now built by JASP from the help texts of the form, and the tooltips are taken from the same texts. It keeps everything the earlier help file said.
- A new confidence level, or a switch to DeLong and back, no longer draws the bootstrap samples again, so the interval updates at once. The analysis keeps the bootstrap AUCs while the file is open, and draws them again only when the variables, the positive level, the direction, the number of bootstrap samples, the "Set seed" option or the seed change. So without a seed, a new level uses the same random samples. If "Store analysis state and plot in jasp-files" is on in JASP's preferences, a saved file holds the bootstrap AUCs too: up to about 800 KB per marker with 100,000 bootstrap samples.
- Faster cut-off table and paired comparison for large data sets. For 10 markers and 100,000 rows (Youden index, closest to (0, 1) and the comparison), about 1 to 1.5 s instead of 5 to 7.5 s, timed in R on one Linux machine.
- The help names the value fields of the cut-off criteria and of the custom prevalence (for example "Double Field - Minimum sensitivity") instead of showing a bare "Double Field". The form looks the same.
- The tooltip of the cut-off criterion is shorter. Its other facts are now in the help, in the "Cut-off" section under Output.
- The field for the number of bootstrap samples is labelled "Samples" instead of "Replicates".
- The bootstrap interval is now computed for large data sets. In 0.1.0 it was empty, with an R error message, once the number of cases times the number of controls passed 2,147,483,647 (for example 46,341 of each).
- When the DeLong standard error cannot be computed for a marker that has two or more cases in each class, the AUC table gives a footnote that says so.
- The curve in the module icon stays inside its box.

## 0.1.0 - 2026-11-26

- New analysis: ROC Curve, for one or more markers and an outcome with two levels.
- AUC table with the DeLong standard error, a confidence interval and a test of AUC = 0.5.
- Choice of DeLong or stratified bootstrap confidence interval for the AUC, with a seed option.
- Choice of direction: higher or lower marker values indicate positive.
- Cut-off table with sensitivity and specificity (Clopper-Pearson intervals), PPV, NPV, likelihood ratios, accuracy and Youden's J.
- Five cut-off criteria: Youden index, closest to (0, 1), fixed sensitivity, fixed specificity and a threshold you enter. Under the Youden index and closest to (0, 1), tied thresholds each get a row; the fixed criteria break ties.
- Sample or custom prevalence for PPV and NPV.
- Paired DeLong comparison of the AUCs of each pair of markers.
- Coordinates table with every threshold.
- Thresholds in the cut-off and coordinates tables get enough significant digits (up to 15) to show each one between the observed values it separates.
- ROC curve plot and sensitivity and specificity by threshold plot.
- Help page and help text on every option.
- Example file `examples/rocCurve-aSAH.jasp` with the aSAH data.
