# Changelog

## Unreleased

## 0.1.1 - 2026-11-27

- Three example files in `examples/`: the aSAH biomarkers, the CT ratings of Hanley and McNeil (1982), and a simulated screening study. The aSAH example is saved again with this version.
- Screenshots in the README.
- Installable module files (`.JASPModule`) for Linux (Flatpak), Windows and macOS (Apple silicon), on the releases page. The README describes how to install them. In JASP 0.99.0 the **Install Local Module** item needs Developer mode.
- Clear error messages when an option is outside its range (confidence level, number of bootstrap samples, fixed sensitivity or specificity, threshold, custom prevalence), instead of a crash or a missing row.
- Empty outcome cells count as missing, as elsewhere in JASP.
- Footnotes use the module's own messages when pROC cannot compute a curve, interval or comparison.
- Faster coordinates table for large data sets. Its time now grows in line with the number of rows, not with its square.
- Faster sensitivity and specificity by threshold plot for large data sets.
- DESCRIPTION has a `Website` field. JASP takes the module's website from it, for example in the message shown for a saved file whose module is not installed.
- Version 0.1.1. Two options have new names: `bootstrapReplicates` is now `bootstrapSamples`, and `compareMarkers` is now `comparisonTable`. This matters for R syntax and scripts that set options by name. Files saved with 0.1.0 keep their settings: `inst/Upgrades.qml` renames the two options when JASP opens them.
- The help page is rewritten, in the format of JASP's own modules. It now has a section on the assumptions, gives the default of every option, and lists all the notes each table can show and when they appear. It also explains what the removed notes said, and what the circles on the ROC plot mark.
- A new confidence level, or a switch to DeLong and back, no longer draws the bootstrap samples again, so the interval updates at once. The analysis keeps the bootstrap AUCs while the file is open, and draws them again only when the variables, the positive level, the direction, the number of bootstrap samples, the "Set seed" option or the seed change. So without a seed, a new level uses the same random samples. If "Store analysis state and plot in jasp-files" is on in JASP's preferences, a saved file holds the bootstrap AUCs too: up to about 800 KB per marker with 100,000 bootstrap samples.
- Faster cut-off table and paired comparison for large data sets. For 10 markers and 100,000 rows (Youden index, closest to (0, 1) and the comparison), about 1 to 1.5 s instead of 5 to 7.5 s, timed in R on one Linux machine.
- The tooltip of the cut-off criterion is shorter. Its other facts are now on the help page.
- The field for the number of bootstrap samples is labelled "Samples" instead of "Replicates".
- The bootstrap interval is now computed for large data sets. In 0.1.0 it was empty, with an R error message, once the number of cases times the number of controls passed 2,147,483,647 (for example 46,341 of each).
- When the DeLong standard error cannot be computed for a marker that has two or more cases in each class, the AUC table gives a footnote that says so.
- A new module icon, drawn in the style of the icons of JASP's own modules: an ROC curve above the chance diagonal, with a cut-off point.
- Fewer notes, as in JASP's own modules. By default the AUC table has one note: the positive level and the direction. These notes are gone, and the help page explains each instead: the DeLong interval, the test of AUC = 0.5, ties between the classes, the classification rule, the Clopper-Pearson intervals, the sample prevalence and pROC's thresholds. The bootstrap note now reads "Percentile bootstrap interval from 2,000 stratified bootstrap samples." A custom prevalence gets a note in the cut-off and coordinates tables. The comparison table's note on left-out rows appears only when rows were left out.
- The plots no longer have text inside the image. A marker without a curve is left out of the plots, and the AUC table gives the reason in a footnote, as before. With more than one marker, the ROC plot has a legend even when only one curve remains, so that curve is named. The help page says what the circles on the ROC plot mark.
- The legend of the ROC plot no longer cuts off the end of marker names (before, a name such as "markerA" lost its last letter). The plot now has room at the right of the legend, and grows wider for names longer than 7 characters, so the curves keep their size.

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
