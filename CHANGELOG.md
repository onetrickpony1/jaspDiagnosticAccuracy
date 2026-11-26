# Changelog

## Unreleased

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
