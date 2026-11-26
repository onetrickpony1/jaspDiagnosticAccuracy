ROC Curve
==========================

The ROC curve shows how well one or more test variables (markers) separate the two classes of an outcome. An example is patients with and without a disease. The analysis gives the area under the curve (AUC) with a confidence interval. If you ask for them, it also gives more: the accuracy at a chosen cut-off, a table of every threshold, a paired comparison of the markers, and plots.

The AUC is the probability that the marker ranks a randomly chosen positive case above a randomly chosen negative case, in the direction you set. Ties count as one half. An AUC of 0.5 means the marker does no better than chance.

### Input
-------

#### Assignment Box
- Outcome: the true class of each case. The variable must have exactly two levels after rows with a missing outcome are removed. Levels with no cases are dropped first.
- Positive level: the level of the outcome that counts as positive (the cases). The other level is negative (the controls). By default this is the second level.
- Test Variables: one or more markers (scale or ordinal). Each marker gets its own ROC curve. An ordinal marker with numbers as labels uses those numbers. An ordinal marker with text labels uses its level order (1, 2, ...), and a footnote says so.

#### Direction
- Higher values indicate positive: a case is classed as positive when its marker value is at or above the threshold. This is the default.
- Lower values indicate positive: a case is classed as positive when its marker value is at or below the threshold.

The direction is never chosen from the data.

#### AUC Confidence Interval
- DeLong: the interval from the DeLong standard error. This is the default.
- Bootstrap: a percentile interval from stratified bootstrap samples: cases and controls are resampled separately.
  - Replicates: the number of bootstrap samples, from 100 to 100000. The default is 2000.

The standard error, z and p always use the DeLong method, whichever interval you choose.

#### Confidence level
- The confidence level for every interval in the output: the AUC, sensitivity, specificity and the AUC differences. The default is 95%.

#### Cut-off table
- Cut-off table: a table with the chosen threshold of each marker. Off by default.
- Cut-off Criterion (enabled when the cut-off table is on): how the threshold is chosen.
  - Youden index: the threshold with the highest sensitivity + specificity - 1. This is the default.
  - Closest to (0, 1): the threshold closest to the top-left corner of the ROC plot, that is, the lowest (1 - sensitivity)² + (1 - specificity)².
  - Fixed sensitivity: among the thresholds with at least this sensitivity, the one with the highest specificity; if several share it, the one among them with the highest sensitivity. The default value is 0.9.
  - Fixed specificity: among the thresholds with at least this specificity, the one with the highest sensitivity; if several share it, the one among them with the highest specificity. The default value is 0.9.
  - Threshold: classify every case with the value you enter, using the rule set by the direction. The same value is used for every marker.

Except for Threshold, the criteria choose from the thresholds of pROC: the midpoints between neighbouring observed values, plus -∞ and ∞. When several thresholds are exactly equally good, each gets its own row. This can happen with Youden index and Closest to (0, 1). Fixed sensitivity and Fixed specificity break ties as above, so they give one row.

#### Prevalence for PPV and NPV
Enabled when the cut-off table or the coordinates table is on.
- Sample prevalence: the share of positive cases among each marker's complete cases. This is the default.
- Custom: a prevalence you enter, between 0 and 1, for example from the population where the test will be used.

#### Tables
- Compare markers: compares the AUCs of each pair of markers with DeLong's test for paired ROC curves. Enabled with two or more test variables.
- Coordinates table: every threshold of each marker with its sensitivity, specificity, PPV and NPV.

#### Plots
- ROC curve: the ROC curves of all markers in one plot.
- Sensitivity and specificity by threshold: both against the threshold, one panel per marker.

#### Set seed
- Makes the bootstrap interval repeatable. The seed is set again before each marker.

### Classification rule
-------
At a threshold t, the rule depends on the direction. If higher values indicate positive, a case counts as positive when its marker value is at or above t. If lower values indicate positive, it counts as positive when its value is at or below t. So a value equal to t counts as positive. This is the rule pROC uses. The cut-off and coordinates tables state the rule in a footnote.

### Missing Values
-------
- The AUC table, the cut-off table, the coordinates table and the plots use, for each marker, the rows where the outcome and that marker are both present. So the numbers of cases can differ between markers. A footnote in the AUC table gives the number of rows left out for each marker.
- The comparison table uses only the rows where the outcome and every marker are present. It has its own n column, and a footnote gives the number of rows left out.
- Infinite values in a marker are treated as missing for that marker, with a footnote.

### Output
-------

#### Area Under the ROC Curve
- Marker: the test variable.
- n Positive, n Negative: the numbers of positive and negative cases used for this marker.
- AUC: the area under the ROC curve.
- SE: the DeLong standard error of the AUC. The SE and the interval need at least two cases in each class. Without them the SE, the interval, z and p are left empty, with a footnote.
- Lower, Upper: the confidence interval for the AUC (DeLong or bootstrap, as chosen).
- z, p: a two-sided Wald test of AUC = 0.5 with the DeLong standard error: z = (AUC - 0.5) / SE.

A marker that is constant, or that has no cases in one class, gets empty cells and a footnote. The other markers still run.

When the DeLong standard error is zero, the classes do not overlap. Then z and p are not defined. They are left empty, with a footnote.

With JASP's default number settings, a p value below .001 shows as < .001, in this table and in the comparison table. If you set a fixed number of decimals in JASP's preferences, the cut-off is 1 divided by 10 to the power of that number instead (0.01 with 2 decimals). A p value below the smallest positive number R can store (about 5 × 10⁻³²⁴) is stored as 0. A footnote then says so. With JASP's default number settings such a p shows as < .001 too.

#### Cut-off
- Marker and Criterion: the test variable and how the threshold was chosen.
- Threshold: the chosen threshold. See "Threshold digits" below.
- Sensitivity: the share of positive cases classed as positive, TP / (TP + FN), with a Clopper-Pearson confidence interval.
- Specificity: the share of negative cases classed as negative, TN / (TN + FP), with a Clopper-Pearson confidence interval.
- PPV: the positive predictive value, the probability that a case classed as positive is positive, at the chosen prevalence. It is computed from sensitivity (se), specificity (sp) and the prevalence p: se p / (se p + (1 - sp)(1 - p)).
- NPV: the negative predictive value, the probability that a case classed as negative is negative, at the chosen prevalence: sp (1 - p) / (sp (1 - p) + (1 - se) p).
- LR+: the positive likelihood ratio, se / (1 - sp).
- LR−: the negative likelihood ratio, (1 - se) / sp.
- Accuracy: the share of all cases classed correctly, (TP + TN) / n, in the sample.
- Youden's J: se + sp - 1.

A value that would need a division by zero is left empty, with a footnote.

#### Paired Comparison of AUCs
- Marker 1, Marker 2: the pair of markers, in the order they were entered.
- n: the number of rows used (the outcome and every marker present).
- Difference: the AUC of marker 1 minus the AUC of marker 2.
- Lower, Upper: the confidence interval for the difference.
- z, p: DeLong's test for two paired ROC curves. When the standard error of the difference is zero, z and p are left empty, with a footnote.

The comparison needs at least two positive and two negative cases among the rows it uses. If those rows have fewer, every pair gets empty cells and a footnote. The footnote gives the numbers of positive and negative cases, and names any marker that has fewer than two cases of that class even on its own rows.

#### Coordinates
- Marker, Threshold: every threshold of each marker, including -∞ and ∞.
- Sensitivity, Specificity, PPV, NPV: as in the cut-off table, at that threshold.

#### Threshold digits
In the cut-off table and in the coordinates table, all thresholds of a marker get the same number of significant digits. It is the smallest number, from 4 to 15, with which each threshold, as JASP rounds it, lies strictly between its neighbouring observed values and differs from the other shown thresholds. The cut-off table counts only its chosen thresholds, so the two tables can use different numbers. When two observed values are too close for a midpoint, pROC uses one of them as the threshold; such a threshold must show exactly. If even 15 digits are not enough, a footnote names the marker and says its thresholds are rounded for display. A threshold you enter (the Threshold criterion) is shown with all its digits (at least 4 significant digits, so 0.2 shows as 0.2000). A threshold of 1e6 or more (or -1e6 or less), entered or chosen, shows in exponent form. This applies with JASP's default number display. If you set a fixed number of decimals in JASP's preferences, JASP uses those instead.

#### ROC Curve
- Sensitivity against 1 - specificity for every marker, with the chance diagonal as a dashed line.
- When the cut-off table is on, circles mark the cut-off chosen for each marker. A marker with tied cut-offs gets a circle for each of them. A note under the plot says what the circles mark.

#### Sensitivity and Specificity by Threshold
- Sensitivity and specificity against the threshold, as step curves. With more than one marker, each marker has its own panel.
- The x axis runs from the first to the last of a set of round tick values that cover the marker's values. When a value lies outside them by no more than a rounding error, the axis reaches that value.
- When a marker's values span less than 1/100000 of their size (for example timestamps within a few hours), the axis shows the threshold minus (or plus) a round number, and the axis title gives that number. With more than one marker, the panel title gives it.

A marker without a curve (for example a constant marker, or one with no cases in a class) is left out of both plots. A note under the plot names it. When no marker has a curve, the plot shows an error message instead.

### References
-------
- Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*(4), 404-413.
- DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: A nonparametric approach. *Biometrics, 44*(3), 837-845.
- Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. *Radiology, 143*(1), 29-36.
- Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J.-C., & Müller, M. (2011). pROC: An open-source package for R and S+ to analyze and compare ROC curves. *BMC Bioinformatics, 12*, 77.
- Youden, W. J. (1950). Index for rating diagnostic tests. *Cancer, 3*(1), 32-35.

### R-packages
---
- pROC
- stats
