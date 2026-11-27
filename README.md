<div align="right">

<b>Maintainer:</b> <a href="https://github.com/onetrickpony1/">onetrickpony1</a>

</div>

# The Diagnostic Accuracy Module

## Overview

<img src='inst/icons/diagnosticAccuracy.svg' width='148' height='112' align='right'/>

**JASP Diagnostic Accuracy module** is a community add-on module for JASP. It has one analysis, **ROC Curve**. It is not made or endorsed by the JASP team.

**Work in progress** (version 0.1.1): options and output may still change.

An ROC curve shows how well a test score separates two groups, for example patients with and without a disease. The score can be a lab value, a rating or a risk score. In this module the score is called a marker.

The analysis answers questions such as:

- How well does the marker separate the two groups? The area under the ROC curve (AUC) gives the answer as one number. It is the probability that the marker ranks a randomly chosen positive case above a randomly chosen negative case, in the direction you set. Ties count as one half.
- Which cut-off should I use, and how good is the test at that cut-off? The cut-off table gives sensitivity, specificity, predictive values and likelihood ratios.
- Is one marker better than another on the same cases? The paired comparison tests the difference between two AUCs.

## Screenshots

The input form and the default output, the AUC table (example `rocCurve-aSAH.jasp`):

<img src='screenshots/01-overview.png' alt='The ROC Curve form beside the AUC table for s100b and ndka' width='100%'/>

<table>
<tr>
<td width='50%'><img src='screenshots/02-roc-plot.png' alt='ROC curves of s100b and ndka with the Youden cut-offs marked'/><br/><sub>ROC curves, with the chosen cut-off of each marker circled.</sub></td>
<td width='50%'><img src='screenshots/08-screening-threshold-plot.png' alt='Sensitivity and specificity by threshold for two simulated markers'/><br/><sub>Sensitivity and specificity by threshold (simulated data).</sub></td>
</tr>
</table>

The cut-off table at the Youden index, with confidence intervals, predictive values and likelihood ratios:

<img src='screenshots/03-cutoff-table-youden.png' alt='Cut-off table for s100b and ndka at the Youden index' width='100%'/>

The paired comparison of two AUCs (DeLong's test):

<img src='screenshots/04-paired-comparison.png' alt='Paired comparison of the AUCs of s100b and ndka'/>

<details>
<summary>More: an ordinal rating, and a simulated screening study</summary>

Radiologists' ratings of 109 CT images, 1 to 5 (Hanley and McNeil, 1982). The AUC, 0.893, is the value in the paper:

<img src='screenshots/05-hanley-mcneil-ordinal.png' alt='AUC and cut-off tables for the CT ratings' width='100%'/>

A simulated screening study of 1,000 people, 100 with the condition, and two markers:

<img src='screenshots/06-screening-tables.png' alt='AUC, cut-off and comparison tables for the simulated markers' width='100%'/>

<img src='screenshots/07-screening-roc-plot.png' alt='ROC curves of the two simulated markers'/>

</details>

The top of the help page, which the (i) button of the analysis opens:

<img src='screenshots/09-help.png' alt='The top of the help page of ROC Curve, with the introduction and the assumptions' width='50%'/>

## Analysis: ROC Curve

### Input

- **Outcome**: the true class of each case. It must have exactly two levels after rows with a missing outcome are removed.
- **Positive level**: the level that counts as positive. By default this is the second level.
- **Test Variables**: one or more markers (scale or ordinal).
- **Direction**: "Higher values indicate positive" (the default) or "Lower values indicate positive". The direction is never chosen from the data.
- **AUC Confidence Interval**: DeLong (the default) or a stratified bootstrap (100 to 100,000 bootstrap samples, 2,000 by default).
- **Confidence level**: 95% by default. It applies to every interval in the output.
- **Cut-off table** (off by default), with the **Cut-off Criterion**: Youden index (the default), closest to (0, 1), fixed sensitivity, fixed specificity, or a threshold you enter.
- **Prevalence for PPV and NPV**: the sample prevalence (the default) or a value you enter.
- **Compare markers** (off by default; needs two or more markers).
- **Coordinates table** (off by default).
- **Plots** (off by default): the ROC curve, and sensitivity and specificity by threshold.
- **Set seed**: makes the bootstrap interval repeatable.

### Output

With the default options the output is the AUC table only. The other tables and the plots appear when you tick them.

- **Area Under the ROC Curve**: per marker, the number of positive and negative cases, the AUC, its DeLong standard error, the confidence interval, and z and p for the test of AUC = 0.5.
- **Cut-off**: per marker, the chosen threshold with sensitivity and specificity (each with a Clopper-Pearson interval), PPV, NPV, LR+, LR−, accuracy and Youden's J. When several thresholds are exactly equally good under the Youden index or closest to (0, 1), each gets its own row. The fixed sensitivity and fixed specificity criteria break ties, so they give one row.
- **Paired Comparison of AUCs**: DeLong's test for each pair of markers, with the difference, its confidence interval, z and p.
- **Coordinates**: every threshold of each marker with sensitivity, specificity, PPV and NPV.
- **ROC Curve** plot: all markers in one plot, with the chance diagonal. When the cut-off table is on, circles mark the chosen cut-offs, one for each tied cut-off.
- **Sensitivity and Specificity by Threshold** plot.

Footnotes are few. The AUC table gives the positive level and the direction, and the number of bootstrap samples under the bootstrap. The comparison table names the test. A custom prevalence gets a footnote in the cut-off and coordinates tables. Other footnotes appear only when they apply, for example when rows were left out, a marker has no curve or a cell is undefined. The plots have no text inside the image; a marker without a curve is left out of them. The help page (the (i) button in JASP) explains the methods, the classification rule and every column.

## R Packages

<img src='https://www.r-project.org/logo/Rlogo.svg' width='100' height='78' align='right'/>

- **pROC**: the ROC curves, the AUC and the DeLong placement values come from pROC; the DeLong standard error, interval and test use pROC's formulas ([pROC on CRAN](https://cran.r-project.org/package=pROC)).
- **stats** (part of R): Clopper-Pearson intervals with `binom.test`.

The bootstrap interval, the choice of cut-off and the statistics at a cut-off are the module's own R code. They use pROC's thresholds.

## Installation

The module is not in JASP's Module Library. There are two ways to install it.

### From a release file

The [releases page](https://github.com/onetrickpony1/jaspDiagnosticAccuracy/releases) has a `.JASPModule` file for Linux (Flatpak, x86-64), Windows (x86-64) and macOS (Apple silicon). Download the file for your system. Then, in JASP:

1. In JASP 0.99.0 the **Install Local Module** item shows only in Developer mode. Open Menu (☰), Preferences, Advanced, and tick **Developer mode**. On Windows this also turns off **Sandbox engines**, and JASP asks you to restart. You can tick Sandbox engines on again: the installed module works with it on. Restart JASP.
2. Click the **+** icon (top right), choose **Install Local Module** and pick the file. A **Diagnostic Accuracy** button appears in the ribbon.
3. After a restart of JASP the module may be unticked in the **+** menu, and its button gone from the ribbon. Tick it in the **+** menu, or open a `.jasp` file that uses it.

The Linux file was tested in the JASP 0.99.0 Flatpak and the Windows file in JASP 0.99.0 on Windows 10. The macOS file has not been tested in JASP.

### As a development module

You can also load the module from its source, as described in the JASP module development guide ("Configure JASP for development"). The steps below were tested with JASP 0.99.0 on Linux. Windows was not tested end to end; the Windows notes below come from an early trial. macOS was not tested.

#### Step 1: install the packages in R

Use the same R version as JASP:

- **Windows** (from an early trial): install R 4.5 (JASP 0.99.0 uses R 4.5.2) and [Rtools45](https://cran.r-project.org/bin/windows/Rtools/). Most packages come as binaries. A few, among them jaspBase and jaspGraphs, are built from source, and that needs Rtools. Start R 4.5 (not another installed version) in the folder of this repository.
- **Linux**: JASP is a Flatpak, so use the R inside it. By default, the JASP Flatpak can see only your home folder, so the repository must be inside it. Install the KDE SDK once with `flatpak remote-add --user --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo` and then `flatpak install --user flathub org.kde.Sdk//6.11`. 6.11 is the runtime of JASP 0.99.0. For another JASP release, `flatpak info org.jaspstats.JASP` shows it on the Sdk line. Then run `flatpak run --devel --command=R org.jaspstats.JASP` in the folder of this repository. Every package is built from source, which took about 7 minutes. The guide installs the beta branch of JASP; the stable branch worked.

renv starts on its own (`.Rprofile`). Then run these lines one at a time:

```r
renv::restore()
utils::install.packages(".", repos = NULL, type = "source")
.libPaths()[1]
```

The first line installs the packages pinned in `renv.lock`. It lists them and asks "Do you want to proceed? [Y/n]". Answer Y. If the packages are already installed, it says the library is already synchronised and does not ask. The second line installs the module. This differs from the guide, which uses `renv::install(".")`: that ignores `renv.lock` and installs newer versions of jaspBase, ggplot2, pROC and other packages.

The last line prints the library path. Copy the path between the quotes. The library is not in the project folder: renv keeps it in your user folder, under `~/.var/app/org.jaspstats.JASP/cache/R/renv/library/` on Linux and, from an early Windows trial, `%LOCALAPPDATA%\R\cache\R\renv\library\` on Windows.

jaspBase and jaspGraphs come from GitHub. The tests did not need a GitHub token. renv may print "GitHub authentication credentials are not available" and "failed to resolve remote 'jasp-stats/jaspGraphs'; skipping". In the tests both were harmless. If renv reports "error code 22" or a rate limit, set a token (GITHUB_PAT) as the guide describes.

#### Step 2: load the module in JASP

1. Open Menu (☰), Preferences, Advanced. Under "Modules options", tick **Developer mode**.
2. In the **Development module** section that appears, leave **Enable renv mode** ticked. Paste the path from step 1 into the field next to **Project library:** and enter `jaspDiagnosticAccuracy` as **Module name:**.
3. On Windows (from an early trial), ticking Developer mode also unticks **Sandbox engines** on the same page. JASP then says the change takes full effect after a restart. Restart JASP before the next step. In the trial, with Sandbox engines ticked, the **Dev: Diagnostic Accuracy** button still appeared, but analyses did not run, the results did not update, and JASP showed no error.
4. Go back to the main window, click the **+** icon (top right) and choose **Install Developer Module**. A **Dev: Diagnostic Accuracy** button appears in the ribbon. It is greyed out until data is loaded, and it opens ROC Curve.

On Windows (from an early trial), after each restart of JASP, choose **Install Developer Module** again. JASP keeps the path and the module name.

## Examples

The folder `examples/` has three `.jasp` files. Each one holds its data and a ready-made ROC Curve analysis. They are also on the [releases page](https://github.com/onetrickpony1/jaspDiagnosticAccuracy/releases). Open one in JASP with the module installed. The files hold saved results, which show even if the module did not load; to check that the module runs, change an option, for example the confidence level, and see that the tables update.

| File | Data | What it shows |
|---|---|---|
| `rocCurve-aSAH.jasp` | `aSAH` from pROC: 113 patients with an aneurysmal subarachnoid haemorrhage (Turck et al., 2010). Outcome `outcome`, positive level Poor. | Two biomarkers, s100b and ndka: the AUC table, the cut-off table (Youden index), the paired comparison and the ROC plot. |
| `rocCurve-hanleyMcNeil1982.jasp` | Ratings 1 to 5 of 109 CT images, from Table I of Hanley and McNeil (1982). Outcome `status`, positive level abnormal. | An ordinal rating: the AUC (0.893, as in the paper), the cut-off table and the ROC plot. JASP's default positive level here is normal; the example sets it to abnormal. |
| `rocCurve-screening-simulated.jasp` | **Simulated** data: 1,000 people, 100 with the condition, two markers. Made by `dev/examples/simulate-screening.py` (fixed seed). They describe no real people. | A strong and a weak marker: the AUC table, the cut-off table, the paired comparison, the ROC plot and the sensitivity and specificity by threshold plot. |

## Validation

The reference values were worked out without the module's code, in base R (`dev/validation/delong.R` and `cutoffs.R`). The table lists what was checked against a second source. The script is `dev/validation/reference.R`; the details, the data and every source are in `dev/validation/README.md`. A run on 2026-11-27 in the R of JASP 0.99.0 (R 4.5.2, pROC 1.19.0.1, ROCR 1.0-11) made 1,189 checks, and all of them agreed.

| What was checked | Against | Agreement |
|---|---|---|
| The base-R DeLong values for pROC's `aSAH` data: AUC (wfns, ndka), variance (s100b, ndka, wfns), CI (ndka) and three paired tests | the same values in pROC's own test files (`test-auc.R`, `test-var.R`, `test-ci.auc.R`, `test-roc.test.R`, pROC 1.19.0.1) | relative tolerance 1e-12 (1e-10 for the paired tests) |
| A DeLong (1988) calculation written in base R, without pROC code: AUC, SE, CI, covariances, and the paired tests (difference, CI, z, p) | pROC `auc`, `var`, `cov`, `ci.auc`, `roc.test` | relative tolerance 1e-12 |
| AUC of the CT ratings in Hanley & McNeil (1982), Table I | their Table II: W = 0.893 | 2,642 / 2,958 = 0.893171, equal to 3 decimals |
| The DeLong et al. (1988) example as reproduced in the SAS/STAT 9.3 User's Guide (Example 53.8): AUC, SE and CI of three markers, two paired contrasts | SAS Outputs 53.8.9 and 53.8.12 | equal at SAS's 4 decimals |
| Every ROC point and the AUC, three `aSAH` markers, both directions | ROCR 1.0-11 | relative tolerance 1e-12 |
| AUC and DeLong SE of the fitted probabilities of a one-predictor logistic regression (`glm`), three `aSAH` markers | the marker's own AUC and SE | relative tolerance 1e-12 |
| Sensitivity and specificity intervals | `binom.test` (Clopper-Pearson) | the reference values are `binom.test`'s |
| Random data: two seeds for each of n = 2, 3, 5, 20, 200, 2,000 and 20,000, three kinds of marker, both directions. AUC, DeLong SE and CI, the paired test, sensitivity and specificity at every threshold, and the best thresholds (Youden; closest to (0, 1) while n positive times n negative is below 2^26) | pROC `auc`, `var`, `ci.auc`, `roc.test`, `coords`; ROCR for the AUC up to n = 2,000 | relative tolerance 1e-12 (1e-9 for the paired z, p and CI) |
| The stratified bootstrap interval, n = 10, 200 and 2,000, both directions, with the same seed (base R) | pROC `ci.auc` | relative tolerance 1e-12 |
| One data set of 50,000 cases and 50,000 controls, one rounded marker and a noisy copy, direction "<": AUC, DeLong SE, the paired test, every threshold, the best Youden threshold and the bootstrap interval (100 samples, two seeds) | pROC `auc`, `var`, `roc.test`, `coords`, `ci.auc` | relative tolerance 1e-12 (1e-9 for the paired z, p and CI) |

pROC has no test of AUC = 0.5 for a single marker. So the single-marker z and p were checked by formula: z = (AUC - 0.5) / SE and its two-sided normal p, with the base-R DeLong SE, which agrees with pROC's `var`.

The tests in `tests/testthat/test-rocCurve.R` run the analysis on the same data and compare its output with these reference values. Most cells are compared at a relative tolerance of 1e-6.

The Hanley & McNeil standard error (0.032) uses their own formula, so the module's DeLong standard error (0.0307) is not expected to equal it.

## Known differences from pROC

- **Tied cut-offs.** When several thresholds are exactly equally good, the cut-off table lists all of them. pROC's `coords(x = "best")` compares the criterion as floating-point numbers and can miss a tie. An example uses the Youden index, with lower values positive. The cases are 1 3 3 5 7 1 2 6 2 7 1 6. The controls are 4 2 8 8 5 1 2 2 7 5 5 3. The module lists 1.5, 3.5 and 7.5; pROC lists only 3.5 and 7.5. At all three thresholds 14 of the 24 cases are classed correctly. No threshold does better, so the three are tied and the module is right.
- **Bootstrap.** The bootstrap interval is the module's own code, written to use the same random draws as pROC's `ci.auc(method = "bootstrap", boot.stratified = TRUE)`. With the same seed it agrees with pROC to rounding (within a relative 1e-12), not bit for bit.

## References

- Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*(4), 404-413. https://doi.org/10.1093/biomet/26.4.404
- DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: A nonparametric approach. *Biometrics, 44*(3), 837-845. https://doi.org/10.2307/2531595
- Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. *Radiology, 143*(1), 29-36. https://doi.org/10.1148/radiology.143.1.7063747
- Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J.-C., & Müller, M. (2011). pROC: An open-source package for R and S+ to analyze and compare ROC curves. *BMC Bioinformatics, 12*, 77. https://doi.org/10.1186/1471-2105-12-77
- Turck, N., Vutskits, L., Sanchez-Pena, P., Robin, X., Hainard, A., Gex-Fabry, M., Fouda, C., Bassem, H., Mueller, M., Lisacek, F., Puybasset, L., & Sanchez, J.-C. (2010). A multiparameter panel method for outcome prediction following aneurysmal subarachnoid hemorrhage. *Intensive Care Medicine, 36*(1), 107-115. https://doi.org/10.1007/s00134-009-1641-y
- Youden, W. J. (1950). Index for rating diagnostic tests. *Cancer, 3*(1), 32-35. https://doi.org/10.1002/1097-0142(1950)3:1<32::AID-CNCR2820030106>3.0.CO;2-3

## Licence

GPL (>= 3). See `LICENSE`.

The ROC calculations use pROC by Xavier Robin and co-authors (GPL >= 3). The test data set `aSAH` comes from pROC.
