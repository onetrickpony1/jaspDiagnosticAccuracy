# Validation: reference values for the ROC Curve analysis

These values are worked out without the module's code. The tests in `tests/testthat/test-rocCurve.R` use them.

## Files

| File | What it does |
|---|---|
| `delong.R` | DeLong, DeLong & Clarke-Pearson (1988) in base R: placement values, AUC variance, paired difference. Also the Hanley & McNeil (1982) Table II calculation. No pROC code. |
| `cutoffs.R` | Thresholds (midpoints plus -Inf and Inf), confusion counts, sensitivity and specificity with Clopper-Pearson CIs (`binom.test`), PPV and NPV by Bayes' rule, LR+, LR-, accuracy, Youden's J, and the four cut-off criteria with ties kept. Base R only. |
| `reference.R` | Writes the data sets, computes every value with the two files above, checks each against pROC, ROCR, pROC's own test files, Hanley & McNeil and SAS, and stops on any disagreement. Writes `reference-values.csv`. |
| `reference-values.csv` | Every value (id, value, source). |

Run it from the repository root, in an R that has pROC 1.19.0.1 and ROCR 1.0-11:

```sh
Rscript dev/validation/reference.R
```

The R of the JASP 0.99.0 Flatpak (R 4.5.2) has both: pROC in the `jaspMetaAnalysis` library and ROCR in the `jaspMachineLearning` library. For example:

```sh
flatpak run --filesystem=$PWD --command=Rscript org.jaspstats.JASP -e '.libPaths(c("/app/Modules/module_libs/jaspMetaAnalysis", "/app/Modules/module_libs/jaspMachineLearning", .libPaths())); source("dev/validation/reference.R")'
```

The run on 2026-11-26 made 146 checks. All of them agreed, at a relative tolerance of 1e-12 unless stated.

## Data sets (in `tests/testthat/`)

| File | Rows | Source |
|---|---|---|
| `aSAH.csv` | 113 | pROC's `aSAH` (pROC 1.19.0.1, GPL >= 3). Original study: Turck et al. (2010), *Intensive Care Medicine* 36(1), 107-115 (pROC `man/aSAH.Rd`). Columns `outcome`, `gender`, `age`, `wfns` (as the integer 1 to 5), `s100b`, `ndka`. |
| `hanleyMcNeil1982.csv` | 109 | Hanley & McNeil (1982), *Radiology* 143, 29-36, Table I (p. 30): ratings 1 to 5 of 109 CT images. Normal: 33, 6, 6, 11, 2. Abnormal: 3, 2, 2, 11, 33. One row per image. |
| `delongOvarian.csv` | 43 | The DeLong et al. (1988) example as printed in the SAS/STAT 9.3 User's Guide, PROC LOGISTIC, Example 53.8 "Comparing Receiver Operating Characteristic Curves" (https://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/statug_logistic_sect066.htm). `totscore` is stored as 10 minus the listed score, as the SAS DATA step does. `popind = 0` is the event. |

## Sources used for comparison

- pROC 1.19.0.1 source at tag `v1.19.0.1`, commit 983e818c433b8fdac7beb68179619b9300eba2e5 (github.com/xrobin/pROC):
  - `tests/testthat/test-auc.R` lines 7 and 10: AUC of wfns and ndka.
  - `tests/testthat/test-var.R` lines 6 to 8: DeLong variance of wfns, ndka, s100b.
  - `tests/testthat/test-ci.auc.R` line 6: DeLong CI of ndka.
  - `tests/testthat/test-roc.test.R` lines 28 to 49: paired DeLong `roc.test` z, p and CI for (wfns, s100b), (wfns, ndka), (ndka, s100b).
  - The pROC man pages (`roc.test.Rd`, `ci.auc.Rd`, `aSAH.Rd`) run examples on aSAH but print no numbers, so the test files above are the documented values.
- pROC run live in JASP's R (functions `roc`, `auc`, `var`, `cov`, `ci.auc`, `roc.test`, `coords`), always with explicit `levels` and `direction` (never `"auto"`).
- ROCR 1.0-11 (`prediction`, `performance`): every ROC point and the AUC.
- Hanley & McNeil (1982), Table II (p. 31): W = 2,642 / (58 x 51) = 0.893, Q1 = 0.8182, Q2 = 0.8313, SE = 0.032. Copy read: https://math.ucdavis.edu/~saito/data/roc/hanley-mcneil-auc.pdf.
- SAS/STAT 9.3, Example 53.8, Outputs 53.8.9 (AUC, SE, 95% Wald CI per marker) and 53.8.12 (paired contrasts).
- Robin et al. (2011), *BMC Bioinformatics* 12, 77, read as Europe PMC full text (PMC3068975).

## Values

"Hand" means computed by `delong.R` or `cutoffs.R`. Full precision is in `reference-values.csv`.

### AUC table, aSAH (positive = Poor, 41 positive, 72 negative)

| Marker, direction | AUC | DeLong SE | 95% CI | z | p | Agreement |
|---|---|---|---|---|---|---|
| s100b, higher | 0.731369 | 0.051659 | 0.630118 to 0.832619 | 4.4787 | 7.51e-06 | hand = pROC `auc`, `var`, `ci.auc`; var = pROC test-var.R |
| ndka, higher | 0.611958 | 0.056487 | 0.501245 to 0.722671 | 1.9820 | 0.0475 | hand = pROC; AUC = test-auc.R l. 10; CI = test-ci.auc.R l. 6 |
| wfns, higher | 0.823679 | 0.038339 | 0.748535 to 0.898823 | 8.4424 | 3.11e-17 | hand = pROC; AUC = test-auc.R l. 7 |
| s100b, lower | 0.268631 | 0.051659 | 0.167381 to 0.369882 | -4.4787 | 7.51e-06 | hand = pROC `direction = ">"`; AUC = 1 - higher |
| ndka, lower | 0.388042 | 0.056487 | 0.277329 to 0.498755 | -1.9820 | 0.0475 | same |
| wfns, lower | 0.176321 | 0.038339 | 0.101177 to 0.251465 | -8.4424 | 3.11e-17 | same |
| s100b, higher, 90% CI | | | 0.646397 to 0.816341 | | | hand = pROC `conf.level = 0.9` |

ROCR gives the same AUC and the same set of ROC points for all six rows.

### Paired comparisons (DeLong), aSAH, all 113 cases

| Pair (first - second) | Difference | 95% CI | z | p | Agreement |
|---|---|---|---|---|---|---|
| wfns - s100b | 0.092310 | 0.010406 to 0.174214 | 2.2090 | 0.02718 | hand = `roc.test` = test-roc.test.R l. 28-31 |
| wfns - ndka | 0.211721 | 0.063401 to 0.360041 | 2.7978 | 0.005146 | hand = `roc.test` = test-roc.test.R l. 37-40 |
| ndka - s100b | -0.119411 | -0.287692 to 0.048871 | -1.3908 | 0.1643 | hand = `roc.test` = test-roc.test.R l. 46-49 |

The tests use the order s100b, ndka, wfns, so the signs flip for (s100b, wfns) and (ndka, wfns). The covariances also equal pROC's `cov()`.

### Missing values (aSAH with ndka missing in rows 1 to 5, s100b in rows 6 to 8, outcome in row 9)

| Quantity | Value | Agreement |
|---|---|---|
| s100b: n positive, n negative, AUC, SE | 39, 70, 0.745788, 0.052428 | hand, per-marker complete cases |
| ndka: n positive, n negative, AUC, SE | 40, 67, 0.595896, 0.058283 | hand, per-marker complete cases |
| s100b - ndka, listwise: n, difference, CI, z, p | 104, 0.158293, -0.019086 to 0.335672, 1.7491, 0.08028 | hand = `roc.test` |

### Hanley & McNeil (1982), CT ratings (positive = abnormal)

| Quantity | Value | Agreement |
|---|---|---|
| AUC | 0.893171 (= 2,642 / 2,958) | hand = pROC = Table II W = 0.893 |
| Hanley-McNeil SE | 0.031990 | hand reproduces Table II: Q1 0.8182, Q2 0.8313, SE 0.032 |
| DeLong SE (what the module reports) | 0.030724 | hand = pROC |
| DeLong 95% CI, z | 0.832952 to 0.953390, 12.797 | hand |

The paper's SE uses its own estimator, so the module's SE is not expected to equal 0.032.

### DeLong et al. (1988) example via SAS (positive = popind 0, 12 positive, 31 negative, direction lower)

| Quantity | Hand (= pROC) | SAS (4 dp) |
|---|---|---|
| Albumin AUC, SE, CI | 0.736559, 0.092694, 0.554882 to 0.918237 | 0.7366, 0.0927, 0.5549 to 0.9182 |
| K-G score AUC, SE, CI | 0.725806, 0.102822, 0.524280 to 0.927333 | 0.7258, 0.1028, 0.5243 to 0.9273 |
| Total protein AUC, SE, CI | 0.647849, 0.100047, 0.451760 to 0.843939 | 0.6478, 0.1000, 0.4518 to 0.8439 |
| Albumin - K-G: difference, CI, chi-square | 0.010753, -0.176117 to 0.197623, 0.0127 | 0.0108, -0.1761 to 0.1976, 0.0127 |
| Total protein - K-G: difference, CI, chi-square | -0.077957, -0.282988 to 0.127074, 0.5554 | -0.0780, -0.2830 to 0.1271, 0.5554 |
| Albumin - total protein: difference, CI, z, p | 0.088710, -0.020639 to 0.198058, 1.5900, 0.1118 | not in SAS; hand = `roc.test` |

SAS reports the AUC each covariate's logistic model gives. All three match with "lower values indicate positive", so one direction serves the whole comparison.

### Cut-off table, aSAH (sample prevalence 41/113 unless stated)

Thresholds and the sensitivity and specificity at every threshold equal pROC's `coords(..., "all")` (pROC lists them in decreasing order for direction ">"; compared after sorting). The best thresholds for Youden and closest to (0, 1) equal pROC's `coords(..., "best")` for all three markers.

| Case | Threshold | Se (95% CI) | Sp (95% CI) | PPV | NPV | LR+ | LR- | Accuracy | J |
|---|---|---|---|---|---|---|---|---|---|
| s100b, Youden | 0.205 | 0.6341 (0.4694, 0.7788) | 0.8056 (0.6953, 0.8894) | 0.6500 | 0.7945 | 3.2613 | 0.4542 | 0.7434 | 0.4397 |
| ndka, Youden | 11.08 | 0.7073 (0.5446, 0.8387) | 0.5139 (0.3931, 0.6335) | 0.4531 | 0.7551 | 1.4551 | 0.5695 | 0.5841 | 0.2212 |
| wfns, Youden | 3.5 | 0.6341 (0.4694, 0.7788) | 0.8333 (0.7270, 0.9108) | 0.6842 | 0.8000 | 3.8049 | 0.4390 | 0.7611 | 0.4675 |
| wfns, closest to (0, 1) | 2.5 | 0.6585 (0.4941, 0.7992) | 0.7917 (0.6798, 0.8784) | 0.6429 | 0.8028 | 3.1610 | 0.4313 | 0.7434 | 0.4502 |
| s100b, fixed sensitivity 0.9 | 0.075 | 0.9024 (0.7687, 0.9728) | 0.2222 (0.1327, 0.3356) | 0.3978 | 0.8000 | 1.1603 | 0.4390 | 0.4690 | 0.1247 |
| s100b, fixed specificity 0.9 | 0.435 | 0.3902 (0.2420, 0.5550) | 0.9028 (0.8099, 0.9600) | 0.6957 | 0.7222 | 4.0139 | 0.6754 | 0.7168 | 0.2930 |
| wfns, threshold 4.5 | 4.5 | 0.4390 (0.2847, 0.6025) | 0.9444 (0.8638, 0.9847) | 0.8182 | 0.7473 | 7.9024 | 0.5940 | 0.7611 | 0.3835 |
| wfns, threshold 4 (equal to an observed value) | 4 | as wfns Youden | | | | | | | |
| wfns, lower, threshold 2 | 2 | 0.3415 (0.2008, 0.5059) | 0.2083 (0.1216, 0.3202) | 0.1972 | 0.3571 | 0.4313 | 3.1610 | 0.2566 | -0.4502 |
| s100b, Youden, prevalence 0.1 | 0.205 | as s100b Youden | | 0.2660 | 0.9520 | | | | |
| s100b, Youden, 90% CIs | 0.205 | (0.4939, 0.7592) | (0.7128, 0.8784) | | | | | | |
| s100b, lower, Youden | 0.035 | 0.0244 (0.0006, 0.1286) | 1 (0.9501, 1) | 1 | 0.6429 | undefined | 0.9756 | 0.6460 | 0.0244 |

Rule: "higher" counts a case as positive when marker >= threshold, "lower" when marker <= threshold. pROC `man/roc.Rd` lines 83 to 87 (same commit): "<": "observations are positive when they are greater than or equal (>=) to the threshold"; ">": "smaller than or equal (<=)". Threshold 4 and 3.5 give the same split for wfns (values 1 to 5), which shows that equality counts as positive.

Ties: x = 1, 2, 3, 4 with cases at 2 and 4 has Youden J = 0.5 at 1.5 and at 3.5. For wfns with direction "lower", every finite threshold has J < 0, so the best J = 0 is shared by -Inf and Inf.

Coordinates for wfns (higher): thresholds -Inf, 1.5, 2.5, 3.5, 4.5, Inf; sensitivity 1, 0.9512, 0.6585, 0.6341, 0.4390, 0; specificity 0, 0.5139, 0.7917, 0.8333, 0.9444, 1. PPV and NPV at the sample prevalence are in the CSV (PPV is undefined at Inf, NPV at -Inf).

### Cross-checks without a table row

| Check | Result |
|---|---|
| Logistic regression with one predictor (`glm`, binomial): AUC and DeLong SE of the fitted probabilities | equal to the marker's AUC and SE for s100b, ndka and wfns (all slopes positive) |
| Tied values: x = 1, 1, 2, 2, 3, 3, 3, 4 with cases at positions 2, 4, 7, 8 | AUC 0.5625, DeLong SE 0.228218, hand = pROC |
| s100b times 1e12 | pROC AUC and SE equal those of s100b |
| One positive case (x = 4 positive; 1, 2, 3, 5 negative) | AUC 0.75; DeLong variance undefined (`var()` of one value is NA); pROC `var()` and `ci.auc()` also give NA |
| Bootstrap CI, s100b, `set.seed(1)` then `ci.auc(boot.n = 2000, boot.stratified = TRUE)` | 0.6243 to 0.8269. Not pinned in the tests: the result depends on any random numbers drawn before the call. |

## What could not be sourced

- **Robin et al. (2011)** reports partial AUCs (specificity 90 to 100%: WFNS 3.1%, S100B 3.0%), a WFNS cut-off of 4.5 and bootstrap CIs, not full AUCs. The shipped `aSAH` data give 3.34% for WFNS (also pROC `test-auc.R` line 16: 0.0334417) and 3.28% for S100B (`reference.R`, section 10), so the paper's case study does not use exactly these 113 rows. The tests therefore use pROC's own test values for aSAH, not the paper's numbers. The cut-off 4.5 is tested for wfns, but the paper prints only its CIs.
- **DeLong et al. (1988)**: read on 2026-11-25 (Biometrics 44(3), 837-845, JSTOR 2531595; a private copy, not kept in this repo). The paper does not print the raw data. Its example (p. 841-844) has 49 patients (12 successes, 37 failures) and gives only summaries: AUCs of .69 (K-G, as 12 minus the score), .72 (albumin) and .65 (total protein) (p. 842); the covariance matrix in Table 2 (p. 843); the contrast K-G minus the mean of ALB and TP, .004 with SD .116 and 95% CI (-.223, .231); and a 2-df chi-square of 1.51, p = .47 (p. 844). These cannot be reproduced without the data. The SAS User's Guide reproduces the example with 43 patients and cites the paper. The values above are checked against SAS's own DeLong output, not against the 1988 paper. The paper is the source for the method (p. 840-841: structural components, S = S10/m + S01/n), which `delong.R` follows.
- **Hanley & McNeil (1982)**: the counts and W come from the paper's Table I and Table II. Its SE is a different estimator from DeLong, so only the AUC is a direct check of the module.
