# Reference values for the ROC Curve tests.
# Run from the repository root, in an R that has pROC 1.19.0.1 and ROCR 1.0-11:
#   Rscript dev/validation/reference.R
# Writes dev/validation/reference-values.csv and the data sets under tests/testthat/.
repo <- normalizePath(".")
if (!file.exists(file.path(repo, "dev", "validation", "reference.R")))
  stop("run this script from the repository root")
here <- file.path(repo, "dev", "validation")
tdir <- file.path(repo, "tests", "testthat")
source(file.path(here, "delong.R")); source(file.path(here, "cutoffs.R"))
suppressMessages(library(pROC)); suppressMessages(library(ROCR))
cat("R", R.version.string, "| pROC", format(packageVersion("pROC")), "| ROCR", format(packageVersion("ROCR")), "\n")
options(digits = 15)

ref <- data.frame(id = character(), value = numeric(), source = character(), stringsAsFactors = FALSE)
put <- function(id, value, source) {
  ref[nrow(ref) + 1, ] <<- list(id, unname(as.numeric(value)), source)
  cat(sprintf("%-55s %.12g   [%s]\n", id, value, source))
}
same <- function(a, b, what, tol = 1e-12) {
  ok <- isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = tol))
  cat(sprintf("CHECK %-60s %s\n", what, if (ok) "agree" else paste("DIFFER:", paste(format(a), collapse = " "), "vs", paste(format(b), collapse = " "))))
  if (!ok) stop("disagreement: ", what)
}

# ---------------------------------------------------------------- data sets
data(aSAH)
asah <- data.frame(outcome = as.character(aSAH$outcome), gender = as.character(aSAH$gender), age = aSAH$age,
                   wfns = as.integer(as.character(aSAH$wfns)), s100b = aSAH$s100b, ndka = aSAH$ndka)
write.csv(asah, file.path(tdir, "aSAH.csv"), row.names = FALSE)

# Hanley & McNeil (1982) Table I (p. 30): ratings 1 (definitely normal) to 5 (definitely abnormal).
hmN <- c(33, 6, 6, 11, 2); hmA <- c(3, 2, 2, 11, 33)
hm <- data.frame(status = rep(c("normal", "abnormal"), c(sum(hmN), sum(hmA))),
                 rating = c(rep(1:5, hmN), rep(1:5, hmA)))
write.csv(hm, file.path(tdir, "hanleyMcNeil1982.csv"), row.names = FALSE)

# DeLong et al. (1988) example as listed in the SAS/STAT 9.3 User's Guide, PROC LOGISTIC Example 53.8
# (43 patients; totscore is used as 10 - totscore, as in the SAS DATA step; popind = 0 is the event).
ov <- matrix(c(
3.0,5.8,10,0, 3.2,6.3,5,1, 3.9,6.8,3,1, 2.8,4.8,6,0,
3.2,5.8,3,1, 0.9,4.0,5,0, 2.5,5.7,8,0, 1.6,5.6,5,1,
3.8,5.7,5,1, 3.7,6.7,6,1, 3.2,5.4,4,1, 3.8,6.6,6,1,
4.1,6.6,5,1, 3.6,5.7,5,1, 4.3,7.0,4,1, 3.6,6.7,4,0,
2.3,4.4,6,1, 4.2,7.6,4,0, 4.0,6.6,6,0, 3.5,5.8,6,1,
3.8,6.8,7,1, 3.0,4.7,8,0, 4.5,7.4,5,1, 3.7,7.4,5,1,
3.1,6.6,6,1, 4.1,8.2,6,1, 4.3,7.0,5,1, 4.3,6.5,4,1,
3.2,5.1,5,1, 2.6,4.7,6,1, 3.3,6.8,6,0, 1.7,4.0,7,0,
3.7,6.1,5,1, 3.3,6.3,7,1, 4.2,7.7,6,1, 3.5,6.2,5,1,
2.9,5.7,9,0, 2.1,4.8,7,1, 2.8,6.2,8,0, 4.0,7.0,7,1,
3.3,5.7,6,1, 3.7,6.9,5,1, 3.6,6.6,5,1), ncol = 4, byrow = TRUE)
ovarian <- data.frame(alb = ov[, 1], tp = ov[, 2], totscore = 10 - ov[, 3], popind = as.integer(ov[, 4]))
write.csv(ovarian, file.path(tdir, "delongOvarian.csv"), row.names = FALSE)
stopifnot(nrow(ovarian) == 43, sum(ovarian$popind == 0) == 12)

# ---------------------------------------------------------------- 1. aSAH: AUC, DeLong SE and CI
isPoor <- asah$outcome == "Poor"
pTests <- c(wfns = 0.823678861788618, ndka = 0.611957994579946)        # pROC tests/testthat/test-auc.R l. 7, 10
pVar <- c(wfns = 0.00146991470882363, ndka = 0.0031908105493913, s100b = 0.00266868245717244)  # test-var.R l. 6-8
pCiNdka <- c(0.501244999271703, 0.611957994579946, 0.722670989888189)  # test-ci.auc.R l. 6
for (m in c("s100b", "ndka", "wfns")) {
  x <- asah[[m]]
  h <- dlAuc(x, isPoor)
  r <- roc(asah$outcome, x, levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
  ci <- as.numeric(ci.auc(r, method = "delong"))
  same(h[["auc"]], auc(r), paste(m, "AUC hand vs pROC"))
  same(h[["var"]], var(r), paste(m, "DeLong var hand vs pROC var()"))
  same(h[c("lower", "upper")], ci[c(1, 3)], paste(m, "DeLong CI hand vs pROC ci.auc"))
  same(h[["var"]], pVar[[m]], paste(m, "var vs pROC test-var.R"), tol = 1e-12)
  if (m %in% names(pTests)) same(h[["auc"]], pTests[[m]], paste(m, "AUC vs pROC test-auc.R"))
  if (m == "ndka") same(c(h[["lower"]], h[["auc"]], h[["upper"]]), pCiNdka, "ndka CI vs pROC test-ci.auc.R")
  for (k in c("auc", "se", "lower", "upper", "z", "p")) put(paste0("aSAH.", m, ".higher.", k), h[[k]], "hand DeLong = pROC")
  # direction lower: the negated marker
  hl <- dlAuc(-x, isPoor)
  rl <- roc(asah$outcome, x, levels = c("Good", "Poor"), direction = ">", quiet = TRUE)
  same(hl[["auc"]], auc(rl), paste(m, "lower AUC hand vs pROC"))
  same(hl[c("lower", "upper")], as.numeric(ci.auc(rl))[c(1, 3)], paste(m, "lower CI hand vs pROC"))
  same(hl[["auc"]], 1 - h[["auc"]], paste(m, "lower AUC = 1 - higher AUC"))
  for (k in c("auc", "se", "lower", "upper", "z", "p")) put(paste0("aSAH.", m, ".lower.", k), hl[[k]], "hand DeLong = pROC")
  put(paste0("aSAH.", m, ".nPositive"), sum(isPoor & !is.na(x)), "count")
  put(paste0("aSAH.", m, ".nNegative"), sum(!isPoor & !is.na(x)), "count")
}
# CI at level 0.90 for s100b
h90 <- dlAuc(asah$s100b, isPoor, 0.90)
same(h90[c("lower", "upper")], as.numeric(ci.auc(roc(asah$outcome, asah$s100b, levels = c("Good", "Poor"), direction = "<", quiet = TRUE), conf.level = 0.9))[c(1, 3)], "s100b CI 90%")
put("aSAH.s100b.higher.ci90.lower", h90[["lower"]], "hand DeLong = pROC"); put("aSAH.s100b.higher.ci90.upper", h90[["upper"]], "hand DeLong = pROC")

# ---------------------------------------------------------------- 2. paired comparisons (roc.test, DeLong)
# pROC tests/testthat/test-roc.test.R l. 28-49: t1 = (wfns, s100b), t2 = (wfns, ndka), t3 = (ndka, s100b)
pT <- list(c("wfns", "s100b", 2.20898359144091, 0.0271757822291882, 0.0104061769564846, 0.174214419249478),
           c("wfns", "ndka", 2.79777591868904, 0.00514557970691098, 0.0634011709339876, 0.3600405634833566),
           c("ndka", "s100b", -1.39077002573558, 0.164295175223054, -0.2876917446341914, 0.0488706064228094))
for (pt in pT) {
  a <- pt[1]; b <- pt[2]
  h <- dlPaired(asah[[a]], asah[[b]], isPoor)
  ra <- roc(asah$outcome, asah[[a]], levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
  rb <- roc(asah$outcome, asah[[b]], levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
  tt <- roc.test(ra, rb, method = "delong", paired = TRUE)
  same(h[["z"]], tt$statistic, paste(a, b, "z hand vs roc.test"))
  same(h[["p"]], tt$p.value, paste(a, b, "p hand vs roc.test"))
  same(h[c("lower", "upper")], tt$conf.int, paste(a, b, "CI hand vs roc.test"))
  same(h[["difference"]], unname(tt$estimate[1] - tt$estimate[2]), paste(a, b, "difference hand vs roc.test"))
  same(h[["cov"]], cov(ra, rb), paste(a, b, "cov hand vs pROC cov()"))
  same(c(h[["z"]], h[["p"]], h[["lower"]], h[["upper"]]), as.numeric(pt[3:6]), paste(a, b, "vs pROC test-roc.test.R"), tol = 1e-10)
  for (k in c("difference", "lower", "upper", "z", "p")) put(paste0("aSAH.compare.", a, "-", b, ".", k), h[[k]], "hand DeLong = roc.test = pROC tests")
}
# order as a user would add them: s100b, ndka, wfns -> pairs (s100b, ndka), (s100b, wfns), (ndka, wfns)
for (pr in list(c("s100b", "ndka"), c("s100b", "wfns"), c("ndka", "wfns"))) {
  h <- dlPaired(asah[[pr[1]]], asah[[pr[2]]], isPoor)
  for (k in c("difference", "lower", "upper", "z", "p")) put(paste0("aSAH.compare.", pr[1], "-", pr[2], ".", k), h[[k]], "hand DeLong (sign flip of pROC tests pair)")
}

# ---------------------------------------------------------------- 3. Hanley & McNeil (1982)
t2 <- hmTable2(hmN, hmA)
cat(sprintf("H&M Table II reproduced: total(5) = %.1f (paper 2,642), W = %.4f (0.893), Q1 = %.4f (0.8182), Q2 = %.4f (0.8313), SE = %.4f (0.032)\n",
            t2[["total5"]], t2[["W"]], t2[["Q1"]], t2[["Q2"]], t2[["se"]]))
stopifnot(abs(t2[["total5"]] - 2642) < 1e-9, round(t2[["W"]], 3) == 0.893, round(t2[["Q1"]], 4) == 0.8182,
          round(t2[["Q2"]], 4) == 0.8313, round(t2[["se"]], 3) == 0.032)
isAb <- hm$status == "abnormal"
hh <- dlAuc(hm$rating, isAb)
rh <- roc(hm$status, hm$rating, levels = c("normal", "abnormal"), direction = "<", quiet = TRUE)
same(hh[["auc"]], auc(rh), "H&M AUC hand DeLong vs pROC"); same(hh[["auc"]], t2[["W"]], "H&M AUC vs Table II W")
same(hh[["var"]], var(rh), "H&M DeLong var hand vs pROC")
put("hm.auc", hh[["auc"]], "H&M 1982 Table II W = 2642/(58*51) = 0.893")
put("hm.se.hanleyMcNeil", t2[["se"]], "H&M 1982 Table II SE = 0.032")
for (k in c("se", "lower", "upper", "z", "p")) put(paste0("hm.delong.", k), hh[[k]], "hand DeLong = pROC")

# ---------------------------------------------------------------- 4. DeLong 1988 example via SAS (event popind = 0)
isEv <- ovarian$popind == 0
sas <- list(alb = c(0.7366, 0.0927, 0.5549, 0.9182), totscore = c(0.7258, 0.1028, 0.5243, 0.9273), tp = c(0.6478, 0.1000, 0.4518, 0.8439))
for (m in names(sas)) {
  hi <- dlAuc(ovarian[[m]], isEv); lo <- dlAuc(-ovarian[[m]], isEv)
  dirn <- if (round(hi[["auc"]], 4) == sas[[m]][1]) "higher" else "lower"
  h <- if (dirn == "higher") hi else lo
  r <- roc(factor(ovarian$popind), ovarian[[m]], levels = c("1", "0"), direction = if (dirn == "higher") "<" else ">", quiet = TRUE)
  same(h[["auc"]], auc(r), paste("ovarian", m, "AUC hand vs pROC")); same(h[["var"]], var(r), paste("ovarian", m, "var hand vs pROC"))
  same(round(c(h[["auc"]], h[["se"]], h[["lower"]], h[["upper"]]), 4), sas[[m]], paste("ovarian", m, "vs SAS Output 53.8.9 (4 dp)"), tol = 1e-8)
  cat("  ovarian", m, "matches SAS with direction", dirn, "\n")
  for (k in c("auc", "se", "lower", "upper", "z", "p")) put(paste0("ovarian.", m, ".", dirn, ".", k), h[[k]], "hand DeLong = pROC = SAS (4 dp)")
}
# SAS Output 53.8.12: Albumin - K-G = 0.0108, SE 0.0953, CI -0.1761 0.1976; Total Protein - K-G = -0.0780, SE 0.1046, CI -0.2830 0.1271
d1 <- dlPaired(-ovarian$alb, -ovarian$totscore, isEv)
d2 <- dlPaired(-ovarian$tp, -ovarian$totscore, isEv)
# All three markers match SAS with direction "lower" (lower values indicate popind = 0), so one direction serves the contrast.
same(round(c(d1[["difference"]], d1[["se"]], d1[["lower"]], d1[["upper"]]), 4), c(0.0108, 0.0953, -0.1761, 0.1976), "ovarian alb - totscore vs SAS 53.8.12", tol = 1e-8)
same(round(c(d2[["difference"]], d2[["se"]], d2[["lower"]], d2[["upper"]]), 4), c(-0.0780, 0.1046, -0.2830, 0.1271), "ovarian tp - totscore vs SAS 53.8.12", tol = 1e-8)
same(round(d1[["z"]]^2, 4), 0.0127, "ovarian alb chi-square vs SAS"); same(round(d2[["z"]]^2, 4), 0.5554, "ovarian tp chi-square vs SAS")
d3 <- dlPaired(-ovarian$alb, -ovarian$tp, isEv)   # not in SAS output; hand DeLong checked against roc.test below
r3a <- roc(factor(ovarian$popind), ovarian$alb, levels = c("1", "0"), direction = ">", quiet = TRUE)
r3b <- roc(factor(ovarian$popind), ovarian$tp, levels = c("1", "0"), direction = ">", quiet = TRUE)
t3 <- roc.test(r3a, r3b, method = "delong", paired = TRUE)
same(c(d3[["z"]], d3[["p"]], d3[["lower"]], d3[["upper"]]), c(t3$statistic, t3$p.value, t3$conf.int), "ovarian alb - tp hand vs roc.test")
for (k in c("difference", "lower", "upper", "z", "p")) put(paste0("ovarian.compare.alb-tp.", k), d3[[k]], "hand DeLong = roc.test")
for (k in c("difference", "lower", "upper", "z", "p")) { put(paste0("ovarian.compare.alb-totscore.", k), d1[[k]], "hand DeLong = SAS (4 dp)"); put(paste0("ovarian.compare.tp-totscore.", k), d2[[k]], "hand DeLong = SAS (4 dp)") }

# ---------------------------------------------------------------- 5. cut-offs on aSAH
cutStats <- c("sensitivity", "sensitivityLower", "sensitivityUpper", "specificity", "specificityLower", "specificityUpper",
              "ppv", "npv", "lrPositive", "lrNegative", "accuracy", "youden", "TP", "FN", "TN", "FP")
doCut <- function(tag, x, isCase, direction, criterion, value = NULL, prevalence = NULL, ciLevel = 0.95) {
  tab <- coAll(x, isCase, direction)
  # cross-check the curve with pROC coords and ROCR
  r <- roc(isCase, x, levels = c(FALSE, TRUE), direction = if (direction == "higher") "<" else ">", quiet = TRUE)
  pc <- coords(r, "all", ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE)
  pc <- pc[order(pc$threshold), ]      # pROC lists thresholds in decreasing order for direction ">"
  same(tab$threshold, pc$threshold, paste(tag, "thresholds hand vs pROC"))
  same(tab$sensitivity, pc$sensitivity, paste(tag, "sensitivity curve hand vs pROC"))
  same(tab$specificity, pc$specificity, paste(tag, "specificity curve hand vs pROC"))
  th <- if (criterion == "threshold") value else coChoose(tab, criterion, value)
  lapply(seq_along(th), function(i) {
    s <- coStats(coCounts(x, isCase, th[i], direction), prevalence, ciLevel)
    id <- paste0(tag, if (length(th) > 1) paste0(".tie", i) else "")
    put(paste0(id, ".threshold"), th[i], "hand")
    for (k in cutStats) put(paste0(id, ".", k), s[[k]], "hand counts; CIs binom.test")
    s
  })
  invisible(th)
}
rocrCheck <- function(tag, x, isCase, direction) {
  xx <- if (direction == "higher") x else -x
  pred <- ROCR::prediction(xx, as.integer(isCase), label.ordering = c(0L, 1L))
  perf <- ROCR::performance(pred, "tpr", "fpr")
  rocrPts <- unique(round(cbind(perf@x.values[[1]], perf@y.values[[1]]), 12))
  tab <- coAll(x, isCase, direction)
  handPts <- unique(round(cbind(1 - tab$specificity, tab$sensitivity), 12))
  o1 <- order(rocrPts[, 1], rocrPts[, 2]); o2 <- order(handPts[, 1], handPts[, 2])
  same(rocrPts[o1, ], handPts[o2, ], paste(tag, "ROC points hand vs ROCR"))
  same(ROCR::performance(pred, "auc")@y.values[[1]], dlAuc(x * if (direction == "higher") 1 else -1, isCase)[["auc"]], paste(tag, "AUC ROCR vs hand"))
}
for (m in c("s100b", "ndka", "wfns")) for (d in c("higher", "lower")) rocrCheck(paste("aSAH", m, d), asah[[m]], isPoor, d)

doCut("aSAH.s100b.higher.youden", asah$s100b, isPoor, "higher", "youden")
doCut("aSAH.s100b.higher.closestTopLeft", asah$s100b, isPoor, "higher", "closestTopLeft")
doCut("aSAH.s100b.higher.fixedSensitivity0.9", asah$s100b, isPoor, "higher", "fixedSensitivity", 0.9)
doCut("aSAH.s100b.higher.fixedSpecificity0.9", asah$s100b, isPoor, "higher", "fixedSpecificity", 0.9)
doCut("aSAH.s100b.higher.threshold0.2", asah$s100b, isPoor, "higher", "threshold", 0.2)
doCut("aSAH.s100b.higher.threshold0.205", asah$s100b, isPoor, "higher", "threshold", 0.205)
doCut("aSAH.s100b.higher.youden.prev0.1", asah$s100b, isPoor, "higher", "youden", prevalence = 0.1)
doCut("aSAH.s100b.higher.youden.ci90", asah$s100b, isPoor, "higher", "youden", ciLevel = 0.9)
doCut("aSAH.ndka.higher.youden", asah$ndka, isPoor, "higher", "youden")
doCut("aSAH.wfns.higher.youden", asah$wfns, isPoor, "higher", "youden")
doCut("aSAH.wfns.higher.closestTopLeft", asah$wfns, isPoor, "higher", "closestTopLeft")
doCut("aSAH.wfns.higher.threshold4.5", asah$wfns, isPoor, "higher", "threshold", 4.5)
doCut("aSAH.wfns.higher.threshold4", asah$wfns, isPoor, "higher", "threshold", 4)
doCut("aSAH.s100b.lower.youden", asah$s100b, isPoor, "lower", "youden")
doCut("aSAH.s100b.lower.threshold0.2", asah$s100b, isPoor, "lower", "threshold", 0.2)
doCut("aSAH.wfns.lower.threshold2", asah$wfns, isPoor, "lower", "threshold", 2)
doCut("aSAH.wfns.lower.threshold2.5", asah$wfns, isPoor, "lower", "threshold", 2.5)
doCut("aSAH.wfns.lower.youden", asah$wfns, isPoor, "lower", "youden")
# pROC's own best thresholds as a cross-check of the Youden and closest-top-left choice
for (m in c("s100b", "ndka", "wfns")) {
  r <- roc(asah$outcome, asah[[m]], levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
  for (meth in c("youden", "closest.topleft")) {
    b <- coords(r, "best", best.method = meth, ret = "threshold", transpose = FALSE)$threshold
    h <- coChoose(coAll(asah[[m]], isPoor, "higher"), if (meth == "youden") "youden" else "closestTopLeft")
    same(sort(b), sort(h), paste(m, meth, "best threshold hand vs pROC coords best"))
  }
}

# Coordinates table rows for wfns (6 thresholds), sample prevalence
tabW <- coAll(asah$wfns, isPoor, "higher")
pS <- mean(isPoor)
for (i in seq_len(nrow(tabW))) {
  se <- tabW$sensitivity[i]; sp <- tabW$specificity[i]
  put(sprintf("aSAH.wfns.coord%d.threshold", i), tabW$threshold[i], "hand")
  put(sprintf("aSAH.wfns.coord%d.sensitivity", i), se, "hand")
  put(sprintf("aSAH.wfns.coord%d.specificity", i), sp, "hand")
  put(sprintf("aSAH.wfns.coord%d.ppv", i), se * pS / (se * pS + (1 - sp) * (1 - pS)), "hand (NaN when se = 0 and sp = 1)")
  put(sprintf("aSAH.wfns.coord%d.npv", i), sp * (1 - pS) / (sp * (1 - pS) + (1 - se) * pS), "hand (NaN when se = 1 and sp = 0)")
}

# ---------------------------------------------------------------- 6. logistic regression with one predictor
for (m in c("s100b", "ndka", "wfns")) {
  fit <- glm(isPoor ~ asah[[m]], family = binomial)
  same(dlAuc(fitted(fit), isPoor)[["auc"]], dlAuc(asah[[m]], isPoor)[["auc"]], paste(m, "glm fitted-probability AUC vs marker AUC"))
  same(dlAuc(fitted(fit), isPoor)[["se"]], dlAuc(asah[[m]], isPoor)[["se"]], paste(m, "glm fitted-probability DeLong SE vs marker SE"))
  cat("  ", m, "glm slope", coef(fit)[2], "\n")
}

# ---------------------------------------------------------------- 7. missing values: per marker and listwise
asahNA <- asah
asahNA$ndka[1:5] <- NA; asahNA$s100b[6:8] <- NA; asahNA$outcome[9] <- NA
okO <- !is.na(asahNA$outcome)
for (m in c("s100b", "ndka")) {
  keep <- okO & !is.na(asahNA[[m]])
  h <- dlAuc(asahNA[[m]][keep], asahNA$outcome[keep] == "Poor")
  put(paste0("aSAHmissing.", m, ".nPositive"), sum(asahNA$outcome[keep] == "Poor"), "count")
  put(paste0("aSAHmissing.", m, ".nNegative"), sum(asahNA$outcome[keep] == "Good"), "count")
  for (k in c("auc", "se", "lower", "upper")) put(paste0("aSAHmissing.", m, ".", k), h[[k]], "hand DeLong, per-marker complete cases")
}
keepL <- okO & !is.na(asahNA$s100b) & !is.na(asahNA$ndka)
hL <- dlPaired(asahNA$s100b[keepL], asahNA$ndka[keepL], asahNA$outcome[keepL] == "Poor")
rA <- roc(asahNA$outcome[keepL], asahNA$s100b[keepL], levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
rB <- roc(asahNA$outcome[keepL], asahNA$ndka[keepL], levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
tL <- roc.test(rA, rB, method = "delong", paired = TRUE)
same(c(hL[["z"]], hL[["p"]], hL[["lower"]], hL[["upper"]]), c(tL$statistic, tL$p.value, tL$conf.int), "missing listwise comparison hand vs roc.test")
put("aSAHmissing.compare.n", sum(keepL), "count (listwise)")
for (k in c("difference", "lower", "upper", "z", "p")) put(paste0("aSAHmissing.compare.s100b-ndka.", k), hL[[k]], "hand DeLong = roc.test, listwise")

# ---------------------------------------------------------------- 8. bootstrap (pROC, stratified) for information only
r <- roc(asah$outcome, asah$s100b, levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
set.seed(1); b1 <- as.numeric(ci.auc(r, method = "bootstrap", boot.n = 2000, boot.stratified = TRUE))
cat("bootstrap s100b, set.seed(1) then ci.auc(boot.n = 2000, stratified):", b1, "(depends on RNG use before the call; not pinned)\n")

# ---------------------------------------------------------------- 9. edge cases (values used by the tests)
# Ties: a marker with all values tied across classes in pairs
tiesX <- c(1, 1, 2, 2, 3, 3, 3, 4); tiesY <- c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
ht <- dlAuc(tiesX, tiesY)
same(ht[["auc"]], auc(roc(tiesY, tiesX, levels = c(FALSE, TRUE), direction = "<", quiet = TRUE)), "ties AUC hand vs pROC")
for (k in c("auc", "se")) put(paste0("ties.", k), ht[[k]], "hand DeLong = pROC")
# Huge values: s100b times 1e12 and plus 1e12 (AUC unchanged by monotone maps)
same(dlAuc(asah$s100b * 1e12, isPoor)[["auc"]], dlAuc(asah$s100b, isPoor)[["auc"]], "huge scale AUC unchanged")
rHuge <- roc(asah$outcome, asah$s100b * 1e12, levels = c("Good", "Poor"), direction = "<", quiet = TRUE)
same(as.numeric(auc(rHuge)), dlAuc(asah$s100b, isPoor)[["auc"]], "pROC AUC with s100b * 1e12")
same(sqrt(var(rHuge)), dlAuc(asah$s100b, isPoor)[["se"]], "pROC SE with s100b * 1e12")
# One positive case: DeLong var uses var() of a single placement value -> NA
h1 <- suppressWarnings(dlAuc(c(4, 1, 2, 3, 5), c(TRUE, FALSE, FALSE, FALSE, FALSE)))
put("onePositive.auc", h1[["auc"]], "hand"); cat("onePositive SE (hand):", h1[["se"]], "\n")
r1 <- roc(c(1, 0, 0, 0, 0), c(4, 1, 2, 3, 5), levels = c(0, 1), direction = "<", quiet = TRUE)
cat("onePositive pROC var:", tryCatch(var(r1), error = function(e) paste("error:", conditionMessage(e))), "\n")
cat("onePositive pROC ci.auc:", tryCatch(as.numeric(ci.auc(r1)), error = function(e) paste("error:", conditionMessage(e)), warning = function(w) paste("warning:", conditionMessage(w))), "\n")


# Tied optimum with finite thresholds: x = 1..4, cases at 2 and 4. Youden 0.5 at 1.5 and at 3.5.
tieX <- c(1, 2, 3, 4); tieY <- c(FALSE, TRUE, FALSE, TRUE)
tt <- coChoose(coAll(tieX, tieY, "higher"), "youden")
cat("finite tie Youden thresholds:", tt, "\n"); stopifnot(identical(tt, c(1.5, 3.5)))
doCut("finiteTie.youden", tieX, tieY, "higher", "youden")

write.csv(ref, file.path(here, "reference-values.csv"), row.names = FALSE)
cat("wrote", nrow(ref), "values\n")

# ---------------------------------------------------------------- 10. Robin et al. (2011) case study, for the record
# The paper reports pAUC (specificity 90-100%) of 3.1% for WFNS and 3.0% for S100B, and a WFNS cut-off of 4.5.
# The pAUC is outside v1; it is printed here to show whether the shipped aSAH data reproduce the paper's numbers.
for (m in c("wfns", "s100b")) {
  r <- roc(asah$outcome, asah[[m]], levels = c("Good", "Poor"), direction = "<", quiet = TRUE, partial.auc = c(1, 0.9))
  cat(sprintf("Robin 2011 check: pAUC(sp 0.9-1) of %s in the shipped aSAH = %.4f\n", m, as.numeric(r$auc)))
}
