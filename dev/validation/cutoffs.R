# Cut-off statistics computed from confusion counts, in base R (no pROC).
# Thresholds: midpoints between neighbouring distinct observed values, plus -Inf and Inf
# (the same set pROC uses; dev/interface.md "Thresholds").
# Classification rule: direction "higher": positive when marker >= t; "lower": positive when marker <= t.
# (pROC man/roc.Rd lines 83-87 at v1.19.0.1: "<" positive when >= the threshold; ">" positive when <= the threshold.)

coThresholds <- function(x) { u <- sort(unique(x)); (c(-Inf, u) + c(u, Inf)) / 2 }

coCounts <- function(x, isCase, t, direction = "higher") {
  pos <- if (direction == "higher") x >= t else x <= t
  c(TP = sum(pos & isCase), FN = sum(!pos & isCase), TN = sum(!pos & !isCase), FP = sum(pos & !isCase))
}

coStats <- function(cnt, prevalence = NULL, ciLevel = 0.95) {
  TP <- cnt[["TP"]]; FN <- cnt[["FN"]]; TN <- cnt[["TN"]]; FP <- cnt[["FP"]]
  n <- TP + FN + TN + FP
  p <- if (is.null(prevalence)) (TP + FN) / n else prevalence
  se <- TP / (TP + FN); sp <- TN / (TN + FP)
  seCI <- binom.test(TP, TP + FN, conf.level = ciLevel)$conf.int
  spCI <- binom.test(TN, TN + FP, conf.level = ciLevel)$conf.int
  ppv <- se * p / (se * p + (1 - sp) * (1 - p))
  npv <- sp * (1 - p) / (sp * (1 - p) + (1 - se) * p)
  c(TP = TP, FN = FN, TN = TN, FP = FP,
    sensitivity = se, sensitivityLower = seCI[1], sensitivityUpper = seCI[2],
    specificity = sp, specificityLower = spCI[1], specificityUpper = spCI[2],
    ppv = ppv, npv = npv,
    lrPositive = if (sp == 1) NA else se / (1 - sp), lrNegative = if (sp == 0) NA else (1 - se) / sp,
    accuracy = (TP + TN) / n, youden = se + sp - 1)
}

coAll <- function(x, isCase, direction = "higher") {
  th <- coThresholds(x)
  out <- t(vapply(th, function(t) { k <- coCounts(x, isCase, t, direction)
    c(threshold = t, sensitivity = k[["TP"]] / (k[["TP"]] + k[["FN"]]), specificity = k[["TN"]] / (k[["TN"]] + k[["FP"]])) },
    numeric(3)))
  as.data.frame(out)
}

# Returns every threshold that is best (ties kept).
coChoose <- function(tab, criterion, value = NULL, tol = 1e-12) {
  se <- tab$sensitivity; sp <- tab$specificity
  idx <- switch(criterion,
    youden = { j <- se + sp - 1; which(j >= max(j) - tol) },
    closestTopLeft = { d <- (1 - se)^2 + (1 - sp)^2; which(d <= min(d) + tol) },
    fixedSensitivity = { ok <- which(se >= value); ok[sp[ok] >= max(sp[ok]) - tol] },
    fixedSpecificity = { ok <- which(sp >= value); ok[se[ok] >= max(se[ok]) - tol] })
  tab$threshold[idx]
}
