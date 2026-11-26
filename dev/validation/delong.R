# Hand-coded DeLong, DeLong & Clarke-Pearson (1988), Biometrics 44:837-845.
# Base R only; no pROC code is used. Structural components (placement values):
#   psi(x, y) = 1 if y > x, 1/2 if y == x, 0 if y < x   (x a control value, y a case value)
#   V10(y_i) = mean over controls j of psi(x_j, y_i)   (one per case, i = 1..m)
#   V01(x_j) = mean over cases i of psi(x_j, y_i)      (one per control, j = 1..n)
#   AUC = mean(V10) = mean(V01)
#   Var(AUC) = S10 / m + S01 / n, with S10 = var(V10), S01 = var(V01) (denominators m - 1 and n - 1).
# For several markers on the same cases, S10 and S01 become covariance matrices and
#   Var(AUC_1 - AUC_2) = L S L', L = (1, -1), S = S10 / m + S01 / n.
# Direction "higher" uses the marker as given; "lower" uses the negated marker.

dlPsi <- function(x, y) (y > x) + 0.5 * (y == x)

dlComponents <- function(marker, isCase) {
  y <- marker[isCase]; x <- marker[!isCase]
  P <- outer(x, y, dlPsi)            # rows: controls, columns: cases
  list(V10 = colMeans(P), V01 = rowMeans(P), m = length(y), n = length(x))
}

dlAuc <- function(marker, isCase, ciLevel = 0.95) {
  c1 <- dlComponents(marker, isCase)
  auc <- mean(c1$V10)
  v <- var(c1$V10) / c1$m + var(c1$V01) / c1$n
  se <- sqrt(v)
  q <- qnorm(1 - (1 - ciLevel) / 2)
  z <- (auc - 0.5) / se
  c(auc = auc, var = v, se = se, lower = auc - q * se, upper = auc + q * se,
    z = z, p = 2 * pnorm(-abs(z)))
}

dlPaired <- function(m1, m2, isCase, ciLevel = 0.95) {
  a <- dlComponents(m1, isCase); b <- dlComponents(m2, isCase)
  S10 <- cov(cbind(a$V10, b$V10)); S01 <- cov(cbind(a$V01, b$V01))
  S <- S10 / a$m + S01 / a$n
  L <- c(1, -1)
  d <- mean(a$V10) - mean(b$V10)
  se <- sqrt(drop(t(L) %*% S %*% L))
  q <- qnorm(1 - (1 - ciLevel) / 2)
  z <- d / se
  c(auc1 = mean(a$V10), auc2 = mean(b$V10), difference = d, cov = S[1, 2], se = se,
    lower = d - q * se, upper = d + q * se, z = z, p = 2 * pnorm(-abs(z)))
}

# Hanley & McNeil (1982), Table II (p. 31): W and its SE from rating counts.
#   nNx = normals rated x, nAx = abnormals rated x, nAgt = abnormals rated > x, nNlt = normals rated < x.
#   W  = sum(nNx * nAgt + 1/2 nNx nAx) / (nN nA)
#   Q2 = sum(nAx * (nNlt^2 + nNlt nNx + 1/3 nNx^2)) / (nA nN^2)
#   Q1 = sum(nNx * (nAgt^2 + nAgt nAx + 1/3 nAx^2)) / (nN nA^2)
#   SE = sqrt((W(1-W) + (nA-1)(Q1-W^2) + (nN-1)(Q2-W^2)) / (nA nN))
hmTable2 <- function(nNx, nAx) {
  nN <- sum(nNx); nA <- sum(nAx)
  nAgt <- nA - cumsum(nAx); nNlt <- cumsum(nNx) - nNx
  W  <- sum(nNx * nAgt + 0.5 * nNx * nAx) / (nN * nA)
  Q2 <- sum(nAx * (nNlt^2 + nNlt * nNx + nNx^2 / 3)) / (nA * nN^2)
  Q1 <- sum(nNx * (nAgt^2 + nAgt * nAx + nAx^2 / 3)) / (nN * nA^2)
  se <- sqrt((W * (1 - W) + (nA - 1) * (Q1 - W^2) + (nN - 1) * (Q2 - W^2)) / (nA * nN))
  c(W = W, Q1 = Q1, Q2 = Q2, se = se, total5 = W * nN * nA)
}
