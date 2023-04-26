source("scripts/03a-parameter-estimation/SP-utils.R")

k1 <- 0.353
k2 <- 0.389

sigma.meas <- 0.05

times <- seq(0, 25, by = 1.0)
sensitivity <- matrix(c(
  S_DO_k1(times, k1, k2),
  S_DO_k2(times, k1, k2)), byrow = F, ncol = 2)

FIM <- t(sensitivity) %*% sensitivity / sigma.meas^2
C <- solve(FIM)  # matrix inverse

