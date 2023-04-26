set.seed(1234)

library(deSolve)
source("scripts/03a-parameter-estimation/SP-utils.R")

times <- seq(0, 25, by = 1.0)
data <- generate.sample.data(times, 0.35, 0.4)

objfun <- function(k1, k2) {
  estimated <- data[,c("BOD", "DO")]
  actual <- streeter_phelps_numerical(times, k1, k2)[, c("BOD", "DO")]

  sum((estimated - actual)^2)
}

optim.with.history <- function(par=c(0.3, 0.4), method="Nelder-Mead") {
  all_ks <- c()
  objfun.wrapped <- function(ks) {
    all_ks <<- c(all_ks, ks)
    objfun(ks[1], ks[2])
  }
  optim(par, objfun.wrapped, method=method)
  return(matrix(all_ks, ncol = 2, byrow = TRUE))
}

plot.trajectory <- function(method="Nelder-Mead") {
  ks1 <- seq(0.3, 0.4, length.out = 25)
  ks2 <- seq(0.3, 0.5, length.out = 25)
  J <- outer(ks1, ks2, Vectorize(objfun))
  ks_history <- optim.with.history(par = c(0.32, 0.45), method=method)

  contour(ks1, ks2, J, levels=c(1, 2, 5, 10, 20, 50, 70))
  points(ks_history, type = "b", pch = 19)
}
plot.trajectory()