library(deSolve)
library(zoo)

our.data <- read.csv("datasets/03-parameter-estimation/vanrolleghem_our.csv")

AR.noise <- function(n, width=5) {
  z <- zoo(rnorm(n + width - 1))
  rollapply(z, width = width, FUN = mean)
}

monod.rhs <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    list(c(-alpha*s/(beta + s)))
  })
}

monod.solve <- function(times, s0, alpha, beta) {
  state <- c(s=s0)
  parameters <- c(alpha=alpha, beta=beta)
  sol <- ode(y = state, times = times, parms = parameters, func = monod.rhs)
  return(sol[,"s"])
}

our.first.order <- function(times, alpha, s0) {
  s0*exp(-alpha*times)
}

our.single.monod <- function(times, alpha, beta, s0) {
  s <- monod.solve(times, s0, alpha, beta)
  alpha * s / (beta + s)
}

our.double.monod <- function(times, alpha1, beta1, s0.1, alpha2, beta2, s0.2) {
  s1 <- monod.solve(times, s0.1, alpha1, beta1)
  s2 <- monod.solve(times, s0.2, alpha2, beta2)

  alpha1 * s1 / (beta1 + s1) + alpha2 * s2 / (beta2 + s2)
}

call_rhs <- function(f, times, parameters) {
  do.call(f, c(list(times), as.list(parameters)))
}

plot.first.order <- function(ps) {
  times <- seq(0, 30, by=0.1)
  fitted <- call_rhs(our.first.order, times, ps) +
    0.1*AR.noise(length(times), width = 30)
  plot(times, fitted, pch = 19, cex = 0.5, ylim=c(0, 1),
       main = "0 inflection points", xlab = "", ylab = "")
}

plot.single.monod <- function(ps) {
  times <- seq(0, 30, by=0.1)
  fitted <- call_rhs(our.single.monod, times, ps) +
    0.05*AR.noise(length(times), width = 30)
  plot(times, fitted, pch = 19, cex = 0.5, ylim=c(0, 1),
       main = "1 inflection point", xlab = "", ylab = "")
}

plot.double.monod <- function(ps) {
  times <- seq(0, 45, by=0.1)
  fitted <- call_rhs(our.double.monod, times, ps) +
    0.05*AR.noise(length(times), width = 30)
  plot(times, fitted, pch = 19, cex = 0.5, ylim=c(0, 1),
       main = "3 inflection points", xlab = "", ylab = "")
}

run_all <- function() {
  par.first.order <- c(0.1038534, 0.9184895)
  par.single.monod <- c(0.4717867, 0.1275079, 4.0865588)
  par.double.monod <- c(0.4, 1.0, 5.65, 0.47, 0.05, 4.09)

  par(mfrow=c(1, 3))
  plot.first.order(par.first.order)
  plot.single.monod(par.single.monod)
  plot.double.monod(par.double.monod)
}

run_all()