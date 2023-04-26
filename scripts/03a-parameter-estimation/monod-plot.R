library(deSolve)

our.data <- read.csv("datasets/03-parameter-estimation/vanrolleghem_our.csv")

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

plot.data <- function() {
  plot(our.data[,1], our.data[,2],
       ylim = c(0, 1), pch = 20, cex = 0.5,
       xlab = "Time", ylab = "OUR")
}

plot.first.order <- function(ps) {
  times <- seq(0, 45, by=0.1)
  fitted <- call_rhs(our.first.order, times, ps)
  lines(times, fitted, col = 3, lwd = 2)
}

plot.single.monod <- function(ps) {
  times <- seq(0, 45, by=0.1)
  fitted <- call_rhs(our.single.monod, times, ps)
  lines(times, fitted, col = 4, lwd = 2)
}

plot.double.monod <- function(ps) {
  times <- seq(0, 45, by=0.1)
  fitted <- call_rhs(our.double.monod, times, ps)
  lines(times, fitted, col = 2, lwd = 2)
}

run_all <- function() {
  par.first.order <- c(0.1038534, 0.9184895)
  par.single.monod <- c(1.128503, 3.468738, 7.827375)
  par.double.monod <- c(0.3440548, 1.6705150, 4.2286317,
                        0.4717867, 0.1275079, 4.0865588)

  plot.data()
  plot.first.order(par.first.order)
  plot.single.monod(par.single.monod)
  plot.double.monod(par.double.monod)

  legend(30, 1.0, legend = c("Double Monod", "Single Monod", "First Order"), col = c(2, 4, 3), lwd = 2)
}

run_all()