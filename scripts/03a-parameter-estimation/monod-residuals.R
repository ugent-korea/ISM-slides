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

x <- our.data[,1]
y <- our.data[,2]

par.first.order <- c(0.1038534, 0.9184895)
par.single.monod <- c(1.128503, 3.468738, 7.827375)
par.double.monod <- c(0.3440548, 1.6705150, 4.2286317,
                      0.4717867, 0.1275079, 4.0865588)

res.first.order <- y - call_rhs(our.first.order, x, par.first.order)
res.single.monod <- y - call_rhs(our.single.monod, x, par.single.monod)
res.double.monod <- y - call_rhs(our.double.monod, x, par.double.monod)

plot.residuals <- function() {
  oldpar <- par(mfrow=c(3, 1), oma=c(4, 2, 4, 2), mar = c(0.7, 3, 0.7, 1))

  plot(x, res.first.order, xaxt = "n", pch = 19, cex = 0.5, xlab = "", ylab = "")
  abline(h=0, lty = "dashed")
  text(30, 0.70*max(res.first.order), "First-order kinematics",
       font=2, cex = 1.3, adj = c(0, 0))

  plot(x, res.single.monod, xaxt = "n", pch = 19, cex = 0.5, xlab = "", ylab = "")
  abline(h=0, lty = "dashed")
  text(30, 0.70*max(res.single.monod), "Single monod",
       font=2, cex = 1.3, adj = c(0, 0))

  plot(x, res.double.monod, pch = 19, cex = 0.5, xlab = "X", ylab = "")
  abline(h=0, lty = "dashed")
  text(30, 0.70*max(res.double.monod), "Double monod",
       font=2, cex = 1.3, adj = c(0, 0))

  mtext("Time",side=1,line=2.5,outer=TRUE)
  par(oldpar)
}

plot.acf <- function() {
  r1 <- acf(res.first.order, plot = FALSE)
  r2 <- acf(res.single.monod, plot= FALSE)
  r3 <- acf(res.double.monod, plot = FALSE)

  plot(r1$lag, r1$acf, type="b", col = 3, lwd = 2,
       xlab = "Lag", ylab = "Autocorrelation")
  lines(r2$lag, r2$acf, type="b", col = 4, lwd = 2)
  lines(r3$lag, r3$acf, type="b", col = 2, lwd = 2)

  legend(12, 1,
         legend = c("First-order", "Single Monod", "Double Monod"),
         col = c(3, 4, 2), lwd = 2)
  crit <- qnorm(0.975) / sqrt(nrow(our.data))
  abline(h=c(crit, -crit), col = "gray", lty = "dashed", lwd = 2)
}

plot.residuals()
plot.acf()