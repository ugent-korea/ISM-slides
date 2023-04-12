library(lattice)
library(latex2exp)

y <- seq(-4, 4, length=50)
x <- seq(-4, 4, length=50)
X <- outer(x, rep(1, length(y)))
Y <- outer(rep(1, length(x)), y)
Z <- (X^2+Y-11)^2 + (X+Y^2-7)^2

sx = seq(5, length(x)-5, by=5)
sy = seq(5, length(y)-5, by=5)

par(mfrow=c(1, 1))
filled.contour(x, y, Z, plot.axes = {
  points(X[sx,sy], Y[sx,sy], pch=19)
}, xlab = TeX(r"($\theta_1$)"), ylab = TeX(r"($\theta_2$)"))