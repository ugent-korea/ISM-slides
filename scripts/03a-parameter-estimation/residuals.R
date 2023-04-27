CWD <- read.table("./datasets/01-linear-regression/christ.csv", header = T, sep = ",", dec = ".")

a <- -77.099
b <- 0.116

x <- CWD$RIP.DENS
y <- CWD$CWD.BASA

plot_residuals <- function(a, b) {
  plot(y ~ x, xlab = "", ylab = "")
  abline(a, b)
  y_pred <- a + b*x
  segments(x, y_pred, x, y, lwd=2)
}

par(mfrow=c(1, 1))
plot_residuals(a, b)
text(1700, 100, paste("y =", a, "+", b, "x"))