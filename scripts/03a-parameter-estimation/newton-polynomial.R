A <- matrix(
  c(4*1^3, 3*1^2, 2*1, 1, 0,
    4*2^3, 3*2^2, 2*2, 1, 0,
    4*4^3, 3*4^2, 2*4, 1, 0,
    1, 1, 1, 1, 1,
    4^4, 4^3, 4^2, 4, 1), nrow = 5, byrow = TRUE)

rhs <- c(0, 0, 0, 1, 2)

coefs <- -solve(A, rhs)

poly <- function(x) {
    coefs %*% c(x^4, x^3, x^2, x, 1)
  }
poly <- Vectorize(poly)

taylor <- function(x, x0) {
  # derivatives
  c0 <- coefs %*% c(x0^4, x0^3, x0^2, x0, 1)
  c1 <- coefs %*% c(4*x0^3, 3*x0^2, 2*x0, 1, 0)
  c2 <- coefs %*% c(12*x0^2, 6*x0, 2, 0, 0)
  # 2nd order Taylor
  c0 + c1*(x - x0) + c2/2*(x-x0)^2
}
taylor <- Vectorize(taylor, "x")

newton_one_step <- function(x0) {
  c1 <- coefs %*% c(4*x0^3, 3*x0^2, 2*x0, 1, 0)
  c2 <- coefs %*% c(12*x0^2, 6*x0, 2, 0, 0)
  x0 - c1/c2
}

x <- seq(0, 5, length.out = 50)
y <- poly(x)

par(mfrow=c(1, 3))

plot_facet <- function(n, guess) {
  x1 <- seq(0, 5, length.out = 50)
  y1 <- taylor(x1, guess)
  plot(x, y, type = "l", 
       main = paste0("x", n, " = ", round(guess, 2)),
       xlab="", ylab="", col = "gray")
  lines(x1, y1)
  points(guess, poly(guess), pch=19, cex=1.5)
}

guess = 5
plot_facet(0, guess)

guess = newton_one_step(guess)
plot_facet(1, guess)

guess = newton_one_step(guess)
plot_facet(2, guess)
