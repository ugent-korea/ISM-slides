library(plot3D)

objective <- function(x, y) {
  return(100*(y-x^2)^2 + (x-1)^2)
}

gradient <- function(p) {
  x <- p[1]; y <- p[2]
  c(-400*(y-x^2)*x + 2*(x-1), 200*(y-x^2))
}
hessian <- function(p) {
  x <- p[1]; y <- p[2]
  matrix(
    c(800*x^2 - 400*(y-x^2) + 2, -400*x, -400*x, 200), 2, 2
  )
}

newton_one_step <- function(p, gamma=1.0) {
  p - gamma*solve(hessian(p), gradient(p))
}

p0 <- c(0.9, -0.5)
px <- c(p0[[1]])
py <- c(p0[[2]])
for (i in 1:5) {
  p0 <- newton_one_step(p0)
  px <- c(px, p0[[1]])
  py <- c(py, p0[[2]])
}

par(mfrow = c(1, 2), mar = c(2, 2, 2, 2))

# Surface plot
y <- seq(-1, 3, length=50)
x <- seq(-2, 2, length=50)
X <- outer(x, rep(1, length(y)))
Y <- outer(rep(1, length(x)), y)
Z <- 100 * (Y - X^2)^2 + (X - 1)^2
surf3D(X, Y, Z, colkey=FALSE, bty="b2", colvar=log(1+Z))

xc <- seq(0, 1, length=50)
yc <- seq(-0.5, 1, length=50)
Xc <- outer(xc, rep(1, length(yc)))
Yc <- outer(rep(1, length(xc)), yc)
Zc <- 100 * (Yc - Xc^2)^2 + (Xc - 1)^2
contour(xc, yc, Zc, levels=c(2, 5, 10, 20, 50, 100, 200), col="gray")
lines(px, py)
points(c(1, 1), pch=4, cex=2, lwd=2)

label_point <- function(n) {
  points(px[[n]], py[[n]], pch=20)
  text(px[[n]], py[[n]], n, adj = c(1, 0), offset = 2)
}

label_point(1)
label_point(2)