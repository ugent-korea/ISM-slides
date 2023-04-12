library(plot3D)

objective <- function(x, y) {
  return(100*(y-x^2)^2 + (x-1)^2)
}
gradient <- function(x, y) {
  return(c(-400*(y-x^2)*x + 2*(x-1), 200*(y-x^2)))
}
backtrack_linesearch <- function(x, y) {
  alpha <- 0.3
  beta <- 0.8
  t <- 1  # initial step size

  g <- gradient(x, y)
  normgrad <- g[1]^2 + g[2]^2
  while(objective(x - t*g[1], y - t*g[2]) > objective(x, y) - alpha*t*normgrad) {
    t <- beta*t
  }
  return(t)
}

grad_descent <- function(x0, y0) {
  xs <- c(x0)
  ys <- c(y0)
  ts <- c()
  for (i in 1:1000) {
    g <- gradient(xs[i], ys[i])
    if (g[1]^2 + g[2]^2 < 0.001) {
      break
    }
    t <- backtrack_linesearch(xs[i], ys[i])
    xs <- c(xs, xs[i] - t*g[1])
    ys <- c(ys, ys[i] - t*g[2])
    ts <- c(ts, t)
  }
  return(list(x=xs,y=ys, t=ts))
}


par(mfrow = c(1, 2), mar = c(2, 2, 2, 2))

# Surface plot
y <- seq(-1, 3, length=50)
x <- seq(-2, 2, length=50)
X <- outer(x, rep(1, length(y)))
Y <- outer(rep(1, length(x)), y)
Z <- 100 * (Y - X^2)^2 + (X - 1)^2
surf3D(X, Y, Z, colkey=FALSE, bty="b2", colvar=log(1+Z))

# Contour plot
ps <- grad_descent(1.0, -0.5)

xc <- seq(0, 1, length=50)
yc <- seq(-0.5, 1, length=50)
Xc <- outer(xc, rep(1, length(yc)))
Yc <- outer(rep(1, length(xc)), yc)
Zc <- 100 * (Yc - Xc^2)^2 + (Xc - 1)^2
contour(xc, yc, Zc, levels=c(2, 5, 10, 20, 50, 100, 200), col="gray")
lines(ps$x, ps$y)
points(c(1, 1), c(-0.5, 1), pch=4, cex=2, lwd=2)

label_point <- function(n) {
  points(ps$x[[n]], ps$y[[n]], pch=20)
  text(ps$x[[n]], ps$y[[n]], n, adj = c(1, 0), offset = 2)
}

label_point(10)
label_point(100)
label_point(500)
label_point(1000)
