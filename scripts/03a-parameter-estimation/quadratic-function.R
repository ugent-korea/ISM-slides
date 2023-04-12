objective <- function(x, y) {
  return(10*sqrt(x^2 + 4*y^2 + 1))
}
gradient <- function(x, y) {
  eps <- 10e-5
  dx <- (objective(x+eps, y) - objective(x-eps, y))/(2*eps)
  dy <- (objective(x, y+eps) - objective(x, y-eps))/(2*eps)
  return(c(dx, dy))
}

# Implementation from Nocedal and Wright -- approximative but fast
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

linesearch <- function(x, y) {
  alpha <- 0.01
  
  g <- gradient(x, y)
  previous <- objective(x, y)
  for (i in 1:100) {
    newx <- x - alpha*g[1]
    newy <- y - alpha*g[2]
    current <- objective(newx, newy)
    if (current > previous) {
      break
    }
    x <- newx
    y <- newy
    previous <- current
  }
  return(i*alpha)
}

grad_descent <- function(x0, y0, n, adaptive=TRUE) {
  xs <- c(x0)
  ys <- c(y0)
  ts <- c()
  for (i in 1:n) {
    g <- gradient(xs[i], ys[i])
    if (adaptive) {
      t <- linesearch(xs[i], ys[i])
    } else {
      t <- 0.1
    }
    xs <- c(xs, xs[i] - t*g[1])
    ys <- c(ys, ys[i] - t*g[2])
    ts <- c(ts, t)
  }
  return(list(x=xs,y=ys, t=ts))
}

xc <- seq(-4, 4, length=50)
yc <- seq(-4, 4, length=50)
Xc <- outer(xc, rep(1, length(yc)))
Yc <- outer(rep(1, length(xc)), yc)
Zc <- objective(Xc, Yc)

par(mfrow=c(1, 2))

contour(xc, yc, Zc, levels=c(20, 40, 60, 80), col="gray", labcex=0.9, axes = FALSE, ylim=c(-4, 4), main="Adaptive")
result_adaptive <- grad_descent(2, -2, 5)
lines(result_adaptive$x, result_adaptive$y)
points(result_adaptive$x, result_adaptive$y, pch=20)
offsets_x <- c(0, 0, 0, 0.1, 0, 0)
offsets_y <- c(-0.4, 0.4, -0.4, 0.4, -0.4, 0.4)
text(result_adaptive$x + offsets_x, result_adaptive$y + offsets_y, 1:6)

contour(xc, yc, Zc, levels=c(20, 40, 60, 80), col="gray", labcex=0.9, axes = FALSE, ylim=c(-4, 4), main="Fixed")
result_fixed <- grad_descent(2, -2, 5, adaptive = FALSE)
lines(result_fixed$x, result_fixed$y)
offsets_y <- c(-0.4, 0.4, 0.4, -0.2, 0.4, -0.4)
offsets_x <- c(0, 0, 0, 0.3, 0, 0)
points(result_fixed$x, result_fixed$y, pch=20)
text(result_fixed$x + offsets_x, result_fixed$y + offsets_y, 1:6)


