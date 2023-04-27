set.seed(1234)

# True model
true.model <- function(x) {
  sin(x) + sqrt(x)
}

# Sample data from true model + noise
n <- 20
x <- seq(0, 5, length.out = n)
y <- true.model(x) + 0.3*rnorm(n)

# Fit polynomial model and return RSS
fit.model <- function(degree) {
  if (degree > 0) {
    m <- lm(y ~ poly(x, degree, raw=TRUE))
  } else {
    m <- lm(y ~ 1)
  }
  sum(m$residuals^2)
}

# Compare penalty terms
degrees <- seq(4, 10)
rss <- sapply(degrees, fit.model)
base <- n*log(rss/n)
penalty.aic <- 2*(degrees + 1)
penalty.bic <- (degrees + 1) * log(n)

# Plot data
plot.data <- function() {
  plot(x, y, pch = 19, xlab = "X", ylab = "Y")
}

# Plot goodness-of-fit functions
plot.gof <- function() {
  par(mfrow=c(1, 3))
  plot(degrees, base, type = "b", 
       col = "darkgray", xlab = "", ylab = "", lwd = 2,
       main = "N log(RSS/N)")
  plot(degrees, base + penalty.aic, type = "b", 
       col = "darkgray", xlab = "", ylab = "", lwd = 2,
       main = "N log(RSS/N) + AIC")
  plot(degrees, base + penalty.bic, type = "b", 
       col = "darkgray", xlab = "", ylab = "", lwd = 2,
       main = "N log(RSS/N) + BIC")
}

# Plot optimal model
plot.optim <- function() {
  plot.data()
  m7 <- lm(y ~ poly(x, 7, raw = TRUE))
  xplot <- seq(0, 5, length.out = 50)
  yplot <- predict(m7, data.frame(x = xplot))
  lines(xplot, yplot, lwd = 2, col = "lightblue")
}