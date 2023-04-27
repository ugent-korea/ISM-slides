set.seed(1234)

# True model
true.model <- function(x) {
  sin(x) + sqrt(x)
}

# Sample data from true model + noise
n <- 100
x <- seq(0, 5, length.out = n)
y <- true.model(x) + 0.3*rnorm(n)


# Fit polynomial
run.experiment <- function() {
  ss_mask <- sample(1:length(x), 25)
  x_ss <- x[ss_mask]
  y_ss <- y[ss_mask]
  x_rest <- x[-ss_mask]
  y_rest <- y[-ss_mask]
  
  plot(x_ss, y_ss, pch=19, col=1, xlim=c(0, 3), ylim=c(-0.4, 3.1), 
       axes=FALSE, frame.plot=TRUE, xlab="", ylab="")
  
  fit.model <- function(n, col) {
    if (n > 0) {
      m <- lm(y_ss ~ poly(x_ss, n, raw=TRUE))
    } else {
      m <- lm(y_ss ~ 1)
    }
    x_plot <- seq(min(x), max(x), length.out = 100)
    y_plot <- predict(m, data.frame(x_ss=x_plot))
    lines(x_plot, y_plot, col = col, lwd = 2)
  }
    
  fit.model(1, col = 3)
  fit.model(2, col = 4)
  fit.model(8, col = 6)
  legend(2, 1, legend = c("Deg. 1", "Deg. 2", "Deg. 8"),
         col = c(3, 4, 6), lwd = 2)
}

# Run 4 experiments
par(mfrow = c(2, 2), mar = c(0.5, 0.5, 0.5, 0.5))
run.experiment()
run.experiment()
run.experiment()
run.experiment()