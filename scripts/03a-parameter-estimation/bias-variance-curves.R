library(tidyverse)

set.seed(1234)

# True model
true.model <- function(x) {
  sin(x) + sqrt(x)
}

# Sample data from true model + noise
n <- 100
x <- seq(0, 5, length.out = n)
y <- true.model(x) + 0.3*rnorm(n)

# Split into test-train
test_mask <- seq(1, length(x), by=10)
x_test <- x[test_mask]
x_train <- x[-test_mask]
y_test <- y[test_mask]
y_train <- y[-test_mask]

run.experiment <- function(n, repeats=100) {
  # Draw bootstrap samples for average predictor
  samples <- replicate(repeats, {
    ss_mask <- sample(1:length(x_train), length(x_train), replace = TRUE)
    x_ss <- x_train[ss_mask]
    y_ss <- y_train[ss_mask]
    if (n > 0) {
      m <- lm(y_ss ~ poly(x_ss, n, raw=TRUE))
    } else {
      m <- lm(y_ss ~ 1)
    }
    predict(m, data.frame(x_ss=x_test))
  })
  average_predictor <- apply(samples, 1, mean)
  b <- mean((average_predictor - true.model(x_test))^2)
  v <- var(average_predictor)
  return(c(bias=b, variance=v))
}

# Plot
degrees <- 0:6
bias_variance <- sapply(degrees, run.experiment)
b <- bias_variance["bias",]
v <- bias_variance["variance",]

plotdata <- data.frame(
  degree = 0:6,
  bias = bias_variance["bias",],
  variance = bias_variance["variance",]
) |> pivot_longer(!degree)

poly_bias_variance <-
  ggplot(plotdata, aes(x = degree, y = value, color = name)) +
  geom_line() +
  geom_point() +
  xlab("Degree") +
  ylab("Error") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(breaks = degrees)
