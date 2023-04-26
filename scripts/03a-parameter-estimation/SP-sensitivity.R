library(ggplot2)
library(gridExtra)

source("scripts/03a-parameter-estimation/SP-utils.R")

times <- seq(0, 25, length.out = 100)
exact <- S_DO_k1_exact(times, 0.35, 0.40)
approximate <- S_DO_k1(times, 0.35, 0.40)

data <- data.frame(
  time = times,
  exact = exact,
  approximate = approximate,
  diff = exact - approximate
)

p_exact <- ggplot(data, aes(x = time)) +
  geom_line(aes(y = exact), color = 4) +
  xlab("") + ylab("") +
  ggtitle("Exact sensitivity")
p_approx <- ggplot(data, aes(x = time)) +
  geom_line(aes(y = approximate), color = 4) +
  xlab("Time") + ylab("") +
  ggtitle("Approximate sensitivity")
p_diff <- ggplot(data, aes(x = time, y = diff)) +
  geom_line(color = 2) +
  xlab("Time") + ylab("") +
  ggtitle("Difference exact - approximate")

grid.arrange(p_exact, p_diff, p_approx, ncol = 2,
             layout_matrix = cbind(c(1, 3), c(2, 2)))
