library(ggplot2)
library(gridExtra)
library(reshape2)

source("scripts/03a-parameter-estimation/SP-utils.R")

times <- seq(0, 25, length.out = 100)
S_k1 <- S_DO_k1(times, 0.35, 0.40)
S_k2 <- S_DO_k2(times, 0.35, 0.40)
S_DO_sat <- S_DO_DO_sat(times, 0.35, 0.40)

data <- data.frame(
  time = times,
  k1 = S_k1,
  k2 = S_k2,
  DO_sat = S_DO_sat
)
data <- melt(data, id="time")

p <- ggplot(data, aes(x = time, y = value)) +
  geom_line(aes(color = variable)) +
  xlab("Time") + ylab("") +
  ggtitle("DO: absolute sensitivity")
print(p)