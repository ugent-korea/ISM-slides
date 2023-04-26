set.seed(1234)

source("scripts/03a-parameter-estimation/SP-utils.R")

library(ggplot2)
library(gridExtra)
library(reshape2)

samples.time <- seq(0, 25, by = 1.0)
samples <- generate.sample.data(samples.time, 0.35, 0.4)

plot.exact.trajectories <- function() {
  times <- seq(0, 25, length.out = 100)

  prepare_one <- function(k1) {
    d <- melt(streeter_phelps_exact(times, k1, 0.35), id="time")
    d[,"Parameter"] <- paste0("k1 = ", k1)
    return(d)
  }

  data <- do.call(rbind, lapply(c(0.25, 0.3, 0.4), prepare_one))
  samples <- melt(samples, id="time")

  ggplot(data, aes(x=time, y=value)) +
    geom_line(aes(color=Parameter)) +
    geom_point(data = samples, aes(x=time, y=value)) +
    facet_grid(variable ~., scales = "free_y") +
    ylab("") + xlab("Time")
}

p <- plot.exact.trajectories()
print(p)

plot.fitted.trajectory <- function() {
  times <- seq(0, 25, length.out = 100)

  data <- melt(streeter_phelps_exact(times, 0.35, 0.39), id="time")
  samples <- melt(samples, id="time")

  ggplot(data, aes(x=time, y=value)) +
    geom_line(color = "Red", size=1, show.legend = FALSE) +
    geom_point(data = samples, aes(x=time, y=value)) +
    facet_grid(variable ~., scales = "free_y") +
    ylab("") + xlab("Time") + ggtitle("Optimal parameters: k1 = 0.35, k2 = 0.39")
}

q <- plot.fitted.trajectory()
print(q)