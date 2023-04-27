# Simple Morris screening for Streeter-Phelps

library(reshape2)
library(ggplot2)
library(gridExtra)

source("scripts/03a-parameter-estimation/SP-utils.R")

set.seed(1234)

n <- 4 # number of subdivisions

# Order of the rows: k1, k2, DO_sat, DO0, BOD_in, BOD0
bounds <- matrix(c(0.1, 0.6,
                   0.1, 0.6,
                   10, 12,
                   6, 10,
                   0.5, 1.5,
                   6, 10), byrow = T, ncol = 2)

deltas <- (bounds[,2] - bounds[,1]) / n
times <- seq(0, 25, length.out = 100)

sp.call <- function(params) {
  do.call(streeter_phelps_numerical, c(list(times), as.list(params)))$DO
}

do_run <- function(params) {
  differences <- c()
  previous <- sp.call(params)
  for (i in 1:length(params)) {
    params[[i]] <- params[[i]] + deltas[[i]]
    current <- sp.call(params)
    differences <- c(differences, (current - previous)/deltas[i])
    previous <- current
  }

  m <- matrix(differences, byrow = F, ncol = length(params))
  colnames(m) <- c("k1", "k2", "DO_sat", "DO0", "BOD_in", "BOD0")

  return(m)
}

rundata <- list()
nruns <- 5
for (r in 1:nruns) {
  idxs <- sample(0:(n-1), nrow(bounds), replace = TRUE)
  params <- bounds[, 1] + idxs*deltas
  EE <- do_run(params)
  rundata[[r]] <- EE
}

mean.EE <- apply(simplify2array(rundata), 1:2, mean)
sd.EE <- apply(simplify2array(rundata), 1:2, sd)

mean.df <- as.data.frame(mean.EE)
mean.df$time <- times
mean.df.long <- melt(mean.df, id="time")

sd.df <- as.data.frame(sd.EE)
sd.df$time <- times
sd.df.long <- melt(sd.df, id="time")

p <- ggplot(mean.df.long, aes(x=time, y=value)) +
  geom_line(aes(color=variable)) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  xlab("Time") + ylab("") +
  ggtitle("Mean EE")
q <- ggplot(sd.df.long, aes(x=time, y=value)) +
  geom_line(aes(color=variable)) +
  theme(legend.position="bottom",
        legend.title = element_blank()) +
  xlab("Time") + ylab("") +
  ggtitle("Stdev EE")
grid.arrange(p, q, ncol = 2)
