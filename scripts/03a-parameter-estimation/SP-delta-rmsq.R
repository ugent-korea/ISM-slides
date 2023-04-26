source("scripts/03a-parameter-estimation/SP-utils.R")

k1 <- 0.35
k2 <- 0.40
DO_sat <- 8.5

scheme1 <- seq(0, 2, by = 0.1)
scheme2 <- seq(0, 20, by = 2)
scheme3 <- seq(0, 10, by = 2)
scheme4 <- seq(10, 20, by = 2)

delta_k1 <- function(times) {
  delta_rmsq(S_DO_k1_relative(times, k1, k2))
}
delta_k2 <- function(times) {
  delta_rmsq(S_DO_k2_relative(times, k1, k2))
}
delta_DO_sat <- function(times) {
  delta_rmsq(S_DO_DO_sat_relative(times, k1, k2, DO_sat))
}

d1_k1 <- delta_k1(scheme1)
d1_k2 <- delta_k2(scheme1)
d1_sat <- delta_DO_sat(scheme1)

d2_k1 <- delta_k1(scheme2)
d2_k2 <- delta_k2(scheme2)
d2_sat <- delta_DO_sat(scheme2)

d3_k1 <- delta_k1(scheme3)
d3_k2 <- delta_k2(scheme3)
d3_sat <- delta_DO_sat(scheme3)

d4_k1 <- delta_k1(scheme4)
d4_k2 <- delta_k2(scheme4)
d4_sat <- delta_DO_sat(scheme4)