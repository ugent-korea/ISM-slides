library(deSolve)

#' Convenience function to generate an "initial state" object
create_spec <- function(k1 = 0.35, k2 = 0.40, DO_sat = 8.5, BOD_in = 1, DO0 = 8.5, BOD0 = 7.33) {
  parameters <- c(
    k1 = k1,
    k2 = k2,
    BOD_in = BOD_in,
    DO_sat = DO_sat
  )
  state <- c(
    BOD = BOD0,
    DO = DO0
  )
  list(parameters=parameters, state=state)
}

#' Solve the Streeter-Phelps model numerically.
streeter_phelps_numerical <- function(times, k1, k2, DO_sat = 8.5, DO0 = 8.5, BOD_in = 1, BOD0 = 7.33) {
  spec <- create_spec(k1, k2, DO_sat=DO_sat, DO0=DO0, BOD_in = BOD_in, BOD0 = BOD0)
  rhs <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      dBOD <- BOD_in - k1*BOD
      dDO <- k2*(DO_sat - DO) - k1*BOD
      
      list(c(dBOD, dDO))
    })
  }
  result <- ode(y = spec$state, times = times, func = rhs, parms = spec$parameters)
  as.data.frame(result[,c("time", "BOD", "DO")])
}

#' Exact solution of the Streeter-Phelps model.
#' 
#' Note: will return NaN if k1 == k2.
streeter_phelps_exact <- function(times, k1, k2) {
  spec <- create_spec(k1, k2)
  
  with(as.list(c(spec$state, spec$parameters)), {
    t <- times
    BOD_t <- BOD_in/k1 + (BOD - BOD_in/k1)*exp(-k1*t)
    D_t <- BOD_in/k2 +
      (DO_sat - DO - BOD_in/k2)*exp(-k2*t) +
      k1/(k2-k1)*(BOD - BOD_in/k1)*(exp(-k1*t) - exp(-k2*t))
    DO_t <- DO_sat - D_t
    
    data.frame(time=t, BOD=BOD_t, DO=DO_t)
  })
}

#' Generate simulated Streeter-Phelps data.
generate.sample.data <- function(times, k1, k2, sigma=0.1) {
  samples <- streeter_phelps_numerical(times, k1, k2)
  
  n <- nrow(samples)
  samples$BOD <- samples$BOD + rnorm(n, sd = sigma)
  samples$DO <- samples$DO + rnorm(n, sd = sigma)
  
  samples
}

#' Local sensitivity function for DO with respect to k1.
#' 
#' Obtained by differentiating the exact solution for DO with respect
#' to k1.
S_DO_k1_exact <- function(times, k1, k2) {
  spec <- create_spec(k1, k2)
  with(as.list(c(spec$state, spec$parameters)), {
    t <- times
    delta <- BOD - BOD_in/k1
    e1 <- exp(-k1*t)
    e2 <- exp(-k2*t)
    
    DO_k1 <- -(k2/(k2-k1)^2*delta*(e1-e2) +
                 1/(k2-k1)*BOD_in/k1*(e1-e2) -
                 k1/(k2-k1)*delta*t*e1)
    
    DO_k1
  })
}

#' Local sensitivity function for DO with respect to k1/k2/DO_sat.
#' 
#' Computed using central differences
S_DO_k1 <- function(times, k1, k2, eps = 1e-6) {
  (streeter_phelps_numerical(times, k1 + eps, k2)$DO -
   streeter_phelps_numerical(times, k1 - eps, k2)$DO)/(2*eps)
}
S_DO_k2 <- function(times, k1, k2, eps = 1e-6) {
  (streeter_phelps_numerical(times, k1, k2 + eps)$DO -
     streeter_phelps_numerical(times, k1, k2 - eps)$DO)/(2*eps)
}
S_DO_DO_sat <- function(times, k1, k2, DO_sat = 8.5, eps = 1e-6) {
  (streeter_phelps_numerical(times, k1, k2, DO_sat + eps)$DO -
     streeter_phelps_numerical(times, k1, k2, DO_sat - eps)$DO)/(2*eps)
}

#' Relative sensitivity functions
S_DO_k1_relative <- function(times, k1, k2, eps = 1e-6) {
  S_DO_k1(times, k1, k2, eps) * k1 / streeter_phelps_numerical(times, k1, k2)$DO 
}
S_DO_k2_relative <- function(times, k1, k2, eps = 1e-6) {
  S_DO_k2(times, k1, k2, eps) * k2 / streeter_phelps_numerical(times, k1, k2)$DO 
}
S_DO_DO_sat_relative <- function(times, k1, k2, DO_sat = 8.5, eps = 1e-6) {
  S_DO_DO_sat(times, k1, k2, DO_sat, eps) * DO_sat / streeter_phelps_numerical(times, k1, k2, DO_sat)$DO 
}

#' Root-mean-square sensitivity
delta_rmsq <- function(sensitivity) {
  sqrt(sum(sensitivity^2)/length(sensitivity))
}