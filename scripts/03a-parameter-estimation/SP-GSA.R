library(purrr)
library(ggplot2)
library(gridExtra)

source("scripts/03a-parameter-estimation/SP-utils.R")

n_simulations <- 100
k1 <- runif(n_simulations, min = 0.1, max = 0.8)
k2 <- runif(n_simulations, min = 0.1, max = 0.8)
times <- seq(0, 25, length.out = 50)

curves <- map2(
  k1, k2, \(x, y) streeter_phelps_numerical(times, x, y)) |>
  list_rbind(names_to = "id")

plot_mc_bod <- ggplot(curves, aes(x = time, y = BOD, group = id)) +
  geom_vline(xintercept = c(2, 15), linetype = "dashed", color = "red") +
  geom_line(alpha = 0.5)

plot_mc_do <- ggplot(curves, aes(x = time, y = DO, group = id)) +
  geom_vline(xintercept = c(2, 15), linetype = "dashed", color = "red") +
  geom_line(alpha = 0.5)

plot_mc_joint <- grid.arrange(plot_mc_bod, plot_mc_do, ncol = 1)
print(plot_mc_joint)

# Grab two timepoints to compare
curves_t2 <- curves[seq(5, nrow(curves), by = 50),]
curves_t2$k1 <- k1
curves_t2$k2 <- k2
curves_t15 <- curves[seq(31, nrow(curves), by = 50),]
curves_t15$k1 <- k1
curves_t15$k2 <- k2
curves_selected <- rbind(curves_t2, curves_t15)


p_k1_bod <- ggplot(
  curves_selected,
  aes(x = k1, y = BOD, color = as.factor(round(time)))) +
  geom_point() + scale_color_discrete(name = "Time")
p_k2_bod <- ggplot(
  curves_selected,
  aes(x = k2, y = BOD, color = as.factor(round(time)))) +
  geom_point() + scale_color_discrete(name = "Time")
p_k1_do <- ggplot(
  curves_selected,
  aes(x = k1, y = DO, color = as.factor(round(time)))) +
  geom_point() + scale_color_discrete(name = "Time")
p_k2_do <- ggplot(
  curves_selected,
  aes(x = k2, y = DO, color = as.factor(round(time)))) +
  geom_point() + scale_color_discrete(name = "Time")

p_all <- grid.arrange(p_k1_bod, p_k2_bod, p_k1_do, p_k2_do, ncol = 2)
print(p_all)