library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)

set.seed(1234)

n <- 50
x <- seq(1, n)
resids_hetero <- sin(2*pi*x/n) + 1.1 * rnorm(n)
resids_homo <- rnorm(n)

data_plot <- function(id, col, title) {
  df <- data.frame(id=id, col=col)
  df %>% ggplot(aes(id, col)) +
    geom_point() + 
    xlab("Time") + ylab("") +
    ggtitle(title) +
    theme_classic()
}

autocorr_plot <- function(y) {
  df <- data.frame(
    acf=acf(y, plot = FALSE)$acf
  )
  df$lag <- seq(1, nrow(df))
  
  threshold <- qnorm(0.95) / sqrt(nrow(df))

  ggplot(df, aes(lag, acf)) +
    geom_bar(stat="identity") +
    scale_x_continuous(breaks = df$lag) +
    geom_hline(yintercept=c(threshold, -threshold),
               linetype="dashed", color = "red", size=1) +
    xlab("Lag") + ylab("Autocorrelation") +
    theme_classic()
}

p1 <- data_plot(x, resids_homo, "Random residuals")
p2 <- data_plot(x, resids_hetero, "Correlated residuals")
p3 <- autocorr_plot(resids_homo)
p4 <- autocorr_plot(resids_hetero)

grid.arrange(p1, p2, p3, p4, ncol = 2)

subsampled <- seq(1, length(resids_hetero), 4)
x_subsampled <- x[subsampled]
resids_subsampled <- resids_hetero[subsampled]

p5 <- data_plot(x_subsampled, resids_subsampled, "Subsampled")
p6 <- autocorr_plot(resids_subsampled)

#grid.arrange(p5, p6, ncol = 2)