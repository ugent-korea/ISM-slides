library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

energy <- read.csv("scripts/03a-parameter-estimation/energy.csv") %>%
  fill(Year) %>%
  mutate(Date = make_date(Year, match(Month, months), 1))

p1 <- energy %>%
  ggplot(aes(Date, Consumption)) +
  #geom_bar(stat='identity') +
  geom_line() + geom_point() +
  theme_classic() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("3 months"),
               labels = scales::label_date_short()) +
  scale_y_continuous("Energy consumption (1000s of TOE)",
                     breaks = scales::breaks_extended(8),
  )

correlation <- acf(energy$Consumption, pl = FALSE)
df <- as.data.frame(correlation$acf)
colnames(df) <- c("ACF")
df$lag <- 1:nrow(df)

threshold <- qnorm(0.95) / sqrt(nrow(df))

p2 <- ggplot(df, aes(lag, ACF)) +
  geom_bar(stat="identity") +
  theme_classic() +
  scale_x_continuous(breaks = df$lag) +
  geom_hline(yintercept=c(threshold, -threshold),
             linetype="dashed", color = "red", size=1)

grid.arrange(p1, p2, ncol = 1)