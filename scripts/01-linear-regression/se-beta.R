# Illustration that formula for SE(\beta) in the course notes gives
# the same results as the output from R

heights <- read.csv("./datasets/01-linear-regression/heights-2022.csv", 
                    stringsAsFactors = TRUE)
m <- lm(Height ~ Palm.width, data=heights)
x <- heights$Palm.width

# Via R
print(summary(m)$coefficients)

# By hand
mse <- sum(m$residuals^2)/m$df.residual
delta2_x_all <- sum((x - mean(x))^2)
se_beta <- sqrt(mse/delta2_x_all)
print(se_beta)