# Illustration that formula for prediction interval (95% CI with SE 
# on predictions) gives the same result as prediction intervals obtained 
# from R.

heights <- read.csv("./datasets/01-linear-regression/heights-2022.csv", 
                    stringsAsFactors = TRUE)
m <- lm(Height ~ Palm.width, data=heights)

x_pred <- 8.75
x <- heights$Palm.width

# Via R (predict)
print("Prediction interval from R:")
pred <- predict(m, interval = "p", se.fit = TRUE,
                newdata = data.frame(Palm.width=x_pred))
print(pred)
y <- pred$fit[,"fit"]

# Manual
s_y <- sqrt(sum(m$residuals^2)/m$df.residual)
n <- m$df.residual + 2
delta2_x <- (x_pred - mean(x))^2
delta2_x_all <- sum((x - mean(x))^2)
standard_error <- s_y*sqrt(1 + 1/n + delta2_x/delta2_x_all) # remove the 1 for CI
margin <- qt(0.975, m$df.residual) * standard_error

print(paste0("Standard error (se.fit): ", standard_error))
print(paste0("95% CI (lwr, upr): ", y - margin, ", ", y + margin))