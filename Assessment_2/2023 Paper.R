library(splines)
library(MASS)
data("Boston")
x = Boston$nox
y = Boston$medv
set.seed(4060)

#-------------------------------------------a----------------------------------------------
# Fit B-spline
knots = quantile(x, c(0.15, 0.40, 0.60, 0.70, 0.85))
b_spline = lm(y ~ bs(x, knots = knots))
coef(b_spline)


#Plots (not needed)
plot(x, y, main = "B-spline (Red) and P-spline (Blue)", col = 'black', pch = 20)
abline(v = knots, lty = 2, col = "grey")


#-------------------------------------------b----------------------------------------------
newx = c(0.4, 0.5, 0.6)
spline_pred = predict(b_spline, newdata = data.frame(x = newx))

spline_pred

#---------------------------------------------c---------------------------------------------
#i)
smooth_spline = smooth.spline(x = x, y = y, cv = TRUE)
smooth_spline$pen.crit
lines(smooth_spline, col = 'blue', lwd = 3)

#ii)
reorder = order(x)
lines(x[reorder], predict(b_spline, newdata = data.frame(x = x))[reorder], col = 'red', lwd = 3)


#---------------------------------------------d---------------------------------------------
# Predictions from P-spline for new x values
newx = c(0.4, 0.5, 0.6)
p_spline_pred = predict(smooth_spline, x = newx)

# Print predictions
p_spline_pred$y

# Comparing predictions
cat("Predictions from B-spline:", spline_pred, "\n")
cat("Predictions from P-spline:", p_spline_pred$y, "\n")


#---------------------------------------------e---------------------------------------------
library(boot)

# Function to compute cross-validated prediction RMSE
rmse_cv = function(data, indices) {
  train = data[indices, ]
  test = data[-indices, ]
  fit = smooth.spline(x = train$x, y = train$y, cv = TRUE)
  pred = predict(fit, x = test$x)
  sqrt(mean((test$y - pred$y)^2))
}

# Data frame for cross-validation
data_cv = data.frame(x = x, y = y)

# Set seed for reproducibility
set.seed(4060)

# Running 5-fold cross-validation
cv_results = boot(data_cv, rmse_cv, R = 5, strata = rep(1:5, length.out = nrow(Boston)))

# Output RMSE from cross-validation
mean(cv_results$t)




