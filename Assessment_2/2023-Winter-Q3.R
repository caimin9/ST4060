# Load necessary libraries
library(splines)  # For B-spline functions
library(MASS)     # For the Boston dataset

# Set the random seed
set.seed(4060)

# Prepare the data
x <- Boston$nox
y <- Boston$medv

knots <- quantile(x, probs = c(0.15, 0.40, 0.60, 0.70, 0.85))
print("Knots at quantiles:")
print(knots)

bspline_fit <- lm(y ~ bs(x, knots = knots))

coefficients <- coef(bspline_fit)
print("B-spline Coefficient Estimates:")
print(coefficients)

#b--------------------------
newx <- data.frame(x = c(0.4, 0.5, 0.6))
predictions_b <- predict(bspline_fit, newdata = newx)
print("Predicted y values from B-spline at new x values:")
print(data.frame(newx, Predicted_y = predictions_b))


#c)-----------------------------------------------------
set.seed(4060)
pspline_fit <- smooth.spline(x, y, cv = TRUE)
penalized_RSS <- pspline_fit$pen.crit
print(paste("P-spline Penalized Criterion (RSS):", round(penalized_RSS, 3)))

#ii)
# Plot the data
plot(x, y, col = "black", pch = 16, xlab = "nox", ylab = "medv", main = "B-spline and P-spline Fits")

# Order x values for smooth plotting
x_ordered <- order(x)
x_sorted <- x[x_ordered]

# B-spline predictions over the range of x
bspline_pred <- predict(bspline_fit, newdata = data.frame(x = x_sorted))

# P-spline predictions over the range of x
pspline_pred <- predict(pspline_fit, x = x_sorted)

# Add B-spline fit to the plot
lines(x_sorted, bspline_pred, col = "red", lwd = 2, lty = 2)

# Add P-spline fit to the plot
lines(x_sorted, pspline_pred$y, col = "blue", lwd = 2)

# Add legend
legend("topright", legend = c("B-spline", "P-spline"), col = c("red", "blue"), lty = c(2,1), lwd = 2)


#d-----------------------
# New x values
newx_values <- c(0.4, 0.5, 0.6)

# Generate predictions
predictions_p <- predict(pspline_fit, x = newx_values)

# Prepare data frame for comparison
comparison <- data.frame(
  x = newx_values,
  B_spline_Pred = predictions_b,
  P_spline_Pred = predictions_p$y
)

print("Comparison of Predicted y values at new x values:")
print(comparison)
#The predicted values from both models are similar but not identical.
#B-spline: The B-spline model uses fixed knots at specified quantiles, which may lead to more abrupt changes in the fitted curve.
#P-spline (Smoothing Spline): The P-spline model uses a smoothing parameter optimized via cross-validation, leading to a smoother fit that balances the trade-off between flexibility and overfitting.
#The slight differences in predictions reflect the different ways each model fits the data, with the P-spline generally providing a smoother estimate.



#e---------------------------------
set.seed(4060)
#create fold
n <- length(y)
K <- 5
folds <- sample(1:K, n, replace = TRUE)
predictions_cv <- numeric(n)

#cv
for (k in 1:K) {
  # Training and test indices
  train_indices <- which(folds != k)
  test_indices <- which(folds == k)
  
  # Training data
  x_train <- x[train_indices]
  y_train <- y[train_indices]
  
  # Test data
  x_test <- x[test_indices]
  y_test <- y[test_indices]
  
  # Fit the P-spline on training data
  pspline_cv_fit <- smooth.spline(x_train, y_train, cv = TRUE)
  
  # Generate predictions on test data
  predictions <- predict(pspline_cv_fit, x = x_test)$y
  
  # Store predictions
  predictions_cv[test_indices] <- predictions
}

# Define RMSE function
RMSE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Calculate RMSE
rmse_cv <- RMSE(y, predictions_cv)
print(paste("5-Fold CV RMSE for P-spline:", round(rmse_cv, 3)))



#Alternate approach if p-spline can't be computed

folds <- sample(1:K, n, replace = TRUE)
predictions_lm <- numeric(n)
for (k in 1:K) {
  # Training and test indices
  train_indices <- which(folds != k)
  test_indices <- which(folds == k)
  
  # Training data
  x_train <- x[train_indices]
  y_train <- y[train_indices]
  
  # Test data
  x_test <- x[test_indices]
  y_test <- y[test_indices]
  
  # Fit linear model
  lm_cv_fit <- lm(y_train ~ x_train)
  
  # Generate predictions
  predictions <- predict(lm_cv_fit, newdata = data.frame(x_train = x_test))
  
  # Store predictions
  predictions_lm[test_indices] <- predictions
}

# Calculate RMSE
rmse_lm_cv <- RMSE(y, predictions_lm)
print(paste("5-Fold CV RMSE for Linear Model:", round(rmse_lm_cv, 3)))

#The P-spline model tends to provide a smoother fit to the data due to its smoothing parameter, which can result in better generalization performance as indicated by the lower RMSE compared to the linear model.
#Cross-validation is a valuable tool for assessing the predictive performance of a model and helps in selecting models that generalize well to unseen data.
