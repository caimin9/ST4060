# Set seed for reproducibility
set.seed(123)

# Model Parameters
theta_0 <- 3         # Intercept
theta_1 <- 1.5       # Slope
sigma_squared <- 1.2 # Variance of error term
sigma <- sqrt(sigma_squared) # Standard deviation
N <- 100             # Number of observations

# Define unique X values and their probabilities
X_values <- c(1, 2, 5, 9)
probabilities <- c(1, 1, 3, 1) / 6 # Probabilities sum to 1

# Simulate predictor variable X
X <- sample(X_values, size = N, replace = TRUE, prob = probabilities)
epsilon <- rnorm(N, mean = 0, sd = sigma)
Y <- theta_0 + theta_1 * X + epsilon

# Create a data frame
data <- data.frame(X = X, Y = Y)

# Assign weights based on X values
data$w <- ifelse(data$X == 5, 0.35, 0.1)

# Inspect the first few rows
head(data)

# Fit OLS model
ols_model <- lm(Y ~ X, data = data)

# Fit WLS model
wls_model <- lm(Y ~ X, data = data, weights = w)

# Summaries
ols_summary <- summary(ols_model)
wls_summary <- summary(wls_model)

# Print OLS Summary
cat("=== OLS Model Summary ===\n")
print(ols_summary)

# Print WLS Summary
cat("\n=== WLS Model Summary ===\n")
print(wls_summary)

# Compare Parameter Estimates
ols_coef <- coef(ols_model)
wls_coef <- coef(wls_model)

comparison <- data.frame(
  Parameter = names(ols_coef),
  OLS_Estimate = ols_coef,
  WLS_Estimate = wls_coef
)

cat("\n=== Parameter Estimates Comparison ===\n")
print(comparison)

# Compare Standard Errors
ols_se <- ols_summary$coefficients[, "Std. Error"]
wls_se <- wls_summary$coefficients[, "Std. Error"]

se_comparison <- data.frame(
  Parameter = names(ols_coef),
  OLS_Std_Error = ols_se,
  WLS_Std_Error = wls_se
)

cat("\n=== Standard Errors Comparison ===\n")
print(se_comparison)

# Compare Residual Standard Error
residual_se <- data.frame(
  Model = c("OLS", "WLS"),
  Residual_SE = c(ols_summary$sigma, wls_summary$sigma)
)

cat("\n=== Residual Standard Error Comparison ===\n")
print(residual_se)

# Visualization
plot(data$X, data$Y, main = "OLS vs WLS Fit", 
     xlab = "X", ylab = "Y", pch = 19, col = "grey")

# Add OLS fitted line
abline(ols_model, col = "blue", lwd = 2)

# Add WLS fitted line
abline(wls_model, col = "red", lwd = 2, lty = 2)

# Highlight weighted observations (w=0.35)
points(data$X[data$w == 0.35], data$Y[data$w == 0.35], 
       col = "green", pch = 19, cex = 1.5)

# Add legend
legend("topleft", legend = c("Observed", "OLS Fit", "WLS Fit", "Weighted Observations"),
       col = c("grey", "blue", "red", "green"), 
       pch = c(19, NA, NA, 19), 
       lty = c(NA, 1, 2, NA), 
       lwd = c(NA, 2, 2, NA),
       pt.cex = c(1, NA, NA, 1.5))




#----------------------Do it using rep instead---------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Model Parameters
theta_0 <- 3         # Intercept
theta_1 <- 1.5       # Slope
sigma_squared <- 1.2 # Variance of error term
sigma <- sqrt(sigma_squared) # Standard deviation
N <- 100             # Number of observations

# Define the X pattern
X_pattern <- c(1, 2, 5, 5, 5, 9)

# Use rep to repeat the pattern to reach N=100 observations
# 'length.out' ensures the total length is exactly N=100
X <- rep(X_pattern, times = ceiling(N / length(X_pattern)), length.out = N)

# Define a named vector for weights based on X values
weights_map <- c("1" = 0.1, "2" = 0.1, "5" = 0.35, "9" = 0.1)

# Assign weights to each observation based on X
W <- weights_map[as.character(X)]

# Simulate error term epsilon
epsilon <- rnorm(N, mean = 0, sd = sigma)

# Generate response variable Y
Y <- theta_0 + theta_1 * X + epsilon

# Create a data frame with all variables
data <- data.frame(X = X, Y = Y, W = W)

# Inspect the first few rows
head(data)

# Fit OLS model
ols_model <- lm(Y ~ X, data = data)

# Fit WLS model using weights W
wls_model <- lm(Y ~ X, data = data, weights = W)

# Summaries
ols_summary <- summary(ols_model)
wls_summary <- summary(wls_model)

# Print OLS Summary
cat("=== OLS Model Summary ===\n")
print(ols_summary)

# Print WLS Summary
cat("\n=== WLS Model Summary ===\n")
print(wls_summary)

# Compare Parameter Estimates
ols_coef <- coef(ols_model)
wls_coef <- coef(wls_model)

comparison <- data.frame(
  Parameter = names(ols_coef),
  OLS_Estimate = ols_coef,
  WLS_Estimate = wls_coef
)

cat("\n=== Parameter Estimates Comparison ===\n")
print(comparison)

# Compare Standard Errors
ols_se <- ols_summary$coefficients[, "Std. Error"]
wls_se <- wls_summary$coefficients[, "Std. Error"]

se_comparison <- data.frame(
  Parameter = names(ols_coef),
  OLS_Std_Error = ols_se,
  WLS_Std_Error = wls_se
)

cat("\n=== Standard Errors Comparison ===\n")
print(se_comparison)

# Compare Residual Standard Error
residual_se <- data.frame(
  Model = c("OLS", "WLS"),
  Residual_SE = c(ols_summary$sigma, wls_summary$sigma)
)

cat("\n=== Residual Standard Error Comparison ===\n")
print(residual_se)

# Visualization
plot(data$X, data$Y, main = "OLS vs WLS Fit",
     xlab = "X", ylab = "Y", pch = 19, col = "grey")

# Add OLS fitted line
abline(ols_model, col = "blue", lwd = 2)

# Add WLS fitted line
abline(wls_model, col = "red", lwd = 2, lty = 2)

# Highlight weighted observations (W = 0.35)
points(data$X[data$W == 0.35], data$Y[data$W == 0.35],
       col = "green", pch = 19, cex = 1.5)

# Add legend
legend("topleft", legend = c("Observed", "OLS Fit", "WLS Fit", "Weighted Observations"),
       col = c("grey", "blue", "red", "green"),
       pch = c(19, NA, NA, 19),
       lty = c(NA, 1, 2, NA),
       lwd = c(NA, 2, 2, NA),
       pt.cex = c(1, NA, NA, 1.5))
