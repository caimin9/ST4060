#Question 3.1

# Set the working directory
setwd("/desired/path")

# Load the dataset and inspect
data("faithful")
?faithful

# Plot eruptions against waiting times
plot(faithful$waiting, faithful$eruptions, main="Eruptions vs Waiting Time",
     xlab="Waiting time in mins", ylab="Eruption duration in mins")

# Fit the linear model
lm.out <- lm(eruptions ~ waiting, data=faithful)

# Display coefficients α and β
print(coefficients(lm.out))

# Add the regression line to the plot
abline(lm.out, col="blue")

# Display a summary of the model
print(summary(lm.out))

# Store the value of the adjusted R-squared
adjusted.R2 <- summary(lm.out)$adj.r.squared

# 95% confidence interval for waiting time of 80 minutes
print(predict(lm.out, newdata=data.frame(waiting=80), interval="confidence"))

# Generate a 5-step-ahead prediction
max_waiting <- max(faithful$waiting)
new_data <- data.frame(waiting=(max_waiting+1):(max_waiting+5))
predictions <- predict(lm.out, newdata=new_data, interval="prediction")

# Extend plot axes and add forecast points
plot(faithful$waiting, faithful$eruptions, xlim=c(40, max_waiting+5), ylim=c(1.6, 6),
     main="Extended Forecast of Eruptions", xlab="Waiting time in mins", ylab="Eruption duration in mins")
abline(lm.out, col="blue")
points(new_data$waiting, predictions[,1], col="red", pch=19)

# Residual diagnostics
par(mfrow=c(1,2))
plot(resid(lm.out), main="Residuals of the Model")
qqnorm(resid(lm.out))
qqline(resid(lm.out), col="red")

# Writing a custom lm() function
mylm <- function(x, y, DOPLOT=FALSE){
  xm <- x - mean(x)
  ym <- y - mean(y)
  b <- sum(xm * ym) / sum(xm^2)
  a <- mean(y) - b * mean(x)
  res <- y - a - b * x
  out <- list(myintercept=a, mycoef=b, residuals=res)
  if(DOPLOT){
    plot(x, y, main="Plot with Linear Fit", xlab="X", ylab="Y")
    abline(a, b, col='red', lwd=1.5)
  }
  return(out)
}

# Test the plotting capability
mylm(faithful$waiting, faithful$eruptions, DOPLOT=TRUE)


#Question 3.2-------------------------------------------------------------
#If you have heteroscedasticity in your data (i.e., the variance of the residuals is not constant), you can perform a Weighted Least Squares (WLS) regression using the same lm() function by specifying the weights argument:
# How to check if variance of residuals isn'y constant:
# - Residual Plot: If the residual plot shows a random scatter around the horizontal line at zero without any pattern, this suggests homoscedasticity. Patterns such as a funnel shape would indicate heteroscedasticity.



# Define the constants and parameters
# Define parameters
theta0 <- 3
theta1 <- 1.5
X <- c(1, 2, 5, 5.5, 9)
sigma <- 1.2
weights <- c(0.1, 0.1, 0.35, 0.35, 0.1)
N <- 100

# Simulate data
set.seed(123) # For reproducibility
epsilon <- rnorm(N, mean = 0, sd = sigma)
Y <- theta0 + theta1 * rep(X, length.out = N) + epsilon

# Fit Ordinary Least Squares (OLS)
ols_model <- lm(Y ~ rep(X, length.out = N))

# Fit Weighted Least Squares (WLS)
wls_model <- lm(Y ~ rep(X, length.out = N), weights = rep(weights, length.out = N))

# Display summaries for both models
summary_ols <- summary(ols_model)
summary_wls <- summary(wls_model)

# Output results
print("OLS Results:")
print(summary_ols)

print("WLS Results:")
print(summary_wls)

# Comparing parameter estimates
cat("Comparison of Parameter Estimates:\n")
cat("OLS Intercept:", summary_ols$coefficients[1,1], "WLS Intercept:", summary_wls$coefficients[1,1], "\n")
cat("OLS Slope:", summary_ols$coefficients[2,1], "WLS Slope:", summary_wls$coefficients[2,1], "\n")

#----------------------------------------------------------------------------------------------------------------------

# can also do the question this way
# Set the parameters and data
# Set the parameters and data
theta0 <- 3
theta1 <- 1.5
X <- c(1, 2, 5, 5.5, 9)
weights <- c(0.1, 0.1, 0.35, 0.35, 0.1)
sigma <- 1.2
N <- 100

# Initialize matrices to store the coefficients
ols_coefficients <- matrix(NA, nrow = N, ncol = 2)  # To store intercept and slope for OLS
wls_coefficients <- matrix(NA, nrow = N, ncol = 2)  # To store intercept and slope for WLS

set.seed(123)  # For reproducibility

# Simulation loop
for (i in 1:N) {
    # Generate noise
    epsilon <- rnorm(length(X), mean = 0, sd = sigma)

    # Generate Y based on the linear model
    Y <- theta0 + theta1 * X + epsilon

    # Fit Ordinary Least Squares (OLS) model
    ols_model <- lm(Y ~ X)

    # Store OLS coefficients
    ols_coefficients[i, ] <- coef(ols_model)

    # Fit Weighted Least Squares (WLS) model
    wls_model <- lm(Y ~ X, weights = weights)

    # Store WLS coefficients
    wls_coefficients[i, ] <- coef(wls_model)
}

# Analyze the results
mean_ols_coefficients <- colMeans(ols_coefficients)
mean_wls_coefficients <- colMeans(wls_coefficients)

print("Mean OLS Coefficients:")
print(mean_ols_coefficients)

print("Mean WLS Coefficients:")
print(mean_wls_coefficients)

# Plotting to compare the coefficient distributions
par(mfrow = c(1, 2))
hist(ols_coefficients[, 2], main = "Distribution of OLS Slopes", xlab = "Slope", col = "blue")
hist(wls_coefficients[, 2], main = "Distribution of WLS Slopes", xlab = "Slope", col = "red")


