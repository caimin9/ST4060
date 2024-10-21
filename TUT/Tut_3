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
N <- 100
theta0 <- 3
theta1 <- 1.5
X <- c(1, 2, 5, 5.5, 9)
weights <- c(0.1, 0.1, 0.35, 0.35, 0.1)

# Extend X to match the required number of observations by repeating it
X_full <- rep(X, length.out = N)

# Initialize containers for the OLS and WLS results
ols_results <- numeric(N)
wls_results <- numeric(N)

# Perform the simulation and analysis
set.seed(123)  # for reproducibility
for (i in 1:N) {
  epsilon <- rnorm(length(X_full), mean = 0, sd = 1.2)
  Y <- theta0 + theta1 * X_full + epsilon

  # Ordinary Least Squares Estimation
  ols_model <- lm(Y ~ X_full - 1)  # '-1' to omit the intercept since it's already included
  ols_results[i] <- coefficients(ols_model)

  # Weighted Least Squares Estimation
  wls_model <- lm(Y ~ X_full - 1, weights = rep(weights, length.out = N))
  wls_results[i] <- coefficients(wls_model)
}

# To view results
mean(ols_results)  # Averaged result of OLS estimations
mean(wls_results)  # Averaged result of WLS estimations



