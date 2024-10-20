# Question 1
set.seed(6040)

M <- 1000  # Number of repetitions
N <- 1000  # Sample size

bootstrap_counts <- numeric(M)

for (i in 1:M) {
  sample_indices <- sample(1:N, N, replace = TRUE)
  bootstrap_counts[i] <- length(unique(sample_indices))
}

percentage_selected <- mean(bootstrap_counts) / N * 100
cat("Average percentage of unique points selected:", round(percentage_selected, 2), "%\n")

#--------------------------------------------------------------------------
#Question 2
set.seed(6015)

M <- 1000  # Number of repetitions
N <- 100   # Sample size
a <- 3     # Shape parameter
b <- 2     # Rate parameter

sample_means <- numeric(M)

for (i in 1:M) {
  sample_data <- rgamma(N, shape = a, rate = b)
  sample_means[i] <- mean(sample_data)
}

# (a) Monte Carlo estimate of the expected value
expected_value <- mean(sample_means)
cat("Monte Carlo estimate of the expected value of the sample mean:", round(expected_value, 2), "\n")

# (b) Is this value surprising?
# Theoretical expected value for Gamma(a, b) is a/b.
theoretical_value <- a / b
cat("Theoretical expected value:", round(theoretical_value, 2), "\n")
cat("Is the value surprising? No, it is consistent with the theoretical value.\n")

# (c) Monte Carlo estimate of the standard error
standard_error <- sd(sample_means)
cat("Monte Carlo estimate of the standard error of the sample mean:", round(standard_error, 2), "\n")

#########################################################################################################
#Question 3
set.seed(4060)

library(datasets)

data(trees)
M <- 100  # Number of bootstrap samples

slope_estimates <- numeric(M)

for (i in 1:M) {
  bootstrap_sample <- trees[sample(1:nrow(trees), nrow(trees), replace = TRUE), ]
  model <- lm(Height ~ Girth, data = bootstrap_sample)
  slope_estimates[i] <- coef(model)[2]
}

# (a) Boxplot of sampling distribution of bootstrap estimates
boxplot(slope_estimates, main = "Bootstrap Estimates of Regression Slope", ylab = "Slope Estimate")

# (b) Bootstrap estimate of the expected value of the least squares estimator
expected_slope <- mean(slope_estimates)
cat("Bootstrap estimate of the expected value of the regression slope:", round(expected_slope, 2), "\n")

# (c) Bootstrap estimate for the standard error
standard_error_slope <- sd(slope_estimates)
cat("Bootstrap estimate of the standard error:", round(standard_error_slope, 2), "\n")

# (d) Empirical bootstrap confidence interval for the slope
confidence_interval <- quantile(slope_estimates, probs = c(0.025, 0.975))
cat("Bootstrap confidence interval for the regression slope: [", round(confidence_interval[1], 2), ", ", round(confidence_interval[2], 2), "]\n")


