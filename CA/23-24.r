#Question1##########################
#a)

set.seed(4060)
theta <- 3
sigma <- 1.5
M <- 1000
N <- 30
results <- replicate(M, {
  sample_data <- rnorm(N, mean = theta, sd = sigma)
  x_bar <- mean(sample_data)
  s <- sd(sample_data)
  lower_bound <- x_bar - 1.96 * s / sqrt(N)
  upper_bound <- x_bar + 1.96 * s / sqrt(N)
  theta >= lower_bound & theta <= upper_bound
})
mean(results) # Monte Carlo estimate of p

#b)
set.seed(4060)
results <- replicate(M, {
  sample_data <- rnorm(N, mean = theta, sd = sigma)
  x_bar <- mean(sample_data)
  s <- sd(sample_data)
  lower_bound <- x_bar - 1.645 * s / sqrt(N)
  upper_bound <- x_bar + 1.645 * s / sqrt(N)
  theta >= lower_bound & theta <= upper_bound
})
mean(results) # Monte Carlo estimate of p

########--------------Q2
#a
library(MASS)
data(Animals)
set.seed(4060)
B <- 1000
bootstrap_means <- replicate(B, {
  sample_indices <- sample(nrow(Animals), replace = TRUE)
  sample_data <- Animals[sample_indices, ]
  mean(sample_data$brain)
})
mean(bootstrap_means) # Bootstrap estimate of mean brain weight

#b
bootstrap_ratios <- replicate(B, {
  sample_indices <- sample(nrow(Animals), replace = TRUE)
  sample_data <- Animals[sample_indices, ]
  mean(sample_data$brain / sample_data$body)
})
mean(bootstrap_ratios)
