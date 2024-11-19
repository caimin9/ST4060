# Load necessary library
library(stats)

# 1. Generate Sample Data
set.seed(123) # for reproducibility
X <- runif(100, min=-2, max=2)  # 100 random numbers from a uniform distribution
true_beta <- 1
Y <- exp(true_beta * X) + rnorm(100, sd=0.1)  # Adding some normal noise

# 2. Define the Model Function
model_function <- function(beta, X) {
  exp(beta * X)
}

# 3. Define the Loss Function (Sum of Least Squares)
loss_function <- function(beta, Y, X) {
  sum((Y - model_function(beta, X))^2)
}

# 4. Use `optim` to Minimize the Loss Function with a suitable method
initial_beta <- 0.5  # Initial guess for beta
optim_result <- optim(par = initial_beta, fn = loss_function, Y = Y, X = X, method = "BFGS")

# Output the results
optim_result$par  # This gives the estimated value of beta

#-------------------------------3.6--------------------------------------------------------------


# Install and load optimx package
if (!require(optimx)) install.packages("optimx", dependencies=TRUE)
library(optimx)

# 1. Generate Sample Data
set.seed(123) # for reproducibility
X <- runif(100, min=-2, max=2)  # 100 random numbers from a uniform distribution
true_beta <- 1
Y <- exp(true_beta * X) + rnorm(100, sd=0.1)  # Adding some normal noise

# 2. Define the Model Function
model_function <- function(beta, X) {
  exp(beta * X)
}

# 3. Define the Loss Function (Sum of Least Squares)
loss_function <- function(beta, Y, X) {
  sum((Y - model_function(beta, X))^2)
}

# 4. Use `optimx` to Minimize the Loss Function
initial_beta <- 0.5  # Initial guess for beta
optimx_result <- optimx(par = initial_beta, fn = loss_function, Y = Y, X = X, method = "BFGS")

# Output the results
optimx_result$par  # This gives the estimated value of beta
