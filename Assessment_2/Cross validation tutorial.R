# Load necessary library
library(MASS)

# Set seed for reproducibility
set.seed(1)

# Prepare the data
x <- Boston[, c("crim", "indus", "rm", "tax")]
y <- Boston$medv

# Define RMSE function
RMSE <- function(real, predicted) {
  sqrt(mean((real - predicted)^2))
}

# Number of observations
n <- length(y)

# ------------------------------
# Part (a): 50%-50% Train-Test Split
# ------------------------------

# Generate random indices for 50% of the data
indices <- sample(1:n, size = n / 2, replace = FALSE)

# Split the data into training and testing sets
x_train <- x[indices, ]
y_train <- y[indices]

x_test <- x[-indices, ]
y_test <- y[-indices]

# Combine x_train and y_train into a data frame
train_data <- data.frame(y = y_train, x_train)

# Fit the linear model
model <- lm(y ~ ., data = train_data)

# Prepare test data with same column names
test_data <- data.frame(x_test)
colnames(test_data) <- colnames(x_train)

# Generate predictions on the test data
predictions <- predict(model, newdata = test_data)

# Calculate RMSE
rmse_a <- RMSE(y_test, predictions)
print(paste("RMSE for 50%-50% train-test split:", round(rmse_a, 3)))

# ------------------------------
# Part (b): Leave-One-Out Cross-Validation (LOO-CV)
# ------------------------------

# Initialize a vector to store predictions
predictions <- numeric(n)

for (i in 1:n) {
  # Training data excludes observation i
  x_train <- x[-i, ]
  y_train <- y[-i]
  
  # Test data is observation i
  x_test <- x[i, , drop = FALSE]
  
  # Fit the model
  train_data <- data.frame(y = y_train, x_train)
  model <- lm(y ~ ., data = train_data)
  
  # Prepare test data with same column names
  test_data <- data.frame(x_test)
  colnames(test_data) <- colnames(x_train)
  
  # Predict
  predictions[i] <- predict(model, newdata = test_data)
}

# Calculate RMSE
rmse_b <- RMSE(y, predictions)
print(paste("RMSE for Leave-One-Out CV:", round(rmse_b, 3)))

# ------------------------------
# Part (c): K-Fold CV with K = 5
# ------------------------------

K <- 5
set.seed(1)
folds <- sample(1:K, n, replace = TRUE)
predictions <- numeric(n)

for (k in 1:K) {
  # Training and test indices
  train_indices <- which(folds != k)
  test_indices <- which(folds == k)
  
  # Training data
  x_train <- x[train_indices, ]
  y_train <- y[train_indices]
  
  # Test data
  x_test <- x[test_indices, ]
  
  # Fit the model
  train_data <- data.frame(y = y_train, x_train)
  model <- lm(y ~ ., data = train_data)
  
  # Prepare test data
  test_data <- data.frame(x_test)
  colnames(test_data) <- colnames(x_train)
  
  # Predict
  preds <- predict(model, newdata = test_data)
  
  # Store predictions
  predictions[test_indices] <- preds
}

# Calculate RMSE
rmse_c <- RMSE(y, predictions)
print(paste("RMSE for 5-Fold CV:", round(rmse_c, 3)))

# ------------------------------
# Part (d): K-Fold CV with K = 10
# ------------------------------

K <- 10
set.seed(1)
folds <- sample(1:K, n, replace = TRUE)
predictions <- numeric(n)

for (k in 1:K) {
  # Training and test indices
  train_indices <- which(folds != k)
  test_indices <- which(folds == k)
  
  # Training data
  x_train <- x[train_indices, ]
  y_train <- y[train_indices]
  
  # Test data
  x_test <- x[test_indices, ]
  
  # Fit the model
  train_data <- data.frame(y = y_train, x_train)
  model <- lm(y ~ ., data = train_data)
  
  # Prepare test data
  test_data <- data.frame(x_test)
  colnames(test_data) <- colnames(x_train)
  
  # Predict
  preds <- predict(model, newdata = test_data)
  
  # Store predictions
  predictions[test_indices] <- preds
}

# Calculate RMSE
rmse_d <- RMSE(y, predictions)
print(paste("RMSE for 10-Fold CV:", round(rmse_d, 3)))
