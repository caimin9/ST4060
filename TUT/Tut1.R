#Question 1.4
# Load necessary libraries
library(datasets)  # for the Old Faithful dataset
library(KernSmooth)  # for KDE and local polynomial regression

# Load the Old Faithful dataset
data("faithful")

# Part (a): Compute and plot a 2D KDE
# Extract the data
eruptions <- faithful$eruptions
waiting <- faithful$waiting

# Compute the 2D KDE
kde2d_result <- bkde2D(cbind(eruptions, waiting), bandwidth=c(0.1, 1))

# Plot the 2D KDE
filled.contour(kde2d_result$x1, kde2d_result$x2, kde2d_result$fhat,
               color.palette=colorRampPalette(c("white", "blue", "yellow", "red", "darkred")),
               xlab="Eruption duration (minutes)", ylab="Waiting time (minutes)",
               main="2D KDE of Old Faithful Eruptions")

# Part (b): Nonparametric regression using local polynomials
# Fit the local polynomial regression
locpoly_result <- locpoly(x=eruptions, y=waiting, bandwidth=0.3, degree=2)

# Plot the nonparametric regression result
plot(eruptions, waiting, xlab="Eruption duration (minutes)", ylab="Waiting time (minutes)",
     main="Local Polynomial Regression", pch=19, col="gray")
lines(locpoly_result$x, locpoly_result$y, col="red", lwd=2)



#Question 1.5
# Load necessary libraries
library(MASS)  # for mvrnorm and kde2d

# Parameters
mu <- c(0, 0)  # Mean vector
sigma <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)  # Covariance matrix

# Generate a sample
set.seed(123)  # For reproducibility
sample <- mvrnorm(n = 1000, mu = mu, Sigma = sigma)

# Fit a 2D kernel density estimator using MASS::kde2d
kde_result <- kde2d(sample[,1], sample[,2], n = 100)

# Compute the product of univariate kernels
x <- kde_result$x
y <- kde_result$y
kde_product <- matrix(0, nrow = length(x), ncol = length(y))
h1 <- h2 <- 0.5  # bandwidths

for (i in 1:length(x)) {
    for (j in 1:length(y)) {
        kde_product[i, j] <- sum(dnorm(x[i], mean = sample[,1], sd = h1) *
                                 dnorm(y[j], mean = sample[,2], sd = h2)) / nrow(sample)
    }
}

# Plotting results

# Contour with sample points for MASS::kde2d
contour(kde_result, main = "MASS::kde2d Contour with Sample Points")
points(sample[,1], sample[,2], pch = 19, cex = 0.3)

# Contour with sample points for product KDE
contour(x, y, kde_product, main = "Product Kernel Contour with Sample Points")
points(sample[,1], sample[,2], pch = 19, cex = 0.3)

# Perspective plots for MASS::kde2d
persp(kde_result, theta = 30, phi = 30, main = "MASS::kde2d Perspective θ = 30")
persp(kde_result, theta = -60, phi = 30, main = "MASS::kde2d Perspective θ = -60")

# Perspective plots for manual product KDE
persp(x, y, kde_product, theta = 30, phi = 30, main = "Product Kernel Perspective θ = 30")
persp(x, y, kde_product, theta = -60, phi = 30, main = "Product Kernel Perspective θ = -60")
