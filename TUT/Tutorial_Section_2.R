##
## ST4060 / ST6040
## Tutorial exercises - Section 2
##

# ------------------------------------------------------------
# Question 2.1

M <- 10000
x <- runif(M, min=2, max=4)
theta.hat <- mean( exp(-x) ) * (4-2)
print(theta.hat)
print(exp(-2) - exp(-4))

# ------------------------------------------------------------
# Question 2.2

M <- 10000
G <- 10
x <- seq(0.1, 2.5, length=G)
cdf <- numeric(G)
vmc <- numeric(G) # variance of MC estimator
ciU <- numeric(G) # Upper bound of CI for MC estimator
ciL <- numeric(G) # Lower bound of CI for MC estimator

for(i in 1:G){
	u <- runif(M, min=0, max=x[i])
	g <- exp(-u^2/2)
	# now compute the adjusted sample average: gbar * (x-0)
	# (remember to add the negative part of the integral!)
	cdf[i] <- x[i] * mean(g) / sqrt(2*pi) + 0.5 
	# compute the associated variance
	vmc[i] <- mean( (g-mean(g))^2 ) / M
	# compute the 95% CI
	ciL[i] <- cdf[i] - 1.96*sqrt(vmc[i])
	ciU[i] <- cdf[i] + 1.96*sqrt(vmc[i])	
}

# Check with theoretic values:
Phi <- pnorm(x)
print( round(rbind(x, cdf, Phi), 3) )

# Display evaluated CI:
print( cbind(round(x,2), round(cbind(ciL, cdf, ciU),4)) )
# Note that something should be done to truncate the CI within (0,1)!

# ------------------------------------------------------------
# Question 2.3

N = 100  # sample size
k = 5
M = 1000 # number of MC loops

# initialise the storing vectors
xbar = tmean = numeric(M)

# MC loop:
for(i in 1:M){
	x = rnorm(N, df=10)
	xbar[i] = mean(x)
	xs = sort(x)
	tmean[i] = mean( xs[(k+1):(N-k)] )
}
rr = range(c(xbar,tmean))
par(mfrow=c(2,1))
hist(xbar, xlim=rr)
hist(tmean, xlim=rr)
# plot(xbar,tmean)

# Biases:
# mean(xbar-15)
# mean(tmean-15)
# Variances:
sd(xbar)
sd(tmean)
# MSEs:
# mean((xbar-15)^2)
# mean((tmean-15)^2)

# ------------------------------------------------------------
# Question 2.4

# Here the objective is to set up M repetitions of an experiment (Monte Carlo 
# repetitions), where for each experiment:
#    - we generate random values, 
#    - do some statistical stuff with it(*), 
#    - store the results.
# Finally, we analyse these results once the M experiments are finished. 
# We'll eventually generate some plots and store some information...

# (*) Here the 'statistical stuff' could be comparing two robust estimators 
#     for linear regression with heavy-tailed noise (e.g. log-Normal, Laplace)...
#     We could e.g. decide to use least squares (LS) and robust M-estimation (RM).

#-------------------------------------------------------------------------

# 0) Load our own work! We're also going to use mylm (script #3)

getwd()
setwd("/Volumes/macdata/teaching/ST3060/practicals")
source("ST3060_toolbox.R") # import all functions defined in the file

# 1) Initialisation of vectors and parameters

N = 50                          # Sample size
M = 100                         # Number of Monte-Carlo repetitions
a = 7                           # Intercept parameter (to be estimated)
b = 3                           # Slope parameter (to be estimated)
x = rep(c(1:5),N/5)             # Vector of regressors (design)
m = 0.5                         # Location for noise distribution
s = 1.2                         # Scale for noise distribution
rseed = 0                       # Random seed
LSvec = RMvec = matrix(0,2,M)   # Storage
MYvec = LSvec                   # More storage
ev = yv = matrix(0,N,M)         # Even more storage
 
set.seed(rseed)   # Sets random seed for reproducibility
library(MASS)     # We need rlm() from this library
library(VGAM)     # We need rlaplace() from this library

# 2) Monte-Carlo experiments

for (mc in 1:M){
	# generate data
	#e = rlaplace(N,location=m,scale=s)
	e = rlnorm(N,meanlog=m,sdlog=s)	
	y = a + b*x + e

	# estimate (a,b) via Least Squares estimation
	LS = lm(y~x)$coef

	# estimate (a,b) via Least Squares estimation, using our own mylm()
	MY = c(mylm(x,y)$myintercept,mylm(x,y)$mycoef)

	# estimate (a,b) via Robust M-estimation
	RM = rlm(y~x,method="MM")$coef

	# store generated data
	yv[,mc] = y
	ev[,mc] = e

	# store estimates for this Monte-Carlo experiment
	LSvec[,mc] = rbind(LS[1],LS[2])
	MYvec[,mc] = rbind(MY[1],MY[2])
	RMvec[,mc] = rbind(RM[1],RM[2])	
}

# 3) Analyse the two sets of estimates

# distributions
par(mfrow=c(2,3))
#
hist(LSvec[1,],main="Estimates of a by LS"); 
hist(RMvec[1,],main="Estimates of a by RM") 
plot(density(LSvec[1,]),col='red'); points(density(RMvec[1,]),t='l',col='blue')
#
hist(LSvec[2,],main="Estimates of b by LS"); 
hist(RMvec[2,],main="Estimates of b by RM") 
plot(density(LSvec[2,]),col='red'); points(density(RMvec[2,]),t='l',col='blue')

# bias: intercept 
mean(LSvec[1,])-a; mean(MYvec[1,])-a; mean(RMvec[1,])-a
# bias: slope
mean(LSvec[2,])-b; mean(MYvec[2,])-b; mean(RMvec[2,])-b
# variance: intercept
var(LSvec[1,]); var(MYvec[1,]); var(RMvec[1,])
# variance: slope
var(LSvec[2,]); var(MYvec[2,]); var(RMvec[2,])
# MSE: intercept (MSE = Bias(estimates)^2 + Var(estimates))
mean((LSvec[1,]-a)^2); mean((MYvec[1,]-a)^2); mean((RMvec[1,]-a)^2)
# MSE: slope
mean((LSvec[2,]-b)^2); mean((MYvec[2,]-b)^2); mean((RMvec[2,]-b)^2)

# 4) Write outputs to file

allres = cbind(x,t(MYvec),t(LSvec),t(RMvec))
results = data.frame(allres)
names(results) = c("x","a_MY","b_MY","a_LS","b_LS","a_RM","b_RM")
head(results)
write.csv(results,"montecarlo.csv")

# 5) Test this file

mydata = read.csv("montecarlo.csv")
head(mydata)
# We should get the same stats as before again! 
# Check bias of intercept estimates 
mean(mydata$a_LS)-a; mean(mydata$a_MY)-a; mean(mydata$a_RM)-a
mean(LSvec[1,])-a; mean(MYvec[1,])-a; mean(RMvec[1,])-a


# ------------------------------------------------------------
# Question 2.5
# NOTE: we still need to adapt this code to run the analysis 
# for varying sample sizes :)

M = 1000 # number of MC loops
N = 50  # sample size
Ns = c(10, 20, 50, 100) # sample sizes
L = length(Ns)

# initialise the storing vectors
sd1 = sd2 = matrix(NA,nrow=M,ncol=L)
colnames(sd1) = colnames(sd2) = Ns

# MC loop:
for(j in 1:L){
	N = Ns[j]
	for(i in 1:M){
		x = rnorm(N, mean=0, sd=2)
		sd1[i,j] = sd(x)
		# sd1[i,j] = sqrt(sum((x-mean(x))^2)/(N-1))
		sd2[i,j] = sqrt(sum((x-mean(x))^2)/N)
	}
}

# Evaluate biases:
# (MC can be used to approximate the bias numerically)
true = 2 # true value of sd
(mean(sd1)-true)
(mean(sd2)-true)
# %-errors give a better idea:
(mean(sd1)-true)/true
(mean(sd2)-true)/true

# Evaluate accuracies:
round(sd(sd1),3)
round(sd(sd2),3)
# they seem comparable; the real difference is in terms of bias

par(mfrow=c(2,2))
hist(sd1[,1],main="sd1")
hist(sd2[,1],main="sd2")
boxplot(sd1,main="sd1"); abline(h=true)
boxplot(sd2,main="sd2"); abline(h=true)

# ------------------------------------------------------------
# Question 2.6

rm(list=ls()) # clear the "cache" (R session environment)

M = 100
N = 100
dfs = c(2,4,10)

means = matrix(NA, nrow=M, ncol=length(dfs))

for(j in 1:length(dfs)){
	df = dfs[j]
	# MC loop:
	for(i in 1:M){
		x = rchisq(N, df=df)
		means[i,j] = mean(x)
	}	
}
boxplot(means, names=dfs, xlab="Numbers of degrees of freedom")
abline(h=dfs, lwd=4, col='pink')
hist(means[,3])

apply(means,2,sd)

# ------------------------------------------------------------
# Question 2.7

rhuber <- function(N,epsilon=0,dof=3){
# Generates N pseudo-random realizations of Huber's contamination model
# 			f(u) = epsilon g(u) + (1-epsilon) h(u)
# using the Standard Normal distribution for g(.) and the t-distribution 
# with 3 degrees of freedom for h().
# Here epsilon is expected to be within [0.5, 1].
	# to randomly sample from either distribution:
	if(epsilon<0 | epsilon>1){
		stop("epsilon must be a value within 0 and 1.")
	}
	draws = runif(N,0,1)
	# initialise output vector of realisations:
	if(epsilon<1){
		x = numeric(N)
		i.h = which(draws<(1-epsilon))
		x[i.h] = rt(length(i.h),dof)
		x[-i.h] = rnorm((N-length(i.h)))
	} else {
		x = rnorm(N)
	}
	return( x )
}

set.seed(1)
N = 100
f1 = rhuber(N,.95)
f2 = rhuber(N,.80)
f3 = rhuber(N,.60)
round(c(mean(f1), sd(f1)),3)
round(c(mean(f2), sd(f2)),3)
round(c(mean(f3), sd(f3)),3)

par(mfrow=c(3,1))
xlims = range(c(f1,f2,f3))
hist(f1, col='navy', xlim=xlims, breaks=20)
hist(f2, col='navy', xlim=xlims, breaks=20)
hist(f3, col='navy', xlim=xlims, breaks=20)

df = data.frame(cbind(f1,f2,f3))
names(df) = c("e095","e080","e060")
nrow(df); head(df)
write.csv(df, "output_of_Q1.csv", row.names=FALSE)

M=500
N=100
mean.f = mean.g = sd.f = sd.g = numeric(M)
for(mc in 1:M){
	f1 = rhuber(N,.60)
	g1 = rnorm(N)
	mean.f[mc] = mean(f1)
	mean.g[mc] = mean(g1)
	sd.f[mc] = sd(f1)
	sd.g[mc] = sd(g1)
}
round(c(mean(mean.f), mean(sd.f)),3)
round(c(mean(mean.g), mean(sd.g)),3)


#-------------------------------------------------------------------------------
# Question 2.9

# original sample and analysis:
plot(cars)
olm = lm(cars$dist~cars$speed)
abline(olm)
olm
summary(olm)

# bootstrap linear regression:
B = 1000
n = nrow(cars) # sample size
bcoefs = matrix(NA, nr=B, nc=2)
for(b in 1:B){
	ib = sample(c(1:n),size=n,replace=TRUE)
	bcars = cars[ib,]
	blm = lm(bcars$dist~bcars$speed)
	# store bootstrap estimates...
	bcoefs[b,] = as.numeric(blm$coef)
}
# Bootstrap estimation of standard errors
# for intercept and slope:
sd(bcoefs[,1])
sd(bcoefs[,2])
par(mfrow=c(2,2))
boxplot(bcoefs[,1],main='intercept')
abline(h=olm$coef[1])
boxplot(bcoefs[,2],main='slope')
abline(h=olm$coef[2])
hist(bcoefs[,1],main='intercept')
abline(v=olm$coef[1])
hist(bcoefs[,2],main='slope')
abline(v=olm$coef[2])

olm$coef[2]
mean(bcoefs[,2])


#Question 2.10
# Set the seed for reproducibility
set.seed(1)

x = mtcars$disp
y = mtcars$mpg
# Assuming x and y vectors are defined and correspond to the data points
n <- length(y) 

# Fit the exponential model using non-linear least squares
fit <- nls(y ~ exp(theta1 + theta2 * x), start = list(theta1 = 3, theta2 = -0.01))

# Extract and print coefficient estimates
coef_estimates <- summary(fit)$coefficients
print(coef_estimates)

# Plot the data and the model fit
plot(x, y, pch = 19, col = "black", main = "Data and Model Fit", xlab = "X", ylab = "Y")
lines(x, predict(fit, list(x = x)), col = "red")

# Bootstrap function (manual implementation)
bootstrap_results <- matrix(nrow = 100, ncol = 2)  # 100 resamples, storing results for theta1 and theta2

for (i in 1:100) {
  indices <- sample(1:n, replace = TRUE)  # Resampling indices
  sample_data <- data.frame(x = x[indices], y = y[indices])
  model <- nls(y ~ exp(theta1 + theta2 * x), data = sample_data, start = list(theta1 = 3, theta2 = -0.01))
  bootstrap_results[i, ] <- coef(model)
}

# Calculate means and standard deviations for theta1 and theta2 from bootstrap
bootstrap_means <- apply(bootstrap_results, 2, mean)
bootstrap_sds <- apply(bootstrap_results, 2, sd)
print(paste("Bootstrap Means: theta1 =", bootstrap_means[1], ", theta2 =", bootstrap_means[2]))
print(paste("Bootstrap Standard Deviations: theta1 =", bootstrap_sds[1], ", theta2 =", bootstrap_sds[2]))

# Calculate standard error for theta1
theta1_se <- sd(bootstrap_results[, 1])
print(paste("Standard Error for theta1:", theta1_se))

# Calculate nonparametric 95% confidence interval for theta1
theta1_ci_low <- quantile(bootstrap_results[, 1], probs = 0.025)
theta1_ci_high <- quantile(bootstrap_results[, 1], probs = 0.975)
print(paste("95% Confidence Interval for theta1: [", theta1_ci_low, ", ", theta1_ci_high, "]", sep=""))







