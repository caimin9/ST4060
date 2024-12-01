##
## ST4060 / ST6040
## Monte Carlo demo script
## Eric Wolsztynski
##

# Example: simple linear regression 
# with non-normal noise (ie with outliers)

N = 100
theta.star = 3
X = runif(n=N,min=1,max=5)

# one experiment:
noise = rnorm(n=100,sd=2)
# noise = rt(n=100,df=1)
Y = theta.star*X + noise
plot(X,Y,pch=20,cex=2)
o = lm(Y~X)
abline(o,col=2,lwd=5)

# repeat this experiment M times:
M = 100
for(m in 1:M){
	noise = rnorm(n=100,sd=2)
	# noise = rt(n=100,df=1)
	Y = theta.star*X + noise
	o = lm(Y~X)
	abline(o,col=m,lwd=5)
}

M = 1000 # nbr of MC repetitions
# we can evaluate the statistics for the estimator:
theta.hat = numeric(M)*NA
for(m in 1:M){
	noise = rnorm(n=100,sd=2)
	# noise = rt(n=100,df=1)
	Y = theta.star*X + noise
	o = lm(Y~X+0)
	theta.hat[m] = o$coef
}
theta.hat
mean(theta.hat)
# MC estimate of the bias of the LSE:
mean(theta.hat)-theta.star 
# MC estimate of the standard error of the LSE
var(theta.hat)
hist(theta.hat,breaks=7)

# compare with std error from the last fit?
summary(o)$coef[2]
sqrt(var(theta.hat))

# Example: distribution of the sample mean 
# We know that due to the Central Limit Theorem,
# Y_bar ~ N(mu, sigma^2/N)
# where mu is the true population mean of a 
# sample of i.i.d. observations Y_1, ..., Y_N, 
# sigma is the population standard deviation, 
# and N is the sample size.

M = 1000
ybar = numeric(M)*NA
for(m in 1:M){
	Y = rnorm(n=100,m=10,sd=2) 
	ybar[m] = mean(Y)
}
hist(ybar)
mean(ybar) # should be close to mu=10
sd(ybar)
2/sqrt(100) # theoretic std deviation of Y_bar










