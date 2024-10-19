# Set up the Monte Carlo simulation:
N = 100  # sample size
M = 1000 # number of Monte Carlo repetitions
beta.true = 10
x = seq(1,10,length=N) 
slopes = numeric(M)
for(j in 1:M){
	# generate a new dataset
	noise = rnorm(N,sd=10)
	y = beta.true * x + noise
	# fit a regression model to it
	lmo = lm(y~x+0)
	# store the estimated slope coefficient
	slopes[j] = coef(lmo)[1]
}

# visualise the last of the M datasets created:
plot(x,y,cex=10,pch=20)
abline(lmo,col=2,lwd=15)

# Sample of Monte Carlo estimates of the slope:
slopes
mean(slopes)
sd(slopes)
summary(lmo)
