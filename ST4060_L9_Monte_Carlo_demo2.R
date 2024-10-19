# Demo: Law of Large Numbers and Central Limit Theorem in action
# These are the principles that Monte Carlo analysis relies on

# CLT: as the number M of Monte Carlo simulations increases, 
# the estimator becomes approximately more normally distributed:
N = 100  # sample size
par(mfrow=c(2,2))
for(M in c(50,500,1000,10000)){ # number of MC loops
	xbar = numeric(M)
	for(i in 1:M){
		# x = rnorm(N, m=0, s=2)
		x = rlnorm(N,2)
		xbar[i] = mean(x)
	}
	hist(xbar, main=paste(M,"MC repetitions"))
}

# MC 95% confidence interval using Normal approx:
ci = c(mean(xbar) - 1.96*sd(xbar), mean(xbar) + 1.96*sd(xbar))
abline(v=ci, lwd=2)

# xbar ~ N(0,2/sqrt(100))
pnorm(-0.3902533,m=0,s=.2)
pnorm(0.3940031,m=0,s=.2)

# MC quantile 95% confidence interval:
ciq = quantile(xbar, c(0.025, 0.975))
abline(v=ciq, lwd=2, col=2)
