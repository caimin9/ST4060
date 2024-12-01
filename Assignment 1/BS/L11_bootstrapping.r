# ---------------------------------------------------------
# ST4060/ST6040 2023-24
# Eric Wolsztynski, UCC
# Bootstrapping
# ---------------------------------------------------------

########################################################################
#### Example 1: Bootstrapping example: sample mean

n = 25

# sample observed form the field:
set.seed(1)
x = rnorm(n) # from N(0,1)
hist(x,col=8) # does not look Normal!

# 95% CI in theory:
(thCI = 0+c(-1,1)*1.96*1/sqrt(n))
abline(v=thCI, lwd=2)

# 95% CI under Normal approx:
(NCI = mean(x)+c(-1,1)*1.96*sd(x)/sqrt(n))
abline(v=NCI,col=2, lwd=2)

# Bootstrap CI from our sample:
B = 1000
bmeans = numeric(B)
for(b in 1:B){
	# resample 
	xb = sample(x,size=n,replace=TRUE)
	bmeans[b] = mean(xb)
}
mean(x)+c(-1,1)*1.96*sd(bmeans)
(BCI=quantile(bmeans,c(.025,.975)))
abline(v=BCI,col=4, lwd=2)

# including bias evaluation:
Bbias = mean(bmeans)-mean(x)
# (BBCI = mean(x)-Bbias+c(-1,1)*1.96*sd(bmeans))
# (BBCI = mean(x)-Bbias+c(-1,1)*qt(.975,(n-1))*sd(bmeans))
# (BBCI = mean(x)+c(-1,1)*qt(.975,(n-1))*sd(bmeans))
abline(v=BBCI, col=3, lty=2, lwd=2)

legend("topright",
	legend=c("Theoretic","Normal","Quantile","Boot-t"),
	col=c(1,2,4,3),lty=c(1,1,1,2),lwd=2)

# Now re-run whole code for a different random seed...


########################################################################
#### Example 2: using boot

library(boot)
my.mean <- function(x,ib){
	return(mean(x[ib]))
}
set.seed(1)
ob = boot(x,my.mean,R=B)
ob
# compare with:
Bbias; sd(bmeans); 

# CIs:
(obci = boot.ci(ob))
# compare with Rizza's definitions:
2*mean(x) - quantile(bmeans,c(0.975,0.025))  	# basic Bootstrap CI
quantile(bmeans,c(0.025,0.975))  				# percentile Bootstrap CI

########################################################################
# ShinyTutorial on resampling - bootstrapping exercise 1

x0 = mean(cars$dist)
# bootstrap sample mean:
B = 1000
n = nrow(cars) # sample size
bmeans = numeric(B)
for(b in 1:B){
	x.star = sample(cars$dist, size=n, replace=TRUE)
	# store bootstrap estimates...
	bmeans[b] = mean(x.star)
}
mean(bmeans)
(bias = mean(bmeans)-x0)
quantile(bmeans,c(0.025,0.975))
2*x0-quantile(bmeans,c(0.975,0.025))
# compare to naive CI:
2*x0-quantile(bmeans,c(0.975,0.025))-bias

########################################################################
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



