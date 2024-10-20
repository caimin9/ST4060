#Question 1
a) the std dev sigma_K of K(.) depends on the choice of a kernel function; it maybe convenient for K to have unit variance,
i.e. sigma_K = 1, as is the case with the Gaussian Kernel
b) K_h(.) has std dev(i.e. scale) hsigma_K. If K is the gaussian kernel, this std dev is h.
c) No, since it's not a pdf (does not integrate to 1, since we're missing 1/sqrt(2*pi)
d) Smaller bias is achieved with under smoothing: Use smaller bandwidth h1





# ------------------------------------------------------------------
# Question 2

set.seed(4060)
N = 100
dfx = 2
thbar = 8
x = runif(N,1,2)
M = 1000
lmos = lmeds = lmeans = numeric(M)
for(i in 1:M){
	z = rt(n=N, df=dfx)
	y = thbar*x + z
	lmo = lm(y~x+0)
	lmos[i] = as.numeric(coef(lmo))
	lmeds[i] = median(y/x)
	lmeans[i] = mean(y/x)
}
#
mean(lmos)
mean(lmeds)
mean(lmeans)
#
sd(lmos)
sd(lmeds)
sd(lmeans)
#
boxplot(lmos,lmeds,lmeans, names=c('OLS','median-based','mean-based'))
abline(h=thbar, col=3)

# ------------------------------------------------------------------
# Question 3

dat = data.frame(wt=mtcars$wt, mpg=mtcars$mpg)
B = 100
set.seed(6040)
int = pval = eff = numeric(B)
for(b in 1:B){
	ib = sample(1:nrow(dat), nrow(dat), replace=TRUE) #this is just giving us indices
	xb = dat[ib,] #here we take all the rows corresponding to those indices
	lmb = lm(mpg~wt, data=xb) # then just make a model on those data points
	int[b] = summary(lmb)$coef[1,1] #intercept
	eff[b] = summary(lmb)$coef[2,1] #slope
	pval[b] = summary(lmb)$coef[2,4] # p values
}
# (a)
mean(eff)

# (b) #naive confidence interval
#Use quantiles for C.I to avoid assuming a theoretical distn of the data
#For naive C.I, it treats the empirical distribution of the bootstrap samples as if it perfectly reflects the true distribution of the estimator.
#Assumes Symmetry and No Bias Correction: 
	#The naive approach doesn’t necessarily take into account the potential bias of the estimator or whether the bootstrapped distribution is symmetric.
	#In cases where there is bias or significant skewness, the naive method can yield misleading intervals.
quantile(pval,c(.025,.975))

# (c) #Appropriate confidence interval
# The appropriate confidence interval uses a bias correction approach. 
#It effectively centers the confidence interval by adjusting for any asymmetry or bias in the bootstrap distribution, 
# which makes it more reliable when dealing with potentially biased estimators or skewed data. 
#This correction provides a more robust estimate compared to the naive method.
lm0 = lm(mpg~wt, data=dat)
eff0 = summary(lm0)$coefficients[2,1]
2*eff0-rev(quantile(eff,c(.025,.975)))

# (d)
#xbar +- 1.96*std error
#The bootstrap confidence interval does not assume normality and may be more robust in cases where the data deviates from normality.
#However, in this case, if the intervals are similar, it suggests that the normality assumption might be reasonable for this dataset
se0 = summary(lm0)$coefficients[2,2]
eff0 + c(-1,1)*1.96*se0

# (e)
# The parametric CI is quite different. 
par(mfrow=c(1,2))
hist(lm0$residuals,xlab="Residuals",main="Model fit residuals")
hist(eff,xlab="Slope estimates",main="Bootstrap slope estimates")
