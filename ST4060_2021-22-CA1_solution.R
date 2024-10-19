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
	ib = sample(1:nrow(dat), nrow(dat), replace=TRUE)
	xb = dat[ib,]
	lmb = lm(mpg~wt, data=xb)
	int[b] = summary(lmb)$coef[1,1]
	eff[b] = summary(lmb)$coef[2,1]
	pval[b] = summary(lmb)$coef[2,4]
}
# (a)
mean(eff)
# (b)
quantile(pval,c(.025,.975))
# (c)
lm0 = lm(mpg~wt, data=dat)
eff0 = summary(lm0)$coefficients[2,1]
2*eff0-rev(quantile(eff,c(.025,.975)))
# (d)
se0 = summary(lm0)$coefficients[2,2]
eff0 + c(-1,1)*1.96*se0
# (e)
# The parametric CI is quite different. 
par(mfrow=c(1,2))
hist(lm0$residuals,xlab="Residuals",main="Model fit residuals")
hist(eff,xlab="Slope estimates",main="Bootstrap slope estimates")