setwd("/Users/ewol/Downloads/")

# -------------------------------------------------------
# Question 2: k-fold CV v LOO-CV

library(ISLR)
dat = na.omit(Hitters)
x = dat[,c("Years","Hits","Walks")]
y = log(dat$Salary)
n = nrow(x)

set.seed(4060)
K = 5
folds = cut(1:n,K,labels=FALSE)
err = numeric(K)
for(k in 1:K){
	i.train = which(folds!=k)
	lm.fit = lm(y[i.train]~., data=x[i.train,])
	lm.pred = predict(lm.fit, newdata=x[-i.train,])
	err[k] = mean((lm.pred-y[-i.train])^2)
}

set.seed(4060)
K = n
folds = cut(1:n,K,labels=FALSE)
err.loo = numeric(K)
for(k in 1:K){
	i.train = which(folds!=k)
	lm.fit = lm(y[i.train]~., data=x[i.train,])
	lm.pred = predict(lm.fit, newdata=x[-i.train,])
	err.loo[k] = mean((lm.pred-y[-i.train])^2)
}

mean(err)
mean(err.loo)
sd(err)
sd(err.loo)
boxplot(err,err.loo,names=c("5-fold CV","LOO-CV"))

# -------------------------------------------------------
# Question 3

# data simulation
if(0){
	set.seed(6040)
	n = 125
	x = runif(n, 1, 10)
	z = rnorm(n, m=0, sd=1.5)
	true = sin(.5+4*x) + .15*x^2 
	y = true + z
	plot(x, y, pch=20)
	points(x, true, pch=15, col=2)
	write.csv(data.frame(x=x,y=y), row.names=FALSE, 
		file="data/nonlinear_dataset.csv")
} else {
	dat = read.csv(file="nonlinear_dataset.csv")
	x = dat$x
	y = dat$y
}

# (1) Fits
nlso1 = nls(y~a*I(x^2)+sin(b+c*x), 
				start=list(a=.05,b=.4,c=2))
nlso2 = nls(y~a*I(x^2)+b*x+c, 
				start=list(a=.05,b=.4,c=2))
y.hat1 = fitted(nlso1)
y.hat2 = fitted(nlso2)
plot(x, y, pch=20)
points(x, y.hat1, pch=8, col=4)
points(x, y.hat2, pch=15, col='tomato')
legend('topleft', bty='n', 
	col=c('black','blue','tomato'),
	pch=c(20,8,15),
	legend=c('data','sinusoidal fit (1)','quadratic fit (2)'))

# RMSEs:
coef(nlso1)
coef(nlso2)
rmse1 = sqrt( mean((y.hat1-y)^2) )
rmse2 = sqrt( mean((y.hat2-y)^2) )

# (2) bootstrap
B = 100
coefs.b = matrix(NA, nrow=B, ncol=3)
set.seed(4060)
for(b in 1:B){
	ib = sample(1:n, n, replace=TRUE)
	xb = x[ib]
	yb = y[ib]
	nlsob = nls(yb~a*I(xb^2)+b*xb+c, 
				start=list(a=.05,b=.4,c=2))
	coefs.b[b,] = coef(summary(nlsob))[,1]
}

# coef(summary(nlsob))[,1]
# ANSWER:
apply(coefs.b,2,mean)
apply(coefs.b,2,sd)
# bootstrap estimates + SE:
cbind(apply(coefs.b,2,mean),apply(coefs.b,2,sd))

# bootstrap CI:
est0 = coef(nlso2)
qls = apply(coefs.b,2,quantile,.025)
qus = apply(coefs.b,2,quantile,.975)
est0
cbind(2*est0 - qus, 2*est0 - qls)


# -------------------------------------------------------
# Question 4

dat = read.csv("insdata.csv")
age = dat$Age
mF = dat$mF

# (a) Compute a first P-spline where the smoothing control parameter is set to .5.  
# Construct a plot of the dataset (black dots) along with the P-spline (red solid curve).

ssp1 = smooth.spline(age,mF,spar=.5)

plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
	xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(ssp1,col=2,t='l',pch='x',lwd=2)

# (b) Compute a second P-spline for the same dataset, where the smoothing control parameter is half that of the P-spline obtained in (a). Provide the R command you used.

ssp2 = smooth.spline(age,mF,spar=.5*ssp1$spar)
points(ssp2,col=4,t='l',pch='x',lwd=2)

# (c) Show that the two P-spline outputs are evaluated over the same points on the x-axis. 

ssp1$x-ssp2$x

# (d)  Compare the MSEs for the P-splines obtained in (a) and (b). Comment on their difference, and propose a reason as to why they differ. 

mean((ssp1$y-mF)^2)
mean((ssp2$y-mF)^2)

# ssp2 has a smaller MSE, which is to be expected as the bias has been improved by halfing the smoothing parameter (the P-spline remains relatively smooth, hence the variance is kept low).

# (e) Compute a B-spline basis using the first, second and third quartiles of the age data as knots. Provide a plot of this B-spline basis.

require(splines)
K = quantile(age,c(.25,.5,.75))
BM = bs(age, knots=K)
matplot(age,BM,xlab="Age",ylab="Spline basis",t='b')

# (f) Quote the coordinates of age 60 on the B-spline basis copmuted in (e), up to four decomal places.

round(BM[which(age==60),],4)
# 0.1713 0.5619 0.2659 0.0000 0.0000 0.0000

# (g) Compute the corresponding B-spline for the (age, mF) data. Provide the R command you used, as well as the output coefficients for the B-spline expression.

bsp = lm(mF~BM)
# (Intercept)          BM1          BM2          BM3          BM4          BM5          BM6  
   # 0.002029     0.007088    -0.008402     0.020189     0.019570     0.145510     0.133805  

# (h) Compare the MSE obtained for that B-spline with the MSE's obtained from the two P-splines obtained in (d).

mean((fitted(bsp)-mF)^2)
plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
	xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(ssp1,col=2,t='l',pch='x',lwd=2)
points(age,fitted(bsp),col=3,t='l',pch='x',lwd=2)
points(ssp2,col=4,t='l',pch='x',lwd=2)

# This B-spline seems comparable to the 1st P-spline, and not as effective as the second one.

# (i) Compute interpolations for all ages within the range of age data, using respectively 
# P-spline and local polynomial regression. Plot the interpolated points over the 
# observations, using red for P-spline values and blue for local polynomial regression values.
# Quote the standard deviations of each of the interpolated samples.

all.ages = seq(min(age),max(age),by=1)

# - P-spline
ssp = ssp1
spred = approx(ssp,xout=all.ages)
sum((spred$y)^2)
sd(spred$y)

# - lowess
lo = loess(mF~age)
lpred = predict(lo,newdata=data.frame(age=all.ages))
sd(lpred)

plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
	xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(all.ages,spred$y,col=2,t='b',pch='x',lwd=2)
points(all.ages,lpred,col=4,t='b',pch=20,lwd=2)
points(age,mF,pch=20)
