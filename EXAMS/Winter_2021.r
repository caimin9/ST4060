rm(list=ls())

# --------------------------------------------------------
# Question 2

N = 1000
dfx = 3
z = rt(n=N, df=dfx)
mean(z)

set.seed(4060)
N = 100
dfx = 3
thbar = 8
x = runif(N,1,2)
M = 1000
lmos.n = lmos = lmosi = lmosii = my = mz = numeric(M)
for(i in 1:M){
	z = rt(n=N, df=dfx)
	y = thbar*x + z
	my[i] = mean(y)
	mz[i] = mean(z)
	lmo = lm(y~x+0)
	lmos[i] = as.numeric(coef(lmo))

	lmo = lm(y~x)
	lmosi[i] = as.numeric(coef(lmo)[2])
	lmosii[i] = as.numeric(coef(lmo)[1])

	y = thbar*x + rnorm(N)
	lmo = lm(y~x+0)
	lmos.n[i] = as.numeric(coef(lmo))
}
#
c(mean(lmos), sd(lmos))
c(mean(lmosi), sd(lmosi))
boxplot(lmos,lmosi)
boxplot(lmosii)
c(mean(lmos.n), sd(lmos.n))
hist(lmos.n)

M = 10000
lmos.mn = lmos.m = numeric(M)
for(i in 1:M){
	z = rt(n=N, df=dfx)
	y = thbar*x + z
	my[i] = mean(y)
	mz[i] = mean(z)
	lmo = lm(y~x+0)
	lmos.m[i] = as.numeric(coef(lmo))

	y = thbar*x + rnorm(N)
	lmo = lm(y~x+0)
	lmos.mn[i] = as.numeric(coef(lmo))
}
#
hist(lmos.n, ylim=c(0,10), freq=FALSE)
hist(lmos.m, add=TRUE, col='navy', freq=FALSE)
hist(lmos.n, freq=FALSE, add=TRUE, col=0, border=8)
hist(lmos.m, add=TRUE, col=0, border='navy', freq=FALSE)
c(mean(lmos), sd(lmos))
c(mean(lmos.n), sd(lmos.n))
c(mean(lmos.m), sd(lmos.m))
c(mean(lmos.mn), sd(lmos.mn))
boxplot(lmos,lmos.n,lmos.m,lmos.mn)
diff(quantile(lmos,c(.025,.975)))
diff(quantile(lmos.m,c(.025,.975)))

shapiro.test(lmos.mn)

# --------------------------------------------------------
# Question 3

library(ISLR)
dat = na.omit(Hitters)
x = dat$Years
y = dat$Salary

# (a)
lo = lm(y~x+I(x^2))
summary(lo) 
nlo2 = nls(y~a+b*x+c*I(x^2), start=list(a=10,b=1,c=1))
summary(nlo2) 
# (b)
y.hat = predict(lo)
(rmse.lo = sqrt( mean((y.hat-y)^2) ))
y.hat = predict(nlo2)
(rmse.nlo = sqrt( mean((y.hat-y)^2) ))
# (c)
so = smooth.spline(x,y)
y.so = approx(so$x,so$y,xout=x)$y
(rmse.so = sqrt( mean((y.so-y)^2) ))
# (d)
so2 = smooth.spline(x,y,df=so$df*4)
y.so2 = approx(so2$x,so2$y,xout=x)$y
(rmse.so2 = sqrt( mean((y.so2-y)^2) ))
# (e)
(rmse.so-rmse.nlo)/rmse.nlo
(rmse.so2-rmse.nlo)/rmse.nlo
(rmse.so2-rmse.so)/rmse.so
# Bonus
plot(x,y)
is = order(x)
lines(x[is],fitted(nlo2)[is],lwd=2,col=2)
lines(so,col=4,lwd=2)
lines(so2,col='navy',lwd=2)

# --------------------------------------------------------
# Question 4

library(ISLR)
dat = na.omit(Hitters)
itrain = c(1:200)
dat.train = dat[itrain,]
dat.test = dat[-itrain,]
Salary.test = dat.test$Salary
dat.test$Salary = NULL
B = 100
set.seed(1)
int = pval = eff = numeric(B)
for(b in 1:B){
	ib = sample(1:nrow(dat.train), nrow(dat.train), replace=TRUE)
	xb = dat.train[ib,]
	lmb = lm(Salary~HmRun, data=xb)
	int[b] = summary(lmb)$coef[1,1]
	eff[b] = summary(lmb)$coef[2,1]
	pval[b] = summary(lmb)$coef[2,4]
}
# (a)
mean(eff)
# (c)
quantile(pval,.99)
# (e)
preds = mean(int) + mean(eff)*dat.test$HmRun
sqrt( mean((preds-Salary.test)^2) )
# (f)
mean(Salary.test)
sd(Salary.test)
# (g)
lmo = lm(Salary~HmRun, data=dat.train)
preds.lmo = predict(lmo,dat.test)
sqrt( mean((preds.lmo-Salary.test)^2) )
