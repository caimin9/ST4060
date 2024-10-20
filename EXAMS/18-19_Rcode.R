#! /usr/bin/Rscript --vanilla
while( dev.next()>1 ){ dev.off() }
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)

setwd("/Volumes/macdata/teaching/st4060/exam/2018-19")

# --------------------------------------------------------------------------------
# Question 2

dat = read.csv(file='FranceRates2004.csv')
dat.test = read.csv(file='FranceRates2004_test.csv')
plot(dat$age, dat$D, pch=20, t='b')
x = dat$age
y = dat$D
points(dat.test$age, dat.test$D, pch=20, col=8)
# (a)
sp1 = smooth.spline(x, y, df=15)
plot(x, y, pch=20, t='b', xlim=c(0,110))
lines(sp1, lwd=2, col=2)
# (b)
sp2 = smooth.spline(x, y, spar=.05)
lines(sp2, lwd=2, col=4)
# (c)
names(sp2)
sqrt(mean((sp1$y-y)^2))
sqrt(mean((sp2$y-y)^2))
# (d)
xt = dat.test$age
yt = dat.test$D
p1 = predict(sp1, x=xt)
p2 = predict(sp2, x=xt)
points(xt,p1$y,pch=15,col=2)
points(xt,p2$y,pch=15,col=4)
points(xt,yt,pch='x',col=8)
# points(xt,yt,pch=20,col=8)
sqrt(mean((yt-p1$y)^2))
sqrt(mean((yt-p2$y)^2))
# (e)
L = 50
scrit = sval = seq(.01,.40,length=L)
for(i in 1:L){
	spi = smooth.spline(x, y, spar=sval[i])
	names(spi)
	scrit[i] = spi$cv.crit
}
plot(sval,scrit)
abline(v=sval[which.min(scrit)])

# --------------------------------------------------------------------------------
# Question 3

set.seed(4060)
N = 50
dfx = 3
thbar = 4
x = runif(N,1,10)
M = 1000
lmos = my = mz = numeric(M)
for(i in 1:M){
	z = rchisq(n=N, df=dfx)
	y = thbar*x + z
	my[i] = mean(y)
	mz[i] = mean(z)
	lmo = lm(y~x+0)
	lmos[i] = as.numeric(coef(lmo))
}
hist(z, col=8)
plot(x, y, pch=20)
points(x, thbar*x, pch=15, col=2)
#
mean(lmos)
sd(lmos)
#
mean(x)
mean(my)
mean(mz)

# --------------------------------------------------------------------------------
# Question 4

plot(cars, pch=20)
abline(lm(dist~speed, cars), lwd=2, col=2)
x = cars$speed
y = cars$dist
N = nrow(cars)
K = 10
folds = cut(1:N,K,labels=FALSE)
p1 = se = sb = numeric(K)
set.seed(1)
for(i in 1:K){
	# CV
	itrain = which(folds!=i)
	lmo = lm(y[itrain]~x[itrain])
	se[i] = summary(lmo)$coef[2,2]
}
set.seed(1)
for(i in 1:K){
	# bootstrapping
	ib = sample(1:N,N,replace=TRUE)
	lmb = lm(y[ib]~x[ib])
	sb[i] = summary(lmb)$coef[2,2]
}
mean(se)
mean(sb)
(mean(se)-mean(sb))/mean(sb) # %-difference
boxplot(cbind(se,sb))
t.test(se,sb)
