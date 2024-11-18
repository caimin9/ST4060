# ---------------------------------------------------------
# ST4060/ST6040 
# Eric Wolsztynski, UCC
# Tutorial questions on splines
# ---------------------------------------------------------

rm(list=ls())
library(splines)

# ------------------------------------------------------------
# QUESTION 4.1
LT = read.table("irl_lifetable_2005.txt", sep=",", header=TRUE)
head(LT)
# keep only the first 106 rows from LT:
SLT = LT[c(1:106),] 
mx = SLT[,8]
x = SLT$Age # age grid
plot(x, log(mx), t='l', lwd=2)
# roughly fit a Makeham model to this data:
onls = nls(mx~A+B*c^x,start=list(A=.0003, B=.00002, c=1.108))
ABc = summary(onls)$coef[,1]
# now add noise to the fitted f.o.m. curve:
set.seed(1)
x = seq(0, 110, by=1)
mx = ABc[1]+ABc[2]*ABc[3]^x
mxn = mx
s1 = which(x<86)
s2 = which(x>85)
mxn[s1] = pmax(0.005,mx[s1]+rnorm(length(s1),0,.03))
mxn[s2] = mx[s2]+rnorm(length(s2),0,.06)
dat = data.frame(x,mx,mxn)

x = dat$x
mxn = dat$mxn
plot(x,mx,pch=21,col=8)
points(x,mxn,pch=20,cex=.8)

# (a) fit NLS:
nls.ns = nls(mxn~A+B*c^x,start=list(A=.0003, B=.00002, c=1.108))
ABc.ns = summary(nls.ns)$coef[,1]
ABc.ns

# (b) smooth the raw data and fit NLS then:
pspl = smooth.spline(x, mxn)
sy = pspl$y
nls.ps = nls(sy~A+B*c^x,start=list(A=.0003, B=.00002, c=1.108))
ABc.ps = summary(nls.ps)$coef[,1]

# compare errors:
cbind(ABc,ABc.ns,ABc.ps)
round(sqrt((ABc-ABc.ns)^2),6)
round(sqrt((ABc-ABc.ps)^2),6)
round(((ABc-ABc.ns)/ABc),4)
round(((ABc-ABc.ps)/ABc),4)

# ------------------------------------------------------------
# QUESTION 4.2

dat = read.csv("insdata.csv")
age = dat$Age
mF = dat$mF
plot(age,mF,t='b')
plot(age,log(mF),t='b')

# (a)
p1 = smooth.spline(age, mF, cv=TRUE)
plot(age, mF, pch=20, t='b')
lines(p1, col=2, lwd=3)

# (b)
par = p1$spar
p2 = smooth.spline(age, mF, spar=par/2)
lines(p2, col=4, lwd=3)

# (c)
# ?smooth.spline
p1$x-p2$x
plot(p1$x, p2$x); abline(a=0, b=1)

# (d) NB: we need to check that the P-spline data is ordered 
# the same way as the original data; it's the case here 
# because the original sample was already ordered, but this 
# is not always true...
p1$x-age 

mse1 = mean( (mF-p1$y)^2 )
mse2 = mean( (mF-p2$y)^2 )
c(mse1, mse2)
sqrt(c(mse1, mse2)) # maybe easier to compare RMSEs instead?
# Explain the difference?
c(sd(p1$y-mF), sd(p2$y-mF))
c(mean(p1$y-mF), mean(p2$y-mF))
# can we explain? (hint: it has to do with the data...)

# (e)
knots = quantile(age, c(.25,.50,.75))
BM = bs(age, knots=knots)
matplot(age,BM,xlab="Age",ylab="Spline basis",t='b')
attributes(BM)
knots
# Note: df = length(knots) + degree... cf. notes!!

# (f) coordinates of age 60 on B-spline basis:
round(BM[which(age==60),],4)
matplot(age,BM,xlab="Age",ylab="Spline basis",t='b')
abline(v=60, lty=3)
abline(h=BM[which(age==60),], lty=2)

# (g) corresponding B-spline for (age, mF) data
bsp = lm(mF~BM)
coef(bsp)
summary(bsp)

# (h) 
mseb = mean((fitted(bsp)-mF)^2)
sqrt(c(mse1, mse2, mseb))

plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
	xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(p1,col=2,t='l',pch='x',lwd=2)
points(age,fitted(bsp),col=3,t='l',pch='x',lwd=4)
points(p2,col=4,t='l',pch='x',lwd=2)

# This B-spline seems like a compromise between the 2 P-splines

# (i) 
# first, see that we're missing some ages:
plot(age)
# interpolation grid:
all.ages = seq(min(age),max(age),by=1)
plot(all.ages)

# - P-spline
sp1 = approx(p1,xout=all.ages)
sum((sp1$y)^2)
sd(sp1$y)

# - lowess
lo = loess(mF~age)
lpred = predict(lo,newdata=data.frame(age=all.ages))
sd(lpred)

plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
	xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(all.ages,sp1$y,col=2,t='b',pch='x',lwd=2)
points(all.ages,lpred,col=4,t='b',pch=20,lwd=2)
points(age,mF,pch=20)

# ------------------------------------------------------------
# QUESTION 4.3

library(splines)
library(car)

head(Prestige)
x = Prestige$income
y = Prestige$prestige

plot(x, y, pch=20)

# polynomial regression (for comparison)
inc.100 <- seq(min(x), max(x), len=100)
mod.lo.inc <- loess(y ~ x, span=.7, degree=1) 
names(mod.lo.inc)
points(mod.lo.inc$x, mod.lo.inc$fitted, pch=15, col=2)

# show the location of the "new points" on graph:
rug(inc.100) 

# generate predictions (note that we need to call the 
# points "x" in the data.frame of new evaluation points,
# so as to match the name of the predictor variable in 
# the loess() call that yielded mod.lo.inc)
pres <- predict(mod.lo.inc, newdata=data.frame(x=inc.100)) 

# display these points
points(inc.100, pres, pch=20, cex=.8, col=4)

# Compute a P-spline
ssp <- smooth.spline(x, y)

# Compute a B-spline
B = bs(x, df=6)
sbp <- lm(y~B)$fitted

# plot(x, y, pch=20)
lines(ssp, lwd=3, col=4)
reorder = order(x)
points(x[reorder], sbp[reorder], pch=20, col=2, t='b', lwd=4)
reorder = order(mod.lo.inc$x)
lines(mod.lo.inc$x[reorder], mod.lo.inc$fitted[reorder], 
	lwd=3, col=8)

legend("bottomright", pch=c(20), col=c(1,2,3,8), bty='n',
	legend=c("data","P-spline","B-spline",
		"polynomial regression"))

plot(x, y, pch=20)
lines(ssp, lwd=3, col=4)
reorder = order(x)
points(x[reorder], sbp[reorder], pch=20, col=2, t='b', lwd=4)

# Now practice calculating the MSEs for these curves...
length(x)
length(y)
length(sbp)
length(ssp$y) # not the original N!!!

plot(sbp, y)
mse.b = mean( (sbp-y)^2 )

# For the P-spline, we need to interpolate!
spp = approx(ssp$x, ssp$y, xout=x, rule=2)$y
mse.p = mean( (spp-y)^2 )

c(mse.b, mse.p)

