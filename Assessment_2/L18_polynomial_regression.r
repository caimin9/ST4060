## ------------------------------------------
## ST4060 / ST6040
## Eric Wolsztynski
## Nonparametric regression / smoothing
## ------------------------------------------

rm(list=ls())

# ------------------------------------------------------------
# Nonparametric regression (Section 4)
# Local polynomial regression

library(car)
f_value = 1
plot(Prestige$income, Prestige$prestige, xlab="Average income", ylab="Prestige", pch=20)
lines(lowess(Prestige$income, Prestige$prestige, 
			f=as.numeric(f_value), iter=0), 
     lwd=2, col='blue')

?lowess
?loess

mod.lo = loess(Prestige$prestige~Prestige$income+Prestige$education, span=.5, degree=2) 
summary(mod.lo) 
# lines(mod.lo, lwd=2, col='green')

#----------------------------------------------------------------------
### Finding/modelling nonlinear patterns

# Example dataset: airquality
dat = na.omit(airquality)
x = dat$Temp
y = dat$Ozone
par(font=2, font.lab=2, font.axis=2) # use bold fonts in figure
plot(x, y, pch=20, xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))

#### Pick a nonlinear model and fit it using nls()
nlso = nls(y~a*exp(b*x), start=list(a=1, b=0.01))
summary(nlso); coef(nlso)
points(x, fitted(nlso), pch=20, col=4)

# how to predict from this fit:
newx = seq(95,100,by=1)
pred.nls = predict(nlso, newdata=list(x=newx))
points(newx, pred.nls, pch=15, col=2)

#----------------------------------------------------------------------
#### Finding/modelling patterns non-parametrically

#### Using local polynomial regression
lo = loess(y~x)
names(lo)
# check output values are in the same order as input values:
summary(c(lo$x - x)) # ok, they are
summary(c(lo$y - y)) # ok, they are
plot(x, y, pch=20, col=1, 
	xlab="Temperature (degrees F)", ylab="Ozone (ppb)", 
	xlim=c(55,105))
points(x, fitted(nlso), pch=20, col=4)
points(x, lo$fitted, col='navy', pch=15)
#
# ... or...
low = lowess(x, y)
names(low)
# check output values are in the same order as input values:
summary(c(low$x - x))       # NO, THEY ARE NOT
summary(c(low$x - sort(x))) # coz lowess() sorted the input values!
lines(sort(x), low$y, col='red', lwd=3)
legend("topleft", legend=c("data","NLS fit","loess fit","lowess fit"), 
       lty=c(0,0,0,1), col=c('grey','blue','navy','red'), pch=c(20,20,15,-1), lwd=3,
       bty='n')
#
# NB: to work out their MSE's:
sqrt( mean( (y-fitted(nlso))^2 ) )
sqrt( mean( (y-fitted(lo))^2 ) )
sqrt( mean( (y[order(x)]-low$y)^2 ) )

# how to predict from the regression polynomial:
# ?predict.loess	 # exists...
# ?predict.lowess  # does NOT exist! (we'd have to use approx() to interpolate)
newx = seq(95,100,by=1)
# abline(v=newx, col=1, lty=3)
pred.lo = predict(lo, newdata=data.frame(x=newx))
points(newx, pred.lo, pch=15, col=3, cex=2)
pred.low = approx(low$x, low$y, xout=newx, rule=2)$y
points(newx, pred.low, pch='x', col='navy', cex=2)

# extrapolating the loess output:
interp.lo = approx(x=lo$x, y=lo$fitted, xout=newx, rule=2)$y 
points(newx, interp.lo, pch=20, col='orange', cex=1.5)

# ------------------------------------------------------------------ 
# Another look at a multidimensional polynomial

L = 100
X1 = seq(9,21,length=L)
X2 = seq(3,17,length=L)

ev <- function(x1,x2){
	return(90 + 150*x1 + 105*x2 - 4*x1^2 - 3*x2^2 - 3*x1*x2)
}

Y = matrix(NA,ncol=L,nrow=L)
for(i in 1:L){
	for(j in 1:L){
		Y[i,j] = ev(X1[i],X2[j])
	}
}
contour(X1,X2,Y)
persp(X1,X2,Y)

# Generate noisy observations:

y = Y + matrix(10*rnorm(L*L),ncol=L,nrow=L) + matrix(50*runif(L*L,-1,1),ncol=L,nrow=L)
contour(X1,X2,Y)
contour(X1,X2,y,add=TRUE,col=2)
persp(X1,X2,y)

# polynomial regression:
gg = expand.grid(X2,X1)
dat = data.frame(y=c(y), x1=gg[,2], x2=gg[,1])
loo = loess(y~., data=dat)
yhat = matrix(loo$fitted,ncol=L,nrow=L)
contour(X1,X2,Y)
contour(X1,X2,y,add=TRUE,col=2)
contour(X1,X2,yhat,add=TRUE,col=4,lwd=4)
par(mfrow=c(2,2))
persp(X1,X2,Y,main='model')
persp(X1,X2,y,main='data')
persp(X1,X2,yhat,main='fitted values')
persp(X1,X2,y-yhat,main='residuals')

# model response as a function of theta:
model <- function(th,x1,x2){
	return(th[1] + th[2]*x1 + th[3]*x2 - th[4]*x1^2 - th[5]*x2^2 - th[6]*x1*x2)
}

# ------------------------------------------------------------------ 
# Using regression splines

library(splines) # contains function bs()
#
# we can compute a B-spline:
KN = quantile(x, c(0.15,0.40, 0.60, 0.70, 0.80, 0.90))
BM = bs(x, knots=KN)
B.spline = lm(y~BM) 
#
# or a P-spline:
P.spline = smooth.spline(x, y, cv=TRUE)  # just ignore that warning message :)
#
# check it out:
plot(x, y, pch=20, col=8, xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))
points(x, fitted(nlso), pch=20, col=4)
points(x, fitted(B.spline), pch=20, col=2)
points(x, fitted(P.spline), pch=15, col='navy')
legend("topleft", legend=c("data","NLS fit","B-spline","P-spline"), 
       col=c(8,4,2,'navy'), pch=c(20,20,20,15), bty='n')

# how to predict from the P-spline:
pred.pspline = predict(P.spline, x=newx)
points(pred.pspline, col=3, pch=14)
