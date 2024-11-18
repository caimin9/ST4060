# ---------------------------------------------------------
# ST4060/ST6040
# Eric Wolsztynski, UCC
# B- and P-splines
# ---------------------------------------------------------

rm(list=ls())

# ------------------------------------------------------------
# Example dataset: airquality
dat = na.omit(airquality)
x = dat$Temp
y = dat$Ozone
par(font=2, font.lab=2, font.axis=2) # use bold fonts in figure
plot(x, y, pch=20, xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))

# NB: how to predict from the regression polynomial:
newx = seq(95,100,by=1)
class(newx)
abline(v=newx, col=1, lty=3)

# ?predict.loess	 # exists...
lo = loess(y~x)
pred.lo = predict(lo, newdata=data.frame(x=newx))
points(newx, pred.lo, pch=15, col=3, cex=2)

# ?predict.lowess  # does NOT exist! (we'd have to use approx() to interpolate)
low = lowess(x, y)
pred.low = approx(low$x, low$y, xout=newx, rule=2)$y
points(newx, pred.low, pch='x', col='navy', cex=2)

# extrapolating the loess output:
interp.lo = approx(x=lo$x, y=lo$fitted, xout=newx, rule=2)$y 
points(newx, interp.lo, pch=20, col='orange', cex=1.5)

#### Using B-splines
library(splines) # contains function bs()
#
# we can compute a B-spline:
KN = quantile(x, c(0.15,0.40, 0.60, 0.70, 0.80, 0.90))
BM = bs(x, knots=KN)
B.spline = lm(y~BM) 
matplot(t(BM),t='l')
# how to predict from the B-spline:
newBM = predict(BM, newx)
pred.bspline = predict(B.spline, newdata=as.data.frame(newBM)) # nope
B.spline = lm(y~., data=as.data.frame(BM))
pred.bspline = predict(B.spline, newdata=as.data.frame(newBM))
#
# or a P-spline:
P.spline = smooth.spline(x, y, cv=TRUE)  # just ignore that warning message :)
# how to predict from the P-spline:
pred.pspline = predict(P.spline, x=newx)

# check it out:
plot(x, y, pch=20, col=8, xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))
points(x, fitted(B.spline), pch=20, col='brown', cex=1.5)
points(x, fitted(P.spline), pch=15, col='navy')
legend("topleft", legend=c("data","NLS fit","B-spline","P-spline"), 
       col=c(8,4,2,'navy'), pch=c(20,20,20,15), bty='n')
points(newx,pred.bspline, col='brown', pch=14)
points(pred.pspline, col=3, pch=14)

# ------------------------------------------------------------------
# NOTE: control on B-spline
KN = quantile(x, c(0.1, 0.4, 0.7))
KN = quantile(x, c(0.05, 0.10, 0.20, 0.23, 0.45, 0.65, 0.95))
KN = quantile(x, c(0.15,0.45, 0.65, 0.95))
KN = quantile(x, c(0.15,0.45, 0.65, 0.95, 0.99))
KN = quantile(x, c(0.15,0.45, 0.65, 0.70, 0.85, 0.91)) # *
KN = quantile(x, c(0.15,0.45, 0.65, 0.70, 0.85, 0.92))
KN = quantile(x, c(0.15,0.45, 0.65, 0.70, 0.85, 0.93))

BM = bs(x, knots=KN)
B.spline.df = lm(y~., data=as.data.frame(BM))
newBM = predict(BM, newx)
pred.bspline = predict(B.spline.df, newdata=as.data.frame(newBM))
#
plot(x, y, pch=20, col=8, 
	xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))
points(x, fitted(B.spline.df), pch=20, col=2, cex=1.3)
lines(newx, pred.bspline, col='navy', pch=8, t='b')

# ------------------------------------------------------------------
# NOTE: control degrees of freedom in P-splines

P.spline3 = smooth.spline(x, y, df=3)
P.spline5 = smooth.spline(x, y, df=5)
P.spline10 = smooth.spline(x, y, df=10)
pred3 = predict(P.spline3, x=newx)
pred5 = predict(P.spline5, x=newx)
pred10 = predict(P.spline10, x=newx)
#
plot(x, y, pch=20, col=8, 
	xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))
points(x, fitted(P.spline3), pch=15, col='navy')
points(x, fitted(P.spline5), pch=15, col='blue')
points(x, fitted(P.spline10), pch=15, col='cyan')
points(pred3, col='navy', pch=20)
points(pred5, col='blue', pch=20)
points(pred10, col='cyan', pch=20)

# NOTE: control smoothness (ie penalty) parameter in P-splines

P.spline10a = smooth.spline(x, y, df=10)
P.spline10b = smooth.spline(x, y, df=10, spar=.9)
P.spline10c = smooth.spline(x, y, df=10, spar=.1)
#
plot(x, y, pch=20, col=8, 
	xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))
points(x, fitted(P.spline10a), pch=15, col='navy')
points(x, fitted(P.spline10b), pch=15, col='blue')
points(x, fitted(P.spline10c), pch=15, col='red')
# plot the latter as a curve (requires re-ordering the points):
is = order(x)
xs = x[is]
lines(xs, fitted(P.spline10c)[is], pch=15, col='red')
