# ---------------------------------------------------------
# Eric Wolsztynski, UCC
# Linear and nonlinear regression
# ---------------------------------------------------------

while( dev.next()>1 ){ dev.off() }
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)

#----------------------------------------------------------------------
#### (A) Finding/modelling linear patterns

#### Example 1: linear regression (univariate model)

x = cars$speed
y = cars$dist
plot(x, y, pch=20, xlab="speed", ylab="distance", xlim=c(0,26))
(lmo = lm(y~x))
abline(lmo, lwd=2, col=2)
#
# how to predict from this fit:
# (just making up 5 new data points arbitrarily here, 
# for the sake of the example:)
newx = seq(30,35,by=1)
lmo.pred = predict(lmo, newdata=data.frame(x=newx))
plot(x, y, pch=20, xlab="speed", ylab="distance", xlim=c(0,35))
points(newx, lmo.pred, pch=20, col=4)
abline(lmo, lwd=2, col=2)

#### Example 2: polynomial regression (univariate model)

plot(pressure, pch=20, cex=2)
x = pressure$temperature
y = pressure$pressure

lin.reg = lm(y~x)
pol.reg2 = lm(y ~ x + I(x^2))
pol.reg3 = lm(y ~ x + I(x^2) + I(x^3))
pol.reg4 = lm(y ~ x + I(x^2) + I(x^3) + I(x^4))
pol.reg5 = lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
lines(x, fitted(pol.reg2), col=4, lwd=3)
lines(x, fitted(pol.reg3), col=2, lwd=3)
lines(x, fitted(pol.reg4), col='orange', lwd=3)
lines(x, fitted(pol.reg5), col='navy', lwd=3)

#### Example 3: linear regression (multivariate model)

library(car)  # contains dataset Prestige
pairs(Prestige)
# Let's simplify this dataset for this example...
# ?Prestige
# ... We want to model (predict)
# income: Average income of incumbents, dollars, in 1971.
# ... based on:
# education: Average education of occupational incumbents, years, in 1971.
# women: Percentage of incumbents who are women.
# prestige: Pineo-Porter prestige score for occupation, 
#           from a social survey conducted in the mid-1960s.
y = Prestige$income
x = Prestige[,c("education","prestige","women")]
class(x) # note x is of class "data.frame" as opposed to "matrix"
lmo = lm(y~x) # and this does not work out well in lm()...
lmo = lm(y~., data=x) # this works though (check out ?lm)
summary(lmo)

# how to predict from this fit:
# (just making up 5 new data points arbitrarily here, 
# for the sake of the example:)
newx = data.frame(education=seq(11,16,length=5), 
					prestige=seq(14,80,length=5),
					women=seq(10,90,length=5))
lmo.pred = predict(lmo, newdata=newx)

#----------------------------------------------------------------------
#### (B) Finding/modelling nonlinear patterns

#### Example 2 (continued): nonlinear regression (univariate model)

plot(pressure, pch=20, cex=2)
x = pressure$temperature
y = pressure$pressure

nlreg <- nls(y~exp(a+b*x), start=list(a=0,b=.5)) # error... need for better initialisation
summary(nlreg)
cc = coef(nlreg)
curve(exp(cc[1]+cc[2]*x), col='orange', add=T, lwd=3) 
points(x, exp(0+0.02*x), col=4, t='b', lwd=3, cex=.5)

nlreg2 <- nls(y~exp(a*x), start=list(a=.02))
cc = coef(nlreg2)
curve(exp(cc[1]*x), col='cyan', add=T, lwd=3) 
legend("topleft", col=c('black','blue','red','cyan'), lwd=3,bty='n',
	legend=c("Data","Fitted (p=2)","Guessed","Fitted (p=1)"))

#### Example 4: nonlinear regression (univariate model)

# airquality
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

#### Alternative: Pick a nonlinear model and fit it using optim()
# First, write a function that evaluates the criterion:
criterion <- function(param,x,y){
# The function has to be defined this way 
# (with param as its 1st argument)
# if we are going to apply optim() to it... cf. ?optim
  a = param[1]
  b = param[2]
  return( mean( (y - a*exp(b*x))^2 ) )
}
# Then, apply optim() to it:
oo = optim(par=list(a=1, b=0.01), fn=criterion, x=x, y=y)
# now reconstruct fitted values from optim() output:
a.hat = oo$par[1]
b.hat = oo$par[2]
y.hat = a.hat * exp(b.hat*x)
points(x, y.hat, pch=21, col=2, cex=1.2)
legend("topleft", legend=c("data","NLS fit","OPTIM fit"), 
      col=c(1,4,2), pch=c(20,20,21), bty='n')

# NB:
# There is also the question of how to pick appropriate values 
# for the model parameters... Typically, a grid search may be
# used. 

# Grid search example using that same criterion:
a.grid = seq(0.1, 0.5, by=0.05)
b.grid = seq(0.05, 0.07, length=10)
ab.grid = expand.grid(a.grid,b.grid)
G = nrow(ab.grid)
crit = numeric(G)
for(i in 1:G){
	crit[i] = criterion(ab.grid[i,],x,y)	
}

#**********************************************************************
#### THEN: "How good we are at explaining the data?"
#**********************************************************************

# Bootstrapping: allows to assess variability of our estimation/fit
# and prediction performance

# Example 4 (continued): airquality
dat = na.omit(airquality)
x.original = dat$Temp
y.original = dat$Ozone
par(font=2, font.lab=2, font.axis=2) # use bold fonts in figure
plot(x.original, y.original, pch=20, 
     xlab="Temperature (degrees F)", ylab="Ozone (ppb)")
# How reliable is our nonlinear model for this data?
# Bootstrap nonlinear model fit from nls():
B = 100
n = length(x.original)
# we need to store bootstrapped coefficient estimates:
coefs = matrix(NA, nrow=B, ncol=2) 
mse.p = mse.f = numeric(B)
for(b in 1:B){
  ib =  sample(1:n, size=n, replace=TRUE)
  x = x.original[ib]
  y = y.original[ib]
  nlso = nls(y~a*exp(b*x), start=list(a=1, b=0.01))
  coefs[b,]= coef(nlso)
  mse.f[b] = mean((fitted(nlso)-y)^2)
  oob = unique(c(1:n)[-ib])
  x.test = x.original[oob]
  nlso.p = predict(nlso,newdata=data.frame(x=x.test))
  mse.p[b] = mean((nlso.p-y.original[oob])^2)
}
boxplot(coefs, col=c(4,2))
boxplot(mse.f, mse.p, col=c(4,2))



