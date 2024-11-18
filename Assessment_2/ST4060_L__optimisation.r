# ---------------------------------------------------------
# Eric Wolsztynski, UCC
# Nonlinear regression and grid-search
# ---------------------------------------------------------

while( dev.next()>1 ){ dev.off() }
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)

#----------------------------------------------------------------------
#### Recall: nonlinear regression (univariate model)

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

# how to perform a grid search
L = 21
# a.grid = seq(0.2, 0.4, length=L)
# b.grid = seq(0.058, 0.68, length=L)
a.grid = seq(0.001, 0.5, length=L)
b.grid = seq(0.048, 0.061, length=L)
crit = matrix(NA, nrow=L, ncol=L)
for(i in 1:L){
	for(j in 1:L){
		crit[i,j] = mean( (y - a.grid[i] * exp( b.grid[j] * x))^2 )		
	}
}
i.opt = arrayInd(which.min(crit), .dim=c(L,L))
a.grid[i.opt[1]] # "optimal" a
b.grid[i.opt[2]] # "optimal" b
crit[i.opt[1], i.opt[2]] # value of the criterion at that "optimal" location
persp(a.grid, b.grid, crit, col="lightblue",theta=0)
persp(a.grid, b.grid, crit, col="lightblue",theta=90)
contour(a.grid, b.grid, crit)

# alternative way to perform the same grid search: 
ab.grid = expand.grid(a.grid,b.grid)
G = nrow(ab.grid)
crit = numeric(G)
for(i in 1:G){
	crit[i] = mean( (y - ab.grid[i,1] * exp( ab.grid[i,2] * x))^2 )		
}
ab.grid[ which.min(crit), ]
# optimal values of a and b are respectively:
 # 0.3503 0.0597

#----------------------------------------------------------------------
# Should we add a third parameter to scale the exponential?
crit <- function(th,x,y){
# Must have theta as 1st parameter and return a single value...
# Here we implement a simple linear regression model
# for the sake fo the example:
	a = th[1]
	b = th[2]
	c = th[3] ## additional scaling parameter
	return( sum( (y-c*exp(a+b*x))^2 ) ) 
}
(optim.out.alt = optim(par=c(0,.02,1), fn=crit, x=x, y=y)) # init c=1
# NB: don't forget to include this additional parameter when evaluating 
# the fitted observations!!! (which I did during the lecture ;))
yhat.alt = optim.out.alt$par[3]*exp(optim.out.alt$par[1] + optim.out.alt$par[2]*x)
# earlier NLS approach:
nlreg <- nls(y~exp(a+b*x), start=list(a=0,b=.02)) 
# The result is not great:
plot(x, y, pch=20, cex=2,
  xlab="Temperature (Celsius)", 
  ylab="Vapor pressure (ml of mercury)",
  main="Example: polynomial regression")
lines(x, yhat.alt, col='navy', lwd=3, pch=20, t='p')
lines(x, fitted(nlreg), col='blue', lwd=3, pch=20, t='p')
# lines(x, yhat, col=2, lwd=3)
legend("topleft", col=c(1,4,2), bty='n',
	legend=c('data','nls','optim'), lwd=3, lty=1)

# Take-home messages: 
# 1. The choice of the optimisation algorithm is important 
# and may yield very different fits.
# 2. No matter what alforithm we used, the exponential model
# we chose was not able to capture finer detail in the 
# pattern of interest in this data...

#----------------------------------------------------------------------
# BONUS: a nicer example of a criterion surface plot...

set.seed(4060)
n = 100
x = runif(n,-1,1)
y = .2*x+.3*x^2+rnorm(n,s=.05)
plot(x,y)

L = 100
ag = seq(-1,1,length=L)
bg = seq(-1,1,length=L)
crit = matrix(NA,nrow=L,ncol=L)
for(i in 1:L){
	for(j in 1:L){
		crit[i,j] = sum((y-ag[i]*x-bg[j]*x^2)^2)
	}
}
persp(ag,bg,crit,col='lightblue',theta=45)
contour(ag,bg,crit)

# pretend this is our dataset, and fit it using optim():
criterion <- function(param,x,y){
# The function has to be defined this way 
# (with param as its 1st argument)
# if we are going to apply optim() to it... cf. ?optim
  a = param[1]
  b = param[2]
  return( mean( (y - a*x - b*x^2)^2 ) )
}
# Then, apply optim() to it:
oo = optim(par=list(a=0, b=0), fn=criterion, x=x, y=y)
# now reconstruct fitted values from optim() output:
a.hat = oo$par[1]
b.hat = oo$par[2]
y.hat = a.hat*x + b.hat*x^2
plot(x, y, pch=20, main="Toy quadratic data")
points(x, y.hat, pch=21, col=2, cex=1.2)
legend("topleft", legend=c("data","NLS fit","OPTIM fit"), 
      col=c(1,4,2), pch=c(20,20,21), bty='n')

