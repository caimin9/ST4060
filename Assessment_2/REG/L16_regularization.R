# ---------------------------------------------------------
# Eric Wolsztynski, UCC
# Overfitting and regularization
# ---------------------------------------------------------

while( dev.next()>1 ){ dev.off() }
rm(list=ls(pos=.GlobalEnv), pos=.GlobalEnv)

#----------------------------------------------------------------------

library(glmnet)

# -------------------------------------------------------------
# (0) Motivation: overfitting

# Load some dataset
dat = mtcars
n = nrow(mtcars)
R = 20
K = 2
fit = mse = numeric(K*R)
fit.l = mse.l = numeric(K*R)
folds = cut(1:n, K, labels=FALSE)
set.seed(4060)

for(r in 1:R){
	# shuffle initial dataset
	dat = dat[sample(1:n,n,replace=FALSE),]
	# define x and y
	y = dat$mpg
	x = dat
	x$mpg = NULL # this to remove "y" from "x"
	xm = as.matrix(x)
	
	for(k in 1:K){		
		# split data into train and test sets
		i.train = which(folds!=k)
		# fit a gLM to the train set
		o = glm(y~., data=x, subset=i.train)
		# get fitted values
		yh = o$fitted.values
		# MSE from model fit
		fit[k+(r-1)*K] = mean((yh-y[i.train])^2)
		# generate predictions for test set from model fit
		yp = predict(o, newdata=x[-i.train,])
		# test set prediction MSE (potentially much larger!?)
		mse[k+(r-1)*K] = mean((yp-y[-i.train])^2)
		
		cv.l = cv.glmnet(xm[i.train,],y[i.train],grouped=FALSE)
		lasso = glmnet(xm[i.train,],y[i.train],lambda=cv.l$lambda.min)
		yh.l = predict(lasso,newx=xm[i.train,])
		fit.l[k+(r-1)*K] = mean((yh.l-y[i.train])^2)
		yp.l = predict(lasso,newx=xm[-i.train,])
		mse.l[k+(r-1)*K] = mean((yp.l-y[-i.train])^2)
	}
}

boxplot(fit,mse,fit.l,mse.l,
	names=c("glm fit","glm error","lasso fit","lasso error"),
	ylim=c(0,25))

# -------------------------------------------------------------
# (1) Hitters example 

library(ISLR)

set.seed(6026)

dat = na.omit(Hitters)
n = nrow(dat)
dat$Salary = log(dat$Salary)

REPS = 50
train.error = test.error = numeric(REPS)
lasso.train.error = lasso.test.error = numeric(REPS)

for(j in 1:REPS){
	dat = dat[sample(1:n, n, replace=FALSE),]
	
	i.train = c(1:200)
	train = dat[i.train,]
	test = dat[-i.train,]
	
	fit = glm(Salary~., data=train)
	pred = predict(fit, newdata=test)
	
	train.error[j] = mean(fit$residuals^2)
	test.error[j] = mean((pred-test$Salary)^2)

	xm = model.matrix(Salary~., data=train)[,-1]
	newxm = model.matrix(Salary~., data=test)[,-1]
	y = train$Salary
	lam = cv.glmnet(xm, y, data=train)
	lasso = glmnet(xm, y, data=train, lambda=lam$lambda.min)
	lasso.pred = predict(lasso, newx=newxm)
	
	lasso.fit = predict(lasso, newx=xm)
	lasso.train.error[j] = mean((lasso.fit-train$Salary)^2)
	lasso.test.error[j] = mean((lasso.pred-test$Salary)^2)
}

summary(fit)
coef(lasso)

par(font=2, font.axis=2)
boxplot(train.error, test.error, 
		main="MLB player salary prediction",
		names=c("Training error", "Test error"),
		col=c('pink','cyan'))
		
par(font=2, font.axis=2)
boxplot(train.error, test.error, 
		lasso.train.error, lasso.test.error, 
		main="MLB player salary prediction",
		names=c("Training error", "Test error", "LASSO \ntraining error", "LASSO \ntest error"),
		col=c('pink','cyan','darkgreen','orange'))
abline(h=median(train.error), lty=2, col='cyan')
abline(h=median(lasso.train.error), lty=2, col='darkgreen')
abline(h=median(test.error), lty=2, col='pink')
abline(h=median(lasso.test.error), lty=2, col='orange')

# -------------------------------------------------------------
# (2) Blood Pressure example 

library(glmnet)

dat = read.table(file="/Users/ewol/Downloads/Blood Pressure.txt",
	header=TRUE)
	
str(dat)
dat$PatientID = NULL # getting rid of this variable

# GLM fit
glm.fit = glm(Systolic~., data=dat)
mean(glm.fit$residuals^2)

# LASSO - note: fitting these models is rather cody...
alpha = 1
# prepare the data:
xm = model.matrix(Systolic~., data=dat)[,-1]
y = dat$Systolic
# compute best lambda:
lam = cv.glmnet(xm, y, alpha=alpha) 
# c) fit model using best lambda:
lasso = glmnet(xm, y, lambda=lam$lambda.min)
# d) recover fitted values:
lasso.fit = predict(lasso, newx=xm)
# e) calculate RSS or MSE from LASSO
mean((lasso.fit-y)^2)

# Defining more user-friendly functions... 
# No need to be able to reproduce this.
regularized.model <- function(x, response, alpha=1, ...){
# wrapping glmnet into a more user-friendly function
	# prepare the data
	if(!(response %in% names(x))){
		stop("The response name provided is not included in the dataset")
	}
	fml = as.formula(paste(response, "~."))
	xm = model.matrix(fml, data=x)[,-1]
	y = x[[response]]

	if(class(y)=="factor"){
		if(length(levels(y))==2){
			fam = "binomial"
		} else {
			fam = "multinomial"
		}
	} else {
		fam = "gaussian"
	}
	
	lam = cv.glmnet(xm, y, alpha=alpha, 
		family=fam, ...) 
	mod = glmnet(xm, y, alpha=alpha, lambda=lam$lambda.min, 
		family=fam, ...)
	mod.fit = predict(mod, newx=xm)
	return(list(model=mod, fitted.values=mod.fit, 
			y=response, family=fam))
}

predict.regularized.model <- function(fit, test.data, ...){
	fml = as.formula(paste(fit$y, "~."))
	newx = model.matrix(fml, data=test.data)[,-1]
	return(predict(fit$model, newx, ...))
}

set.seed(4060)
ridge.fit = regularized.model(dat,"Systolic",alpha=0)
enet.fit = regularized.model(dat,"Systolic",alpha=.6)
lasso.fit = regularized.model(dat,"Systolic",alpha=1)

effects = cbind(as.numeric(coef(glm.fit)),
		as.numeric(coef(ridge.fit$model)),
		as.numeric(coef(enet.fit$model)),
		as.numeric(coef(lasso.fit$model)))
rownames(effects) = rownames(coef(ridge.fit$model))
colnames(effects) = c("GLM","ridge","e-net","LASSO")

Errors = c(mean(glm.fit$residuals^2), 
			mean((ridge.fit$fitted.values-y)^2),
			mean((enet.fit$fitted.values-y)^2),
			mean((lasso.fit$fitted.values-y)^2))

effects = data.frame(rbind(effects, Errors))
round(effects, 3)


# -------------------------------------------------------------
# (3) Prestige example 

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
lmo = lm(y~., data=x) # this works though (check out ?lm)
summary(lmo)
# how to predict from this fit:
newx = data.frame(education=seq(11,16,length=5), 
					prestige=seq(14,80,length=5),
					women=seq(10,90,length=5))
lmo.pred = predict(lmo, newdata=newx)

# regularized regression (LASSO, ridge, etc.)
# NB: this one needs a little hack regarding format for x, if x is a data.frame
lasso = glmnet(x, y, alpha=1) # does not work because x is a data.frame (= list)
xm = model.matrix(y~., data=x)[,-1]
class(x); class(xm)
lasso = glmnet(xm, y, alpha=1)
coef(lasso) 
# What's going on??? 
# Oh yeah, we need to pick one value for smoothing parameter lambda!
lasso.cv = cv.glmnet(xm, y, alpha=1)
lasso = glmnet(xm, y, alpha=1, lambda=lasso.cv$lambda.min)
# let's compare LASSO with our earlier linear regression output:
cbind(coef(lasso), coef(lmo))

# how to predict from this fit:
# ?predict.glmnet
lasso.pred = predict(lasso, newx=as.matrix(newx))
# NB:
cbind(lasso.pred, lmo.pred) # close enough to the lm() predictions
mean((lasso.pred-lmo.pred)^2)
mean((lasso.pred-lmo.pred)/lmo.pred)*100 # less than 1% error...
