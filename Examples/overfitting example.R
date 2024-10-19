## ------------------------------------------
## ST4060 / ST6040
## Eric Wolsztynski
## Motivation: overfitting
## ------------------------------------------

# (1) illustration of overfitting in 1D 

dat = na.omit(airquality)
x = dat$Temp
y = dat$Ozone
par(font=2, font.lab=2, font.axis=2) # use bold fonts in figure
plot(x, y, pch=20, xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))

dat = data.frame(x=x, y=y) # re-building data frame
lm.all = lm(y~., data=dat)

n = nrow(dat)
i.train = which(y<100)
lm.train = lm(y~., data=dat[i.train,])

par(font=2, font.lab=2, font.axis=2) # use bold fonts in figure
plot(x, y, pch=20, xlab="Temperature (degrees F)", ylab="Ozone (ppb)",
	xlim=c(55,105))
points(dat[i.train,], pch=20, cex=1.2, col=2)
abline(lm.train, lwd=3, col=2)
abline(lm.all, lwd=2, col=1, lty=2)

lm.predict = predict(lm.train, newdata=dat[-i.train,])
points(x[-i.train], lm.predict, pch='*', col='blue', cex=1.6)
points(x[-i.train], y[-i.train], pch=10, col='blue', cex=1.6)

# train-set error
sqrt(mean( (fitted(lm.train)-y[i.train])^2 ))
# test-set error
sqrt(mean( (lm.predict-y[-i.train])^2 ))


# (2) illustration of overfitting in multivariate regression
par(mfrow=c(1,2))

# Load some dataset
dat = mtcars
n = nrow(mtcars)
R = 20
K = 10
fit = mse = numeric(K*R)
folds = caret::createFolds(1:n, K, list=FALSE)
set.seed(4060)

for(r in 1:R){
	# shuffle initial dataset
	dat = dat[sample(1:n,n,replace=FALSE),]
	# define x and y
	y = dat$mpg
	x = dat
	x$mpg = NULL # this to remove "y" from "x"

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
	}
}
# CV estimate of model fit error:
mean(fit)
# CV estimate of model prediction error:
mean(mse)
boxplot(fit,mse,names=c("Model fit error","Model prediction error"),
				main=paste(K,"-fold CV",sep=''), ylim=c(0,100))
abline(h=median(fit),lwd=4)



