# Recall the 'cars' example:
par(font=2, font.axis=2, font.lab=2)
plot(cars, pch=20, cex=2, xlim=c(0,25))

# First model, simple linear regression:
# Y = beta_0 + beta_1*X + noise
fit1 = lm(dist~speed, data=cars)
abline(fit1, col='red', lwd=4)

# Second model with intercept=0, i.e.
# Y = beta*X + noise
fit2 = lm(dist~speed+0, data=cars)
abline(fit2, col='blue', lwd=4)

hist(cars$dist)
# suggests the normal distribution may not be
# appropriate to model Y...
# Try a different model/assumption:
# Third model:
fit3 = glm(dist~speed+0, data=cars, 
					family=Gamma(link="identity"))
abline(fit3, col='navy', lwd=4)
summary(fit3)

# Shape of the LS criterion:
L = 1000
betas = seq(1, 5, length=L)
ls.crit = numeric(L)
x = cars$speed
y =cars$dist
# evaluate the model at L=1000 values of beta:
for(i in 1:L){
	res = y-betas[i]*x
	ls.crit[i] = mean((res)^2)
}
plot(betas, ls.crit)
# minimum of this criterion:
min(ls.crit)
# argument of the minimum for this criterion:
betas[which.min(ls.crit)]

# another GLM example with more than one variable:
fit = glm(mpg~., data=mtcars)
summary(fit)
fit = glm(mpg~cyl+wt, data=mtcars)
summary(fit)

