# explore the data!
x = cars$speed
y = cars$dist
plot(x, y, pch=20, cex=2)
cars

# fit a linear regression model
# (Y = beta_0 + beta_1*X + noise):
fit = lm(y~x)
abline(fit, col='red', lwd=4)

sum(x*y)/sum(x^2)
# compare this value to the output of this second linear regression model (Y = beta*X + noise):
lm(y~x+0)


hist(x)
hist(y)
