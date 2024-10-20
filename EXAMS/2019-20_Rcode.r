### ---------------------------------------------------------------------------
### Question 2

# Data simulation - I/O
if(1){
	set.seed(4060)
	n = 128
	x1 = runif(n, 1, 8)
	x2 = runif(n, .25, 1.15)
	th = c(.2, .2, .1)
	y = th[1] + th[2]*x1 + exp(-th[3]*x2) + rnorm(n, mean=0, sd=.1)
	summary(y)
	x1x2y = data.frame(x1=x1,x2=x2,y=y)
	write.csv(x1x2y, 
		file="/Volumes/macdata/teaching/st4060/exam/2019-20/examdata_2019-20/x1x2y.csv", 
		row.names=FALSE)
} else {
	setwd("/Volumes/macdata/teaching/st4060/exam/2019-20")
	# Load dataset x1x2y.csv into R using the following instruction:
	dat = read.csv(file="x1x2y.csv")
	x1 = dat$x1
	x2 = dat$x2
	y  = dat$y
}

# (a) Create a figure containing the following two plots:
# (i) a set of boxplots showing the distributions of x1, x2 and y respectively;
# (ii) a scatterplot of x1 and x2, using full black dots to represent data points, 
# and using the values in y as dot size.
##### ST1050
par(mfrow=c(1,2), font=2, font.axis=2, font.lab=2)
boxplot(x1x2y, col=c('blue','white','red'))
plot(x1, x2, pch=20, cex=y)

# (b) Inspect the relationship between y and each of x1 and x2. Provide a simple graphical representation of this relationship (using a maximum of 2 graphs), and comment on this output.
##### ST1050
par(mfrow=c(1,2), font=2, font.axis=2, font.lab=2)
plot(x1, y, pch=20)
plot(x2, y, pch=20)
# Linear in x1, not clear whether y and x2 are related at all.

# (c) Fit a linear model in x1 and x2 to the data, using y as the dependent variable. Quote the summary for this output. 
##### ST1050
lmo = lm(y~x1+x2)
summary(lmo)

# (d) Fit a nonlinear model 
# $$
# y = a + b*x1 + exp(-c*x2)
# $$
# to the data, using nls() for optimisation and with initial parameter values list(a=0, b=1, c=0.5). 
# Provide the list of coefficient estimates and their associated p-value. Comment on this output.
nlmo = nls(y~a+b*x1+exp(-c*x2), start=list(a=0,b=1,c=.1))
summary(nlmo)

# (e) 
# (i) Quote the residual sums of squares for the two fits.
# (ii) Comment on the percentage difference between these two values, and indicate which model you would rather use, and why.

rss.lm = sum(lmo$residuals^2)
rss.nlm = sum(residuals(nlmo)^2)
round(c(rss.lm, rss.nlm, (rss.lm-rss.nlm)/rss.lm), 4)
# There is no difference in RSS. Both models seem to represent the data similarly; 
# we should therefore opt for the simpler linear regression model for easier interpretation. 

# (f) Fit the LASSO model with regularization parameter lambda=0.1 to the data, using y as the dependent variable. Quote the coefficient estimates you obtained. 
library(glmnet)
lasso = glmnet(cbind(x1,x2), y, alpha=1, lambda=.1)
coef(lasso)

# (g) Comment on the output of (f), and explain this output with respect to the plots obtained in (a) and (b). If you did not manage to answer this previous question item, indicate what you expect to find in the LASSO output. 

# Variable x2 has been muted. Its contribution is deemed much less important in explaining y than that from the linear term. This is consistent with the plots obtained in (a) and (b), which both show that most of the variability in y is captured in the x1 direction.

# (h) Perform ridge regression with regularization parameter lambda=0.1 to the data, using y as the dependent variable. 
ridge = glmnet(cbind(x1,x2), y, alpha=0, lambda=.1)
coef(ridge)

# (i) Quote the percentage difference between the coefficient estimates obtained in (h) and those obtained for the ordinary linear regression model obtained in (c).
# If you did not manage to answer this previous question item, provide the R instruction you would have used to obtain this result.
##### ST1050
(coef(ridge)-coef(lmo))/coef(lmo)



### ---------------------------------------------------------------------------
### Question 3

### (a) 
a = 3 # shape
b = 2 # rate

x = seq(0,10,length=1000)
par(font=2, font.axis=2, font.lab=2)
plot(x, dgamma(x, shape=a, rate=b), pch=20, ylab="Gamma(3,2) distribution")

### (b) 
set.seed(4060)
M = 1000
N = c(50,100,500)
means = NULL
for(n in N){
	z = matrix(rgamma(n*M, shape=a, rate=b), nrow=M, ncol=n)
	means = cbind(means, apply(z, 1, mean))
}
summary(means)
par(font=2, font.lab=2, font.axis=2)
boxplot(means, names=N, pch='x', xlab="Sample size", ylab="Distributions of sample means")
abline(h=a/b, lwd=2, col=8)

### (c) 
M = 1000
N = 100
coefs0 = matrix(NA, nrow=M, ncol=1)
coefs = matrix(NA, nrow=M, ncol=2)
set.seed(4060)
	x = runif(N, -5, 5)
for(m in 1:M){
	y = 4*x + rgamma(N, shape=a, rate=b)
	coefs0[m] = coef(lm(y~x+0))
	coefs[m,] = coef(lm(y~x))
}
c(apply(coefs0,2,quantile,.025), apply(coefs0,2,quantile,.975))
cbind(apply(coefs,2,quantile,.025), apply(coefs,2,quantile,.975))

par(font=2, font.axis=2, font.lab=2)
boxplot(cbind(coefs, coefs0), 
	main="Distributions of Monte Carlo estimates",
	names=c(expression(theta[0]), expression(theta[1]), expression(theta^star)))
abline(h=4, col=3)

### ---------------------------------------------------------------------------
### Question 4

x = trees$Girth
y = trees$Height
summary(lm(y~x))
x = rep(trees$Girth, 100)
N = length(x)
x = x + runif(N, -1, +1)
y = 62.0313 + 1.0544*x + 4*rnorm(N)
plot(x,y)
points(trees$Girth, trees$Height, pch=15, col=4)
N = nrow(trees)
cc = numeric(N)
set.seed(4060)
for(i in 1:N){
	# CV
	lmo = lm(y[-i]~x[-i])
	cc[i] = summary(lmo)$coef[2,1]
}
mean(cc)

set.seed(4060)
K = 1000
cb = numeric(K)
for(i in 1:K){
	# bootstrapping
	ib = sample(1:N,N,replace=TRUE)
	lmb = lm(y[ib]~x[ib])
	cb[i] = summary(lmb)$coef[2,1]
}
mean(cb)

par(font=2, font.axis=2, font.lab=2)
boxplot(cbind(cc,cb), names=c("CV","Bootstrap"))
abline(h=1.0544)
t.test(cc,cb)

round(cc, 3)
