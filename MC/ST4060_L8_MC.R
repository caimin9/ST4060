# Question 2.5

N = 10   # sample size
M = 1000 # MC reps
# storing vectors:
s1 = numeric(M) # s^2 
s2 = numeric(M) # \hat{sigma}^2
s3 = numeric(M) # output from sd()

# Monte Carlo repetitions:
for(m in 1:M){
	x = rnorm(N, m=0, sd=2)
	xbar = mean(x)
	term = sum((x-xbar)^2)
	s1[m] = sqrt(term/(N-1))
	s2[m] = sqrt(term/(N))
	s3[m] = sd(x)
}
par(mfrow=c(1,1), font.axis=2)
boxplot(s1,s2,s3, names=c('s1','s2','s3'))
abline(h=2, lwd=3, col='orange')

# Monte Carlo estimates of the expected values 
# of these estimators:
mean(s1)
mean(s2)
# Monte Carlo estimates of the standard errors 
# of these estimators:
sd(s1)
sd(s2)

# Question 2.6

N = 100   # sample size
M = 100 # MC reps
ndf = c(2,4,10)
# storing vectors:
xbar = matrix(NA,nrow=M,ncol=3) 
# Monte Carlo repetitions:
for(j in 1:3){
	for(m in 1:M){
		x = rchisq(N, df=ndf[j])
		xbar[m,j] = mean(x)
	}
}	
boxplot(xbar, names=ndf)
abline(h = apply(xbar,2,mean), col=3, lwd=3)


