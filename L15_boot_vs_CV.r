## ------------------------------------------
## ST4060 / ST6040
## Eric Wolsztynski
## Bootstrapping vs cross-validation
## ------------------------------------------

# (0) preamble

# Monte Carlo analysis of the 63.2% property of the bootstrap
N = 1000
M = 100
set.seed(1)
sizes = numeric(B) 
for(i in 1:B){ 
	x = sample(1:N,N,replace=TRUE)
	sizes[i] = length(unique(x)) 
}
mean(sizes/N) # shoudl be near 63.2%
boxplot(sizes/N)

# (1) Boot vs CV...

# make up some data
N = 1000
set.seed(1)
x = runif(N, 2, 20)
y = 2+5*x+10*rnorm(N)
plot(x,y)

# Bootstrapping - OOB evaluation
fitb = mseb = numeric(B)
set.seed(4060)
for(b in 1:B){
	ib = sample(1:N, N, replace=TRUE)
	xb = x[ib]
	yb = y[ib]
	lmb = lm(yb~xb)
	# model fit error:
	fitb[b] = mean((fitted(lmb)-yb)^2) 
	# prediction error from OOB points:
	predb = predict(lmb, newdata=data.frame(xb=x[-ib]))
	mseb[b] = mean((predb-yb[-ib])^2)
}

# k-fold CV:
K = 3 
folds = cut(1:N, K, labels=FALSE)
fitk = msek = numeric(K)
set.seed(4060)
for(k in 1:K){
	ik = which(folds!=k)
	# train:
	xk = x[ik]
	yk = y[ik]
	lmk = lm(yk~xk)
	fitk[k] = mean((fitted(lmk)-yk)^2) # model fit error
	# test:
	predk = predict(lmk, newdata=data.frame(xk=x[-ik]))
	msek[k] = mean((predk-yk[-ik])^2) # test set error
}
par(mfrow=c(2,1))
boxplot(fitb,fitk,col=c(4,2),main="Training", names=c('Bootstrap','k-fold CV'))
boxplot(mseb,msek,col=c(4,2),main="Test",names=c('Bootstrap','k-fold CV'))

# (3) repeated k-fold CV:
# repeat the k-fold CV loop R times, shuffling the initial 
# dataset at the start of each new run of k-fold CV:
K = 3
R = round(B/K)
folds = cut(1:N, K, labels=FALSE)
fitk = msek = numeric(K*R)
set.seed(4060)
for(r in 1:R){ # R repeats of a whole k-fold CV 
	# first shuffle the dataset:
	ris = sample(1:N, N, replace=FALSE)
	# (we need to shuffle x and y in *the same way* so as 
	# to not break up the pairs (x,y))
	x = x[ris]
	y = y[ris]
	# perform a whole k-fold CV pass on shuffled dataset:
	for(k in 1:K){
		kk = (r-1)*K+k
		ik = which(folds!=k)
		# train:
		xk = x[ik]
		yk = y[ik]
		lmk = lm(yk~xk)
		fitk[kk] = mean((fitted(lmk)-yk)^2) # model fit error
		# test:
		predk = predict(lmk, newdata=data.frame(xk=x[-ik]))
		msek[kk] = mean((predk-yk[-ik])^2) # test set error
	}
}

par(mfrow=c(2,1))
boxplot(fitb,fitk,col=c(4,2),main="Training", names=c('Bootstrap','k-fold CV'))
boxplot(mseb,msek,col=c(4,2),main="Test",names=c('Bootstrap','k-fold CV'))
