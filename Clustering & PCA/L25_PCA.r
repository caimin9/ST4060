##
## ST4060 / ST6040
## Eric Wolsztynski
## Demo on Principal Component Analysis
##

rm(list=ls())

# 1st example

plot(EuStockMarkets, lwd=3)
pca = prcomp(EuStockMarkets)
par(mfrow=c(4,1), mar=c(1,2,0,2))
for(j in 1:4){
	plot(pca$x[,j], t='l', lwd=3)
}

# 2nd example

x = iris[,1:4]
par(mfrow=c(3,1), mar=c(1,2,1,2))
plot(x[,1:2], pch=20, cex=2)
plot(x[,1:2], pch=20, cex=2, col=c(1,2,4)[iris$Species])
plot(x[,c(3,4)], pch=20, cex=2, col=c(1,2,4)[iris$Species])

# PCA: to scale or not to scale 
pca = prcomp(x)
pca.s = prcomp(x,scale=TRUE)
plot(pca, main="Proportion of variance explained", 
	xlab="Principle Component")
plot(pca.s, add=TRUE, col='pink')
plot(pca, add=TRUE, col='0', border='navy', lwd=2)

# Proportion of variance explained / of information captured
names(pca)
summary(pca)
# reconstruct each of these lines:
pca$sdev
pca$sdev^2/sum(pca$sdev^2)
cumsum(pca$sdev^2/sum(pca$sdev^2))

# explore PCA output:
print(pca)
pca$rotation # same matrix :)
apply(abs(pca$rotation),2,which.max)
row.names(pca$rotation)[apply(abs(pca$rotation),2,which.max)]

spec.dec = eigen(cov(x))
sqrt(spec.dec$values)-pca$sdev
spec.dec$vectors-pca$rotation

# reproduce projection:
x = as.matrix(iris[,1:4])
n = nrow(x)
m = matrix(apply(x,2,mean),nr=n,nc=4,byr=T)
pca = prcomp(x,center=T,scale=F)
ee = eigen(cov(x))
pca$rotation-ee$vectors
z = (x-m) %*% (pca$rotation)
pca$x-z

# what about if we'd scaled?
summary(pca.s)
row.names(pca.s$rotation)[apply(abs(pca.s$rotation),2,which.max)]
sqrt(eigen(cor(x))$values)-pca.s$sdev
eigen(cor(x))$vectors-pca.s$rotation

m = matrix(apply(x,2,mean),ncol=4,nrow=nrow(x),byrow=T)
s = matrix(apply(x,2,sd),ncol=4,nrow=nrow(x),byrow=T)
xs = ((x-m))
pca.s = prcomp(xs,scale=F,center=F)
M = pca.s$x %*% t(pca.s$rotation)
head(M); head(xs)

# reconstruct original data:
pca = prcomp(x,center=TRUE,scale=TRUE)
dim(pca$x)
M = pca$x %*% t(pca$rotation)
M <- scale(M, center=FALSE, scale=1/pca$scale)
M <- scale(M, center=-pca$center, scale=FALSE)
head(M)
head(x)

plot(x[,1:2],pch=20,col=c(1,2,4)[iris$Species],cex=2)
plot(pca$x[,1:2],pch=20,col=c(1,2,4)[iris$Species],cex=2,main="Unscaled PCA image points")
plot(pca.s$x[,1:2],pch=20,col=c(1,2,4)[iris$Species],cex=2,main="Scaled PCA image points")

# This may be useful for classification... we could look into this in Semester 2!

y = iris$Species=='virginica'
o = glm(y~., data=x, fam='binomial')
op = glm(y~., data=data.frame(x=pca$x), fam='binomial')
opr = glm(y~., data=data.frame(x=pca$x[,1:3]), fam='binomial')
summary(o)
summary(op)
plot(fitted(o),fitted(op)); abline(a=0,b=1)
plot(fitted(o),fitted(opr)); abline(a=0,b=1)
sum(residuals(o)^2)
sum(residuals(op)^2)
sum(residuals(opr)^2)

