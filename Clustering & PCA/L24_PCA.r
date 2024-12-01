##
## ST4060 / ST6040
## Eric Wolsztynski
## Demo on Principal Component Analysis
##

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

# explore loadings:
print(pca)
pca$rotation # same matrix :)
apply(abs(pca$rotation),2,which.max)
row.names(pca$rotation)[apply(abs(pca$rotation),2,which.max)]



# what about if we'd scaled?
summary(pca.s)
row.names(pca.s$rotation)[apply(abs(pca.s$rotation),2,which.max)]

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

