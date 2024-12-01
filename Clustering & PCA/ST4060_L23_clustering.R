##
## ST4060 / ST6040 
## Demo of clustering 
## Eric Wolsztynski
##

# ------------------------------------------------
# Illustration of hierarchical clustering
data(eurodist)

hc = hclust(eurodist, method="ward")
hc = hclust(eurodist, method="single")
hc = hclust(eurodist, method="complete")
plot(hc)

# ------------------------------------------------

x = iris[,1:4]
plot(x[,1:2], pch=20, cex=2)
plot(x[,1:2], pch=20, cex=2, col=c(1,2,4)[iris$Species])
plot(x[,c(3,4)], pch=20, cex=2, col=c(1,2,4)[iris$Species])
plot(scale(x)[,c(3,4)], pch=20, cex=2, col=c(1,2,4)[iris$Species])

# ------------------------------------------------ 
# hierarchical clustering

xs = apply(x, 2, scale)
ho = hclust(dist(x), method="ward.D")
hc = cutree(ho, k=3)
plot(x[,1:2], pch=20, cex=2, col=hc)
plot(x[,3:4], pch=20, cex=2, col=hc)

# ------------------------------------------------ 
# partition-based clustering

set.seed(1)
ko = kmeans(x, centers=3)
ko 
plot(x[,1:2], pch=20, cex=2, col=ko$cluster)
plot(x[,3:4], pch=20, cex=2, col=ko$cluster)

table(hc, ko$cluster)
table(iris$Species, ko$cluster)

COLS = c(1,2,4)
xs = apply(x,2,scale) 
k1 = kmeans(xs, 3)
table(k1$cluster, iris$Species)
plot(x[,1:2], col=COLS[k1$cluster], pch=20, cex=2)
table(k1$cluster, hc) # compare with hclust output

# How to 'decide' on optimal number of clusters? 
library(NbClust) # this package is handy...
?NbClust
nbo = NbClust(x, method="kmeans")
names(nbo)
nbo$All.index
nbo$Best.partition
plot(x[,1:2], col=c(1,2,4)[nbo$Best.partition], pch=20)
# ... but we're still none the wiser!

# ------------------------------------------------
# Gaussian mixture modelling

library(MASS)
library(mclust)

om = Mclust(x) # letting the function decide on # of components
om2 = Mclust(x,G=2) # requiring 2 components
om3 = Mclust(x,G=3) # requiring 3 components
summary(om)
summary(om3)

plot(om,what="density")
pdf2 = densityMclust(x,G=2)
pdf3 = densityMclust(x,G=3) 
plot(pdf2,what="density",type="persp")
plot(pdf3,what="density",type="persp")
# 3 clusters seem required for classification wrt petal width 
# and perhaps petal length.

# (dimension reduction -- this is just as a demo)
om2dr = MclustDR(om2)
om3dr = MclustDR(om3)
plot(om3dr, what="boundaries")
plot(om3dr, what="classification")
plot(om2dr, what="classification")

# (other ways of visualising -- this is just as a demo)
plot(x[,4],om3$uncertainty,pch=20,xlab="Petal Width",ylab="Uncertainty")
plot(x[,3],om3$uncertainty,pch=20,xlab="Petal Length",ylab="Uncertainty")
cols = c(1:2)[1+as.numeric(om3$uncertainty>.05)]
cexs = c(.5,1.5)[1+as.numeric(om3$uncertainty>.05)]
plot(x[,4],x[,3],pch=20,col=cols,cex=cexs,xlab="Petal Width",ylab="Petal Length")
misclassed = which(om3$classification-as.numeric(iris[,5])>0)
points(x[misclassed,4],x[misclassed,3],pch=21,col=4,cex=1.8)


# ------------------------------------------------
# Other example: The faithful dataset

# (a) Try crude clustering of the dataset
plot(faithful)
om = Mclust(faithful)
summary(om)

# (b) Evaluate the mixture pdf for the 2D dataset
# ?plot.densityMclust
pdf = densityMclust(faithful)
plot(pdf, what="density", data=faithful, drawlabels=TRUE)
plot(pdf, what="density", type="image", col="steelblue", grid=200)
plot(pdf, what="density", type="persp", theta=-25, phi=20)

# (c) Visualize output classification
cols = c(1,2,4)[om$classification]
plot(faithful, pch=20, col=cols)

# uncertainty:
plot(om$uncertainty, t='l') 
# above graph not very informative in terms of which points 
# are associated with higher uncertainty, so let's check 
# this on the scatterplot:
plot(faithful, pch=21, col=8)
points(faithful, cex=om$uncertainty*3, pch=20)

# boundaries:
omdr = MclustDR(om)
plot(omdr, what="contour")
plot(omdr, what="boundaries")
