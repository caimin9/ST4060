library(ISLR)

dat = na.omit(Hitters)
dat$Salary = log(dat$Salary)
y = dat$Salary
xm = model.matrix(Salary~.+0,dat)
pca = prcomp(xm, scale=TRUE)

j = which(summary(pca)$importance[3,]>.95)[1]
df = data.frame(Salary=y,x=pca$x)
df.sub = data.frame(Salary=y,x=pca$x[,1:j])

o1 = lm(Salary~., dat) # linear regression on original dataset
o2 = lm(Salary~., df)  # linear regression on projected dataset
o3 = lm(Salary~., df.sub) # linear regression on projected subset

# RSS
sum(residuals(o1)^2)
sum(residuals(o2)^2)
sum(residuals(o3)^2)

par(mfcol=c(2,3), pch=20)
plot(fitted(o1),fitted(o2))
plot(fitted(o1),fitted(o3))
plot(fitted(o1),y)
plot(fitted(o3),y)
hist(residuals(o1),xlim=c(-3,3))
hist(residuals(o3),xlim=c(-3,3))

dim(dat)
dim(df)
dim(df.sub) # half the number of variables...

summary(o1)$adj.r.squared
# to replicate:
n = nrow(dat)
p = ncol(dat)-1
rss = sum(residuals(o1)^2)
tss = (n-1)*var(y)
(r2 = 1 - rss/tss)
(r2.adj = 1-(1-r2)*(n-1)/(n-p-1))
summary(o3)$adj.r.squared
