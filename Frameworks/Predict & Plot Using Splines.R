#2020 Q4

dat = read.csv("insdata.csv")
age =dat$Age
mF = dat$mF

plot(age,mF, cex =2,pch=20, main ='Female Mortaility Rates Vs Age')
ss = smooth.spline(age,mF, spar =.5)
lines(ss, col ='red' , lwd =3)

#b)
new_spar = ss$spar/2
ss_2 = smooth.spline(age,mF, spar = new_spar)
lines(ss_2, col = 'blue', lwd = 3)
legend("topleft", col=c('red','blue'), lwd=3,bty='n',
       legend=c("1st Spline","2nd Spline"))

#c)
ss_2$x == ss$x

#d)
ss_yfit = fitted(ss)
ss2_yfit = fitted(ss_2)

mse1 = mean((mF - ss_yfit)^2)
mse2 = mean((mF - ss2_yfit)^2)

# They differ becuase they have different smoothing parameters. The spline with the lower smoothing
# parameter has a lower mse because the penalised criterion has a lower effect on it. For a p-spline
# as the smoothing parametr goes to 0 the function just becomes the mse
# The bias has improved

#e)
library(splines)
kns = quantile(age,c(.25,.5,.75))
b_spline = bs(age,knots = kns)

matplot(age,b_spline)

#f)
round(b_spline[which(age ==60),],4)

#g)
blm = lm(mF ~ b_spline, data = dat)
blm

#h)
bmse = mean((fitted(blm)-mF)^2)
bmse
# The mse computed for the b-spline is similar to that of the p-spline with smoothing param =.5
# Not as effective as the second one

#i)
# Interpolations for ages
plot(age,mF, pch =20, cex =2)
ages = seq(min(age), max(age), by =1)

ss_pred = approx(ss, xout =ages)$y
points(ages,ss_pred, col = 'red', lwd =2)

lss = loess(mF ~ age)
lss_pred = predict(lss,newdata =data.frame(age = ages))
lss_pred

points(ages, lss_pred, col ='blue', pch =16)



plot(age,mF,t='b',pch=20,main="Crude force of mortality (Females)",
     xlab="Age (years)",ylab="Force of mortality",cex=1.2)
points(ages,ss_pred,col=2,t='b',pch='x',lwd=2)
points(ages,lss_pred,col=4,t='b',pch=20,lwd=2)
