data = read.csv("nonlinear_dataset.csv")
data

N =125
x = data$x
y = data$y

nlm1 = nls(y ~ a*I(x^2) + sin(b+c*x),start =list(a=.05,b=.4,c = 2))
nlm2 = nls(y ~ a*I(x^2) + b*x + c, start =list(a=.05,b=.4,c = 2))
coef(nlm1)
coef(nlm2)

rmse_1 = sqrt(mean( (fitted(nlm1) - y)^2) )
rmse_2 = sqrt(mean( (fitted(nlm2) - y)^2) )

#b) scatterplots of model fits
line_1 = coef(nlm1)[1]*I(x^2) + sin(coef(nlm1)[2]+coef(nlm1)[3]*x)
line_2 = coef(nlm2)[1]*I(x^2) + coef(nlm2)[2]*x + coef(nlm2)[3]

#can't put in line 1 and line 2 into curve function --> it needs a function as a parameter
plot(x,y, pch =20)
#Can use curve function or use points
points(x,fitted(nlm1), col = 'blue' , pch =17)
points(x,fitted(nlm2), col ='red', pch =16)

curve(coef(nlm1)[1]*I(x^2) + sin(coef(nlm1)[2]+coef(nlm1)[3]*x),add =T, col ='blue',lwd =2)
curve(coef(nlm2)[1]*I(x^2) + coef(nlm2)[2]*x + coef(nlm2)[3],add =T, col ='red',lwd =2)

legend("topleft", col=c('blue','red'), lwd=3,bty='n',
       legend=c("Sin Model","Quadratic model"))
grid()

