set.seed(4060)

M =1000
n =50
theta.star =4
x = runif(n,1,10)
thetas = numeric(M)
for(i in 1:M){
  noise = rchisq(n, df =3)
  y = theta.star*x + noise
  model = lm(y ~ x+0)
  thetas[i] = coef(model)[2]
}

mean(thetas)
sd(thetas)
