B =1000
n = length(x)

#Can change mean for sd or whatever we want to compute
replicate(B, mean(sample(x,size = n, replace =TRUE)))



# For loop Method

set.seed(1)

B = 100
n = length(data)
effect = p_vals = int = numeric(B)
for(b in 1:B){
  indices = sample(1:n,size = n, replace = TRUE)
  x = data$x[indices]
  y = data$y[indices]
  olm = lm(y ~ x, data = data)
  effect[b] = coef(olm)[2] #x1
  p_vals[b] = summary(olm)$coefficients[8]
  
  #Stor the intercept for part d)
  int[b] = coef(olm)[1] #x0
}
