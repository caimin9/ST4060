B =1000
n = length(x)

#Can change mean for sd or whatever we want to compute
replicate(B, mean(sample(x,size = n, replace =TRUE)))
