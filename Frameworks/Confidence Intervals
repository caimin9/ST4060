#------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2022 Question 2 Bootstrapping

# statistic of original sample:
p0 = cor.test(dat$Age,dat$Time)$p.value  # P-value of original data

# 95% Normal CI: #pb is the bootstrapped p-value
mean(pb)+c(-1,1)*1.96*sd(pb)

# BS-adjusted CI: Nonparametric bootstrap 95% CI (q∗α denoting quantile bootstrap p-value): (2p0 − q∗0.975, 2p0 − q∗0.025)
2*p0 - quantile(pb,c(.975,.025))

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#2020 Question 3
#Using (a) and (c), quote the 95% bootstrap confidence interval for the model parameters of the regression model Yi = a(X^2)i + b(Xi) + c + εi 
# bootstrap CI: # coefs.b are all the coefficients in a vector
est0 = coef(nlso2)
qls = apply(coefs.b,2,quantile,.025)
qus = apply(coefs.b,2,quantile,.975)
est0
cbind(2*est0 - qus, 2*est0 - qls)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#t-test = t.test(a,b)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

#2024 Question2 d
