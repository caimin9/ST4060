# Define RMSE function
RMSE = function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

set.seed(4060)
K =5
n = length(x)

#### Jumble up dataset --> change x_train,test & y_train,test if want it jumbled up
indices = sample(1:n,n,replace = FALSE)
x_new = x[indices]
y_new = y[indices]
######

folds = cut(1:n,K,labels =FALSE)
folds
predict_rmse = numeric(K)
for(k in 1:K){
  test_index = which(folds == k)
  train_index = which(folds != k)
  
  x_train = x[train_index]
  x_test = x[test_index]
  
  y_train = y[train_index]
  y_test = y[test_index]
  
  ss = smooth.spline(x_train,y_train)
  
  y_pred = predict(ss, x_test)$y
  predict_rmse[k] = RMSE(y_pred,y_test)
}

mean(predict_rmse)
predict_rmse


