set.seed(4060)

K =10
lambda = 10
n = length(y)
cases = cut(1:n , K,labels = F )

train_RMSE = test_RMSE = numeric(K)
cv_train_RMSE = cv_test_RMSE = numeric(K)
glm_train_RMSE = glm_test_RMSE = numeric(K)

for(k in 1:K){
  train_indices = which(cases !=k)
  test_indices = which(cases ==k)
  
  x_train = x[train_indices,]
  x_test = x[test_indices,]
  y_train = y[train_indices]
  y_test = y[test_indices]
  
  #a) Lasso Model
  train_model = glmnet(x_train,y_train, lambda = lambda, alpha =1)
  y_train_pred = predict(train_model, x_train)
  train_RMSE[k] = rmse(y_train_pred,y_train)
  
  y_pred_test = predict(train_model,x_test)
  test_RMSE[k] = rmse(y_pred_test,y_test)
  
  #b) Use cv value for lambda
  min_lambda = cv.glmnet(x_train,y_train)$lambda.min
  cv_train_model = glmnet(x_train,y_train, lambda = min_lambda, alpha =1)
  cv_y_train_pred = predict(cv_train_model, x_train)
  cv_train_RMSE[k] = rmse(cv_y_train_pred,y_train)
  
  cv_y_pred_test = predict(cv_train_model,x_test)
  cv_test_RMSE[k] = rmse(cv_y_pred_test,y_test)
  
  #c) WATCH OUT for this , how you define the glm model!!!!!
  glm_train_model = glm(Systolic~., data =dat[train_indices,], family ='gaussian')
  glm_y_train_pred = predict(glm_train_model, newdata = data.frame(x_train))
  glm_train_RMSE[k] = rmse(glm_y_train_pred,y_train)
  
  glm_y_pred_test = predict(glm_train_model,newdata = data.frame(x_test))
  glm_test_RMSE[k] = rmse(glm_y_pred_test,y_test)

}

#a)
mean(train_RMSE)
mean(test_RMSE)

#b)
mean(cv_train_RMSE)
mean(cv_test_RMSE)

#c)
mean(glm_train_RMSE)
mean(glm_test_RMSE)

boxplot(train_RMSE, cv_train_RMSE, test_RMSE, cv_test_RMSE, 
        names = c("Lasso Train","CV Lasso Train","Lasso Test","CV Lasso Test"),
        las =1)
