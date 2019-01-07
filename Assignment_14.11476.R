#Libraries
library(caret)
library(data.table)
library(MatrixModels)
library(glmnet)

setwd("C:/Users/shillar.mpofu/Desktop/DS Assignments/Technical/MODELLING IN R -IV")
getwd()
blogTrain = read.csv('blogData_train.csv')

#combine all files except "blogData_train.csv" into one dataframe
filenames=list.files(full.names=TRUE)
allTest=lapply(filenames[-61],function(i){ read.csv(i, header=FALSE)})
blogTest= do.call(rbind.data.frame, allTest)

# error measure
mse = function(y_hat, y) {
  mse = mean((y - y_hat)^2)
  return(mse)
}

# create design matrices
train_x = model.Matrix(X0.0.57 ~ . - 1, data = blogTrain, sparse = F)
train_x_sparse = model.Matrix(X0.0.57 ~ . - 1, data = blogTrain, sparse = T)
train_y = blogTrain$X0.0.57

test_x = model.Matrix(V281 ~ . - 1, data = blogTest, sparse = F)
test_y = blogTest$V281

# Linear Model Using LASSO
mdl_lasso = cv.glmnet(train_x_sparse, train_y, family = "gaussian", alpha = 1)
pred_lasso = predict(mdl_lasso, newx = test_x)
mse(pred_lasso, test_y)
