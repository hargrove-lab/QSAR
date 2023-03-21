# set working directory
setwd(path)

# load data
data <- read.csv('KD_refine.csv')

# Creat the evaluation function: eval_results, which contained RMSE and Rsquare
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square)
}

# create trainingset and testset id using kenStone on Mahalanobis distance
library(prospectr)
xspace <- data[,-1]
ks <- kenStone(as.matrix(xspace), k=12, metric = "mahal",pc=0.99, .center = TRUE, .scale = FALSE)
ks$test

trainid <- ks$test


# assign testset and trainingset
trainingset <- data[trainid,]
testset <- data[-trainid,]

x_train <- as.matrix(trainingset[-1])
y_train <- data.matrix(trainingset[1])
x <- x_train
y <- y_train
x_test <- as.matrix(testset[-1])
y_test <- as.matrix(testset[1])


# Create a blank plot
library(yarrr)
plot(x = 1,
     type = "n",
     xlim = c(min(data$lnKD)-2, max(data$lnKD)+2), 
     ylim = c(min(data$lnKD)-2, max(data$lnKD)+2),
     pch = 16,
     xlab = expression("Obs. lnk"[on]), 
     ylab = expression("Obs. lnk"[on]),
     main = expression("lnk"[on]*": test and training data distrbution"),
     cex.main=2, cex.lab=2, cex.axis=1.5)

# add training and test distribution
points(x=y_test,y=y_test,pch = 16,
     col = transparent("red", trans.val = .2),
     cex = 2)

points(x = y_train,
       y = y_train,
       pch = 16,
       col = transparent("steelblue3", trans.val = .5),
       cex = 2)

abline(coef = c(0,1), 
       lty = 2,lwd = 2)

legend("bottomright",
       legend = c("training", "test"),
       col = transparent(c('steelblue3', 'red'), .2),
       pch = c(16, 16),cex = 2,
       bg = "white")


# stepwise regression
library(car)

mdl_null = lm(lnKD~1, data=trainingset)
summary(mdl_null)

mdl_full = lm(lnKD ~ ., data=trainingset)
summary(mdl_full)

mdl_stepwise = step(mdl_null, scope = formula(c(mdl_full, mdl_null)), direction="both", trace=1, criteria="BIC", k=log(40))
summary(mdl_stepwise)

predict <- predict(mdl_stepwise,newdata = testset)
fitted <- mdl_stepwise$fitted.values
a <- eval_results(testset$lnKD,predict,testset)
b <- eval_results(trainingset$lnKD,fitted,trainingset)

plot(x = 1,
     type = "n",
     xlim = c(min(data$lnKD)-2, max(data$lnKD)+2), 
     ylim = c(min(data$lnKD)-2, max(data$lnKD)+2),
     pch = 16,
     xlab = "Pred. lnKD", 
     ylab = "Obs. lnKD",
     main = "lnKD-stepwise model")

text(x = c(1,4.5,1,4.5),
     y = c(15,15,13,13),
     labels = c("R2_train = ", round(b[2],2), "R2_test =", round(a[2],2)),cex = 1.5)

# Add training points
points(x = fitted,
       y = as.matrix(trainingset[,1]),
       pch = 16,
       col = 'blue',
       cex = 2)

# Add test points
points(x = predict,
       y = y_test,
       pch = 16,
       col = 'red',
       cex = 2)

# Add ref line
abline(coef = c(0,1), 
       lty = 2,lwd = 2)

legend("bottomright",
       legend = c("training", "test"),
       col = c('blue', 'red'),
       pch = c(16, 16),cex = 2,
       bg = "white")

# lasso regression
library(glmnet)
set.seed(1)
lambdas <- 10^seq(2, -6, length = 100)

# use sv.glmnet to find the best lambda for lasso from 5-fold cv
lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
plot(lasso_reg)

# plot the shrinkage graph with multiple lambda values
lasso_model <- glmnet(x_train, y_train, alpha = 1, nlambda =100,standardize = TRUE)
print(lasso_model)
p1 <- plot(lasso_model,xvar="lambda",label = T, lwd=4,cex.lab= 2,cex.axis=2,xlim = c(-4.5,0.5), ylim=c(-20,20))
p1.lty=2
box(lwd=4)

# chose the lambda with lowest mean-squrared error from cv
lambda_best_lasso <- lasso_reg$lambda.min
lambda_best_lasso

# build the lasso regression model using selected descriptors
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda =lambda_best_lasso,standardize = TRUE)
summary(lasso_model)

# find the non-zero coefficients and their names
lasso.coef  <-  predict(lasso_model,type="coefficients")
lasso.coef
lasso.coef[lasso.coef!=0] 
lasso_nonzerocoef <- predict(lasso_model,type="nonzero")
lasso_nonzerocoef
colnames(data[,lasso_nonzerocoef$s0+1])

# model evaluation on lasso model using all non-zero descriptors
lasso_fittings <- predict(lasso_model, s = lambda_best_lasso, newx = x)
lasso_predictions <- predict(lasso_model, s = lambda_best_lasso, newx = x_test)
eval_results(y_test, lasso_predictions, testset)
eval_results(y_train, lasso_fittings, trainingset)

# use stepwise further select the model
data_step <- trainingset[,append(lasso_nonzerocoef$s0+1,1,0)]
mdl_null = lm(lnKD~1, data=data_step)
summary(mdl_null)

mdl_full = lm(lnKD ~ ., data=data_step)
summary(mdl_full)

mdl_stepwise = step(mdl_null, scope = formula(c(mdl_full, mdl_null)), direction="both", trace=1, criteria="BIC", k=log(40))
summary(mdl_stepwise)

predict <- predict(mdl_stepwise,newdata = testset)
fitted <- mdl_stepwise$fitted.values
eval_results(testset$lnKD,predict,testset)
eval_results(trainingset$lnKD,fitted,trainingset)

# exhaustively search for all combinations
# m = number of features in the model, data_step contains all non-zero descriptor candidates, "results" summarizes all results
data_step <- trainingset[,append(lasso_nonzerocoef$s0+1,1,0)]
m <- 3
idx <- combn(rep(1:(length(data_step)-1)),m)
results <- NULL
for (i in 1:ncol(idx)) {

  data_exhau <- data_step[,append(idx[,i]+1,1,0)]
  mdl_exhau <- lm(lnKD~.,data=data_exhau)
  
  predict <- predict(mdl_exhau,newdata = testset)
  fitted <- mdl_exhau$fitted.values
  a <- eval_results(testset$lnKD,predict,testset)
  b <- eval_results(trainingset$lnKD,fitted,trainingset)
  
  result <- data.frame(test=a, 
                       train=b
                       )
  results<- rbind(results,result)
}

# idrows find all candidates with top performance, and print out the model summary for statistical significance check
idrows <- which(results$test.Rsquare>=0.7&results$train.Rsquare>=.7)

for (val in idrows) {
  data_exhau <- data_step[,append(idx[,val]+1,1,0)]
  mdl_exhau <- lm(lnKD~.,data=data_exhau)
  s <- summary(mdl_exhau)
  print(s)
  print(val)
  cat("R2_test:", results[val,2])
 }


# loop search for opt lambda
results <- NULL
lambda <- seq(0.001,5, length=10000)
for (val in lambda) {
  lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = val,standardize = TRUE)
  summary(lasso_model)
  lasso.coef = predict(lasso_model,type="coefficients")
  
  lasso_fittings <- predict(lasso_model, s = val, newx = x_train)
  lasso_predictions <- predict(lasso_model, s = val, newx = x_test)
  R2_test <- as.matrix(eval_results(y_test, lasso_predictions, testset)[2])
  R2_train <- as.matrix(eval_results(y_train, lasso_fittings, trainingset)[2])
  idx_nonzero <- which(lasso.coef!=0)
  len <- length(idx_nonzero)
  feature <- colnames(trainingset[,idx_nonzero[-1]])
  feature <- paste0(feature,collapse = ",")
  result <- data.frame(R1=R2_test, 
                        R2=R2_train,
                        len=len-1,
                       lambda=val,
                       Variable=feature)
  results<- rbind(results,result)
}

lasso_pred <- predict(lasso,as.data.frame(x_test))
lasso_fitted <- predict(lasso,trainingset[-1])
a <- eval_results(y_test,lasso_pred, testset)
a
b <- eval_results(y,lasso_fitted, trainingset)
b


# elastic net
set.seed(1)
lambdas <- 10^seq(1, -5, length = 1000)
elas_reg <- cv.glmnet(x_train, y_train, alpha = 0.5, lambda = lambdas, standardize = TRUE, nfolds = 5)
plot(elas_reg)
lambda_best_elas <- elas_reg$lambda.min 
lambda_best_elas
elas_model <- glmnet(x_train, y_train, alpha = .5, lambda = lambda_best_elas,standardize = TRUE)
summary(elas_model)
print(elas_model)
plot(elas_model,xvar="dev",label = T)
plot(elas_model,xvar="lambda",label = T)
elas.coef  <-  predict(elas_model,type="coefficients")
elas.coef
elas.coef[elas.coef!=0] 
elas_nonzerocoef <- predict(elas_model,type="nonzero")
elas_nonzerocoef
colnames(data[,elas_nonzerocoef$s0+1])

# model evaluation
elas_fittings <- predict(elas_model, s = lambda_best_elas, newdata = x)
elas_predictions <- predict(elas_model, s = lambda_best_elas, newdata = x_test)
eval_results(y_test, elas_predictions, testset)
eval_results(y_train, elas_fittings, trainingset)
idx_nonzero <- which(elas.coef!=0)
length(idx_nonzero)
colnames(trainingset[,idx_nonzero])


# ridge regression
set.seed(1000)
lambdas <- 10^seq(2, -3, length = 1000)
ridge_reg <- cv.glmnet(x, y, alpha = 0, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best_ridge <- ridge_reg$lambda.min 
lambda_best_ridge
ridge_model <- glmnet(x, y, alpha = 0, lambda = lambda_best_ridge,standardize = TRUE)
summary(ridge_model)
ridge.coef = predict(ridge_model,type="coefficients")
ridge.coef
ridge.coef[ridge.coef!=0] 

ridge_fittings <- predict(ridge_model, s = lambda_best_ridge, newdata = x)
ridge_predictions <- predict(ridge_model, s = lambda_best_ridge, newdata = x_test)
eval_results(y_test, ridge_predictions, testset)
eval_results(y, ridge_fittings, trainingset)

# loop search for opt lambda
results <- NULL
lambda <- seq(0.001,5, length=10000)
for (val in lambda) {
  ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = val,standardize = TRUE)
  summary(ridge_model)
  ridge.coef = predict(ridge_model,type="coefficients")
  
  ridge_fittings <- predict(ridge_model, s = val, newdata = x_train)
  ridge_predictions <- predict(ridge_model, s = val, newdata = x_test)
  R2_test <- as.matrix(eval_results(y_test, ridge_predictions, testset)[2])
  R2_train <- as.matrix(eval_results(y_train, ridge_fittings, trainingset)[2])
  idx_nonzero <- which(ridge.coef!=0)
  len <- length(idx_nonzero)
  feature <- colnames(trainingset[,idx_nonzero[-1]])
  feature <- paste0(feature,collapse = ",")
  result <- data.frame(R1=R2_test, 
                       R2=R2_train, 
                       len=len-1,
                       lambda=val,
                       Variable=feature)
  results<- rbind(results,result)
}
