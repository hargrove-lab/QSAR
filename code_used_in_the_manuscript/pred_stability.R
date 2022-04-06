setwd("Z:/Projects/Kinetics based SAR/Data/Calculation/R regression/20210907_new des")


# load data
data <- read.csv('KD_refine.csv')

# Creat the evaluation function: eval_results, which contains RMSE and Rsquare
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



# randomize the data splitting 100 times (36:12)
results <- NULL
for (i in 1:100){
  set.seed(i)
  testid <- sample(seq_len(nrow(data)),size=12)
  
  # assign testset and trainingset
  trainingset <- data[-testid,]
  testset <- data[testid,]
  x_train <- as.matrix(trainingset[-1])
  y_train <- data.matrix(trainingset[1])
  x <- x_train
  y <- y_train
  x_test <- as.matrix(testset[-1])
  y_test <- as.matrix(testset[1])
    mdl <- lm(formula = "lnKD~1+PEOE_VSA_POS+vsa_other+vsurf_DW12+vsurf_ID3", 
            data = trainingset) # using the same descriptors to build the model
  
  predict <- predict(mdl,newdata = testset)
  fitted <- mdl$fitted.values
  a <- eval_results(testset$lnKD,predict,testset)
  b <- eval_results(trainingset$lnKD,fitted,trainingset)
  result <- data.frame(test=a, 
                       train=b
  )
  results<- rbind(results,result)
}

# plot
barplot(results$test.Rsquare,xlim = c(0,i*1.2),ylim=c(-.5,1.5),lwd=3)
abline(h=mean(results$test.Rsquare), col ="Red",lwd = 5,xlim=c(0,i))
text(x = c(0.1*i,0.3*i,0.4*i,0.5*i),
     y = c(1.2,1.2,1.2,1.2),cex = 1.5,
     labels = c("R2_test = ", round(mean(results$test.Rsquare),2), "+/-", round(sd(results$test.Rsquare),2)))

barplot(results$train.Rsquare,xlim = c(0,i*1.2),ylim=c(0,1),lwd=3)
abline(h=mean(results$train.Rsquare), col ="Red",lwd = 5,xlim=c(0,i))
text(x = c(0.1*i,0.3*i,0.4*i,0.5*i),
     y = c(0.9,0.9,0.9,0.9),cex = 1.5,
     labels = c("R2_train = ", round(mean(results$train.Rsquare),2), "+/-", round(sd(results$train.Rsquare),2)))
