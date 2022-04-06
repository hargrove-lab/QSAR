# load data
setwd("Z:/Projects/Kinetics based SAR/Data/Calculation/R regression/20210907_new des")

data <- read.csv('kon_refine.csv')

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


# create trainingset and testset id using kenStone on euclidian distance
library(prospectr)
xspace <- data[,-1]
ks <- kenStone(as.matrix(xspace), k=40, metric = "euclid",  .center = TRUE, .scale = FALSE)
ks$test
testid <- ks$test

# assign testset and trainingset
trainingset <- data[-testid,]
testset <- data[testid,]

x_train <- as.matrix(trainingset[-1])
y_train <- data.matrix(trainingset[1])
x <- x_train
y <- y_train
x_test <- as.matrix(testset[-1])
y_test <- as.matrix(testset[1])

# y scrambling

results <- NULL
for (i in 1:500){
  set.seed(i)
  idx_sc <- sample(1:40,40)
  y_sc <- trainingset[,1][idx_sc]
  trainingset_sc <- cbind(y_sc,trainingset[,-1])
  mdl_sc <- lm(formula = "y_sc~1+E_rsol+PEOE_VSA.1+Q_RPC.+vsa_other+vsurf_A", 
               data = trainingset_sc)
  predict <- predict(mdl_sc,newdata = testset)
  fitted <- mdl_sc$fitted.values
  a <- eval_results(testset$lnkon,predict,testset)
  b <- eval_results(trainingset_sc$y_sc,fitted,trainingset_sc)
  
  result <- data.frame(test=a, 
                       train=b
  )
  results<- rbind(results,result)
}

plot(results$train.Rsquare,ylim=c(0,1),main="Y-scrambling",xlab = "index", ylab = "R2_train",pch = 16,col = "black",cex = 2,cex.axis = 2)
points(x = 501, y = 0.72, col = "Red", pch = 19,cex = 3)
box (lwd =2 )
text(x = 460,
     y = 0.72,
     labels = c("R2_train = 0.79"),cex = 1.5)
