setwd("Z:/Projects/Kinetics based SAR/Data/Calculation/R regression/20210907_new des")

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

# build a tree
library(tree)
tree.KD <- tree(lnKD~.,data,subset = trainid)
plot(tree.KD)
text(tree.KD)

# evaluate the prediction and fitting
pred_KD <-predict(tree.KD,newdata = testset)
fitted <- predict(tree.KD,,newdata = trainingset)
eval_results(testset$lnKD,pred_KD,testset)
eval_results(trainingset$lnKD,fitted,trainingset)

# use CV to select best size
set.seed(1)
tree.KD_cv <- cv.tree(tree.KD)
plot(tree.KD_cv$size,tree.KD_cv$dev,type = 'b')
prune_KD <- prune.tree(tree.KD,best =6)
pred_KD <-predict(prune_KD,newdata = testset)
plot(prune_KD)
text(prune_KD)
fitted <- predict(prune_KD,,newdata = trainingset)
eval_results(testset$lnKD,pred_KD,testset)
eval_results(trainingset$lnKD,fitted,trainingset)

# bagging: set mtry = 193 in randomForest method
library(randomForest)
set.seed(1)
rf_KD <- randomForest(lnKD~.,data = trainingset,importance = TRUE,ntree = 200,sampsize=24, mtry = 193)
summary(rf_KD)
print(rf_KD)

# plot
plot(rf_KD,main ="Averaged OOB error", cex.lab=2,cex.axis=2,cex.main=2, lwd=4, col = "red")
pred_KD <-predict(rf_KD,newdata = testset)
fitted <- predict(rf_KD,,newdata = trainingset)
eval_results(testset$lnKD,pred_KD,testset)
eval_results(trainingset$lnKD,fitted,trainingset)
varImpPlot(rf_KD,main = "Variable importance plot")


# random forest 
library(randomForest)
set.seed(1)
rf_KD <- randomForest(lnKD~.,data = trainingset,importance = TRUE,sampsize = 34,ntree = 100,mty=40)
summary(rf_KD)
print(rf_KD)
plot(rf_KD,main ="Averaged OOB error", cex.lab=2,cex.axis=2,cex.main=2, lwd=4, col = "red")
pred_KD <-predict(rf_KD,newdata = testset)
fitted <- predict(rf_KD,,newdata = trainingset)
eval_results(testset$lnKD,pred_KD,testset)
eval_results(trainingset$lnKD,fitted,trainingset)
varImpPlot(rf_KD,main = "Variable importance plot")

# plot
predict <- predict(rf_KD,newdata = data)
id  <-  numeric(48)
id[-trainid] <- 1
data_plot <- cbind(predict,data$lnKD,id)
colnames(data_plot) <- c("predict", "obs","id")

ggplot(as.data.frame(data_plot), aes(x=obs,y=predict))+
  ggtitle(expression("Baseline model of lnK"[D]*"")) +
  xlab(expression("Observed lnK"[D]*"")) + ylab(expression("Predicted lnK"[D]*""))+
  # THE DATA POINT
  geom_point(aes(color = factor(id)),size = 5,alpha =1) +
  xlim(min(data$lnKD)-2,max(data$lnKD)+2)+
  ylim(min(data$lnKD)-2,max(data$lnKD)+2)+
  scale_color_manual(labels = c("Training set", "Test set"), values = c("dodgerblue", "red2"))+
  
  # title
  theme_bw()+
  theme(axis.ticks.length=unit(.4,"lines"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(size = 20), 
        axis.text.x = element_text(size=20), 
        axis.title = element_text(size = 25,face = 'bold'),title =element_text(size = 25,face = 'bold') )+
  # legend  
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(colour="black", size=20, face="bold"))+
  theme(legend.position = c(0.80, 0.1))+
  # rec
  theme(panel.background = element_rect(colour = "black", size = 3.5))+
  # ref line  
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size=1.5)

ggsave("rfmdl.tiff", units="in", width=8, height=8, dpi=600)

# boosting using GBM in r
library(gbm)
set.seed(1)
boost_KD <- gbm(lnKD~.,data=trainingset,distribution = 'gaussian',n.trees=2000,interaction.depth=1,
                shrinkage = 0.01,cv.folds = 5,verbose = TRUE,n.minobsinnode=4,bag.fraction = 0.5 )
summary(boost_KD)
print(boost_KD)
sqrt(min(boost_KD$cv.error))
gbm.perf(boost_KD, method = "cv")
legend(1200, .5, c("OOB(Out Of Bag estimator method)", "CV(Cross Valdation method)"), cex=0.8, col=c("black", "green"),  lty=1)
pred_KD <-predict(boost_KD,newdata = testset,n.trees = 990)
fitted <- predict(boost_KD,newdata = trainingset,n.trees =990)
eval_results(testset$lnKD,pred_KD,testset)
eval_results(trainingset$lnKD,fitted,trainingset)

# plot
predict <- predict(boost_KD,newdata = data,ntrees =500)
id  <-  numeric(48)
id[-trainid] <- 1
data_plot <- cbind(predict,data$lnKD,id)
colnames(data_plot) <- c("predict", "obs","id")

ggplot(as.data.frame(data_plot), aes(x=obs,y=predict))+
  ggtitle(expression("Baseline model of lnK"[D]*"")) +
  xlab(expression("Observed lnK"[D]*"")) + ylab(expression("Predicted lnK"[D]*""))+
  # THE DATA POINT
  geom_point(aes(color = factor(id)),size = 5,alpha =1) +
  xlim(min(data$lnKD)-2,max(data$lnKD)+2)+
  ylim(min(data$lnKD)-2,max(data$lnKD)+2)+
  scale_color_manual(labels = c("Training set", "Test set"), values = c("dodgerblue", "red2"))+
  
  # title
  theme_bw()+
  theme(axis.ticks.length=unit(.4,"lines"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(axis.text.y = element_text(size = 20), 
        axis.text.x = element_text(size=20), 
        axis.title = element_text(size = 25,face = 'bold'),title =element_text(size = 25,face = 'bold') )+
  # legend  
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(colour="black", size=20, face="bold"))+
  theme(legend.position = c(0.80, 0.1))+
  # rec
  theme(panel.background = element_rect(colour = "black", size = 3.5))+
  # ref line  
  geom_abline(intercept = 0, slope = 1, color="black", 
              linetype="dashed", size=1.5)

ggsave("gbmmdl.tiff", units="in", width=8, height=8, dpi=600)

