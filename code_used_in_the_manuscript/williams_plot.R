# Williams plot for lnKD model

# load data
setwd("Z:/Projects/Kinetics based SAR/Data/Calculation/R regression/20210907_new des")

data <- read.csv('KD_refine.csv')

# create trainingset and testset id using kenStone on Mahalanobis distance
library(prospectr)
xspace <- data[,-1]
ks <- kenStone(as.matrix(xspace), k=12, metric = "mahal",pc=0.99, .center = TRUE, .scale = FALSE)
ks$test
trainid <- ks$test
id <- seq(from = 1, to = 48, by = 1)
testid <- id[-trainid]

# assign testset and trainingset
trainingset <- data[trainid,]
testset <- data[-trainid,]
x_train <- as.matrix(trainingset[-1])
y_train <- data.matrix(trainingset[1])
x <- x_train
y <- y_train
x_test <- as.matrix(testset[-1])
y_test <- as.matrix(testset[1])

# model gonna be assessed 
mdl <- lm(formula = "lnKD~1+PEOE_VSA_POS+vsurf_DW12+vsa_other+vsurf_ID3", 
          data = trainingset)

summary(mdl)

# williams plot for lnKD
# X_train <- cbind(trainingset$PEOE_VSA_POS,trainingset$vsurf_DW12,trainingset$vsa_other,trainingset$vsurf_ID3)
# X_test <- cbind(testset$PEOE_VSA_POS,testset$vsurf_DW12,testset$vsa_other,testset$vsurf_ID3)
wp_x <- cbind(data$PEOE_VSA_POS,data$vsurf_DW12,data$vsa_other,data$vsurf_ID3)

# h_train <- diag(X_train%*%inv((t(X_train)%*%X_train))%*%t(X_train))
# h_test <- diag(X_test%*%inv((t(X_test)%*%X_test))%*%t(X_test))
h <- diag(wp_x%*%inv((t(wp_x)%*%wp_x))%*%t(wp_x))

stdres_train <- (mdl$residuals-mean(mdl$residuals))/sd(mdl$residuals)
res_test <- predict(mdl,newdata=testset)-testset$lnKD
stdres_test <- (res_test-mean(mdl$residuals))/sd(mdl$residuals)

wp_mt <- matrix(0,48,3)
wp_mt[testid,1] <-1 
# wp_mt[trainid,2] <- h_train
# wp_mt[testid,2] <- h_test
wp_mt[,2] <- h
wp_mt[trainid,3] <- stdres_train
wp_mt[testid,3] <- stdres_test

colnames(wp_mt)=c("id","hatvalue","stdres")

plot(diag(h),stdred,col=c("blue4"),pch =19,cex = 2,cex.lab=2,cex.axis = 2,xlim=c(0,0.5),ylim=c(-4,4))
  
