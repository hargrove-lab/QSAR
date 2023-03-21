setwd(path)

# load data
data <- read.csv('kon_refine.csv')
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

# model gonna be assessed 
mdl <- lm(formula = "lnkon~1+GCUT_PEOE_0+vsurf_DW12+vsa_other+vsurf_DD23", 
          data = trainingset)
mdl <- lm(formula = "lnkon~1+PEOE_VSA_POS+vsurf_DW12+vsa_other+vsurf_ID3", 
          data = trainingset)
mdl <- lm(formula = "lnkon~1+a_base+a_nN+vsurf_DD13", 
          data = trainingset)
summary(mdl)
plot(mdl,which = 1:5,col=c("blue4"),pch =19,cex = 2,cex.lab=2,cex.axis = 2)

# plot qq plot

qqnorm(mdl$residuals, pch = 19,cex = 2.5,col="blue")
qqline(mdl$residuals, col = "black", lwd = 3,lty = 2)
