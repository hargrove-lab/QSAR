setwd(path)

# load data
data <- read.csv('kon_refine.csv')
library(ggplot2)
#library(kableExtra)
#library(lattice)
#library(dplyr)
library(rms) #for VIF
library(MASS)
# EDA
# Is the distribution of the response variable normal?
# data$lnkon <- exp(data$lnkon)
ggplot(data,aes(x=lnkon)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(26),bins = 26) +
  geom_density(alpha=.25, fill="lightblue") +
  scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of lnkon",y="lnkon",x='lnkon') + 
  theme_classic() + theme(legend.position="none")
hist(data$lnkon)

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


# explore relationship b/w each descriptor with y variable
ggplot(data, aes(x=PEOE_VSA_POS,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs E_rsol",x="E_rsol",y="lnkon")

ggplot(data, aes(x=vsurf_DW12,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs PEOE_VSA.1",x="PEOE_VSA.1",y="lnkon")

ggplot(data, aes(x=vsurf_ID3,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs Q_RPC.",x="Q_RPC.",y="lnkon")

ggplot(data, aes(x=vsa_other,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs vsa_other",x="vsa_other",y="lnkon")

ggplot(data, aes(x=a_base,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs vsurf_A",x="vsurf_A",y="lnkon")

ggplot(data, aes(x=a_ICM,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs a_ICM",x="a_ICM",y="lnkon")

ggplot(data, aes(x=GCUT_PEOE_0,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs GCUT_PEOE_0",x="GCUT_PEOE_0",y="lnkon")

ggplot(data, aes(x=GCUT_SMR_2,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs GCUT_SMR_2",x="GCUT_SMR_2",y="lnkon")

ggplot(data, aes(x=SlogP_VSA3,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs SlogP_VSA3",x="SlogP_VSA3",y="lnkon")

ggplot(data, aes(x=vsurf_DD23,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs vsurf_DD23",x="vsurf_DD23",y="lnkon")

ggplot(data, aes(x=vsurf_DW12,y=lnkon))+geom_point(alpha = .5, color ="blue4")+
  geom_smooth( method = 'lm',col="red3")+theme_classic()+labs(title ="lnkon vs vsurf_DW12",x="vsurf_DW12",y="lnkon")


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



# find optimal lambda for boxcox transformation

bc <- boxcox(mdl,lambda=seq(-2,4))
best_lambda <- bc$x[which.max(bc$y)]
best_lambda
mdl_inv <- lm(formula = "((lnkon)^best_lambda-1)/best_lambda~1+E_rsol+PEOE_VSA.1+vsa_other+vsurf_DW12", 
              data = trainingset)
summary(mdl_inv)
plot(mdl_inv,which = 1:5,col=c("blue4"),cex.lab=2,cex.axis = 2)

#Now some model assessment
ggplot(trainingset,aes(x=a_base,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs a_base",x="a_base",y="Residuals")

ggplot(trainingset,aes(x=a_nN,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs a_nN",x="a_nN",y="Residuals")

ggplot(trainingset,aes(x=vsurf_DD13,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs vsurf_DD13",x="vsurf_DD13",y="Residuals")

ggplot(trainingset,aes(x=vsa_other,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs vsa_other",x="vsa_other",y="Residuals")

ggplot(trainingset,aes(x=vsurf_DW12,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs vsurf_DW12",x="vsurf_DW12",y="Residuals")

ggplot(trainingset,aes(x=PEOE_VSA_POS,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs PEOE_VSA_POS",x="PEOE_VSA_POS",y="Residuals")

ggplot(trainingset,aes(x=vsurf_ID3,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs vsurf_ID3",x="vsurf_ID3",y="Residuals")

ggplot(trainingset,aes(x=vsurf_DD23,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs vsurf_DD23",x="vsurf_DD23",y="Residuals")

ggplot(trainingset,aes(x=GCUT_PEOE_0,y=mdl$residual))+geom_point(alpha=.7,cex=4)+geom_hline(yintercept = 0,col="red3")+
  theme_classic()+labs(title = "Residuals vs GCUT_PEOE_0",x="GCUT_PEOE_0",y="Residuals")


plot(mdl,which = 1:5,col=c("blue4"))
