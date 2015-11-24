setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Pre-Step_feature_selection")
getwd()
library("knitr")
library("RSQLite")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_60')
library("rJava")
library("RWeka")
library("RWekajars")
library("SnowballC")
library("ROCR") # visualize performance of classifiers
library("caret") # for confusion matrix
library("e1071") # may be needed for caret
library(MASS) # stepwise regression
library(leaps) # all subsets regression

# doing PCA on ALL working data. 
# train and validation dataset have to have the same PCAs
# when analyze Fin.Test.Data, group process with working data 
# first to get the PCAs. Otherwise, they won't have the same PCAs.



#Selecting top 500 combination PCA/MCA elements for AIC analysis
#Running AIC on top 500 principle components

full <- glm(Activity~ ., data=Top500comb, family=binomial)
null <- glm(Activity~1, data=Top500comb, family=binomial)
stepF <- stepAIC(null, scope=list(lower=null, upper=full), 
                 direction= "forward", trace=FALSE)
stepF
summary(full)

Top500comb$Activity <- as.factor(Top500comb$Activity)


library(caTools)
set.seed(106)
spl <- sample.split(PCA500$Activity, SplitRatio =0.7)
tPCA500 <- subset(PCA500, spl==TRUE)
vPCA500 <- subset(PCA500, spl==FALSE)

write.csv(tPCA500, "tPCA500.csv", row.names=FALSE)
write.csv(vPCA500, "vPCA500.csv", row.names=FALSE)




