setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Pre-Step_feature_selection")
getwd()
library(caTools)

train <- read.csv("train.csv")
train$Activity <- as.factor(train$Activity)

table(train$Activity)

var.vec <-apply(train, 2, var)

hist(var.vec, xlab="Variance", ylab="Frequency",
     main="Feature Variance", col="red")

corr.values <-cor(train[,-1])
plot(corr.values)
hist(corr.values[,1:1776], xlab='Correlation coefficient',
     ylab="Frequency", main="Correlations in Feature Set", col="gray")

dim(corr.values)
1776*1775
