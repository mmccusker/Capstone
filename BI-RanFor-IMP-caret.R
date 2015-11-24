setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Pre-Step_feature_selection")
#setwd("~/Capstone/Pre-Step_feature_selection")
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
library(randomForest)
library(caTools)

train <- read.csv("train.csv")

BI <- train

set.seed(88)
spl <- sample.split(BI$Activity, SplitRatio =0.7)
working.data <- subset(BI, spl==TRUE)
fin.test.data <- subset(BI, spl==FALSE)

working.data$Activity <- as.factor(working.data$Activity)
fin.test.data$Activity <- as.factor(fin.test.data$Activity)

write.csv(fin.test.data, "Final_Test_Data.csv")
write.csv(working.data, "Working_Data.csv")


# cv and tuning with caret (mtry- # randomly selected predictors)

ctrl <- trainControl(method="cv", repeats=1, number=10)
#classProbs=TRUE (leaving this out)
RFTune <- train(Activity ~ ., data=working.data, method="rf", trControl=ctrl)
#mtry=59, acc=0.790
RFTune$bestTune


str(RFTune)
names(RFTune)
summary(RFTune)


f<-RFTune$finalModel
imp<-varImp(f)
impVar<-order(imp, decreasing=T)
write.csv(impVar, "impVar.csv")

impVar<-read.csv("impVar.csv")

impVar$X<-NULL

impVarNum <- as.vector(impVar$x)

Top100 <- impVarNum[1:100]
Top150 <- impVarNum[1:150]
Top50 <- impVarNum[1:50]
Top200 <- impVarNum[1:200]
Top250 <- impVarNum[1:250]
Top500 <- impVarNum[1:500]

work <- working.data
work$Activity <- NULL

Top100WD <-work[,Top100]
Top150WD <-work[,Top150]
Top50WD <-work[,Top50]
Top200WD <-work[,Top200]
Top250WD <-work[,Top250]
Top500WD <- work[,Top500]

Top100WD$Activity <-working.data$Activity
Top150WD$Activity <-working.data$Activity
Top50WD$Activity <-working.data$Activity
Top200WD$Activity <-working.data$Activity
Top250WD$Activity <-working.data$Activity
Top500WD$Activity <-working.data$Activity

write.csv(Top100WD, "Top100WD.csv", row.names=FALSE)
write.csv(Top150WD, "Top150WD.csv", row.names=FALSE)
write.csv(Top50WD, "Top50WD.csv", row.names=FALSE)
write.csv(Top200WD, "Top200WD.csv", row.names=FALSE)
write.csv(Top250WD, "Top250WD.csv", row.names=FALSE)
write.csv(Top500WD, "Top500WD.csv", row.names=FALSE)


# creating training and validation datasets

library(caTools)
set.seed(106)
spl <- sample.split(Top100WD$Activity, SplitRatio =0.7)
training100.data <- subset(Top100WD, spl==TRUE)
validation100.data <- subset(Top100WD, spl==FALSE)

set.seed(106)
spl <- sample.split(Top150WD$Activity, SplitRatio =0.7)
training150.data <- subset(Top150WD, spl==TRUE)
validation150.data <- subset(Top150WD, spl==FALSE)

set.seed(106)
spl <- sample.split(Top50WD$Activity, SplitRatio =0.7)
training50.data <- subset(Top50WD, spl==TRUE)
validation50.data <- subset(Top50WD, spl==FALSE)

set.seed(106)
spl <- sample.split(Top50WD$Activity, SplitRatio =0.7)
training200.data <- subset(Top200WD, spl==TRUE)
validation200.data <- subset(Top200WD, spl==FALSE)

set.seed(106)
spl <- sample.split(Top50WD$Activity, SplitRatio =0.7)
training250.data <- subset(Top250WD, spl==TRUE)
validation250.data <- subset(Top250WD, spl==FALSE)

set.seed(106)
spl <- sample.split(Top50WD$Activity, SplitRatio =0.7)
training500.data <- subset(Top500WD, spl==TRUE)
validation500.data <- subset(Top500WD, spl==FALSE)

write.csv(training100.data, "training100.csv", row.names=FALSE)
write.csv(training150.data, "training150.csv", row.names=FALSE)
write.csv(training50.data, "training50.csv", row.names=FALSE)
write.csv(training200.data, "training200.csv", row.names=FALSE)
write.csv(training250.data, "training250.csv", row.names=FALSE)
write.csv(training500.data, "training500.csv", row.names=FALSE)

write.csv(validation100.data, "validation100.csv", row.names=FALSE)
write.csv(validation150.data, "validation150.csv", row.names=FALSE)
write.csv(validation50.data, "validation50.csv", row.names=FALSE)
write.csv(validation200.data, "validation200.csv", row.names=FALSE)
write.csv(validation250.data, "validation250.csv", row.names=FALSE)
write.csv(validation500.data, "validation500.csv", row.names=FALSE)


# creating Working and Final Test Datasets

# use Importance variables from before (50 features)
# this may not be necessary if I can just use finTest for all
# without specifying features (if automatically uses same features as model)
n <- names(top50wd)
Top50.ft <- finTest[,n]
n <- names(top100wd)
Top100.ft <- finTest[,n]

