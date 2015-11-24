setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Pre-Step_feature_selection")
#setwd("~/Boehringer-Ingelheim/Pre-Step_feature_selection")
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
library(caret) # experiment design
library(MASS) # stepwise regression
library(leaps) # all subsets regression


# doing PCA on ALL working data. 
# train and validation dataset have to have the same PCAs
# when analyze Fin.Test.Data, group process with working data 
# first to get the PCAs. Otherwise, they won't have the same PCAs.

binary.data <- apply(WD,2,function(x) { all(na.omit(x) %in% 0:1) })
WD.b <- WD[binary.data]
WD.c <- WD[!binary.data]

data<- WD.c

# removing min variance columns

var1<-sapply(data, var)
hist(var1, breaks=100)

sorted.var <-sort(var1, decreasing=FALSE)
head(sorted.var)

# PCA with scaling

pr <- prcomp(data, scale = TRUE)
vars <- apply(pr$x, 2, var) 
props <- vars / sum(vars)
cp<- cumsum(props)
plot(cp, type="l")

str(pr)

class(pr$x)
matPC <- pr$x


#Running a logistic regression with prcomps (accounting for 80% of variance)
PCA.WD.c <- matPC
PCA.WD.c <- as.data.frame(PCA.WD.c)

comb.df <- data.frame(PCA.WD.c[,1:400], coord.df[,1:400])
comb.df$Activity <- WD$Activity




# now run RF Importance on principle components!
# cv and tuning with caret

ctrl <- trainControl(method="cv", repeats=1, number=10)
#classProbs=TRUE (leaving this out)
RFTune <- train(Activity ~ ., data=comb.df, method="rf", trControl=ctrl)


RFTune$bestTune


f<-RFTune$finalModel
imp.comb<-f$importance


sorted.imp.comb<-sort(imp.comb, decreasing=T)


comb.df$Activity <- NULL
ordered.imp <-order(imp.comb, decreasing=T)
ordered.features<-comb.df[ordered.imp]


Top100comb <- ordered.features[,1:100]
Top200comb <- ordered.features[,1:200]
Top500comb <- ordered.features[,1:500]

Top100comb$Activity <- WD$Activity
Top200comb$Activity <- WD$Activity
Top500comb$Activity <- WD$Activity

write.csv(Top100comb, "Top100comb.csv", row.names=FALSE)
write.csv(Top200comb, "Top200comb.csv", row.names=FALSE)
write.csv(Top500comb, "Top500comb.csv", row.names=FALSE)

Top500comb <- read.csv("Top500comb.csv")

library(caTools)
set.seed(106)
spl <- sample.split(Top100comb$Activity, SplitRatio =0.7)
training100comb <- subset(Top100comb, spl==TRUE)
validation100comb <- subset(Top100comb, spl==FALSE)

set.seed(106)
spl <- sample.split(Top200comb$Activity, SplitRatio =0.7)
training200comb <- subset(Top200comb, spl==TRUE)
validation200comb <- subset(Top200comb, spl==FALSE)

set.seed(106)
spl <- sample.split(Top500comb$Activity, SplitRatio =0.7)
training500comb <- subset(Top500comb, spl==TRUE)
validation500comb <- subset(Top500comb, spl==FALSE)

write.csv(training100comb, "training100comb.csv", row.names=FALSE)
write.csv(training200comb, "training200comb.csv", row.names=FALSE)
write.csv(training500comb, "training500comb.csv", row.names=FALSE)
write.csv(validation100comb, "validation100comb.csv", row.names=FALSE)


comb.feat.100 <- names(training100comb)
sort(comb.feat.100)
comb.feat.200 <- names(training200comb)
sort(comb.feat.200)
comb.feat.500 <- names(training500comb)
sort(comb.feat.500)

