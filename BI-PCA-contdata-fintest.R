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

binary.data <- apply(train,2,function(x) { all(na.omit(x) %in% 0:1) })
train.b <- train[binary.data]
train.c <- train[!binary.data]

data<- train.c

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



PCA.train.c <- matPC
PCA.train.c <- as.data.frame(PCA.train.c)

comb.df <- data.frame(PCA.train.c[,1:400], coord.df[,1:400])
comb.df$Activity <- train$Activity

# combined PCA/MCA of entire dataset ("train" from Kaggle
# now select right features and divide into WD and fintest

feat.100 <- names(tcomb.100)
feat.200 <- names(tcomb.200)
feat.500 <- names(tcomb500)

Top100comb.all <- comb.df[feat.100]
Top200comb.all <- comb.df[feat.200]
Top500comb.all <- comb.df[feat.500]

write.csv(Top100comb.all, "Top100combAll.csv", row.names=FALSE)
write.csv(Top200comb.all, "Top200combAll.csv", row.names=FALSE)
write.csv(Top500comb.all, "Top500combAll.csv", row.names=FALSE)


# working, final test data COMBINATION DATA

library(caTools)
set.seed(88)
spl <- sample.split(Top100comb.all$Activity, SplitRatio =0.7)
Top100comb.wd <- subset(Top100comb.all, spl==TRUE)
Top100comb.ftd <- subset(Top100comb.all, spl==FALSE)

Top100comb.wd$Activity <- as.factor(Top100comb.wd$Activity)
Top100comb.ftd$Activity <- as.factor(Top100comb.ftd$Activity)


set.seed(88)
spl <- sample.split(Top200comb$Activity, SplitRatio =0.7)
Top200comb.wd <- subset(Top200comb.all, spl==TRUE)
Top200comb.ftd <- subset(Top200comb.all, spl==FALSE)

Top200comb.wd$Activity <- as.factor(Top200comb.wd$Activity)
Top200comb.ftd$Activity <- as.factor(Top200comb.ftd$Activity)

set.seed(88)
spl <- sample.split(Top500comb$Activity, SplitRatio =0.7)
Top500comb.wd <- subset(Top500comb.all, spl==TRUE)
Top500comb.ftd <- subset(Top500comb.all, spl==FALSE)

Top500comb.wd$Activity <- as.factor(Top500comb.wd$Activity)
Top500comb.ftd$Activity <- as.factor(Top500comb.ftd$Activity)



write.csv(Top100comb.wd, "Top100comb.wd.csv", row.names=FALSE)
write.csv(Top100comb.ftd, "Top100comb.ftd.csv", row.names=FALSE)
write.csv(Top200comb.wd, "Top200comb.wd.csv", row.names=FALSE)
write.csv(Top200comb.ftd, "Top200comb.ftd.csv", row.names=FALSE)
write.csv(Top500comb.wd, "Top500comb.wd.csv", row.names=FALSE)
write.csv(Top500comb.ftd, "Top500comb.ftd.csv", row.names=FALSE)


comb.feat.100 <- names(training100comb)
sort(comb.feat.100)
comb.feat.200 <- names(training200comb)
sort(comb.feat.200)
comb.feat.500 <- names(training500comb)
sort(comb.feat.500)

