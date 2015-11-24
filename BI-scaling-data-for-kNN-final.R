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
library("e1071") # needed for SVM
library("kknn", lib.loc="~/R/win-library/3.2")
library(caTools)

# scaling for kNN (scale whole dataset)

#grabbing whole dataset

train <- read.csv("train.csv")

BI <- train
tail(str(BI))
data <- BI
dat<-data[,-1]

#scaling whole dataset

dat_scaled<-sapply(dat, scale)
dat.all.scaled<-as.data.frame(dat_scaled)
dat.all.scaled$Activity <- data$Activity
dat.all.scaled$Activity <- as.factor(dat.all.scaled$Activity)

#selecting right feature (all)

set.seed(88)
spl <- sample.split(dat.all.scaled$Activity, SplitRatio =0.7)
dat.all.scaled.wd <- subset(dat.all.scaled, spl==TRUE)
dat.all.scaled.ftd <- subset(dat.all.scaled, spl==FALSE)

write.csv(dat.all.scaled.wd, "dat.all.scaled.wd.csv", row.names=FALSE)
write.csv(dat.all.scaled.ftd, "dat.all.scaled.ftd.csv", row.names=FALSE)


#selecting right feature (top 50)
n<- names(top50wd)
top50all.scaled <- dat.all.scaled[,n]

set.seed(88)
spl <- sample.split(top50all.scaled$Activity, SplitRatio =0.7)
top50all.scaled.wd <- subset(top50all.scaled, spl==TRUE)
top50all.scaled.ftd <- subset(top50all.scaled, spl==FALSE)

write.csv(top50all.scaled.wd, "top50all.scaled.wd.csv", row.names=FALSE)
write.csv(top50all.scaled.ftd, "top50all.scaled.ftd.csv", row.names=FALSE)


#selecting right features (top 100)
n<- names(top100wd)
top100all.scaled <- dat.all.scaled[,n]

set.seed(88)
spl <- sample.split(top100all.scaled$Activity, SplitRatio =0.7)
top100all.scaled.wd <- subset(top100all.scaled, spl==TRUE)
top100all.scaled.ftd <- subset(top100all.scaled, spl==FALSE)

write.csv(top100all.scaled.wd, "top100all.scaled.wd.csv", row.names=FALSE)
write.csv(top100all.scaled.ftd, "top100all.scaled.ftd.csv", row.names=FALSE)


#selecting right features (top 150)
n<- names(top150wd)
top150all.scaled <- dat.all.scaled[,n]

set.seed(88)
spl <- sample.split(top150all.scaled$Activity, SplitRatio =0.7)
top150all.scaled.wd <- subset(top150all.scaled, spl==TRUE)
top150all.scaled.ftd <- subset(top150all.scaled, spl==FALSE)

write.csv(top150all.scaled.wd, "top150all.scaled.wd.csv", row.names=FALSE)
write.csv(top150all.scaled.ftd, "top150all.scaled.ftd.csv", row.names=FALSE)


#selecting right features (top 200)
n<- names(top200wd)
top200all.scaled <- dat.all.scaled[,n]

set.seed(88)
spl <- sample.split(top200all.scaled$Activity, SplitRatio =0.7)
top200all.scaled.wd <- subset(top200all.scaled, spl==TRUE)
top200all.scaled.ftd <- subset(top200all.scaled, spl==FALSE)

write.csv(top200all.scaled.wd, "top200all.scaled.wd.csv", row.names=FALSE)
write.csv(top200all.scaled.ftd, "top200all.scaled.ftd.csv", row.names=FALSE)


#selecting right features (top 250)
n<- names(top250wd)
top250all.scaled <- dat.all.scaled[,n]

set.seed(88)
spl <- sample.split(top250all.scaled$Activity, SplitRatio =0.7)
top250all.scaled.wd <- subset(top250all.scaled, spl==TRUE)
top250all.scaled.ftd <- subset(top250all.scaled, spl==FALSE)

write.csv(top250all.scaled.wd, "top250all.scaled.wd.csv", row.names=FALSE)
write.csv(top250all.scaled.ftd, "top250all.scaled.ftd.csv", row.names=FALSE)


#selecting right features (top 500)
n<- names(top500wd)
top500all.scaled <- dat.all.scaled[,n]

set.seed(88)
spl <- sample.split(top500all.scaled$Activity, SplitRatio =0.7)
top500all.scaled.wd <- subset(top500all.scaled, spl==TRUE)
top500all.scaled.ftd <- subset(top500all.scaled, spl==FALSE)

write.csv(top500all.scaled.wd, "top500all.scaled.wd.csv", row.names=FALSE)
write.csv(top500all.scaled.ftd, "top500all.scaled.ftd.csv", row.names=FALSE)



