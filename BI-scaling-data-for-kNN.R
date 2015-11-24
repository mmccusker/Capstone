setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Step_1_all_classifiers")
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

# scaling for kNN (scale training data)

#50 features
dat <- top50wd
dat$Activity <- NULL
dat_scaled<-sapply(dat, scale)
dat.scaled<-as.data.frame(dat_scaled)
dat.scaled$Activity <- top50wd$Activity
dat.scaled$Activity <- as.factor(dat.scaled$Activity)

library(caTools)
set.seed(106)
spl <- sample.split(dat.scaled$Activity, SplitRatio =0.7)
t50.scaled <- subset(dat.scaled, spl==TRUE)
v50.scaled <- subset(dat.scaled, spl==FALSE)

#100 features
dat <- top100wd
dat$Activity <- NULL
dat_scaled<-sapply(dat, scale)
dat.scaled<-as.data.frame(dat_scaled)
dat.scaled$Activity <- top50wd$Activity
dat.scaled$Activity <- as.factor(dat.scaled$Activity)

library(caTools)
set.seed(106)
spl <- sample.split(dat.scaled$Activity, SplitRatio =0.7)
t100.scaled <- subset(dat.scaled, spl==TRUE)
v100.scaled <- subset(dat.scaled, spl==FALSE)

# 150 features
dat <- top150wd
dat$Activity <- NULL
dat_scaled<-sapply(dat, scale)
dat.scaled<-as.data.frame(dat_scaled)
dat.scaled$Activity <- top50wd$Activity
dat.scaled$Activity <- as.factor(dat.scaled$Activity)

library(caTools)
set.seed(106)
spl <- sample.split(dat.scaled$Activity, SplitRatio =0.7)
t150.scaled <- subset(dat.scaled, spl==TRUE)
v150.scaled <- subset(dat.scaled, spl==FALSE)

# 200 features
dat <- top200wd
dat$Activity <- NULL
dat_scaled<-sapply(dat, scale)
dat.scaled<-as.data.frame(dat_scaled)
dat.scaled$Activity <- top200wd$Activity
dat.scaled$Activity <- as.factor(dat.scaled$Activity)

library(caTools)
set.seed(106)
spl <- sample.split(dat.scaled$Activity, SplitRatio =0.7)
t200.scaled <- subset(dat.scaled, spl==TRUE)
v200.scaled <- subset(dat.scaled, spl==FALSE)


# 250 features
dat <- top250wd
dat$Activity <- NULL
dat_scaled<-sapply(dat, scale)
dat.scaled<-as.data.frame(dat_scaled)
dat.scaled$Activity <- top200wd$Activity
dat.scaled$Activity <- as.factor(dat.scaled$Activity)

library(caTools)
set.seed(106)
spl <- sample.split(dat.scaled$Activity, SplitRatio =0.7)
t250.scaled <- subset(dat.scaled, spl==TRUE)
v250.scaled <- subset(dat.scaled, spl==FALSE)

# 500 features
dat <- top500wd
dat$Activity <- NULL
dat_scaled<-sapply(dat, scale)
dat.scaled<-as.data.frame(dat_scaled)
dat.scaled$Activity <- top500wd$Activity
dat.scaled$Activity <- as.factor(dat.scaled$Activity)

library(caTools)
set.seed(106)
spl <- sample.split(dat.scaled$Activity, SplitRatio =0.7)
t500.scaled <- subset(dat.scaled, spl==TRUE)
v500.scaled <- subset(dat.scaled, spl==FALSE)

#writing to files
write.csv(t50.scaled, "t50scaled.csv", row.names=FALSE)
write.csv(v50.scaled, "v50scaled.csv", row.names=FALSE)
write.csv(t100.scaled, "t100scaled.csv", row.names=FALSE)
write.csv(v100.scaled, "v100scaled.csv", row.names=FALSE)
write.csv(t150.scaled, "t150scaled.csv", row.names=FALSE)
write.csv(v150.scaled, "v150scaled.csv", row.names=FALSE)
write.csv(t200.scaled, "t200scaled.csv", row.names=FALSE)
write.csv(v200.scaled, "v200scaled.csv", row.names=FALSE)
write.csv(t250.scaled, "t250scaled.csv", row.names=FALSE)
write.csv(v250.scaled, "v250scaled.csv", row.names=FALSE)
write.csv(t500.scaled, "t500scaled.csv", row.names=FALSE)
write.csv(v500.scaled, "v500scaled.csv", row.names=FALSE)

