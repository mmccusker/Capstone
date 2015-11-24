setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Pre-Step_scaling")
getwd()
library(caTools)


# scaled data for kNN (Training and Validation Data Sets)

t50.scaled <- read.csv("t50scaled.csv")
v50.scaled <- read.csv("v50scaled.csv")
t100.scaled <- read.csv("t100scaled.csv")
v100.scaled <- read.csv("v100scaled.csv")
t150.scaled <- read.csv("t150scaled.csv")
v150.scaled <- read.csv("v150scaled.csv")
t200.scaled <- read.csv("t200scaled.csv")
v200.scaled <- read.csv("v200scaled.csv")
t250.scaled <- read.csv("t250scaled.csv")
v250.scaled <- read.csv("v250scaled.csv")
t500.scaled <- read.csv("t500scaled.csv")
v500.scaled <- read.csv("v500scaled.csv")


# scaled data for kNN (Working Data and Final Test Data)

dat.all.scaled.wd <- read.csv("dat.all.scaled.wd.csv")
dat.all.scaled.ftd <- read.csv("dat.all.scaled.ftd.csv")
dat.all.scaled.wd$Activity <- as.factor(dat.all.scaled.wd$Activity)
dat.all.scaled.ftd$Activity <- as.factor(dat.all.scaled.ftd$Activity)

top50all.scaled.wd <- read.csv("top50all.scaled.wd.csv")
top50all.scaled.ftd <- read.csv("top50all.scaled.ftd.csv")
top50all.scaled.wd$Activity <- as.factor(top50all.scaled.wd$Activity)
top50all.scaled.ftd$Activity <- as.factor(top50all.scaled.ftd$Activity)

top100all.scaled.wd <- read.csv("top100all.scaled.wd.csv")
top100all.scaled.ftd <- read.csv("top100all.scaled.ftd.csv")
top100all.scaled.wd$Activity <- as.factor(top100all.scaled.wd$Activity)
top100all.scaled.ftd$Activity <- as.factor(top100all.scaled.ftd$Activity)

top150all.scaled.wd <- read.csv("top150all.scaled.wd.csv")
top150all.scaled.ftd <- read.csv("top150all.scaled.ftd.csv")
top150all.scaled.wd$Activity <- as.factor(top150all.scaled.wd$Activity)
top150all.scaled.ftd$Activity <- as.factor(top150all.scaled.ftd$Activity)

top200all.scaled.wd <- read.csv("top200all.scaled.wd.csv")
top200all.scaled.ftd <- read.csv("top200all.scaled.ftd.csv")
top200all.scaled.wd$Activity <- as.factor(top200all.scaled.wd$Activity)
top200all.scaled.ftd$Activity <- as.factor(top200all.scaled.ftd$Activity)

top250all.scaled.wd <- read.csv("top250all.scaled.wd.csv")
top250all.scaled.ftd <- read.csv("top250all.scaled.ftd.csv")
top250all.scaled.wd$Activity <- as.factor(top250all.scaled.wd$Activity)
top250all.scaled.ftd$Activity <- as.factor(top250all.scaled.ftd$Activity)

top500all.scaled.wd <- read.csv("top500all.scaled.wd.csv")
top500all.scaled.ftd <- read.csv("top500all.scaled.ftd.csv")
top500all.scaled.wd$Activity <- as.factor(top500all.scaled.wd$Activity)
top500all.scaled.ftd$Activity <- as.factor(top500all.scaled.ftd$Activity)

