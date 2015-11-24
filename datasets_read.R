setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Pre-Step_feature_selection")
getwd()
library(caTools)

train <- read.csv("train.csv")
train$Activity <- as.factor(train$Activity)


WD <- read.csv("Working_Data.csv")
WD[,1] <-NULL
WD$Activity <- as.factor(WD$Activity)

finTest <- read.csv("Final_Test_Data.csv")
finTest$Activity <- as.factor(finTest$Activity)

# Files needed for training and validation datasets

library(caTools)
set.seed(106)
spl <- sample.split(WD$Activity, SplitRatio =0.7)
tWD <- subset(WD, spl==TRUE)
vWD <- subset(WD, spl==FALSE)

write.csv(tWD, "tWD.csv", row.names=FALSE)
write.csv(vWD, "vWD.csv", row.names=FALSE)

tWD<- read.csv("tWD.csv")
vWD <- read.csv("vWD.csv")
tWD$Activity <- as.factor(tWD$Activity)
vWD$Activity <- as.factor(vWD$Activity)

tr.50<- read.csv("training50.csv")
v.50 <- read.csv("validation50.csv")
tr.50$Activity <- as.factor(tr.50$Activity)
v.50$Activity <- as.factor(v.50$Activity)

tr.100<- read.csv("training100.csv")
v.100 <- read.csv("validation100.csv")
tr.100$Activity <- as.factor(tr.100$Activity)
v.100$Activity <- as.factor(v.100$Activity)

tr.150<- read.csv("training150.csv")
v.150 <- read.csv("validation150.csv")
tr.150$Activity <- as.factor(tr.150$Activity)
v.150$Activity <- as.factor(v.150$Activity)

tr.200<- read.csv("training200.csv")
v.200 <- read.csv("validation200.csv")
tr.200$Activity <- as.factor(tr.200$Activity)
v.200$Activity <- as.factor(v.200$Activity)

tr.250<- read.csv("training250.csv")
v.250 <- read.csv("validation250.csv")
tr.250$Activity <- as.factor(tr.250$Activity)
v.250$Activity <- as.factor(v.250$Activity)

t500 <- read.csv("training500.csv")
v500 <- read.csv("validation500.csv")
t500$Activity <- as.factor(t500$Activity)
v500$Activity <- as.factor(v500$Activity)

tpca.100<- read.csv("training100pca.csv")
vpca.100 <- read.csv("validation100pca.csv")
tpca.100$Activity <- as.factor(tpca.100$Activity)
vpca.100$Activity <- as.factor(vpca.100$Activity)

tpca.200<- read.csv("training200pca.csv")
vpca.200 <- read.csv("validation200pca.csv")
tpca.200$Activity <- as.factor(tpca.200$Activity)
vpca.200$Activity <- as.factor(vpca.200$Activity)

tPCA500 <- read.csv("tPCA500.csv")
vPCA500 <- read.csv("vPCA500.csv")
tPCA500$Activity <- as.factor(tPCA500$Activity)
vPCA500$Activity <- as.factor(vPCA500$Activity)

### TRAINING AND VALIDATION PCA/MCA COMBINATION DATASETS

tcomb.100<- read.csv("training100comb.csv")
vcomb.100 <- read.csv("validation100comb.csv")
tcomb.100$Activity <- as.factor(tcomb.100$Activity)
vcomb.100$Activity <- as.factor(vcomb.100$Activity)

tcomb.200<- read.csv("training200comb.csv")
vcomb.200 <- read.csv("validation200comb.csv")
tcomb.200$Activity <- as.factor(tcomb.200$Activity)
vcomb.200$Activity <- as.factor(vcomb.200$Activity)

tcomb500 <- read.csv("training500comb.csv")
vcomb500 <- read.csv("validation500comb.csv")
tcomb500$Activity <- as.factor(tcomb500$Activity)
vcomb500$Activity <- as.factor(vcomb500$Activity)

# WD

top50wd <- read.csv("Top50WD.csv")
top50wd$Activity <- as.factor(top50wd$Activity)
top100wd <- read.csv("Top100WD.csv")
top100wd$Activity <- as.factor(top100wd$Activity)
top150wd <- read.csv("Top150WD.csv")
top150wd$Activity <- as.factor(top150wd$Activity)
top200wd <- read.csv("Top200WD.csv")
top200wd$Activity <- as.factor(top200wd$Activity)
top250wd <- read.csv("Top250WD.csv")
top250wd$Activity <- as.factor(top250wd$Activity)
top500wd <- read.csv("Top500WD.csv")
top500wd$Activity <- as.factor(top500wd$Activity)

# PCA files (Working Data and Final Data Set)

PCAall500.wd <- read.csv("PCAall500.wd.csv")
PCAall500.ftd <- read.csv("PCAall500.ftd.csv")
PCAall500.wd$Activity <- as.factor(PCAall500.wd$Activity)
PCAall500.ftd$Activity <- as.factor(PCAall500.ftd$Activity)

Top100pc.wd <- read.csv("Top100pc.wd.csv")
Top100pc.ftd <- read.csv("Top100pc.ftd.csv")
Top100pc.wd$Activity <- as.factor(Top100pc.wd$Activity)
Top100pc.ftd$Activity <- as.factor(Top100pc.ftd$Activity)

Top200pc.wd <- read.csv("Top200pc.wd.csv")
Top200pc.ftd <- read.csv("Top200pc.ftd.csv")
Top200pc.wd$Activity <- as.factor(Top200pc.wd$Activity)
Top200pc.ftd$Activity <- as.factor(Top200pc.ftd$Activity)


# PCA/MCA COMBINATION files (Working Data and Final Data Set) 


Top100comb.wd <- read.csv("Top100comb.wd.csv")
Top100comb.ftd <- read.csv("Top100comb.ftd.csv")
Top100comb.wd$Activity <- as.factor(Top100comb.wd$Activity)
Top100comb.ftd$Activity <- as.factor(Top100comb.ftd$Activity)

Top200comb.wd <- read.csv("Top200comb.wd.csv")
Top200comb.ftd <- read.csv("Top200comb.ftd.csv")
Top200comb.wd$Activity <- as.factor(Top200comb.wd$Activity)
Top200comb.ftd$Activity <- as.factor(Top200comb.ftd$Activity)

Top500comb.wd <- read.csv("Top500comb.wd.csv")
Top500comb.ftd <- read.csv("Top500comb.ftd.csv")
Top500comb.wd$Activity <- as.factor(Top500comb.wd$Activity)
Top500comb.ftd$Activity <- as.factor(Top500comb.ftd$Activity)


library(RSQLite)
sqlite <- dbDriver("SQLite")
#m_con <- dbConnect(sqlite, dbname=":memory:")
m_con <- dbConnect(sqlite, dbname="my_db.sqlite3")
dbWriteTable(m_con, "WorkingData", WD)
dbReadTable(m_con, "WorkingData")
dbWriteTable(m_con, "Train", train)
dbWriteTable(m_con, "FinalTestData", finTest)