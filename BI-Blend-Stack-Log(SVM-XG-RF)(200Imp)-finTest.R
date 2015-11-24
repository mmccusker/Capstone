setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Step_3_combining")
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
library(kernlab) # for svm with caret
library(ada)
library(caTools)
library(adabag) 
library(plyr) 
library(randomForest)  
library(xgboost)

set.seed(88)
spl <- sample.split(top200wd$Activity, SplitRatio =0.5)
WD200.a <- subset(top200wd, spl==TRUE)
WD200.b <- subset(top200wd, spl==FALSE)

############## SVM ####################
#tuning parameters with CV in caret (200 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(sigma=0.01,
                    C= 2.5)
svm.tune<- train(Activity ~ ., data=WD200.a,
                 method="svmRadial",
                 trControl=ctrl,
                 tuneGrid = grid)
svm.tune$bestTune
# predicting on validation dataset
svm.predict.WDb <- predict(svm.tune, newdata=WD200.b) 
sum(svm.predict.WDb == WD200.b$Activity)/length(svm.predict.WDb)
svm.predict.ftd <- predict(svm.tune, newdata=finTest) 
sum(svm.predict.ftd == finTest$Activity)/length(svm.predict.ftd)


#XG
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(nrounds=c(500, 100),
                    max_depth= c(10, 15, 20),
                    eta = 0.3)
xgb.tune<- train(Activity ~ ., 
                 data=WD200.a,
                 method="xgbTree",
                 trControl=ctrl,
                 tuneGrid = grid)
xgb.tune$bestTune
xgb.tune
# predicting on validation dataset
xgb.predict.b <- predict(xgb.tune, newdata=WD200.b) 
sum(xgb.predict.b == WD200.b$Activity)/length(xgb.predict.b)
xgb.predict.ftd <- predict(xgb.tune, newdata=finTest) 
sum(xgb.predict.ftd == finTest$Activity)/length(xgb.predict.ftd)

############# RF ###################
# cv and tuning with caret (200 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=WD200.a, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset
RF.predict.WDb <- predict(RFTune, newdata=WD200.b) 
sum(RF.predict.WDb == WD200.b$Activity)/length(RF.predict.WDb)
RF.predict.ftd <- predict(RFTune, newdata=finTest) 
sum(RF.predict.ftd == finTest$Activity)/length(RF.predict.ftd)

########LogReg for CV ###################
# predict on validation data (200 features)
model<-glm(formula = Activity ~ ., family = "binomial", 
           data = WD200.a)
predictGLM.b = predict(model, newdata=WD200.b, type="response")
t <- table(WD200.b$Activity, predictGLM.b > 0.5)
(t[1,1]+t[2,2])/(sum(t))
predictGLM.ftd = predict(model, newdata=finTest, type="response")
t <- table(finTest$Activity, predictGLM.ftd > 0.5)
(t[1,1]+t[2,2])/(sum(t))


#predictions as features 

WD200.b$svm <- svm.predict.b
WD200.b$xgb <- xgb.predict.b
WD200.b$RF <- RF.predict.b
WD200.b$glm <- predictGLM.b


finTest$svm <- svm.predict.ftd
finTest$xgb <- xgb.predict.ftd
finTest$RF <- RF.predict.ftd
finTest$glm <- predictGLM.ftd

# running the augmented files with Log Reg
##################################################################
model<-glm(Activity ~ ., family = "binomial", 
           data = WD200.b)
predictGLM.final = predict(model, newdata=finTest, type="response")
t <- table(finTest$Activity, predictGLM.final > 0.5)
(t[1,1]+t[2,2])/(sum(t))

# running augmented files with RF
##########################################################
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=WD200.b, method="rf", trControl=ctrl, tuneGrid=grid)
RF.predict.final <- predict(RFTune, newdata=finTest) 
sum(RF.predict.final == finTest$Activity)/length(RF.predict.final)


WD200.b$glm <- NULL
# running augmented files with RF (withoout LR)
##########################################################
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=WD200.b, method="rf", trControl=ctrl, tuneGrid=grid)
RF.predict.final <- predict(RFTune, newdata=finTest) 
sum(RF.predict.final == finTest$Activity)/length(RF.predict.final)


######### Majority rule- Blending #####################

#Logistic regression
model<-glm(formula = Activity ~ ., family = "binomial", 
           data = top200wd)
predictGLM = predict(model, newdata=finTest, type="response")
t <- table(finTest$Activity, predictGLM > 0.5)
(t[1,1]+t[2,2])/(sum(t))

#RF
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=top200wd, method="rf", trControl=ctrl, tuneGrid=grid)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)

#XG
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(nrounds=c(500, 100),
                    max_depth= c(10, 15, 20),
                    eta = 0.3)
xgb.tune<- train(Activity ~ ., 
                 data=top200wd,
                 method="xgbTree",
                 trControl=ctrl,
                 tuneGrid = grid)
xgb.tune$bestTune
xgb.tune
# predicting on validation dataset
xgb.predict <- predict(xgb.tune, newdata=finTest) 
sum(xgb.predict == finTest$Activity)/length(xgb.predict)

#SVM
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(sigma=c(0.01, 0.03, 0.05),
                    C= 1)
svm.tune<- train(Activity ~ ., data=top200wd,
                 method="svmRadial",
                 trControl=ctrl,
                 tuneGrid = grid)
svm.predict <- predict(svm.tune, newdata=finTest) 
sum(svm.predict == finTest$Activity)/length(svm.predict)


#from blog- combining predictions with equal weights
svm_R <- as.numeric(svm.predict)-1
xgb_R <- as.numeric(xgb.predict)-1
RF_R <- as.numeric(RF.predict)-1
glm_R <- as.numeric(predictGLM)


predictions<-(svm_R + xgb_R + RF_R)/3
t<- table(predictions > 0.5, finTest$Activity)
(t[1,1]+t[2,2])/sum(t)

predictions<-(svm_R + glm_R + RF_R)/3
t<- table(predictions > 0.5, finTest$Activity)
(t[1,1]+t[2,2])/sum(t)



