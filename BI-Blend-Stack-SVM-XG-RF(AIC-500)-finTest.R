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
spl <- sample.split(top500wd$Activity, SplitRatio =0.5)
top500.a <- subset(top500wd, spl==TRUE)
top500.b <- subset(top500wd, spl==FALSE)


############ SVM ###############
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(sigma=c(0, 0.01, 0.03, 0.05),
                    C= c(0, 1, 2))
svm.tune<- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                   D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                   D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                   D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                   D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                   D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                   D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                   D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                   D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                   D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                   D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                   D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                   D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                   D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                   D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                   D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143, 
                 data = top500.a, 
                 method="svmRadial",
                 trControl=ctrl,
                 tuneGrid = grid)
svm.tune$bestTune
svm.tune$finalModel
svm.tune
# predicting on validation dataset (AIC on top 500 features)
svm.predict.b <- predict(svm.tune, newdata=top500.b) 
sum(svm.predict.b == top500.b$Activity)/length(svm.predict.b)
svm.predict.ftd <- predict(svm.tune, newdata=finTest) 
sum(svm.predict.ftd == finTest$Activity)/length(svm.predict.ftd)

##################### XG #####################
#tuning parameters with CV in caret (AIC on top500 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(nrounds=c(500, 100),
                    max_depth= c(10, 15, 20),
                    eta = 0.3)
xgb.tune<- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                   D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                   D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                   D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                   D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                   D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                   D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                   D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                   D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                   D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                   D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                   D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                   D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                   D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                   D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                   D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143, 
                 data=top500.a,
                 method="xgbTree",
                 trControl=ctrl,
                 tuneGrid = grid)
xgb.tune$bestTune
xgb.tune
# predicting on validation dataset (AIC on top500 features)
xgb.predict.b <- predict(xgb.tune, newdata=top500.b) 
sum(xgb.predict.b == top500.b$Activity)/length(xgb.predict.b)
xgb.predict.ftd <- predict(xgb.tune, newdata=finTest) 
sum(xgb.predict.ftd == finTest$Activity)/length(xgb.predict.ftd)


################### RF #################
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                  D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                  D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                  D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                  D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                  D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                  D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                  D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                  D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                  D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                  D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                  D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                  D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                  D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                  D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                  D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143,
                data = top500.a, 
                method="rf", 
                trControl=ctrl, 
                tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (AIC on top500 features)
RF.predict.b <- predict(RFTune, newdata=top500.b) 
sum(RF.predict.b == top500.b$Activity)/length(RF.predict.b)
RF.predict.ftd <- predict(RFTune, newdata=finTest) 
sum(RF.predict.ftd == finTest$Activity)/length(RF.predict.ftd)

##############################################################
# Logistic Regression
model1 <-glm(formula = Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
               D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
               D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
               D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
               D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
               D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
               D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
               D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
               D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
               D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
               D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
               D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
               D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
               D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
               D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
               D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143, 
             family = binomial, data = top500.a)
predictGLM.b = predict(model1, newdata=top500.b, type="response")
t <- table(top500.b$Activity, predictGLM.b > 0.5)
(t[1,1]+t[2,2])/(sum(t))
predictGLM.ftd = predict(model1, newdata=finTest, type="response")
t <- table(finTest$Activity, predictGLM.ftd > 0.5)
(t[1,1]+t[2,2])/(sum(t))


##### Stacking--- using Logistic Regression to combine ##########

top500.b$svm <- svm.predict.b
top500.b$xg <- xgb.predict.b
top500.b$RF <- RF.predict.b
top500.b$glm <- predictGLM.b

finTest$svm <- svm.predict.ftd
finTest$xg <- xgb.predict.ftd
finTest$RF <- RF.predict.ftd
finTest$glm <- predictGLM.ftd


########## Running Log Reg on augmented data set ##########
model1 <-glm(formula = Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
               D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
               D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
               D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
               D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
               D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
               D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
               D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
               D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
               D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
               D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
               D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
               D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
               D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
               D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
               D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143 +
               xg + RF + svm, 
             family = binomial, data = top500.b)
predictGLM.final = predict(model1, newdata=finTest, type="response")
t <- table(finTest$Activity, predictGLM.final > 0.5)
(t[1,1]+t[2,2])/(sum(t))

# running augmented files with RF
##########################################################
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                  D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                  D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                  D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                  D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                  D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                  D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                  D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                  D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                  D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                  D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                  D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                  D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                  D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                  D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                  D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143 +
                  xg + RF + svm + glm, data=top500.b, method="rf", trControl=ctrl, tuneGrid=grid)
RF.predict.final <- predict(RFTune, newdata=finTest) 
sum(RF.predict.final == finTest$Activity)/length(RF.predict.final)


# running augmented files with RF (withoout LR)
##########################################################
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                  D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                  D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                  D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                  D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                  D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                  D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                  D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                  D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                  D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                  D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                  D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                  D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                  D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                  D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                  D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143 +
                  xg + RF + svm, data=top500.b, method="rf", trControl=ctrl, tuneGrid=grid)
RF.predict.final <- predict(RFTune, newdata=finTest) 
sum(RF.predict.final == finTest$Activity)/length(RF.predict.final)





### Majority Rule- rerun models with whole training dataset, predicting final test data

############ SVM ###############
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(sigma=c(0, 0.01, 0.03, 0.05),
                    C= c(0, 1, 2))
svm.tune<- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                   D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                   D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                   D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                   D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                   D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                   D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                   D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                   D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                   D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                   D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                   D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                   D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                   D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                   D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                   D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143, 
                 data = top500wd, 
                 method="svmRadial",
                 trControl=ctrl,
                 tuneGrid = grid)
svm.tune$bestTune
svm.tune$finalModel
svm.tune
# predicting on validation dataset (AIC on top 500 features)
svm.predict <- predict(svm.tune, newdata=finTest) 
sum(svm.predict == finTest$Activity)/length(svm.predict)

##################### XG #####################
#tuning parameters with CV in caret (AIC on top500 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid <- expand.grid(nrounds=c(500, 100),
                    max_depth= c(10, 15, 20),
                    eta = 0.3)
xgb.tune<- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                   D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                   D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                   D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                   D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                   D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                   D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                   D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                   D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                   D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                   D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                   D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                   D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                   D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                   D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                   D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143, 
                 data=top500wd,
                 method="xgbTree",
                 trControl=ctrl,
                 tuneGrid = grid)
xgb.tune$bestTune
xgb.tune
# predicting on validation dataset (AIC on top500 features)
xgb.predict <- predict(xgb.tune, newdata=finTest) 
sum(xgb.predict == finTest$Activity)/length(xgb.predict)


################### RF #################
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
                  D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
                  D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
                  D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
                  D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
                  D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
                  D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
                  D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
                  D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
                  D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
                  D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
                  D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
                  D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
                  D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
                  D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
                  D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143,
                data = top500wd, 
                method="rf", 
                trControl=ctrl, 
                tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (AIC on top500 features)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)

##############################################################
# Logistic Regression
model1 <-glm(formula = Activity ~ D27 + D146 + D129 + D60 + D806 + D480 + 
               D1150 + D117 + D66 + D961 + D999 + D149 + D900 + D881 + D599 + 
               D954 + D51 + D1309 + D13 + D1016 + D1049 + D459 + D42 + D1361 + 
               D14 + D1009 + D81 + D1012 + D100 + D504 + D612 + D949 + D1071 + 
               D44 + D194 + D688 + D5 + D92 + D131 + D202 + D1008 + D994 + 
               D1031 + D958 + D661 + D204 + D998 + D87 + D41 + D54 + D609 + 
               D6 + D212 + D697 + D1076 + D972 + D132 + D1018 + D583 + D207 + 
               D1119 + D45 + D224 + D1003 + D1094 + D1087 + D3 + D686 + 
               D1078 + D1198 + D1026 + D1014 + D1017 + D1158 + D1174 + D596 + 
               D976 + D1067 + D103 + D89 + D649 + D791 + D983 + D1058 + 
               D107 + D1002 + D750 + D441 + D78 + D813 + D210 + D810 + D155 + 
               D470 + D1047 + D1033 + D1059 + D1164 + D162 + D997 + D95 + 
               D119 + D314 + D492 + D807 + D63 + D143 + D1100 + D1160 + 
               D1203 + D29 + D911 + D973 + D226 + D71 + D86 + D159 + D1023 + 
               D35 + D449 + D505 + D38 + D742 + D118 + D2 + D88 + D175 + 
               D1029 + D1006 + D1192 + D219 + D98 + D808 + D514 + D1143, 
             family = binomial, data = top500wd)
predictGLM = predict(model1, newdata=finTest, type="response")
t <- table(finTest$Activity, predictGLM > 0.5)
(t[1,1]+t[2,2])/(sum(t))


# Majority Rule

svm_R <- as.numeric(svm.predict)-1
xgb_R <- as.numeric(xgb.predict)-1
RF_R <- as.numeric(RF.predict)-1
log_R <- as.numeric(predictGLM)

predictions<-(svm_R + xgb_R + RF_R)/3
t<- table(predictions > 0.5, finTest$Activity)
(t[1,1]+t[2,2])/sum(t)

predictions<-(svm_R + log_R + RF_R)/3
t<- table(predictions > 0.5, finTest$Activity)
(t[1,1]+t[2,2])/sum(t)
