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
library("e1071") # may be needed for caret
library(caTools)
library(randomForest)


# cv and tuning with caret (all features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=WD, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (all features)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)


# cv and tuning with caret (50 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=top50wd, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (50 features)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)


# cv and tuning with caret (100 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=top100wd, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (100 features)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)


# cv and tuning with caret (150 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=top150wd, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (150 features)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)


# cv and tuning with caret (200 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=top200wd, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (200 features)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)


# cv and tuning with caret (250 features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=top250wd, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune
# predicting on validation dataset (250 features)
RF.predict <- predict(RFTune, newdata=finTest) 
sum(RF.predict == finTest$Activity)/length(RF.predict)

#tuning parameters with CV in caret (AIC on top500 features)
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


#tuning parameters with CV in caret (top 100 comb pca/mca features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=Top100comb.wd, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune

#10    0.7669419  0.5286176  0.03125020   0.06352533
#50    0.7692233  0.5330009  0.04393951   0.08942898


# predicting on validation dataset (top 100 pca features)
RF.predict <- predict(RFTune, newdata=Top100comb.ftd) 
sum(RF.predict == Top100comb.ftd$Activity)/length(RF.predict)

#0.7822222

#tuning parameters with CV in caret (top 200 pca features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ ., data=Top200comb.wd, method="rf", trControl=ctrl, tuneGrid=grid)
RFTune$bestTune
RFTune

#10    0.7612385  0.5163756  0.02797194   0.05569957
#50    0.7688590  0.5326875  0.03083754   0.06186624

# predicting on validation dataset (top 200 pca features)
RF.predict <- predict(RFTune, newdata=Top200comb.ftd)
sum(RF.predict == Top200comb.ftd$Activity)/length(RF.predict)
#0.792

#tuning parameters with CV in caret (AIC on top 500 PCA features)
ctrl <- trainControl(method="cv", repeats=1, number=10)
grid  <- expand.grid(mtry = c(10, 50))
RFTune <- train(Activity ~ PC3 + PC7 + PC6 + PC50 + PC19 + PC26 + 
                  PC10 + PC47 + PC15 + PC46 + PC25 + Dim.44 + PC62 + PC21 + 
                  PC28 + PC55 + Dim.106 + Dim.169 + Dim.18 + PC13 + PC86 + 
                  PC30 + PC227 + Dim.39 + PC108 + PC104 + PC128 + PC90 + PC45 + 
                  PC66 + Dim.71 + PC40 + PC223 + PC8 + Dim.116 + Dim.378 + 
                  PC247 + PC167 + PC215 + PC243 + PC336 + Dim.172 + PC17 + 
                  PC150 + Dim.285 + PC48 + PC68 + Dim.104 + PC181 + Dim.87 + 
                  PC107 + Dim.99 + PC162 + PC172 + PC397 + PC184 + Dim.201 + 
                  Dim.131 + PC92 + PC85 + Dim.32 + Dim.65 + Dim.168 + PC65 + 
                  Dim.110 + Dim.124 + PC154 + PC135 + PC134 + Dim.7 + Dim.14 + 
                  Dim.79 + Dim.29 + Dim.381 + Dim.4 + Dim.1 + PC365 + PC396 + 
                  PC142 + Dim.21 + Dim.101 + PC367 + Dim.132 + Dim.154 + PC37 + 
                  PC166 + PC250 + PC138 + PC157 + PC190 + PC300 + PC177 + Dim.67 + 
                  Dim.113 + Dim.266 + Dim.37 + Dim.6 + PC71 + Dim.333 + PC33 + 
                  Dim.98 + Dim.171 + PC241 + Dim.25 + PC94 + Dim.344 + Dim.133 + 
                  PC254 + Dim.137 + PC194 + PC305 + Dim.252 + Dim.196 + PC4 + 
                  PC328 + Dim.192 + Dim.212 + PC341 + PC179 + Dim.274 + Dim.111 + 
                  Dim.30 + PC203 + Dim.184 + PC271 + PC231 + PC321 + Dim.386 + 
                  Dim.66 + PC79 + PC211 + PC36 + PC20 + Dim.15 + PC64 + Dim.144 + 
                  Dim.78 + PC316 + Dim.223 + PC237 + Dim.181 + Dim.239 + Dim.74 + 
                  Dim.261 + PC63 + Dim.267 + Dim.235 + Dim.189 + PC111 + Dim.62 + 
                  Dim.269 + Dim.11 + PC24 + Dim.16 + Dim.176 + PC315 + PC382 + 
                  PC326 + PC276 + PC43 + PC102 + PC214 + PC60 + PC58 + Dim.167 + 
                  PC311 + Dim.204 + Dim.49 + PC49 + PC375 + PC112 + Dim.141 + 
                  Dim.55 + PC262 + Dim.312, 
                data=Top500comb.wd, 
                method="rf", 
                trControl=ctrl, 
                tuneGrid=grid)
RFTune$bestTune
RFTune
#10    0.7611721  0.5193689  0.03730676   0.07539666
#50    0.7600372  0.5179298  0.03689401   0.07397635

# predicting on validation dataset (AIC on top 500 PCAfeatures)
RF.predict <- predict(RFTune, newdata=Top500comb.ftd)
sum(RF.predict == Top500comb.ftd$Activity)/length(RF.predict)

#0.786034
