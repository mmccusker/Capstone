
#MCA(dataframe as factors)
library("FactoMineR", lib.loc="~/R/win-library/3.2")

binary.data <- apply(train,2,function(x) { all(na.omit(x) %in% 0:1) })
train.b <- train[binary.data]
train.c <- train[!binary.data]

train.b.noAct <- train.b
train.b.noAct$Activity <- NULL

train.b.f <- apply(train.b.noAct, 2, factor)


mca2<-MCA(train.b.f, ncp=835)
mca2$eig

eig <- mca2$eig
plot(eig[,3])

ind <- mca2$ind
coord <-ind$coord

coord.df <- as.data.frame(coord)


