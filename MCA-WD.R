
#MCA(dataframe as factors)
library("FactoMineR", lib.loc="~/R/win-library/3.2")

binary.data <- apply(WD,2,function(x) { all(na.omit(x) %in% 0:1) })
WD.b <- WD[binary.data]
WD.c <- WD[!binary.data]

WD.b.noAct <- WD.b
WD.b.noAct$Activity <- NULL

WD.b.f <- apply(WD.b.noAct, 2, factor)


mca2<-MCA(WD.b.f, ncp=835)
mca2$eig

eig <- mca2$eig
plot(eig[,3])

ind <- mca2$ind
coord <-ind$coord

coord.df <- as.data.frame(coord)


