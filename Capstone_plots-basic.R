setwd("~/Data Analytics/Capstone course/Boehringer-Ingelheim/Step_4_graphics")
getwd()
library(ggplot2)

cap <- read.csv("Cap.csv")

names(cap)
str(cap$feature_sel)

cap$feature_sel <- factor(cap$feature_sel, levels=c("Original features", "50 mostImp", "100 mostImp", "150 mostImp", "200 mostImp",         
 "250 mostImp", "AIC mostImp", "100 mostImp PCs", "200 mostImp PCs", "AIC PCs"))

feats <- c("Original", "50", "100", "150", "200", "250", "AIC", "100 PC", "200 PC", "AIC PC")
cap$feats <- feats

cap.num <- c(1:10)
cap$cap.num <- cap.num

ftd <- cap[,c(1, seq(5, 37, by=4))]
vd <- cap[,c(1, seq(3, 37, by=4))]
str(ftd)
str(vd)

#colors()

# Final test data plot

plot(cap$cap.num, cap$knn.ftd, pch=3, type="b", col="purple", lwd=2, lty=2, xaxt="n", 
     ylim=c(0.65, 0.825), main="Final Test Data",  xlab="Feature Selection", ylab="Accuracy")
lines(cap$cap.num, cap$log.ftd, pch=3, type="b", col="brown", lwd=2, lty=2, xaxt="n")
lines(cap$cap.num, cap$j48.ftd, pch=3, type="b", col="blue", lwd=2, lty=2, xaxt="n")
lines(cap$cap.num, cap$svm.ftd, pch=3, type="b", col="red", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$rf.ftd, pch=3, type="b", col="magenta", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$ada.ftd, pch=3, type="b", col="green3", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$xg.ftd, pch=3, type="b", col="turquoise2", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$stack.ftd, pch=19, type="b", col="black", xaxt="n", cex=1.5)
lines(cap$cap.num, cap$maj.ftd, pch=8, type="b", col="maroon", cex=3, xaxt="n")
axis(1, at=1:10, labels=cap$feats)
legend('bottom', c("Majority Rule", "Stacking",  "Random Forest", "SVM", "Adaboost","XGboost",  "J48",
                 "Log Reg", "kNN"), lty=c(0, 0, 1, 1, 1, 1, 3, 3, 3), pch=c(8, 19, 3, 3, 3, 3, 3, 3, 3), lwd=3,
                 col=c("maroon", "black", "magenta", "red", "green3","turquoise2", 
                       "blue", "brown", "purple"), cex=0.7)



# Final test data plot (top performers)

plot(cap$cap.num, cap$knn.ftd, type="l", col="purple", lwd=2, lty=0, xaxt="n", 
     ylim=c(0.76, 0.83), main="Final Test Data (Top Performers)",  xlab="Feature Selection", ylab="Accuracy")
lines(cap$cap.num, cap$log.ftd, type="l", col="brown", lwd=2, lty=0, xaxt="n")
lines(cap$cap.num, cap$j48.ftd, type="l", col="blue", lwd=2, lty=0, xaxt="n")
lines(cap$cap.num, cap$svm.ftd, pch=3, type="b", col="red", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$rf.ftd, pch=3, type="b", col="magenta", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$ada.ftd, pch=3, type="b", col="green3", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$xg.ftd, pch=3, type="b", col="turquoise2", lwd=2, lty=1, xaxt="n")
lines(cap$cap.num, cap$stack.ftd, pch=19, type="b", col="black", xaxt="n", cex=1.5)
lines(cap$cap.num, cap$maj.ftd, pch=8, type="b", col="maroon", cex=3, xaxt="n")
axis(1, at=1:10, labels=cap$feats)
legend('bottom', c("Majority Rule", "Stacking",  "Random Forest", "SVM", "Adaboost","XGboost"), lty=c(0, 0, 1, 1, 1, 1), pch=c(8, 19, 3, 3, 3, 3), lwd=3,
       col=c("maroon", "black", "magenta", "red", "green3","turquoise2"), cex=0.8)



old.par <- par()
new.par <- par(mfrow = c(1,1))
par(old.par)
