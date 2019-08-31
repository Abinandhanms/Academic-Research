setwd('D:/MS/Trisemester-3/Wildfire/Model')

Data<-read.csv("Accuracy_scores.csv",header = T,na.strings = c(""),stringsAsFactors = F)

names(Data)[1]<-"Model"
names(Data)[2]<-"Accuracy"

shapiro.test(Data$Accuracy)



wilcox.test(Data$Accuracy ~ Data$Model, alternative = "two.sided",conf.int=T, conf.level = 0.95,paired = FALSE,exact=F,correct=T)
