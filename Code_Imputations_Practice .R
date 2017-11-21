bandsdata<-read.csv('bandsdata.csv')
str(bandsdata)
summary(bandsdata)

sum(is.na(bandsdata))
#There are missing values tagged with '?', so we will replace them with NA
bandsdata[bandsdata=="?"]<-NA
sum(is.na(bandsdata))
colSums(is.na(bandsdata))
attach(bandsdata)

bandsdata[,21:39]<-data.frame(apply(bandsdata[,21:39],2,function(x) as.numeric(x)))
str(bandsdata)

trialdata<-bandsdata

for(i in 1:nrow(trialdata)){
  if(i%%7==0){
    trialdata$ink.temperature[i]<-NA
    trialdata$humifity[i]<-NA
  }
}

sum(is.na(trialdata))
trialdata_knn<-trialdata
sum(is.na(trialdata_knn))

library(DMwR)
trialdata<-centralImputation(trialdata)
trialdata_knn<-knnImputation(trialdata_knn, k=5)
sum(is.na(trialdata))
sum(is.na(trialdata_knn))

#seq(5,539,5)

regr.eval(bandsdata[seq(7,539,7),c('humifity','ink.temperature')],trialdata[seq(7,539,7),c('humifity','ink.temperature')])
regr.eval(bandsdata[seq(7,539,7),c('humifity','ink.temperature')],trialdata_knn[seq(7,539,7),c('humifity','ink.temperature')])

par(mfrow = c(1,3))
boxplot(bandsdata$humifity, xlab = 'humidity', ylab = 'original')
boxplot(trialdata$humifity, xlab = 'humidity', ylab = 'Central_Imp')
boxplot(trialdata_knn$humifity, xlab = 'humidity',ylab = 'Knn_Imp')

boxplot(bandsdata$ink.temperature, xlab = 'ink.temp', ylab = 'original')
boxplot(trialdata$ink.temperature, xlab = 'ink.temp', ylab = 'Central_Imp')
boxplot(trialdata_knn$ink.temperature, xlab = 'ink.temp', ylab = 'Knn_Imp')
