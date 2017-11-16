bandsdata<-read.csv('bandsdata.csv')
str(bandsdata)
summary(bandsdata)

sum(is.na(bandsdata))
#There are missing values tagged with '?', so we will replace them with NA
bandsdata[bandsdata=="?"]<-NA
sum(is.na(bandsdata))
colSums(is.na(bandsdata))
attach(bandsdata)

catdata<-bandsdata[,c(2:20,40)]
num_data<-bandsdata[,c(1,21:39)]

str(catdata)
catdata<-data.frame(apply(catdata,2,'factor'))

str(num_data)
num_data<-data.frame(apply(num_data,2,function(x) as.numeric(x)))

#There are lot of discrepancies in the data set regarding case sensitivity.
levels(ink.color)
levels(paper.type)
?tolower()

catdata<-as.data.frame(sapply(catdata[,names(catdata)],function(x) tolower(x)))
str(catdata)
summary(catdata)

mydata<-data.frame(num_data,catdata)

#No of missing values
sum(is.na(mydata))

#percentage of missing values (4.57%)
miss_val<-sum(is.na(mydata))/(539*40)

#Columns with NAs
w<-colSums(is.na(mydata))/dim(mydata)[1]
cols_with_more_NAs<-w[w>0.2]
which(w>0.2)

#Rows with NAs
x<-rowSums(is.na(mydata))/dim(mydata)[2]
rows_with_more_NAs<-x[x>0.2]
which(x>0.2)

str(mydata)
summary(mydata)

#Proportions of class levels
227/312

#Plots of categoric attributes
sink("myfile.txt")
sink()

names(catdata)

#To check the number of levels
str(catdata)
summary(catdata)

plot(catdata$band.type)

# pdf('mygraph1.pdf')
# lapply(catdata[,names(catdata)],function(x) plot(x,xlab = names(catdata)))
# dev.off()
# plot(catdata[,2])


# for(i in 1:20){
#   dev.copy(jpeg,filename=paste(names(catdata[i]),"plot.jpg",sep="_"))
#   plot(catdata[,i], xlab = names(catdata[i]), ylab = 'Frequency')
#   dev.off ()
# }

#Plots of numeric variables
library(ggplot2)
hist(num_data$viscosity)
ggplot()+geom_density(aes(num_data$viscosity))

###Histogram
# for(i in 1:20){
#   dev.copy(jpeg,filename=paste(names(num_data[i]),"plot.jpg",sep="_"))
#   hist(num_data[,i],xlab = names(num_data[i]), ylab = 'Frequency')
#   dev.off ()
# }

boxplot(num_data[,4], xlab = 'num')
boxplot.stats(num_data[,4])

###Box-Plots
# for(i in 1:20){
#   dev.copy(jpeg,filename=paste(names(num_data[i]),"bwplot.jpg",sep="_"))
#   boxplot(num_data[,i], xlab = names(num_data[i]))
#   dev.off()
# }

#Descriptive Stats of numeric attributes
summary(num_data)
apply(num_data,2,function(x) sd(x,na.rm = TRUE))

rows_with_more_NAs<-x[x>0.2]
which(x>0.2)

sum(is.na(mydata))
mydata1<-mydata[1:486,]
sum(is.na(mydata1))
colSums(is.na(mydata1))

#Corrplot
num_data1<-na.omit(num_data)
library(corrplot)
cor_mat<-cor(num_data1)
corrplot(cor(num_data1))
write.csv(cor_mat,'correl_matrix.csv',row.names = FALSE)