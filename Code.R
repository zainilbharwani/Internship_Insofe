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
# lapply(catdata[,names(catdata)],function(x) plot(x,xlab = names(catdata[,i])))
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
mydata_knn<-mydata[1:539,]
mydata_central<-mydata[1:539,]

#Corrplot
num_data1<-na.omit(num_data)
library(corrplot)
cor_mat<-cor(num_data1)
corrplot(cor(num_data1))
#write.csv(cor_mat,'correl_matrix.csv',row.names = FALSE)

#######Pre-Processing#######
sum(is.na(mydata_knn))
colSums(is.na(mydata_knn))

# mydata_knn$paper.mill.location<-NULL
# mydata_central$paper.mill.location<-NULL
sum(is.na(mydata_knn))
summary(mydata_knn)

library(DMwR)
mydata_knn<-knnImputation(mydata_knn)
sum(is.na(mydata_knn))
str(mydata_central)

mydata_central<-centralImputation(mydata_central)
sum(is.na(mydata_central))

#Checking the distributions
#catdata$paper.mill.location<-NULL
#catdata <- catdata[1:539,]

###Comparison of plots before and after imputation
##Categorical Plots
# for(i in 1:19){
#   dev.copy(jpeg,filename=paste(names(catdata[i]),"verifyplot.jpg",sep="_"))
#   par(mfrow = c(1,3))
#   plot(mydata_central[,i+20], xlab = names(mydata_central[i+20]), ylab = 'Frequency_mydata_central')
#   plot(catdata[,i], xlab = names(catdata[i]), ylab = 'Frequency')
#   plot(mydata_knn[,i+20], xlab = names(mydata_knn[i+20]), ylab = 'Frequency_mydata_knn')
#   dev.off ()
# }

#num_data<-num_data[1:539,]

###Histogram
# for(i in 1:20){
#   dev.copy(jpeg,filename=paste(names(num_data[i]),"verifyplot.jpg",sep="_"))
#   par(mfrow = c(1,3))
#   hist(mydata_central[,i], xlab = names(mydata_central[i]), ylab = 'Frequency_mydata_central')
#   hist(num_data[,i],xlab = names(num_data[i]), ylab = 'Frequency')
#   hist(mydata_knn[,i], xlab = names(mydata_knn[i]), ylab = 'Frequency_mydata_knn')
#   dev.off ()
# }

##Box Plots for comparison before and after imputation
# for(i in 1:20){
#   dev.copy(jpeg,filename=paste(names(num_data[i]),"verifybwplot.jpg",sep="_"))
#   par(mfrow = c(1,3))
#   boxplot(mydata_central[,i], xlab = names(mydata_central[i]), ylab = 'Freq_mydata_central')
#   boxplot(num_data[,i], xlab = names(num_data[i]), ylab = 'Freq_numdata')
#   boxplot(mydata_knn[,i], xlab = names(mydata_knn[i]), ylab = 'Freq_mydata_knn')
#   dev.off()
# }

####Normalize###
library(vegan)
mydata_knn[,2:20]<-decostand(mydata_knn[,2:20], method = 'range')
summary(mydata_knn)

mydata_central[,2:20]<-decostand(mydata_central[,2:20], method = 'range')
summary(mydata_central)

names(mydata_knn)

str(mydata_knn)

##Using knn or central imputation for different variables
sum(is.na(mydata))
knn_df<-mydata[,c(1:7,9:17,19)]
central_df<-mydata[,-c(1:7,9:17,19)]

sum(is.na(knn_df))
sum(is.na(central_df))

knn_df<-knnImputation(knn_df)
central_df<-centralImputation(central_df)

#Normalizing both knn_df and central_df
knn_df<-decostand(knn_df,method = 'range')
central_df[,1:3]<-decostand(central_df[,1:3],method = 'range')

finaldata<-cbind(knn_df,central_df)
sum(is.na(finaldata))

# for(i in 1:39){
#   dev.copy(png,filename = paste(names(finaldata[i]),'relplot.png',sep = '_'))
#   plot(x = finaldata[,i], y = finaldata$band.type, xlab = names(finaldata[i]), ylab = 'band.type')
#   dev.off()
# }

#Performing PCA
pca_comp<-princomp(finaldata[,2:20],cor = TRUE)
summary(pca_comp)

pca_data<-data.frame(pca_comp$scores)
pca_data<-cbind(pca_data,finaldata[,21:40])
plot(pca_comp)

save.image("C:/Users/hp/Desktop/Insofe Intern/code_workspace.RData")
