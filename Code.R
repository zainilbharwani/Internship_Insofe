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

#Descriptive Stats of numeric attributes
summary(num_data)
apply(num_data,2,function(x) sd(x))

#Plots of catego
