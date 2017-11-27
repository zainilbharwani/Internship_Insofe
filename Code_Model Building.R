load('code_workspace.RData')
str(finaldata)

finaldata$timestamp<-NULL
finaldata$cylinder.number<-NULL
finaldata$job.number<-NULL
finaldata$ink.color<-NULL
finaldata$cylinder.division<-NULL
finaldata$customer<-NULL
str(finaldata)

attach(finaldata)

######Model Building######
library(caret)
set.seed(256)
trainrows<-createDataPartition(finaldata$band.type, p = 0.80, list = F)
traindata<-finaldata[trainrows,]
val_testdata<-finaldata[-trainrows,]
valrows<-createDataPartition(val_testdata$band.type, p = 0.50, list = F)
valdata<-val_testdata[valrows,]
testdata<-val_testdata[-valrows,]

str(traindata)
str(valdata)
str(testdata)

#Logistic regression 
library(glmnet)
model_glm<-glm(band.type~., data = traindata, family = 'binomial')
model_glm
summary(model_glm)
prob_train<-predict(model_glm, newdata = traindata, type = 'response')
prob_val<-predict(model_glm, newdata = valdata, type = 'response')

output_glm_train<-ifelse(prob_train<0.55,'band','noband')
output_glm_val<-ifelse(prob_val<0.55,'band','noband')

library(MLmetrics)
Accuracy(output_glm_train,traindata$band.type)
Accuracy(output_glm_val,valdata$band.type)

#StepAIC
library(MASS)
model_stepaic<-stepAIC(model_glm)
model_stepaic

#Decision Trees
library(rpart)
library(rpart.plot)
model_rpart<-rpart(band.type~., data = traindata, cp = -1)
model_rpart

#dev.copy(png, filename = paste('rpart_plot.png'))
rpart.plot(model_rpart, cex = 0.5)
#dev.off()

printcp(model_rpart)
plotcp(model_rpart)
prune_tree<-prune(model_rpart, cp=0.0078)

#dev.copy(png, filename = paste('prunetree_plot.png'))
rpart.plot(prune_tree, cex = 0.5)
#dev.off()

#Predicting using full tree
output_rpart_train<-predict(model_rpart, newdata = traindata, 'class')
output_rpart_val<-predict(model_rpart, newdata = valdata, 'class')

Accuracy(output_rpart_train,traindata$band.type)
Accuracy(output_rpart_val,valdata$band.type)

#Predicting using prune tree
output_prune_train<-predict(prune_tree, newdata = traindata, 'class')
output_prune_val<-predict(prune_tree, newdata = valdata, 'class')

Accuracy(output_prune_train,traindata$band.type)
Accuracy(output_prune_val,valdata$band.type)

library(C50)
model_c50<-C5.0(x = traindata[,-34], y = traindata[,34], rules = F)
model_c50$rules

output_c50_train<-predict(model_c50, newdata = traindata[,-34])
output_c50_val<-predict(model_c50, newdata = valdata[,-34])

Accuracy(output_c50_train,traindata$band.type)
Accuracy(output_c50_val,valdata$band.type)
#plot(model_c50)
