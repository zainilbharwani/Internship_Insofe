load('code_workspace.RData')
str(finaldata)

finaldata$timestamp<-NULL
finaldata$cylinder.number<-NULL
finaldata$job.number<-NULL
finaldata$ink.color<-NULL
finaldata$cylinder.division<-NULL
finaldata$customer<-NULL

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

##Logistic regression##
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

##StepAIC##
library(MASS)
model_stepaic<-stepAIC(model_glm)
model_stepaic

prob_stepaic_train<-predict(model_stepaic, newdata = traindata, type = 'response')
prob_stepaic_val<-predict(model_stepaic, newdata = valdata, type = 'response')

output_stepaic_train<-ifelse(prob_stepaic_train<0.45,'band','noband')
output_stepaic_val<-ifelse(prob_stepaic_val<0.45,'band','noband')

Accuracy(output_stepaic_train,traindata$band.type)
Accuracy(output_stepaic_val,valdata$band.type)

##Decision Trees###
library(rpart)
library(rpart.plot)
model_rpart<-rpart(band.type~., data = traindata, cp = -1)
model_rpart

#dev.copy(png, filename = paste('rpart_plot.png'))
rpart.plot(model_rpart, cex = 0.5)
#dev.off()

printcp(model_rpart)
plotcp(model_rpart)
prune_tree<-prune(model_rpart, cp=0.0478)

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

#C5.0 model
library(C50)
model_c50<-C5.0(x = traindata[,-33], y = traindata[,33], rules = F)
model_c50$rules
model_c50$trials

output_c50_train<-predict(model_c50, newdata = traindata[,-33])
output_c50_val<-predict(model_c50, newdata = valdata[,-33])

Accuracy(output_c50_train,traindata$band.type)
Accuracy(output_c50_val,valdata$band.type)
#plot(model_c50)

##KNN##
model_knn<-knn3(band.type~., data = traindata, k=10)
model_knn

prob_knn_train<-predict(model_knn, newdata = traindata)
prob_knn_val<-predict(model_knn, newdata = valdata)

output_knn_train<-ifelse(prob_knn_train[,1]>prob_knn_train[,2], 'band', 'noband')
output_knn_val<-ifelse(prob_knn_val[,1]>prob_knn_val[,2], 'band', 'noband')

Accuracy(output_knn_train,traindata$band.type)
Accuracy(output_knn_val,valdata$band.type)

##SVM##
library(e1071)
model_svm<-svm(band.type~., data = traindata, kernel = 'linear')
model_svm

output_svm_train<-predict(model_svm, newdata = traindata)
output_svm_val<-predict(model_svm, newdata = valdata)

Accuracy(output_svm_train, traindata$band.type)
Accuracy(output_svm_val, valdata$band.type)

##Confusion Matrix for all the models
confusionMatrix(output_glm_train, traindata$band.type)
confusionMatrix(output_rpart_train, traindata$band.type)
confusionMatrix(output_prune_train, traindata$band.type)
confusionMatrix(output_c50_train, traindata$band.type)
confusionMatrix(output_knn_train, traindata$band.type)
confusionMatrix(output_svm_train, traindata$band.type)

confusionMatrix(output_glm_val, valdata$band.type)
confusionMatrix(output_rpart_val, valdata$band.type)
confusionMatrix(output_prune_val, valdata$band.type)
confusionMatrix(output_c50_val, valdata$band.type)
confusionMatrix(output_knn_val, valdata$band.type)
confusionMatrix(output_svm_val, valdata$band.type)

##Regularization
