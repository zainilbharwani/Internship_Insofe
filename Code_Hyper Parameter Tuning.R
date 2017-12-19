load('Model_building_workspace.RData')

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

####Hyper-Parameter Tuning####
#Using Stepaic to determine useful features
library(MASS)
model_stepaic<-stepAIC(model_glm)
model_stepaic
summary(model_stepaic)

prob_stepaic_train<-predict(model_stepaic, newdata = traindata, type = 'response')
prob_stepaic_val<-predict(model_stepaic, newdata = valdata, type = 'response')

#Tuning the threshold parameter
#0.55
output_stepaic_train<-ifelse(prob_stepaic_train<0.55,'band','noband')
output_stepaic_val<-ifelse(prob_stepaic_val<0.55,'band','noband')
confusionMatrix(valdata$band.type, output_stepaic_val)

#0.5
output_stepaic_train<-ifelse(prob_stepaic_train<0.5,'band','noband')
output_stepaic_val<-ifelse(prob_stepaic_val<0.5,'band','noband')
confusionMatrix(valdata$band.type, output_stepaic_val)

#0.4
output_stepaic_train<-ifelse(prob_stepaic_train<0.45,'band','noband')
output_stepaic_val<-ifelse(prob_stepaic_val<0.45,'band','noband')
confusionMatrix(valdata$band.type, output_stepaic_val)

##C50 model tuning
library(C50)
acc_train = c()
rec_train = c()
acc_val = c()
rec_val = c()

for(i in 1:20){
  
model_c50<-C5.0(x = traindata[,-34], y = traindata[,34], rules = T, trials = i)
model_c50$rules
model_c50$trials

output_c50_train<-predict(model_c50, newdata = traindata[,-34])
output_c50_val<-predict(model_c50, newdata = valdata[,-34])

acc_c50_train<-Accuracy(output_c50_train,traindata$band.type)
acc_c50_val<-Accuracy(output_c50_val,valdata$band.type)

rec_c50_train<-Recall(output_c50_train,traindata$band.type)
rec_c50_val<-Recall(output_c50_val,valdata$band.type)

rec_train<-append(rec_train, rec_c50_train)
rec_val<-append(rec_val, rec_c50_val)

acc_train<-append(acc_train, acc_c50_train)
acc_val<-append(acc_val, acc_c50_val)
}

par(mfrow = c(2,1))
#dev.copy(png, "rec_C50_plot.png")
plot(rec_train, type = "s", col = "blue", xlab='Trials')
plot(rec_val, type = "s", col = "red", xlab='Trials')
#dev.off()
confusionMatrix(valdata$band.type, output_c50_val)

##KNN model tuning
acc_train = c()
rec_train = c()
acc_val = c()
rec_val = c()
a = seq(1,29,by=2)
for(i in a){
  
  model_knn<-knn3(band.type~., data = traindata, k=9)
  model_knn
  
  prob_knn_train<-predict(model_knn, newdata = traindata)
  prob_knn_val<-predict(model_knn, newdata = valdata)
  
  output_knn_train<-ifelse(prob_knn_train[,1]>prob_knn_train[,2], 'band', 'noband')
  output_knn_val<-ifelse(prob_knn_val[,1]>prob_knn_val[,2], 'band', 'noband')

  acc_knn_train<-Accuracy(output_knn_train,traindata$band.type)
  acc_knn_val<-Accuracy(output_knn_val,valdata$band.type)
  
  rec_knn_train<-Recall(output_knn_train,traindata$band.type)
  rec_knn_val<-Recall(output_knn_val,valdata$band.type)
  
  acc_train<-append(acc_train,acc_knn_train)
  acc_val<-append(acc_val,acc_knn_val)
  
  rec_train<-append(rec_train,rec_knn_train)
  rec_val<-append(rec_val,rec_knn_val)
}

rec_train
rec_val

par(mfrow = c(2,1))
#dev.copy(png, "rec_KNN_plot.png")
plot(y=rec_train, x=a, type = "s", col = "blue", xlab='Number of Neighbours')
plot(y=rec_val, x=a, type = "s", col = "red", xlab='Number of Neighbours')
#dev.off()
confusionMatrix(valdata$band.type, output_knn_val)

##SVM
library(e1071)
model_svm<-svm(band.type~., data = traindata, kernel = 'sigmoid')
model_svm

output_svm_train<-predict(model_svm, newdata = traindata)
output_svm_val<-predict(model_svm, newdata = valdata)

confusionMatrix(valdata$band.type, output_svm_val)

##RF
library(randomForest)
set.seed(256)
model_randomforest<-randomForest(band.type~., data = traindata, ntree = 80)
model_randomforest

output_randomforest_train<-predict(model_randomforest, newdata = traindata)
output_randomforest_val<-predict(model_randomforest, newdata = valdata)

confusionMatrix(valdata$band.type, output_randomforest_val)
