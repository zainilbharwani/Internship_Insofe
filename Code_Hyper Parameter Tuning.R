load('Model_building_workspace.RData')

library(caret)
set.seed(256)
trainrows<-createDataPartition(finaldata$band.type, p = 0.80, list = F)
traindata<-finaldata[trainrows,]
val_testdata<-finaldata[-trainrows,]
valrows<-createDataPartition(val_testdata$band.type, p = 0.5, list = F)
valdata<-val_testdata[valrows,]
testdata<-val_testdata[-valrows,]

str(traindata)
str(valdata)
str(testdata)

library(MLmetrics)
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
confusionMatrix(output_stepaic_val, valdata$band.type)

#0.5
output_stepaic_train<-ifelse(prob_stepaic_train<0.5,'band','noband')
output_stepaic_val<-ifelse(prob_stepaic_val<0.5,'band','noband')
confusionMatrix(output_stepaic_val, valdata$band.type)

#0.45
output_stepaic_train<-ifelse(prob_stepaic_train<0.45,'band','noband')
output_stepaic_val<-ifelse(prob_stepaic_val<0.45,'band','noband')
confusionMatrix(output_stepaic_val, valdata$band.type)

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

rec_c50_train<-Recall(y_pred = output_c50_train, y_true = traindata$band.type)
rec_c50_val<-Recall(y_pred = output_c50_val, y_true = valdata$band.type)

rec_train<-append(rec_train, rec_c50_train)
rec_val<-append(rec_val, rec_c50_val)

acc_train<-append(acc_train, acc_c50_train)
acc_val<-append(acc_val, acc_c50_val)
}

#par(mfrow = c(2,1))
#dev.copy(png, "rec_C50_plot.png")
plot(rec_train, type = "s", col = "blue", xlab='Trials')
plot(rec_val, type = "s", col = "red", xlab='Trials')
#dev.off()
confusionMatrix(output_c50_val, valdata$band.type)

##KNN model tuning
acc_train = c()
rec_train = c()
acc_val = c()
rec_val = c()
a = seq(1,29,by=2)
for(i in a){
  
  model_knn<-knn3(band.type~., data = traindata, k=i)
  model_knn
  
  prob_knn_train<-predict(model_knn, newdata = traindata)
  prob_knn_val<-predict(model_knn, newdata = valdata)
  
  output_knn_train<-ifelse(prob_knn_train[,1]>prob_knn_train[,2], 'band', 'noband')
  output_knn_val<-ifelse(prob_knn_val[,1]>prob_knn_val[,2], 'band', 'noband')

  acc_knn_train<-Accuracy(output_knn_train,traindata$band.type)
  acc_knn_val<-Accuracy(output_knn_val,valdata$band.type)
  
  rec_knn_train<-Recall(y_pred = output_knn_train,y_true = traindata$band.type)
  rec_knn_val<-Recall(y_pred = output_knn_val,y_true = valdata$band.type)
  
  acc_train<-append(acc_train,acc_knn_train)
  acc_val<-append(acc_val,acc_knn_val)
  
  rec_train<-append(rec_train,rec_knn_train)
  rec_val<-append(rec_val,rec_knn_val)
}

rec_train
rec_val

#par(mfrow = c(2,1))
#dev.copy(png, "rec_KNN_plot.png")
plot(y=rec_train, x=a, type = "s", col = "blue", xlab='Number of Neighbours')
plot(y=rec_val, x=a, type = "s", col = "red", xlab='Number of Neighbours')
#dev.off()
confusionMatrix(output_knn_val, valdata$band.type)

##SVM
library(e1071)
model_svm<-svm(band.type~., data = traindata, kernel = 'linear')
model_svm

output_svm_train<-predict(model_svm, newdata = traindata)
output_svm_val<-predict(model_svm, newdata = valdata)

confusionMatrix(output_svm_val, valdata$band.type)

##KSVM
library(kernlab)
model_ksvm<-ksvm(band.type~., data = traindata, type = "C-svc", kernel = "anovadot")
model_ksvm

output_ksvm_train<-predict(model_ksvm, newdata = traindata)
output_ksvm_val<-predict(model_ksvm, newdata = valdata)

confusionMatrix(output_ksvm_val, valdata$band.type)

##RF
library(randomForest)
set.seed(256)
model_randomforest<-randomForest(band.type~., data = traindata, ntree = 50)
model_randomforest

output_randomforest_train<-predict(model_randomforest, newdata = traindata)
output_randomforest_val<-predict(model_randomforest, newdata = valdata)

confusionMatrix(reference = valdata$band.type, data = output_randomforest_val)

Accuracy(y_true = valdata$band.type, y_pred = output_randomforest_val)
Recall(y_true = valdata$band.type, y_pred = output_randomforest_val)

##Cforest
library(party)

acc_cforest_train = c()
acc_cforest_val = c()
recall_cforest_train = c()
recall_cforest_val = c()
#recall_cforest_test = c()

for(ntree in 1:100){
  set.seed(256)
  model_cforest<-cforest(band.type~., data = traindata, controls=cforest_control(ntree=ntree))
  model_cforest
  
  output_cforest_train<-predict(model_cforest, newdata = traindata)
  output_cforest_val<-predict(model_cforest, newdata = valdata)
  
  acc_cforest_train[ntree]<-Accuracy(output_cforest_train, traindata$band.type)
  acc_cforest_val[ntree]<-Accuracy(output_cforest_val, valdata$band.type)
  
  recall_cforest_train[ntree]<-Recall(y_pred = output_cforest_train, y_true = traindata$band.type)
  recall_cforest_val[ntree]<-Recall(y_pred = output_cforest_val, y_true = valdata$band.type)
  
  # output_cforest_test<-predict(model_cforest, newdata = testdata)
  # recall_cforest_test[ntree]<-Recall(output_cforest_test, testdata$band.type)
}

max(recall_cforest_train)
max(recall_cforest_val)

#par(mfrow = c(2,1))
plot(recall_cforest_train, type = 's', main = 'Recall vs Number of trees for train using cforest', xlab = 'Number of trees', ylab = 'Recall_train_finaldata_tuned', col = 'blue')
plot(recall_cforest_val, type = 's', main = 'Recall vs Number of trees for validation using cforest', xlab = 'Number of trees', ylab = 'Recall_val_finaldata_tuned', col = 'red')

#Checking the variable importance in RF
model_randomforest$importance
varImpPlot(model_randomforest)

####Using selected features from StepAIC for model building####
#KNN
acc_train = c()
rec_train = c()
acc_val = c()
rec_val = c()
a = seq(1,39,by=2)
for(i in a){
  
  model_knn<-knn3(band.type ~ viscosity + ink.temperature + humifity + 
                  roughness + varnish.pct + press.speed + ESA.Voltage + wax + 
                  blade.pressure + current.density + paper.type + ink.type + 
                  type.on.cylinder + press.type + press + unit.number + paper.mill.location, 
                  data = traindata, k=i)
  model_knn
  
  prob_knn_train<-predict(model_knn, newdata = traindata)
  prob_knn_val<-predict(model_knn, newdata = valdata)
  
  output_knn_train<-ifelse(prob_knn_train[,1]>prob_knn_train[,2], 'band', 'noband')
  output_knn_val<-ifelse(prob_knn_val[,1]>prob_knn_val[,2], 'band', 'noband')
  
  acc_knn_train<-Accuracy(output_knn_train,traindata$band.type)
  acc_knn_val<-Accuracy(output_knn_val,valdata$band.type)
  
  rec_knn_train<-Recall(y_pred = output_knn_train,y_true = traindata$band.type)
  rec_knn_val<-Recall(y_pred = output_knn_val,y_true = valdata$band.type)
  
  acc_train<-append(acc_train,acc_knn_train)
  acc_val<-append(acc_val,acc_knn_val)
  
  rec_train<-append(rec_train,rec_knn_train)
  rec_val<-append(rec_val,rec_knn_val)
}
rec_train
rec_val

plot(y=rec_train, x=a, type = "s", col = "blue", xlab='Number of Neighbours')
plot(y=rec_val, x=a, type = "s", col = "red", xlab='Number of Neighbours')

#SVM
library(e1071)
model_svm<-svm(band.type ~ viscosity + ink.temperature + humifity + 
               roughness + varnish.pct + press.speed + ESA.Voltage + wax + 
               blade.pressure + current.density + paper.type + ink.type + 
               type.on.cylinder + press.type + press + unit.number + paper.mill.location, 
               data = traindata, kernel = 'linear')
model_svm

output_svm_train<-predict(model_svm, newdata = traindata)
output_svm_val<-predict(model_svm, newdata = valdata)

confusionMatrix(output_svm_val, valdata$band.type)

##Rpart
library(rpart)
library(rpart.plot)
model_rpart<-rpart(band.type ~ viscosity + ink.temperature + humifity + 
                   roughness + varnish.pct + press.speed + ESA.Voltage + wax + 
                   blade.pressure + current.density + paper.type + ink.type + 
                   type.on.cylinder + press.type + press + unit.number + paper.mill.location, 
                   data = traindata, cp = -1)
model_rpart

rpart.plot(model_rpart, cex = 0.5)

printcp(model_rpart)
plotcp(model_rpart)

output_rpart_train<-predict(model_rpart, newdata = traindata, 'class')
output_rpart_val<-predict(model_rpart, newdata = valdata, 'class')

confusionMatrix(output_rpart_val, valdata$band.type)

#Pruned tree
prune_tree<-prune(model_rpart, cp = 0.0054945)

output_prune_train<-predict(prune_tree, newdata = traindata, 'class')
output_prune_val<-predict(prune_tree, newdata = valdata, 'class')

confusionMatrix(output_prune_val, valdata$band.type)

###Ensembles
#RF
library(randomForest)
set.seed(256)
model_randomforest<-randomForest(band.type ~ viscosity + ink.temperature + humifity + 
                                 roughness + varnish.pct + press.speed + ESA.Voltage + wax + 
                                 blade.pressure + current.density + paper.type + ink.type + 
                                 type.on.cylinder + press.type + press + unit.number + paper.mill.location,
                                 data = traindata, ntree = 150)
model_randomforest
plot(model_randomforest)

output_randomforest_train<-predict(model_randomforest, newdata = traindata)
output_randomforest_val<-predict(model_randomforest, newdata = valdata)

confusionMatrix(output_randomforest_val, valdata$band.type)

#Cforest
set.seed(256)
model_cforest<-cforest(band.type ~ viscosity + ink.temperature + humifity + 
                       roughness + varnish.pct + press.speed + ESA.Voltage + wax + 
                       blade.pressure + current.density + paper.type + ink.type + 
                       type.on.cylinder + press.type + press + unit.number + paper.mill.location,
                       data = traindata, controls=cforest_control(ntree=200))
model_cforest

output_cforest_train<-predict(model_cforest, newdata = traindata)
output_cforest_val<-predict(model_cforest, newdata = valdata)

confusionMatrix(output_cforest_val, valdata$band.type)

#GBM
library(gbm)
traindata$band.type<-ifelse(traindata$band.type == 'band', 0, 1)
valdata$band.type<-ifelse(valdata$band.type=='band', 0, 1)

model_boosting<-gbm(band.type ~ viscosity + ink.temperature + humifity + 
                    roughness + varnish.pct + press.speed + ESA.Voltage + wax + 
                    blade.pressure + current.density + paper.type + ink.type + 
                    type.on.cylinder + press.type + press + unit.number + paper.mill.location,
                    data = traindata, n.trees = 8000, distribution = 'bernoulli')
model_boosting

prob_boosting_train<-predict.gbm(model_boosting, newdata = traindata, n.trees = 8000, type = 'response')
prob_boosting_val<-predict.gbm(model_boosting, newdata = valdata, n.trees = 8000, type = 'response')

output_boosting_train<-ifelse(prob_boosting_train<0.6, 0, 1)
output_boosting_val<-ifelse(prob_boosting_val<0.6, 0, 1)

confusionMatrix(output_boosting_val, valdata$band.type)

#Xgboost
colnames(traindata[,1:19])
boost_traindata<-cbind(traindata[,1:19],dummy.data.frame(traindata[,20:33]),band.type = traindata[,34])
boost_valdata<-cbind(valdata[,1:19],dummy.data.frame(valdata[,20:33]),band.type = valdata[,34])

library(xgboost)
model_xgboost<-xgboost(data = data.matrix(boost_traindata[,-67]), label = traindata$band.type, nrounds = 5, objective = "binary:logistic")
model_xgboost

prob_xgboost_train<-predict(model_xgboost, newdata = data.matrix(boost_traindata[,-67]))
prob_xgboost_val<-predict(model_xgboost, newdata = data.matrix(boost_valdata[,-62]))

output_xgboost_train<-ifelse(prob_xgboost_train<0.55, '0', '1')
output_xgboost_val<-ifelse(prob_xgboost_val<0.55, '0', '1')

confusionMatrix(output_xgboost_train, traindata$band.type)
confusionMatrix(output_xgboost_val, valdata$band.type)
