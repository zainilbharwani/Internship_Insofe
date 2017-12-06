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
# model_stepaic<-stepAIC(model_glm)
# model_stepaic

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
model_c50<-C5.0(x = traindata[,-34], y = traindata[,34], rules = F)
model_c50$rules
model_c50$trials

output_c50_train<-predict(model_c50, newdata = traindata[,-34])
output_c50_val<-predict(model_c50, newdata = valdata[,-34])

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
library(glmnet)
summary(traindata)
fit_lasso<-glmnet(x = as.matrix(traindata[,1:19]), y = traindata[,34], alpha = 1, family = 'binomial')
fit_lasso
summary(fit_lasso)
plot(fit_lasso, xvar = 'lambda')

coef(fit_lasso)
# predict(fit_lasso,as.matrix(traindata[,1:19]))
# Accuracy(traindata$band.type, )

####Ensemble Techniques####
##Stacking of C50, KNN, SVM models
stack_df_train<-data.frame(cbind('C50' = output_c50_train, 'KNN' = output_knn_train, 'SVM' = output_svm_train, 'Target' = traindata$band.type))

stack_df_train$C50<-ifelse(stack_df_train$C50==1,'band','noband')
stack_df_train$SVM<-ifelse(stack_df_train$SVM==1,'band','noband') 
stack_df_train$Target<-ifelse(stack_df_train$Target==1, 'band', 'noband')

stack_df_val<-data.frame(cbind('C50' = output_c50_val, 'KNN' = output_knn_val, 'SVM' = output_svm_val, 'Target' = valdata$band.type))

stack_df_val$C50<-ifelse(stack_df_val$C50==1,'band','noband')
stack_df_val$SVM<-ifelse(stack_df_val$SVM==1,'band','noband')
stack_df_val$Target<-ifelse(stack_df_val$Target==1,'band','noband')

#Using KNN as a classifier
model_stack_knn<-knn3(Target~., data=stack_df_train)
model_stack_knn

prob_stack_knn_train<-predict(model_stack_knn, newdata = stack_df_train)
output_stack_knn_train<-ifelse(prob_stack_knn_train[,1]>prob_stack_knn_train[,2], 'band', 'noband')

prob_stack_knn_val<-predict(model_stack_knn, newdata = stack_df_val)
output_stack_knn_val<-ifelse(prob_stack_knn_val[,1]>prob_stack_knn_val[,2], 'band', 'noband')

Accuracy(output_stack_knn_train,stack_df_train$Target)
Accuracy(output_stack_knn_val,stack_df_val$Target)

Recall(output_stack_knn_train,stack_df_train$Target)
Recall(output_stack_knn_val,stack_df_val$Target)

Precision(output_stack_knn_val,stack_df_val$Target)

##Stacking does not result in any significant improvement in the recall scores

##Random Forest
library(randomForest)
set.seed(256)
model_randomforest<-randomForest(band.type~., data = traindata, ntree = 200)
model_randomforest

plot(model_randomforest)

output_randomforest_train<-predict(model_randomforest, newdata = traindata)
output_randomforest_val<-predict(model_randomforest, newdata = valdata)

Accuracy(output_randomforest_train, traindata$band.type)
Accuracy(output_randomforest_val, valdata$band.type)

Recall(output_randomforest_train, traindata$band.type)
Recall(output_randomforest_val, valdata$band.type)

Precision(output_randomforest_val, valdata$band.type)

###Code for installing library reprtree
# options(repos='http://cran.rstudio.org')
# have.packages <- installed.packages()
# cran.packages <- c('devtools','plotrix','randomForest','tree')
# to.install <- setdiff(cran.packages, have.packages[,1])
# if(length(to.install)>0) install.packages(to.install)
# 
# library(devtools)
# if(!('reprtree' %in% installed.packages())){
#   install_github('araastat/reprtree')
# }
# for(p in c(cran.packages, 'reprtree')) eval(substitute(library(pkg), list(pkg=p)))

reprtree:::plot.getTree(model_randomforest, depth = 4)

#Using cforest()
library(party)

acc_cforest_train = 1:100
acc_cforest_val = 1:100
recall_cforest_train = 1:100
recall_cforest_val = 1:100
#recall_cforest_test = 1:100

for(ntree in 1:100){
  set.seed(256)
  model_cforest<-cforest(band.type~., data = traindata, controls=cforest_control(ntree=ntree))
  model_cforest
  
  output_cforest_train<-predict(model_cforest, newdata = traindata)
  output_cforest_val<-predict(model_cforest, newdata = valdata)
  
  acc_cforest_train[ntree]<-Accuracy(output_cforest_train, traindata$band.type)
  acc_cforest_val[ntree]<-Accuracy(output_cforest_val, valdata$band.type)
  
  recall_cforest_train[ntree]<-Recall(output_cforest_train, traindata$band.type)
  recall_cforest_val[ntree]<-Recall(output_cforest_val, valdata$band.type)
  
  # output_cforest_test<-predict(model_cforest, newdata = testdata)
  # recall_cforest_test[ntree]<-Recall(output_cforest_test, testdata$band.type)
}

max(recall_cforest_train)
max(recall_cforest_val)

plot(recall_cforest_train, type = 's', main = 'Recall vs Number of trees for train using cforest', xlab = 'Number of trees', ylab = 'Recall_train_pcadata', col = 'blue')
plot(recall_cforest_val, type = 's', main = 'Recall vs Number of trees for validation using cforest', xlab = 'Number of trees', ylab = 'Recall_val_pcadata', col = 'red')

###Boosting
library(mlbench)
library(gbm)
traindata$band.type<-ifelse(traindata$band.type == 'band', 0, 1)
valdata$band.type<-ifelse(valdata$band.type=='band', 0, 1)
model_boosting<-gbm(band.type~., data = traindata, n.trees = 15000)
model_boosting

prob_boosting_train<-predict.gbm(model_boosting, newdata = traindata, n.trees = 15000, type = 'response')
prob_boosting_val<-predict.gbm(model_boosting, newdata = valdata, n.trees = 15000, type = 'response')

output_boosting_train<-ifelse(prob_boosting_train<0.5, 0, 1)
output_boosting_val<-ifelse(prob_boosting_val<0.5, 0, 1)

Recall(output_boosting_train, traindata$band.type)
Accuracy(output_boosting_train, traindata$band.type)

Recall(output_boosting_val, valdata$band.type)
Accuracy(output_boosting_val, valdata$band.type)

# write.csv(finaldata, 'finaldata.csv', row.names = FALSE)
# write.csv(mydata_knn, 'mydata_knn.csv', row.names = FALSE)
# write.csv(mydata_central, 'mydata_central.csv', row.names = FALSE)
# write.csv(pca_data, 'pca_data.csv', row.names = FALSE)
