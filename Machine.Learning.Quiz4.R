#Q1
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
set.seed(33833)
mod1=train(y~.,data=vowel.train,method="rf")
mod2=train(y~.,data=vowel.train,method="gbm")
pred1=predict(mod1,newdata=vowel.test)
pred2=predict(mod2,newdata=vowel.test)
all1=confusionMatrix(pred1,vowel.test$y)
all2=confusionMatrix(pred2,vowel.test$y)
pred=data.frame(pred1,pred2,y=vowel.test$y,agree=pred1==pred2)
accuracy=sum(pred1[pred$agree]==pred$y[pred$agree])/sum(pred$agree)
all1$overall[1]
all2$overall[1]
accuracy


#Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]
set.seed(62433)
mod_rf=train(diagnosis~.,data=training,method="rf")
mod_gbm=train(diagnosis~.,data=training,method="gbm")
mod_lda=train(diagnosis~.,data=training,method="lda")
pred_rf=predict(mod_rf,newdata=testing)
pred_gbm=predict(mod_gbm,newdata=testing)
pred_lda=predict(mod_lda,newdata=testing)
pred=data.frame(pred_rf,pred_gbm,pred_lda,diagnosis=testing$diagnosis)
mod_com=train(diagnosis~.,data=pred,method="rf")
pred_com=predict(mod_com,pred)
o1=confusionMatrix(pred_rf,testing$diagnosis)$overall[1]
o2=confusionMatrix(pred_gbm,testing$diagnosis)$overall[1]
o3=confusionMatrix(pred_lda,testing$diagnosis)$overall[1]
o4=confusionMatrix(pred_com,testing$diagnosis)$overall[1]
dat=data.frame(o1,o2,o3,o4)
colnames(dat)=c("rf","gbm","lda","stacked")
dat

#Q3
set.seed(3523)
library(elastincnet)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]
set.seed(233)
mod=train(CompressiveStrength~.,training,method="lasso")
plot.enet(mod$finalModel,xvar="penalty",use.color=T)


#Q4
library(forecast)
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012, ]
testing = dat[(year(dat$date)) > 2011, ]
tstrain = ts(training$visitsTumblr)
mod=bats(tstrain)
fcast=forecast(mod,level=95,h=dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
  dim(testing)[1]


#Q5
set.seed(3523)
library(forecast)
library(e1071)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]
set.seed(325)
mod=svm(CompressiveStrength ~., data=training)
pred=predict(mod,testing)
accuracy(pred,testing$CompressiveStrength)
