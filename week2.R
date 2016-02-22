library(caret)
library(kernlab)
data(spam)
#split the data based on type
inTrain=createDataPartition(y=spam$type,p=0.75,list=FALSE)
training=spam[inTrain,]
testing=spam[-inTrain,]
set.seed(32343)

#Standardizing Variables(Preprocessing)
mean(training$capitalAve)
sd(training$capitalAve)   #we see small mean but much larger sd

trainCapAve=training$capitalAve
trainCapAveS=(trainCapAve-mean(trainCapAve))/sd(trainCapAve) #(take value -mean)/sd
mean(trainCapAveS)  #make the mean of the variables be 0
sd(trainCapAveS)    #make the sd be 1

testCapAve=testing$capitalAve
testCapAveS=(testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)


pre0bj=preProcess(training[,-58],method = c("center","scale"))
trainCapAveS=predict(pre0bj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS=predict(pre0bj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

set.seed(32343)
modelFit=train(type ~.,data=training, preProcess=c("center","scale"),method="glm")
modelFit













#K-fold validtion to split training set into smaller data sets
folds=createFolds(spam$type,k=10,list=T,returnTrain=T)
sapply(folds,length)

#Resampling
folds=createResample(spam$type,times=10,list=T)
sapply(folds,length)

#Times Slices
tme=1:1000      #create time vector
folds=createTimeSlices(tme,initialWindow=20,horizon=10) #create times
#slices that have a window of about 20 samples in them, and predict the 
#next 10 values out
names(folds)
folds$train[[1]]
folds$test[[1]]

modelFit=train(type ~.,data=training,method="glm")

predictions=predict(modelFit,testing)


#to test modelFit well or not,compare the predict outcome to the actual outcome
confusionMatrix(predictions,testing$type)
predict


#plotting predictors
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)
inTrain=createDataPartition(Wage$wage,p=0.7,list = F)
training=Wage[inTrain,]
testing=Wage[-inTrain,]
#feature plot
featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot = "pairs")

