library(caret)
data(faithful)
set.seed(333)
inTrain=createDataPartition(faithful$waiting,p=0.5,list=F)
trainFaith=faithful[inTrain,]
testFaith=faithful[-inTrain,]
plot(trainFaith$waiting,trainFaith$eruptions,xlab="Waiting",ylab="Duration")


#Linear Model
#ED=b0+b1WT+e   (Eruption Duration=constant+constant*Waiting Time+error term)
lm1=lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

#Model Fit
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,xlab="Waiting",ylab="Duration",col="blue")
lines(trainFaith$waiting,lm1$fitted.values,lwd=3)   #Lines, NOT line

#Predict a new value
coef(lm1)[1]+coef(lm1)[2]*80

newdata=data.frame(waiting=80)
predict(lm1,newdata)

#Plot Predictions
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,xlab="Waiting",ylab="Duration",col="blue")
lines(trainFaith$waiting,predict(lm1),lwd=3)   #Lines, NOT line
plot(testFaith$waiting,testFaith$eruptions,pch=19,xlab="Waiting",ylab="Duration",col="blue")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)   #Lines, NOT line

#Get training set/test set errors
#Calculate RMSE on training
sqrt(sum((lm1$fitted.values-trainFaith$eruptions)^2))
#fitted value were the predictions that we get on the training set


#Calculate RMSE on tests
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))
#use lm to predict,so now this is predicting values on the test set


#Predicting Intervals
pred1=predict(lm1,newdata=testFaith,internal="prediction")
ord=order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord,],pred1[ord,],type='l',col=c(1,2,2),
         lty=c(1,1,1),lwd=3)


#Same process with caret(line12)
modFit=train(eruptions ~ waiting,data = trainFaith, method="lm")
summary(modFit$finalModel)


#Multiple covariates, to identify which prodictor is the most important
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
Wage=subset(Wage,select=-c(logwage))
inTrain=createDataPartition(Wage$wage,p=0.7,list = F)
training=Wage[inTrain,]
testing=Wage[-inTrain,]

#Fit a linear model
#ED=b0+b1*age+b2*I(Jobclass="Information"+sum(rk*I(education=levelk)))
modFit=train(wage~age+jobclass+education,method="lm",data=training)
finMod=modFit$finalModel

plot(finMod,pch=19,cex=0.5,col="#00000010")
qplot(finMod$fitted,finMod$residuals,col=race,data=training)

#Predicted vs. truth in test set
pred=predict(modFit,testing)
qplot(wage,pred,col=year,data=testing)

#use all covariates
modFitAll=train(wage~.,data=training,method="lm")
pred=predict(modFitAll,testing)
qplot(wage,pred,data=testing)















