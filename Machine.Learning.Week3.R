#Predicting with trees
library(caret)
library(rattle)
library(ggplot2)
data(iris)
table(iris)
inTrain=createDataPartition(iris$Species,p=0.7,list=F)
training=iris[inTrain,]
testing=iris[-inTrain,]
qplot(Petal.Width,Sepal.Width,col=Species,data=training)

modFit=train(Species ~ ., method="rpart",data=training)  #rpart:regression and classification trees
print(modFit$finalModel)
fancyRpartPlot(modFit$finalModel)

predict(modFit,newdata=testing)


#Bagging (useful for nonlinear model)
library(ElemStatLearn)
data(ozone,package="ElemStatLearn")
ozone=ozone[order(ozone$ozone),]
table(ozone)
ll=matrix(NA,nrow=10,ncol=155)
for(i in 1:10) {
  ss=sample(1:dim(ozone)[1],replace=T)
  ozone0=ozone[ss,];
  ozone0=ozone0[order(ozone0$ozone),]
  loess0=loess(temperature ~ ozone,data=ozone0,span=0.2)
  ll[i,]=predict(loess0,newdata=data.frame(ozone=1:155))
  }

plot(ozone$ozone,ozone$temperature,pch=19,cex=.5)
for(i in 1:10){
  lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)


#Bagging in caret:train(bagEarth,treebag,badFDA) or bag
predictors=data.frame(ozone=ozone$ozone)
temperature=ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
                               bagControl = bagControl(fit = ctreeBag$fit,
                                                       predict = ctreeBag$pred,
                                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone,temperature,col="grey",pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")


#Random Forests (Cross-Validation very important)
library(caret)
library(rattle)
library(ggplot2)
data(iris)
table(iris)
inTrain=createDataPartition(iris$Species,p=0.7,list=F)
training=iris[inTrain,]
testing=iris[-inTrain,]
modFit=train(Species~ .,data=training,method="rf",prox=T)

#Getting a single tress
getTree(modFit$finalModel,k=2)

#Predicting new values
pred=predict(modFit,testing)
testing$predRight=pred==testing$Species
table(pred,testing$Species)
qplot(Petal.Width,Petal.Length,col=predRight,data=testing,main="newdata Predictions")


#Boosting
#1.Take lots of weak predictors
#2.Weight them and add them up
#3.Get a stronger predictor
#gbm:boosting with trees
#mboost:model based boosting
#ada:statistical boosting based on additive logistic regression
#gamBoost:for boosting generalized additive models
#Wage example
library(ggplot2)
library(caret)
library(ISLR)
data(Wage)
Wage=subset(Wage,select=-c(logwage))
inTrain=createDataPartition(Wage$wage,p=0.7,list=F)
training=Wage[inTrain,]
testing=Wage[-inTrain,]
modFit=train(wage ~ .,method="gbm",data=training,verbose=F)
qplot(predict(modFit,testing),wage,data=testing)



#Model Based Prediction
#1.Assume the data follow a probabillistic model
#2.use Bayes' theorem to identify optimal classifiers
library(caret)
library(ggplot2)
data(iris)
table(iris)
inTrain=createDataPartition(iris$Species,p=0.7,list=F)
training=iris[inTrain,]
testing=iris[-inTrain,]
modlda=train(Species~.,data=training,method="lda")
modnb=train(Species~.,data=training,method="nb")
plda=predict(modlda,testing)
pnb=predict(modnb,testing)
table(plda,pnb)
eq=(plda==pnb)
qplot(Petal.Width,Sepal.Width,col=eq,data=testing)





