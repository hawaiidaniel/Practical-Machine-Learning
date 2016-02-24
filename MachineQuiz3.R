#Q1
library(rattle)
library(AppliedPredictiveModeling)
data(segmentationOriginal)
suppressMessages(library(caret))
inTrain=createDataPartition(segmentationOriginal$Case,p=0.65,list=F)
training=segmentationOriginal[inTrain,]
testing=segmentationOriginal[-inTrain,]
set.seed(125)
modFit=train(Class ~ .,method="rpart",data=training)
fancyRpartPlot(modFit$finalModel)


#Q3
library(pgmm)
data(olive)
olive = olive[, -1]
inTrain=createDataPartition(olive$Area,p=0.65,list=F)
training=olive[inTrain,]
testing=olive[-inTrain,]
modFit=train(Area ~ .,method="rpart",data=training)
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit,newdata)


#Q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1], size = dim(SAheart)[1] / 2, replace = F)
trainSA = SAheart[train, ]
testSA = SAheart[-train, ]
set.seed(13234)
modFit=train(chd ~ age+alcohol+obesity+tobacco+typea+ldl,
             method="glm",family="binomial",data=trainSA)
missClass = function(values, prediction){
  sum(((prediction > 0.5) * 1) != values) / length(values)}
missClass(trainSA$chd,predict(modFit,newdata=trainSA))
missClass(testSA$chd,predict(modFit,newdata=testSA))


#Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y=as.factor(vowel.train$y)
vowel.test$y=as.factor(vowel.test$y)
set.seed(33833)
# modFit=train(y~ .,data=vowel.train,method="rf",prox=F)
# varImp(modFit,scale=F)
modFit=randomForest(y~.,data=vowel.train)
order(varImp(modFit,scale=F),decreasing = T)

