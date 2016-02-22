# #Q1
# library(AppliedPredictiveModeling)
# data(AlzheimerDisease)
# adData=data.frame(diagnosis,predictors)
# inTrain=createDataPartition(diagnosis,p=0.5,list=F)
# training=adData[inTrain,]
# testing=adData[-inTrain,]



# #Q2
library(Hmisc)
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
qplot(seq(774),CompressiveStrength,data=training)


splitOn <- cut2(training$Age, g = 4)

splitOn <- mapvalues(splitOn, 
                     from = levels(factor(splitOn)), 
                     to = c("red", "blue", "yellow", "green"))


# automatically includes index of samples
plot(training$CompressiveStrength, col = splitOn)
     

#3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(log(training$Superplasticizer+1),breaks =20)

#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p=3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
ss <- training[,grep('^IL', x = names(training) )]
preProc <- preProcess(ss, method='pca', thresh=0.8, 
                      outcome=training$diagnosis)
preProc$rotation


IL_col_idx <- grep("^[Ii][Ll].*", names(training))
preObj <- preProcess(training[, IL_col_idx], method=c("center", "scale", "pca"), thresh=0.8)
preObj



#Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(3433)
IL <- grep("^IL", colnames(training), value=TRUE)
ILpredictors <- predictors[, IL]
newadData=data.frame(diagnosis,ILpredictors)
newinTrain = createDataPartition(newadData$diagnosis, p = 3/4)[[1]]
newtraining = newadData[ newinTrain,]
newtesting = newadData[-newinTrain,]

modFit=train(diagnosis~.,data=newtraining,method="glm")
pred=predict(modFit,newtesting)
c=confusionMatrix(pred,newtesting$diagnosis)
acc1=c$overall[1]
acc1

#PCA
modelFit=train(newtraining$diagnosis ~ .,method="glm",preProcess="pca",
               data=newtraining,trControl=trainControl(preProcOptions=list(thresh=0.8)))
pred2=predict(modelFit,newtesting)
c2=confusionMatrix(pred2,newtesting$diagnosis)
acc2=c2$overall[1]
acc2







     
     