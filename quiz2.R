install.packages("AppliedPredictiveModeling")

##################################
# question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData <- data.frame(diagnosis, predictors)
trainIndex <- createDataPartition(diagnosis, p=0.50, list=F)
training <- adData[trainIndex, ]
testing <- adData[-trainIndex, ]

##################################
# question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4, groups=)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

library(Hmisc)
sapply(training[,1:8],cut2) -> training[,1:8]
sapply(training[,1:8],as.factor) -> training[,1:8]

par(mfcol=c(2,4))
for (i in 1:8) 
{
  plot(training$CompressiveStrength,
       main=names(training[i]),
       xlab="stepwise index",
       ylab="CompressiveStrength",
       col=training[,i])
}

###########################################################
#  question 3

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

par(mfrow=c(2,4))
for(i in 1:8)
{
  hist(training[,i], xlab="", main=names(training[i]))
}

min(training$Superplasticizer)

log10(training$Superplasticizer)


###########################################################
# question 4

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

cols <- which(substr(names(training), start=1, stop=2) == "IL")

# preprocessing with PCA method=c("center","scale")
# "center","scale",
preProc <- preProcess(training[,cols], method=c("pca"), thresh=.8)
preProc

###########################################################
# question 5 (continuing from 4)

# preprocessing with PCA continued...
trainPC <- predict(preProc, training[,cols])
modelFit <- train(training$diagnosis ~ ., method="glm", data=trainPC)

testPC <- predict(preProc, testing[,cols])
confusionMatrix(testing$diagnosis, predict(modelFit,testPC))

# now without PCA, same set of predictors, but no PCA
modelFitNoPCA <- train(training$diagnosis ~ ., method="glm", data=training[,cols])
confusionMatrix(testing$diagnosis, predict(modelFitNoPCA,testing[,cols]))



