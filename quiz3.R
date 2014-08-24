library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

####################
# q1
names(segmentationOriginal)

table(segmentationOriginal$Case)

# 1. subset the data
training <- subset(segmentationOriginal, subset=Case == "Train")
testing <- subset(segmentationOriginal, subset=Case == "Test")

# 2. set the seed
set.seed(125)

# 3. train
modFit <- train(Class ~ ., method="rpart", data=training)
print(modFit$finalModel)
fancyRpartPlot(modFit$finalModel)

#####################
# q3
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]

modFit <- train(Area ~ ., method="rpart", data=olive)
fancyRpartPlot(modFit$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
predict(modFit, newdata=newdata)

#####################
# q4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method="glm", data=trainSA, family="binomial")

predictionsTest <- predict(modFit, testSA)
predictionsTrain <- predict(modFit, trainSA)

missClass(values=testSA$chd, prediction=predictionsTest)
missClass(values=trainSA$chd, prediction=predictionsTrain)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

#####################
# q5

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train
vowel.test

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

str(vowel.train)
str(vowel.test)

set.seed(33833)

modelRf <- randomForest(y ~ ., data=vowel.train, importance=F)
varImp(modelRf)

modelFit <- train(y ~ ., method="rf", data=vowel.train)
varImpResult <- varImp(modelRf)

sorted <- order(varImpResult$Overall, decreasing=T)

varImpResult$row.name

varImpResult[with(varImpResult, order(-Overall)), ]

varImpResult[sorted, c(0,1)]

