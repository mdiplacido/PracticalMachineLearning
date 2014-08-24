####################################
# predicting trees
library(caret)

data(iris)
library(ggplot2)
names(iris)

table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=.7, list=F)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

dim(training)
dim(testing)

# you can see 3 distinct clusters by colour here
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

# now we train a predictive model
modFit <- train(Species ~ ., method="rpart", data=training)
print(modFit$finalModel)

plot(modFit$finalModel, uniform=T, main="Classification Tree")
text(modFit$finalModel, use.n=T, all=T, cex=.8)

# prettier plot
install.packages("rattle")
install.packages("rpart.plot")
library(rattle)

fancyRpartPlot(modFit$finalModel)

# now that we have trained, we can predict
predict(modFit, newdata=testing)

####################################
# bagging
install.packages("ElemStatLearn")
library(ElemStatLearn)

data(ozone, package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# predict temperature as a function of Ozone
ll <- matrix(NA, nrow=10, ncol=155)

for(i in 1:10)
{
  # random sample with replacement, so row indexes can apper more than once (like bootstrapping)
  ss <- sample(1:dim(ozone)[1], replace=T)
  # get the ozone values
  ozone0 <- ozone[ss,]
  # order them by their ozone levels
  ozone0 <- ozone0[order(ozone0$ozone),]
  # create a smoothed predictor of tempurature as a function of ozone from the bootstrapped sample
  loess0 <- loess(temperature ~ ozone, data=ozone0, span=0.2)
  # now using our model from the i'th sampling of the data we try and predict the temperature of the original (unsampled) ozone dataset 
  # for a range of ozone values from 1 to 155.
  ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for (i in 1:10) { lines(1:155, ll[i,], col="grey", lwd=2)}
lines(1:155, apply(ll,2, mean), col="red", lwd=2)

####################################
# random forests

data(iris)
library(ggplot2)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

library(caret)
modFit <- train(Species~.,data=training,method="rf",prox=T)
modFit

# tried with 2 trees, 3, and 4. 3 trees produced the best
# accuracy.  
# Resampling results across tuning parameters:
#   
#   mtry  Accuracy  Kappa  Accuracy SD  Kappa SD
# 2     0.950     0.925  0.0289       0.0439  
# 3     0.951     0.926  0.0314       0.0476  
# 4     0.948     0.921  0.0312       0.0473  
# 
# Accuracy was used to select the optimal model using  the
# largest value.
# The final value used for the model was mtry = 3. 

getTree(modFit$finalModel, k=2)

# now you can plot the centers of the predictions 
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

pred <- predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species)

# showing the missed predicted values
qplot(Petal.Width, Petal.Length, colour=predRight, data=testing, main="newdata predictions")

####################################
# boosting example

library(ISLR)
data(Wage)
library(ggplot2)
library(caret)

# remove the variable we are trying to predict
Wage <- subset(Wage, select=-c(logwage))

inTrain <- createDataPartition(y=Wage$wage, p=.7, list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage ~ ., method="gbm", data=training, verbose=F)
print(modFit)

# now we can plot some predictions.  
# still some variability but we took a bunch of weak predictors and created a weighted average which produced
# a good predictive model

qplot(predict(modFit, testing), wage, data=testing)

###########################################################
# model based prediction using priors.

data(iris)
library(ggplot2)

table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=.7, list=F)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
dim(training)
dim(testing)

modlda = train(Species ~ ., data=training, method="lda")
modnb = train(Species ~ ., data=training, method="nb")
plda = predict(modlda,testing)
pnb = predict(modnb,testing)

# table just shows that the the lda (linear discriminant analysis) and nb (naieve bayes) are in agreement for the most part.
# even though the predictors are not necessariliy independent naieve bayes does pretty well.
table(plda,pnb)

# comparison of results
equalPredictions = (plda == pnb)
qplot(Petal.Width, Sepal.Width, colour=equalPredictions, data=testing)
