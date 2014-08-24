# install.packages("caret")
# install.packages("kernlab");
# install.packages("e1071");

set.seed(32323);

######################################
# intro

library(caret);
library(kernlab);
data(spam);


inTrain <- createDataPartition(y = spam$type, p=0.75, list=F)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

dim(training)

# now we try and predict the type using all the variables with the generalized linear model
modelFit <- train(type ~., data = training, method ="glm")
modelFit

modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)

######################################
# data splitting

inTrain <- createDataPartition(y = spam$type, p=0.75, list=F)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

dim(training)

folds <- createFolds(y=spam$type, k=10, list=T, returnTrain=T)
# here he is just checking the length of the folds created.
sapply(folds, length)

folds[[1]][1:10]

# here's how you get just the test set of folds, notice that returnTrain is now FALSE
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain=F)

# you can even do a bootstrapping or resampling approach for your training and test sets
folds <- createResample(y=spam$type, times=10, list=T)
# here he is just checking the length of the folds created.
sapply(folds, length)

# notice that because of resampling you can get duplicate samples
folds[[1]][1:10]

# here he shows how you can do timeslicing or windowing
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
folds$train[[1]]
folds$train[[21]]

##############################################
#  training options

inTrain <- createDataPartition(y = spam$type, p=0.75, list=F)

training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

dim(training)

# now we try and predict the type using all the variables with the generalized linear model
modelFit <- train(type ~., data = training, method ="glm")
modelFit

# dump the default train args
args(train.default)

##############################################
# plotting predictors
install.packages("ISLR")

library(ISLR);
library(ggplot2);
library(caret);

data(Wage);
summary(Wage);

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

dim(training);
dim(testing);

featurePlot(x=training[, c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")

qplot(age, wage, data=training)

qplot(age, wage, colour=jobclass, data=training)

qq <- qplot(age, wage, colour=education,data=training);
qq <- qq + geom_smooth(method='lm', formula=y~x);
print(qq)

library(Hmisc)

cutWage <- cut2(training$wage, g=3)
table(cutWage)

min(training$wage)
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"));
p1

p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"));
p2

install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, ncol=2)


t1 <- table(cutWage, training$jobclass)
t1

prop.table(t1, 1)

qplot(wage, colour=education, data=training, geom="density")

################################################################
# preprocessing data

set.seed(32323)
data(spam);

inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

hist(training$capitalAve, main="", xlab="avg. captical run length");

# it looks skewed
mean(training$capitalAve)
sd(training$capitalAve)

trainCapAve <- training$capitalAve;
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)

mean(trainCapAveS)
sd(trainCapAveS)

hist(trainCapAveS)
hist(trainCapAve)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

preObj <- preProcess(training[,-58], method=c("center","scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve

mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)

sd(testCapAveS)

set.seed(32343)
modelFit <- train(type~., data=training, preProcess=c("center", "scale"), method="glm")
modelFit

preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve

par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

# eg. how to replace missing values
# make some values NA
install.packages("RANN")
library(RANN)
set.seed(13343)

training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=.05) == 1
training$capAve[selectNA] <- NA

dim((subset(training, is.na(capAve) == F, select=c("capAve"))))

#impute and standardize 
preObj <- preProcess(training[,-58], method="knnImpute")
# bug in carrot? http://stackoverflow.com/questions/25220185/error-no-points-in-data-when-using-knnimpute-in-caret-package
capAve <- predict(preObj, training[,-58])$capAve

################################################################
# covariate creation

library(ISLR)
library(caret)
data(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F)

training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

nsv <- nearZeroVar(training, saveMetrics=T)
nsv


library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

model1 <- lm(wage ~  bs(age, df=3), data = Wage)

# require(stats); require(graphics)
# bs(women$height, df = 5)
# summary(fm1 <- lm(weight ~ bs(height, df = 5), data = women))
# 
# ## example of safe prediction
# plot(women, xlab = "Height (in)", ylab = "Weight (lb)")
# ht <- seq(57, 73, length.out = 200)
# lines(ht, predict(fm1, data.frame(height = ht)))

lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)

################################################################
# Processing with principal component analysis (PCA)

set.seed(32323)
data(spam);

inTrain <- createDataPartition(y=spam$type, p=0.75, list=F)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

# taking all spam features and checking if they are highly correlated
# he exludes 58 which is the variable we are predicting.
M <- abs(cor(training[,-58]))

# he then 0's out the diagnal in the matrix because everything is perfectly 
# correlated with itself
diag(M) <- 0

# now we find which variables are highly correlated, he uses a cuttoff of .8 
which(M > .8, arr.ind=T)

names(spam)[c(34, 32, 40)]

# now you can plot these together to see that they are highly correlated
plot(spam[,34], spam[,32])

plot(spam[,40], spam[,32])

plot(spam[,40], spam[,34])

# when you add them together and then plot them against their differences, you can see that Y (their differences) 
# does not explain much of the variability, it seems that most of the variability is explained when adding them together, 
# so perhaps a composite of the two (adding them together) is a better way to summarize the two variables.

X <- 0.71 * training$num415 + 0.71 * training$num857
Y <- 0.71 * training$num415 - 0.71 * training$num857

plot(X,Y)

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])

# PCA on spam data

typeColor <- ((spam$type=="spam")*1 + 1)

# he takes the log10 here to normalize the data that may be on different scales.
prComp <- prcomp(log10(spam[,-58]+1))

plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

# doing PCA using caret, again he does log here, it looks like he's adding one to avoid infinity values
preProc <- preProcess(log10(spam[,-58]+1), method="pca", pcaComp=2)

# with caret you have to then predict spam using the email features + the principal components
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col=typeColor)

# preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~ ., method="glm", data=trainPC)

# now we predict spam on the TEST set using a model (from the Training set) that has a reduced
# feature set via PCA (see above)
testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))

summary(modelFit)

