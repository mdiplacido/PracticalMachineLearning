setwd("C:\\Users\\marcodi\\Documents\\Practical Machine Learning\\Course Project")

reportBadColumns <- function(df, threshold, verbose=F)
{
  removeColumns <- c()
  columns <- names(df)
  for (i in 1:length(columns))
  {
    theColumn <- columns[i]
    percentMissing <- mean(is.na(df[,theColumn]) == T)
    if (percentMissing > threshold)
    {
      removeColumns[length(removeColumns) + 1] <- theColumn
      message <- paste("Column", theColumn)
      message <- paste(message, "exceeds the threshold at:")
      message <- paste(message, percentMissing)
      
      if (verbose)
      {
        print(message)
      }
    }
  }
  
  invisible(removeColumns)
}

getAllFactorColumns <- function(df, exclude)
{
  factors <- c()
  columns <- names(df)
  for (i in 1:length(columns))
  {
    if (is.factor(df[,i]) == T)
    {
      factors[length(factors) + 1] <- columns[i]
    }
  }
  
  invisible(factors[! factors %in% exclude])
}

columnsAsNumeric <- function(df, columnNames)
{
  allNames <- names(df)
  for (i in 1:length(columnNames))
  {
    columnName <- columnNames[i]
    columnName <- paste("^",columnName, sep="")
    columnName <- paste(columnName, "$", sep="")
    columnIndex <- grep(columnName, allNames)
    
    if (is.factor(df[,columnIndex]) == F)
    {
      warning(paste(columnName,"is not a factor type"))
    }
    
    df[,columnIndex] <- as.numeric(df[,columnIndex])
  }
  
  invisible(df)
}

mirrorColumns <- function (dfSource, dfTarget, skipRemove)
{
   targetNames <- names(dfTarget)
   sourceNames <- names(dfSource)
   
   commonCols <- intersect(sourceNames, targetNames)
   
   for (i in 1:length(commonCols))
   {
     columnName <- commonCols[i]
     columnName <- paste("^",columnName, sep="")
     columnName <- paste(columnName, "$", sep="")
     
     sourceIndex <- grep(columnName, sourceNames)
     targetIndex <- grep(columnName, targetNames)
     
     if (is.numeric(dfSource[,sourceIndex]) == T)
     {
       dfTarget[,targetIndex] <- as.numeric(dfTarget[,targetIndex])
     }
     else
     {
       warning(paste("don't know how to mirror column", columnName))
     }
   }
   
   commonCols = c(commonCols, skipRemove)
   dfTarget <- dfTarget[ , which(names(dfTarget) %in% commonCols)]
   
   invisible(dfTarget)
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


quiz <- read.csv(file="./pml-testing.csv", header=TRUE)
train <- read.csv('./pml-training.csv',header=TRUE)

# fixup types, some stuff gets setup as factor when it is really numeric
numericColumns <- c("total_accel_belt", "kurtosis_roll_belt", "kurtosis_picth_belt", "kurtosis_yaw_belt", 
                    "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt", "max_yaw_belt", "amplitude_yaw_belt", 
                    "kurtosis_roll_arm", "kurtosis_picth_arm", "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", 
                    "skewness_yaw_arm", "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                    "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell", "min_yaw_belt", "amplitude_yaw_dumbbell",
                    "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm", "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm",
                    "max_yaw_forearm", "min_yaw_forearm", "amplitude_yaw_forearm")

train <- columnsAsNumeric(df=train, numericColumns)

# step 1.  check to see which columns have more than 95% their values as NA.  
# lets remove those columns from *all* the training data (we have not split yet)
badColumns <- reportBadColumns(df=train, threshold=.95)
badQuizColumns <- reportBadColumns(df=quiz, threshold=.95)
allBadColumns <- unique(c(badColumns, badQuizColumns))

trainBadRemoved <- train[ , -which(names(train) %in% allBadColumns)]

# remove other useless columns 
# badColumns <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", )
str(trainBadRemoved)

table(trainBadRemoved$classe)

# set the random seed so we get reproducable results
set.seed(124537)

library(caret)
remainingFactorCols <- getAllFactorColumns(trainBadRemoved, exclude=c("classe"))

# get rid of any remaining factor columns for now.  we'll leave the classe which is what we are trying to predict
trainBadRemoved <- trainBadRemoved[ , -which(names(trainBadRemoved) %in% remainingFactorCols)]

# get rid of some more columns
trainBadRemoved <- trainBadRemoved[ , -which(names(trainBadRemoved) %in% c("X", "raw_timestamp_part_1", "raw_timestamp_part_2", "num_window"))]

inTrain <- createDataPartition(y=trainBadRemoved$classe, p=.7, list=F)

training <- trainBadRemoved[inTrain, ]
testing <- trainBadRemoved[-inTrain, ]

##############################################################
# try out random forest see how we do with one held out set

# currently at the last col position
classeColIdx <- ncol(training)
classeColIdx

# preprocessing step 1 impute the missing values, this also appears to center and scale the variables
# preProcKnn <- preProcess(training[,-classeColIdx], method="knnImpute")
# trainKnn <- predict(preProcKnn, training[,-classeColIdx])

# preProcPCA <- preProcess(trainKnn, method="pca")
preProcPCA <- preProcess(training[,-classeColIdx], method="pca")
trainPCA <- predict(preProcPCA, training[,-classeColIdx])

# now we train a random forest using the 36 components to predict the class
modelFit <- randomForest(training$classe ~ ., data=trainPCA)

# in sample error rate
confusionMatrix(training$classe, predict(modelFit,trainPCA))

# modelFit <- randomForest(training$classe ~ ., data=training)
# modelFit <- train(classe ~ ., method="rpart", data=training)

# theTree <- getTree(modelFit$finalModel)
# install.packages("rattle")
# install.packages("rpart.plot")
# library(rattle)
# fancyRpartPlot(modelFit)

# now we predict classe on the TEST set using a model (from the Training set) that has a reduced
# feature set via PCA (see above)

# preprocessing step 1 impute the missing values for test set, this also appears to center and scale the variables
# note: not sure if these need to be centered and scaled against the training set...
preProcKnnTest <- preProcess(testing[,-classeColIdx], method="knnImpute")
testKnn <- predict(preProcKnnTest, testing[,-classeColIdx])

# this part makes sense, we do prediction using the PCA from the training phase (preProcPCA)
# testPCA <- predict(preProcPCA, testKnn)
testPCA <- predict(preProcPCA, testing[,-classeColIdx])

# out of sample error rate
confusionMatrix(testing$classe, predict(modelFit,testPCA))

# confusionMatrix(testing$classe, predict(modelFit,testing))

# now lets predict the quiz set
# fixup the quiz data
quizFixed <- mirrorColumns(dfSource=testing, dfTarget=quiz, skipRemove=c("problem_id"))

# problem_id and classe are in the same column position
problemIdColIdx = length(names(quizFixed))
quizPCA <- predict(preProcPCA, quizFixed[,-problemIdColIdx])
answers <- predict(modelFit,quizPCA)
answers

pml_write_files(answers)


# modelFit <- train(training$classe ~ ., method="rf", data=trainPCA)


