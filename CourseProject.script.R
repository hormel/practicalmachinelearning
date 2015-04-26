## 
## Johns Hopkins University / Coursera
## Practical Machine Learning
## Course Project
## April 2015
##
## This script implements the Prediction Assignment for the Johns Hopkins / Coursera Practical
## Machine Learning course.  It imports the Weight Lifting Exercise dataset from the UCI Human
## Activity Recognition database, trains a Random Forest for prediction of exercise correctness,
## and tests the model on the course testing dataset.
## 

## 
## 1. Read and clean the data by removing any variables which have more than half missing
##    values or are descriptive of the data collection.  This limits the data available
##    prediction to just features collected from the subject.
##
##    The script assumes the files are in the current working directory.
## 

library (caret)
library (randomForest)

dataTrainingRaw <- read.csv ("pml-training.csv", na.strings = "NA")
fKeep <- sapply (1:ncol (dataTrainingRaw), function (i) { if ( 0.5 < length (which (is.na (dataTrainingRaw [ , i]))) / nrow (dataTrainingRaw)) { FALSE } else { (((7 < i) & !is.factor (dataTrainingRaw [ , i])) | (names (dataTrainingRaw) [i] == "classe")) } })
dataTraining <- dataTrainingRaw [ , fKeep]

##
## 2. Segment the data for training (80%) and testing (20%) then train a random forest
##    with 50 trees per run.  This keeps the training time manageable and (after some
##    experimentation) preserves accuracy.  Training uses out-of-bag resampling and
##    performs 5 iterations
##
##    A seed is set for reproducibility, and variable importance is examined.
##

set.seed (314159)
indexTraining <- createDataPartition (dataTraining$classe, p = 0.80, list = FALSE)
trainingSet <- dataTraining [indexTraining, ]
testingSet <- dataTraining [-indexTraining, ]

startTime <- Sys.time ()

trainOptions <- trainControl(method = "oob", 5) 
model <- train (classe ~ ., method = "rf", data = trainingSet, ntree=50, trControl = trainOptions)

variableImportance <- varImp (model)$importance
varOrder <- order (-variableImportance [ , 1])
head (cbind (variable = names (trainingSet) [varOrder], importance = variableImportance [varOrder, 1]), 10)

endTime <- Sys.time ()
endTime - startTime

##
## 3. Apply the model to predict the held-back testing data, to estimate out-of-sample
##    accuracy.
##

predictRF <- predict (model, testingSet)
confusionMatrix (predictRF, testingSet$classe)
