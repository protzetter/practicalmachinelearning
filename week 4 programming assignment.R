# This is the R code for the week 4 programming assignment
# Patrick Rotzetter, December 2016

#Load required libraries
library(dplyr)
library(caret)
library(adabag)
library(rpart)
library(randomForest)

#Read training and test files

trainData<-read.csv("pml-training.csv",header=TRUE,sep = ",", na.strings = "#DIV/0!")
testData<-read.csv("pml-testing.csv",header=TRUE,sep = ",", na.strings = "#DIV/0!")

# Duplicate original data sets for data investigation
trainDataCheck<-trainData
testDataCheck<-testData

# the first 7 columns are not required for prediction purposes as they are timestamps or related to the test person

trainDataCheck<-select(trainDataCheck,-c(1:7))
testDataCheck<-select(testDataCheck,-c(1:7))
trainDataCheck[, 1:152] <- sapply(trainDataCheck[, 1:152], function(x) as.numeric(as.character(x)))
testDataCheck[, 1:152] <- sapply(testDataCheck[, 1:152], function(x) as.numeric(as.character(x)))

# Determine percentage  of NAs in each column
naPercentage <-sapply(trainDataCheck, function(y) sum(length(which(is.na(y))))/length(y))
# Most of columns with NAs show 97% or mroe NAs, so they can be removed from the data set

# Find columns with more than 97% NAs
AllNA <- which(naPercentage>0.97)

# Remove columns with more than 97% NAs from train and test data
trainData<-select(trainData,-c(1:7))
testData<-select(testData,-c(1:7))
trainData<-trainData[,-AllNA]
testData<-testData[,-AllNA]

# test on adaboost only for reference
#modBoost = boosting.cv( classe ~ ., data = trainData[],v=1000)

# Train a random forest model, cross validation will be done automatically using OOB observations
set.seed(1000)
modRF <- randomForest(classe ~ ., data = trainData, ntree = 100, mtry = 3, importance = TRUE)

# Predict using trained model
prediction <- predict(modRF, newdata = testData)

# write outcome in distincs files for the quizz
n = length(prediction)
for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(prediction[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}

