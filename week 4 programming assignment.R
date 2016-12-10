# This is the R code for the week 4 programming assignment
# Patrick Rotzetter, December 2016

#Load required libraries
library(dplyr)
library(caret)

#Read training and test files

trainData<-read.csv("pml-training.csv",header=TRUE,sep = ",", na.strings = "#DIV/0!")
testData<-read.csv("pml-testing.csv",header=TRUE,sep = ",", na.strings = "#DIV/0!")

trainDataCheck<-trainData
testDataCheck<-testData

# the first 7 columns are not required for prediction purposes as they are timestamps or related to the test person

trainDataCheck<-select(trainDataCheck,-c(1:7))
testDataCheck<-select(testDataCheck,-c(1:7))
trainDataCheck[, 1:152] <- sapply(trainDataCheck[, 1:152], function(x) as.numeric(as.character(x)))
testDataCheck[, 1:152] <- sapply(testDataCheck[, 1:152], function(x) as.numeric(as.character(x)))

# Determine percentage  of NAs in each column
naPercentage <-sapply(trainDataCheck, function(y) sum(length(which(is.na(y))))/length(y))

# Find columns with more than 90% NAs
AllNA <- which(naPercentage>0.9)

# Remove columns with more than 90% NAs from train and test data
trainData<-select(trainData,-c(1:7))
testData<-select(testData,-c(1:7))
trainData<-trainData[,-AllNA]
testData<-testData[,-AllNA]

modForest<-train(classe ~ ., data = trainData, method = "rf", ntree = 100, importance=TRUE)
