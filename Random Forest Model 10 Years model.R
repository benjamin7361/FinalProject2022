# Clear environment
rm(list=ls())

# Import libraries----
library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)
library(rpart)
library(rpart.plot)
library(randomForest)

# Read in prepared data
diab_data <- read.csv('df.csv')

# Quick view of the data
head(diab_data)

# Structure of data frame
str(diab_data)

# Drop redundant columns
diab_data <- diab_data[-1]
diab_data <- diab_data[-1]
diab_data <- diab_data[-1]

# Splitting data in train and test data
split <- sample.split(diab_data, SplitRatio = 0.8)
split
train <- subset(diab_data, split == "TRUE")
test <- subset(diab_data, split == "FALSE")

train$gender <- as.factor(train$gender)

# Random Forest Model----
set.seed(120)
classifier_RF = randomForest(x = train[-2], y = train$gender, ntree = 50)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-2])

# Confusion Matrix
confusion_mtx = table(test[, 2], y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)
