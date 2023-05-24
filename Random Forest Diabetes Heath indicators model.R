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
library(groupdata2)
library(caret)

# Read in prepared data
diab_data <- read.csv('df2.csv')

# Quick view of the data
head(diab_data)

# Structure of data frame
str(diab_data)

# Splitting data in train and test data
split <- sample.split(diab_data, SplitRatio = 0.8)
split
train <- subset(diab_data, split == "TRUE")
test <- subset(diab_data, split == "FALSE")

train$Diabetes_binary <- as.factor(train$Diabetes_binary)

# Random Forest Model----
set.seed(120)
classifier_RF = randomForest(x = train[-2], y = train$Diabetes_binary, ntree = 50)

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

################################################################
# Down-sampling the original data----

# Clear environment
rm(list=ls())

# Read in prepared data
diab_data <- read.csv('df2.csv')

# Downsample dataset
ds = downsample(diab_data, cat_col = "Diabetes_binary")

# Data Partition----
set.seed(120)
trainIndex2 <- createDataPartition(ds$Diabetes_binary, p = .8,
                                   list = FALSE,
                                   times = 1)
Train2 <- ds[ trainIndex2,]
Valid2 <- ds[-trainIndex2,]

# Checking proportions----
table(Train2$Diabetes_binary)
table(Valid2$Diabetes_binary)
proportions2 <- table(Train2$Diabetes_binary)/length(Train2$Diabetes_binary)
percentages2 <- proportions2*100
percentages2
proportions3 <- table(Valid2$Diabetes_binary)/length(Valid2$Diabetes_binary)
percentages3 <- proportions3*100
percentages3

# Factor target variable
Train2$Diabetes_binary <- as.factor(Train2$Diabetes_binary)

# Random Forest Model----
classifier_RF = randomForest(x = Train2[-2], y = Train2$Diabetes_binary, ntree = 50)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = Valid2[-2])

# Confusion Matrix
confusion_mtx = table(Valid2[, 2], y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)

#######################################################################
# Import libraries----
library(smotefamily)
library(fitdistrplus)

# Clear environment
rm(list=ls())

# Read in prepared data
diab_data <- read.csv('df2.csv')

# SMOTE sampling----
sm = SMOTE(X = diab_data, target = diab_data$Diabetes_binary, K = 2, dup_size = 0)
sm

# Checking distribution
normal_dist <- fitdist(sm$data$Diabetes_binary, "norm")
normal_dist
plot(normal_dist)

# Data Partition----
set.seed(120)
trainIndex3 <- createDataPartition(sm$data$Diabetes_binary, p = .8,
                                   list = FALSE,
                                   times = 1)
Train3 <- sm$data[ trainIndex3,]
Valid3 <- sm$data[-trainIndex3,]

# Checking proportions----
table(Train3$Diabetes_binary)
table(Valid3$Diabetes_binary)
proportions3 <- table(Train3$Diabetes_binary)/length(Train3$Diabetes_binary)
percentages3 <- proportions3*100
percentages3
proportions4 <- table(Valid3$Diabetes_binary)/length(Valid3$Diabetes_binary)
percentages4 <- proportions4*100
percentages4

# Factor target variable
Train3$Diabetes_binary <- as.factor(Train3$Diabetes_binary)

# Random Forest Model----
classifier_RF = randomForest(x = Train3[-2], y = Train3$Diabetes_binary, ntree = 50)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = Valid3[-2])

# Confusion Matrix
confusion_mtx = table(Valid3[, 2], y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)
