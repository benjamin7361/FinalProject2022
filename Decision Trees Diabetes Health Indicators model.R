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
library(caret)
library(groupdata2)


# Read in prepared data
diab_data <- read.csv('df2.csv')

# Quick view of the data
head(diab_data)

# Structure of data frame
str(diab_data)

# Model 1: Regular Splitting----

# Data Partition----
set.seed(120)
trainIndex <- createDataPartition(diab_data$Diabetes_binary, p = .8,
                                  list = FALSE,
                                  times = 1)
Train <- diab_data[ trainIndex,]
Valid <- diab_data[-trainIndex,]

# Checking for percentage distribution of the classes for target variable 
# in train and test
table(Train$Diabetes_binary)
table(Valid$Diabetes_binary)
proportions <- table(Train$Diabetes_binary)/length(Train$Diabetes_binary)
percentages <- proportions*100
percentages
proportions1 <- table(Valid$Diabetes_binary)/length(Valid$Diabetes_binary)
percentages1 <- proportions1*100
percentages1

# Fitting data to tree----
fit <- rpart(Diabetes_binary~., data = Train, method = 'class', cp = 0.001)
rpart.plot(fit)

# Predicted model----
predict_model<-predict(fit, Valid[, -2], type="class")
m_at <- table(Valid$Diabetes_binary, predict_model)
m_at

# Accuracy of model----
accuracy <- sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', accuracy))

############################################################################
# Model 2: Down-sampling original data----
# Steps: 
#      1). Down-sample the data
#      2). Data Partition
#      3). Checking percentage proportions of unique classes in train and valid
#      4). Modelling
#      5). Predictions
#      6). Accuracy

# Clear environment
rm(list=ls())

# Read in prepared data
diab_data <- read.csv('df2.csv')

# Down-sampling the original data----
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

# Fitting data to tree----
fit2 <- rpart(Diabetes_binary~., data = Train2, method = 'class', cp = 0.001)
rpart.plot(fit2)

# Predicted model----
predict_model2<-predict(fit2, Valid2[, -2], type="class")
m_at2 <- table(Valid2$Diabetes_binary, predict_model2)
m_at2

# Accuracy of model----
accuracy2 <- sum(diag(m_at2)) / sum(m_at2)
print(paste('Accuracy for test is found to be', accuracy2))

############################################################################
# Model 3: Smote sampled data----
# Import libraries----
library(smotefamily)

# Clear environment
rm(list=ls())

# Read in prepared data
diab_data <- read.csv('df2.csv')

# SMOTE sampling----
sm = SMOTE(X = diab_data, target = diab_data$Diabetes_binary, K = 2, dup_size = 0)
sm

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

# Fitting data to tree----
fit3 <- rpart(Diabetes_binary~., data = Train3, method = 'class', cp = 0.001)
rpart.plot(fit3)

# Predicted model----
predict_model3<-predict(fit3, Valid3[, -2], type="class")
m_at3 <- table(Valid3$Diabetes_binary, predict_model3)
m_at3

# Accuracy of model----
accuracy3 <- sum(diag(m_at3)) / sum(m_at3)
print(paste('Accuracy for test is found to be', accuracy3))
