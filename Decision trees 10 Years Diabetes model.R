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

# Data partition----
set.seed(100)
ind <- sample(2, nrow(diab_data), replace = T, prob = c(0.8, 0.2))
train <- diab_data[ind == 1,]
test <- diab_data[ind == 2,]

# Fitting data to tree----
fit <- rpart(gender~., data = train, method = 'class', cp = 0.001)
rpart.plot(fit)

# Predicted model----
predict_model<-predict(fit, test[, -2], type="class")
m_at <- table(test$gender, predict_model)
m_at

# Accuracy of model----
accuracy <- sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', accuracy))
