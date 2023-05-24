# Clear environment
rm(list=ls())

# Import libraries----
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(groupdata2)
library(caret)

# Import csv file as a pandas dataframe
df = read.csv("df2.csv")

# Factoring features
df$Diabetes_binary <- as.factor(df$Diabetes_binary)

# Box-plot analysis----
df %>%
  ggplot(aes(x=Diabetes_binary, y=HighChol, fill = Diabetes_binary)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

df %>%
  ggplot(aes(x=Diabetes_binary, y=Stroke, fill = Diabetes_binary)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

df %>%
  ggplot(aes(x=Diabetes_binary, y=HeartDiseaseorAttack, fill = Diabetes_binary)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

# Data sampling----
set.seed(1234)
ind <- sample(2, nrow(df), replace = T, prob = c(0.8, 0.2))
train <- df[ind == 1,]
test <- df[ind == 2,]

# Factoring features
train$HighChol <- as.factor(train$HighChol)
train$Stroke <- as.factor(train$Stroke)
train$HeartDiseaseorAttack <- as.factor(train$HeartDiseaseorAttack)

test$HighChol <- as.factor(test$HighChol)
test$Stroke <- as.factor(test$Stroke)
test$HeartDiseaseorAttack <- as.factor(test$HeartDiseaseorAttack)

# Model----
model <- naive_bayes(Diabetes_binary ~ ., data = train, usekernel = T) 
plot(model) 

# Remove Diabetes Binary from test
test_df <- test[, -2]

# Remove Diabetes Binary from train
train_df <- train[, -2]

# Predictions----
p <- predict(model, test_df, type = 'prob')
head(cbind(p, test[, 2]))

# Confusion matrix for train data----
p1 <- predict(model, train_df)
(tab1 <- table(p1, train$Diabetes_binary))

#### This concludes the models accuracy
1 - sum(diag(tab1)) / sum(tab1)

# Confusion matrix for test data----
p2 <- predict(model, test_df)
(tab2 <- table(p2, test$Diabetes_binary))

#### This concludes the models accuracy
1 - sum(diag(tab2)) / sum(tab2)
############################################################
# Down-sampling----

# Clear environment
rm(list=ls())

# Import csv file as a pandas dataframe
df = read.csv("df2.csv")

# Downsample dataset
ds = downsample(df, cat_col = "Diabetes_binary")

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

# Factor other variables
Train2$HighChol <- as.factor(Train2$HighChol)
Train2$Stroke <- as.factor(Train2$Stroke)
Train2$HeartDiseaseorAttack <- as.factor(Train2$HeartDiseaseorAttack)

# Model----
model2 <- naive_bayes(Diabetes_binary ~ ., data = Train2, usekernel = T) 
plot(model2) 

# Factor Target variable
Valid2$Diabetes_binary <- as.factor(Valid2$Diabetes_binary)

# Factor other variables
Valid2$HighChol <- as.factor(Valid2$HighChol)
Valid2$Stroke <- as.factor(Valid2$Stroke)
Valid2$HeartDiseaseorAttack <- as.factor(Valid2$HeartDiseaseorAttack)

# Test set for predictions----
Valid2_test <- Valid2[,-2]

# Predictions----
p3 <- predict(model2, Valid2_test, type = 'class')
head(cbind(p3, Valid2[, 2]))

# Confusion matrix for train data----
p4 <- predict(model2, Train2[, -2])
(tab3 <- table(p4, Train2$Diabetes_binary))

#### This concludes the models accuracy
1 - sum(diag(tab3)) / sum(tab3)

# Confusion matrix for test data----
p5 <- predict(model2, Valid2_test)
(tab4 <- table(p5, Valid2$Diabetes_binary))

#### This concludes the models accuracy
1 - sum(diag(tab4)) / sum(tab4)

###############################################################
# Smote----

# Clear environment
rm(list=ls())

# Import csv file as a pandas dataframe
df = read.csv("df2.csv")

# Import libraries
library(smotefamily)

# SMOTE sampling----
sm = SMOTE(X = df, target = df$Diabetes_binary, K = 2, dup_size = 0)
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

# Factor target variable
Train3$Diabetes_binary <- as.factor(Train3$Diabetes_binary)

# Factor other variables
Train3$HighChol <- as.factor(Train3$HighChol)
Train3$Stroke <- as.factor(Train3$Stroke)
Train3$HeartDiseaseorAttack <- as.factor(Train3$HeartDiseaseorAttack)

# Model----
model3 <- naive_bayes(Diabetes_binary ~ ., data = Train3, usekernel = T) 
plot(model3) 

# Factor Target variable
Valid3$Diabetes_binary <- as.factor(Valid3$Diabetes_binary)

# Factor other variables
Valid3$HighChol <- as.factor(Valid3$HighChol)
Valid3$Stroke <- as.factor(Valid3$Stroke)
Valid3$HeartDiseaseorAttack <- as.factor(Valid3$HeartDiseaseorAttack)

# Test set for predictions----
Valid3_test <- Valid3[,-2]

# Predictions----
p6 <- predict(model3, Valid3_test, type = 'class')
head(cbind(p6, Valid3[, 2]))

# Confusion matrix for train data----
p7 <- predict(model3, Train3[, -2])
(tab5 <- table(p7, Train3$Diabetes_binary))

#### This concludes the models accuracy
1 - sum(diag(tab5)) / sum(tab5)

# Confusion matrix for test data----
p8 <- predict(model3, Valid3_test[, -2])
(tab6 <- table(p8, Valid3$Diabetes_binary))

#### This concludes the models accuracy
1 - sum(diag(tab6)) / sum(tab6)
