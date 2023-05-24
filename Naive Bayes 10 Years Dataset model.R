# Clear environment
rm(list=ls())

# Import libraries----
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Import csv file as a pandas data frame
df = read.csv("df.csv")

# Factoring features
df$race <- as.factor(df$race)
df$gender <- as.factor(df$gender)

# Box-plot analysis----
df %>%
  ggplot(aes(x=gender, y=age, fill = gender)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

df %>%
  ggplot(aes(x=gender, y=change, fill = gender)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

df %>%
  ggplot(aes(x=gender, y=race, fill = gender)) +
  geom_boxplot() +theme_bw()+
  ggtitle("Box Plot")

# Drop redundant columns
df <- df[-1]
df <- df[-1]
df <- df[-1]

# Data sampling----
set.seed(1234)
ind <- sample(2, nrow(df), replace = T, prob = c(0.8, 0.2))
train <- df[ind == 1,]
test <- df[ind == 2,]

# Model----
model <- naive_bayes(gender ~ ., data = train, usekernel = T) 
plot(model) 

# Remove gender from test
test_df <- test[, -2]

# Remove gender from train
train_df <- train[, -2]

# Predictions----
p <- predict(model, newdata = test_df, type = 'prob')
head(cbind(p, test[, 2]))

# Confusion matrix for train data----
p1 <- predict(model, train_df)
(tab1 <- table(p1, train$gender))

#### This concludes the models accuracy
1 - sum(diag(tab1)) / sum(tab1)

# Confusion matrix for test data----
p2 <- predict(model, test_df)
(tab2 <- table(p2, test$gender))

#### This concludes the models accuracy
1 - sum(diag(tab2)) / sum(tab2)
