# Clear environment
rm(list=ls())

# Import libraries
library('MASS')
library('heatmaply')
library('psych')
library('factoextra')
library('caret')

# Import cleaned dataset from data cleaning file script
cleaned_df = read.csv("df.csv")
correlation_df = read.csv("df_cor.csv")

# View dataset
View(cleaned_df)

# View dataset
View(correlation_df)

# Statistics for whole dataframe
summary(cleaned_df)

# Statistics for correlation dataframe
summary(correlation_df)

# Dataset information
str(cleaned_df)

# Dataset information
str(correlation_df)

# Statistics for individual columns
summary(correlation_df$gender)

summary(correlation_df$race)

## Histogram analysis

hist(correlation_df$gender, xlab = "No.of Gender ",
     col = "green", border = "black")

hist(correlation_df$race, xlab = "No.of Races ",
     col = "red", border = "black")

hist(correlation_df$race,
     main="Histogram of Race categories",
     xlab="Race distribution",
     col="darkmagenta",
)

hist(correlation_df$gender,
     main="Histogram of Gender categories",
     xlab="Gender distribution",
     col="red",
)

# Histogram of gender column
hist.default(correlation_df$gender,col='blue')

# Histogram of race column
hist.default(correlation_df$race,col='blue')

# Histogram of insulin column
hist.default(correlation_df$insulin,col='blue')

# Frequency analysis of features
table(correlation_df$race)

table(correlation_df$gender)

table(correlation_df$insulin)

# Visualisation anlysis
plot(table(correlation_df$race))

plot(table(correlation_df$gender))

# Scatter plot of two features
plot(correlation_df$gender~correlation_df$race,col='blue')

plot(correlation_df$race~correlation_df$insulin,col='blue')

# T-test analysis
t.test(correlation_df$gender)

t.test(correlation_df$race)

t.test(correlation_df$insulin)

# Chi-square tests: Pearsons chi-squared test
ch_test_1 = data.frame(correlation_df$gender,correlation_df$insulin)           
ch_test_1 = table(correlation_df$gender,correlation_df$insulin)                
print(ch_test_1)

print(chisq.test(ch_test_1))

ch_test_2 = data.frame(correlation_df$race,correlation_df$insulin)           
ch_test_2 = table(correlation_df$race,correlation_df$insulin)                
print(ch_test_2)

print(chisq.test(ch_test_2))


#############################################################
# Clear environment
rm(list=ls())

# Read in file
st_dt = read.csv('diabetes_binary_health_indicators_BRFSS2015.csv')

# View dataframe
View(st_dt)

# Statistics for dataframe
summary(st_dt)

# Information about dataset
str(st_dt)

# Statistics for indiovudal columns
summary(st_dt$Diabetes_binary)

summary(st_dt$Smoker)

summary(st_dt$Age)

## Histogram analysis

# Histogram of Smoker column
hist.default(st_dt$Smoker,col='green')

# Histogram of race column
hist.default(st_dt$Age,col='green')

# Histogram of insulin column
hist.default(st_dt$Diabetes_binary,col='green')

# Visualisation anlysis
plot(table(st_dt$Smoker))

plot(table(st_dt$Age))

# Scatter plot of two features
plot(st_dt$Diabetes_binary~st_dt$Age,col='blue')
plot(st_dt$Diabetes_binary~st_dt$Smoker,col='blue')

# T-test analysis
t.test(st_dt$Smoker)

t.test(st_dt$Age)

t.test(st_dt$Diabetes_binary)

# Chi-square tests: Pearsons chi-squared test
ch_test_3 = data.frame(st_dt$Age,st_dt$Diabetes_binary)           
ch_test_3 = table(st_dt$Age,st_dt$Diabetes_binary)                
print(ch_test_3)

print(chisq.test(ch_test_3))

ch_test_4 = data.frame(st_dt$Smoker,st_dt$Diabetes_binary)           
ch_test_4 = table(st_dt$Smoker,st_dt$Diabetes_binary)                
print(ch_test_4)

print(chisq.test(ch_test_4))

###Heatmap Visualisation of dataframes

# plotting corr heatmap --- Binary dataset
heatmaply_cor(x = cor(st_dt), xlab = "Features",
              ylab = "Features", k_col = 2, k_row = 2)

# plotting corr heatmap --- Correlation dataset
heatmaply_cor(x = cor(correlation_df), xlab = "Features",
              ylab = "Features", k_col = 2, k_row = 2)

# plotting corr heatmap ---- Non-correlated dataset
heatmaply_cor(x = cor(cleaned_df), xlab = "Features",
              ylab = "Features", k_col = 2, k_row = 2)

## Cohen's Kappa score!
## split up my data into independent variables
X = correlation_df[c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
y = correlation_df[c(1)]

# One-Hot encoding
dummy <- dummyVars(" ~ .", data=X)

final_df <- data.frame(predict(dummy, newdata=X))
View(final_df)

dummy_2 <- dummyVars(" ~ .", data=y)

final_df_2 <- data.frame(predict(dummy_2, newdata=y))
View(final_df_2)

cohen.kappa(x=cbind(X,y))
