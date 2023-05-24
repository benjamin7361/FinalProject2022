# Clear environment
rm(list=ls())

# Import packages
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)
library(readxl)
library(DMwR2)
library(caTools)
library(tidyr)

# Import csv file as a pandas dataframe
df = read.csv("diabetic_data.csv")

# View dataframe
View(df)

# Cleaning function
clean_names=function(df){
  #names(df)
  names(df)=gsub(" ","_",names(df))
  names(df)=gsub("\\(","",names(df))
  names(df)=gsub("\\)","",names(df))
  names(df)=gsub("/","_",names(df))
  names(df)=gsub("\\.","_",names(df))
  names(df)=gsub("\\-","_",names(df))
  names(df)=gsub("\\?","",names(df))
  names(df)=gsub("\\%","",names(df))
  names(df)=gsub("\u20AC","Euro",names(df))
  names(df)=gsub("\\{","",names(df))
  names(df)=gsub("\\}","",names(df))
  #add x if starts with a digit
  names(df)[grep("^\\d",names(df))]=paste0("x",names(df[grep("^\\d",names(df))]))
  #head(df)
  return(df)
}

#Factor function
as.factor.all=function(DF){
  lapply(DF,as.factor)
}

# Replace all '?' values with 'unknown'.
df[df == '?'] <- "unknown"
df

# Categorical variables should be encoded
unique(df$race)
unique(df$weight) #10 resembles the unknown values
unique(df$gender)
unique(df$age)

# Factor the categorical columns to numeric
df$age <- as.numeric(factor(df$age))
df$weight <- as.numeric(factor(df$weight))
df$payer_code <- as.numeric(factor(df$payer_code))
df$medical_specialty <- as.numeric(factor(df$medical_specialty))
df$diag_1 <- as.numeric(factor(df$diag_1))
df$diag_2 <- as.numeric(factor(df$diag_2))
df$diag_3 <- as.numeric(factor(df$diag_3))
df$max_glu_serum <- as.numeric(factor(df$max_glu_serum))
df$A1Cresult <- as.numeric(factor(df$A1Cresult))
df$metformin <- as.numeric(factor(df$metformin))
df$repaglinide <- as.numeric(factor(df$repaglinide))
df$nateglinide <- as.numeric(factor(df$nateglinide))
df$chlorpropamide <- as.numeric(factor(df$chlorpropamide))
df$glimepiride <- as.numeric(factor(df$glimepiride))
df$acetohexamide <- as.numeric(factor(df$acetohexamide))
df$glipizide <- as.numeric(factor(df$glipizide))
df$glyburide <- as.numeric(factor(df$glyburide))
df$tolbutamide <- as.numeric(factor(df$tolbutamide))
df$pioglitazone <- as.numeric(factor(df$pioglitazone))
df$rosiglitazone <- as.numeric(factor(df$rosiglitazone))
df$acarbose <- as.numeric(factor(df$acarbose))
df$miglitol <- as.numeric(factor(df$miglitol))
df$troglitazone <- as.numeric(factor(df$troglitazone))
df$tolazamide <- as.numeric(factor(df$tolazamide))
df$examide <- as.numeric(factor(df$examide))
df$citoglipton <- as.numeric(factor(df$citoglipton))
df$insulin <- as.numeric(factor(df$insulin))
df$glyburide.metformin <- as.numeric(factor(df$glyburide.metformin))
df$glipizide.metformin <- as.numeric(factor(df$glipizide.metformin))
df$glimepiride.pioglitazone <- as.numeric(factor(df$glimepiride.pioglitazone))
df$metformin.rosiglitazone <- as.numeric(factor(df$metformin.rosiglitazone))
df$metformin.pioglitazone <- as.numeric(factor(df$metformin.pioglitazone))
df$change <- as.numeric(factor(df$change))
df$diabetesMed <- as.numeric(factor(df$diabetesMed))
df$readmitted <- as.numeric(factor(df$readmitted))

# Change datatype for all character colums containign numeric data
df$weight <- as.numeric(as.character(df$weight))
print(df)
sapply(df, class)


# Saving clean data as R data file 
# This will be used later in further analysis(except correlation analysis which a separate data set has been made for)!
save(df, file = "df.Rdata")

# Data will also be saved as a csv file so it can be used on other platforms like Power Bi, Python etc...
write.csv(df, 'df.csv')

## Note:
# This is the main data set being used for the project. The only exception for this copy is that this data set will not be used for analysis which requires having all features encoded. 
# Reason being is that binary features have to be kept because it is a binary analysis.
