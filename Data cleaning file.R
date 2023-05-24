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
library(readr)

# Import csv file as a pandas data frame
df = read.csv("diabetic_data.csv")

# View data frame
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

# Factor function
as.factor.all=function(DF){
  lapply(DF,as.factor)
}

# Replace all '?' values with 'unknown'.
df[df == '?'] <- NA
df

# Categorical variables should be encoded
unique(df$race)
unique(df$weight) #10 resembles the unknown values
unique(df$gender)
unique(df$age)

# Check for percentage of Na's within columns
colMeans(is.na(df))

## There are 7 columns with missing values 
## Columns that contain around 50% or more will be dropped from the data frame
## Columns that have lesser missing values will be kept but rows with missing values are removed

new_df <- subset(df, select = -c(6, 11, 12))

#view updated data frame
new_df

# The next step is to remove rows that contain  missing values 
transformed_df <- na.omit(new_df)
transformed_df

# Lets observe the unique values for the features and see if there are values that don't make sense
unique(transformed_df$encounter_id)             # Unique values
unique(transformed_df$patient_nbr)              # Unique values
unique(transformed_df$race)                     # Other value is debatable for keeping
unique(transformed_df$gender)                   # Unknown value may be dropped as it is too ambiguous
unique(transformed_df$age)                      # These should be encoded in a format that's understandable    
unique(transformed_df$admission_type_id)        # Unique values
unique(transformed_df$discharge_disposition_id) # Unique values
unique(transformed_df$admission_source_id)      # Unique values
unique(transformed_df$time_in_hospital)         # Unique values
unique(transformed_df$num_lab_procedures)       # Unique values
unique(transformed_df$num_procedures)           # Unique values
unique(transformed_df$num_medications)          # Unique values
unique(transformed_df$number_outpatient)
unique(transformed_df$number_emergency)
unique(transformed_df$number_inpatient)
unique(transformed_df$diag_1)
unique(transformed_df$diag_2)
unique(transformed_df$diag_3)
unique(transformed_df$number_diagnoses)
unique(transformed_df$max_glu_serum)
unique(transformed_df$A1Cresult)
unique(transformed_df$metformin)
unique(transformed_df$repaglinide)
unique(transformed_df$nateglinide)
unique(transformed_df$chlorpropamide)
unique(transformed_df$glimepiride)
unique(transformed_df$acetohexamide)
unique(transformed_df$glipizide)
unique(transformed_df$glyburide)
unique(transformed_df$tolbutamide)
unique(transformed_df$pioglitazone)
unique(transformed_df$rosiglitazone)
unique(transformed_df$acarbose)
unique(transformed_df$miglitol)
unique(transformed_df$troglitazone)
unique(transformed_df$tolazamide)
unique(transformed_df$examide)
unique(transformed_df$citoglipton)
unique(transformed_df$insulin)
unique(transformed_df$glyburide.metformin)
unique(transformed_df$glipizide.metformin)
unique(transformed_df$glimepiride.pioglitazone)
unique(transformed_df$metformin.rosiglitazone)
unique(transformed_df$metformin.pioglitazone)
unique(transformed_df$change)
unique(transformed_df$diabetesMed)
unique(transformed_df$readmitted)

# Note: Race stays the same but categories are encoded

transformed_df$race = factor(transformed_df$race,
                   levels = c('Caucasian', 'AfricanAmerican', 'Other', 'Asian', 'Hispanic'),
                   labels = c(1, 2, 3, 4, 5))
transformed_df$race <- as.numeric(factor(transformed_df$race))

table(transformed_df$gender)


# remove rows where column 'gender' is equal to Unknown/Invalid
new_df <- subset(transformed_df, gender != "Unknown/Invalid") 

# view updated data frame
new_df

View(new_df)

str(new_df)

# Encoding the ranges in age column
new_df$age = factor(new_df$age,
                             levels = c('[10-20)', '[20-30)', '[30-40)', '[40-50)', '[50-60)', '[60-70)', '[70-80)', '[80-90)', '[90-100)', '[0-10)'),
                             labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
new_df$age <- as.numeric(factor(new_df$age))

# See stats after cleaning 
summary(new_df)

head(new_df)

str(new_df)

min(new_df$gender)
max(new_df$gender)

# Encoding of character variables and datatype changing
new_df$gender = factor(new_df$gender,
                             levels = c('Female', 'Male'),
                             labels = c(1, 2))
new_df$gender <- as.numeric(factor(new_df$gender))

new_df$max_glu_serum = factor(new_df$max_glu_serum,
                             levels = c('None', '>300', 'Norm', '>200'),
                             labels = c(1, 2, 3, 4))
new_df$max_glu_serum <- as.numeric(factor(new_df$max_glu_serum))

new_df$A1Cresult = factor(new_df$A1Cresult,
                             levels = c('None', '>7', '>8', 'Asian'),
                             labels = c(1, 2, 3, 4))
new_df$A1Cresult <- as.numeric(factor(new_df$A1Cresult))

new_df$metformin = factor(new_df$metformin,
                             levels = c('No', 'Steady', 'Up', 'Down'),
                             labels = c(1, 2, 3, 4))
new_df$metformin <- as.numeric(factor(new_df$metformin))

new_df$repaglinide = factor(new_df$repaglinide,
                             levels = c('No', 'Up', 'Steady', 'Down'),
                             labels = c(1, 2, 3, 4))
new_df$repaglinide <- as.numeric(factor(new_df$repaglinide))

new_df$nateglinide = factor(new_df$nateglinide,
                             levels = c('No', 'Steady', 'Down', 'Up'),
                             labels = c(1, 2, 3, 4))
new_df$nateglinide <- as.numeric(factor(new_df$nateglinide))

new_df$chlorpropamide = factor(new_df$chlorpropamide,
                             levels = c('No', 'Steady', 'Down', 'Up'),
                             labels = c(1, 2, 3, 4))
new_df$chlorpropamide <- as.numeric(factor(new_df$chlorpropamide))

new_df$glimepiride = factor(new_df$glimepiride,
                             levels = c('No', 'Steady', 'Down', 'Up'),
                             labels = c(1, 2, 3, 4))
new_df$glimepiride <- as.numeric(factor(new_df$glimepiride))

new_df$acetohexamide = factor(new_df$acetohexamide,
                            levels = c('No', 'Steady'),
                            labels = c(1, 2))
new_df$acetohexamide <- as.numeric(factor(new_df$acetohexamide))

new_df$glipizide = factor(new_df$glipizide,
                              levels = c('No', 'Steady', 'Up', 'Down'),
                              labels = c(1, 2, 3, 4))
new_df$glipizide <- as.numeric(factor(new_df$glipizide))

new_df$glyburide = factor(new_df$glyburide,
                          levels = c('No', 'Steady', 'Up', 'Down'),
                          labels = c(1, 2, 3, 4))
new_df$glyburide <- as.numeric(factor(new_df$glyburide))

new_df$tolbutamide = factor(new_df$tolbutamide,
                          levels = c('No', 'Steady'),
                          labels = c(1, 2))
new_df$tolbutamide <- as.numeric(factor(new_df$tolbutamide))

new_df$pioglitazone = factor(new_df$pioglitazone,
                            levels = c('No', 'Steady', 'Up', 'Down'),
                            labels = c(1, 2, 3, 4))
new_df$pioglitazone <- as.numeric(factor(new_df$pioglitazone))

new_df$rosiglitazone = factor(new_df$rosiglitazone,
                             levels = c('No', 'Steady', 'Up', 'Down'),
                             labels = c(1, 2, 3, 4))
new_df$rosiglitazone <- as.numeric(factor(new_df$rosiglitazone))

new_df$acarbose = factor(new_df$acarbose,
                              levels = c('No', 'Steady', 'Up', 'Down'),
                              labels = c(1, 2, 3, 4))
new_df$acarbose <- as.numeric(factor(new_df$acarbose))

new_df$miglitol = factor(new_df$miglitol,
                         levels = c('No', 'Steady', 'Down', 'Up'),
                         labels = c(1, 2, 3, 4))
new_df$miglitol <- as.numeric(factor(new_df$miglitol))

new_df$troglitazone = factor(new_df$troglitazone,
                         levels = c('No', 'Steady'),
                         labels = c(1, 2))
new_df$troglitazone <- as.numeric(factor(new_df$troglitazone))

new_df$tolazamide = factor(new_df$tolazamide,
                             levels = c('No', 'Steady', 'Up'),
                             labels = c(1, 2, 3))
new_df$tolazamide <- as.numeric(factor(new_df$tolazamide))

new_df$examide = factor(new_df$examide,
                           levels = c('No'),
                           labels = c(1))
new_df$examide <- as.numeric(factor(new_df$examide))

new_df$citoglipton = factor(new_df$citoglipton,
                        levels = c('No'),
                        labels = c(1))
new_df$citoglipton <- as.numeric(factor(new_df$citoglipton))

new_df$insulin = factor(new_df$insulin,
                            levels = c('Up', 'No', 'Steady', 'Down'),
                            labels = c(1, 2, 3, 4))
new_df$insulin <- as.numeric(factor(new_df$insulin))

new_df$glyburide.metformin = factor(new_df$glyburide.metformin,
                        levels = c('No', 'Steady', 'Down', 'Up'),
                        labels = c(1, 2, 3, 4))
new_df$glyburide.metformin <- as.numeric(factor(new_df$glyburide.metformin))

new_df$glipizide.metformin = factor(new_df$glipizide.metformin,
                                    levels = c('No', 'Steady'),
                                    labels = c(1, 2))
new_df$glipizide.metformin <- as.numeric(factor(new_df$glipizide.metformin))

new_df$glimepiride.pioglitazone = factor(new_df$glimepiride.pioglitazone,
                                    levels = c('No', 'Steady'),
                                    labels = c(1, 2))
new_df$glimepiride.pioglitazone <- as.numeric(factor(new_df$glimepiride.pioglitazone))

new_df$metformin.rosiglitazone = factor(new_df$metformin.rosiglitazone,
                                         levels = c('No'),
                                         labels = c(1))
new_df$metformin.rosiglitazone <- as.numeric(factor(new_df$metformin.rosiglitazone))

new_df$metformin.pioglitazone = factor(new_df$metformin.pioglitazone,
                                         levels = c('No', 'Steady'),
                                         labels = c(1, 2))
new_df$metformin.pioglitazone <- as.numeric(factor(new_df$metformin.pioglitazone))

new_df$change = factor(new_df$change,
                                       levels = c('Ch', 'No'),
                                       labels = c(1, 2))
new_df$change <- as.numeric(factor(new_df$change))

new_df$diabetesMed = factor(new_df$diabetesMed,
                       levels = c('Yes', 'No'),
                       labels = c(1, 2))
new_df$diabetesMed <- as.numeric(factor(new_df$diabetesMed))

new_df$readmitted = factor(new_df$readmitted,
                            levels = c('>30', 'NO', '<30'),
                            labels = c(1, 2, 3))
new_df$readmitted <- as.numeric(factor(new_df$readmitted))

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

# Change datatype for all character column containing numeric data
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



#####################################################################################################
# Clear environment
rm(list=ls())

#Import binary dataset
# Import csv file as a pandas dataframe
df2 = read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# View dataframe
View(df2)

# Replace all '?' values with 'unknown'.
df2[df2 == '?'] <- "unknown"
df2

# All features are in numeric format which means all data cleansing changes can be saved
save(df2, file = "df2.Rdata")

#Save as a csv file
write.csv(df2, 'df2.csv')

