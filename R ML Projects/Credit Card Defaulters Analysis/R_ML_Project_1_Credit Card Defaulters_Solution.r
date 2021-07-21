#-----------------------------------Credit Card Defaulters Analysis------------------------------------


# Problem Statement : In this project we have to analyze or clean credit card defaulters data. Here
# we are performing task like replacing null values & some data manipulation.

#  Setting current working directory
setwd("C:/Users/gohil/Desktop/GOAL/FINGERTIPS/6. R/PROJECTS/Project-1 Credit Card Defaulters/")

library(knitr)
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("mice")
library(mice)
library(lattice)
library(reshape2)
install.packages("DataExplorer")
library(DataExplorer)
library(rpart)
library(ROSE)
library(dplyr)

#Load Data
df = read.csv("Credit_Card_Data.csv")
View(df)

# Data Pre-processing

#Since the header names are in the first row of the dataset, we will assign the header name to be one from the first row and then delete the first row from dataset.
colnames(df) <- as.character(unlist(df[1,]))
df <- df[-1,]
View(df)

nrow(df)
ncol(df)

names(df)

# We'll rename our target variable
colnames(df)[colnames(df) == "default payment next month"] <- "default_payment"

#checking if there any null values in our dataset
sum(is.na(df))
mean(is.na(df))

#-------------------------------------
#1. Replace missing value with mean.
for (i in 1:ncol(df)) {
  df[is.na(df[,i]), i] = mean(df[,i], na.rm = TRUE)
}

library(imputeTS)
df= na_mean(df)
View(df)
#-------------------------------------

dim(df)

str(df)

df[,1:25] <- sapply(df[,1:25], as.character)
str(df)

df[,1:25] <- sapply(df[,1:25], as.numeric)
str(df)

summary(df)

# Basic information about the dataframe
introduce(df)

View(df)

#EDUCATION: "Education (1 = graduate school; 2 = university; 3 = high school; 4 = others)"
count(df, vars= EDUCATION)

#MARRIAGE: "Marital status (1 = married; 2 = single; 3 = others)
count(df, vars=MARRIAGE)

# We'll replace 0's with NaN, and replace others too
df$EDUCATION[df$EDUCATION == 0] <- 4
df$EDUCATION[df$EDUCATION == 5] <- 4
df$EDUCATION[df$EDUCATION == 6] <- 4
df$MARRIAGE[df$MARRIAGE == 0] <- 3

count(df, vars=EDUCATION)
count(df, vars=MARRIAGE)

plot_correlation(df)

plot_histogram(df)


#(Feature engineering)-- deleting Weak correlated columns
df_new <- select(df, -one_of('ID','AGE','BILL_AMT1','BILL_AMT2','BILL_AMT3','BILL_AMT4','BILL_AMT5','BILL_AMT6'))
View(df_new)

# Standardization -- we'll use the scale method transform our dataset using it.
df_new[,1:16] <- scale(df_new[,1:16])
View(df_new)

# Train and Test Split
set.seed(42)
df_train = df_new%>%
  sample_frac(0.7)

df_test = df_new%>%
  anti_join(df_train)

dim(df_train)
dim(df_test)


#Model Development
## fit a logistic regression model with the training dataset
log_model <- glm(default_payment~. , data = df_train, family = binomial(link = "logit"))
summary(log_model)

#Prediction
df_test[1:10,]

## to predict using logistic regression model, probabilities obtained
log_predictions <- predict(log_model, df_test, type="response")

## Look at probability output
head(log_predictions, 10)

log_predictions_rd <- ifelse(log_predictions > 0.5, 1 ,0)
head(log_predictions_rd,10)

View(log_predictions)

table(log_predictions_rd,df_test[,17])

accuracy <- table(log_predictions_rd, df_test[,17])
cat("Model Accuracy:",sum(diag(accuracy)/sum(accuracy)))

accuracy.meas(df_test$default_payment,log_predictions)
