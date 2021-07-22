#------------------------------Employee Attrition Analysis--------------------------

#Setting the path of directory
setwd('C:/Users/gohil/Desktop/GOAL/FINGERTIPS/6. R/PROJECTS/Project-2 Employee Attrition Analysis')


#Preparing required libraries
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

#Load the data
df=read.csv("Employee Attrition Analysis.csv")
View(df)

# Move target data to the end of the dataframe
df <- df%>%select(-one_of('Attrition'),one_of('Attrition'))
View(df)

summary(df)

introduce(df)

unique(df$Attrition)

# Class distribution of target data
barplot(prop.table(table(df$Attrition)),
        col=rainbow(2),
        ylim = c(0,1),
        main="Class Distribution")

prop.table(table(df$Attrition))

# Checking Null values in dataframe
sum(is.na(df))
mean(is.na(df))

anyNA(df)

df$Attrition[df$Attrition == 'Yes'] <- 1
df$Attrition[df$Attrition == 'No'] <- 0
str(df)
df$Attrition <- as.numeric(df$Attrition)
str(df)

View(df)

df[,c(2,4,5,6,7,8,11)] <- lapply(df[,c(2,4,5,6,7,8,11)], as.factor)

write.csv(df,"C:/Users/gohil/Desktop/GOAL/FINGERTIPS/6. R/PROJECTS/Project-2 Employee Attrition Analysis/cleanedfile.csv", row.names=FALSE)

# Train and Test Split
set.seed(42)
df_train = df%>%
  sample_frac(0.7)

df_test = df%>%
  anti_join(df_train)

dim(df_train)
dim(df_test)

#Model Training
model = glm(Attrition~.,data=df_train,family="binomial")
summary(model)

#Now, we are going to design the model by the "Stepwise selection" method to fetch 
#significant variables of the model. Execution of the code will give us a list of output 
#where the variables are added and removed based on our significance of the model. 
#The AIC value at each level reflects the goodness of the respective model. 
#As the value keeps dropping it leads to a better fitting logistic regression model.

model = step(object = model,direction = "both")
summary(model)

#From our above result we can see, Business travel, Distance from home, Environment satisfaction,
#Job involvement, Job satisfaction, Marital status, Number of companies worked, Over time, 
#Relationship satisfaction, Total working years, Years at the company, years since last promotion,
#years in the current role all these are most significant variables in determining employee attrition. 
#If the company mostly looks after these areas then there will be a lesser chance of losing an employee.

#Generating a ROC curve for training data
install.packages('pROC')
library(pROC)

troc = roc(response=model$y,predictor = model$fitted.values,plot=T)
troc$auc


trpred = ifelse(test=model$fitted.values>0.5,yes = 1,no=0)
table(model$y,trpred)


#Comparing the result with testing data
testpred=predict.glm(object=model,newdata=df_test,type = "response")
testpred
tsroc = roc(response=df_test$Attrition,predictor = testpred,plot=T)
tsroc$auc

#Creating the classification table for the testing data set
testpred=ifelse(test=testpred>0.5,yes=1,no=0)
table(df_test$Attrition,testpred)
