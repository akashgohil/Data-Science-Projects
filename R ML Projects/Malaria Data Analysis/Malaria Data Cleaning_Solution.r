#----------------------------Malaria Data Prediction------------------------------------

# Setting current working directory
setwd("C:/Users/gohil/Desktop/GOAL/FINGERTIPS/6. R/PROJECTS/Project-3 Malaria Data Cleaning")

# Load data
df = read.csv("Malaria Data.csv")
View(df)

nrow(df)
ncol(df)

# Data Pre-processing

#1. Filter the Gujarat, Rajasthan and Maharashtra in state.
View(df %>% filter(df$State == "Gujarat" & df$State == "Maharashtra" & df$State == "Rajasthan"))

#2. Select the data where mortality is high and very high.
View(df %>% filter(df$mortality_category == "High"& df$mortality_category == "Very High"))
     
unique(df$State)

unique(df$mortality_category)

summary(df)

colnames(df)

is.na(df)


sum(is.na(df))
mean(is.na(df))


#3. Handle the missing values in mortality rate column .
for(i in 1:ncol(df)){
  df[is.na(df[,i]),i] = mean(df[,i], na.rm = TRUE)
}

#4. Rename the mortality column to mortality_category.
df <- df%>%rename(mortality_category = rate)

#Algorithms
#1. Classification on mortality column.
library(randomForest)
library(rpart)
library(rpart.plot)
model <- rpart(mortality_category~., df, method="class")
print(model)
rpart.plot(model)

#predicted class
pre=predict(model,df,type = "class")
View(pre)
table(df_test$Attrition,pre)

accuracy.meas(df_test$Attrition,pre)