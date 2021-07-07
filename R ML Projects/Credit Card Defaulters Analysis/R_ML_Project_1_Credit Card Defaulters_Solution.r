# Credit Card Defaulters Analysis


# Problem Statement : In this project we have to analyze or clean credit card defaulters data. Here
# we are performing task like replacing null values & some data manipulation.

#  Setting current working directory
setwd("C:/Users/gohil/Desktop/GOAL/FINGERTIPS/6. R/PROJECTS/Project-1 Credit Card Defaulters")

# Load data
df = read.csv(file = "R_ML_Project_1_Credit Card Defaulters.csv")
View(df)

# Data Pre-processing

nrow(df)
ncol(df)

names(df)

head(df, 5)

sum(is.na(df))
mean(is.na(df))

#1. Replace missing value with mean.
for (i in 1:ncol(df)) {
  df[is.na(df[,i]), i] = mean(df[,i], na.rm = TRUE)
}

library(imputeTS)
df= na_mean(df)
View(df)

#---------------------
#2. Drop default status column. (Last one)
df$Default.Status = NULL

#3. Select top 2000 rows.
head(df, 2000)

df[1:2000,]

#4. Select this column LIMIT_BAL, AGE, BILL_AMT1, PAY_AMT1
attach(df)
select(df,LIMIT_BAL,AGE,BILL_AMT1,PAY_AMT1)

#5. Select those data only who has , BILL_AMT1 more then 50000.
View(df %>%
  select(1:10) %>%
  filter(BILL_AMT1 > 50000))

#6. Check the correlation on BILL_AMT1 vs PAY_AMT1.
cor(df$BILL_AMT1, df$PAY_AMT1)

#7. Rename the column BILL_AMT1 as Bill_amount and PAY_AMT1 as pay_amount.
df = df %>% 
  rename(
    Bill_amount = BILL_AMT1,
    pay_amount = PAY_AMT1
  )
names(df)
#--------------------

df$AVG_BILL_AMT = df$BILL_AMT1 + df$BILL_AMT2 + df$BILL_AMT3 + df$BILL_AMT4 + df$BILL_AMT5 + df$BILL_AMT6
df$AVG_PAY_AMT = df$PAY_AMT1 + df$PAY_AMT2 + df$PAY_AMT3 + df$PAY_AMT4 + df$PAY_AMT5 + df$PAY_AMT6

df[4:15] <- NULL
df$ID <- NULL

#8. Modeling
names(df)
attach(df)
View(df)
table(df$Default.Status)

df <- df[sample(nrow(df)),]
View(df)

barplot(prop.table(table(df$Default.Status)),
        col = rainbow(2),
        ylim = c(0,0.8),
        main = "Class Distribution")

#Default = 1
#No Default = 0
df$Default.Status = ifelse(df$Default.Status == "Default", 1, 0)
View(Default.Status)

install.packages('dplyr')

# Train and Test Split
set.seed(42)
df_train = df%>%
  sample_frac(0.8)

df_test = df%>%
  anti_join(df_train)

table(df_train$Default.Status)
table(df_test$Default.Status)

install.packages('rpart')
install.packages('caret')
install.packages('e1071')
install.packages('ROSE')
install.packages('rpart')
library(caret)
library(e1071)

#logistic regression
logmodel = glm(Default.Status~., df_train,family=binomial(link="logit"))
logmodel

# View the forest results.
print(logmodel) 
summary(logmodel)

confusionMatrix(predict(logmodel, df_test), df_test$Default.Status)

#predicted class
pre=predict(logmodel,df_test, type = 'response')
pre
View(pre)
table(df_test$Default.Status,pre)

accuracy.meas(df_test$Default.Status,pre)

if(pre < 0.5){
  print("Predicted class is Not Default")
}else{
  print("Predicted class is Default")
}



#random forest
library(party)
library(randomForest)

rfmodel <- randomForest(Default.Status~.,df_train)

# View the forest results.
print(rfmodel)

pre=predict(rfmodel,df_test)
View(pre)


table(df_test$Default.Status,pre)

if(pre[11] < 0.5){
  print("Predicted class is Not Default")
}else{
  print("Predicted class is Default")
}

accuracy.meas(df_test$Default.Status,pre)
