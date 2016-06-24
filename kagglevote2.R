
library(rpart)
library(Matrix)


#load the data
full_train <- read.csv("./kaggle/train2016.csv")
full_test <- read.csv("./kaggle/test2016.csv")
noparty <- names(full_train)[which(!names(full_train) %in% "Party")]
combined_df <- rbind(full_train[,noparty], full_test)

#fix income levels (they are out of order to begin with)
incomelevels <- levels(full_train$Income)
incomelevels <- c(incomelevels[7],incomelevels[3],incomelevels[4],incomelevels[5],incomelevels[2],incomelevels[6])
full_train$Income <- factor(full_train$Income, ordered = TRUE, levels = incomelevels)
full_test$Income <- factor(full_test$Income, ordered = TRUE, levels = incomelevels)

#Impute missing year of birth
missing_YOB <- subset(full_train, is.na(YOB) == TRUE)
full_train <- subset(full_train, is.na(YOB) == FALSE)

missing_test_YOB <- subset(full_test, is.na(YOB) == TRUE)
full_test <- subset(full_test, is.na(YOB) == FALSE)

YOB_tree <- rpart(YOB ~ EducationLevel + Gender + Income + HouseholdStatus, data = combined_df, method = "anova")
guess_YOB <- predict(YOB_tree, newdata = missing_YOB)
guess_test_YOB <- predict(YOB_tree, newdata = missing_test_YOB)

missing_YOB$YOB <- as.integer(guess_YOB)
full_train <- rbind(full_train, missing_YOB)

missing_test_YOB$YOB <- as.integer(guess_test_YOB)
full_test <- rbind(full_test, missing_test_YOB)

#impute missing values for income
missing_Income <- subset(full_train, is.na(Income) == TRUE)
full_train <- subset(full_train, is.na(Income) == FALSE)

missing_test_income <- subset(full_test, is.na(Income) == TRUE)
full_test <- subset(full_test, is.na(Income) == FALSE)

Income_tree <- rpart(Income ~ EducationLevel + Gender + HouseholdStatus + YOB, data = combined_df, method = "class")
guess_income <- predict(Income_tree, newdata = missing_Income, type = "class")
guess_test_income <- predict(Income_tree, newdata = missing_test_income, type = "class")

missing_Income$Income <- guess_income
full_train <- rbind(full_train, missing_Income)

missing_test_income$Income <- guess_test_income
full_test <- rbind(full_test, missing_test_income)

full_train$Age <- 2016 - full_train$YOB
full_test$Age <- 2016 - full_test$YOB

#remove some outliers
full_train <- subset(full_train, Age < 95)
full_train <- subset(full_train, Age > 5)


full_train$YOB <- NULL
full_test$YOB <- NULL





##################################################
### Beginning of actual XGBoost model building ###
##################################################

train <- full_train
test <- full_test

train$USER_ID <- NULL
test$USER_ID <- NULL

sparse_matrix <- sparse.model.matrix(Party ~ .-1, data = train)

output_vector <- train[,"Party"]

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

foo <- full_train

for( i in seq(1:108)){
    #foo[,i] <- factor(foo[,i], ordered = TRUE, levels = c(0,1))
    if (length((levels(foo[,i]))) == 3) {
        levels(foo[,i]) <- c(-1,0,1)
    }
    if (length((levels(foo[,i]))) == 2) {
        levels(foo[,i]) <- c(0,1)
    }
    
    if (max(foo[,i]) > 1)

    foo[,i] <- as.integer(foo[,i])    
}


