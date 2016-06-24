library(caTools)
library(party)

library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(corrplot)

full_train <- read.csv("./kaggle/train2016.csv")
full_test <- read.csv("./kaggle/test2016.csv")
Party <- full_train$Party
full_train$Party <- NULL

full_train$training <- TRUE
full_test$training <- FALSE

full_train$Party <- Party
full_test$Party <- Party[1:nrow(full_test)]


full <- rbind(full_train, full_test)

factormap <- function(n) {
    if (n == 1) return (0)
    if (n == 2) return (-1)
    if (n == 3) return (1)
}

#full[,7] first question in survey
#full[,107] last question in survey

for (i in seq(7,107)){
    if(!is.factor(full[,i])) break
    foo <- as.numeric(full[,i])
    for (j in seq(1, length(foo))){
        foo[j] <- factormap(foo[j])
    }
    full[,i] <- foo
}

#sum up the values for no apparent reason
full$sum <- rowSums(full[,7:107])

#Fix missing values (imputation)

missing_YOB <- subset(full, is.na(YOB) == TRUE)
full <- subset(full, is.na(YOB) == FALSE)

YOB_tree <- rpart(YOB ~ EducationLevel + Gender + Income + HouseholdStatus + sum, data = full, method = "anova")
guess_YOB <- predict(YOB_tree, newdata = missing_YOB)

missing_YOB$YOB <- as.integer(guess_YOB)
full <- rbind(full, missing_YOB)

#impute missing values for income
missing_Income <- subset(full, is.na(Income) == TRUE)
full <- subset(full, is.na(Income) == FALSE)

Income_tree <- rpart(Income ~ EducationLevel + Gender + HouseholdStatus + YOB + sum, data = full, method = "class")
guess_income <- predict(Income_tree, newdata = missing_Income, type = "class")

missing_Income$Income <- guess_income
full <- rbind(full, missing_Income)

#replace YOB with Age
full$Age <- 2016 - full$YOB

#throw away people from the future / dead people
full <- subset(full, Age < 105)
full <- subset(full, Age > 5)

#throw away YOB
full$YOB <- NULL

#we need to split our data back to training/test
full_train <- subset(full, training == TRUE)
Party <- full_train$Party 
#full_train$Party <- NULL
full_train$Party <- Party
full_train$training <- NULL

full_test <- subset(full, training == FALSE)
full_test$Party <- NULL
full_test$training <- NULL

split <- sample.split(full_train$Party, SplitRatio = 0.6)

train <- subset(full_train, split == TRUE)
test <- subset(full_train, split == FALSE)

train$Party <- as.numeric(train$Party)
train$Party <- train$Party - 1
train$sum <- NULL

####for messing around
justD <- subset(full_train, Party == "Democrat")
justR <- subset(full_train, Party == "Republican")

justD$sum <- NULL
justR$sum <- NULL

D <- cor(justD[,6:107])
R <- cor(justR[,6:107])
#justD[,6] first row of questions




#### now we can start predicting things
fit <- train(Party ~ Age + Gender + Income + HouseholdStatus + EducationLevel + Q122120 + Q121699 + Q121700 + Q120379 + Q120472 + Q120194 + Q119851 + Q118232 + Q116881 + Q116953 + Q115611 + Q115899 + Q115390 + Q113181 + Q110740 + Q109244 + Q108855 + Q108754 +
                 Q108342 + Q106997 + Q106389 + Q101163 + Q99480 + Q98059 + Q98197, data = train,
             method = "rf",
             controls = trainControl(method="cv", 3))

test_fit <- predict(fit, newdata = test, OOB=TRUE)
table(test$Party, test_fit)

fit2 <- cforest(Party ~ sum, data = train, controls=cforest_unbiased(ntree=250, mtry=3))

test_fit2 <- predict(fit2, test, OOB=TRUE)
table(test$Party, test_fit2)

fit3 <- glm(Party ~ sum, data = train, family = binomial)
test_fit3 <- predict(fit3, newdata = test, type = "response")
table(test$Party, test_fit3)
Party ~ Age + Gender + Income + HouseholdStatus + EducationLevel + Q122120 + Q121699 + Q121700 + Q120379 + Q120472 + Q120194 + Q119851 + Q118232 + Q116881 + Q116953 + Q115611 + Q115899 + Q115390 + Q113181 + Q110740 + Q109244 + Q108855 + Q108754 +
    Q108342 + Q106997 + Q106389 + Q101163 + Q99480 + Q98059 + Q98197