library(mice)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(ROCR)
set.seed(100)
train <- read.csv("train.csv")

test <- read.csv("test.csv")

train$Name <- as.character(train$Name)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)

test$Survived <- as.factor(rep(0,nrow(test)))
test$Pclass <- as.factor(test$Pclass)

#we have two missing values for embarked. 72% of passengers embarked from "S" so we will just use that
train[62, "Embarked"] <- 'S'
train[830, "Embarked"] <- 'S'

#we have 177 missing age values and want to fill them in with something plausible
#we accomplish this by predicting the age based on other factors

age_tree <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare, data = train, method = "anova")

missing_age <- subset(train, is.na(Age) == TRUE)
imputed_age <- predict(age_tree, newdata = missing_age)
missing_age <- imputed_age

missing_test_age <- subset(test, is.na(Age) == TRUE)
imputed_test_age <- predict(age_tree, newdata = missing_test_age)
missing_test_age$Age <- imputed_test_age

test <- subset(test, is.na(Age) == FALSE)
test <- rbind(test, missing_test_age)

train <- subset(train, is.na(Age) == FALSE)
train <- rbind(train, missing_age)

#####################################
# parse titles from passenger names #
#####################################


fem_titles <- c("Master", "Miss.", "Mrs.")

train$title <- ifelse(fem_titles[1] %in% train$Name | fem_titles[2] %in% train$Name | fem_titles[3] %in% train$Name, 0, 1) 

split = sample.split(train$Survived, 0.6)

tr <- subset(train, split == TRUE)
te <- subset(train, split == FALSE)

survived_rf <- randomForest(Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare, data = train)

surv_pred <- predict(survived_rf, newdata = test, type = "class") #, method = "class"
test$Survived <- surv_pred

#one guy is still NA. But manual inspection reveals that he is 60 years old. Lets assume that he died.
test[122, "Survived"] <- 0


table(te$Survived, surv_pred)

pred = prediction(surv_pred[,2], te$Survived)
perf = performance(pred, "tpr", "fpr")
survAUC = as.numeric(performance(pred, "auc")@y.values)

test_surv_pred <- predict(survived_rf, newdata = test, method = "class") #, method = "class"
test$Survived <- test_surv_pred

final_predictions <- test[, colnames(test) %in% c("PassengerId", "Survived")]

write.csv(final_predictions, file = "final_titanic.csv", row.names = FALSE)

fixya <- read.csv("final_titanic.csv")
write.csv(fixya, file = "final_titanic.csv")

