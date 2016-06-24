library(ggplot2)
library(caret)
library(caTools)
library(party)
library(randomForest)
library(rpart)
library(rpart.plot)
library(neuralnet)

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

train$from.training.set <- TRUE
test$from.training.set <- FALSE
test$Survived <- FALSE

Survived <- train$Survived
Survived <- as.logical(Survived)
train[,2] <- NULL
train$Survived <- Survived
full <- rbind(train, test)
#we will drop the survival column from the training set in order to combine it with the test set

full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)
full[full$Embarked == "",]$Embarked <- 'S'
full[is.na(full$Fare),]$Fare <- 15.75
full$Pclass <- as.factor(full$Pclass)

missing_age <- subset(full, is.na(Age) == TRUE)
full <- subset(full, is.na(Age) == FALSE)


age_fit <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked, data = full)
guess_missing_age <- predict(age_fit, newdata = missing_age)

missing_age$Age <- guess_missing_age
full <- rbind(full, missing_age)

#ggplot(full, aes(x = Age, col = Pclass)) + geom_bar()







## actual prediction time ##
full_train <- subset(full, from.training.set == TRUE)
full_test <- subset(full, from.training.set == FALSE)


split <- sample.split(full_train$Survived, SplitRatio = 0.6)
train <- subset(full_train, split == TRUE)
test <- subset(full_train, split == FALSE)

fit <- cforest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = full_train, controls=cforest_unbiased(ntree=10000, mtry=5))

test_pred <- predict(fit, full_test, OOB=TRUE)
#table(test$Survived, test_pred > 0.5)
full_test$Survived <- as.numeric(test_pred > 0.5)
final_submission <- full_test[, colnames(full_test) %in% c("PassengerId", "Survived")]

write.csv(final_submission, "final_titanic.csv", row.names = FALSE)


#####neural net####
#f <- as.formula(paste("Survived ~", paste(n[!n %in% "Survived"], collapse = " + ")))

