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
#Survived <- as.logical(Survived)
train[,2] <- NULL
train$Survived <- Survived
full <- rbind(train, test)
#we will drop the survival column from the training set in order to combine it with the test set

full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)
full$Pclass <- factor(full$Pclass, ordered = TRUE)

full$bestguess <- 0
full[full$Sex == 'female',]$bestguess <- 1
full[full$Sex == 'female' & full$Pclass == '3',]$bestguess <- sample(c(0,1), 216, replace = TRUE)

str(full[full$Survived == full$bestguess & full$from.training.set == TRUE,])
str(full[full$from.training.set == TRUE,])

justmen <- subset(full, Sex == 'male' & from.training.set == TRUE)

justwomen <- subset(full, Sex == 'female' & from.training.set == TRUE & Pclass == '3')

justtrain <- subset(full, from.training.set == TRUE)

menfit <- rpart(Survived ~ Pclass + SibSp + Parch + Fare + Embarked, data = justtrain, method = "class", control = rpart.control(minsplit = 0, minbucket = 0, maxdepth = 5))

fmen <- subset(full, Sex == 'male')

justtest <- subset(full, from.training.set == FALSE)
guessmen <- predict(menfit, newdata = justtest, type = "class")



### Well, we had some ####


smen <- subset(justmen, Survived == TRUE)

menfit <- cforest(Survived ~ Pclass + SibSp + Parch + Fare + Embarked, data = justmen, controls=cforest_unbiased(ntree=10000, mtry=5))

testmen <- subset(full, Sex == 'male' & from.training.set == FALSE)

predict_men <- predict(menfit, newdata = testmen, type = "response")
testmen$Survived <- predict_men

fem <- full[full$Sex == 'female' & full$from.training.set == FALSE,]
fem$Survived <- fem$bestguess
full[full$Sex == 'female' & full$from.training.set == FALSE,] <- fem

test <- subset(full, from.training.set == FALSE)
final_submission <- test[, colnames(test) %in% c("PassengerId", "Survived")]

write.csv(final_submission, "final_titanic.csv", row.names = FALSE)


full$Sex <- ifelse(full$Sex == 'male', 1 , 0)

full[full$Embarked == "",]$Embarked <- 'S'
full[is.na(full$Fare),]$Fare <- 15.75
emb <- unique(full$Embarked)

remap_embarked <- function(n) {
    if (n == 'C') return (0)
    if (n == 'Q') return (-1)
    if (n == 'S') return (1)
}
    

for (i in 1:1309) {
    if (is.numeric(full[i,11])) break
    full[i,11] <- remap_embarked(full[i,11])
}

full$Embarked <- as.numeric(full$Embarked)

#### normalize data and impute age values
missing_age <- subset(full, is.na(Age) == TRUE)
full <- subset(full, is.na(Age) == FALSE)


f <- as.formula("Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked")
guess_age <- cforest(f, data = full, controls=cforest_unbiased(ntree=2500, mtry=5))
predicted_age <- predict(guess_age, missing_age, OOB = TRUE)







#full$Pclass <- as.factor(full$Pclass)

missing_age <- subset(full, is.na(Age) == TRUE)
full <- subset(full, is.na(Age) == FALSE)


#age_fit <- lm(Age ~ Pclass + Sex + SibSp + Parch + Embarked, data = full)
#guess_missing_age <- predict(age_fit, newdata = missing_age)

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

