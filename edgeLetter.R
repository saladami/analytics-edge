library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)


set.seed(2000)
letters <- read.csv("letters_ABPR.csv")

letters$isB <- as.factor(letters$letter == "B")

split <- sample.split(letters$letter, 0.5)
test <- subset(letters, split == FALSE)
train <- subset(letters, split == TRUE)

table(test$isB)

#CART model for predicting whether a letter is B

CARTb <- rpart(isB ~ . -letter, data = train, method="class")

predB <- predict(CARTb, newdata = test, type = "class")

table(test$isB, predB)

BForest = randomForest(isB ~ . -letter, data = train)

predBForest <- predict(BForest, newdata = test, type = "class")

#using all 4 letters
CARTletter <- rpart(letter ~ . -isB, data = train, method="class")
pCARTletter <- predict(CARTletter, newdata = test, type = "class")
table(test$letter, pCARTletter)

set.seed(1000)
#random forest using all 4 letters
letterForest = randomForest(letter ~ . -isB, data = train)
pLetterForest <- predict(letterForest, newdata = test, type = "class")
table(test$letter, pLetterForest)