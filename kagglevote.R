library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)

#load the data
full_train <- read.csv("./kaggle/train2016.csv")
full_test <- read.csv("./kaggle/test2016.csv")

######preprocessing data######
####income buckets are out of order####
incomelevels <- levels(full_train$Income)
incomelevels <- c(incomelevels[7],incomelevels[3],incomelevels[4],incomelevels[5],incomelevels[2],incomelevels[6])
full_train$Income <- factor(full_train$Income, ordered = TRUE, levels = incomelevels)
full_test$Income <- factor(full_test$Income, ordered = TRUE, levels = incomelevels)

######impute missing values for YOB
missing_YOB <- subset(full_train, is.na(YOB) == TRUE)
full_train <- subset(full_train, is.na(YOB) == FALSE)

missing_test_YOB <- subset(full_test, is.na(YOB) == TRUE)
full_test <- subset(full_test, is.na(YOB) == FALSE)

YOB_tree <- rpart(YOB ~ EducationLevel + Gender + Income + HouseholdStatus, data = full_train, method = "anova")
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

Income_tree <- rpart(Income ~ EducationLevel + Gender + HouseholdStatus + YOB, data = full_train, method = "class")
guess_income <- predict(Income_tree, newdata = missing_Income, type = "class")
guess_test_income <- predict(Income_tree, newdata = missing_test_income, type = "class")

missing_Income$Income <- guess_income
full_train <- rbind(full_train, missing_Income)

missing_test_income$Income <- guess_test_income
full_test <- rbind(full_test, missing_test_income)

#Add Age variable
full_train$Age <- 2016 - full_train$YOB
full_train <- subset(full_train, Age < 105)
full_train <- subset(full_train, Age > 5)

full_test$Age <- 2016 - full_test$YOB
full_train$YOB <- NULL
full_test$YOB <- NULL

#try to convert all the factors to numbers (one hot encoding)
#full_train[,7]
#full_train[,107]



#full_test[,6]
#full_test[,106]
for (i in seq(6:106)){
    
    
}


#plot(full_train$Age)
split <- sample.split(full_train$Party, SplitRatio = 0.7)

train <- subset(full_train, split == TRUE)
test <- subset(full_train, split == FALSE)


#####OLD BORING RANDOM FOREST####
#party_rf <- randomForest(Party ~.-USER_ID, data = full_train, na.action=na.roughfix, nodesize = 50)
#party_pred <- predict(party_rf, newdata = test, method = "class")
#table(test$Party, party_pred)
#final_test_pred <- predict(party_rf, newdata = full_test, method = "class")
####END OLD BORING RANDOM FOREST####

library(party)
library(caret)
#fit <- cforest(Party ~.-USER_ID, data = train, controls=cforest_unbiased(ntree=1000, mtry=3))
fit <- train(Party ~.-USER_ID, data = train,
             method = "rf",
             controls = trainControl(method="cv", 5))

test_fit <- predict(fit, test, OOB=TRUE)
table(test$Party, test_fit)


#hold on just a second
full_test$Predictions <- test_fit
submit <- full_test[,c("USER_ID", "Predictions")]

write.csv(submit, "submit.csv", row.names = FALSE)



###clustering

ftrain <- full_train
ftrain$Party <- NULL

#train_m <- model.matrix( ~. - 1, data=ftrain )
#preproc <- preProcess(train_m)
#train.matrix <- predict(preproc, train_m)

distance <- dist(ftrain)
trainclusters <- hclust(distance, method = "ward.D")
plot(trainclusters)
#divide into 3 clusters

pcluster <- cutree(trainclusters, 2)
cl1 <- subset(full_train, pcluster == 1)
cl2 <- subset(full_train, pcluster == 2)

#####
#table(train$Party == "Republican", train$Q99982)
justR <- subset(full_train, Party == "Republican")
justD <- subset(full_train, Party == "Democrat")
single <- subset(full_train, HouseholdStatus == "Single (no kids)")

Rquest <- data.frame()
Dquest <- data.frame()

for (i in seq(1:108)) {
    foo <- t(as.matrix(table(justR[,i])))
    Rquest <- rbind(Rquest, c(foo[1], foo[2], foo[3]))
}

for (i in seq(1:108)) {
    foo <- t(as.matrix(table(justD[,i])))
    Dquest <- rbind(Dquest, c(foo[1], foo[2], foo[3]))
}

Dquest <- Dquest[7:107,]
Rquest <- Rquest[7:107,]

Rquest$X1L <- Rquest$X1L / 2611
Rquest$X1L.1 <- Rquest$X1L.1 / 2611
Rquest$X1L.2 <- Rquest$X1L.2 / 2611

Dquest$X1L <- Dquest$X1L / 2945
Dquest$X1L.1 <- Dquest$X1L.1 / 2945
Dquest$X1L.2 <- Dquest$X1L.2 / 2945




compare <- abs(Rquest - Dquest)
possiblecor <- c(0)
for (i in seq(1:108)){
    if (!i %in% fc & !i %in% gc){possiblecor <- c(possiblecor, i)}
    
}
