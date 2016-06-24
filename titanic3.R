library(mice)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(ROCR)
library(ggplot2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("train.csv")
test <- read.csv("test.csv")
#pre-processing and imputation
train$Name <- as.character(train$Name)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)

test$Name <- as.character(real_test$Name)
test$Pclass <- as.factor(real_test$Pclass)
test$Survived <- as.factor("0")
levels(test$Survived) <- levels(train$Survived)

#we just guess blindly that women will survive and men won't
#we additionally assume that women in 3rd class died
test$Survived <- ifelse(test$Sex == 'female' & test$Pclass != 3,1,0)
final_submission <- test[, colnames(test) %in% c("PassengerId", "Survived")]
write.csv(final_submission, "final_titanic.csv", row.names = FALSE)

aggregate(Survived ~ Sex + Pclass, data = train, FUN = sum)


#we have two missing values for embarked. 72% of passengers embarked from "S" so we will just use that
train[62, "Embarked"] <- 'S'
train[830, "Embarked"] <- 'S'

#lets get the titles from person's names
train$split_names <- strsplit(train$Name, " ")

test$split_names <- strsplit(real_test$Name, " ")


get_title <- function(n){
    m <- unlist(n)
    if ("Mr." %in% m) return ("Mr.")
    if ("Mrs." %in% m) return ("Mrs.")
    if ("Miss." %in% m) return ("Miss.")
    if ("Ms." %in% m) return ("Ms.")
    if ("Master." %in% m) return ("Master.")
    if ("Rev." %in% m) return ("Rev.")
    if ("Dr." %in% m) return ("Dr.")
    if ("Col." %in% m) return ("Military.")
    if ("Major." %in% m) return ("Military.")
    if ("Mlle." %in% m) return ("Miss.")
    if ("Mme." %in% m) return ("Mrs.")
    if ("Capt." %in% m) return ("Military.")
   # if ("Sir." %in% m) return ("Noble")
   # if ("Lady." %in% m) return ("Noble")
    if ("Don." %in% m) return ("Mr.")
    if ("Dona." %in% m) return ("Mrs.")
   # if ("Countess." %in% m) return ("Noble")
    #if ("Jonkheer." %in% m) return ("Noble")
    return ("Mr.")
}

train.titles <- sapply(train$split_names, get_title)
train$title <- train.titles
train$title <- as.factor(train$title)
#notitle <- subset(train, title == "no title")

test.titles <- sapply(test$split_names, get_title)
levels(test$title) <- levels(train$title)
test$title <- as.factor(test.titles)


test$title <- as.factor(test$title)

train$split_names <- NULL
test$split_names <- NULL

#we have 177 missing age values and want to fill them in with something plausible
#we accomplish this by predicting the age based on other factors

age_model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title, method = "anova", data = train)
ntrain$Age <- predict(age_model, newdata = ntrain)

missing_age <- subset(train, is.na(Age) == TRUE)
imputed_age <- predict(age_model, newdata = missing_age)
missing_age$Age <- abs(imputed_age)
train <- subset(train, is.na(Age) == FALSE)
train <- rbind(train, missing_age)

missing_test_age <- subset(test, is.na(Age) == TRUE)
imputed_test_age <- predict(age_model, newdata = missing_test_age)
missing_test_age$Age <- abs(imputed_test_age)
test <- subset(test, is.na(Age) == FALSE)
test <- rbind(test, missing_test_age)

survive_rf <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title + Age, method = "class", na.action = na.roughfix, data = train)
test_pred <- predict(survive_rf, newdata = test, type = "class")

test$Survived <- test_pred
test[122, "Survived"] <- 0
final_submission <- test[, colnames(test) %in% c("PassengerId", "Survived")]
write.csv(final_submission, "final_titanic.csv", row.names = FALSE)

#######################
# interactive tree???
########################

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


#################################
## conditional inference trees ##
#################################
library(party)
fit <- cforest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title, data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

test_fit <- predict(fit, test, OOB=TRUE, type = "response")
