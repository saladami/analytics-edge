


##############
# predictions
##############

real_test$Survived <- as.factor("0")

controlRf <- trainControl(method="cv", 5)

survived_rf <- randomForest(Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked + title, data = train)
surv_pred <- predict(survived_rf, newdata = real_test, type = "class")

real_test$Survived <- surv_pred
real_test[153, "Survived"] <- "0" #screw this one guy in particular
real_test$split_names <- NULL




final_submission <- test[, colnames(test) %in% c("PassengerId", "Survived")]
write.csv(final_submission, "final_titanic.csv", row.names = FALSE)

###############################
#start over with common sense
###############################
table(train$Survived)
table(train$Survived, train$Sex)

justmen <- subset(train, Sex == 'male')
male.over30 <- subset(justmen, Age > 30)
justwomen <- subset(train, Sex == 'female')

men.survived <- subset(train, Sex == 'male' & Survived == 1)
women.died <- subset(train, Sex == 'female' & Survived == 0)

men.srv.model <- glm(Survived)

#tweetl <- glm(Negative ~ ., data = train, family = binomial)
#pltweet <- predict(tweetl, newdata = test, type = "response")
ggplot(data = justwomen, aes(x = PassengerId, y = Age, col = Survived)) + geom_jitter()
ggplot(data = train, aes(x = Age, y = Survived, col = Sex)) + geom_jitter()
foo <- c(0)
for (a in (15:55)) {
    m <- subset(justmen, Age > a)
    acc <- mean(m$Survived == 0)
    foo <- c(foo, acc)
}

ggplot(men.survived, aes(x=Age, col = Pclass))+geom_histogram()+facet_grid(~Survived)
m2535 <- subset(justmen, Age < 35 & Age > 25)

ggplot(female.third, aes(x = Parch + SibSp, y = Survived)) + geom_jitter()

female.third <- subset(justwomen, Pclass = 3)


fem <- subset(female.third, Survived == 1)


