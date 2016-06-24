library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)
stevens <- read.csv("stevens.csv")
set.seed(88)

spl <- sample.split(stevens$Reverse, SplitRatio = 0.7)

train = subset(stevens, spl == TRUE)
test = subset(stevens, spl == FALSE)

stree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", minbucket = 100)
prp(stree)

predCART <- predict(stree, newdata = test, type = "class") 
p2 <- predict(stree, newdata = test)

pred = prediction(p2[,2], test$Reverse)
perf = performance(pred, "tpr", "fpr")
stevensAUC = as.numeric(performance(pred, "auc")@y.values)

sforest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree = 250)

predictForest = predict(sforest, newdata = test)
table(test$Reverse, predictForest)

numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "rpart", trControl=numFolds, tuneGrid = cpGrid)

stree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "class", cp = 0.18)

predCV <- predict(stree2, newdata = test, type = "class")
table(test$Reverse, predCV)

cd <- read.csv("ClaimsData.csv")

split = sample.split(cd, SplitRatio = 0.6)
train = subset(cd, split == TRUE)
test = subset(cd, split == FALSE)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow = TRUE, nrow = 5)
table(train$bucket2009, train$bucket2008)

gerber = read.csv("gerber.csv")

gm <- glm(voting ~ self + hawthorne + civicduty + neighbors, family = binomial, data = gerber)
votepredict <- predict(gm, type = "response")

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
#makes a tree that doesn't do anything (root node)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)

CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data = gerber, cp = 0.0)

gm2 <- glm(voting ~ sex + control, family = binomial, data = gerber)
LogModelSex = glm(voting ~ control + sex, data=gerber, family="binomial")
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
ppp <- predict(LogModel2, newdata=Possibilities, type="response")


