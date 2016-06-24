library(caTools)
library(ROCR)

data <- read.csv("quality.csv")

set.seed(88)

split = sample.split(data$PoorCare, SplitRatio = 0.75)

train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data = train, family = binomial)
summary(QualityLog)

PredictTrain <- predict(QualityLog, type ="response")

tapply(PredictTrain, train$PoorCare, mean)

#use t = 0.5 with our prediction model
table(train$PoorCare, PredictTrain > 0.9)

#ROC curve 
ROCRpred <- prediction(PredictTrain, train$PoorCare)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))

fram <- read.csv("framingham.csv")
str(fram)

split = sample.split(fram$TenYearCHD, SplitRatio = 0.65)
framTrain <- subset(fram, split == TRUE)
framTest <- subset(fram, split == FALSE)

framinghamLog <- glm(TenYearCHD ~ ., data=framTrain, family = binomial)

predictTest <- predict(framinghamLog, type = "response", newdata = framTest)
table(framTest$TenYearCHD, predictTest > 0.5)

#check ROC curve
ROCRpred <- prediction(predictTest, framTest$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

eightteams <- c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")
ws_predict <- glm(WorldSeries ~., family = binomial, data = bb)

set.seed(144)
spl = sample.split(loans$not.fully.paid, 0.7)
train = subset(loans, spl == TRUE)
test = subset(loans, spl == FALSE)
lmod <- glm(not.fully.paid ~., data = train, family = binomial)
predicted.risk <- predict(lmod, type = "response", newdata = test)

lmod2 <- glm(not.fully.paid ~ int.rate, data = train, family = binomial)
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]


