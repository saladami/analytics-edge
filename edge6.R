#clustering
a <- c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
b <- c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)


movies <- read.table("u.item.txt", header = FALSE, sep = "|", quote ="\"")

colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

#this is ruined, see Unit6_Netflix.R for official code

kos <- read.csv("dailykos.csv")

distance <- dist(kos)

#full-on hierarchical clustering
koscl <- hclust(distance, method = "ward.D")
plot(koscl)

#divide into 7 clusters
kosGroups <- cutree(koscl, k = 7)

#should have done this in a loop
cluster3 <- subset(kos, kosGroups == 3)
cluster4 <- subset(kos, kosGroups == 4)
cluster5 <- subset(kos, kosGroups == 5)
cluster6 <- subset(kos, kosGroups == 6)
cluster7 <- subset(kos, kosGroups == 7)
str(cluster3)

#view # in each cluster
table(kosGroups)

cluster1 <- subset(kos, kosGroups == 1)
tail(sort(colMeans(cluster1)))

#k-means clustering
set.seed(1000)

koskcl <- kmeans(kos, 7)
table(koskcl$cluster)
for (i in (1:7)) {
    cl <- subset(kos, koskcl$cluster == i)
    print(tail(sort(colMeans(cl))))
    print(i)
}

#table(hierGroups, KMeansCluster$cluster)

####Airline stuff####
library(caret)
airlines <- read.csv("AirlinesCluster.csv")
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)

distance <- dist(airlinesNorm)
airclustHR <- hclust(distance, method = "ward.D")
plot(airclustHR)
#divide into 5 clusters

aircluster <- cutree(airclustHR, 5)
tapply(airlines$Balance, aircluster, mean)

#k-means
set.seed(88)
airkcluster <- kmeans(airlinesNorm, 5)

####Stock Chimps?

stocks <- read.csv("StocksCluster.csv")
mean(stocks$PositiveDec)
cor(stocks)
which.max(cor(stocks)) #doesn't work, returns correlation with itself
stockcorr <- cor(stocks)
diag(stockcorr) <- c(rep(0, 12)) #write 0 in the main diagonal

which.max(stockcorr) #rerun old command, 119?

sort(colMeans(stocks))

library(caTools)
set.seed(144)
split <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
train <- subset(stocks, split == TRUE)
test <- subset(stocks, split == FALSE)

StocksModel <- glm(PositiveDec ~ ., data = train, family = "binomial")
stocksp <- predict(StocksModel, newdata = train, type = "response")
table(train$PositiveDec, stocksp > 0.5)

stocks_testp <- predict(StocksModel, newdata = test, type = "response")
table(test$PositiveDec, stocks_testp > 0.5)

#clustering bs
limtrain <- train
limtest <- test

limtrain$PositiveDec <- NULL
limtest$PositiveDec <- NULL

preproc <- preProcess(limtrain)
normtrain <- predict(preproc, limtrain)

normtest <- predict(preproc, limtest)

set.seed(144)

km <- kmeans(normtrain, 3)
library(flexclust)

km.kcca <- as.kcca(km, normtrain)

cltrain <- predict(km.kcca)
cltest <- predict(km.kcca, newdata = normtest)

st1 <- subset(train, km$cluster == 1)
st2 <- subset(train, km$cluster == 2)
st3 <- subset(train, km$cluster == 3)

test1 <- subset(test, km$cluster == 1)
test2 <- subset(test, km$cluster == 2)
test3 <- subset(test, km$cluster == 3)
StocksModel1 <- glm(PositiveDec ~ ., data = st1, family = "binomial")
StocksModel2 <- glm(PositiveDec ~ ., data = st2, family = "binomial")
StocksModel3 <- glm(PositiveDec ~ ., data = st3, family = "binomial")

pred1 <- predict(StocksModel1, newdata = test1, type = "response")
pred2 <- predict(StocksModel2, newdata = test2, type = "response")
pred3 <- predict(StocksModel3, newdata = test3, type = "response")


table(test1$PositiveDec, pred1 > 0.5)
table(test2$PositiveDec, pred2 > 0.5)
table(test3$PositiveDec, pred3 > 0.5)

for (i in (2:12)) {
    print(c(StocksModel1$coefficients[i], StocksModel2$coefficients[i], StocksModel3$coefficients[i]))
}


#stocksp1 <- predict(StocksModel, newdata = train, type = "response")
#table(train$PositiveDec, stocksp > 0.5)







