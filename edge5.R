library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
###################
## wiki vandalism #
###################
#Homework begins here

wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)

corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, stemDocument)

dtmAdded <- DocumentTermMatrix(corpusAdded)
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))

#repeat for removed
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)

dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))

colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))

#combine the two
wikiWords <- cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal <- wiki$Vandal
set.seed(123)
split <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
train <- subset(wikiWords, split == TRUE)
test <- subset(wikiWords, split == FALSE)

#build a CART model
vandalCART <- rpart(Vandal ~ ., data = train, method = "class")
prp(vandalCART)

pCARTvandal <- predict(vandalCART, newdata = test, type = "class")
table(test$Vandal, pCARTvandal)

#time to try something else... web address added?
wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE),1,0)

train2 <- subset(wikiWords2, split == TRUE)
test2 <- subset(wikiWords2, split == FALSE)

vandalCART2 <- rpart(Vandal ~ ., data = train2, method = "class")
prp(vandalCART2)

pCARTvandal2 <- predict(vandalCART2, newdata = test2, type = "class")
table(test2$Vandal, pCARTvandal2)

#try # of words
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

train3 <- subset(wikiWords2, split == TRUE)
test3 <- subset(wikiWords2, split == FALSE)

vandalCART3 <- rpart(Vandal ~ ., data = train3, method = "class")
#prp(vandalCART3)

pCARTvandal3 <- predict(vandalCART3, newdata = test3, type = "class")
table(test3$Vandal, pCARTvandal3)

#wikiwords3 using metadata
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

train4 <- subset(wikiWords3, split == TRUE)
test4 <- subset(wikiWords3, split == FALSE)

vandalCART4 <- rpart(Vandal ~ ., data = train4, method = "class")
#prp(vandalCART4)

pCARTvandal4 <- predict(vandalCART4, newdata = test4, type = "class")
table(test4$Vandal, pCARTvandal4)

###################
## clinical trial #
###################

trials <- read.csv("clinical_trial.csv", stringsAsFactors = FALSE)

#make a corpus for preprocessing
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

#lowercase
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))

#remove punctuation
corpusTitle = tm_map(corpusTitle, content_transformer(removePunctuation))
corpusAbstract = tm_map(corpusAbstract, content_transformer(removePunctuation))

#remove stop words
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

#stem the document
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

#document term matrix
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

#keep only the top 5% most frequent terms
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

#get ready to combine this data
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm <- cbind(dtmTitle, dtmAbstract)

set.seed(144)

split <- sample.split(dtm$trial, 0.7)
train <- subset(dtm, split == TRUE)
test <- subset(dtm, split == FALSE)

trialCART <- rpart(trial ~ ., data = train, method = "class")

prp(trialCART)

ptrialCART <- predict(trialCART, newdata = train, type = "class")

pttrialCART <- predict(trialCART, newdata = test)
table(test$trial, pttrialCART[,2] > 0.5)

#ROC
pred <- prediction(pttrialCART[,2], test$trial)
perf = performance(pred, "tpr", "fpr")
trialAUC = as.numeric(performance(pred, "auc")@y.values)


#Homework Ends Here

tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)

tweets$Negative = as.factor(tweets$Avg <= -1)

corpus = Corpus(VectorSource(tweets$Tweet))

#just like tapply, applies tolower to the entire set
#pre-processing steps to remove punctuation, stop words, stemming, etc.
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)
#generate frequency matrix
frequencies = DocumentTermMatrix(corpus)

findFreqTerms(frequencies, lowfreq = 20)

sparse = removeSparseTerms(frequencies, 0.995)
tweetsSparse <- as.data.frame(as.matrix(sparse))
#fix the names
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

train = subset(tweetsSparse, split == TRUE)
test = subset(tweetsSparse, split == FALSE)

#build a classification tree
tweetCART <- rpart(Negative ~ ., data = train, method = "class")

prp(tweetCART)

pCARTtweet <- predict(tweetCART, newdata = test, type = "class")
table(test$Negative, pCARTtweet)

#build a random forest
tweetrf = randomForest(Negative ~ ., data = train)

pRF <- predict(tweetrf, newdata = test)
table(test$Negative, pRF)

#logistic regression
tweetl <- glm(Negative ~ ., data = train, family = binomial)
pltweet <- predict(tweetl, newdata = test, type = "response")
table(test$Negative, pltweet)
