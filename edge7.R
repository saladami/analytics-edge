library(ggplot2)
library(maps)
library(ggmap)

WHO <- read.csv("WHO.csv")

scatterplot <- ggplot(data = WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point(color = "blue", size = 3, shape = 15)
scatterplot

ggplot(data = WHO, aes(x = log(FertilityRate), y = Under15, color = Region)) + geom_point() + 
    scale_color_brewer(palette="Dark2") #+ stat_smooth(method = "lm", level = 0.99)

model <- lm(Under15 ~ log(FertilityRate), data = WHO)


###crime trends
mvt <- read.csv("mvt.csv", stringsAsFactors = FALSE)
mvt$Date <- strptime(mvt$Date, format("%m/%d/%y %H:%M"))
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hours <- mvt$Date$hour

WeekdayCounts <- as.data.frame(table(mvt$Weekday))
ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group = 1), alpha = 0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

WeekdayCounts$Var1 <- factor(WeekdayCounts$Var1, ordered = TRUE, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


DayHourCounts <- as.data.frame(table(mvt$Weekday, mvt$Hours))
DayHourCounts$Var1 <- factor(DayHourCounts$Var1, ordered = TRUE, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

DayHourCounts$Hour <- as.numeric(as.character(DayHourCounts$Var2))

ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group = Var1, color = Var1), size = 2)

ggplot(DayHourCounts, aes(x = Var1, y = Hour)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name = "Total MV Thefts", low = "white", high = "red") + 
        theme(axis.title.y = element_blank())

chicago <- get_map(location = "chicago", zoom = 11)
ggmap(chicago)

ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

#camden <- get_map(location = "camden, NJ", zoom = 10)
#ggmap(camden)

LatLonCounts <- as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))
LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size = Freq)) + scale_color_gradient(low = "yellow", high = "red")

ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill = "red")

LatLonCounts2 <- subset(LatLonCounts, Freq > 0)

### murders

murders <- read.csv("murders.csv")

states_map <- map_data("state")

ggplot(states_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

murders$region <- tolower(murders$State)

murderMap <- merge(states_map, murders, by = "region")
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")


ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

murderMap$MurderRate <- murderMap$Murders / murderMap$Population * 100000

##homework: election forecasting
statesMap <- map_data("state")
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling <- read.csv("PollingImputed.csv")

Train <- subset(polling, Year %in% c(2004, 2008))
Test <- subset(polling, Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

####facebook network data####
library(igraph)
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")

g <- graph.data.frame(edges, FALSE, users)
plot(g, vertex.size = 5, vertex.label = NA)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

#apple tweets
library(tm)
library(wordcloud)
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
corpus = Corpus(VectorSource(tweets$Tweet))

corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))

wc <- wordcloud(colnames(allTweets), colSums(allTweets), scale = c(3,0.25))

library(RColorBrewer)
display.brewer.all()
