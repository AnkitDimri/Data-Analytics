library(ggplot2) 
library(readr)
library(tm)
library(wordcloud)
library(plyr)
library(lubridate)
library(syuzhet)


#Import the twitter data set
tweetsdata=read.csv('demonetization-tweets.csv',stringsAsFactors = FALSE)

summary(tweetsdata)

tweetsdata$created_date=as.Date(tweetsdata$created,format='%Y-%m-%d %H:%M:%S')
tweetsdata$hour = format(as.POSIXct(tweetsdata$created,format="%Y-%m-%d %H:%M:%S"),"%H")
tweetsdata$isRetweetNum=ifelse(tweetsdata$isRetweet==FALSE,0,1)
tweetsdata$retweetedNum=ifelse(tweetsdata$retweeted==FALSE,0,1)
tweetsdata$tweet=c(1)

HourFrame=as.data.frame(table(tweetsdata$hour))
colnames(HourFrame)=c("Hour","TweetCount")
HourFrame$Hour=as.numeric(HourFrame$Hour)
y=ddply(tweetsdata, .(tweetsdata$hour), numcolwise(sum))
HourFrame$retweetedNum=y$isRetweetNum
ggplot(HourFrame,aes(x=Hour))+geom_line(aes(y = TweetCount, colour = "TotalTweets")) + 
  geom_line(aes(y = retweetedNum, colour = "Retweets"))

devices=tweetsdata$statusSource
devices <- gsub("","", devices)
devices <- strsplit(devices, ">")
devices <- sapply(devices,function(x) ifelse(length(x) > 1, x[2], x[1]))
devices <- strsplit(devices, "<")
devices <- sapply(devices,function(x) ifelse(length(x) > 1, x[1], x[2]))

devices_source=as.data.frame(table(devices))
colnames(devices_source)=c("Device","TweetCount")
devices_source=devices_source[devices_source$TweetCount>50,]
devices_source=devices_source[order(-devices_source$TweetCount),]

ggplot(devices_source,aes(x=reorder(Device, -TweetCount),y=TweetCount,fill=TweetCount))+geom_bar(stat='identity') +coord_flip()

y=ddply(tweetsdata, .(screenName), numcolwise(sum))
popularUsers=y[,c("screenName","retweetCount","tweet")]
popularUsers=popularUsers[order(-popularUsers$retweetCount),]
popularUsers=head(popularUsers,n=10)
popularUsers

#Most Replies
Replies=tweetsdata[is.na(tweetsdata$replyToSN)==FALSE,]
y=ddply(Replies, .(replyToSN), numcolwise(sum))
Replies=y[,c("replyToSN","tweet")]
Replies=Replies[order(-Replies$tweet),]
Replies=head(Replies,n=20)
colnames(Replies)=c("User","RepliesReceived")
Replies

some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweetsdata$text)
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
some_txt<-gsub("@\\w+","",some_txt)
some_txt<-gsub("[[:punct:]]"," ",some_txt)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)


tweetSentiment <- get_nrc_sentiment(some_txt)

barplot(
  sort(colSums(prop.table(tweetSentiment[, 1:8]))), 
  #  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Tweets text", xlab="Percentage"
)

corpus = tm::Corpus(tm::VectorSource(some_txt)) 

# Cleaning up 
# Handling UTF-8 encoding problem from the dataset 
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, to='UTF-8', sub='byte'))  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words 
corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces

tdm <- tm::DocumentTermMatrix(corpus.cleaned) 
tdm.tfidf <- tm::weightTfIdf(tdm)

tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 

#dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

dist.matrix = read.csv("distancematrix.csv")
x <- dist.matrix[1:(length(dist.matrix)-1)]
clustering.kmeans <- kmeans(tfidf.matrix,2,nstart=100)
points <- cmdscale(x, k = 2) 
