library (stringr)
library (BSDA)

# Import dataset
df = read.csv ("demonetization-tweets.csv")

# Display Dataset
head (df)

# Size of the dataframe
summary (df)
nrow (df)
ncol (df)

# Displaying some of the tweets
head (df [,1])

# Clean the dataset
tweets = df [,1]
tweets = str_replace_all (tweets, "[^[:alnum:]]", " ")
head (tweets)

df [,1] = tolower (tweets)
head (df)
df$created_date=as.Date (df$created, format = '%Y-%m-%d %H:%M:%S')
df$hour = format (as.POSIXct (df$created, format = "%Y-%m-%d %H:%M:%S"), "%H")
df$isRetweetNum = ifelse (df$isRetweet == FALSE, 0, 1)
df$retweetedNum = ifelse (df$retweeted == FALSE, 0, 1)
df$tweet = c (1)

# Plotting the trends
options (repr.plot.width=6, repr.plot.height=4)
HourFrame = as.data.frame (table (df$hour))
colnames (HourFrame) = c ("Hour","TweetCount")
HourFrame$Hour = as.numeric (HourFrame$Hour)
y=ddply (df, .(df$hour), numcolwise(sum))
HourFrame$retweetedNum=y$isRetweetNum
ggplot (HourFrame, aes (x = Hour)) + geom_line (aes (y = TweetCount, colour = "TotalTweets")) + geom_line (aes (y = retweetedNum))


y = ddply (df, .(screenName), numcolwise (sum))
popularUsers = y [, c ("screenName", "retweetCount", "tweet")]
popularUsers = popularUsers [ order ( -popularUsers$retweetCount),]
popularUsers = head (popularUsers, n = 10)
popularUsers

                                      

#Most Replies
Replies = df [is.na (df$replyToSN) == FALSE,]
y = ddply (Replies, .(replyToSN), numcolwise(sum))
Replies = y [, c ("replyToSN", "tweet")]
Replies = Replies [order (-Replies$tweet),]
Replies = head (Replies, n = 20)
colnames (Replies) = c ("User", "RepliesReceived")
Replies


some_txt <- gsub ("(RT|via)((?:\\b\\w*@\\w+)+)", "", df$text)
some_txt <- gsub ("http[^[:blank:]]+", "", some_txt)
df [,1] = some_txt



mysentiment <- get_nrc_sentiment ((some_txt))
Sentimentscores <- data.frame (colSums (mysentiment [,]))
names (Sentimentscores) <- "Score"
SentimentScores <- cbind ("sentiment" = rownames (Sentimentscores), Sentimentscores)
rownames (SentimentScores) <- NULL
ggplot (data = SentimentScores, aes (x = sentiment, y = Score)) + geom_bar (aes (fill = sentiment), stat = "identity") + theme (legend.position = "none") + xlab("Sentiments") + ylab ("scores") + ggtitle ("Total sentiment based on scores")


t.test(mysentiment['positive'], mu = 0, alternative = "two.sided")
mysentiment
positive_mean = mean(mysentiment$positive)
sigmap = sd(mysentiment$positive)
negative_mean = mean(mysentiment$negative)
sigman = sd(mysentiment$negative)
countp = length(mysentiment$positive)
countn = length(mysentiment$negative)


z_calcs1 = zsum.test(mean.x = positive_mean,sigma.x = sigmap,n.x = countp,mean.y = negative_mean,sigma.y =sigman,n.y = countn )
print(z_calcs1)



