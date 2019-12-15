## Create token for direct authentication to access Twitter data

token <- rtweet::create_token(
  app = "Jacob Watkins 1",
  consumer_key <- 
  consumer_secret <- 
  access_token <- 
  access_secret <- 

##packages
library(rtweet)
library(tidytext)
library(tidyverse)
library(lubridate)
library(textdata)

##Trump dataset download
rdt_eco <- search_fullarchive(q="economy from:realDonaldTrump OR jobs from:realDonaldTrump", n=1000, fromDate= 201701210000, toDate=201909220000, env_name="devenvironment",token=token)

##sentiment analysis of trump dataset over time 

bing <- get_sentiments("bing")
v <- c(positive="1", negative="-1")
bing$value <- as.character(v[bing$sentiment])
bing$value <- as.numeric(bing$value)
bing <- select(bing, word, value)

rdt_eco <- mutate(rdt_eco, date = as.Date(ymd_hms(rdt_eco$created_at)))

rdt_sentiment <- rdt_eco %>%
  mutate(tweet= c(513:1)) %>%
  unnest_tokens(word,text) %>%
  filter(!(word == "trump")) %>%
  inner_join(bing) %>%
  select(date, tweet, word, value) %>%
  group_by(tweet, date) %>%
  summarize(value=sum(value)) %>%
  group_by(date) %>%
  summarize(value=mean(value))


rdtplot_daily <- ggplot(rdt_sentiment, aes(date, value)) +
  geom_col(fill= "orange") +
  xlab("Date") +
  ylab("Sentiment Score") +
  theme(axis.text.x= element_text(angle = 90)) +
  ggtitle(" Daily Sentiment Scores of Donald Trump's Tweets About the Economy")


rdt_sentiment$date = format(as.Date(rdt_sentiment$date), "%Y-%m")

rdt_sentiment <- rdt_sentiment %>%
  group_by(date) %>%
  summarize(value = mean(value)) 

rdtplot_monthly <- ggplot(rdt_sentiment, aes(date, value)) +
  geom_col(fill= "blue") +
  xlab("Month") +
  ylab("Sentiment Score") +
  ylim(-1, 2.5) +
  theme(axis.text.x= element_text(angle = 90)) +
  ggtitle("Sentiment Scores of Donald Trump's Tweets About the Economy")
  
 summary(rdt_sentiment)

##most common positive and negative words

bing_words_counts <- rdt_eco %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

rdt_posneg <- bing_words_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word =reorder(word, n)) %>% 
  ggplot(aes(word,n, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Contribution to Sentiment", x="Word") +
  coord_flip()

##word cloud and comparison word cloud for trump data set

library(wordcloud)

rdt_eco_unnest <- rdt_eco %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  filter(!(word =="economy")) 

rdt_eco_unnest %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=100))

install.packages("reshape2")
library(reshape2)

trump_cloud = rdt_eco_unnest %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort=TRUE) %>%
  acast(word~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "blue"), max.words=100)


## preliminary data cleaning and merging of general_eco

eco1 <- read.csv("eco1.csv", header=TRUE)
eco2 <- read.csv("eco2.csv", header=TRUE)
eco3 <- read.csv("eco3.csv", header=TRUE)
eco4 <- read.csv("eco4.csv", header=TRUE)

general_eco <- rbind(eco1, eco2, eco3, eco4)

general_eco <- mutate(general_eco, date_notime = as.Date(mdy_hm(general_eco$date)))

general_eco$text <- as.character(general_eco$text)

##preliminary sentiment analysis of general_eco

general_sentiment <- general_eco %>%
  mutate(tweet= c(433757:1)) %>%
  unnest_tokens(word,text) %>%
  filter(!(word == "trump")) %>%
  inner_join(bing) %>%
  select(date_notime, tweet, word, value) %>%
  group_by(tweet, date_notime) %>%
  summarize(value=sum(value)) %>%
  group_by(date_notime) %>%
  summarize(value=sum(value))

generalplot_daily <- ggplot(general_sentiment, aes(date, value)) +
  geom_col(fill= "dark green") +
  xlab("Date") +
  ylab("Sentiment Score") +
  theme(axis.text.x= element_text(angle = 90)) +
  ggtitle("Daily Sentiment Scores of General Tweets About the Economy")


general_sentiment$date_notime = format(as.Date(general_sentiment$date_notime), "%Y-%m")

general_sentiment <- general_sentiment %>%
  group_by(date_notime) %>%
  summarize(value = mean(value)) 

generalplot_monthly <- ggplot(general_sentiment, aes(date_notime, value)) +
  geom_col(fill= "dark green") +
  xlab("Month") +
  ylab("Sentiment Score") +
  theme(axis.text.x= element_text(angle = 90)) +
  ggtitle("Sentiment Scores of Twitter About the Economy")

summary(general_sentiment)

##easy comparison between trump and general

library(gridExtra)  

grid.arrange(rdtplot_daily, generalplot_daily, rdtplot_monthly, generalplot_monthly, ncol=2, nrow=2)


## most common positive and negative words general

general_words_counts <- general_eco %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

general_posneg<- general_words_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word =reorder(word, n)) %>% 
  ggplot(aes(word,n, fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y="Contribution to Sentiment", x="Word") +
  coord_flip()

grid.arrange(rdt_posneg, general_posneg, ncol=2)

##general word clouds

general_eco_unnest <- general_eco %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  filter(!(word =="economy")) %>%
  filter(!(word =="U.S."))

general_eco_unnest %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=100))

general_cloud = general_eco_unnest %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment, sort=TRUE) %>%
  acast(word~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("red", "blue"), max.words=100)

##effect of donald trumps tweets on general sentiment
general_eco$date <- as.character(general_eco$date)
general_eco$date <- as.POSIXct(general_eco$date, format= "%m/%d/%y %H:%M")

rdt_eco_char <- mutate(rdt_eco, datetime = as.character(created_at))

##nested for loop

hours_to_second <- rep(0,23)
list_hours <-c(2:24)

for (j in 1:23){
  a <- list_hours[j]*3600
  hours_to_second[j] <- a
}

sentiment_change_hr <- rep(0, 23)

beforemean <- rep(0,513)
aftermean <- rep(0,513)
for (s in 1:23){
  for (i in 1:513){
    datei <- as.POSIXct(rdt_eco_char$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
    yesterday <- as.POSIXct(format(datei-hours_to_second[s], "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    one_day_before <- filter(general_eco, date > yesterday & date < datei)
    
    one_day_before<- one_day_before %>%
      mutate(tweet = c(n():1)) %>%
      unnest_tokens(word, text) %>%
      filter(!(word =="trump")) %>%
      inner_join(bing)) %>%
      select(date, tweet, word, value) %>%
      group_by(tweet, date) %>%
      summarize(value=sum(value))
    
    beforemean[i] <- mean(one_day_before$value)
    
    dateii <- as.POSIXct(rdt_eco_char$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
    tomorrow <- as.POSIXct(format(dateii+hours_to_second[s], "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    one_day_after <- filter(general_eco, date < tomorrow & date > dateii)
    
    one_day_after<- one_day_after %>%
      mutate(tweet = c(n():1)) %>%
      unnest_tokens(word, text) %>%
      filter(!(word =="trump")) %>%
      inner_join(bing) %>%
      select(date, tweet, word, value) %>%
      group_by(tweet, date) %>%
      summarize(value=sum(value))
    
    aftermean[i] <- mean(one_day_after$value)
  }

  sentiment_change <- as.tibble(cbind(beforemean,aftermean))
  
  sentiment_change <- mutate(sentiment_change, change = aftermean-beforemean)
  
  sentiment_change_hr[s] <-mean(sentiment_change$change)
}

hour <- c(2:24)

sentiment_change_hr <- cbind(hour, sentiment_change_hr$sentiment_change_hr)
sentiment_change_hr <- as_tibble(sentiment_change_hr)                             
View(sentiment_change_hr)

plot(hour, sentiment_change_hr$V2)

##isolating positive and negative trump posts 

rdt_eco <- mutate(rdt_eco, date = as.character(rdt_eco$created_at))

rdt_sentiment <- rdt_eco %>%
  mutate(tweet= c(513:1)) %>%
  unnest_tokens(word,text) %>%
  filter(!(word == "trump")) %>%
  inner_join(bing)) %>%
  select(date, tweet, word, value) %>%
  group_by(tweet, date) %>%
  summarize(value=sum(value)) 

rdt_sentiment <- arrange(rdt_sentiment, desc(tweet))

rdt_eco_char <- mutate(rdt_eco_char, tweet = c(513:1))

rdt_sentiment <- mutate(rdt_sentiment, boolean = value > 0)

rdt_eco_pos <- filter(rdt_eco_char, rdt_sentiment$boolean)
rdt_eco_neg <- filter(rdt_eco_char, !(rdt_sentiment$boolean))

##nested for loop for positive and negative
hours_to_second <- rep(0,23)
list_hours <-c(2:24)

for (j in 1:23){
  a <- list_hours[j]*3600
  hours_to_second[j] <- a
}

sentiment_change_pos_hr <- rep(0, 23)

beforemean <- rep(0,311)
aftermean <-rep(0,311)

for (s in 1:23){
  for (i in 1:311){
    datei <- as.POSIXct(rdt_eco_pos$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
    yesterday <- as.POSIXct(format(datei-hours_to_second[s], "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    one_day_before <- filter(general_eco, date > yesterday & date < datei)
    
    one_day_before<- one_day_before %>%
      mutate(tweet = c(n():1)) %>%
      unnest_tokens(word, text) %>%
      filter(!(word =="trump")) %>%
      inner_join(bing)) %>%
      select(date, tweet, word, value) %>%
      group_by(tweet, date) %>%
      summarize(value=sum(value))
    
    beforemean[i] <- mean(one_day_before$value)
    
    dateii <- as.POSIXct(rdt_eco_pos$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
    tomorrow <- as.POSIXct(format(dateii+hours_to_second[s], "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    one_day_after <- filter(general_eco, date < tomorrow & date > dateii)
    
    one_day_after<- one_day_after %>%
      mutate(tweet = c(n():1)) %>%
      unnest_tokens(word, text) %>%
      filter(!(word =="trump")) %>%
      inner_join(bing)) %>%
      select(date, tweet, word, value) %>%
      group_by(tweet, date) %>%
      summarize(value=sum(value))
    
    aftermean[i] <- mean(one_day_after$value)
  }
  
  sentiment_change <- as.tibble(cbind(beforemean,aftermean))
  
  sentiment_change <- mutate(sentiment_change, change = aftermean-beforemean)
  
  sentiment_change_pos_hr[s] <-mean(sentiment_change$change)
}

plot(c(2:24), sentiment_change_pos_hr)


hours_to_second <- rep(0,23)
list_hours <-c(2:24)

for (j in 1:23){
  a <- list_hours[j]*3600
  hours_to_second[j] <- a
}

sentiment_change_neg_hr <- rep(0, 23)

beforemean <- rep(0,148)
aftermean <-rep(0,148)
for (s in 1:23){
  for (i in 1:148){
    datei <- as.POSIXct(rdt_eco_neg$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
    yesterday <- as.POSIXct(format(datei-hours_to_second[s], "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    one_day_before <- filter(general_eco, date > yesterday & date < datei)
    
    one_day_before<- one_day_before %>%
      mutate(tweet = c(n():1)) %>%
      unnest_tokens(word, text) %>%
      filter(!(word =="trump")) %>%
      inner_join(bing) %>%
      select(date, tweet, word, value) %>%
      group_by(tweet, date) %>%
      summarize(value=sum(value))
    
    beforemean[i] <- mean(one_day_before$value)
    
    dateii <- as.POSIXct(rdt_eco_neg$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
    tomorrow <- as.POSIXct(format(dateii+hours_to_second[s], "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    one_day_after <- filter(general_eco, date < tomorrow & date > dateii)
    
    one_day_after<- one_day_after %>%
      mutate(tweet = c(n():1)) %>%
      unnest_tokens(word, text) %>%
      filter(!(word =="trump")) %>%
      inner_join(bing)) %>%
      select(date, tweet, word, value) %>%
      group_by(tweet, date) %>%
      summarize(value=sum(value))
    
    aftermean[i] <- mean(one_day_after$value)
  }
  
  sentiment_change <- as.tibble(cbind(beforemean,aftermean))
  
  sentiment_change <- mutate(sentiment_change, change = aftermean-beforemean)
  
  sentiment_change_neg_hr[s] <-mean(sentiment_change$change)
}


plot(c(2:24), sentiment_change_neg_hr)

## average sentiment mean of all trump tweets versus economy tweets

rdt_mean_eco <- rdt_eco %>%
  mutate(tweet = c(513:1)) %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  inner_join(bing) %>%
  select(date, tweet, word, value) %>%
  group_by(tweet, date) %>%
  summarize(value=mean(value))

rdt_overall <- read.csv("trump.csv", header=TRUE) 
rdt_overall <- as_tibble(rdt_overall)
rdt_overall$text <- as.character(rdt_overall$text)

rdt_mean_overall <- rdt_overall%>%
  mutate(tweet = c(8050:1)) %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  inner_join(bing) %>%
  select(date, tweet, word, value) %>%
  group_by(tweet, date) %>%
  summarize(value=mean(value))

mean(rdt_mean_eco$value)
mean(rdt_mean_overall$value)




t.test(rdt_mean_eco_sum$value, rdt_mean_overall_sum$value, alternative = "greater")

##rdt vs general mean comparison

rdt_mean_eco_sum <- rdt_eco %>%
  mutate(tweet = c(513:1)) %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  inner_join(bing) %>%
  select(date, tweet, word, value) %>%
  group_by(tweet, date) %>%
  summarize(value=sum(value))

general_mean_eco_sum <- general_eco %>%
  mutate(tweet = c(433757:1)) %>%
  unnest_tokens(word, text) %>%
  filter(!(word =="trump")) %>%
  inner_join(get_sentiments("afinn")) %>%
  select(date, tweet, word, value) %>%
  group_by(tweet, date) %>%
  summarize(value=mean(value))

summary(rdt_mean_eco_sum$value)
summary(general_mean_eco_sum$value)

t.test(rdt_mean_eco_sum$value, general_mean_eco_sum$value, alternative = "greater")


##8 hours after, this code was changed to 2-24 hours to get individual datasets for pvalue tests
beforemean <- rep(0,513)
aftermean <- rep(0,513)

for (i in 1:513){
  datei <- as.POSIXct(rdt_eco_char$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
  yesterday <- as.POSIXct(format(datei-28800, "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
  one_day_before <- filter(general_eco, date > yesterday & date < datei)
  
  one_day_before<- one_day_before %>%
    mutate(tweet = c(n():1)) %>%
    unnest_tokens(word, text) %>%
    filter(!(word =="trump")) %>%
    inner_join(bing) %>%
    select(date, tweet, word, value) %>%
    group_by(tweet, date) %>%
    summarize(value=sum(value))
  
  beforemean[i] <- mean(one_day_before$value)
  
  dateii <- as.POSIXct(rdt_eco_char$datetime[i], format= "%Y-%m-%d %H:%M:%S" )
  tomorrow <- as.POSIXct(format(dateii+28800, "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
  one_day_after <- filter(general_eco, date < tomorrow & date > dateii)
  
  one_day_after<- one_day_after %>%
    mutate(tweet = c(n():1)) %>%
    unnest_tokens(word, text) %>%
    filter(!(word =="trump")) %>%
    inner_join(bing) %>%
    select(date, tweet, word, value) %>%
    group_by(tweet, date) %>%
    summarize(value=sum(value))
  
  aftermean[i] <- mean(one_day_after$value)
}

sentiment_change_8hr <- as_tibble(cbind(beforemean, aftermean))
sentiment_change_8hr <- mutate(sentiment_change_8hr, change= aftermean-beforemean)
mean(sentiment_change_8hr$change)

t.test(sentiment_change_8hr$beforemean, sentiment_change_8hr$aftermean, alternative = "less")










