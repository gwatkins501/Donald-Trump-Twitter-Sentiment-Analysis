## function that performs creates sentiment scores before and after an influencers tweets

##Note: this function takes two uncleaned datasets from GOT3: influencer_tweets (x) and general_tweets (y)

##Note: time_period_before specifies how long before influencer tweet to mean sentiment, and vice versa for after

## filter words should be a list of words (character type) to exclude from the sentiment, stop words specific to your data set of interest

##packages required to execute function: tidyverse, tidytext, lubridate, textdata

##function

sentiment_change <- function(x, y, time.period.before = 24, time.period.after = 24, filter.words) {
  bing <- get_sentiments("bing")
  v <- c(positive="1", negative="-1")
  bing$value <- as.character(v[bing$sentiment])
  bing$value <- as.numeric(bing$value)
  bing <- select(bing, word, value)
  
  x <- x %>%
    mutate(date = as.character(x$date)) %>%
    mutate(date = as.POSIXct(x$date, format= "%Y-%m-%d %H:%M:%S"))
    
  y <- y %>%
    mutate(date = as.character(y$date)) %>%
    mutate(date = as.POSIXct(y$date, format= "%Y-%m-%d %H:%M:%S")) %>%
    mutate(text = as.character(y$text)) %>%
    mutate(tweet = c(n():1)) %>%
    unnest_tokens(word, text)
  
  for (f in 1:length(filter.words)) {
    y <- y %>%
      filter(!(word==filter.words[f]))
  }
  
  y <- y %>%
    inner_join(bing) %>%
    group_by(tweet, date) %>%
    summarize(value=sum(value))
  
  mean_before <- rep(0, length(x$date))
  mean_after <- rep(0, length(x$date))
  
  for (i in 1:length(x$date)) {
    datei <- x$date[i]
    before_bound <- as.POSIXct(format(datei-(3600*time.period.before), "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    before <- filter(y, date > before_bound & date < datei)
    mean_before[i] <- mean(before$value)
    
    after_bound <- as.POSIXct(format(datei+(3600*time.period.after), "%Y-%m-%d %H:%M:%S"), format="%Y-%m-%d %H:%M:%S")
    after <- filter(y, date < after_bound & date > datei)
    mean_after[i] <- mean(after$value)
  }
  
 sent_change <- as_tibble(cbind(mean_before, mean_after))
 sent_change <- mutate(sent_change, sentiment_change = mean_after - mean_before)
 
 return(sent_change)
}

## test with my original data to verify function works

testing <- sentiment_change(rdt_eco, general_eco, 12,12,c("trump"))

## if you want to use this function to find the sentiment change for multiple time periods, nest it in a for loop

sentiment_change_over_time <- rep(0,24)

for (j in 1:24) {
  results <- sentiment_change(rdt_eco, general_eco, time.period.after = j, filter.words = c("trump"))
  sentiment_change_over_time[j] <- mean(results$sentiment$change)
}












