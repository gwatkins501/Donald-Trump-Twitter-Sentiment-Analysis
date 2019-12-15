
##dickey fuller test, test for unit root/stationarity

library(tseries)
adf.test(sentiment_change_hr) ## dataset from nested for loop

fd <- diff(sentiment_change_hr, differences = 1)

fd <- cbind (c(1:22), fd)
fd <- as_tibble(fd)


## plot of first differences 

ggplot(fd, aes(V1,fd)) +
  geom_col(fill= ("maroon")) +
  ylab("First Difference") +
  ggtitle("First Difference of Sentiment Change After Trump Tweet")


## test for serial correlation
Box.test(fd$fd, type="Ljung-Box")   
