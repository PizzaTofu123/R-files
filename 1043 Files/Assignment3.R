# B.2
twitter_data <- read.csv("Twitter_Data_1.csv", header = FALSE, sep = "\t")
time_stamp <- strptime(twitter_data[[1]], "%a %b %d %H:%M:%S %z %Y", tz = "UTC")

# B.3
hist(time_stamp,
     main = "Donald Trump tweets over time",
     xlab = "Date",
     ylab = "Frequency of tweets",
     breaks = 100,
     freq=TRUE,
     ylim = c(0,10)
)

# B.5
twitter_data <- read.csv("tweets_author.csv", header = FALSE)
hist(twitter_data$V1, freq = TRUE, breaks = 200, xlim=c(0,15), xlab = "Number of tweets", main = "Frequency for each number of
     tweets per person")

