library(tidyverse)


candidates_in_hiphop <- read_csv("genius_hip_hop_lyrics_csv.csv")


mentions <- ggplot(candidates_in_hiphop) + geom_bar(aes(x = candidate, fill = candidate)) + 
  ylab("Number of Time Mentioned")


positive_sentiment <- subset(candidates_in_hiphop, sentiment == "positive")


positive_mentions <- ggplot(positive_sentiment) + geom_bar(aes(x = candidate, fill = candidate)) + 
  ylab("Number of times mentioned positively")


ggplot(candidates_in_hiphop) + geom_freqpoly(aes(x = album_release_date)) + xlim(1988,2016)


trump_only <- subset(candidates_in_hiphop, candidate == "donald trump")


trump_sentiment <- ggplot(trump_only) + geom_bar(aes(x = sentiment, fill = theme)) + 
  xlab("Sentiment of Donald Trump in a Hip Hop Song") + ylab("Number of Songs")

trump_theme <- ggplot(trump_only) + geom_bar(aes(x = theme, fill = sentiment)) + 
  xlab("Context of which Donald Trump is talked about") + ylab("Number of Songs")


trump_positive <- subset(trump_only, sentiment == "positive")


trump_negative <- subset(trump_only, sentiment == "negative")


trump_neutral <- subset(trump_only, sentiment == "neutral")


trump_sentiment_time <- ggplot() + 
  geom_smooth(data = trump_positive, aes(x = album_release_date, y = ..count.., fill = sentiment), 
              stat = "bin", color = "green") +
  geom_smooth(data = trump_neutral, aes(x = album_release_date, y = ..count.., fill = sentiment), 
              stat = "bin", color = "black") + 
  geom_smooth(data = trump_negative, aes(x = album_release_date, y = ..count.., fill = sentiment), 
              stat = "bin", color = "red") + 
  xlab("Year") + ylab("Number of Trump Mentions") + ggtitle("Positive, Negative, and Neutral Mentions of Trump Over Time")


legend(1990, 15, legend = c("positive", "neutral", "negative"), col = c("green", "black", "red"))
 


trump_money <- subset(trump_only, theme == "money")


trump_politics <- subset(trump_only, theme == "political")


trump_theme_time <- ggplot() + 
  geom_smooth(data = trump_money, aes(x = album_release_date, y = ..count..), stat = "bin", color = "green") +
  geom_smooth(data = trump_politics, aes(x = album_release_date, y = ..count..), stat = "bin", color = "red") + 
  xlab("Year") + ylab("Number of Trump mentions")
