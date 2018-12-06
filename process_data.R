library(tidyverse)

#I read in the data here from Genius and really like the dataset since politics and rap are two of my favorite things. 
#There were many things I expected but also a few surprises as well. 

candidates_in_hiphop <- read_csv("genius_hip_hop_lyrics_csv.csv")

# I created the ggplot below because I thought it was the most basic and important one to create first. It simply shows 
#me all the presidential candidates in 2016 that were mentioned in a hip-hop song and how many times they were 
#mentioned. 

mentions <- ggplot(candidates_in_hiphop, aes(x = candidate, fill = candidate)) + geom_bar() + 
  ylab("Number of Time Mentioned")

#After seeing (and expecting) the popularity of Trump, I decided to focus on him and see if I could notice and analyze 
#anything else in the data over time regarding Trump. 

trump_only <- subset(candidates_in_hiphop, candidate == "donald trump")

#For the two graphs below, I wanted to see how many of the Trump references were positive, negative, and neutral, and the 
#dataset even has a theme category so I was able to see the general subject of the line too. What I found was Trump's 
#money was very popular and his politics was very unpopular. 

trump_sentiment <- ggplot(trump_only) + geom_bar(aes(x = sentiment, fill = theme)) + 
  xlab("Sentiment of Donald Trump in a Hip Hop Song") + ylab("Number of Songs")

trump_theme <- ggplot(trump_only) + geom_bar(aes(x = theme, fill = sentiment)) + 
  xlab("Context of which Donald Trump is talked about") + ylab("Number of Songs")


trump_positive <- subset(trump_only, sentiment == "positive")


trump_negative <- subset(trump_only, sentiment == "negative")


trump_neutral <- subset(trump_only, sentiment == "neutral")

## Here I wanted to do a line graph to see the sentiment and two variables over time and it was pretty cool to see 
# how right around the time Trump started running for president, negative mentions of him skyrocekted and positive 
#ones declined. 

trump_sentiment_time <- ggplot() + 
  geom_smooth(data = trump_positive, aes(x = album_release_date, y = ..count.., fill = sentiment), 
              stat = "bin", color = "green") +
  geom_smooth(data = trump_neutral, aes(x = album_release_date, y = ..count.., fill = sentiment), 
              stat = "bin", color = "black") + 
  geom_smooth(data = trump_negative, aes(x = album_release_date, y = ..count.., fill = sentiment), 
              stat = "bin", color = "red") + 
  xlab("Year") + ylab("Number of Trump Mentions") + ggtitle("Positive, Negative, and Neutral Mentions of Trump Over Time")

#Rappers also started talking more about his politics and less about his money.

trump_money <- subset(trump_only, theme == "money")


trump_politics <- subset(trump_only, theme == "political")


trump_theme_time <- ggplot() + 
  geom_smooth(data = trump_money, aes(x = album_release_date, y = ..count..), stat = "bin", color = "green") +
  geom_smooth(data = trump_politics, aes(x = album_release_date, y = ..count..), stat = "bin", color = "red") + 
  xlab("Year") + ylab("Number of Trump mentions")
