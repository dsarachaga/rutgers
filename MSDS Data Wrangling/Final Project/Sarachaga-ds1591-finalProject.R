#Data Wrangling project

#websites
# https://ritetag.com/best-hashtags-for/shopping
# https://developer.twitter.com/en/apps
# https://developer.twitter.com/en/account/get-started
# https://towardsdatascience.com/access-data-from-twitter-api-using-r-and-or-python-b8ac342d3efe
# https://rpubs.com/chrisbrunsdon/twitterclouds


knitr::opts_chunk$set(echo=TRUE)
library(dplyr)
library(lubridate)
library(magrittr)
library(rvest)
library(stringr)
library(tidyverse)
library(tidytext)
library(xml2)
library(lubridate)
library(rtweet) 
library(ggplot2)
library(dplyr)
library(ggmap)
library(igraph)
library(ggraph)
library(wordcloud)
library(jsonlite)

set.seed(1)

# Getting API's keys
source("API-keys.R")

twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)

identical(twitter_token, get_token())


rt.1 <- search_tweets(
  q = "\"food delivery\" -filter:retweets -filter:quote",
  n = 18000,
  geocode = lookup_coords("New York City, NY"),
  include_rts = FALSE
)

rt.2 <- search_tweets(
  q = "delivery OR #onlineshopping OR #online OR #fooddelivery OR #delivery OR #grubhub OR #ubereats OR #seamless OR #onlinefooddelivery OR #onlinedelivery OR #onlinefood -filter:retweets -filter:quote",
  n = 18000,
  geocode = lookup_coords("New York City, NY"),
  include_rts = FALSE
)

rt = rbind(rt.1, rt.2)

nrow(rt.1)
nrow(rt.2)
nrow(rt)

# Getting the first day from the Twitter response
if ((month(min(rt$created_at)) < 10) &(day(min(rt$created_at)) < 10)){
    first_date = paste0(toString(year(min(rt$created_at))),'0', toString(month(min(rt$created_at))),'0', toString(day(min(rt$created_at))))
  }else if (day(min(rt$created_at)) >= 10){
    first_date = paste0(toString(year(min(rt$created_at))),'0', toString(month(min(rt$created_at))), toString(day(min(rt$created_at))))
    }else{
    first_date = paste0(toString(year(min(rt$created_at))), toString(month(min(rt$created_at))), toString(day(min(rt$created_at))))
    }
  

# Getting the last day from the Twitter response
if ((month(max(rt$created_at)) < 10) &(day(max(rt$created_at)) < 10)){
  last_date = paste0(toString(year(max(rt$created_at))),'0', toString(month(max(rt$created_at))),'0', toString(day(max(rt$created_at))))
}else if (day(max(rt$created_at)) >= 10){
  last_date = paste0(toString(year(max(rt$created_at))),'0', toString(month(max(rt$created_at))), toString(day(max(rt$created_at))))
}else{
  last_date = paste0(toString(year(max(rt$created_at))), toString(month(max(rt$created_at))), toString(day(max(rt$created_at))))
}

unique(rt$location)

rt %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n))

# Filtering online tweets that have a location in NYC
rt.NYC = rt %>%
  filter(grepl('New York|NYC|New York City|Manhattan|Bronx|Brooklyn|Queens', location)) %>%
  filter(lang == "en")


unique(rt.NYC$location)
unique(rt.NYC$lang)

nrow(rt.NYC)

# Rounding the hours to facilitate the analysis.
rt.NYC = rt.NYC %>%
  mutate(created_at = round_date(created_at, unit = 'hour'))



# Weather information
weather_url = paste0("https://api.weather.com/v1/geocode/40.77/-73.86/observations/historical.json?apiKey=",weatherAPI,"&startDate=",first_date,"&endDate=",last_date,"&units=e")
weather_raw <- weather_url %>% read_html()

df_tbl_weather <- weather_raw %>%
  html_text() %>%
  # what we got here is a JSON file, and use jsonlite to parse it.
  fromJSON() %>% .[["observations"]] %>%
  select(valid_time_gmt, ## Time
         temp,           ## Temperature
         dewPt,          ## Dew Point
         rh,             ## Humidity
         wdir_cardinal,  ## Wind
         wspd,           ## Wind Speed
         precip_hrly,    ## Precip.
         precip_total,   ## Precip Accum
         wx_phrase       ## Condition 
  ) %>%
  # original time is UNIX time. 
  mutate(valid_time_gmt = as_datetime(valid_time_gmt, tz = "EST"))


names(df_tbl_weather) <- c("Time", "Temperature", "Dew Point", "Humidity", "Wind", "Wind Speed", "Wind Gust", "Pressure", "Precip.", "Precip Accum", "Condition")

head(df_tbl_weather)

# Rounding the hours to facilitate the analysis.
df_tbl_weather = df_tbl_weather %>%
  mutate(Time = round_date(Time, unit = 'hour'))


#
colnames(rt.NYC)

df_delivery = rt.NYC[, c("created_at", "text", "source", "hashtags", "lang", "country", "country_code", "geo_coords", "coords_coords", "location")]


df_weather = df_tbl_weather[, c("Time", "Temperature", "Precip.", "Precip Accum", "Condition")]


df_delivery.final <- left_join(df_delivery,df_weather, by = c("created_at" = "Time"))

df_delivery.final <- df_delivery.final[complete.cases(df_delivery.final[ , "Temperature"]),]



df_delivery.final %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")



df_delivery.final %>%
  count(Temperature, sort = TRUE) %>%
  mutate(Temperature = reorder(Temperature, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = Temperature, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")

df_delivery.final %>%
  count(Precip., sort = TRUE) %>%
  mutate(Precip. = reorder(Precip., n)) %>%
  top_n(20) %>%
  ggplot(aes(x = Precip., y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")

unique(df_delivery.final$Condition)

df_delivery.final %>%
  count(Condition, sort = TRUE) %>%
  mutate(Condition = reorder(Condition, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = Condition, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Location",
       title = "Where Twitter users are from - unique locations ")



df_delivery.final <- lat_lng(df_delivery.final)


get_map(location = c(-73.94,40.78), zoom = 12) %>% 
  ggmap() +
  geom_point(data = df_delivery.final, aes(x = lng, y = lat), col = rgb(0, .3, .7, .75), size=3, alpha=0.5)


#plot the density map
get_map(location = c(-73.94,40.78), zoom = 12) %>% 
  ggmap() +
  stat_density2d(
  aes(x = lng, y = lat, fill = ..level.., alpha = ..level..*2), 
  size = 2, bins = 5, data = df_delivery.final, geom = "polygon") +
  scale_fill_gradient()


ts_plot(df_delivery.final)
## plot time series of tweets
ts_plot(df_delivery.final, "3 hours") +
  theme_minimal() +
  theme(plot.title = ggplot2::element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


df_delivery.final %>% 
  group_by(source)%>% 
  summarise(Total=n()) %>% 
  arrange(desc(Total)) %>% 
  head(10) %>%
  ggplot(aes(reorder(source, Total), Total, fill = source)) + geom_bar(stat="identity", fill = rgb(0, .3, .7, .75)) + 
  coord_flip() + 
  labs(title="Top Tweet Sources", x="", subtitle="There were more tweets coming from iPhone Vs Android smartphones", caption = "\nSource: Data collected from Twitter's REST API via rtweet")+
  theme_minimal()


# First, remove http elements manually
df_delivery.final$stripped_text <- gsub("http.*","", df_delivery.final$text)
df_delivery.final$stripped_text <- gsub("https.*","", df_delivery.final$stripped_text)

# Second, remove punctuation, convert to lowercase, add id for each tweet!

df_delivery.final_clean <- df_delivery.final %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# Third, remove stop words from your list of words
cleaned_tweet_words <- df_delivery.final_clean %>%
  anti_join(stop_words)

# Finally, plot the top 15 words
cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  geom_col(position = "dodge",fill = rgb(0, .3, .7, .75))+
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most Common words found in #delivery tweets",
       subtitle = '"delivery220" , "Stipe" and "stipemiocicdelivery" tops our most used words"', caption = "\nSource: Data collected from Twitter's REST API via rtweet") + 
  theme_minimal() + 
  theme(legend.position = "") 



# remove punctuation, convert to lowercase, add identity for each tweet!
delivery_tweets_paired_words <- df_delivery.final %>%
  select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

delivery_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

delivery_tweets_separated_words <- delivery_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

delivery_tweets_filtered <- delivery_tweets_separated_words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
delivery_words_counts <- delivery_tweets_filtered %>%
  count(word1, word2, sort = TRUE)


# plot #delivery word network
delivery_words_counts %>%
  filter(n >= 24) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "dodgerblue4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets using the hashtag - #delivery",
       subtitle = "Text mining twitter data ",
       x = "", y = "", caption = "\nSource: Data collected from Twitter's REST API via rtweet") + theme_minimal()



hashtagFreq = tolower(as_vector(df_delivery.final$hashtags)) %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) 

names(hashtagFreq) = c("hashtag", "freq")

png("wordcloud_packages.png", width=1280,height=800)
suppressWarnings(wordcloud(hashtagFreq$hashtag,
          hashtagFreq$freq, 
          scale=c(8,.3),
          min.freq=2,
          max.words=Inf, 
          random.order=FALSE, 
          rot.per=.15, 
          colors=brewer.pal(6, "Dark2")))


groups = unique(df_delivery.final$Condition)
df_delivery.final$ConditionAsNum = as.numeric(factor(df_delivery.final$Condition, levels=groups))

df_delivery.cor = df_delivery.final %>% 
  group_by(Temperature,Precip., ConditionAsNum)%>% 
  summarise(Total=n()) %>% 
  arrange(desc(Total)) 

res = round(cor(df_delivery.cor),2)

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#There is a slight positive correlation between temperature and weather condition
#Non of the weather variables seem to have a correlation with the amount of tweets. Moreover, they all have a weak negative linear relationship.


aux = df_delivery.final %>%
  count(Temperature, sort = TRUE) %>%
  top_n(20) %>%
  arrange(Temperature)


ggplot(aes(x = Temperature, y = n)) +
  geom_bar(stat="identity", fill = rgb(0, .3, .7, .75)) + 
  coord_flip() +
  labs(x = "Count",
       y = "Temperature",
       title = "What was the Temperature when the Tweets was written?")
