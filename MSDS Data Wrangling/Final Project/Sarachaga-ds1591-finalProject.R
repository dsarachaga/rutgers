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
library(xml2)
library(lubridate)
library(rtweet) 


# Twitter API
appname <-'shopping&weather'
consumer_key <- "3G4AK5R7gF74I0GREacgwZI1b"
consumer_secret <-"R8ELe7ZiAI5sgFm0oMGF7SiJ4a2fb4XoMGjIWZHfJ6qn28jUg5"
access_token <- "307057822-c34aV7twDynLraoRpPbIbwTKZkW2IWqYdWgNLsnS"
access_secret <- "1vszbrxXUxbjuoXiD5jaIFA8H36ZfYWk3Hinq7kc1DD6U" 

twitter_token <- create_token(
  app = appname,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret
)

identical(twitter_token, get_token())


#Google API token AIzaSyBoMKV52wHwaYdZ_I2anLcJgsGX6nCrcTM
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
  filter(grepl('New York|NYC|New York City|Manhattan|Bronx|Brooklyn|Queens', location))


unique(rt.NYC$location)

nrow(rt.NYC)

# Rounding the hours to facilitate the analysis.
rt.NYC = rt.NYC %>%
  mutate(created_at = round_date(created_at, unit = 'hour'))



# Weather information
weather_url = paste0("https://api.weather.com/v1/geocode/40.77/-73.86/observations/historical.json?apiKey=6532d6454b8aa370768e63d6ba5a832e&startDate=",first_date,"&endDate=",last_date,"&units=e")
weather_raw <- weather_url %>% read_html()

df_tbl_weather <- weather_raw %>%
  html_text() %>%
  # what we got here is a JSON file, and use jsonlite to parse it.
  jsonlite::fromJSON() %>% .[["observations"]] %>%
  select(valid_time_gmt, ## Time
         temp,           ## Temperature
         dewPt,          ## Dew Point
         rh,             ## Humidity
         wdir_cardinal,  ## Wind
         wspd,           ## Wind Speed
         gust,           ## Wind Gust
         pressure,       ## Pressure
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

df_delivery = rt.NYC[, c("created_at", "text", "hashtags", "lang", "country", "country_code", "geo_coords", "coords_coords", "location")]

class(df_tbl_weather)

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


register_google(key = "AIzaSyAHNuFJt59VQvqWXTHfm2GtcZFsYYhk9qc")

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
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


