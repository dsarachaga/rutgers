library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)


# Exercise 2 

cleaner <- function(x){
  str_replace_all(x, " \\([0-9]+\\)|(\n)", "")
}

cleaner <- function(x){
  str_replace_all(x, "(^[ \t]+|[ \t]+$)|(\n)", "")
}

weather <- "https://weather.com/weather/hourbyhour/l/USNJ0524:1:US"
# browseURL(weather)

forecast <- weather %>%
  read_html() %>% 
  html_table(fill = TRUE)

length(forecast)
class(forecast)

class(forecast[[1]])

weather.df <- forecast[[1]]

# The column names are misplaced

# Drop the first column
weather.df <- weather.df[,-1]

length(weather.df)

names(weather.df)[7] <- "Wind_"

weather.df <- weather.df %>%
  rename(Time = Description) %>%
  rename(Description = Temp) %>%
  rename(Temp = Feels) %>%
  rename(Feels = Precip) %>%
  rename(Precip = Humidity) %>%
  rename(Humidity = Wind) %>%
  rename(Wind = Wind_)


weather.df$Time <- weather.df$Time %>% 
  str_replace_all("am", "am - ")  %>% 
  str_replace_all("pm", "pm - ")

weather.df <- weather.df %>% separate(Time, c("Time", "Day"), sep = "-") %>% map_df(cleaner)

# Fixing the time, precipitation and humidity percentage. In Precipitation case, I calculate it as a numerical value 
# (from 0 to 1), so when using it on the plot, it keeps it in order. I put Humidity in a percentage format (0 to 100),
# so when using it on the plot is correctly represented.
weather.clean <- weather.df %>% 
  mutate(Time = parse_time(Time, '%I:%M %p')) %>%
  mutate(Precip = as.numeric(sub("%", "", Precip))/100) %>%
  mutate(Humidity = as.numeric(sub("%", "", Humidity)))

weather.clean$Temp <- weather.clean$Temp %>% 
  str_replace_all("°", "") %>%
  as.numeric()


# Because I am checking the data today, if the day is not the same as today's, then it means that it corresponds to tomorrow
weather.clean1 <- weather.clean %>% 
  filter(wday(today(tzone = "EST"), label=TRUE) == weather.clean$Day) %>%
  mutate("Datetime" = as_datetime(paste(today(tzone = "EST"), Time, sep= ' '), tz = "EST"))


weather.clean2 <- weather.clean %>% 
  filter(wday(today(tzone = "EST"), label=TRUE) != weather.clean$Day) %>%
  mutate("Datetime" = as_datetime(paste(today(tzone = "EST") + 1, Time, sep= ' '), tz = "EST"))

weather.clean <- rbind(weather.clean1,weather.clean2)

weather.clean$Datetime <- format(weather.clean$Datetime,format='%Y%m%d %H:%M')

# Tidyng wind
weather.clean <- weather.clean %>% separate(Wind, c("Wind_Direction", "Wind_Speed", "Wind_measurement"), sep = " ") %>% map_df(cleaner)
weather.clean <- weather.clean %>% mutate(Wind_Speed = as.numeric(Wind_Speed))

weather.plot <- gather(weather.clean, key="measure", value="value", c("Temp",  "Humidity",  "Wind_Speed"))
weather.plot <- weather.plot %>% 
  mutate(value = as.numeric(value))

# time of day on the x-axis against temperature, humidity, and windspeed
ggplot(weather.plot, aes(Datetime, value, group = 1)) +
  geom_line(color = "dodgerblue4") + geom_point(aes(color=Precip)) + 
  ggtitle("Humidity, Temperature and Wind vs Time of the day") + 
  facet_wrap(~ measure, ncol = 1, scales = "free", strip.position = "left", 
             labeller = labeller( measure = c(Temp = "Temperature (ºF)", Humidity = "Humidity (%)", "Wind_Speed" = "Wind (mph)"))) +
  ylab(NULL) +
  xlab("Time of the day") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_color_brewer(palette = "Dark2")


# Exercise 3
youtube <- "https://www.youtube.com/feed/trending" %>%
  read_html() %>% html_nodes("div") %>% 
  html_text()

youtube.clean <- youtube[2] %>% str_extract_all("(?!= Duration).*(?=views)") %>% unlist()

youtube.clean <- tibble(text = youtube.clean)

youtube.clean <- youtube.clean %>% separate(text, c("title", "rest"), sep = "- Duration: ") %>%
  subset(!is.na(rest)) %>%
  mutate(title = str_trim(title))

youtube.clean <- youtube.clean %>% separate(rest, c("minutes", "rest"), sep = ":")

youtube.clean <- youtube.clean %>% separate(rest, c("seconds", "rest"), sep = "\\.", extra = "merge")

youtube.clean <-  youtube.clean %>% separate(rest, c("rest", "views"), sep = " ago") %>%
   subset(!is.na(views))

youtube.clean <-  youtube.clean %>%
  mutate(rank = 1:dim(youtube.clean)[1])

youtube.clean$time_unit <- word(youtube.clean$rest, -1)

youtube.clean$rest <- 
  youtube.clean$rest %>% 
  str_replace_all("(day)s?", "") %>%
  str_replace_all("(week)s?", "") %>%
  str_replace_all("(hour)s?", "") 

youtube.clean <- youtube.clean %>%
    mutate(rest = str_trim(rest))

youtube.clean$time <-
  youtube.clean$rest %>% str_extract_all("[0-9]+$") %>% unlist()

youtube.clean$views <-
  youtube.clean$views %>%
  str_replace_all("[,]|[ ]", "") %>% as.numeric()

# part a
youtube.week <- youtube.clean %>%
  filter (time_unit == "week" | time_unit == "weeks") %>%
  mutate(post_date = (as.numeric(time)*7))

youtube.hour <- youtube.clean %>%
  filter (time_unit == "hour" | time_unit == "hours") %>%
  mutate(post_date = (as.numeric(time)/24))

youtube.day <- youtube.clean %>%
  filter (time_unit == "day" | time_unit == "days") %>%
  mutate(post_date = (as.numeric(time)))


youtube.clean <- rbind(youtube.hour, youtube.day, youtube.week) %>%
  arrange(rank)


# part b
youtube.clean <- youtube.clean %>% 
  mutate(length = as.numeric(minutes) + as.numeric(seconds)/60)

# part c
youtube.clean <- youtube.clean %>% 
  mutate(popularity = views/post_date)

ggplot(youtube.clean, aes(rank, popularity/1000000, label=title)) + 
  geom_point() + 
  geom_smooth() +
  geom_label(aes(label=ifelse(popularity/1000000>5,as.character(title),NA)),hjust=0, vjust=0) +
  ggtitle("Popularity vs Rank") +
  xlab("Rank") +
  ylab("Popularity (M)")



