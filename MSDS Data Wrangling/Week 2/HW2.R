library(tidyverse)
library(magrittr)
library(lubridate)


tv <- read_csv('https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv')

# part a
tv %>% filter(genres == "Action,Adventure,Drama", date > ymd("17/1/1")) %>% 
  group_by(title, seasonNumber, date) %>% 
  arrange(desc(av_rating))

# part b
tv %>% group_by(title) %>% summarise(frequency = n()) %>% arrange(desc(frequency))
dat <- filter(tv, title == "Law & Order" | title == "Law & Order: Special Victims Unit")
ggplot(dat, aes(date, av_rating, color = title)) + 
  geom_point() + geom_line() + geom_smooth() +
  ggtitle("date and rating") + 
  xlab("date") + 
  ylab("rating")

# part c
dat <- filter(tv, title == "Criminal Minds")
ggplot(dat, aes(date, av_rating, color = title)) + 
  geom_point() + geom_line() + geom_smooth() +
  ggtitle("date and rating") + 
  xlab("date") + 
  ylab("rating")



