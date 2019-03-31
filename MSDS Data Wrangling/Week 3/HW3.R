library(tidyverse)
library(lubridate)
library(magrittr)

# install.packages("Lahman")
library(Lahman)
library(babynames)

head(babynames)
head(Master)

# Exercise 2
# part a
Master.babynames <- Master %>%
  mutate(year = birthYear,name = nameFirst) %>% 
  filter(!is.na(year)&!is.na(name)) %>%
  group_by(year, name) %>% 
  summarize(n = n()) %>% 
  select(year, name, n) %>% 
  arrange(desc(n))

# part b
Master.yearCheck <- Master %>% 
  mutate(yearBirthDate = year(ymd(birthDate))) %>% 
  {table(.$birthYear, .$yearBirthDate)} 
   
# part c
head(Fielding)

Master.name <- Master %>% 
  select(playerID, nameFirst, nameLast, nameGiven)
Fielding.G <- Fielding %>% 
  group_by(playerID) %>%
  summarize(careerTotal = sum(G)) 

player_careerTotal <- left_join(Master.name,Fielding.G)

# part d

fullName <- player_careerTotal %>% 
  mutate(fullName = str_c(nameFirst, ' ', nameLast))
  
head(fullName)

# part e
players500 <- player_careerTotal %>% 
  filter (`career total` >= 500) 

Master.players500 <- Master %>% 
  mutate(year = birthYear,name = nameFirst) %>% 
  filter(!is.na(year)&!is.na(name)) %>%
  group_by(year, name) %>% 
  summarize(n = n()) %>% 
  select(year, name, n) %>% 
  arrange(desc(n)) 

topNames <- semi_join(Master.players500, players500, by = c("name" = "first name")) %>%
  group_by(name) %>% 
  summarize(total = sum(n)) %>% 
  top_n(5, total) %>% 
  arrange(desc(total))

topNames.history <- semi_join(Master.players500, topNames, by = c("name" = "name"))

topNames.history %>%
  ggplot(aes(year, n, colour = name)) + 
  ggtitle("Most popular names through the years") + 
  xlab("Year") + 
  ylab("Total") + 
  scale_colour_discrete(name = "name", labels = c("Bill", "John", "Jim", "Mike", "Joe")) +
  geom_smooth()


# Exercise 3

restaurants <- read_csv('https://data.cityofnewyork.us/api/views/9w7m-hzhe/rows.csv?accessType=DOWNLOAD&bom=true&query=select+*')
head(restaurants)

# part a)
rodents <- c("rodent|mice|rat")
restaurants.rodents <- restaurants %>% 
  filter(str_detect(restaurants$`VIOLATION DESCRIPTION`, rodents)) 
  
nrow(restaurants.rodents)

# sum(str_detect(restaurants$`VIOLATION DESCRIPTION`, rodents), na.rm = TRUE)


# Restaurants with a violation for rat
restaurants.rat <- restaurants %>% 
  filter(str_detect(restaurants$`VIOLATION DESCRIPTION`, "rat")) %>%
  group_by(DBA) %>%
  summarize(total = n())

# Number of restaurants with a violation for rat
nrow(restaurants.rat)

# Restaurants with a violation for mice
restaurants.mice <- restaurants %>% 
  filter(str_detect(restaurants$`VIOLATION DESCRIPTION`, "mice")) %>%
  group_by(DBA) %>%
  summarize(total = n())

# Number of restaurants with a violation for mice
nrow(restaurants.mice)


# Are restaurant owners more likely to get cited for rats or mice?
# Restaurantes are more cited for rats

# Are there any cases of restaurants that are cited for both rats and mice?
restaurants.rat_mice <- 
  semi_join(restaurants.rat %>% select(DBA), restaurants.mice %>% select(DBA), by = c("DBA" = "DBA"))

# Number of restaurants with a violation for rat and mice
nrow(restaurants.rat_mice)

# part b)
insects <- c("roach|flies")
restaurants.insects <- restaurants %>% 
  filter(str_detect(restaurants$`VIOLATION DESCRIPTION`, insects)) 

nrow(restaurants.insects)

# sum(str_detect(restaurants$`VIOLATION DESCRIPTION`, insects), na.rm = TRUE)

# Restaurants with a violation for roach
restaurants.roach <- restaurants %>% 
  filter(str_detect(restaurants$`VIOLATION DESCRIPTION`, "roach")) %>%
  group_by(DBA) %>%
  summarize(total = n())

# Number of restaurants with a violation for roach
nrow(restaurants.roach)

# Restaurants with a violation for flies
restaurants.flies <- restaurants %>% 
  filter(str_detect(restaurants$`VIOLATION DESCRIPTION`, "flies")) %>%
  group_by(DBA) %>%
  summarize(total = n())

# Number of restaurants with a violation for flies
nrow(restaurants.flies)


# Are restaurant owners more likely to get cited for roaches or flies?
# Restaurantes are more cited for flies

# part c)
# Are there any restaurants that have been cited for both rodents and insects?

restaurants.bothR_I <- 
  semi_join(restaurants.rodents %>% select(DBA), restaurants.insects %>% select(DBA), by = c("DBA" = "DBA"))

# Number of restaurants with a violation for rat and mice
nrow(restaurants.bothR_I)

# part d)
# Using the boro variable, rank the five boroughs according to the total number of rodent and/or insect violations.
boro_violations <- c("rodent|mice|rat|roach|flies")
restaurants.boro <- restaurants %>% 
  filter(str_detect(restaurants$`VIOLATION DESCRIPTION`, boro_violations)) %>% 
  group_by(BORO) %>%
  summarize(total = n()) %>%
  top_n(5, total) %>% 
  arrange(desc(total))




