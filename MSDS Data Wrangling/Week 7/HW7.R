library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)
library(httr)
library(curl)
library(jsonlite)
library(kableExtra)
library(printr)
library(tidytext)
library(wordcloud)
library(dplyr)

source("api-keys.R")

# Exercise 2

heart_disease_by_year <-function(race_selected = "All")
{
  url <- "https://healthdata.nj.gov/resource/5dpz-3wxj.json"
  
  json_result <- url %>% curl() %>% readLines()
  prettify(json_result)
  
  heart_disease <- json_result %>% fromJSON() %>% as.data.frame()
  head(heart_disease)
  
  heart_disease.clean <- heart_disease %>% 
    mutate(heartdisease = as.numeric(heartdisease)) %>%
    mutate(year = as.numeric(year))
  
  heart_disease.plot <- heart_disease.clean  %>%
    subset(!is.na(heartdisease)) %>%
    filter(race == race_selected)
  
  min_plot <- min(heart_disease.plot$heartdisease)
  max_plot <- max(heart_disease.plot$heartdisease)
  
  heart_disease.plot <- heart_disease.clean  %>%
    subset(!is.na(heartdisease)) %>%
    filter(year != "Target") %>%
    filter(race == race_selected)
  
  heart_disease.target <- heart_disease  %>%
    subset(!is.na(heartdisease)) %>%
    filter(year == "Target") %>%
    filter(race == race_selected)
  
  heart_disease.target <- heart_disease.target$heartdisease %>%
    as.numeric()
  
  ggplot(heart_disease.plot, aes(year, heartdisease)) + 
    geom_point() + 
    geom_smooth() +
    ylim(min_plot, max_plot) +
    ggtitle(paste("Heart Disease vs Year. Race selection:", race_selected)) +
    geom_hline(yintercept = heart_disease.target, color = "red") +
    xlab("Year") +
    ylab("Heart Disease")

  print(heart_disease.plot)  
}

heart_disease_by_year()
heart_disease_by_year("Hispanic")
heart_disease_by_year("Asian")
heart_disease_by_year("Black")
heart_disease_by_year("White")


# Exercise 3

# Most Popular articles  
url.MostPopular <- paste0(
    "https://api.nytimes.com/svc/mostpopular/v2/viewed/1.json?api-key=", 
    api.key.NYTimesMostPop
  )

json_result.MostPopular <- url.MostPopular %>% curl() %>% readLines()

prettify(json_result.MostPopular)

MostPopular.titles <-json_result.MostPopular %>% fromJSON() %>% 
  .$results %>% .$title 

MostPopular.stop_words <-
  tibble(title = MostPopular.titles) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(!grepl("\\d+\\.*\\d*", word)) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(MostPopular.stop_words, aes(word, n)) +
  ggtitle("Top 10 Most common non-stop words in the titles of the Most Popular articles") + 
  xlab("Word in title") +
  ylab("Count") +
  geom_bar(stat="identity", fill = "dodgerblue4") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# World Top Stories
url.WorldTop <- paste0(
  "https://api.nytimes.com/svc/topstories/v2/world.json?api-key=", 
  api.key.NYTimesTopStories
)

json_result.WorldTop <- 
  url.WorldTop %>% fromJSON()

names(json_result.WorldTop)
names(json_result.WorldTop$results)

WorldTop.titles <- json_result.WorldTop %>% .$results %>% .$title 

WorldTop.stop_words <-
  tibble(title = WorldTop.titles) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  filter(!grepl("\\d+\\.*\\d*", word)) %>%  #I add this filter so I keep only words and not numbers
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(WorldTop.stop_words, aes(word, n)) +
  geom_bar(stat="identity", fill = "dodgerblue4") +
  ggtitle("Top 10 Most common non-stop words in the titles of the World Top Stories articles") + 
  xlab("Word in title") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Exercise 4
url_genetics <- "http://www.nature.com/articles/ng.3097/tables/3"

nature_genetics <- url_genetics %>%
  read_html() %>% 
  html_table(fill = TRUE) %>% 
  as.data.frame()

head(nature_genetics)
# checking the table, the rows that contain "Genes with previous literature support (GRAIL)" and 
# "New genes without previous evidence" are always the same.

# part a)
nature_genetics.clean <- nature_genetics %>%
  subset(!(str_trim(Locus..height.SNP.) %in% 
             c("Genes with previous literature support (GRAIL)","New genes without previous evidence")))

head(nature_genetics.clean)

# part b)
nature_genetics.clean <- 
  nature_genetics.clean %>% separate(Prioritization.P.value, c("p_value", "exponential"), sep = " × 10")

nature_genetics.clean$exponential <- nature_genetics.clean$exponential %>%
  str_replace_all("−", "-")

nature_genetics.clean <- nature_genetics.clean %>% 
  mutate(p_value = as.numeric(paste0(p_value,'e',replace_na(exponential,1))))
  
nature_genetics.clean <- subset(nature_genetics.clean, select=-c(exponential))

# Final p-values
nature_genetics.clean$p_value


# part c)
nature_genetics.clean$Top.ranking.reconstituted.gene.sets <- 
  nature_genetics.clean$Top.ranking.reconstituted.gene.sets %>% str_replace_all("\\) ", "\\); ") %>%
  str_trim()


# part d)
nature_genetics.clean %>%
  kable(digits = 20, col.names = c("Locus (height SNP)",
                                   "Gene",
                                   "New locus",
                                   "Prioritization P value",
                                   "Lines of supporting evidence",
                                   "Top-ranking reconstituted gene sets")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
