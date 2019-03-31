library(tidyverse)
library(lubridate)
library(magrittr)
library(Lahman)
library(babynames)
library(kableExtra)
library(printr)
library(tidytext)
library(wordcloud)
library(dplyr)
  
library(gutenbergr)
books <- gutenberg_download(gutenberg_id = c(11, 1400),meta_fields = "title")


# (a) Find the 10 most common non-stop-words in Great Expectations. Create a world cloud of them.
books %>% distinct(title)

great_expectations <- books %>%
  filter(title == 'Great Expectations') 

great_expectations.tbl <- 
  tibble(text = great_expectations$text) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, "(?i)^chapter [0-9ivxlc]")))

tidy_great_expectations <- great_expectations.tbl %>%
  unnest_tokens(word, text)

tidy_great_expectations %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

tidy_great_expectations %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))


# (b) Find the 10 most common bigrams in Great Expectations that do not include stop words.
tidy_great_expectations_bigram <- great_expectations.tbl %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

great_expectations_bigrams_sep <- tidy_great_expectations_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

great_expectations_bigrams_filtered <- great_expectations_bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

na.omit(great_expectations_bigrams_filtered %>% 
  mutate(bigram = str_c(word1, ' ', word2)) %>%
  count(bigram, sort = TRUE)) %>%
  head(10)


# (c) Plot the sentiment for the two books.

tidy_books <- books %>%
  group_by(title) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()  %>%
  unnest_tokens(word, text)

books_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(books_sentiment, aes(index, sentiment, fill = title)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 3, scales = "free_x") + 
  theme(legend.position = "none") +
  geom_smooth(span = .15)


# Exercise 2
fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

# part a)
fed_rd.new <- fed_rd %>%
  mutate(rd_budget_frac = rd_budget/total_outlays)

fit <- lm(rd_budget_frac ~ department + year, fed_rd.new)

conf.int <- augment(fit) %>%
  mutate(fit = .fitted) %>%
  mutate(upper = .fitted + 1.96 * .se.fit) %>%
  mutate(lower = .fitted + 1.96 * .se.fit) %>%
  select(rd_budget_frac, department, year, fit, upper, lower)

fed_rd.new <- fed_rd.new %>%
  left_join(.,conf.int)

  

# part b)
ploting <- function(department){
  
  coefs.plot=fed_rd.new[which(fed_rd.new$department==department),]
  
  ggplot(coefs.plot, aes(year, rd_budget_frac))+
    geom_point()+
    geom_line(aes(y=lower), color = "red", linetype = "dashed")+
    geom_line(aes(y=upper), color = "red", linetype = "dashed")+
    geom_smooth()
}

paste("The department wise plots are as shown below")
ploting("NASA")
ploting("NSF")
ploting("DHS")
ploting("DOD")



# exercise 3
# part a)
getwd()

untidy_migrant <- readxl::read_excel("UN_MigrantStockByOriginAndDestination_2015.xlsx", sheet = "Table 16", skip = 15)

head(untidy_migrant)

untidy_migrant <- untidy_migrant %>%
  rename(to_country = X__2)

untidy_migrant <- untidy_migrant %>%
  rename(country_code = X__4)

head(untidy_migrant)

migrate_to_canada <- untidy_migrant[which(untidy_migrant$to_country == "Canada"),]
migrate_to_canada <- migrate_to_canada %>% gather(`Afghanistan`:`Zimbabwe`, key = "from_country", value = "count")

migrate_to_canada %>%
  arrange(desc(count)) %>%
  head(5)

# part b)
migrate_from_canada <- untidy_migrant[which(untidy_migrant$from_country == "Canada"),]
migrate_from_canada <- migrate_from_canada %>% gather(`Afghanistan`:`Zimbabwe`, key = "to_country", value = "count")


# I exclude the country codes from the agrupations, that is those higher or equal than 900
migrate_from_canada <- untidy_migrant %>%filter(country_code < 900)
head(arrange(migrate_from_canada, desc(Canada)), n = 5)$to_country


# part c)
migration_pairs <- untidy_migrant %>%
  filter(country_code < 900) %>%
  gather(`Afghanistan`:`Zimbabwe`, key = "from_country", value = "count") %>% arrange(desc(count))

#Excluding the unnecessary columns
drop_columns <- c("Total","Other North", "Other South", "Country_code", "..1", "..3", "..5")
pairs_top_10 <- migration_pairs[ , !(names(migration_pairs) %in% drop_columns)]

pairs_top_10 %>%
  select(to_country, from_country) %>%
  head(10)

