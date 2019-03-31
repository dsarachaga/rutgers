library(tidyverse)
library(lubridate)
library(magrittr)
library(Lahman)
library(babynames)
library(kableExtra)
library(printr)



phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

# part a
phd_field %>% split(.$major_field) %>% 
  map(~ lm(n_phds ~ year + field, data = .)) %>% 
  map(summary) %>% 
  map_dbl("r.squared")



phd_field %>% split(.$major_field) %>% 
  map(~ lm(n_phds ~ year + field, data = .)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tibble(major_field = names(.), slope = .) %>%
  left_join(.,phd_field) %>%
  select(broad_field,slope) %>%
  ggplot(aes(broad_field, slope)) + geom_boxplot(color = 'dodgerblue4', fill="deepskyblue3")


# part b
phd_field %>% mutate(field_rank = min_rank(desc(n_phds))) %>%
  arrange(field, field_rank)


na.omit(phd_field %>%
  group_by(major_field,field) %>% 
  summarize(total_n_phds = sum(na.omit(n_phds))) %>%
  mutate(field_rank = min_rank(desc(total_n_phds))) %>%
  arrange(major_field,field, field_rank))

na.omit(phd_field %>%
  group_by(broad_field,year) %>% 
  summarize(total_n_phds = sum(na.omit(n_phds))) %>%
  mutate(field_rank = min_rank(desc(total_n_phds))) %>%
  arrange(broad_field,year, field_rank))

# part c

phd_field %>%
  group_by(field,year) %>% 
  summarize(total_n_phds = sum(na.omit(n_phds))) %>%
  arrange(field, year, total_n_phds) %>%
  filter (total_n_phds > lag(total_n_phds))
 

na.omit(phd_field %>%
  group_by(broad_field, quartile = ntile(n_phds, 4)) %>%
  summarize(total_n_phds = sum(na.omit(n_phds))))
  
  
# part d

signa_to_noise_ratio <-function(string)
{
  
  if (string %in% phd_field$broad_field)
  {
    
    sub = na.omit(phd_field[which(phd_field$broad_field==string),])
    sig_noise_ratio = mean(sub$n_phds)/sd(sub$n_phds)
    paste("The signal-to-noise ratio for %s is %f", string, sig_noise_ratio)
  }
  else print("The string is not a valid broad_field")
  
}


signa_to_noise_ratio("Life sciences")
signa_to_noise_ratio("wrong string")




