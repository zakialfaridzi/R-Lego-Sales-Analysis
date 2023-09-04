library(tidyverse)
library(dsbox)

glimpse(lego_sales)

# top 3 most common first names
lego_sales %>% 
  count(first_name) %>% 
  arrange(desc(n)) %>% 
  top_n(3)

# top 3 most purchased themes
lego_sales %>% 
  count(theme, sort = TRUE)

# the most common subtheme
lego_sales %>% 
  filter(theme=='Star Wars') %>% 
  count(subtheme, sort = TRUE)

# age grouping and see what's the most common age group
lego_sales <- lego_sales %>% 
  mutate(age_group = case_when(
    age<=18 ~ "18 and under",
    age>=19 & age <=25 ~ "19 - 25",
    age>=26 & age <=35 ~ "26 - 35",
    age>=36 & age <=50 ~ "36 - 50",
    age>=51 ~ "51 and over"
  )) %>% 
  relocate(age_group)

lego_sales %>% 
  count(age_group, sort = TRUE)

# purchase quantities by age
lego_sales %>% 
  group_by(age_group) %>% 
  summarise(total_n = sum(quantity)) %>% 
  arrange(total_n)

# age group with the most spendings
lego_sales %>% 
  mutate(amount_spent = us_price*quantity) %>% 
  group_by(age_group) %>% 
  summarise(total_spent = sum(amount_spent)) %>% 
  arrange(total_spent)