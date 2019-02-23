library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

data <- read_csv("datathon_propattributes.csv")


data %>% select(transaction_date) %>% distinct() %>% view()
data %>% 
  mutate(transaction_date = mdy(transaction_date)) %>%
  select(transaction_date) %>%
  separate(transaction_date, c("year","month", "day"), sep = "-", convert = TRUE) %>%
  # select(month, day, year) %>%
  arrange(desc(year))
  # view()