library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

housing <- read_csv("datathon_propattributes.csv") %>%
  mutate(transaction_date = mdy(transaction_date)) %>%
  mutate(day = day(transaction_date), 
         month = month(transaction_date), 
         year = year(transaction_date)) %>%
  mutate(year = ifelse(year <= 2018, year, year - 100 ),
         price_category = cut(sale_amt,
                              c(0,
                                1000,
                                5000, 
                                10000, 
                                50000, 
                                100000,
                                200000, 
                                300000,
                                600000, 
                                1000000,
                                10000000),
                              dig.lab=10)) %>%
  arrange(desc(year))
  
housing %>% select(prop_state) %>% distinct()

xana <- housing %>% filter(is.na(distressed_sale_flg),
                !is.na(geocode_latitude), 
                !is.na(geocode_longitude)) %>%
  select(geocode_latitude, geocode_longitude)



us <- map_data("state")
us %>% select(region) %>% distinct() %>% view()

ggplot() +
  geom_polygon(data = filter(us, region %in% c("massachusetts", 
                                               "rhode island", 
                                               "pennsylvania", 
                                               "new york", 
                                               "conneticut",
                                               "new hampshire",
                                               "new jersey",
                                               "vermont")), 
               aes(long, lat, group = group), 
               fill = "white", colour = "black") +
  geom_point(data = filter(housing,
                           !is.na(geocode_longitude), 
                           !is.na(geocode_latitude),
                           !is.na(price_category),
                           prop_state == "PA") %>% 
               select(geocode_longitude, 
                      geocode_latitude, 
                      price_category),
             aes(geocode_longitude, 
                 geocode_latitude,
                 color = price_category)) +
  coord_quickmap() +
  xlim(-81, -74.5) +
  ylim(39.5, 42.5)

ggplot() +
  geom_point(aes(head(housing$total_living_square_feet, 10000), head(housing$sale_amt, 10000))) + 
  scale_y_log10()

housing$avm_std_deviation0

#geom_point(data=xana, aes(x=geocode_longitude, y=geocode_latitude)) 
# c("massachusetts", 
#   "rhode island", 
#   "pennsylvania", 
#   "new york", 
#   "conneticut",
#   "new hampshire",
#   "new jersey",
#   "vermont"))