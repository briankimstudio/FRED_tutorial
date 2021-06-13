# dealmoa@gmail.com
# Fredbundan123$
# key 4fa77f3b24c813f8a276a5083bc2855a

install.packages("fredr")
library(fredr)
library(ggplot2)
library(tidyverse)
fredr_set_key("4fa77f3b24c813f8a276a5083bc2855a")

# UNRATE : unemployment rate
dataset <- fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)

dataset %>%
  ggplot(aes(x=date,y=value)) +
  labs(title="Unemployment rate in USA", x="Year",y="%") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "year" , date_labels = "%Y") +
  geom_line()


popular_funds_series <- fredr_series_search_text(
  search_text = "federal funds",
  order_by = "popularity",
  sort_order = "desc",
  limit = 1
)

popular_funds_series_id <- popular_funds_series$id

popular_funds_series_id %>%
  fredr(
    observation_start = as.Date("1990-01-01"),
    observation_end = as.Date("2000-01-01")
  ) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "Rate", color = "Series")

library(purrr)


map_dfr(c("UNRATE", "FEDFUNDS"), fredr) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "Rate", color = "Series")

fredr_series_search_tags(
  series_search_text = "unemployment",
  limit = 100L
)

a<-fredr_category_series(category_id = 1L, limit = 10L, order_by = "popularity")

fredr("HLTHSCEXPHCSA")

library(purrr)

map_dfr(c("UNRATE", "JTSJOL"), fredr) %>%
  ggplot(data = ., mapping = aes(x = date, y = value, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = "Rate", color = "Series")

dataset <- fredr(
  series_id = c("UNRATE", "JTSJOL"),
  observation_start = as.Date("1990-01-01"),
  observation_end = as.Date("2000-01-01")
)

dataset <- map_dfr(c("UNRATE", "JTSJOL"), fredr)
dataset_wider <- dataset %>%
  select(date,series_id,value) %>% pivot_wider(names_from="series_id", values_from="value")

dataset_wider %>% 
  drop_na() %>%
  ggplot(aes(x=date)) +
  geom_line( aes(y=UNRATE), color="red") + # Divide by 10 to get the same range than the temperature
  geom_line( aes(y=JTSJOL/1000), color="blue") + 
  scale_y_continuous(
    name = "Unemployment rate",
    sec.axis = sec_axis(~.*1000,name="Job openings")) +
  theme(axis.title.y.left = element_text(colour = "red"),
        axis.title.y.right = element_text(colour = "blue"))


b<-a %>% select(date,series_id,value) %>% pivot_wider(names_from="series_id", values_from="value")

b %>%
  ggplot(aes(x=date)) +
  geom_line( aes(y=JTSJOL)) 

  
b %>% drop_na() %>%
  ggplot(aes(x=date)) +
  geom_line( aes(y=UNRATE), size=1, color="red") + # Divide by 10 to get the same range than the temperature
  geom_line( aes(y=JTSJOL/1000), size=1,color="blue") + 
  scale_y_continuous(
    name = "Unemployment rate",
    sec.axis = sec_axis(~.*1000,name="Job openings"))
