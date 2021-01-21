library(tidyverse)
d <- read_csv("~/src/applied-stats/data/airbnb.csv")

d <- d %>% 
  mutate(price = str_remove(price, "\\$"))  %>% 
  mutate(price = as.numeric(str_remove(price, ".00"))) %>% 
  filter(price > 60, !is.na(price)) %>% 
  filter(!is.na(review_scores_rating)) %>% 
  filter(!is.na(room_type)) %>% 
  filter(!is.na(host_is_superhost))

summary(lm(price ~ room_type, data = d))
summary(lm(log(price) ~ room_type, data = d))

summary(lm(price ~ review_scores_rating, data = d))
summary(lm(log(price) ~ log(review_scores_rating), data = d))

mod <- lm(log(price) ~ room_type+log(review_scores_rating)+host_is_superhost, data = d)

plot(d$review_scores_rating, resid(mod))
cor(d$review_scores_rating, resid(mod))

d %>% 
  ggplot(aes(price, fill = room_type)) + 
  geom_histogram(aes(y = ..density..), position = 'dodge') + scale_x_log10()
d %>% 
  ggplot(aes(host_is_superhost, price, fill = host_is_superhost)) + geom_boxplot() + 
  scale_y_log10()

d %>% 
  group_by(room_type) %>% 
  summarize(cor = cor(review_scores_rating, price))

summary(mod)
