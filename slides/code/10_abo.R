library(tidyverse)
library(haven)
library(lme4)

d <- read_dta("~/Downloads/pma_00001.dta")

d <- d %>% 
  filter(aborev<90) %>% 
  select(urban:aborev) %>% 
  mutate_all(funs(as_factor(.))) %>% 
  mutate(resp = 1:n()) %>% 
  select(resp, urban:aborev)

d <- d %>% 
  mutate(abortion = ifelse(aborev=="no", 0, 1)) %>% 
  select(-aborev) %>% 
  mutate(age = as.numeric(age))


regions <- sort(as.character(unique(d$geougdet)))

hdi <- tibble(region = regions, hdi = c(0.54, 0.586, 0.53, 0.526, 0.639, 0.45, 0.475, 0.518, 0.486, 0.499))

d <- d %>% 
  rename(region = geougdet) %>% 
  left_join(hdi)

d %>% 
  group_by(region, hdi) %>% 
  summarize(abo = mean(abortion)) %>% 
  ggplot(aes(hdi, abo)) + geom_point()

d %>% 
  group_by(region, hdi) %>% 
  summarize(abo = mean(abortion)) %>% 
  ungroup() %>% 
  summarize(cor(hdi, abo))

mod <- glmer(abortion ~ (1|region)+age, data = d, family = "binomial")
summary(mod)

d <- d %>% 
  mutate(hdi_c = (hdi - mean(hdi))/sd(hdi))

mod2 <- glmer(abortion ~ (1|region)+ age + hdi_c, data = d, family = "binomial")
summary(mod2)

write_csv(d, "data/pma.csv")

