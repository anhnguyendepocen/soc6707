library(tidyverse)
library(labelled)
#devtools::install_github("hodgettsp/cesR")
library(cesR)

get_decon()

get_ces("ces2019_web")

# convert values to factor type
ces2019_web <- to_factor(ces2019_web)
d <- decon %>% bind_cols(ces2019_web %>% select(cps19_votechoice))

df <- d %>% select(yob, gender, province_territory, income, cps19_votechoice) %>% 
  rename(vote_for = cps19_votechoice) %>% 
  filter(!is.na(income), income<10^7, income> 0, !is.na(vote_for)) %>% 
  #mutate(vote_liberal = ifelse(vote_for=="Liberal Party", 1, 0)) %>% 
  #mutate(yob = as.numeric(yob)) %>% 
  filter(vote_for != "Don't know/ Prefer not to answer") %>% 
  rename(hh_income = income) %>% 
  mutate(gender = ifelse(gender == "A woman", "Woman", ifelse(gender == "A man", "Man", as.character(gender)))) %>% 
  filter(province_territory!="Nunavut", province_territory!="Northwest Territories")

write_csv(df, "data/ces2019.csv")

# source of census: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/inc-rev/Table.cfm?Lang=Eng&T=101&S=99&O=A#details-panel3

census <- read_csv("assignments/code/census_income.CSV")

census <- census %>% 
  janitor::clean_names() %>% 
  select(geographic_name, median_household_total_income_2015_constant_dollars_2015) %>% 
  rename(province_territory = geographic_name, median_hh_income = median_household_total_income_2015_constant_dollars_2015)

write_csv(census, file = "data/census_income.csv")

df <- df %>% 
  left_join(census)


