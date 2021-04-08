library(tidyverse)
library(here)
library(lme4)
library(brms)
library(tidybayes)

# pma
d <- read_csv(here("data/pma.csv"))

d <- d %>% filter(age>14, age<50)

age_groups <- seq(15, 45, by = 5)
d$age_group <- as.numeric(as.character(cut(d$age, 
                                           breaks= c(age_groups, Inf), 
                                           labels = age_groups, 
                                           right = FALSE)))

d <- d %>% mutate(age_group = factor(age_group)) %>% 
  mutate(age_group = fct_relevel(age_group, "35", after = 0))

d <- d %>% mutate(log_hdi_c = (log(hdi)-mean(log(hdi)))/sd(log(hdi)))

# make bigger regions

d <- d %>% mutate(region_2 = case_when(region == "north"|region== "karamoja"|region == "west nile" ~ "northern",
                                       region == "eastern" ~ "eastern",
                                       region == "central 1"|region == "central 2"|region == "east central"|region == "kampala" ~ "central",
                                       region == "western"|region == "south west" ~ "western"))

# census
dc <- haven::read_dta("~/Desktop/ipumsi_00025.dta")

dc <- dc %>% 
  select(regnug, perwt, age, marst, edattain) %>% 
  mutate(regnug = as_factor(regnug),
         marst = as_factor(marst),
         edattain = as_factor(edattain))

dc <- dc %>% filter(age>14, age<50)


dc$age_group <- as.numeric(as.character(cut(dc$age, 
                                           breaks= c(age_groups, Inf), 
                                           labels = age_groups, 
                                           right = FALSE)))

dc <- dc %>% mutate(age_group = factor(age_group)) %>% 
  mutate(age_group = fct_relevel(age_group, "35", after = 0))


# recode educ and marital to be the same

table(d$educattgen)
table(dc$edattain)


d <- d %>% mutate(educ = case_when(educattgen=="never attended"~"less than primary",
                                   educattgen=="primary/middle school" ~"primary",
                                   educattgen=="secondary/post-primary"|educattgen=="tertiary/post-secondary" ~ "more than primary",
                                   TRUE ~ "NA")) %>% 
  filter(educ != "NA", marstat!="no response or missing")

dc <- dc %>% mutate(educ = case_when(edattain=="less than primary completed"~"less than primary",
                                     edattain=="primary completed"~ "primary",
                                     edattain=="secondary completed"|edattain=="university completed" ~ "more than primary",
                                     TRUE ~ "NA")) %>% 
  filter(educ !="NA")


table(d$marstat)
table(dc$marst)

d <- d %>% mutate(marital = case_when(marstat == "never married" ~ "single/never married",
                                      marstat == "currently living with partner"| marstat == "currently married" ~ "married/in union",
                                      marstat== "divorced or separated" ~ "divorced/separated",
                                      marstat== "widow or widower"~ "widowed",
                                      TRUE ~ "NA"))

dc <- dc %>% mutate(marital = case_when(marst == "single/never married" ~ "single/never married",
                                        marst == "married/in union" ~ "married/in union",
                                        marst== "separated/divorced/spouse absent" ~ "divorced/separated",
                                        marst== "widowed"~ "widowed",
                                      TRUE ~ "NA")) %>% 
  filter(marital != "NA")


table(d$marital)
table(dc$marital)

mod2 <- glmer(abortion ~ (1|region_2)+marital+educ+ (1|age_group), data = d, family = "binomial")
summary(mod2)

## Need to grab estimates by region, marital, educ and age group

regions <- unique(d$region_2)
marital_groups <- unique(d$marital)
educ_groups  <- unique(d$educ)

pred_df <- tibble(region_2 = NA, marital = NA, educ = NA, age_group = NA) %>% 
  tidyr::complete(region_2 = regions, 
                  marital = marital_groups,
                  educ = educ_groups, 
                  age_group = age_groups) %>% 
  mutate_all(.funs = funs(as_factor(.))) %>% 
  drop_na() %>% 
  mutate(age_group = fct_relevel(age_group, "35", after = 0)) %>% 
  filter(age_group != "45")

pred_df <- bind_cols(pred_df, tibble(prob = predict(mod2, newdata = pred_df, type = "response")))
pred_df

# ideally we want standard errors here...


# poststratify

census_counts <- dc %>% 
  group_by(regnug, marital, educ, age_group) %>% 
  summarize(n = sum(perwt)) %>% 
  filter(age_group!="45") %>% 
  rename(region_2 = regnug)

pred_df <- pred_df %>% 
  left_join(census_counts) %>% 
  mutate(n_abo = prob*n) %>% 
  replace_na(replace = list(n = 0,n_abo = 0))

## normal post-stratification

pma_cells <- d %>% 
  group_by(region_2, educ, marital, age_group) %>% 
  summarize(#n_sample = sum(fqweight), n_abo_sample = sum(fqweight*abortion),
            n_sample = n(), n_abo_sample = sum(abortion),
            prop_abo = n_abo_sample/n_sample)

pma_cells <- pma_cells %>% 
  left_join(census_counts) %>% 
  mutate(n_abo = prop_abo*n) %>% 
  replace_na(replace = list(n = 0,n_abo = 0)) %>% 
  ungroup()

# national

pred_df %>% 
  summarize(mrp = sum(n_abo)/sum(n)) %>% 
  bind_cols(pma_cells %>% summarize(poststrat = sum(n_abo)/sum(n))) %>% 
  bind_cols(pma_cells %>% summarize(raw = sum(n_abo_sample)/sum(n_sample))) %>% 
  pivot_longer(mrp:raw, "estimate", values_to = "prop_abo")

# by age 

pred_df %>% 
  group_by(age_group) %>% 
  summarize(mrp = sum(n_abo)/sum(n)) %>% 
  left_join(pma_cells %>% group_by(age_group) %>%summarize(poststrat = sum(n_abo)/sum(n))) %>% 
  left_join(pma_cells %>% group_by(age_group) %>%summarize(raw = sum(n_abo_sample)/sum(n_sample))) 

# by educ 

pred_df %>% 
  group_by(educ) %>% 
  summarize(mrp = sum(n_abo)/sum(n)) %>% 
  left_join(pma_cells %>% group_by(educ) %>%summarize(poststrat = sum(n_abo)/sum(n))) %>% 
  left_join(pma_cells %>% group_by(educ) %>%summarize(raw = sum(n_abo_sample)/sum(n_sample))) 

# by marital

pred_df %>% 
  group_by(marital) %>% 
  summarize(mrp = sum(n_abo)/sum(n)) %>% 
  left_join(pma_cells %>% group_by(marital) %>%summarize(poststrat = sum(n_abo)/sum(n))) %>% 
  left_join(pma_cells %>% group_by(marital) %>%summarize(raw = sum(n_abo_sample)/sum(n_sample))) 


# BRMS --------------------------------------------------------------------

mod3 <- brm(abortion ~ (1|region_2)+educ+ age_group, data = d, family = "bernoulli")

## try with brms

pred2 <- mod3 %>% 
  predicted_draws(pred_df  %>% 
                    select(region_2:age_group) %>% 
                    mutate(age_group = factor(age_group, levels = age_groups[-length(age_groups)])))

pred2 <- pred2 %>% 
  left_join(census_counts) %>% 
  mutate(n_abo = n*.prediction) 


## national
pred2 %>% 
  group_by(.draw) %>% 
  summarize(prop_abo = sum(n_abo, na.rm = TRUE)/sum(n, na.rm = TRUE)) %>% 
  ungroup() %>% 
  summarize(point = median(prop_abo), 
            lower = quantile(prop_abo, 0.1), 
            upper = quantile(prop_abo, 0.9)) %>% 
  mutate(type = "mrp") %>% 
  bind_rows(pma_cells %>% summarize(poststrat = sum(n_abo)/sum(n)) %>% 
  bind_cols(pma_cells %>% summarize(raw = sum(n_abo_sample)/sum(n_sample))) %>% 
  pivot_longer(poststrat:raw, names_to = "type", values_to = "point")) %>% 
  select(type, point:upper)

# by age

prop_group <- pred2 %>% 
  group_by(.draw, age_group) %>% 
  summarize(prop_abo = sum(n_abo, na.rm = TRUE)/sum(n, na.rm = TRUE)) %>% 
  group_by(age_group) %>% 
  summarize(point = median(prop_abo), 
            lower = point - 0.5*sd(prop_abo), 
            upper = point + 0.5*sd(prop_abo)) %>% 
  mutate(type = "mrp") %>% 
  bind_rows(pma_cells %>% group_by(age_group) %>%summarize(poststrat = sum(n_abo)/sum(n)) %>% 
  left_join(pma_cells %>% group_by(age_group) %>%summarize(raw = sum(n_abo_sample)/sum(n_sample))) %>% 
  pivot_longer(-age_group, names_to = "type", values_to = "point")) %>% 
  arrange(age_group)


prop_group %>% 
  ggplot(aes(age_group, point, color = type)) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = NA, alpha = 0.5) + 
  theme_bw(base_size = 14) +
  labs(title = "Abortion prevalence by age group", x = "age", y = "proportion of women reporting abortions")
ggsave("slides/12_misc/pred_age.pdf", width = 6, height = 4)


# by educ

prop_group <- pred2 %>% 
  group_by(.draw, educ) %>% 
  summarize(prop_abo = sum(n_abo, na.rm = TRUE)/sum(n, na.rm = TRUE)) %>% 
  group_by(educ) %>% 
  summarize(point = median(prop_abo), 
            lower = point - 0.5*sd(prop_abo), 
            upper = point + 0.5*sd(prop_abo)) %>% 
  mutate(type = "mrp") %>% 
  bind_rows(pma_cells %>% group_by(educ) %>%summarize(poststrat = sum(n_abo)/sum(n)) %>% 
              left_join(pma_cells %>% group_by(educ) %>%summarize(raw = sum(n_abo_sample)/sum(n_sample))) %>% 
              pivot_longer(-educ, names_to = "type", values_to = "point")) %>% 
  arrange(educ)


prop_group %>% 
  mutate(educ = fct_relevel(educ, "more than primary", after = 2)) %>% 
  ggplot(aes(educ, point, color = type)) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = NA, alpha = 0.5) + 
  theme_bw(base_size = 14) +
  labs(title = "Abortion prevalence by education", x = "education level", y = "proportion of women reporting abortions")
ggsave("slides/12_misc/pred_educ.pdf", width = 6, height = 4)

# by marital

prop_group <- pred2 %>% 
  group_by(.draw, marital) %>% 
  summarize(prop_abo = sum(n_abo, na.rm = TRUE)/sum(n, na.rm = TRUE)) %>% 
  group_by(marital) %>% 
  summarize(point = median(prop_abo), 
            lower = point - 0.5*sd(prop_abo), 
            upper = point + 0.5*sd(prop_abo)) %>% 
  mutate(type = "mrp") %>% 
  bind_rows(pma_cells %>% group_by(marital) %>%summarize(poststrat = sum(n_abo)/sum(n)) %>% 
              left_join(pma_cells %>% group_by(marital) %>%summarize(raw = sum(n_abo_sample)/sum(n_sample))) %>% 
              pivot_longer(-marital, names_to = "type", values_to = "point")) %>% 
  arrange(marital)


prop_group %>% 
  mutate(marital = factor(marital, c("single/never married", "married/in union", "divorced/separated", "widowed"))) %>% 
  ggplot(aes(marital, point, color = type)) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = NA, alpha = 0.5) + 
  theme_bw(base_size = 14) +
  labs(title = "Abortion prevalence by marital status", x = "", y = "proportion of women reporting abortions")+
  coord_flip()
ggsave("slides/12_misc/pred_marital.pdf", width = 6, height = 4)

# region

prop_group <- pred2 %>% 
  group_by(.draw, region_2) %>% 
  summarize(prop_abo = sum(n_abo, na.rm = TRUE)/sum(n, na.rm = TRUE)) %>% 
  group_by(region_2) %>% 
  summarize(point = median(prop_abo), 
            lower = point - 0.5*sd(prop_abo), 
            upper = point + 0.5*sd(prop_abo)) %>% 
  mutate(type = "mrp") %>% 
  bind_rows(pma_cells %>% group_by(region_2) %>%summarize(poststrat = sum(n_abo)/sum(n)) %>% 
              left_join(pma_cells %>% group_by(region_2) %>%summarize(raw = sum(n_abo_sample)/sum(n_sample))) %>% 
              pivot_longer(-region_2, names_to = "type", values_to = "point")) %>% 
  arrange(region_2)


prop_group %>% 
  #mutate(marital = factor(marital, c("single/never married", "married/in union", "divorced/separated", "widowed"))) %>% 
  ggplot(aes(region_2, point, color = type)) + geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = NA, alpha = 0.5) + 
  theme_bw(base_size = 14) +
  labs(title = "Abortion prevalence by region", x = "", y = "proportion of women reporting abortions")
ggsave("slides/12_misc/pred_region.pdf", width = 6, height = 4)

# proportions of census/pma

d %>% 
  group_by(age_group) %>% 
  tally() %>% 
  mutate(pma = n/sum(n)) %>% 
  left_join(census_counts %>% 
              group_by(age_group) %>% 
              summarize(n = sum(n)) %>% 
              mutate(census = n/sum(n)) %>% 
              select(-n)) %>% 
  mutate(age_group = as.character(age_group)) %>% 
  arrange(age_group) %>% 
  pivot_longer(pma:census) %>% 
  ggplot(aes(age_group, value, fill = name)) + geom_bar(stat = "identity", position = 'dodge')+
  theme_bw(base_size = 14) + 
  labs( x = "age", y = "proportion")+
  scale_fill_brewer(palette = "Set1", name = "source")
ggsave("slides/12_misc/prop_sample_age.pdf", width = 6, height = 4)

d %>% 
  group_by(educ) %>% 
  tally() %>% 
  mutate(pma = n/sum(n)) %>% 
  left_join(census_counts %>% 
              group_by(educ) %>% 
              summarize(n = sum(n)) %>% 
              mutate(census = n/sum(n)) %>% 
              select(-n)) %>% 
  mutate(educ = fct_relevel(educ, "more than primary", after = 2)) %>% 
  pivot_longer(pma:census) %>% 
  ggplot(aes(educ, value, fill = name)) + geom_bar(stat = "identity", position = 'dodge')+
  theme_bw(base_size = 14) + 
  labs( x = "education", y = "proportion")+
  scale_fill_brewer(palette = "Set1", name = "source")
ggsave("slides/12_misc/prop_sample_educ.pdf", width = 6, height = 4)

d %>% 
  group_by(region_2) %>% 
  tally() %>% 
  mutate(pma = n/sum(n)) %>% 
  left_join(census_counts %>% 
              group_by(region_2) %>% 
              summarize(n = sum(n)) %>% 
              mutate(census = n/sum(n)) %>% 
              select(-n)) %>% 
  #mutate(educ = fct_relevel(educ, "more than primary", after = 2)) %>% 
  pivot_longer(pma:census) %>% 
  ggplot(aes(region_2, value, fill = name)) + geom_bar(stat = "identity", position = 'dodge')+
  theme_bw(base_size = 14) + 
  labs( x = "region", y = "proportion")+
  scale_fill_brewer(palette = "Set1", name = "source")
ggsave("slides/12_misc/prop_sample_region.pdf", width = 6, height = 4)


d %>% 
  group_by(marital) %>% 
  tally() %>% 
  mutate(pma = n/sum(n)) %>% 
  left_join(census_counts %>% 
              group_by(marital) %>% 
              summarize(n = sum(n)) %>% 
              mutate(census = n/sum(n)) %>% 
              select(-n)) %>% 
  #mutate(educ = fct_relevel(educ, "more than primary", after = 2)) %>% 
  pivot_longer(pma:census) %>% 
  ggplot(aes(marital, value, fill = name)) + geom_bar(stat = "identity", position = 'dodge')+
  theme_bw(base_size = 14) + 
  labs( x = "marital status", y = "proportion")+
  scale_fill_brewer(palette = "Set1", name = "source")+coord_flip()
ggsave("slides/12_misc/prop_sample_marital.pdf", width = 6, height = 4)

