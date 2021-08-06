# calc stuff for manu
# started 8/5/2021
# updated:

library(maRsden)
library(tidyverse)
library(janitor)
library(saapsim)
library(lme4)

#2013-2020
yd <- 
  mrs_cornylds %>% 
  filter(between(year, 2013, 2020),
         rot_trt != "3y") %>% 
  distinct()


m1 <- lmer(yield_Mgha ~ rot_trt + (1|block:year), data = yd)

summary(m1)

emmeans::emmeans(m1, specs = "rot_trt")

yd$yield_Mgha %>% 
  summary()


# by specific year -----------------------------------------------------------------
sy <- c(2013, 2014, 2018, 2019, 2020)

ydy <- 
  mrs_cornylds %>% 
  filter(year %in% sy,
         rot_trt != "3y") %>% 
  distinct() %>% 
  mutate(year = paste0("Y", year))


m2 <- lmer(yield_Mgha ~ rot_trt*year + (1|block), data = ydy)
m2a <- lm(yield_Mgha ~ rot_trt*year, data = ydy)
anova(m2a)

emmeans::emmeans(m2a, specs = ~rot_trt:year) %>% 
  broom::tidy() %>% 
  select(rot_trt, year, estimate) %>% 
  pivot_wider(names_from = rot_trt, values_from = estimate) %>% 
  clean_names() %>% 
  mutate(diff = x4y - x2y) %>% 
  arrange(-diff)

emmeans::emmeans(m2a, specs = pairwise~rot_trt|year) 
  

# harvest index -----------------------------------------------------------


hi <- read_csv("01_growth-analysis/dat_harvest-indices.csv") %>% 
  left_join(mrs_plotkey) %>% 
  distinct() %>% 
  select(year, rot_trt, block, hi) %>% 
  rename("value" = hi) %>% 
  mutate(name = "Harvest index",
         year = paste0("Y", year))

hi

m3 <- lmer(value ~ rot_trt*year + (1|block), data = hi)
m3a <- lm(value ~ rot_trt*year, data = hi)
anova(m3, m3a)

anova(m3)

emmeans::emmeans(m3a, specs = ~rot_trt:year) %>% 
  broom::tidy() %>% 
  select(rot_trt, year, estimate) %>% 
  pivot_wider(names_from = rot_trt, values_from = estimate) %>% 
  clean_names() %>% 
  mutate(diff = x4y - x2y) %>% 
  arrange(-diff)

emmeans::emmeans(m3a, specs = pairwise~rot_trt|year) 


# harvest index -----------------------------------------------------------

#--grain weights
yc <- mrs_krnl500 %>% 
  left_join(mrs_plotkey) %>% 
  select(year, rot_trt, block, krnl500_g) %>% 
  distinct() %>% 
  rename("value" = krnl500_g) %>% 
  mutate(name = "Weight of 500 kernals",
         year = paste0("Y", year))

yc

m4 <- lmer(value ~ rot_trt*year + (1|block), data = yc)
m4a <- lm(value ~ rot_trt*year, data = yc)
anova(m4, m4a)

anova(m4)

emmeans::emmeans(m4a, specs = ~rot_trt:year) %>% 
  broom::tidy() %>% 
  select(rot_trt, year, estimate) %>% 
  pivot_wider(names_from = rot_trt, values_from = estimate) %>% 
  clean_names() %>% 
  mutate(diff = x4y - x2y) %>% 
  arrange(-diff)

emmeans::emmeans(m4a, specs = pairwise~rot_trt|year) 



