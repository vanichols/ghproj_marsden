#########################
#
# Date of creation: Dec 4 2019
# Date last modified: 
# Author: Gina Nichols (virginia.nichols@gmail.com)
#
# Purpose: Make fig of my hypothesis
#          
# Inputs: 
#
# Outputs:
#
# NOTE: See what estimates look like over years
#
#########################

# Clear env, load packages, set wd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(tidyverse)



# fake data ---------------------------------------------------------------

marsraw <- read_csv("_theme-explore-prev-data/_data/_tidy/td_corn-yields.csv") 

mars <- 
  marsraw %>% 
  filter(trt %in% c("C2", "C4")) %>% 
  arrange(year) %>% 
  mutate(yld_kgha = yld_Mgha*1000) %>% 
  group_by(year, trt) %>% 
  summarise(yld = mean(yld_kgha)) %>% 
  spread(trt, yld) %>% 
  mutate(loc = "Mars") %>% 
  select(loc, year, C2, C4)

cobs <- read_csv("_theme-explore-prev-data/_data/_tidy/td_COBS-corn-yields.csv")


# futz --------------------------------------------------------------------

m1 <- 
  mars %>% 
  mutate(yldpen = C4 - C2) %>% 
  select(loc, year, yldpen)

c1 <- 
  cobs %>% 
  mutate(yldpen = CS - CC) %>% 
  select(loc, year, yldpen)

bind_rows(m1, c1) %>% 
  mutate(yp_bu = yldpen/1000*16) %>% 
  ggplot(aes(year, yp_bu)) +
  geom_col() + 
  facet_grid(loc~.)


# mars bu/ac
marsraw %>% 
  filter(trt != "C3") %>% 
  ggplot(aes(year, yld_buac)) + 
  stat_summary(aes(color = trt)) + 
  stat_summary(geom = "line", aes(color = trt))

# mars mg/ha
marsraw %>% 
  filter(trt != "C3") %>% 
  ggplot(aes(year, yld_Mgha)) + 
  stat_summary(aes(color = trt)) + 
  stat_summary(geom = "line", aes(color = trt))

# 2 Mg/ha is 2000 kg/ha is 
2000*0.014
# 28 kg N. Could N below 1 m be 30 kg N? IT's getting an extra 60 kg N from somewhere!