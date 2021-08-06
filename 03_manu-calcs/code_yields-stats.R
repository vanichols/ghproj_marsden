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
