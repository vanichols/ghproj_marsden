# Gina
# get model estimates for yields
# 8/6/2021


rm(list=ls())
library(tidyverse)
library(lubridate)
library(patchwork)
library(maRsden)
library(lme4)
library(emmeans)


# 2013-2020 ---------------------------------------------------------------

ydat13 <- 
  mrs_cornylds %>% 
  filter(harv_crop != "C3",
         between(year, 2013, 2020)) %>% 
  mutate(year = paste0("Y", year))


m1 <- lmer(yield_Mgha ~ rot_trt + (1|year) + (1|year:block), data = ydat13)
m2 <- lmer(yield_Mgha ~ rot_trt + (1|year) + (1|block), data = ydat13)

anova(m1, m2)

emmeans(m1, specs = "rot_trt") %>% 
  broom::tidy() %>% 
  mutate(respvar = "corn yields") %>% 
  write_csv("01_yields/dat_ylds13-lmer-est.csv")


contrast(emmeans(m1, ~rot_trt), "pairwise") %>% 
  broom::tidy() %>% 
  mutate(respvar = "corn yields") %>% 
  write_csv("01_yields/dat_ylds13-lmer-sig.csv")

# 2004-2020 ---------------------------------------------------------------


ydat <- 
  mrs_cornylds %>% 
  filter(harv_crop != "C3",
         between(year, 2004, 2020)) %>% 
  mutate(year = paste0("Y", year))


m3 <- lmer(yield_Mgha ~ rot_trt + (1|year) + (1|year:block), data = ydat)
m4 <- lmer(yield_Mgha ~ rot_trt + (1|year) + (1|block), data = ydat)

anova(m3, m4)

emmeans(m3, specs = "rot_trt") %>% 
  broom::tidy() %>% 
  mutate(respvar = "corn yields") %>% 
  write_csv("01_yields/dat_ylds04-lmer-est.csv")


contrast(emmeans(m3, ~rot_trt), "pairwise") %>% 
  broom::tidy() %>% 
  mutate(respvar = "corn yields") %>% 
  write_csv("01_yields/dat_ylds04-lmer-sig.csv")
