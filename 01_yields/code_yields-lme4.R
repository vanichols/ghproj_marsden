# Gina
# get model estimates for yields
# 8/6/2021


rm(list=ls())
library(tidyverse)
library(lubridate)
library(patchwork)
library(maRsden)
library(lme4)
library(lmerTest)
library(emmeans)


# 2013-2020 ---------------------------------------------------------------

ydat13 <- 
  mrs_cornylds %>% 
  filter(harv_crop != "C3",
         between(year, 2013, 2020)) %>% 
  mutate(year = paste0("Y", year)) %>% 
  distinct()


#--year as fixed effect
m0 <- lmer(yield_Mgha ~ rot_trt*year + (1|year:block), data = ydat13)
m0b <- lmer(yield_Mgha ~ rot_trt*year + (1|block), data = ydat13)
anova(m0, m0b)

em_m0 <- emmeans(m0, specs = c("rot_trt", "year")) 

#--get percents
em_m0 %>% 
  broom::tidy() %>% 
  select(rot_trt, year, estimate) %>% 
  pivot_wider(names_from = rot_trt, values_from = estimate) %>% 
  janitor::clean_names() %>% 
  mutate(pct = (x4y/x2y)*100)

11.9-9.89

em_m0 %>% 
  broom::tidy() %>% 
  mutate(respvar = "corn yields fixed year") %>% 
  write_csv("01_yields/dat_ylds13-lmer-est-year-as-fixed.csv")

contrast(em_m0, method = "pairwise") %>% 
  broom::tidy() %>% 
  separate(contrast, into = c("x1", "x2"), sep = " - ") %>% 
  separate(x1, into = c("rot1", "y1"), sep = " ") %>% 
  separate(x2, into = c("rot2", "y2"), sep = " ") %>% 
  filter(y1 == y2) %>% 
  mutate(respvar = "corn yields year as fixed") %>% 
  write_csv("01_yields/dat_ylds13-lmer-sig-year-as-fixed.csv")


#--year as random
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
