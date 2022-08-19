# Gina
# analyze nutrients
# created: 8/19/2022


rm(list=ls())
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(patchwork)
library(maRsden)
library(lme4)
library(lmerTest)
library(emmeans)


# data --------------------------------------------------------------------

#--how many nutrients did we measure? 27?!
msmt <- 
  mrs_nutrients18 %>% 
  select(msmt) %>% 
  distinct()

msmt

nuts <- c("k", "s", "zn", "ca", "mg", "na", "fe", "cu", "mn", "b") 

dat <- 
  mrs_nutrients18 %>% 
  filter(msmt %in% nuts) %>% 
  distinct() %>%
  left_join(mrs_plotkey) %>% 
  select(block, rot_trt, msmt, value) %>% 
  distinct()


# stats -------------------------------------------------------------------

m0 <- lmer(value ~ rot_trt + msmt + (1|block), data = dat)
m1 <- lmer(value ~ rot_trt * msmt + (1|block), data = dat)
anova(m0, m1)

em_m0 <- emmeans(m1, specs = c("rot_trt", "msmt")) 

em_m0 %>% 
  as_tibble() %>% 
  ggplot(aes(rot_trt, emmean, color = rot_trt)) + 
  geom_point() + 
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) + 
  facet_wrap(~msmt, scales = "free")

# fuck this