rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)


dat <- 
  mrs_rootdist_ml %>% 
  group_by(year) %>% 
  mutate(dap_min = min(dap)) %>% 
  filter(dap == dap_min) %>% 
  left_join(mrs_plotkey) %>% 
  select(year, depth, rot_trt, block, roots_kgha) %>% 
  distinct() %>% 
  group_by(year, rot_trt, block) %>% 
  summarise(roots_kgha = sum(roots_kgha, na.rm = T)) %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"))


d19 <- 
  dat %>% 
  filter(year == 2019) %>% 
  pivot_wider(names_from = rot_trt, values_from = roots_kgha)
  
d20 <- 
  dat %>% 
  filter(year == 2020) %>% 
  pivot_wider(names_from = rot_trt, values_from = roots_kgha)

d19 %>% 
  bind_rows(d20) %>% 
  mutate_if(is.numeric, round, 0) %>% 
  write_csv("01_rootdist-ml/dat_table-s1-bkgd-values.csv")
