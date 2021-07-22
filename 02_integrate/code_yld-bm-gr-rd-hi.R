#--combine yields, growth rates, harvest indices, root depth, etc.
# created 7/21/2021


library(maRsden)
library(tidyverse)
library(grafify)
library(janitor)



# data --------------------------------------------------------------------

#--bm over time, growth over time
gr <- read_csv("01_growth-analysis/dat_growth-analysis.csv")

#--harvest index
hi <- read_csv("01_growth-analysis/dat_harvest-indices.csv")

#--grain weights
yc <- mrs_krnl500 

#--yields
ylds <-  
  mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  filter(harv_crop != "C3") %>% 
  filter(year %in% c(2013, 2014, 2018, 2019, 2020)) 

#--maximum root depth
rd <- read_csv("01_rootdepth/dat_nls-parameters-eu.csv") %>% 
  pivot_wider(names_from = rot_trt, values_from = value) %>% 
  clean_names() 
