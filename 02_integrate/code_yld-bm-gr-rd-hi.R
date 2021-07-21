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

#--added root mass, seems different from what Matt got...
#--shit it's because I need the soil volume to get kg/ha
rm_depths <- 
  mrs_rootdist_ml %>% 
  group_by(year) %>% 
  mutate(min_dap = min(days_after_planting),
         max_dap = max(days_after_planting),
         minmax_x = ifelse(days_after_planting == min_dap, "min", 
                           ifelse(days_after_planting == max_dap, "max", NA))) %>% 
  filter(!is.na(minmax_x)) 

rm <- 
  rm_depths %>% 
  group_by(year, date, days_after_planting, plot_id) %>% 
  summarise(root_weights_g = sum(root_weights_g, na.rm= T))
  
  
  left_join(mrs_plotkey) %>% 
  group_by(year, block, rot_trt, minmax_x, depth) %>% 
  summarise(root_weights_g = mean(root_weights_g, na.rm = T)) %>% #--he took 4 per plot
  select(year, block, rot_trt, depth, minmax_x, root_weights_g) %>% 
  pivot_wider(names_from = minmax_x, values_from = root_weights_g) %>% 
  clean_names() %>% 
  group_by(year, block, rot_trt) %>% 
  summarise(max = sum(max, na.rm = T),
            min = sum(min, na.rm = T)) %>% 
  group_by(year, rot_trt) %>% 
  summarise(max = mean(max),
            min = mean(min)) %>% 
  mutate(roots_added_g = max - min)
  

rm %>% 
  ggplot(aes(rot_trt, roots_added_g)) + 
  geom_point() + 
  facet_grid(year ~ .)
