# Gina
# created 7/21/2021

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)

# mrs_rootdist_mlsum %>% 
#   left_join(mrs_plotkey) %>% 
#   select(date, dap, block, plot, rot_trt, harv_crop, roots_kgha) %>% 
#   arrange(date, block, rot_trt) %>% 
#   write_csv("01_rootdist-ml/dat_matt-compare.csv")


rm <- 
  mrs_rootdist_mlsum %>% 
  group_by(year) %>% 
  mutate(min_dap = min(dap),
         max_dap = max(dap),
         minmax_x = ifelse(dap == min_dap, "min", 
                           ifelse(dap == max_dap, "max", NA))) %>% 
  filter(!is.na(minmax_x)) %>% 
  left_join(mrs_plotkey)

rm %>% 
  select(year, block, rot_trt, minmax_x, roots_kgha) %>% 
  pivot_wider(names_from = minmax_x, values_from = roots_kgha) #%>% 
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
