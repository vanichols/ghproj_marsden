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
         minmax_x = ifelse(dap == min_dap, "beg", 
                           ifelse(dap == max_dap, "end", NA))) %>% 
  filter(!is.na(minmax_x)) %>% 
  left_join(mrs_plotkey)

rm_add <- 
  rm %>% 
  select(year, block, rot_trt, minmax_x, roots_kgha) %>% 
  pivot_wider(names_from = minmax_x, values_from = roots_kgha) %>% 
  mutate(roots_added = end - beg)

rm_add %>% write_csv("01_rootdist-ml/dat_roots-added.csv")

#--lot of error
rm_add %>% 
  ggplot(aes(rot_trt, roots_added)) + 
  stat_summary(geom = "bar") + 
  stat_summary() + 
  facet_grid(.~year)
  
#--in 2020 the 4 yr had less root stuff at the end of the season
rm %>% 
  select(year, block, rot_trt, minmax_x, roots_kgha) %>%
  group_by(year, rot_trt, minmax_x) %>% 
  summarise(roots_kgha = mean(roots_kgha, na.rm = T)) %>%
  ggplot(aes(rot_trt, roots_kgha, fill = minmax_x)) + 
  geom_col(position = position_dodge()) + 
  facet_grid(.~year)

