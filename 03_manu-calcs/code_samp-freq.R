# calc stuff for manu
# started 7/20/2021
# updated:

library(maRsden)
library(tidyverse)
library(janitor)
library(saapsim)


#--get planting date
pd <- mrs_rootdepth %>% 
  ungroup() %>% 
  filter(rootdepth_cm == 0) %>% 
  select(year, date) %>% 
  distinct() %>% 
  rename("planting_date" = date)


# biomass freq ------------------------------------------------------------

mrs_cornbio %>%
  left_join(mrs_plotkey) %>% 
  ungroup() %>% 
  group_by(year, block, rot_trt) %>% 
  tally() %>% 
  group_by(year) %>% 
  summarise(nav = mean(n))
  


# root depth freq ------------------------------------------------------------

#--number of times I sampled
mrs_rootdepth %>%
  ungroup() %>% 
  filter(rootdepth_cm != 0) %>% 
  select(year, date) %>% 
  distinct() %>% 
  group_by(year) %>% 
  tally() 


#--starting sampling (dap)
mrs_rootdepth %>% 
  ungroup() %>% 
  left_join(pd) %>% 
  mutate(dap = date - planting_date,
         dap2 = as.numeric(dap)) %>% 
  select(year, date, dap2) %>% 
  distinct() %>% 
  filter(dap2 != 0) %>% 
  group_by(year) %>% 
  filter(dap2 == min(dap2))

#--last sample
mrs_rootdepth %>% 
  ungroup() %>% 
  left_join(pd) %>% 
  mutate(dap = date - planting_date,
         dap2 = as.numeric(dap)) %>% 
  select(year, date, dap2) %>% 
  distinct() %>% 
  filter(dap2 != 0) %>% 
  group_by(year) %>% 
  filter(dap2 == max(dap2))

  


# penetrom ----------------------------------------------------------------


mrs_penetrom %>% 
  ungroup() %>% 
  left_join(pd) %>% 
  select(year, date, planting_date) %>% 
  mutate(dap = as.numeric(date - planting_date)) %>% 
  select(year, dap) %>% 
  distinct()


mrs_penetrom %>% 
  ungroup() %>% 
  select(depth_cm) %>% 
  summary()


45/2.54
