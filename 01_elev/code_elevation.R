# Gina
# does average elevation of rot treatments vary by year?
# 10/20/2020
# notes: 


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)
library(GinaBooty)


mrs_plotkey
mrs_elevation

dat <- 
  mrs_plotkey %>% 
  left_join(mrs_elevation %>% 
              select(plot, mean_elev_m, median))

datm <- 
  dat %>% 
  filter(harv_crop %in% c("C2", "C3", "C4")) %>% 
  group_by(year, rot_trt, harv_crop) %>% 
  summarise(mean_elev = mean(mean_elev_m)) %>% 
  ungroup()
  
# viz ---------------------------------------------------------------------
datm %>% 
  ggplot(aes(year, mean_elev, color = rot_trt)) + 
  geom_point() + 
  geom_line()


# difference in elev ------------------------------------------------------

datdev <- 
  datm %>%
  select(-rot_trt) %>% 
  pivot_wider(names_from = harv_crop, values_from = mean_elev) %>% 
  mutate(devC3 = C3 - C2,
         devC4 = C4 - C2) %>% 
  select(-(C2:C4)) %>% 
  pivot_longer(devC3:devC4) %>% 
  rename(elev_dev = value)

datdev %>% 
  ggplot(aes(year, elev_dev, fill = name)) + 
  geom_col(position = position_dodge2())

datm %>% 
  ggplot(aes(rot_trt, mean_elev, color = rot_trt)) +
  geom_point(size = 4) + 
  facet_wrap(~year)

# compare elev diffs to yield diffs ---------------------------------------

ylddev <- 
  mrs_cornylds %>% 
  left_join(mrs_plotkey) %>%
  group_by(year, rot_trt) %>% 
  summarise(yield = mean(yield_Mgha, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = yield) %>% 
  mutate(devC3 = `3y` - `4y`,
         devC4 = `4y` - `2y`) %>% 
  select(year, devC3:devC4) %>% 
  pivot_longer(devC3:devC4) %>% 
  rename(yield_dev = value)

ylddev %>% 
  left_join(datdev) %>% 
  ggplot(aes(elev_dev, yield_dev, color = name)) + 
  geom_point(size = 3) + 
  geom_label(aes(label = year)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Biggest corn yield bumps when average plot elevations are lower than control")

#--was the biggest yield bump for C4 in 2018?
mrs_cornylds %>% 
  left_join(mrs_plotkey) %>%
  ggplot(aes(rot_trt, yield_Mgha)) + 
  stat_summary() + 
  facet_grid(.~year)
  
#--I need to look at what the water table data said
  