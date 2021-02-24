# Gina
# look at root and biomass data on log scale for matt
# created: 2/24/2021


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)
library(naniar)

theme_set(theme_bw())

mrs_rootdepth
mrs_cornbio

# look at it --------------------------------------------------------------

#--rot depth - what is missing?
#--ok, pretty random
mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, rootdepth_cm)) + 
  geom_miss_point() +
  facet_grid(rot_trt~year) +
  scale_y_log10()

#--log-scale
mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, rootdepth_cm)) + 
  stat_summary(aes(color = rot_trt), geom = "point") +
  stat_summary(geom = "line", aes(color = rot_trt)) +
  #geom_point(aes(color = rot_trt)) +
  scale_y_log10() +
  labs(y = "log(rootdepth_cm)") +
  facet_grid(.~year) 

ggsave("02_integrate/matt_roots.png")

#--biomass
mrs_cornbio %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, mass_gpl)) + 
  geom_miss_point() +
  facet_grid(rot_trt~year) +
  scale_y_log10()

mrs_cornbio %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, mass_gpl)) + 
  stat_summary(aes(color = rot_trt), geom = "point") +
  stat_summary(geom = "line", aes(color = rot_trt)) +
  #geom_point(aes(color = rot_trt)) +
  labs(y = "Biomass (grams/plant), log scale") +
  scale_y_log10() +
  facet_grid(.~year) 

ggsave("02_integrate/matt_cornbio.png")

# data for matt -----------------------------------------------------------

bio_matt <- 
  mrs_cornbio %>% 
  left_join(mrs_plotkey) %>% 
  select(year, date, doy, plot_id, block, plot, rot_trt, harv_crop, mass_gpl)

bio_matt %>% write_csv("02_integrate/matt_cornbio.csv")


root_matt <- 
  mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  select(year, date, doy, plot_id, block, plot, rot_trt, harv_crop, subrep_id, rootdepth_cm)

root_matt %>% write_csv("02_integrate/matt_roots.csv")

