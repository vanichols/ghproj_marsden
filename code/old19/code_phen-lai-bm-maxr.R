#########################
##
## Date of creation: June 26 2019
## Date last modified: June 26 2019
##
## Author: Gina
## Purpose: Process data
##          
## Inputs: 
##
## Outputs:  td_maxroot, td_lai-bm, td_phen
##           (figs in glimpse) lai-bm, maxroot
##
## NOTE: 
##
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data
library(fuzzyjoin) #--to do fuzzy joining of dates
library(here)
library(ggrepel) #--to make labels repel each othter

setwd(here())

key <- read_csv("_data/tidy/td_year-plot-trt-key.csv") %>%
  filter(year == 2019) %>%
  mutate(block = str_sub(plot, 1, 1))

mydir <- "_theme-2019data/_data/raw/"

# LAI autoread------------------------------------------------

lairaw <- 
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('lai', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest() %>%
  fill(date)

# biomass autoread-----------------------------------------------------------------

bmraw <- 
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('biomass', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest() %>%
  fill(date, plot) %>%
  mutate(wgtbag_g = ifelse(is.na(wgtbag_g), 0, wgtbag_g),
         wgt_g = wgtall_g - wgtbag_g) %>%
  select(-wgtall_g, -wgtbag_g) %>%
  mutate(wgt_g = ifelse(is.na(wgt_g), 0, wgt_g)) %>%
  spread(organ, value = wgt_g) %>%
  mutate(tot_g = ear + ear_cob + ear_husk + ear_kernals + 
           brnleaf + LAIgleaf + othergleaf + stemtass)

# merge 'em ---------------------------------------------------------------

#--growth analysis = ga

garaw <- 
  lairaw %>%
  left_join(bmraw) %>%
  mutate(tot_g_m2 = tot_g,
         tot_g_pl  = tot_g / totpl_no,
         LAI_m2_pl = LAI_cm2 / subsampl_no / (100^2),
         LAI_m2_m2 = LAI_cm2 / (100^2),
         date = as.Date(date)) %>%
   select(date, plot, tot_g_m2, tot_g_pl, LAI_m2_pl, LAI_m2_m2) 

garaw %>%
  write_csv("_theme-2019data/_data/tidy/td_lai-bm.csv")

#--look at it, merge w/key just for visualizing
garaw %>%
  left_join(key) %>%
  gather(tot_g_m2:LAI_m2_m2, key = msmt, value = val) %>%
  ggplot(aes(system, val)) + 
  geom_point(size = 2, color = 'red') +
  geom_text_repel(aes(label = block), hjust = 1, vjust = 1, color = "gray80") +
  stat_summary(fun.y = mean, geom = "point", size = 5) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) + 
  facet_grid(msmt~date, scales = "free") + 
  labs(title = "LAI and Biomass") + 
  theme_bw()

ggsave("_theme-2019data/_figs/glimpse/lai-bm.png")


# phenology autoread ------------------------------------------------

phenraw <- 
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('phen', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest() %>%
  fill(date, plot, totpl_no) %>%
  mutate(plht_cm = ifelse(is.na(plht_cm), plht_in * 2.54, plht_cm)) %>%
  group_by(date, plot) %>%
  summarise(stage_Vx = mean(stage_Vx),
            plht_cm = mean(plht_cm)) %>%
  ungroup() %>%
  mutate(stage = paste0("V", round(stage_Vx, 0)),
         date = as.Date(date)) %>%
  select(date, plot, stage, plht_cm)

phenraw %>%
  write_csv("_theme-2019data/_data/tidy/td_phen.csv")

phenraw %>%
  left_join(key) %>%
  ggplot(aes(system, plht_cm)) + 
  geom_point(size = 2, color = "red") +
  geom_text(aes(label = block), hjust = 1, vjust = 1, color = "gray80") +
  geom_text_repel(aes(label = stage), y = 2, color = "gray80") +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) + 
  facet_grid(~date, scales = "free") +
  labs(title = "Plant Height") + 
  theme_bw()
ggsave("_theme-2019data/_figs/glimpse/plantht.png")  
  

# root depth autoread ------------------------------------------------

mrdraw <- 
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('maxrootdepth', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot) %>%
  mutate(mrd_cm = ifelse(is.na(maxrootdepth_cm), maxrootdepth_in*2.54, maxrootdepth_cm)) %>%
  group_by(date, plot) %>%
  summarise(mrd_cm = mean(mrd_cm, na.rm = T)) %>%
  ungroup() %>%
  mutate(date = as_date(date)) %>% 
  # sampled over two days, fix it
  mutate(date2 = ifelse(date == as.Date("2019-09-16"), as.Date("2019-09-17"), as.Date(date)),
         date3 = as_date(date2)) %>% 
  select(-date, -date2) %>% 
  rename("date" = "date3")


mrdraw %>%
  write_csv("_theme-2019data/_data/tidy/td_maxroot.csv")

#--look at it, merge w/key just for visualizing
mrdraw %>%
  left_join(key) %>%
  ggplot(aes(system, mrd_cm)) + 
  geom_point(size = 2, color = "red") +
  geom_text(aes(label = block), hjust = 1, vjust = 1, color = "gray80") +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) + 
  facet_grid(.~date, scales = "free") + 
  scale_y_reverse() + 
  theme_bw()

ggsave("_theme-2019data/_figs/glimpse/maxroot.png")

