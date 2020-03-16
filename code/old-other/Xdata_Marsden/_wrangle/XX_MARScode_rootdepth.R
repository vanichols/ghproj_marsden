#########################
##
## Date of creation: May 28 2018
## Date last modified: May 28 2018
##
## Author: Gina
## Purpose: Process rooting depth data
##          
## Inputs: from '2018_fieldseason' folder, _data_raw folder, 
##   "dat_mars-soil_rootdepth.xlxs"
##
## Outputs: 
##
## NOTE: 
##
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files


##### Set working directory to wherever this file is kept #####
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

##=========================================##
##   Read in data
##=========================================##

root <- read_xlsx("../2018_field-season/_data_raw/dat_mars-rootdepth.xlsx", skip = 5, na = "NA") %>%
  mutate(rootdepth_cm = rootdepth_in * 2.54 ) %>%
  
  # Average sub-plot measurements to get a value for each block
  group_by(date, block, trt, stage) %>%
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T))

anns <- root %>% 
  group_by(date, stage) %>%
  summarise(maxval = max(rootdepth_cm, na.rm = T) + 2)

##===================================================================##
##   Look at it
##===================================================================##

ggplot(root, aes(date, rootdepth_cm, color = trt)) + 
  geom_point(size = 4) + 
  scale_y_reverse() + 
  facet_grid(.~block) + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  theme_bw()

ggplot(data=root, aes(date, rootdepth_cm)) + 
  #stat_summary(fun.y="mean", geom="line", size = 2, aes(color = trt)) +
  stat_summary(fun.data="mean_se", geom="crossbar",  aes(color = trt)) +
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  #guides(color = F) + 
  theme_bw() +
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) +

  labs(x = "", y = "In Row Root Depth [cm]", title = "Marsden\nCorn Root Depth", color = "Treatment") +

  geom_text(data = anns, aes(x=date, maxval, label = stage), fontface = "italic", hjust = 0) +
  scale_y_reverse()
  
ggsave("../_figs/fig_MARSpoint_rootdepth.png", width = 4, height = 5, units = "in")

