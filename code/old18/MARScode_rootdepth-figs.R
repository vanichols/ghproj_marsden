#########################
##
## Date of creation: Feb 5 2019
## Date last modified: 
##                      Feb 5 2019 (updating)
##
## Author: Gina
## Purpose: Make root depth figs
##          
## Inputs: td_rootdepth
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
library(patchwork)

##### Set working directory to wherever this file is kept #####
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

##=========================================##
# Read data ---------------------------------------------------------------
##=========================================##

roo <- read_csv("../_data/tidy/td-rootdepth.csv")

dat <- 
  roo %>%
  # Average sub-plot measurements to get a value for each block
  group_by(date, block, trt, stage) %>%
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T))

anns <- dat %>% 
  group_by(date, stage) %>%
  summarise(maxval = max(rootdepth_cm, na.rm = T) + 2)

##===================================================================##
##   Look at it
##===================================================================##

blockfig<- 
  ggplot(roo, aes(date, rootdepth_cm, fill = trt)) + 
  geom_jitter(size = 4, width = 0.5, pch = 21, alpha = 0.5) + 
  scale_y_reverse() + 
  facet_wrap(.~block, ncol = 2) + 
  scale_fill_manual(values = c("darkgoldenrod1", "cyan3")) +
  labs(x = "",
       y = "Root Depth [cm]\n",
       title = "Marsden\n Corn root depth by block",
       fill = "Treatment") + 
  theme_bw() + 
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10)))


#ggsave("../_figs/MARSfig_rootdepth-by-block.png", width = 4, height = 5, units = "in")


sumfig <- 
  ggplot(dat, aes(date, rootdepth_cm)) + 
  #stat_summary(fun.y="mean", geom="line", size = 2, aes(color = trt)) +
  stat_summary(fun.data = "mean_se", geom = "crossbar",  aes(color = trt)) +
  
  geom_text(data = anns, 
            aes(x = date, maxval, label = stage), fontface = "italic", hjust = 0) +
  
  labs(title = "Marsden\nCorn root depth", 
       x = "", 
       y = "In Row Root Depth [cm]", 
       color = "Treatment") +

  scale_y_reverse() + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme_bw() +
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10)))
  
  
#ggsave("../_figs/MARSfig_rootdepth.png", width = 4, height = 5, units = "in")

blockfig | sumfig
ggsave("../_figs/MARSfig_rootdepth.png", width = 8, height = 5, units = "in")
