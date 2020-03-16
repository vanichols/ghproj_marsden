#########################
#
# Date of creation: Feb 4 2019
# Date last modified: Feb 4 2019
#
# Author: Gina
# Purpose: Make graphs of biomass data
#
# Inputs: td-biomass.csv
#    
# Outputs: MARSfig_bio-ylds
#
# NOTES: 
#
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

#~~~~~~~~~~~~~~~~~~~
# Read in data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

bio <- read_csv("../_data/tidy/td-biomass.csv")
krnl <- read_csv("../_data/tidy/td-500krnlwt.csv")
yld <- read_csv("../_data/tidy/td-corn-yield.csv")

anns <- bio %>% 
  group_by(date, ds_stage) %>%
  summarise(maxval = max(tbm_Mgha, na.rm = T) + 2)

#~~~~~~~~~~~~~~~~~~~
# Questions ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

# Is the harvest index the same?

harfig<- bio %>%
  filter(date > "2018-09-01") %>%
  select(-date) %>%
  left_join(yld) %>%
  select(-date) %>%
  mutate(HI = yield_Mgha / tbm_Mgha) %>%

  ggplot(aes(trt, HI)) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2,  
               aes(color = trt)) +
  stat_summary(fun.y = "mean", geom = "point", size = 4,  
               aes(color = trt)) +
  
  labs(title = "Marsden\nCorn `harvest index`", 
       x = "", 
       y = "Harvest Index", 
       color = "Treatment") +
  
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme_bw() +
  guides(color = F)
  

#~~~~~~~~~~~~~~~~~~~
# Make figs ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

# Biomass by block
#
bio %>%
  mutate(doy = yday(date)) %>%
  filter(doy < 300) %>%

  ggplot(aes(date, tbm_Mgha)) + 
  geom_jitter(size = 4, width = 0.5, pch = 21, alpha = 0.5, aes(fill = trt)) + 
  geom_line(aes(group = trt)) + 
  
  facet_wrap(.~block, ncol = 2) + 
  scale_fill_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  labs(x = "",
       y = "Biomass [Mg/ha]\n",
       title = "Marsden\n Corn biomass by block",
       fill = "Treatment") + 
  theme_bw() + 
  theme(legend.position = c(1, 0), 
        legend.justification = c(1, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10)))


# Biomass averages
#
biofig <- 
bio %>%
  mutate(doy = yday(date)) %>%
  filter(doy < 300) %>%
  
  ggplot(aes(date, tbm_Mgha)) + 
  stat_summary(fun.data = "mean_se", geom = "crossbar",   
               aes(color = trt)) +
  
  geom_text(data = anns, 
            aes(x = date, maxval, label = ds_stage), fontface = "italic", hjust = 0) +
  
  labs(title = "Marsden\nCorn biomass", 
       x = "", 
       y = "Total Biomass [Mg/ha]", 
       color = "Treatment") +
  
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme_bw() +
  theme(legend.position = c(0, 1), 
        legend.justification = c(0, 1), 
        legend.background = element_rect(color = "black"),
        legend.box.margin = margin(c(10,10,10,10)))

# 500 kernal weight
krnfig <- 
krnl %>%
  ggplot(aes(trt, ds_krnl500_g)) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2,  
               aes(color = trt)) +
  stat_summary(fun.y = "mean", geom = "point", size = 4,  
               aes(color = trt)) +
  
  labs(title = "Marsden\nCorn 500 kernal weight", 
       x = "", 
       y = "500 Kernal Weight [g]", 
       color = "Treatment") +
  
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme_bw() +
  guides(color = F)
  # theme(legend.position = c(0, 1), 
  #       legend.justification = c(0, 1), 
  #       legend.background = element_rect(color = "black"),
  #       legend.box.margin = margin(c(10,10,10,10)))

# yield
yldfig <- 
yld %>%
  ggplot(aes(trt, yield_Mgha)) + 
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2,  
               aes(color = trt)) +
  stat_summary(fun.y = "mean", geom = "point", size = 4,  
               aes(color = trt)) +
  
  labs(title = "Marsden\nCorn grain yields", 
       x = "", 
       y = "Yield [Mg/ha]", 
       color = "Treatment") +
  
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme_bw() +
  guides(color = F)
  # theme(legend.position = c(0, 1), 
  #       legend.justification = c(0, 1), 
  #       legend.background = element_rect(color = "black"),
  #       legend.box.margin = margin(c(10,10,10,10)))


biofig / (krnfig | yldfig | harfig)

ggsave("../_figs/MARSfig_bio-ylds.png", width = 7, height = 5, units = "in")

