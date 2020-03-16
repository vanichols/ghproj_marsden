#########################
##
## Date of creation: Feb 5 2019
## Date last modified: 
##                      Feb 5 2019 (updating)
##
## Author: Gina
## Purpose: Make nutrients and residue figs
##          
## Inputs: td-residue, td-nutrients
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





##=========================================##
# Read data ---------------------------------------------------------------
##=========================================##

roo <- read_csv("_theme-2018data/_data/tidy/td-residue.csv")%>%
  mutate(residue_Mgha = residue_g * 0.01)
noo <- read_csv("_theme-2018data/_data/tidy/td-nutrients.csv") %>%
  mutate_if(is.character, as.factor)

##===================================================================##
##   Look at it
##===================================================================##

roo %>%
  ggplot(aes(trt, residue_Mgha)) + 
  stat_summary(fun.data = "mean_se", geom = "crossbar",  aes(color = trt)) +
  
  labs(title = "Marsden\nCorn plot residue at planting", 
       x = "", 
       y = "Surface Residue [Mg/ha]", 
       color = "Treatment") +
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  guides(color = F) + 
  theme_bw() 

ggsave("../_figs/MARSfig_residue.png", width = 3, height = 4)

# Micronutrients
noo %>%
  filter(msmt %in% c("B", "CA", "CU", "FE", "K", "MG", "MN", "Na", "S", "ZN",
                     "CEC", "OM")) %>%
  
  ggplot(aes(trt, value, group = interaction(trt, depth_cm))) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2,  
               aes(color = depth_cm)) +
  stat_summary(fun.y = "mean", geom = "point", aes(color = depth_cm)) +
  
  facet_wrap(~ msmt, scales = "free") +

  labs(title = "Marsden\nMicronutrients", 
       x = "", 
       y = "") +
  
  #scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme_bw() +
  theme(legend.position = "bottom")
  
  
ggsave("../_figs/MARSfig_micronutrients.png", height = 6, width = 6)
