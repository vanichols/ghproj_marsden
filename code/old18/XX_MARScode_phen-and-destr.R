#########################
##
## Date of creation: June 26 2018
## Date last modified: June 26 2018
##
## Author: Gina
## Purpose: Process phenology and destructive data
##          
## Inputs: from '2018_fieldseason' folder, _data_raw folder, 
##   "dat_mars-destructive_sampling.xlxs"
#    
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

#_________________________________________________________________________
#
# Read in data ------------------------------------------------------------
#_________________________________________________________________________

des <- read_xlsx("../2018_field-season/_data_raw/dat_mars-destructive_sampling.xlsx", skip = 5, na = "NA")
phen <- read_xlsx("../2018_field-season/_data_raw/dat_mars-phenology.xlsx", skip = 5, na = "NA")

#_________________________________________________________________________
#
# Look at it ------------------------------------------------------------
#_________________________________________________________________________

#-Green leaf LAI
ggplot(des, aes(date, ds_gLAI_cm2, color = trt)) + 
  geom_jitter(size = 2)

ggplot(data=des, aes(date, ds_gLAI_cm2)) + 
  stat_summary(fun.y="mean", geom="line", size = 2, aes(color = trt)) +
  stat_summary(fun.data="mean_se", geom="linerange",  size=2, aes(color = trt)) +
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) +

  labs(x = "", y = "Green Leaf Area [cm2]", title = "Marsden\nCorn Green Leaf LAI", color = "Treatment") +
  theme_bw() 
    

#-Plant height

ggplot(phen, aes(date, phen_plht_cm)) + 
  geom_jitter(size = 2, aes(color = trt))

ggplot(phen, aes(date, phen_plht_cm)) + 
  #geom_point(size = 2) +
  stat_summary(fun.y="mean", geom="line", size = 2, aes(color = trt)) +
  stat_summary(fun.data="mean_se", geom="linerange",  size=2, aes(color = trt)) +
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) +
  
  labs(x = "", y = "Plant Height [cm]", title = "Marsden\nCorn Plant Height", color = "Treatment") +
  theme_bw() 



#-Plant height + gLAI
dat  <- full_join(des, phen, by = c("plot", "trt", "date", "rep")) %>%
  group_by(date, trt) %>%
  summarise(gLAI_cm2 = mean(ds_gLAI_cm2),
            height_cm = mean(phen_plht_cm))

ggplot(dat, aes(x=date)) + 

  geom_col(aes(y=gLAI_cm2/10000, fill = trt), position = "dodge", color = "black") + 
  
  geom_line(aes(y = height_cm/100, color = trt, group = trt)) +
  geom_point(aes(y = height_cm/100, fill = trt), pch = 21, size = 4) +
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  scale_fill_manual(values = c("darkgoldenrod1", "cyan3")) +
  theme_bw() +

  theme(legend.position = c(0, 1), 
        legend.justification = c(0, 1), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) +
  guides(color = F) +
  
  labs(x = "", 
       y = "Plant Height [m] or\nGreen Leaf Area [m2 leaf per m2 land] ", 
       title = "Marsden\nCorn Plant Height and Leaf Area", 
       fill = "Treatment") 

ggsave("../_figs/gLAI-and-height_line-and-bar.png", width = 4, height = 5, units = "in")

#_________________________________________________________________________
#
# Play with stats ------------------------------------------------------------
#_________________________________________________________________________

# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor 

phendates <- unique(phen$date)
phen1 <- phen %>% filter(date == phendates[1])
phen2 <- phen %>% filter(date == phendates[2])
phen3 <- phen %>% filter(date == phendates[3])
t.test(phen1$phen_plht_cm~phen1$trt)
t.test(phen2$phen_plht_cm~phen2$trt)
t.test(phen3$phen_plht_cm~phen3$trt)


desdates <- unique(des$date)
des1 <- des %>% filter(date == desdates[1])
des2 <- des %>% filter(date == desdates[2])
des3 <- des %>% filter(date == desdates[3])
t.test(des1$ds_gLAI_cm2~des1$trt)
t.test(des2$ds_gLAI_cm2~des2$trt)
t.test(des3$ds_gLAI_cm2~des3$trt)


anova(lm(data=des, ds_gLAI_cm2~date+trt))
anova(lm(data=phen, phen_plht_cm~date+trt))
