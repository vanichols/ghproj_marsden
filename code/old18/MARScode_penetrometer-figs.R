#########################
##
## Date of creation: Feb 4 2019
## Date last modified: Feb 4 2019
##                     Feb 6 (update tillage depths)
##
## Author: Gina
## Purpose: Process penetrometer data and paired soil moisture data
##          
## Inputs: td-penetrometer.csv
##
## Outputs: many figs, 
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


pen <- read_csv("../_data/tidy/td-penetrometer.csv")


# First sampling data -----------------------------------------------------


# Average resis and mois across all reps
# subtract 1 from all depths since we didn't actually start at the surface w/0 pressure
#
pen %>%
  filter(samp_doy == 130) %>% 
  mutate(depth_in = depth_cm * 2.54,
         resis_psi = resis_kPa / 6.89476) %>%
  group_by(trt, block, depth_in) %>%
  summarise(resis_psi = mean(resis_psi),
            soilmois_g.g = mean(soilmois_g.g, na.rm = T)) %>%
  
  ggplot(aes(depth_in, resis_psi, 
             group = interaction(trt,block), 
             color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  coord_flip() + 
  scale_x_reverse() + 
  
  # Add tillage depths
  geom_vline(xintercept = 6, linetype = 3) + 
  geom_vline(xintercept = 9, linetype = 3) + 
  
  # add labels
  annotate(geom = "text", 
           label = "cultivator 6 in.\nbefore planting", 
           x = 6, y = 10, hjust = 0, fontface = "italic") +
  annotate(geom = "text", 
           label = "moldboard 9 in.\nprevious fall", 
           x = 9, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0.9,0.9),
        legend.background = element_rect(colour = "black")) + 
  labs(y = "Resistance [PSI]", x = "Depth [in]", color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

ggsave("../_figs/MARSfig_spring-penet-in.png",
       width = 4.24, height = 7, units = "in")

# Look at ave soil mois by block/resis
pen %>%
  filter(samp_doy == 130) %>% 
  mutate(depth_in = depth_cm * 2.54,
         resis_psi = resis_kPa / 6.89476) %>%

  group_by(trt, block, depth_in) %>%
  summarise(resis_psi = mean(resis_psi),
            soilmois_g.g = mean(soilmois_g.g, na.rm = T)) %>%
  
  ggplot(aes(depth_in, resis_psi, 
             group = interaction(trt,block), 
             color = soilmois_g.g)) + 
  geom_line(size = 2, aes(linetype = trt)) + 
  coord_flip() + 
  scale_x_reverse() + 
  
  # Add tillage depths
  geom_vline(xintercept = 6, linetype = 3) + 
  geom_vline(xintercept = 9, linetype = 3) + 
  
  # add labels
  annotate(geom = "text", 
           label = "cultivator 6 in.\nbefore planting", 
           x = 6, y = 10, hjust = 0, fontface = "italic") +
  annotate(geom = "text", 
           label = "moldboard 9 in.\nprevious fall", 
           x = 9, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = "black")) + 
  labs(y = "Resistance [PSI]", 
       x = "Depth [in]",
       color = "Soil\nMoisture\n[g/g]", 
       linetype = "Treatment") + 
  scale_color_gradient(low = "red", high = "blue")


ggsave("../_figs/MARSfig_spring-penet-mois-in.png", 
       width = 4.24, height = 7, units = "in")


# Look at just the values we have soil moisture for

pen %>%
  filter(samp_doy == 130, soilpair_YN == "Y") %>% 
  mutate(depth_in = depth_cm * 2.54,
         resis_psi = resis_kPa / 6.89476) %>%
  
  ggplot(aes(depth_in, resis_psi, group = samp_ID, color = soilmois_g.g)) +
  geom_point() + 
  geom_line() + 
  facet_grid(block~trt) + 
  coord_flip() + 
  scale_x_reverse() + 
  scale_color_gradient(low = "red", high = "blue") + 
  theme_bw() +  
  theme(legend.position = "bottom") +
  labs(y = "Resistance [PSI]", 
       x = "Depth [in]", 
       color = "Soil Moisture\n[g H2O/g soil]") 

ggsave("../_figs/MARSfig_penet-mois2-in.png", width = 4.24, height = 7, units = "in")



# trt-by-depth in cm (no soil mois) ---------------------------------------
pen %>%
  filter(samp_doy == 130) %>% 
  mutate(depth_in = depth_cm * 2.54,
         resis_psi = resis_kPa / 6.89476) %>%
  group_by(trt, block, depth_cm) %>%
  summarise(resis_psi = mean(resis_psi),
            soilmois_g.g = mean(soilmois_g.g, na.rm = T)) %>%
  
  ggplot(aes(depth_cm, resis_psi, group = interaction(trt,block), color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  #scale_linetype_manual(values = c("longdash", "F1")) +
  coord_flip() + 
  scale_x_reverse() + 
  
  # Add tillage depths
  geom_vline(xintercept = 15, linetype = 3) + 
  geom_vline(xintercept = 23, linetype = 3) + 
  
  # add labels
  annotate(geom = "text", 
           label = "cultivated (C2 and C4) 15 cm\nbefore planting", 
           x = 15, y = 10, hjust = 0, fontface = "italic") +
  annotate(geom = "text", 
           label = "moldboard (C4) 23 cm\nprevious fall", 
           x = 23, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) + 
  labs(y = "Resistance [PSI]\nTaken at Planting", x = "Depth [cm]", color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

ggsave("../_figs/MARSfig_penet-cm.png", width = 4.24, height = 7, units = "in")



# - Look at each measurement
#
pen %>%
  filter(samp_doy == 197) %>%  
  ggplot(aes(depth_cm, resis_kPa, 
             group = samp_ID, 
             color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  coord_flip() + 
  scale_x_reverse() + 
  facet_grid(.~block) +
  
  # Add tillage depths
  geom_vline(xintercept = 15, linetype = 3) +   
  
  geom_vline(xintercept = 23, linetype = 3) + 
  
  
  # add labels
  #annotate(geom = "text", label = "chisel (C2 and C4) 10-13 cm\nbefore planting", 
  #         x = 11.5, y = 10, hjust = 0, fontface = "italic") +
  #annotate(geom = "text", label = "moldboard (C4) 20-23 cm\nprevious fall", 
  #         x = 21.5, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) + 
  labs(y = "Resistance [kPa]\nTaken July 16 2018", x = "Depth [cm]", color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Make graphs w/all data ----------------------------------------------------
#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


pen %>%
  mutate(plot = as.factor(plot),
         fancy_date = paste(month(date, label = T), 
                            day(date), 
                            sep = "-"),
         fancy_date = factor(fancy_date, levels = c("May-10", "Jul-16"))) %>% 
  filter(depth_cm <= 40) %>%
  
  # Plot it
  ggplot(aes(depth_cm, resis_kPa, group = samp_ID, color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
 
  coord_flip() + 
  scale_x_reverse() + 
  facet_grid(fancy_date~block) +
  
  # Add tillage depths
  geom_vline(xintercept = 15, linetype = 3) +   
  
  geom_vline(xintercept = 23, linetype = 3) +   
  
  theme_classic() + 
  theme(legend.position = c(1, 1), 
        legend.justification = c(1, 1), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) + 
  labs(y = "Resistance [KPa]", 
       x = "Depth [cm]", 
       color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))


ggsave("../_figs/MARSfig_penet-all.png", height = 8, width = 8)


# Let them have different scales

p2 <- pen %>%
  mutate(plot = as.factor(plot),
         fancy_date = paste(month(date, label = T), 
                            day(date), 
                            sep = "-"),
         fancy_date = factor(fancy_date, levels = c("May-10", "Jul-16"))) %>% 
  filter(depth_cm <= 40, fancy_date == "Jul-16") %>%
  
  # Plot it
  ggplot(aes(depth_cm, resis_kPa, group = samp_ID, color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  
  coord_flip() + 
  scale_x_reverse() + 
  facet_grid(fancy_date ~ block) +
  
  # Add tillage depths
  geom_vline(xintercept = 15, linetype = 3) +   
  geom_vline(xintercept = 23, linetype = 3) +   
  
  theme_classic() + 
  guides(color = F) +
  # theme(legend.position = c(1, 1), 
  #       legend.justification = c(1, 1), 
  #       legend.background = element_rect(color = "black"),
  #       legend.box.margin=margin(c(10,10,10,10))) + 
  labs(y = "Resistance [KPa]", 
       x = "Depth [cm]", 
       color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

p1 <- pen %>%
  mutate(plot = as.factor(plot),
         fancy_date = paste(month(date, label = T), 
                            day(date), 
                            sep = "-"),
         fancy_date = factor(fancy_date, levels = c("May-10", "Jul-16"))) %>% 
  filter(depth_cm <= 40, fancy_date == "May-10") %>%
  
  # Plot it
  ggplot(aes(depth_cm, resis_kPa, group = samp_ID, color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  
  coord_flip() + 
  scale_x_reverse() + 
  facet_grid(fancy_date ~ block) +
  
  # Add tillage depths
  geom_vline(xintercept = 15, linetype = 3) +   
  
  geom_vline(xintercept = 23, linetype = 3) +   
  
  theme_classic() + 
  theme(legend.position = c(1, 1), 
        legend.justification = c(1, 1), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) + 
  labs(y = NULL, #"Resistance [KPa]", 
       x = "Depth [cm]", 
       color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))


(p1) / (p2)
ggsave("../_figs/MARSfig_penet-all-separate-scales.png", 
       height = 8, width = 8)


save.image(file = "../newpresentme/MARSenv_penetrometer-figs.RData")

save.image("C:/Users/Gina Laptippytop/Box Sync/1_Gina_Projects/proj_Marsden/MARSproj_field-season-2018/_make-figs/MARSenv_penetrometer-figs.RData")














