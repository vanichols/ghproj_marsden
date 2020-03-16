#########################
##
## Date of creation: May 11 2018
## Date last modified: May 16 2018
##                     Feb 4 2019 - revisit
##
## Author: Gina
## Purpose: Process penetrometer data and paired soil moisture data
##          
## Inputs: from '2018_fieldseason' folder, _data_raw folder, 
##   "dat_mars-soil_penetrometer.xlxs"
##   "dat_mars-soil_penetrometer-OLK.xlxs"
##   "dat_mars-soil_penetrometer-SPECTRUM-07-16-18.xlxs"
##
## Outputs: many figs, td-penetrometer.csv
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
##   Read in data from first sampling
##=========================================##

# Soil moisture
#
sm <- read_excel("../_data/raw_entered/rd_mars-soil_penetrometer.xlsx",
                 skip = 5, na = "NA") %>%
  mutate(samp_ID = paste(block, trt, N, sep = "-"),
         #Calculate %mois by weight
         soilmois_g.g = 
           1 - (drysoil_g - drybag_g) / (wetsoil_g - wetbag_g)) %>%
  
  # something went wrong with B2-C4-N5 and -N6 moistures, change them to NA
  mutate(soilmois_g.g = ifelse(samp_ID == 'B2-C4-N5', NA, soilmois_g.g),
         soilmois_g.g = ifelse(samp_ID == 'B2-C4-N6', NA, soilmois_g.g)) %>%
  
  # Get rid of comments associated with N
  mutate(N = str_trim( str_sub(N, 1,3))) %>%
  select(-date, -msmtdepth_in, -soildepth_in, -(wetsoil_g:drybag_g), -notes)
     
ggplot(sm, aes(trt, soilmois_g.g, color = block)) + 
  geom_point() + 
  facet_grid(.~block)

# Penetrometer
#
olk <- read_excel("../_data/raw_entered/rd_mars-soil_penetrometer-OLK.xlsx",
                  skip = 5) %>%
  # remove 'Logger Started' rows
  filter(Number != 'Logger Started') %>%
  
  # remove XX measurements
  filter(Number != 'XX') %>%
  
  # Make and N column that matches sm data
  mutate(N = str_trim( (str_sub(Number, -2, -1))),
         N = paste("N", N, sep = ""),
         N = ifelse(soilpair_YN == "N", paste("X", N, sep = ""), N)) %>%
  
  # Make samp_ID that matches sm data
  mutate(samp_ID = paste(block, trt, N, sep = "-")) %>%
  
  # gather depths into one col
  gather(depth_00:depth_18, key = "depth_in", value = "resis_psi") %>%
  
  select(-Number, -`Cone Size`) %>%
  # sort that shit
  arrange(samp_ID, depth_in)


##=========================================##
##   Merge sm with olk NOTE: I think the depths are off. What it thinks is 1, is actually 0.  
##=========================================##

dat <- olk %>% left_join(sm, by = c("samp_ID", "block", "trt", "N", "soilpair_YN")) %>%
  
  # Change depth_in to numeric inches
  mutate(depth_in = as.numeric(str_sub(depth_in, -2, -1)),
         depth_cm = depth_in * 2.54)

##=========================================##
##   Look at the data
##=========================================##

# Average resis and mois across all reps
# subtract 1 from all depths since we didn't actually start at the surface w/0 pressure
#
dat %>% mutate(depth_in = depth_in - 1) %>%
  filter(depth_in >=0) %>%
  group_by(trt, block, depth_in) %>%
  summarise(resis_psi = mean(resis_psi),
            soilmois_g.g = mean(soilmois_g.g, na.rm = T)) %>%
  
  ggplot(aes(depth_in, resis_psi, group = interaction(trt,block), color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  #scale_linetype_manual(values = c("longdash", "F1")) +
  coord_flip() + 
  scale_x_reverse() + 
  
  # Add tillage depths
  geom_vline(xintercept = 5, linetype = 3) + 
  geom_vline(xintercept = 9, linetype = 3) + 
  
  # add labels
  annotate(geom = "text", label = "chisel 4-5 in.\nbefore planting", x = 5, y = 10, hjust = 0, fontface = "italic") +
  annotate(geom = "text", label = "moldboard 8-9 in.\nprevious fall", x = 9, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0.9,0.9),
        legend.background = element_rect(colour = "black")) + 
  labs(y = "Resistance [PSI]", x = "Depth [in]", color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

ggsave("../_figs/MARSfig_spring-penet-in.png", width = 4.24, height = 7, units = "in")

# Look at ave soil mois by block/resis
dat %>% mutate(depth_in = depth_in - 1) %>%
  filter(depth_in >=0) %>%
  group_by(trt, block, depth_in) %>%
  summarise(resis_psi = mean(resis_psi),
            soilmois_g.g = mean(soilmois_g.g, na.rm = T)) %>%
  
  ggplot(aes(depth_in, resis_psi, group = interaction(trt,block), color = soilmois_g.g)) + 
  geom_line(size = 2, aes(linetype = trt)) + 
  coord_flip() + 
  scale_x_reverse() + 
  
  # Add tillage depths
  geom_vline(xintercept = 5, linetype = 3) + 
  geom_vline(xintercept = 9, linetype = 3) + 
  
  # add labels
  annotate(geom = "text", label = "chisel 4-5 in.\nbefore planting", x = 5, y = 10, hjust = 0, fontface = "italic") +
  annotate(geom = "text", label = "moldboard 8-9 in.\nprevious fall", x = 9, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = "black")) + 
  labs(y = "Resistance [PSI]", x = "Depth [in]", color = "Soil\nMoisture\n[g/g]", linetype = "Treatment") + 
  scale_color_gradient(low = "red", high = "blue")


ggsave("../_figs/MARSfig_spring-penet-mois-in.png", width = 4.24, height = 7, units = "in")


# Look at just the values we have soil moisture for

dat %>% mutate(depth_in = depth_in - 1) %>%
  filter(depth_in >=0) %>%
  filter(soilpair_YN == "Y") %>%
  ggplot(aes(depth_in, resis_psi, group = samp_ID, color = soilmois_g.g)) +
  geom_point() + 
  geom_line() + 
  facet_grid(block~trt) + 
  coord_flip() + 
  scale_x_reverse() + 
  scale_color_gradient(low = "red", high = "blue") + 
  theme_bw() +  
  theme(legend.position = "bottom") +
  labs(y = "Resistance [PSI]", x = "Depth [in]", color = "Soil Moisture\n[g H2O/g soil]") 

ggsave("../_figs/MARSfig_penet-mois2-in.png", width = 4.24, height = 7, units = "in")



# trt-by-depth in cm (no soil mois) ---------------------------------------

dat %>% mutate(depth_cm = depth_cm - 2.54) %>%
  filter(depth_cm >=0) %>%
  group_by(trt, block, depth_cm) %>%
  summarise(resis_psi = mean(resis_psi),
            soilmois_g.g = mean(soilmois_g.g, na.rm = T)) %>%
  
  ggplot(aes(depth_cm, resis_psi, group = interaction(trt,block), color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  #scale_linetype_manual(values = c("longdash", "F1")) +
  coord_flip() + 
  scale_x_reverse() + 
  
  # Add tillage depths
  geom_vline(xintercept = 13, linetype = 3) + 
  geom_vline(xintercept = 23, linetype = 3) + 
  
  # add labels
  annotate(geom = "text", label = "chisel (C2 and C4) 10-13 cm\nbefore planting", x = 13, y = 10, hjust = 0, fontface = "italic") +
  annotate(geom = "text", label = "moldboard (C4) 20-23 cm\nprevious fall", x = 23, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) + 
  labs(y = "Resistance [PSI]\nTaken at Planting", x = "Depth [cm]", color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

ggsave("../_figs/MARSfig_penet-cm.png", width = 4.24, height = 7, units = "in")



#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Second sampling data ----------------------------------------------------
#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Penetrometer
#
poke <- read_excel("../_data/raw_entered/rd_mars-soil_penetrometer-Spctrm-Gina-07-16-18.xlsx", skip = 5) %>%
  
  # remove 'Logger Started' rows
  filter(Number != 'Logger Started') %>%
  
  # remove XX measurements
  filter(Number != 'XX') %>%
  
  # Make an N column
  mutate(N = str_trim( (str_sub(Number, -2, -1))),
         N = paste("N", N, sep = "")) %>%
  
  # Make samp_ID and rep column
  mutate(samp_ID = paste(plot, trt, N, sep = "-"),
         rep = plot %/% 10) %>%
  
  # gather depths into one col
  gather(depth_00.0:depth_45.0, key = "depth_cm", value = "resis_KPa") %>%
  
  select(-Number, -`Cone Size`) %>%
  # sort that shit
  arrange(samp_ID, depth_cm) %>%
  
  # Change depth_in to numeric cm
  mutate(depth_cm = as.numeric(str_sub(depth_cm, -4, -1)))

poke

# - Graph it by plot
#

poke %>%
  mutate(plot = as.factor(plot)) %>%
  filter(depth_cm <= 40) %>%
  group_by(trt, plot, depth_cm) %>%
  summarise(resis_KPa = mean(resis_KPa)) %>%
  
  ggplot(aes(depth_cm, resis_KPa, group = interaction(trt,plot), color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  #scale_linetype_manual(values = c("longdash", "F1")) +
  coord_flip() + 
  scale_x_reverse() + 
  
  # Add tillage depths
  #geom_vline(xintercept = 10, linetype = 3) +   
  geom_vline(xintercept = 13, linetype = 3) + 

  #geom_vline(xintercept = 20, linetype = 3) +   
  geom_vline(xintercept = 23, linetype = 3) + 

  
  # add labels
  annotate(geom = "text", label = "chisel (C2 and C4) 10-13 cm\nbefore planting", x = 11.5, y = 10, hjust = 0, fontface = "italic") +
  annotate(geom = "text", label = "moldboard (C4) 20-23 cm\nprevious fall", x = 21.5, y = 10, hjust = 0, fontface = "italic") + 
  
  theme_classic() + 
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10))) + 
  labs(y = "Resistance [KPa]\nTaken July 16 2018", x = "Depth [cm]", color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

ggsave("../_figs/fig_MARS-penet-071618.png", 
       width = 4.24, height = 7, units = "in")


# - Look at each measurement
#
poke %>%
  mutate(plot = as.factor(plot)) %>%
  filter(depth_cm <= 40) %>%
  
  ggplot(aes(depth_cm, resis_KPa, group = interaction(N, plot), color = trt)) + 
  geom_line(size = 2, alpha = 1) + 
  #scale_linetype_manual(values = c("longdash", "F1")) +
  coord_flip() + 
  scale_x_reverse() + 
  facet_grid(.~rep) +
  
  # Add tillage depths
  geom_vline(xintercept = 10, linetype = 3) +   
  geom_vline(xintercept = 13, linetype = 3) + 
  
  geom_vline(xintercept = 20, linetype = 3) +   
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
  labs(y = "Resistance [KPa]\nTaken July 16 2018", x = "Depth [cm]", color = "Treatment") + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3"))

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Combine two samplings, I hate you Gina ----------------------------------
#
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

mykey <- read_csv("../_data/tidy/td-plot-trt-key.csv") %>%
  mutate(block = paste0("B", block))

dat2 <- 
  dat %>%
  left_join(mykey) %>%
  mutate(date = as_date("2018-05-10"), 
         samp_doy = yday(date),
         resis_kPa = resis_psi * 6.89476) %>%
    select(year, date, samp_doy, plot, block, trt, soilpair_YN,
           soilmois_g.g,
           samp_ID, depth_cm, resis_kPa)
    

poke2 <- 
  poke %>%
  left_join(mykey) %>%
  mutate(samp_ID = paste(block, trt, N, sep = "-"),
         date = as_date("2018-07-16"),
         samp_doy = yday(date), 
         soilpair_YN = 'N',
         soilmois_g.g = NA) %>%
  rename("resis_kPa" = resis_KPa) %>%
  select(year, date, samp_doy, plot, block, trt, soilpair_YN,
         soilmois_g.g,
         samp_ID, depth_cm, resis_kPa)


pen <- bind_rows(dat2, poke2)  


write_csv(pen, "../_data/tidy/td-penetrometer.csv")

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
  geom_vline(xintercept = 10, linetype = 3) +   
  #geom_vline(xintercept = 13, linetype = 3) + 
  
  geom_vline(xintercept = 20, linetype = 3) +   
  #geom_vline(xintercept = 23, linetype = 3) + 
  

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
  geom_vline(xintercept = 10, linetype = 3) +   
  #geom_vline(xintercept = 13, linetype = 3) + 
  
  geom_vline(xintercept = 20, linetype = 3) +   
  #geom_vline(xintercept = 23, linetype = 3) + 
  
  
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
  geom_vline(xintercept = 10, linetype = 3) +   
  #geom_vline(xintercept = 13, linetype = 3) + 
  
  geom_vline(xintercept = 20, linetype = 3) +   
  #geom_vline(xintercept = 23, linetype = 3) + 
  
  
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



















