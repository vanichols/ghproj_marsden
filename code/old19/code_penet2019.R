#########################
##
## Date of creation: June 11 2019
## Date last modified: June 11 2019
##                     Oct 31 2019 (made a figure for ASA, this is a mess)
##
## Author: Gina
## Purpose: Process penetrometer data into one file
##          
## Inputs: 
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
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data


setwd(here())

# Data from first sampling June 11 2019 ------------------------------------------------

raw <- read_excel("_theme-2019data/_data/raw/rd_20190611-penet.xlsx", skip = 4)
key <- read_csv("_data/tidy/td_year-plot-trt-key.csv") %>%
  filter(year == 2019)
  

pen <- 
  raw %>%
  clean_names() %>%
  select(-longitude, -latitude, -cone_size, -trt_guess) %>%
  rename(plot = plot_guess) %>%

  # remove 'Logger Started' rows
  filter(number != 'Logger Started',
         !is.na(plot_anon)) %>%
  
  #--merge w/key
  left_join(key, by = "plot") %>%
  
  # Make and N column that matches sm data
  mutate(N = str_trim( (str_sub(number, -2, -1))),
         N = paste("N", N, sep = "")) %>%

  # gather depths into one col, make numeric
  gather(x0_0_cm:x45_0_cm, key = "depth_cm", value = "resis_kpa") %>%
  mutate(depth_cm = str_sub(depth_cm, start = 2, end = -4),
         depth_cm = str_replace(depth_cm, "_", "."),
         depth_cm = as.numeric(depth_cm),
         plot = as.factor(plot)) %>%
  #--weird 0 values
  filter(resis_kpa >0)

pen %>% write_csv("_theme-2019data/_data/tidy/td_penetrometer19.csv")

# look at it --------------------------------------------------------------

pen %>%
  #group_by(depth_cm, plot_anon) %>%
  #summarise(resis_kpa = mean(resis_kpa)) %>%
  ggplot(aes(depth_cm, resis_kpa)) + 
  geom_point(aes(color = plot)) +
#  geom_line(aes(color = plot)) + 
  scale_x_reverse() +
  coord_flip() +
  facet_wrap(~plot_anon) + 
  theme_bw()
  

pen %>%
  group_by(depth_cm, plot, trt) %>%
  summarise(resis_kpa = mean(resis_kpa)) %>%
  mutate(trtdesc = ifelse(trt == "C4", 
                          "Following Alfalfa",
                          "Following Soybean")) %>% 
  ggplot(aes(depth_cm, resis_kpa, group = as.factor(plot))) + 
  geom_line(aes(color = trtdesc), size = 3) + 
  geom_point() +
  scale_x_reverse() +
  geom_vline(xintercept = 0, color = "brown", size = 1.5) +
  geom_text(x = -1, y = 1100, label = "Soil Surface", hjust = 1, vjust = 1, color= "brown") +
  coord_flip() +
  labs(color = NULL, x = "Depth (cm)", y = "Resistance (kPa)") +
  scale_color_manual(values = c("green4", "olivedrab1")) +
  theme_bw() + 
  theme(legend.position = "top",
        legend.text = element_text(size = rel(1.2)))

#ggsave("_theme-2019data/_figs/glimpse/penet-resis.png")
ggsave("_figs/fig_ASA-penetrometer.png", height = 8, width = 4)
