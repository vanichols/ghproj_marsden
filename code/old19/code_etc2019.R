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
library(fuzzyjoin) #--to do fuzzy joining of dates
library(here)

setwd(here())

key <- read_csv("_data/tidy/td_year-plot-trt-key.csv") %>%
  filter(year == 2019) %>%
  mutate(block = str_sub(plot, 1, 1))

# Wyatt's stand count data------------------------------------------------

scraw <- read_excel("_theme-2019data/_data/raw_entered/rd_20190626-standcounts.xlsx",
                 skip = 5) %>%
  fill(date, plot) %>%
  left_join(key) 

scraw %>%
  mutate(block = str_sub(plot, 1, 1)) %>%
  group_by(plot, system, block) %>%
  # summarise(pop_no = mean(pop_no),
  #           sd = sd(pop_no)) %>%
  # 
  ggplot(aes(system, pop_no)) + 
#  geom_col(aes(fill = system), position = "dodge") +
  stat_summary(fun.y = mean, geom = "bar", position = "dodge", aes(fill = system)) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = "dodge", aes(fill = system)) +
  labs(y = "Plants per Acre")
  

#
# Soil N ------------------------------------------------

soiln <- 
  read_excel("_theme-2019data/_data/peeps/Javed_Forecast_soilN2019.xlsx") %>%
  clean_names() %>%
  filter(site == "Marsden") %>%
  rename(plot = sample_id,
         date = sampling_date,
         no3N_mgkg = no3_n_mg_kg_1_soil,
         nh4N_mgkg = nh4_n_mg_kg_1_soil) %>%
  mutate(plot = as.numeric(plot),
         depth = as.factor(depth),
         totN_mgkg = no3N_mgkg + nh4N_mgkg) %>%
  left_join(key)

soiln %>%
  gather(no3N_mgkg:totN_mgkg, key = nit, value = mgkg) %>%
  
  ggplot(aes(block, mgkg)) + 
  geom_point(aes(color = system), size = 3) + 
  facet_grid(depth ~ nit)
