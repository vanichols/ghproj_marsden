#########################
##
## Date of creation: Nov 5 2019
#
## Date last modified: 
##
## Author: Gina
## Purpose: Process Javed's nitrogen data
##          
## Inputs: soilN2018
##
## Outputs: td_soilN
##
## NOTE: 
##
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files


# read data ---------------------------------------------------------------

mykey <- read_csv("_data/tidy/td_year-plot-trt-key.csv") %>%
  mutate(block = paste0("B", plot%/%10)) %>% 
  filter(year == 2018)

#--note: this seems wrong
# soilnraw <- read_csv("_theme-2018data/_data/peeps/tidy-soilN.csv") %>% 
#   filter(site == "MARSDEN") %>% 
#   mutate(Ntot = NO3_kg + NH4_kg)

soilnrawnames <- names(read_excel("_theme-2018data/_data/peeps/soilN2018.xlsx", skip = 1))

soilnraw <- read_excel("_theme-2018data/_data/peeps/soilN2018.xlsx", skip = 2) 

names(soilnraw) <- soilnrawnames

soilnraw %>% 
  filter(!is.na(Location)) %>% 
  select(Date, Location, Plot, Depth, Treatment, Replication, `NO3-N...14`, `NH4-N...7`) %>% 
  filter(Location == "Marsden") %>% 
  rename("no3_kgha"=7,
         "nh4_kgha"=8) %>% 
  mutate_at(vars(no3_kgha, nh4_kgha), funs(as.numeric)) %>% 
  mutate(date = as_date(Date)) %>% 
  write_csv("_theme-2018data/_data/tidy/td-soiln18.csv")

# wrangle ---------------------------------------------------------------------


soilnraw %>% 
  mutate(depth_cm = recode(depth, 
                           '01' = '0-30',
                           '12' = '30-60',
                           '23' = '60-90'),
         depth_cm = factor(depth_cm, levels = rev(c("0-30", "30-60", "60-90")))) %>% 
  ggplot(aes(depth_cm, y = Ntot)) +
  stat_summary(aes(color = trt)) +
  facet_grid(~date) + 
  coord_flip()
           
