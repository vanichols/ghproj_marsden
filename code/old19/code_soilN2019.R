#########################
##
## Date of creation: Nov 5 2019
#
## Date last modified: 
##
## Author: Gina
## Purpose: Process Javed's nitrogen data
##          
## Inputs: Javed_Forecast... (in peeps folder)
##
## Outputs: td_soilN19
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
  filter(year == 2019)

read_excel("_theme-2019data/_data/peeps/Javed_Forecast_soilN2019.xlsx") %>% 
  clean_names() %>% 
  filter(site == "Marsden") %>% 
  rename("plot" = sample_id) %>% 
  # get it in kgha
  mutate(bd = 1.3,
         no3_kgha = no3_n_mg_kg_1_soil * 1.3 * 0.3 * 10,
         nh4_kgha = nh4_n_mg_kg_1_soil * 1.3 * 0.3 * 10) %>% 
  mutate(date = as_date(sampling_date)) %>% 
  select(date, plot, depth, no3_kgha, nh4_kgha) %>% 
  write_csv("_theme-2019data/_data/tidy/td-soiln19.csv")

