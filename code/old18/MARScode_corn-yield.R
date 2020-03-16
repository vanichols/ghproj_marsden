#########################
#
# Date of creation: Nov 28 2018
#
# Date last modified: Nov 28 2018
#                     Feb 5 2019
#                     Oct 31 2019
# Author: Gina
# Purpose: Process 2018 yield data
#
# Inputs: rd_mars_2018-corn-yields.csv
#    
#
# Outputs: td-corn-yield.csv
#

# NOTES: 
##
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files



#~~~~~~~~~~~~~~~~~~~
# Read in data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

mykey <- read_csv("../_data/tidy/td-plot-trt-key.csv") %>%
  mutate(block= paste0("B", block))

yld <- read_csv("../_data/raw_entered/rd_mars_2018-corn-yields.csv", skip = 4) %>%
  
  # Average over conventional/low trts
  group_by(plot, rot) %>%
  summarise_all(mean) %>%
  select(-trt) %>%
  mutate(date = as_date('2018-10-31'),
         yield_Mgha = yield_Mgha155 * (1 - 0.155)) %>%
  rename(trt = rot) %>%
  left_join(mykey) %>%
  select(date, trt, block, plot, yield_Mgha) %>%
  
  # get rid of C3
  filter(!is.na(block))
  

#~~~~~~~~~~~~~~~~~~~
# Write to tidy folder ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

write_csv(yld, "../_data/tidy/td-corn-yield.csv")
