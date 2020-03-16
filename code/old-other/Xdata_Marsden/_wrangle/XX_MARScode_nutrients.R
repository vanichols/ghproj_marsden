#########################
##
## Date of creation: May 11 2018
## Date last modified: May 16 2018
##
## Author: Gina
## Purpose: Process Agsource data on soil samples
##          
## Inputs: from '2018_fieldseason' folder, _data_raw folder, 
##   "dat_mars-soil_nutrients.xlxs"
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

##=========================================##
##   Read in data
##=========================================##

ag <- read_xlsx("../2018_field-season/_data_raw/dat_mars-soil_nutrients.xlsx", skip = 5, na = "NA") %>%
  mutate(depth = str_sub(samp_id, -1),
         block = str_sub(samp_id, 1, 2),
         trt = str_sub(samp_id, 7, 8),
         depth_cm = ifelse(depth == 1, "0-30", ifelse(depth == 2, "30-60", "60-90"))) %>%
  gather(PH:SALTS, key = 'msmt', value = 'value')

##===================================================================##
##   Look at it, but literally nothing is different/interesting
##===================================================================##

# Phosphorous
#
ggplot(filter(ag, msmt == "P_M3"), aes(trt, value, color = block)) + 
  geom_jitter(size = 2, width = 0.1 ) + 
  facet_grid(.~depth_cm)


# CA 
#
ggplot(filter(ag, msmt == "CA"), aes(trt, value, color = block)) + 
  geom_jitter(size = 2, width = 0.1 ) + 
  facet_grid(.~depth_cm)

# OM
#
ggplot(filter(ag, msmt == "OM"), aes(trt, value, color = block)) + 
  geom_jitter(size = 2, width = 0.1 ) + 
  facet_grid(.~depth_cm)
