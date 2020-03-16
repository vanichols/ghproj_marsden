#########################
##
## Date of creation: May 11 2018
#
## Date last modified: May 16 2018
#                      Feb 5 2019 (updating)
##
## Author: Gina
## Purpose: Process Agsource data on soil samples
##          
## Inputs: rd_mars-soil_nutrients.xlxs
##
## Outputs: td-nutrients
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

mykey <- read_csv("../_data/tidy/td-plot-trt-key.csv") %>%
  mutate(block = paste0("B", block))

ag <- read_excel("../_data/raw_entered/rd_mars-soil_nutrients.xlsx", 
                 skip = 5, na = "NA") %>%
  mutate(depth = str_sub(samp_id, -1),
         block = str_sub(samp_id, 1, 2),
         trt = str_sub(samp_id, 7, 8),
         depth_cm = ifelse(depth == 1, "0-30", ifelse(depth == 2, "30-60", "60-90")),
         date = as_date("2018-05-16")) %>%
  gather(PH:SALTS, key = 'msmt', value = 'value') %>%
  left_join(mykey) %>%
  select(date,  samp_id, trt, block, plot, depth_cm, msmt, value)

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

##===================================================================##
# Write it ----------------------------------------------------------------
##===================================================================##

write_csv(ag, "../_data/tidy/td-nutrients.csv")
