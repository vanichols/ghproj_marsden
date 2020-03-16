#########################
#
# Date of creation: Nov 28 2018
# Date last modified: Nov 28 2018
#                     Feb 4 2019, updated
#
# Author: Gina
# Purpose: Process destructive data
#
# Inputs: rd_mars-destructive-sampling.xlsx
#    
# Outputs: td-biomass, td-lai, td-500krnlwt 
#
# NOTES: 
#
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

#~~~~~~~~~~~~~~~~~~~
# Read in data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

mykey <- read_csv("../_data/tidy/td-plot-trt-key.csv") %>%
  mutate(block = paste0("B", block))

bm <- read_xlsx("../_data/raw_entered/rd_mars-destructive_sampling.xlsx", 
                skip = 5, na = "NA") %>%
  mutate(date = as_date(date)) %>%
  
  # Add up all organs
  mutate(gleafwt_g = ds_gleafwtsubsam_g + ds_gleafwtother_g,
         tbm_gm2 = gleafwt_g + ds_deadleafwt_g + 
           ds_stemtass_g + ds_ears_g + 
           ds_husks_g + ds_cobs_g + ds_kernals_g,
         tbm_Mgha = tbm_gm2/100)
  
tbio <- bm %>% left_join(mykey) %>%
  select(date, trt, block, plot, ds_stage, ds_nopl, tbm_Mgha)

lai <- bm %>% left_join(mykey) %>%
  select(date, trt, block, plot, ds_stage, ds_nopl, ds_gLAI_cm2) %>%
  mutate(ds_gLAI_cm2 = as.numeric(ds_gLAI_cm2),
         ds_gLAI_cm2 = round(ds_gLAI_cm2, 2))

kw <- bm %>% left_join(mykey) %>%
  select(date, trt, block, plot, ds_stage, ds_nopl, ds_krnl500_g) %>%
  filter(!is.na(ds_krnl500_g))

#~~~~~~~~~~~~~~~~~~~
# Write to tidy folder ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

write_csv(tbio, "../_data/tidy/td-biomass.csv")
write_csv(lai, "../_data/tidy/td-lai.csv")
write_csv(kw, "../_data/tidy/td-500krnlwt.csv")
