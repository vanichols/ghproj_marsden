#########################
#
# Date of creation: Nov 28 2018
# Date last modified: Nov 28 2018
#                     Feb 4 2019, updated
#
# Author: Gina
# Purpose: Process destructive data
#
# Inputs: rd_mars-residue
#    
# Outputs: td-residue 
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

mitch <- read_excel("../_data/raw_entered/rd_mars-residue.xlsx",
                    skip = 5) %>%
  mutate(date = as_date(date)) %>%
  left_join(mykey) %>%
  mutate(residue_g = residueandbag_g - drybag_g) %>%
  select(date, trt, block, plot, residue_g)

#~~~~~~~~~~~~~~~~~~~
# Write to tidy folder ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

write_csv(mitch, "../_data/tidy/td-residue.csv")
