#########################
##
## Date of creation: May 28 2018
## Date last modified: May 28 2018
##                      Feb 5 2019 (updating)
##
## Author: Gina
## Purpose: Process rooting depth data
##          
## Inputs: rd_mars-rootdepth.xlsx
##         td-plot-trt-key
##
## Outputs: td_rootdepth
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
# Read data ---------------------------------------------------------------
##=========================================##

mykey <- read_csv("../_data/tidy/td-plot-trt-key.csv") %>%
  mutate(block = paste0("B", block))

roo <- read_xlsx("../_data/raw_entered/rd_mars-rootdepth.xlsx", skip = 5, na = "NA") %>%
  mutate(block = paste0("B", block)) 

##=========================================##
# Wrangle data ---------------------------------------------------------------
##=========================================##

dat <- 
  roo %>%
  left_join(mykey) %>%
  mutate(rootdepth_cm = rootdepth_in * 2.54,
         date = as_date(date)) %>%
  select(date, trt, stage, block, plot, subrep, rootdepth_cm)

##=========================================##
# Writee data ---------------------------------------------------------------
##=========================================##

write_csv(dat, "../_data/tidy/td-rootdepth.csv")
