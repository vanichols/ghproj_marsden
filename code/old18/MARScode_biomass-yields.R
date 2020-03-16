#########################
#
# Date of creation: 
# Nov 28 2018
# Date last modified: 
# Nov 28 2018

# Author: Gina
# Purpose: See how biomass and yield data compare

# Inputs: td-biomass.csv
#         td-corn-yield.csv

# Outputs: 
#

# NOTES: 
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

#~~~~~~~~~~~~~~~~~~~
# Read in data ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

pk <- read_csv("../_data/tidy/td-plot-trt-key.csv")
bm <- read_csv("../_data/tidy/td-biomass.csv")

yld <- read_csv("../_data/tidy/td-corn-yield.csv") %>%left_join(pk)
  


#~~~~~~~~~~~~~~~~~~~
# Fig ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~

ggplot(dat, aes(doy, tbm_Mgha)) + 
  geom_jitter(size = 2, aes(color = trt)) + 
  facet_grid(~rep)

  