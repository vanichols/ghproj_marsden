#########################
##
## Date of creation: June 11 2019
## Date last modified: June 11 2019
##                     Oct 31 2019 this is a mess....
##
## Author: Gina
## Purpose: Process penetrometer data into one file
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

# Data from first sampling June 11 2019 ------------------------------------------------

# uh oh. it's not here...

sm <- read_excel("_theme-2018data/_data/raw_entered/rd_mars-soil_penetrometer.xlsx", skip = 5)


olk <- read_excel("_theme-2018data/_data/raw_entered/rd_mars-soil_penetrometer-OLK.xlsx",
                  skip = 5) %>%
  # remove 'Logger Started' rows
  filter(Number != 'Logger Started') %>%
  
  # remove XX measurements
  filter(Number != 'XX') %>%
  
  # Make and N column that matches sm data
  mutate(N = str_trim( (str_sub(Number, -2, -1))),
         N = paste("N", N, sep = ""),
         N = ifelse(soilpair_YN == "N", paste("X", N, sep = ""), N)) %>%
  
  # Make samp_ID that matches sm data
  mutate(samp_ID = paste(block, trt, N, sep = "-")) %>%
  
  # gather depths into one col
  gather(depth_00:depth_18, key = "depth_in", value = "resis_psi") %>%
  
  select(-Number, -`Cone Size`) %>%
  # sort that shit
  arrange(samp_ID, depth_in)

# Merge soil moisture + olk penetrometer
# NOTE: I think the depths are off. What it thinks is 1, is actually 0.  But whatever. 
#~~~~~~~~~~~~~~~~~~~~~~~~

dat <- olk %>% left_join(sm, by = c("samp_ID", "block", "trt", "N", "soilpair_YN")) %>%
  
  # Change depth_in to numeric inches
  mutate(depth_in = as.numeric(str_sub(depth_in, -2, -1)),
         depth_cm = depth_in * 2.54)

poke <- read_excel("_theme-2018data/_data/raw_entered/rd_mars-soil_penetrometer-Spctrm-Gina-07-16-18.xlsx", skip = 5)

  
  
# Second sampling July 16 2019 --------------------------------------------

poke %>%   
  # remove 'Logger Started' rows
  filter(Number != 'Logger Started') %>%
  
  # remove XX measurements
  filter(Number != 'XX') %>%
  
  # Make an N column
  mutate(N = str_trim( (str_sub(Number, -2, -1))),
         N = paste("N", N, sep = "")) %>%
  
  # Make samp_ID and rep column
  mutate(samp_ID = paste(plot, trt, N, sep = "-"),
         rep = plot %/% 10) %>%
  
  # gather depths into one col
  gather(depth_00.0:depth_45.0, key = "depth_cm", value = "resis_KPa") %>%
  
  select(-Number, -`Cone Size`) %>%
  # sort that shit
  arrange(samp_ID, depth_cm) %>%
  
  # Change depth_in to numeric cm
  mutate(depth_cm = as.numeric(str_sub(depth_cm, -4, -1)))

poke


# Combine them, I hate you Gina -------------------------------------------

# Read in key
#~~~~~~~~~~

mykey <- read_csv("_theme-2018data/_data/tidy/td-plot-trt-key.csv") %>%
  mutate(block = paste0("B", block))


# Get them into the same format
#~~~~~~~~~~
dat2 <- 
  dat %>%
  left_join(mykey) %>%
  mutate(date = as_date("2018-05-10"), 
         samp_doy = yday(date),
         resis_kPa = resis_psi * 6.89476) %>%
    select(year, date, samp_doy, plot, block, trt, soilpair_YN,
           soilmois_g.g,
           samp_ID, depth_cm, resis_kPa)
    
poke2 <- 
  poke %>%
  left_join(mykey) %>%
  mutate(samp_ID = paste(block, trt, N, sep = "-"),
         date = as_date("2018-07-16"),
         samp_doy = yday(date), 
         soilpair_YN = 'N',
         soilmois_g.g = NA) %>%
  rename("resis_kPa" = resis_KPa) %>%
  select(year, date, samp_doy, plot, block, trt, soilpair_YN,
         soilmois_g.g,
         samp_ID, depth_cm, resis_kPa)


pen <- bind_rows(dat2, poke2)  


write_csv(pen, "../_data/tidy/td-penetrometer.csv")
