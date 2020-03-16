#########################
##
## Date of creation: July 8 2019
## Date last modified: 
##
## Author: Gina
## Purpose: Take tidy data and combine it
##          
## Inputs: td_lai-bm, td_maxroot, td_phen
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
library(fuzzyjoin) #--to do fuzzy joining of dates
library(here)

setwd(here())

#--read in data

lb <- read_csv("_theme-2019data/_data/tidy/td_lai-bm.csv")
mr <- read_csv("_theme-2019data/_data/tidy/td_maxroot.csv")
phen <- read_csv("_theme-2019data/_data/tidy/td_phen.csv")


# get all msmts taken within a few days of each other merged --------------

#--add 1 day window around phenology

phenfuzz <- 
  phen %>%
  mutate(date1 = date - days(1),
         date2 = date + days(1)) %>%
  select(date1, date2, plot, stage)


dat <- 
  lb %>% 
  fuzzy_left_join(phenfuzz, 
                  by = c("plot" = "plot",
                         "date" = "date1", 
                         "date" = "date2"),
                  match_fun = list(`==`, `>=`, `<=`)) %>%
  rename(plot = plot.x) %>%
  fuzzy_left_join(mr, 
                  by = c("plot" = "plot",
                         "date1" = "date", 
                         "date2" = "date"),
                  match_fun = list(`==`, `<=`, `>=`)) %>%
  rename(plot = plot.x, date = date.x) %>%
  #left_join(mrdraw, by = c("date", "plot")) %>%
  select(date1, date2, plot, stage, tot_g_m2, tot_g_pl, LAI_cm2_pl, LAI_m2_m2, maxrootdepth_cm) %>%
  mutate(date1 = ymd(date1),
         date2 = ymd(date2))


# write it ----------------------------------------------------------------

dat %>%
  write_csv("_theme-2019data/_data/tidy/td_phen-lai-bm-maxr.csv")

