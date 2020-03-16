#############################
##
## Feb 20 2018
## Look at Marsden Farm weather (from https://mesonet.agron.iastate.edu/request/coop/fe.phtml)
## Compare to 2003-2017 yields (corn)
##
## INPUTS: _data/_raw/rd_cornyld_2003-17.csv
##                    rd_cornyld_2018.csv
##
## OUPUTS: _data/_tidy/td_corn-yields.csv
##

##############################


##

rm(list=ls())
library(tidyverse)
library(lubridate)
library(here)

# Read files --------------------------------------------------------------

cyld <- read_csv("_data/_raw/rd_cornyld_2003-17.csv", skip = 4) %>%
  rename(yld_buac = bu_ac,
         yld_Mgha = Mg_ha)
cyld18 <- read_csv("_data/_raw/rd_cornyld-2018.csv")


# wrangle -----------------------------------------------------------------

cyld18 %>%
  fill(Plot, Rotation) %>%
  rename(trt = Rotation,
         plot = Plot) %>%
  select(plot, trt, Treatment, yld_buac, yld_Mgha155mois) %>%
  mutate(yld_Mgha = yld_Mgha155mois * (1 - 0.155)) %>%
  group_by(trt, plot) %>%
  summarise_if(is.numeric, mean) %>%
  select(-yld_Mgha155mois) %>%
  mutate(year = 2018,
         block = as.numeric(str_sub(plot, 1, 1))) %>%
  select(year, trt, block, yld_buac, yld_Mgha) %>%
  bind_rows(cyld) %>%
  write_csv("_data/_tidy/td_corn-yields.csv")  
