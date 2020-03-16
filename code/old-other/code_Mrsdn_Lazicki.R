#############################
##
## Feb 2 2018
## Look at Lazicki's Data
## 
##
##############################


##

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files
library(writexl) # used to write Excel files


##### Set working directory to wherever this file is kept #####
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))


## Read in data file
lz <- read.csv("../_data_raw/dat_Lazicki.csv", header=TRUE, na.strings = ".")


lz_sys <- lz%>% select(block, system, depth, crop, date, WFPS) %>% group_by(system, depth, date) %>% summarise(mWFPS = mean(WFPS))
lz_sys$system <- as.factor(lz_sys$system)

ggplot(lz_sys, aes(x=date, y = mWFPS, fill = system)) +
  geom_col(position = "dodge", aes(color = "black")) + 
  facet_wrap(~depth) + 
  scale_fill_manual(values = c("white", "lightgreen", "forestgreen"))


## Look at variance in root values w/in block
## Note date '4' is late season sampling, date '3' is earlier, Pat recommends using date 3

lz %>% select(block, system, depth, crop, date, lengthpcm3soil) %>%
  filter(crop == "C", !is.na(lengthpcm3soil),
         date == 3) %>%
  group_by(system, depth) %>%
  summarise(rng1 = range(lengthpcm3soil)[1],
            rng2 = range(lengthpcm3soil)[2],
            sd = sd(lengthpcm3soil),
            mn = mean(lengthpcm3soil)) %>%
  ggplot(aes(x=system, color = as.factor(system))) +
  geom_point(aes(y = sd), color = "black") + 
  #geom_point(aes(y = rng1), size = 2) + 
  #geom_point(aes(y = rng2), size = 2) + 
  facet_grid(depth~.)
