#############################
##
## Author: Gina
## Created: April 2 2018
## Purpose: Look at texture data from 2009 vs 2014
## Inputs: dat_texture2009 (from Matt)
##         dat_texture2014 (from Hanna)
##
##############################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)

####==SETS WORKING DIRECTORY TO WHEREVER ACTIVE DOCUMENT IS KEPT===####
#
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

####==================####
##   Read in data
####==================####

## Read in data file
## Make new columns: text2009 and text2014

t1 <- read_csv("../_data_raw/dat_texture2009.csv", skip = 5) %>% 
  select(-silt.coarse_prct, -silt.fine_prct, -silt.clay_prct) %>%
  gather(5:7, key = "text", value = "prct2009") %>%
  mutate(depth2009_cm = "0-20")


t2 <- read_csv("../_data_raw/dat_texture2014.csv", skip = 0) %>%
  select(rot, block, plot, depth_cm, sand_prct, silt_prct, clay_prct) %>%
  gather(sand_prct, silt_prct, clay_prct, key = "text", value = "prct2014") %>%
  filter(depth_cm == "0-15") %>%
  rename(depth2014_cm = depth_cm)

## Merge to compare plot textures in 2009 and 2014
dt <- t1 %>% right_join(t2, by = c("plot", "block", "rot", "text")) %>%
  gather(prct2009, prct2014, key = "meas_year", value = "prct")

ggplot(dt, aes(text, prct, color = as.factor(rot), shape = as.factor(block))) + 
  geom_point(size = 4) + 
  facet_grid(.~meas_year)

# What is the range in textures observed in 2009?
ggplot(t1, aes(text, prct2009, color = as.factor(rot), shape = as.factor(block))) + 
  geom_point(size = 4) + 
  annotate(geom = "text", x = 1, y = 20, label = "Range: 21-33.5%", fontface = "italic") +
  annotate(geom = "text", x = 2, y = 20, label = "Range: 22-45.5%", fontface = "italic") +
  annotate(geom = "text", x = 3, y = 20, label = "Range: 30.5-46%", fontface = "italic") +
  labs(x = " ", y = "Percent [%]") +
  ggtitle("Range in Marsden soil textures 0-20cm\nSamples collected 2009")

# What is the range in textures observed in 2009?
ggplot(t1, aes(text, prct2009, fill = as.factor(block))) + 
  geom_boxplot(size = 1) + 
  annotate(geom = "text", x = 1, y = 20, label = "Range: 21-33.5%", fontface = "italic") +
  annotate(geom = "text", x = 2, y = 20, label = "Range: 22-45.5%", fontface = "italic") +
  annotate(geom = "text", x = 3, y = 20, label = "Range: 30.5-46%", fontface = "italic") +
  labs(x = " ", y = "Percent [%]") +
  ggtitle("Range in Marsden soil textures 0-20cm\nSamples collected 2009")


# What is the range in textures observed in 2009?
ggplot(t1, aes(text, prct2009, fill = as.factor(rot))) + 
  geom_boxplot(size = 1) + 
  annotate(geom = "text", x = 1, y = 20, label = "Range: 21-33.5%", fontface = "italic") +
  annotate(geom = "text", x = 2, y = 20, label = "Range: 22-45.5%", fontface = "italic") +
  annotate(geom = "text", x = 3, y = 20, label = "Range: 30.5-46%", fontface = "italic") +
  labs(x = " ", y = "Percent [%]") +
  ggtitle("Range in Marsden soil textures 0-20cm\nSamples collected 2009")


t3 <- read_csv("../_data_raw/dat_texture2014.csv", skip = 0) %>%
  select(rot, block, plot, depth_cm, sand_prct, silt_prct, clay_prct) %>%
  gather(sand_prct, silt_prct, clay_prct, key = "text", value = "prct2014") %>%
#  filter(depth_cm == "0-15") %>%
  rename(depth2014_cm = depth_cm)

# What is the range in textures observed in 2014 with Hanna's samples?
ggplot(filter(t3, text == "sand_prct", !is.na(block)), aes(text, prct2014, color = as.factor(rot))) + 
  geom_point(size = 4, aes(shape = as.factor(block))) + 
  #annotate(geom = "text", x = 1, y = 20, label = "Range: 25-41%", fontface = "italic") +
  #annotate(geom = "text", x = 2, y = 20, label = "Range: 24-44%", fontface = "italic") +
  #annotate(geom = "text", x = 3, y = 20, label = "Range: 17-37%", fontface = "italic") +
  labs(x = " ", y = "Percent [%]") +
  facet_grid(depth2014_cm~block) +
  ggtitle("Range in Marsden soil textures 0-100cm\nSamples collected 2014 from corn plots") 
