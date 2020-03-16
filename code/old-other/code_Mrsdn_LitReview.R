#############################
##
# Created: March 8 2019
# Last edited: 
#
# Purpose: Make pie char of Teasdale and Cavigelli (2017) variance decomposition
#
# Inputs:
#
# Outputs:
#
#
# Notes:  
#
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(here)


# make fake data ----------------------------------------------------------

mysources <- 
c("Weather", 
  "System (Org/Conv)", 
  "Sub-system (tillage/rot length)", 
  "Block", 
  "Other")

dat <- tibble(source = mysources,
              var = c(74, 16, 1, 1, 8)) %>%
  mutate(source = as_factor(source, levels = mysources))

# pie chart ---------------------------------------------------------------

ggplot(dat, aes(x = "", y = var, fill=source))+
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(var, "%")), position = position_stack(vjust = 0.5)) +
  
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "gray80")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Phones - Market Share") +
  theme_classic() + 
  theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
