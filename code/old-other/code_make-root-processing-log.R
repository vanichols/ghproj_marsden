# created feb 14 2020
# make a sheet where I can keep track of where samples are in the whole process
# also will use sheet to print labels
# modified: feb 29 2020 (added June 17 sampling of alflafa)

library(tidyverse)
library(readxl)
library(lubridate)


# template ----------------------------------------------------------------

alf0 <- read_excel("_theme-2019roots/_data/templates/template-fresh-wt.xlsx", sheet = 1)
alf1 <- read_excel("_theme-2019roots/_data/templates/template-fresh-wt.xlsx", sheet = 2)
alf2 <- read_excel("_theme-2019roots/_data/templates/template-fresh-wt.xlsx", sheet = 3)
alf3 <- read_excel("_theme-2019roots/_data/templates/template-fresh-wt.xlsx", sheet = 4)

crn1 <- read_excel("_theme-2019roots/_data/templates/template-fresh-wt.xlsx", sheet = 5)
crn2 <- read_excel("_theme-2019roots/_data/templates/template-fresh-wt.xlsx", sheet = 6)

oat1 <- read_excel("_theme-2019roots/_data/templates/template-fresh-wt.xlsx", sheet = 7)

clean <- alf0 %>% 
  bind_rows(alf1, alf2, alf3, crn1, crn2, oat1) %>% 
  select(site, crop, trt, samp_date, plot, depth_cm) %>% 
  #--create an official sample id
  mutate(depth_cm = as_factor(depth_cm)) %>% 
  arrange(crop, trt, samp_date, plot, depth_cm) %>% 
  mutate(samp_date = as_date(samp_date),
         month = month(samp_date, label = T),
         crop_abb = str_sub(crop, 1, 3)) %>% 
  group_by(crop, trt, samp_date, plot) %>% 
  mutate(depth_cat = 1:n()) %>% 
  ungroup() %>% 
  mutate(samp_id = paste(crop_abb, month, paste0("p", plot), depth_cat, sep = "-")) %>% 
  select(site, crop, trt, samp_date, plot, depth_cm, samp_id)

write_csv(clean, "_theme-2019roots/_data/templates/temp_samp-ids2.csv")
