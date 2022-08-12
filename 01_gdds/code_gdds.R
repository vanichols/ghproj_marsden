# Gina
# calculate gdds for each day
# 8/12/2022


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)
#library(GinaBooty)

mrs_wea
mrs_phen
mrs_cornplant



gdds_for_ref <- 
  read_csv("01_rootdepth/td_rootdepth-elev-wea.csv") %>%
  select(year, date, doy, cum_gdd) %>% 
  distinct()



cum_gdds <- 
  mrs_wea %>% 
  left_join(mrs_cornplant %>% 
              select(year, plant_doy),
            by = c("year")) %>% 
  filter(!is.na(plant_doy),
         day >= plant_doy) %>% 
  #--calculate GDDs
  mutate(
    maxt_c30 = ifelse(maxt_c > 30, 30, maxt_c),
    mint_c0 = ifelse(mint_c < 0, 0, mint_c),
        avgt_c = (maxt_c30 + mint_c)/2,
        gdds_raw = (avgt_c - 10),
    gdds = ifelse(gdds_raw < 0 , 0, gdds_raw)
) %>% 
  group_by(year) %>% 
  mutate(cum_gdds = cumsum(gdds)) %>% 
  select(year, date, day, cum_gdds) %>% 
  rename(doy = day)

cum_gdds

#--they don't quite line up, but they are close enough
gdds_for_ref %>% 
  left_join(cum_gdds) %>% 
  ggplot(aes(cum_gdd, cum_gdds)) + 
  geom_point()
  

cum_gdds %>% 
  write_csv("01_gdds/td_gdds.csv")
