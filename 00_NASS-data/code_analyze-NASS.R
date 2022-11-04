#--how much land is dedicated to corn/soybean?

library(tidyverse)


d <- 
  read_csv("00_NASS-data/dat_NASS-raw.csv") %>% 
  janitor::clean_names()

d1 <- 
  d %>% 
  filter(program == "CENSUS") %>% 
  select(year, period, state, commodity:value) %>% 
  filter(grepl("ACRES HARVESTED", data_item),
         value != "(D)") %>% 
  mutate(value = parse_number(value))

d1
