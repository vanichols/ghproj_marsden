#############################
##
## Feb 25 2018
## Process Will's data 
## Writes file to _data_raw
## dat_crngrwth_2013-14v2.csv
##
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)

####==SETS WORKING DIRECTORY TO WHEREVER ACTIVE DOCUMENT IS KEPT===####
#
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

####========2013 Data=== ==========####
##

# Start w/ 2013
w13 <- read.csv("../data_Corn2013_WO.csv", skip=2, header=TRUE)

# Separate his cols by mass (4-8), whole plant (9-12), leaf (13-16)
#  stalk (17-20), cob+tassle(21-24),  grain (25-28)
# Rename if necssary

y13 <- w13  %>%
  mutate(date = as.character(Date), 
         lubedate = mdy(date), 
         year = year(lubedate), 
         doy = yday(lubedate)) %>%
  select(year, doy, everything(), -lubedate, -Date, -date)
  
m13 <- y13 %>% select(1:9) %>% 
  rename("plant" = !!names(.[5]),
         "leaf" = !!names(.[6]),
         "stalk" = !!names(.[7]),
         "cobtass" = !!names(.[8]),
         "grain" = !!names(.[9])) %>%
  gather(organ, mass_g, -year, -doy, -System, -Plot)
  

p13 <- y13 %>% select(1:4, 10:13) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "plant")

l13 <- y13 %>% select(1:4, 14:17) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "leaf")

s13 <- y13 %>% select(1:4, 18:21) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "stalk")

ct13 <- y13 %>% select(1:4, 22:25) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "cobtassle")

g13 <- y13 %>% select(1:4, 26:29) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "grain")

CN13 <- bind_rows(p13, l13, s13, ct13, g13)
d13 <- CN13 %>% left_join(m13, 
                          by = c("year", 
                                 "doy", 
                                 "Plot", 
                                 "System",
                                  "organ")) %>%
  select(year, doy, Plot, System, organ, mass_g, everything())


####============2014 data==============####
##
w14 <- read.csv("../data_Corn2014_WO.csv", skip=2, header=TRUE)

# Separate his cols by mass (4-8), whole plant (9-12), leaf (13-16)
#  stalk (17-20), cob+tassle(21-24),  grain (25-28)
# Rename if necssary

y14 <- w14  %>%
  mutate(date = as.character(Date), 
         lubedate = mdy(date), 
         year = year(lubedate), 
         doy = yday(lubedate)) %>%
  select(year, doy, everything(), -lubedate, -Date, -date)

m14 <- y14 %>% select(1:9) %>% 
  rename("plant" = !!names(.[5]),
         "leaf" = !!names(.[6]),
         "stalk" = !!names(.[7]),
         "cobtass" = !!names(.[8]),
         "grain" = !!names(.[9])) %>%
  gather(organ, mass_g, -year, -doy, -System, -Plot)


p14 <- y14 %>% select(1:4, 10:13) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "plant")

l14 <- y14 %>% select(1:4, 14:17) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "leaf")

s14 <- y14 %>% select(1:4, 18:21) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "stalk")

ct14 <- y14 %>% select(1:4, 22:25) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "cobtassle")

g14 <- y14 %>% select(1:4, 26:29) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "grain")

CN14 <- bind_rows(p14, l14, s14, ct14, g14)
d14 <- CN14 %>% left_join(m14, 
                          by = c("year", 
                                 "doy", 
                                 "Plot", 
                                 "System",
                                 "organ")) %>%
  select(year, doy, Plot, System, organ, mass_g, everything())

####===========Put together and write to dat_raw=========#######
dat <- bind_rows(d13, d14) %>%
  filter(!is.na(year))

write.csv(dat, 
          file = paste("../_data_raw/dat_crngrwth_2013-14v2.csv", sep = ""), row.names = FALSE) 

