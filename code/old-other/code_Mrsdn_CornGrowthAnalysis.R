#############################
##
# Created: January 29 2018
# Last edited: Jan 25 2019
#
# Purpose: Look at Will's growth data
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
library(patchwork)
library(here)
library(nlme)

library(nlraa) #--something Fernando uses w/a self-starting function

setwd(here())


# Wrangle -----------------------------------------------------------------

# Will's data

dwo <- 
  read_csv("_data/_raw/rd_crngrwth_2013-14v2.csv") %>%
  mutate(System = as.character(System),
         trt = recode(System,
                      "4" = "R4",
                      "3" = "R3",
                      "2" = "R2"),
         crop = "corn",
         block = str_sub(as.character(Plot), 1, 1)) %>%
  rename(plot = Plot) %>%
  select(year, block, plot, trt, crop, organ, everything(), -System)
  
# Weather data from Matt, not sure where he got it
wea <- 
  read_csv("_data/_raw/rd_wthr_1987-2017_Marsden.csv") %>%
  mutate(doy = yday(day),
         year = year(day)) %>%
  filter(year %in% c(2013, 2014)) %>%
  mutate(avgc = (highc + lowc)/2,
         gdd_c = if_else(avgc > 30, 30, 
                         if_else(avgc < 0, 0, avgc))) %>%
  select(year, doy, highc, lowc, avgc, gdd_c, precipmm, narr_srad)

# Look at it --------------------------------------------------------------

dwo %>%
  filter(organ == "plant") %>%
  ggplot(aes(doy, mass_g)) + 
  geom_point() +
  geom_line(aes(group = plot, color = trt)) + 
  facet_grid(~year)

wea %>%
  arrange(year, doy) %>%
  group_by(year) %>%
  mutate(cgdd_c = cumsum(gdd_c)) %>%
  ggplot(aes(doy, cgdd_c)) + 
geom_line(aes(color = factor(year)))


# Combine -----------------------------------------------------------------

dat <- 
  wea %>%
  arrange(year, doy) %>%
  group_by(year) %>%
  mutate(cgdd_c = cumsum(gdd_c)) %>%
  select(year, doy, cgdd_c) %>%
  right_join(dwo)


# Look at it again --------------------------------------------------------

dat %>%
  ggplot(aes(doy, mass_g)) + 
  geom_line(aes(group = interaction(trt, organ, year),
                color = trt)) + 
  facet_grid(organ~block, scales = "free")


# Make a beta function ----------------------------------------------------

# note: te > tm
MyBeta <- function(wmax, te, tm, t) {
  exp1 <- (te - t) / (te - tm)
  exp2 <- (t / te)
  exp3 <- (te / (te - tm))
  
  y <- wmax * ( 1 + exp1) * (exp2 ^ exp3)
  
  return(y)
}


# See what would make it work ---------------------------------------------

# It's hard to find values to make it work....
dat %>%
  filter(year == 2013,
         organ == "plant",
         trt == "R2") %>%
  mutate(betay = MyBeta(wmax = 800, te = 250, tm = 200, t = doy)) %>%
  ggplot(aes(doy, mass_g)) + 
  geom_line() + 
  geom_line(aes(doy, betay))


tibble(doy = seq(160, 300, 10),
       mass_g = MyBeta(wmax = 800, te = 250, tm = 200, t = doy)) %>%
  ggplot(aes(doy, mass_g)) + 
  geom_line()

# From Fernando - it doesn't like it. 
#install.packages("nlraa", repos="http://R-Forge.R-project.org")
fit.lis <- nlsList(Yield ~ SSbgf(DOY, w.max, t.e, t.m), data = smG)

tmp <- dat %>%
  filter(year == 2013,
         organ == "plant") %>%
  unite(block, trt, col = "eu")

tmp2 <- groupedData(mass_g ~ doy | eu, data = tmp)

fit.lis <- nlsList(mass_g ~ SSbgf(doy, w.max, t.e, t.m), data = tmp2)

fit.lis2 <- nlsList(Yield ~ bgf2(DOY, w.max, w.b = 0, t.e, t.m, t.b = 141),
                      data = smG,
                      start = c(w.max = 30, t.e=280, t.m=240))

fit.lis2 <- nlsList(mass_g ~ bgf2(doy, w.max, w.b = 0, t.e, t.m, t.b = 150),
                    data = tmp2,
                    start = c(w.max = 30, t.e=280, t.m=240))
