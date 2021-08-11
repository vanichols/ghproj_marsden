# Created 5/9/2021
# Purpose: Analyze soil moisture data
# Updated: 5/11/2021, Walk Miranda through problem, incorporate her suggestions


# My questions:
# Should I scale the soil moisture data?
# The values are between 0-1, do I need to account for that somehow?
# How to best incorporate block and year? 
# Analyze depths separately?

## Source a couple of files
source("https://raw.githubusercontent.com/vanichols/saapsim/master/R/dateconvs.R")

#--my package with the data
#remotes::install_github("vanichols/maRsden")
library(maRsden)

library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(emmeans)

#--scale and center
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#--scale from 0-1
scale_this2 <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm=TRUE) - min(x, na.rm = T))
}

#################---fem walking me through it---#################################

# data --------------------------------------------------------------------

#--I have 2 years of data
#--each year, there are 4 blocks, each with 2 treatments, and there are 2 depths

#--keep data before Oct 1, only
saf_doy_to_date(300)
saf_date_to_doy("2001-10-01")

rawdat <- 
  mrs_soilsensors %>% 
  filter(sensor_unit == "soilVWC") %>% 
  left_join(mrs_plotkey) %>%
  select(year, doy, block, rot_trt, plot_id, sensor_depth_cm, value) %>% 
  arrange(block, plot_id, sensor_depth_cm) %>% 
  group_by(plot_id, sensor_depth_cm) %>% 
  mutate(value_sc = scale_this(value),
         value_sc2 = scale_this2(value),
         sqrt_value = sqrt(value),
         sqrt_scvalue2 = sqrt(value_sc2)) %>% 
  filter(doy < 274) %>% 
  filter(!(plot_id == "2019_27" & doy < 185)) %>% 
  filter(plot_id != "2019_35")  %>% #--this sensor never really worked
  ungroup()

ggplot(data = rawdat) + 
  geom_line(aes(x = doy, y = value, group = plot_id, color = rot_trt)) +
  facet_grid(sensor_depth_cm ~ year) + 
  labs(title = "raw sensor values")

  
rawdat %>% 
  group_by(doy, year, sensor_depth_cm, rot_trt) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  ggplot(aes(doy, value)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(sensor_depth_cm~year)

# calc differences by block -----------------------------------------------

#--wait, I'm missing some plots, so not every block would have a comparison...
#--Miranda says do it by trt

#--make factors
sw_f <- 
  rawdat %>% 
  mutate(
    plot_id = as.factor(plot_id),
    block = as.factor(block),
    rot_trt = as.factor(rot_trt)
  )

#--should I scale things?
sw_f %>% 
  select(-starts_with("sqrt")) %>% 
  pivot_longer(value:value_sc2) %>% 
  filter(sensor_depth_cm == 15) %>% 
  ggplot() + 
  geom_line(aes(x = doy, y = value, group = plot_id, color = rot_trt)) +
  facet_wrap( ~ year + name, scales = "free", ncol = 3)

# fit gams ----------------------------------------------------------------
# 15cm, 2018, raw values --------------------------------------------------

sw_d15_y18 <- 
  sw_f %>% 
  filter(year == 2018, sensor_depth_cm == 15, !is.na(value))

## Looking at the data by trt
ggplot(data = sw_d15_y18, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point()

## What does a GAMM model look like?
mod_d15_y18 <- mgcv::gamm(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt,
                          random = list(block = ~ 1),
                          data = sw_d15_y18, method = "REML")

mod_d15_y18_prd <- predict(mod_d15_y18$gam, se.fit = TRUE)
sw_d15_y18_prd <- cbind(sw_d15_y18, 
                        Estimate = mod_d15_y18_prd$fit, 
                        Q2.5 = mod_d15_y18_prd$fit - 1.96 * mod_d15_y18_prd$se.fit,
                        Q97.5 = mod_d15_y18_prd$fit + 1.96 * mod_d15_y18_prd$se.fit)


# write 2018 15cm results -------------------------------------------------

sw_d15_y18_res <- 
  sw_d15_y18_prd %>% 
  as_tibble() %>% 
  select(year, doy, block, rot_trt, plot_id, sensor_depth_cm, value, Estimate, Q2.5, Q97.5) %>% 
  mutate(mod_desc = "random effect of block, 35 knots, reml")


# 15cm, 2019, raw values --------------------------------------------------

sw_d15_y19 <- 
  sw_f %>% 
  filter(year == 2019, sensor_depth_cm == 15, !is.na(value))

## Looking at the data by trt
ggplot(data = sw_d15_y19, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point()

## What does a GAMM model look like?
mod_d15_y19 <- mgcv::gamm(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt,
                          random = list(block = ~ 1),
                          data = sw_d15_y19, method = "REML")

mod_d15_y19_prd <- predict(mod_d15_y19$gam, se.fit = TRUE)
sw_d15_y19_prd <- cbind(sw_d15_y19, 
                        Estimate = mod_d15_y19_prd$fit, 
                        Q2.5 = mod_d15_y19_prd$fit - 1.96 * mod_d15_y19_prd$se.fit,
                        Q97.5 = mod_d15_y19_prd$fit + 1.96 * mod_d15_y19_prd$se.fit)


# write 2019 15cm results -------------------------------------------------

sw_d15_y19_res <- 
  sw_d15_y19_prd %>% 
  as_tibble() %>% 
  select(year, doy, block, rot_trt, plot_id, sensor_depth_cm, value, Estimate, Q2.5, Q97.5) %>% 
  mutate(mod_desc = "random effect of block, 35 knots, reml")




# 45cm, 2018, raw values --------------------------------------------------

sw_d45_y18 <- 
  sw_f %>% 
  filter(year == 2018, sensor_depth_cm == 45, !is.na(value))

## Looking at the data by trt
ggplot(data = sw_d45_y18, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point()

## What does a GAMM model look like?
mod_d45_y18 <- mgcv::gamm(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt,
                          random = list(block = ~ 1),
                          data = sw_d45_y18, method = "REML")

mod_d45_y18_prd <- predict(mod_d45_y18$gam, se.fit = TRUE)
sw_d45_y18_prd <- cbind(sw_d45_y18, 
                        Estimate = mod_d45_y18_prd$fit, 
                        Q2.5 = mod_d45_y18_prd$fit - 1.96 * mod_d45_y18_prd$se.fit,
                        Q97.5 = mod_d45_y18_prd$fit + 1.96 * mod_d45_y18_prd$se.fit)


# write 2018 45cm results -------------------------------------------------

sw_d45_y18_res <- 
  sw_d45_y18_prd %>% 
  as_tibble() %>% 
  select(year, doy, block, rot_trt, plot_id, sensor_depth_cm, value, Estimate, Q2.5, Q97.5) %>% 
  mutate(mod_desc = "random effect of block, 35 knots, reml")


# 45cm, 2019, raw values --------------------------------------------------

sw_d45_y19 <- 
  sw_f %>% 
  filter(year == 2019, sensor_depth_cm == 45, !is.na(value))

## Looking at the data by trt
ggplot(data = sw_d45_y19, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point()

## What does a GAMM model look like?
mod_d45_y19 <- mgcv::gamm(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt,
                        random = list(block = ~ 1),
                        data = sw_d45_y19, method = "REML")

mod_d45_y19_prd <- predict(mod_d45_y19$gam, se.fit = TRUE)
sw_d45_y19_prd <- cbind(sw_d45_y19, 
                     Estimate = mod_d45_y19_prd$fit, 
                     Q2.5 = mod_d45_y19_prd$fit - 1.96 * mod_d45_y19_prd$se.fit,
                     Q97.5 = mod_d45_y19_prd$fit + 1.96 * mod_d45_y19_prd$se.fit)


# write 2019 45cm results -------------------------------------------------

sw_d45_y19_res <- 
  sw_d45_y19_prd %>% 
  as_tibble() %>% 
  select(year, doy, block, rot_trt, plot_id, sensor_depth_cm, value, Estimate, Q2.5, Q97.5) %>% 
  mutate(mod_desc = "random effect of block, 35 knots, reml")





# record all results ------------------------------------------------------


sw_d15_y18_res %>% 
  bind_rows(sw_d15_y19_res) %>%
  bind_rows(sw_d45_y18_res) %>% 
  bind_rows(sw_d45_y19_res) %>% 
  readr::write_csv("01_soilsens/dat_soilsens-gam-fem.csv")
