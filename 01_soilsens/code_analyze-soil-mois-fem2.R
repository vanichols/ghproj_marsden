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

# The general format of a gam() call is gam(y ~ s(x1, by = x2, k = 15) + x2, data = data), 
# where x terms within s() control the shape of the wiggly curve(s), 
# x terms outside of s() control the intercept(s) (if desired), and k controls the number of knots. 
# Also, I think you need x1 to be continuous and x2 to be a factor/category, but I admittedly had trouble figuring this out...

library(mgcv)

#--try just doing 2018 first, for 15 cm depth, on unscaled values
#--I have about 170 days of data. How many knots can I get away with?

#--need separate data frame for predicting

# XXXX 15cm, 2018 --------------------------------------------------

# 15cm, 2018, raw values --------------------------------------------------

sw_d15y18 <- 
  sw_f %>% 
  filter(year == 2018, sensor_depth_cm == 15, !is.na(value))

## Looking at the data by plot_id
ggplot(data = sw_d15y18, aes(x = doy, y = value, color = plot_id)) + 
  geom_point()

## Looking at the data by trt
ggplot(data = sw_d15y18, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point()

## Looking at data by plot_id and rot_trt
ggplot(data = sw_d15y18, aes(x = doy, y = value, color = plot_id)) +
  facet_wrap(~rot_trt) + 
  geom_point()

## A question arises here because 2018_44 is very different from the rest
## The default number of knots was 70, that is too high
mod_v0 <- mgcv::gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
           data = sw_d15y18, method = "REML")
mod_v1 <- mgcv::gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt,
                    data = sw_d15y18, method = "REML")
nlraa::IC_tab(mod_v0, mod_v1, criteria = "BIC")

mod_v <- mgcv::gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt,
                    data = sw_d15y18, method = "REML")

mod_v_prd <- predict(mod_v, se.fit = TRUE)
sw_d15y18A <- cbind(sw_d15y18, Estimate = mod_v_prd$fit, 
                    Q2.5 = mod_v_prd$fit - 1.96 * mod_v_prd$se.fit,
                    Q97.5 = mod_v_prd$fit + 1.96 * mod_v_prd$se.fit)

## GAM fit with 95% confidence bands
ggplot(data = sw_d15y18A, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3)
  
## How does the model change when we incorporate the random effect of block?
mod_v2 <- mgcv::gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt + s(block, bs = "re"),
                   data = sw_d15y18, method = "REML")

mod_v2_prd <- predict(mod_v2, se.fit = TRUE, exclude = "s(block)")
sw_d15y18A2 <- cbind(sw_d15y18, Estimate = mod_v2_prd$fit, 
                    Q2.5 = mod_v2_prd$fit - 1.96 * mod_v2_prd$se.fit,
                    Q97.5 = mod_v2_prd$fit + 1.96 * mod_v2_prd$se.fit)

ggplot(data = sw_d15y18A2, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3)

## What does a GAMM model look like?
mod_v3 <- mgcv::gamm(value ~ s(doy, by = rot_trt, bs = "cr", k = 35) + rot_trt,
                     random = list(block = ~ 1),
                     data = sw_d15y18, method = "REML")

mod_v3_prd <- predict(mod_v3$gam, se.fit = TRUE)
sw_d15y18A3 <- cbind(sw_d15y18, Estimate = mod_v3_prd$fit, 
                     Q2.5 = mod_v3_prd$fit - 1.96 * mod_v3_prd$se.fit,
                     Q97.5 = mod_v3_prd$fit + 1.96 * mod_v3_prd$se.fit)

ggplot(data = sw_d15y18A3, aes(x = doy, y = value, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3)

## computing differences and p-values
diffs <- do.call(rbind, lapply(lapply(139:273, function (x) contrast(emmeans(mod_v, ~rot_trt, at = list(doy = x)), "pairwise")), broom::tidy))
diffsA <- bind_cols(doy = 139:273, diffs)
diffsA$sign <- ifelse(diffsA$p.value < 0.05, "yes", "no")

## differences
ggplot(data = diffsA, aes(x = doy, y = estimate)) + 
  geom_point()

## difference p-value
ggplot(data = diffsA, aes(x = doy, y = p.value)) + 
  geom_point() + 
  geom_hline(yintercept = 0.05) + 
  geom_tile(aes(x = doy, y = 0.85, height = 0.05, fill = sign)) 

#### FULL MODEL ####

