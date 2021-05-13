# Data processing

#--should I scale the soil moisture data?

library(magrittr)
library(maRsden)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(lemon) # chunk option `render = lemon_print` makes tables prettier

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
saapsim::saf_doy_to_date(300)
saapsim::saf_date_to_doy("2001-10-01")

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

mod_v <- gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
           data = sw_d15y18, method = "REML")

plot(mod_v, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod_v)

#To [check a GAM with `gam.check()`](https://rdrr.io/cran/mgcv/man/gam.check.html), we look for a few things:
  
#   - If `edf` is too close to `k'`, we may need more knots
# - `k-index` is the estimate divided by the residual variance. 
# The further below 1 this is, the more likely it is that there is missed pattern left in the residuals.
# - The p-value for the `k-index` is computed by simulation: the residuals are randomly re-shuffled 
# `k.rep` times to obtain the null distribution of the differencing variance estimator, if there is no pattern in the residuals.
# - If the p-value is too close to zero, there is a significant pattern in the residuals that should be addressed.
# 
# In this case, the `edf` values are not too close to 7, and the `k-index` is close to 1, but the p-values are a bit low for my taste. This is likely due to the heteroskedastic residuals (i.e., megaphone shape), because the curves are all close to zero at zero depth but spread out a lot for higher depths.
# 


# 15cm, 2018, sqrt values -------------------------------------------------

#--better p-values
mod_vsq <- gam(sqrt_value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
             data = sw_d15y18, method = "REML")

plot(mod_vsq, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod_vsq)



# 15cm, 2018, scaled vals -------------------------------------------------

#--very bad p values
mod_v2 <- gam(value_sc2 ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
               data = sw_d15y18, method = "REML")

plot(mod_v2, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod_v2)



# 15cm, 2018, sqrt scaled vals -------------------------------------------------

#--still very bad p-values
mod_v2sq <- gam(sqrt_scvalue2 ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
              data = sw_d15y18, method = "REML")

plot(mod_v2sq, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod_v2sq)

# view the 24 fitted curves to visually inspect differences
sw_d15y18$p <- predict(mod15_18)

ggplot(data = sw_d15y18) +
  geom_line(aes(x = doy, y = value_sc2, color = rot_trt, group = plot_id)) +
  geom_line(aes(x = doy, y = p, color = rot_trt, group = plot_id), size = 3) + 
  facet_wrap( ~ year, ncol = 2) 


# 
# ## View residuals by group
# myd$resid <- mod$residuals
# 
# ggplot(data = myd) + 
#   geom_line(aes(x = depth_cm, y = resid, group = rep_id, color = year_doy)) +
#   facet_wrap( ~ trt_block_yr, ncol = length(unique(myd$year_doy)))
# ## View data with fitted curves by group
# 
# ggplot(data = myd) + 
#   geom_line(aes(x = depth_cm, y = resis_Mpa, group = rep_id, color = year_doy)) +
#   geom_line(aes(x = depth_cm, y = p^2, group = rep_id), color = "black") + # note p^2
#   facet_wrap( ~ trt_block_yr, ncol = length(unique(myd$year_doy)))



# 15cm, 2018 diffs ------------------------------------------

#To test for differences between treatments, 
# we'll use the following function from [https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/](https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/):

smooth_diff <- function(model, newdata, f1, f2, var, alpha = 0.05,
                        unconditional = FALSE) {
    xp <- predict(model, newdata = newdata, type = 'lpmatrix')
    c1 <- grepl(f1, colnames(xp))
    c2 <- grepl(f2, colnames(xp))
    r1 <- newdata[[var]] == f1
    r2 <- newdata[[var]] == f2
    ## difference rows of xp for data from comparison
    X <- xp[r1, ] - xp[r2, ]
    ## zero out cols of X related to splines for other lochs
    X[, ! (c1 | c2)] <- 0
    ## zero out the parametric cols
    X[, !grepl('^s\\(', colnames(xp))] <- 0
    dif <- X %*% coef(model)
    se <- sqrt(rowSums((X %*% vcov(model, unconditional = unconditional)) * X))
    crit <- qt(alpha/2, df.residual(model), lower.tail = FALSE)
    upr <- dif + (crit * se)
    lwr <- dif - (crit * se)
    data.frame(pair = paste(f1, f2, sep = '-'),
               diff = dif,
               se = se,
               upper = upr,
               lower = lwr)
}




var_df_18 <- data.frame(var1 = "2y", 
                        var2 = "4y",
                        stringsAsFactors = F)

newdata_18 <- expand.grid(doy = unique(sw_d15y18$doy),
                       rot_trt = levels(sw_d15y18$rot_trt))


#--on raw vals
out18_v <- purrr::map_dfr(1:nrow(var_df_18), function(i) {
  d <- smooth_diff(mod_v, newdata_18, 
                   f1 = var_df_18[i,1], f2 = var_df_18[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})

#--on sqrt vals
out18_vsq <- purrr::map_dfr(1:nrow(var_df_18), function(i) {
  d <- smooth_diff(mod_vsq, newdata_18, 
                   f1 = var_df_18[i,1], f2 = var_df_18[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})



res_d15y18 <- 
  out18_vsq %>% 
  mutate(type = "sqrt vals") %>% 
  bind_rows(out18_v %>% 
              mutate(type = "raw vals")) %>% 
  mutate(doy = rep(unique(sw_d15y18$doy), 2)) %>% 
  as_tibble() %>% 
  mutate(year = 2018, 
         depth_cm = "15cm")

res_d15y18 %>% 
  ggplot(aes(doy, diff)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_hline(yintercept = 0) + 
  facet_grid(.~type)


# XXXXX 15cm 2019 ---------------------------------------------------------------


# 15cm, 2019, raw values --------------------------------------------------

#--very bad

sw_d15y19 <- 
  sw_f %>% 
  filter(year == 2019, sensor_depth_cm == 15, !is.na(value))

mod2_v <- gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
             data = sw_d15y19, method = "REML")

plot(mod2_v, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod2_v)


# 15cm, 2019, sqrt values -------------------------------------------------

#--also very bad
mod2_vsq <- gam(sqrt_value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
               data = sw_d15y19, method = "REML")

plot(mod2_vsq, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod2_vsq)



# 15cm, 2019, scaled vals -------------------------------------------------

#--also very bad

mod2_v2 <- gam(value_sc2 ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
              data = sw_d15y19, method = "REML")

plot(mod2_v2, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod2_v2)



# 15cm, 2019, sqrt scaled vals -------------------------------------------------

#--still very bad p-values
mod2_v2sq <- gam(sqrt_scvalue2 ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
                data = sw_d15y19, method = "REML")

plot(mod2_v2sq, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod2_v2sq)

# view the 24 fitted curves to visually inspect differences
sw_d15y19$p <- predict(mod2_v)

ggplot(data = sw_d15y19) +
  geom_line(aes(x = doy, y = value, color = rot_trt, group = plot_id)) +
  geom_line(aes(x = doy, y = p, color = rot_trt, group = plot_id), size = 3) + 
  facet_wrap( ~ year, ncol = 2) 


# diffs 15cm 2019 ---------------------------------------------------------

var_df_19 <- data.frame(var1 = "2y", 
                        var2 = "4y",
                        stringsAsFactors = F)

newdata_19 <- expand.grid(doy = unique(sw_d15y19$doy),
                          rot_trt = levels(sw_d15y19$rot_trt))


#--on raw vals
out19_v <- purrr::map_dfr(1:nrow(var_df_19), function(i) {
  d <- smooth_diff(mod2_v, newdata_19, 
                   f1 = var_df_19[i,1], f2 = var_df_19[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})

#--on sqrt vals
out19_vsq <- purrr::map_dfr(1:nrow(var_df_19), function(i) {
  d <- smooth_diff(mod2_vsq, newdata_19, 
                   f1 = var_df_19[i,1], f2 = var_df_19[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})


res_d15y19 <- 
  out19_vsq %>% 
  mutate(type = "sqrt vals") %>% 
  bind_rows(out19_v %>% 
              mutate(type = "raw vals")) %>% 
  mutate(doy = rep(unique(sw_d15y19$doy), 2)) %>% 
  as_tibble() %>% 
  mutate(year = 2019, 
         depth_cm = "15cm")


res_d15y19 %>% 
  ggplot(aes(doy, diff)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_hline(yintercept = 0) + 
  facet_grid(.~type)

# XXXXX 45cm 2018 ---------------------------------------------------------------


# 45cm, 2018, raw values --------------------------------------------------

#--ok

sw_d45y18 <- 
  sw_f %>% 
  filter(year == 2018, sensor_depth_cm == 45, !is.na(value))

mod3_v <- gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
              data = sw_d45y18, method = "REML")

plot(mod3_v, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod3_v)


# 45cm, 2018, sqrt values -------------------------------------------------

#--also fine
mod3_vsq <- gam(sqrt_value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
                data = sw_d45y18, method = "REML")

plot(mod3_vsq, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod3_vsq)



# 45cm, 2018, scaled vals -------------------------------------------------

#--alos fine

mod3_v2 <- gam(value_sc2 ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
               data = sw_d45y18, method = "REML")

plot(mod3_v2, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod3_v2)



# view the 24 fitted curves to visually inspect differences
sw_d45y18$p <- predict(mod3_v)

ggplot(data = sw_d45y18) +
  geom_line(aes(x = doy, y = value, color = rot_trt, group = plot_id)) +
  geom_line(aes(x = doy, y = p, color = rot_trt, group = plot_id), size = 3) + 
  facet_wrap( ~ year, ncol = 2) 


# diffs 45cm 2018 ---------------------------------------------------------

var_df_18 <- data.frame(var1 = "2y", 
                        var2 = "4y",
                        stringsAsFactors = F)

newdata_18 <- expand.grid(doy = unique(sw_d45y18$doy),
                          rot_trt = levels(sw_d45y18$rot_trt))


#--on raw vals
out18_v45 <- purrr::map_dfr(1:nrow(var_df_18), function(i) {
  d <- smooth_diff(mod3_v, newdata_18, 
                   f1 = var_df_18[i,1], f2 = var_df_18[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})

#--on scaled vals
out18_v245 <- purrr::map_dfr(1:nrow(var_df_18), function(i) {
  d <- smooth_diff(mod3_v2, newdata_18, 
                   f1 = var_df_18[i,1], f2 = var_df_18[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})


res_d45y18 <- 
  out18_v245 %>% 
  mutate(type = "scaled 0-1 vals") %>% 
  bind_rows(out18_v45 %>% 
              mutate(type = "raw vals")) %>% 
  mutate(doy = rep(unique(sw_d45y18$doy), 2)) %>% 
  as_tibble() %>% 
  mutate(year = 2018, 
         depth_cm = "45cm")


res_d45y18 %>% 
  ggplot(aes(doy, diff)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_hline(yintercept = 0) + 
  facet_grid(.~type)




# XXXXX 45cm 2019 ---------------------------------------------------------------


# 45cm, 2019, raw values --------------------------------------------------

#--ok

sw_d45y19 <- 
  sw_f %>% 
  filter(year == 2019, sensor_depth_cm == 45, !is.na(value))

mod4_v <- gam(value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
              data = sw_d45y19, method = "REML")

plot(mod4_v, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod4_v)


# 45cm, 2019, sqrt values -------------------------------------------------

#--also fine
mod4_vsq <- gam(sqrt_value ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
                data = sw_d45y19, method = "REML")

plot(mod4_vsq, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod4_vsq)



# 45cm, 2019, scaled vals -------------------------------------------------

#--very bad

mod4_v2 <- gam(value_sc2 ~ s(doy, by = rot_trt, bs = "cr", k = 70) + rot_trt,
               data = sw_d45y19, method = "REML")

plot(mod4_v2, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod4_v2)



# view the 24 fitted curves to visually inspect differences
sw_d45y19$p <- predict(mod4_v)

ggplot(data = sw_d45y19) +
  geom_line(aes(x = doy, y = value, color = rot_trt, group = plot_id)) +
  geom_line(aes(x = doy, y = p, color = rot_trt, group = plot_id), size = 3) + 
  facet_wrap( ~ year, ncol = 2) 


# diffs 45cm 2019 ---------------------------------------------------------

var_df_19 <- data.frame(var1 = "2y", 
                        var2 = "4y",
                        stringsAsFactors = F)

newdata_19 <- expand.grid(doy = unique(sw_d45y19$doy),
                          rot_trt = levels(sw_d45y19$rot_trt))


#--on raw vals
out19_v45 <- purrr::map_dfr(1:nrow(var_df_19), function(i) {
  d <- smooth_diff(mod4_v, newdata_19, 
                   f1 = var_df_19[i,1], f2 = var_df_19[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})

#--on sqrt vals
out19_vsq45 <- purrr::map_dfr(1:nrow(var_df_19), function(i) {
  d <- smooth_diff(mod4_vsq, newdata_19, 
                   f1 = var_df_19[i,1], f2 = var_df_19[i,2], 
                   var = "rot_trt")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})


res_d45y19 <- 
  out19_vsq45 %>% 
  mutate(type = "sqrt vals") %>% 
  bind_rows(out19_v45 %>% 
              mutate(type = "raw vals")) %>% 
  mutate(doy = rep(unique(sw_d45y19$doy), 2)) %>% 
  as_tibble() %>% 
  mutate(year = 2019, 
         depth_cm = "45cm")


res_d45y19 %>% 
  ggplot(aes(doy, diff)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) + 
  geom_hline(yintercept = 0) + 
  facet_grid(.~type)



