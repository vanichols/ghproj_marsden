## fit a logistic to each eu
# started July 20 2021
# ugh


library(tidyverse)
library(nlraa)
library(nlme)
library(emmeans)
library(maRsden)


rd_raw <- 
  read_csv("01_rootdepth/td_rootdepth-elev-wea.csv") %>% 
  group_by(year, doy, cum_gdd, rot_trt, block, plot_id, mean_elev_m) %>% 
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T)) %>% 
  mutate(year_rot = paste(year, rot_trt, sep = "_"))

#--calculate days after planting
rd <-
  rd_raw %>% 
  ungroup() %>% 
  arrange(year, doy, plot_id) %>%
  mutate(dop = ifelse(rootdepth_cm == 0, doy, NA)) %>%
  fill(dop) %>%
  mutate(dap = doy - dop)

#--only keep point before the max is reached
rd_max <- 
  rd %>% 
  filter(!(dap > 125 & year == 2020)) %>% 
  filter(!(dap > 100 & year == 2019)) 

## Set up NLME ##################

rd_mod <- 
  rd_max %>% 
  mutate(eu = as.factor(year_rot)) %>% #--ignore block, fit to year/rot
  select(eu, cum_gdd, rootdepth_cm)


## Typically we only have one observation per experimental unit
## it is no big deal but NLME sort of assumes this

# note: can use dap OR cum_gdd as x-axis

#--build functions

# build function to fit logistic ------------------------------------------

nls_logis <- function(dat){
  res <- coef(nls(rootdepth_cm ~ SSlogis(cum_gdd, Asym, xmid, scal), data = dat))
}



#--test on one
coef(nls_logis(filter(rd_mod, eu == "2018_2y"))) %>% 


#--map function to data
leach_aic <-
  leach %>%
  group_by(eu) %>% 
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(
    blin = data %>% map(possibly(aic_blin, NULL)),
    expf = data %>% map(possibly(aic_expf, NULL)),
    explin = data %>% map(possibly(aic_explin, NULL)),
    is_null = blin %>% map_lgl(is.null)
  ) %>% 
  filter(is_null == 0) %>%  
  unnest(cols = c(blin, expf, explin)) 




#--use cum_gdd
rd_maxG <- groupedData(rootdepth_cm ~ cum_gdd | plot_id, data = na.omit(rd_max))

plot(rd_maxG)


## Fit model to each individual curve
fitL <- nlsLMList(rootdepth_cm ~ SSlogis(cum_gdd, Asym, xmid, scal), data = rd_maxG)

plot(fitL)

fm0 <- nlme(fitL, random = pdDiag(Asym + xmid + scal ~ 1))

plot(fm0)

## Incorporate the effect of rotation
fxf <- fixef(fm0)
## Update model
fm1 <- update(fm0, 
              fixed = Asym + xmid + scal ~ rotation,
              start = c(fxf[1], 0, fxf[2], 0, fxf[3], 0))

# how to specify fixed effects? Look at the order...
fm1a <- update(fm0,
               fixed = list(Asym ~  rotation + mean_elev_m,
                            xmid + scal ~ rotation),
               start = c(fxf[1], 0, 0, # asym
                         fxf[2], 0, 
                         fxf[3], 0))


anova(fm1a) ## Not much of an effect?

fm2 <- update(fm1, random = list(year.f = pdDiag(Asym + xmid + scal ~ 1),
                                 plot_id = pdDiag(Asym + xmid + scal ~ 1)),
              groups = ~year.f/plot_id)

anova(fm2)
## The year accounts for most of the variability
## We do not need this effect at the level of plot for xmid and scal
fm2
## Simpler model
fm3 <- update(fm2, random = list(year.f = pdDiag(Asym + xmid + scal ~ 1),
                                 plot_id = pdDiag(Asym ~ 1)),
              groups = ~year.f/plot_id)

# data --------------------------------------------------------------------

dat <- 
  read_csv("01_proc-raw-outs/pro_apdat.csv") %>% 
  arrange(site_id, year, nrate_kgha) %>%
  mutate(yearF = as.factor(year),
         rotation = dplyr::recode(rotation,                                                                "sc" = "cs")) %>% 
  mutate(eu = paste0(site_id,"_", year, rotation)) %>% #--add an eu identifier
  mutate(site_id = as.factor(site_id),
         rotation = as.factor(rotation)) %>% 
  filter(!is.na(nyear_leach_kgha_tot)) %>% 
  select(eu, site_id, rotation, yearF, crop, nrate_kgha, nyear_leach_kgha_tot, yield_maize_buac)


dat %>% 
  select(eu) %>% 
  distinct() %>% 
  tally()

unique(dat$site_id)

# leach -------------------------------------------------------------------

leach <- 
  dat %>% 
  select(eu, everything()) %>% 
  rename("leaching_kgha" = nyear_leach_kgha_tot) %>% 
  pivot_longer(leaching_kgha:yield_maize_buac) %>%
  filter(name == "leaching_kgha")

#--build functions

aic_blin <- function(x){
  AIC(nls(value ~ SSblin(nrate_kgha, a, b, xs, c), data = x))
}


aic_expf <- function(x){
  AIC(nls(value ~ SSexpf(nrate_kgha, a, c), data = x))
}

aic_explin <- function(x){
  AIC(nlsLM(value ~ SSexplin(nrate_kgha, cm, rm, tb), data = x))
}

#--test on one
aic_blin(filter(leach, eu == "gent_2000cc"))


#--map function to data
leach_aic <-
  leach %>%
  group_by(eu) %>% 
  nest(data = c(value, nrate_kgha, yearF)) %>%
  mutate(
    blin = data %>% map(possibly(aic_blin, NULL)),
    expf = data %>% map(possibly(aic_expf, NULL)),
    explin = data %>% map(possibly(aic_explin, NULL)),
    is_null = blin %>% map_lgl(is.null)
  ) %>% 
  filter(is_null == 0) %>%  
  unnest(cols = c(blin, expf, explin)) 

leach_aic %>% 
  filter(is_null == "TRUE")

leach_aic %>%
  select(eu, blin, expf, explin) %>% 
  pivot_longer(blin:explin) %>% 
  ggplot(aes(eu, value)) +
  geom_point(aes(color = name))

#--what % is blin best? etc
leach_aic %>%
  select(eu, blin, expf, explin) %>% 
  pivot_longer(blin:explin) %>% 
  group_by(eu) %>% 
  mutate(min = min(value)) %>% 
  filter(value == min) %>%
  ungroup() %>% 
  summarise(blinT = sum(ifelse(name == "blin", 1, 0))/n(),
            expfT = sum(ifelse(name == "expf", 1, 0))/n(),
            explinT = sum(ifelse(name == "explin", 1, 0))/n())
