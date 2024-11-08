## fit a logistic to each eu
# started July 20 2021
# ugh. finished july 21 2021. Fit to each trt/year, not each rep.


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


# ignore block ------------------------------------------------------------


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
  
  res2 <- tibble(param = names(res),
         value = as.numeric(res))
  return(res2)
  
}


#--test on one
(nls_logis(filter(rd_mod, eu == "2018_2y"))) 


#--map function to each eu data
logis_rd <-
  rd_mod %>%
  group_by(eu) %>% 
  nest(data = c(cum_gdd, rootdepth_cm)) %>%
  mutate(
    mod_fit = data %>% map(possibly(nls_logis, NULL)),
   # is_null = mod_fit %>% map_lgl(is.null)
  ) %>% 
  #filter(is_null == 0) %>%  
  unnest(cols = c(mod_fit)) %>% 
  left_join(mrs_plotkey %>% mutate(eu = paste(year, rot_trt, sep = "_"))) %>% 
  ungroup() %>% 
  select(year, rot_trt, value, param) %>% 
  distinct() 

logis_rd %>% 
  write_csv("01_rootdepth/dat_nls-parameters-eu.csv")


logis_rd %>% 
  ggplot(aes(param, value, color = rot_trt)) + 
  geom_point()


# get uncertainty around parameters ---------------------------------------

nls_logis2 <- function(dat){
  res <- (summary(nls(rootdepth_cm ~ SSlogis(cum_gdd, Asym, xmid, scal), data = dat))$parameters)
  
  res2 <- res %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    mutate(param = row.names(res))
  
  return(res2)
  
}

nls_logis2(dum_dat)

#--map function to each eu data
logis2_rd <-
  rd_mod %>%
  group_by(eu) %>% 
  nest(data = c(cum_gdd, rootdepth_cm)) %>%
  mutate(
    mod_fit = data %>% map(possibly(nls_logis2, NULL)),
    # is_null = mod_fit %>% map_lgl(is.null)
  ) %>% 
  #filter(is_null == 0) %>%  
  unnest(cols = c(mod_fit)) %>% 
  left_join(mrs_plotkey %>% mutate(eu = paste(year, rot_trt, sep = "_"))) %>% 
  ungroup() %>% 
  select(year, rot_trt, param, estimate, std_error) %>% 
  distinct() %>% 
  mutate(loCI = estimate - 1.96 * std_error,
         hiCI = estimate + 1.96 * std_error) %>% 
  select(-std_error)


logis2_rd %>% 
  write_csv("01_rootdepth/dat_nls-parameters-eu-CIs.csv")

# include block ------------------------------------------------------------

rd_mod2 <- 
  rd_max %>% 
  mutate(eu = as.factor(paste(year_rot, block, sep = "_"))) %>% #fit to year/rot/block
  select(eu, cum_gdd, rootdepth_cm)


#--test on one
(nls_logis(filter(rd_mod2, eu == "2018_2y_b1"))) 


#--map function to each eu data
logis_rd2 <-
  rd_mod2 %>%
  group_by(eu) %>% 
  nest(data = c(cum_gdd, rootdepth_cm)) %>%
  mutate(
    mod_fit = data %>% map(possibly(nls_logis, NULL)),
    # is_null = mod_fit %>% map_lgl(is.null)
  ) %>% 
  #filter(is_null == 0) %>%  
  unnest(cols = c(mod_fit)) %>% 
  left_join(mrs_plotkey %>% mutate(eu = paste(year, 
                                              rot_trt, 
                                              block,
                                              sep = "_"))) %>% 
  ungroup() %>% 
  select(year, block, rot_trt, value, param) %>% 
  distinct() 

logis_rd2 %>% 
  write_csv("01_rootdepth/dat_nls-parameters-eu-block.csv")


logis_rd2 %>% 
  ggplot(aes(param, value, color = rot_trt)) + 
  geom_point() + 
  facet_grid(param~year, scales = "free")


# stats -------------------------------------------------------------------

logis_rd2

library(lme4)
library(lmerTest)
library(emmeans)

m1 <- lmer(value ~ rot_trt*yearF + (1|block), 
     data = logis_rd2 %>% 
       filter(param == "Asym") %>% 
       mutate(yearF = as.factor(year)))

anova(m1)
em1 <- emmeans(m1, specs = c("rot_trt", "yearF"))

#--not ven close to being sig diff
pairs(emmeans(em1, specs = c("rot_trt", "yearF"))) %>% 
  as.data.frame() %>% 
  separate(contrast, sep = "-", into = c("v1", "v2")) %>% 
  mutate(v1 = str_trim(v1),
         v2 = str_trim(v2)) %>% 
  separate(v1, sep = " ", into = c("rot1", "yr1")) %>% 
  separate(v2, sep = " ", into = c("rot2", "yr2")) %>% 
  filter(rot1 != rot2) %>% 
  filter(yr1 == yr2)


 
em1 %>% 
  as.data.frame() %>% 
  ggplot(aes(rot_trt, emmean)) + 
  geom_point() + 
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL)) + 
  facet_grid(.~yearF)
