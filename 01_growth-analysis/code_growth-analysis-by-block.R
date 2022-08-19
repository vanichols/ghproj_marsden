# try to do growth analysis
# started 1/15/2021
# updated: 7/21/2021, trying to work through it again...
#         9/15/2021, doing stats on parameters
#         8/19/2022, try to get max biomass per block

library(maRsden)
library(tidyverse)
library(nlraa)
library(janitor)
library(nlraa)
library(nlme)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)
library(janitor)
library(broom)

theme_set(theme_bw())

# data --------------------------------------------------------------------

dat <- 
  mrs_cornbio %>%
  left_join(mrs_plotkey) %>% 
  ungroup() 

dat %>% 
  group_by(year, block, rot_trt) %>% 
  tally() %>% 
  filter(block == "b1",
         rot_trt == "2y")

dat %>% 
  ggplot(aes(doy, mass_gpl)) + 
  geom_point(aes(color = rot_trt),size = 3) + 
  facet_grid(.~year)

mod_dat <- 
  dat %>% 
  select(plot_id, block, doy, mass_gpl, year, rot_trt) %>% 
  mutate(yearF = as.factor(year),
         rot_trt = as.factor(rot_trt))

mod_dat %>%
  ggplot(aes(doy, mass_gpl, color = rot_trt)) + 
  geom_point(size = 4) + 
  facet_grid(.~yearF) + 
  labs(title = "observations")


# individual model fitting -----------------------------------------------------------

mod_coefs <- 
  mod_dat %>% 
  group_by(yearF, block, rot_trt) %>% 
  nest() %>% 
  mutate(mfit = data %>% map(~nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = .)),
         mcoef = mfit %>% map(coef),
         mtib = mcoef %>% map(broom::tidy)) %>% 
  unnest(cols = c(mtib)) %>% 
  select(-data, -mfit, -mcoef)


mod_coefs %>% 
  write_csv("01_growth-analysis/dat_growth-anal-params-block.csv")


# old ---------------------------------------------------------------------


#--conf ints?
mod_dat %>% 
  group_by(yearF, rot_trt) %>% 
  nest() %>% 
  mutate(mfit = data %>% map(~nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = .)),
         mcoef = mfit %>% map(confint),
         mtib = mcoef %>% map(broom::tidy)) %>% 
  unnest(cols = c(mtib)) %>% 
  select(-data, -mfit, -mcoef)

mod_coefs %>% 
  pivot_wider(names_from = names,
              values_from = x) %>% 
  select(yearF, everything()) %>% 
  arrange(yearF, rot_trt) 

a <- nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mod_dat %>% filter(yearF == "2013", rot_trt == "4y"))

coef(a)
confint(a) %>% as.data.frame() %>% rownames_to_column() %>%  as_tibble()

params <- 
  confint(a) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%  
  as_tibble() %>% 
  select(rowname) %>% 
  pull() %>% 
  as.vector()

#--this is hard, maybe not worth it...
mod_coefs_int <-
  mod_dat %>%
  group_by(yearF, rot_trt) %>%
  nest() %>%
  mutate(mfit = data %>% map(~nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = .)),
         mconf = mfit %>% map(possibly(confint, NULL))) %>%
  filter(!is.null(mconf)) %>% 
  mutate( mdf = mconf %>% map(as.data.frame))



# 2013 cis ----------------------------------------------------------------
y13_2y <- nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mod_dat %>% filter(yearF == "2013", rot_trt == "2y"))
y13_4y <- nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mod_dat %>% filter(yearF == "2013", rot_trt == "4y"))

#--2013 p< 0.05 xmid is lower in 4yr
bind_rows(
confint(y13_2y, level = 0.99) %>% as.data.frame() %>% rownames_to_column() %>%  as_tibble() %>% mutate(rot_trt = "2y"),
confint(y13_4y, level = 0.99) %>% as.data.frame() %>% rownames_to_column() %>%  as_tibble() %>% mutate(rot_trt = "4y")
) %>% 
  arrange(rowname) %>% 
  filter(rowname == "xmid")


# 2018 cis ----------------------------------------------------------------

y18_2y <- nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mod_dat %>% filter(yearF == "2018", rot_trt == "2y"))
y18_4y <- nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mod_dat %>% filter(yearF == "2018", rot_trt == "4y"))


#--2018 p< 0.01 xmid is higher in 4yr
bind_rows(
  confint(y18_2y, level = 0.99) %>% as.data.frame() %>% rownames_to_column() %>%  as_tibble() %>% mutate(rot_trt = "2y"),
  confint(y18_4y, level = 0.99) %>% as.data.frame() %>% rownames_to_column() %>%  as_tibble() %>% mutate(rot_trt = "4y")
) %>% 
  arrange(rowname) %>% 
  filter(rowname == "xmid")




mod_coefs_int <-
  mod_dat %>%
  group_by(yearF, rot_trt) %>%
  nest() %>%
  mutate(mfit = data %>% map(~nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = .)),
         mconf = mfit %>% map(possibly(confint, NULL))) %>%
  filter(!is.null(mconf)) %>% 
  mutate( mdf = mconf %>% map(as.data.frame))


length(rep(params, 10))
myparams <- (rep(params, 10))

tidy_coefs <- 
  mod_coefs_int %>% 
  select(rot_trt, yearF, mdf) %>% 
  unnest(mdf) %>% 
  as_tibble() %>% 
  mutate(names = myparams) %>% 
  left_join(mod_coefs) %>% 
  janitor::clean_names() %>% 
  rename("parameter" = names,
         "value" = x,
         "lo95" = x2_5_percent,
         "hi95" = x97_5_percent) %>% 
  select(year_f, rot_trt, parameter, value, lo95, hi95) %>% 
  arrange(year_f, rot_trt, parameter) %>% 
  mutate_if(is.numeric, round, 1) 

tidy_coefs %>% 
  write_csv("01_growth-analysis/dat_growth-anal-params-eu.csv")



# ugh how do I test them --------------------------------------------------

bm <- 
  mod_dat %>% 
  mutate(eu = paste(plot_id, rot_trt, sep = "_")) %>% 
  select(eu, yearF, rot_trt, doy, mass_gpl) %>% 
  mutate_if(is.character, as.factor)

bmG <- groupedData(mass_gpl ~ doy| eu, data = bm)

#--example
#a <- nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mod_dat %>% filter(yearF == "2013", rot_trt == "4y"))

bm_modG <- nlsList(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = bmG) 

#--scal might not need to depend on rot_trt. But try it anyways. 
plot(intervals(bm_modG))

bm_mod1 <- nlme(bm_modG, random = pdDiag(Asym + xmid + scal ~ 1)) #--add random effects

#--add fixed effect of rotation
fxf1 <- fixef(bm_mod1) 

bm_mod2 <- update(bm_mod1, 
                fixed = list(Asym + xmid + scal ~ rot_trt),
                start = c(fxf1[1], 0, #--Asym
                          fxf1[2], 0, #--xmid
                          fxf1[3], 0)) #--scal

plot(bm_mod2, id = 0.01) #--seems ok, could try using varPower

#--nope
bm_mod3 <- update(bm_mod2, weights = varPower())

#--try another level of randomness
bm_mod4 <- update(bm_mod2, random = list(site = pdDiag(Asym + xmid + scal ~ 1),
                                   eu = pdDiag(Asym + xmid + scal ~ 1)),
               groups = ~ yearF/eu)


plot(bm_mod4, id = 0.01) #--this does look better. 
fxf2 <- fixef(bm_mod4) #--now we have a value for 2y and 4y


## Parameter values and contrast among groups
contrast(emmeans(bm_mod4, ~ rot_trt, param = "Asym"), "pairwise")
contrast(emmeans(bm_mod4, ~ rot_trt, param = "xmid"), "pairwise")
contrast(emmeans(bm_mod4, ~ rot_trt, param = "scal"), "pairwise")
