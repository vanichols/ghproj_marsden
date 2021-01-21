# try to do growth analysis
# started 1/15/2021
# updated:

library(maRsden)
library(tidyverse)


#remotes::install_github("femiguez/nlraa")
library(nlraa)
library(nlme)
library(emmeans)
library(car) #--overwrites recode in dplyr
library(minpack.lm)
library(janitor)

theme_set(theme_bw())

# data --------------------------------------------------------------------

dat <- 
  mrs_cornbio %>%
  left_join(mrs_plotkey) %>% 
  ungroup() 

dat %>% 
  ggplot(aes(doy, mass_gpl)) + 
  geom_point(aes(color = rot_trt)) + 
  facet_grid(.~year)

mod_dat <- 
  dat %>% 
  select(plot_id, doy, mass_gpl, year, rot_trt) %>% 
  mutate(yearF = as.factor(year),
         rot_trt = as.factor(rot_trt))

mod_dat %>%
  ggplot(aes(doy, mass_gpl, color = rot_trt)) + 
  geom_point() + 
  facet_grid(.~yearF) + 
  labs(title = "observations")

ggsave("01_growth-analysis/fig_observations.png")

# individual model fitting -----------------------------------------------------------

mod_coefs <- 
  mod_dat %>% 
  group_by(yearF, rot_trt) %>% 
  nest() %>% 
  mutate(mfit = data %>% map(~nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = .)),
         mcoef = mfit %>% map(coef),
         mtib = mcoef %>% map(broom::tidy)) %>% 
  unnest(cols = c(mtib)) %>% 
  select(-data, -mfit, -mcoef)

a <- nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mod_dat %>% filter(yearF == "2013", rot_trt == "4y"))

coef(a)
confint(a) %>% as.data.frame() %>% rownames_to_column() %>%  as_tibble()

#--this is hard, maybe not worth it...
# mod_coefs_int <- 
#   mod_dat %>% 
#   group_by(yearF, rot_trt) %>% 
#   nest() %>% 
#   mutate(mfit = data %>% map(~nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = .)),
#          mconf = mfit %>% map(possibly(confint, NULL))) %>% 
#   filter(!is.null(mconf)),
#          mdf = mconf %>% map(as.data.frame)) %>% 
#            map(rownames_to_column) %>%
#            map(as_tibble)) %>% 
#   unnest(cols = c(mtib)) 


mod_coefs %>% 
  ggplot(aes(yearF, x, color = rot_trt)) + 
  geom_point() +
  facet_wrap(~names, scales = "free")

new_dat <- 
  mod_dat %>% 
  select(yearF, rot_trt) %>% 
  expand_grid(., doy = seq(130, 300, 1))

prd_dat <- 
  mod_dat %>% 
  group_by(yearF, rot_trt) %>% 
  nest() %>% 
  mutate(mfit = data %>% 
           map(possibly(~nls(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = .), NULL))) %>% 
  mutate(doy = list(seq(130, 300, 1)),
         mpreds = mfit %>% map(predict, newdata = data.frame(doy = seq(130, 300, 1)))
         ) %>% 
  select(-data, -mfit) %>% 
  unnest(cols = c(doy, mpreds))


prd_dat %>%
  ggplot(aes(doy, mpreds, color = rot_trt)) + 
  geom_line(size = 2) + 
  geom_point(data = mod_dat,
             aes(doy, mass_gpl, color = rot_trt),
             alpha = 0.5) +
  facet_grid(.~yearF) + 
  labs(title = "logistic fit",
       y = "Biomass per plant (g)")

ggsave("01_growth-analysis/fig_fe-logistic-fit.png")

# beta fit ----------------------------------------------------------------

#--test
t.bgf <- nls(mass_gpl ~ SSbgf(doy, w.max, t.e, t.m),
         data = mod_dat %>% filter(yearF == "2013", rot_trt == "4y"))

coef(t.bgf)
confint(t.bgf) %>% as.data.frame() %>% rownames_to_column() %>%  as_tibble()

mod_coefs_beta <- 
  mod_dat %>% 
  group_by(yearF, rot_trt) %>% 
  nest() %>% 
  mutate(mfit = data %>% 
           map(~nls(mass_gpl ~ SSbgf(doy, w.max, t.e, t.m), data = .)),
         mcoef = mfit %>% map(coef),
         mtib = mcoef %>% map(broom::tidy)) %>% 
  unnest(cols = c(mtib)) %>% 
  select(-data, -mfit, -mcoef)

# derivatives by individuals --------------------------------------------------------------


fun_logisticderiv <- function(Asym = Asym, xmid = xmid, scal = scal, doy = doy){
  
  fnum = Asym * exp(1/scal * (-doy + xmid))
  fdenom = scal * (1 + exp((xmid - doy)/scal))^2
  drv = fnum/fdenom
  return(drv)
  
}

dat_deriv <- 
  mod_coefs %>% 
  pivot_wider(names_from = names, values_from = x) %>% 
  left_join(new_dat) %>% 
  rowwise() %>% 
  mutate(abs_gr = fun_logisticderiv(Asym = Asym,
                                 xmid = xmid,
                                 scal = scal,
                                 doy = doy)) 

dat_deriv %>% 
  left_join(prd_dat) %>% 
  filter(!is.na(mpreds)) %>%
  mutate(rel_gr = abs_gr/mpreds) %>% 
  rename("mass_gpl" = mpreds) %>% 
  pivot_longer(abs_gr:rel_gr) %>% 
  mutate(name = factor(name, levels = c("mass_gpl", "abs_gr", "rel_gr"))) %>% 
  ggplot(aes(doy, value, color = rot_trt, group = name)) + 
  stat_summary(fun = "mean", geom = "line", aes(color = rot_trt, group = rot_trt)) +
  facet_grid(name~yearF, scales = "free")

ggsave("01_growth-analysis/fig_fe-growth-analysis.png")



# rue ---------------------------------------------------------------------

dat_deriv

rue <- 
  mrs_cornlai %>% 
  left_join(prd_dat)  %>% 
  mutate(
    lai_m2pl = lai_cm2pl * (1/100^2),
    rue = mpreds/(lai_m2pl)) 

mrs_cornlai %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, lai_cm2pl)) + 
  geom_point(aes(color = rot_trt)) +
  facet_grid(.~year)

rue %>% 
  select(year, doy, rot_trt, rue) %>% 
  group_by(year, doy, rot_trt) %>% 
  summarise(rue = mean(rue, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = rue) %>% 
  janitor::clean_names() %>% 
  mutate(rue_ratio = x4y/x2y) %>% 
  filter(doy < 225, !is.na(rue_ratio)) %>% 
  ggplot(aes(doy, rue_ratio)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_grid(.~year) + 
  labs(y = "Ratio of 4yr RUE to 2 yr RUE",
       title = "Ratio of 4yr to 2yr rotation RUE is always > 1")
  
ggsave("01_growth-analysis/fig_rue-ratio.png")



# all otgether ------------------------------------------------------------

rue_rat <- 
  rue %>% 
  select(year, doy, rot_trt, rue) %>% 
  group_by(year, doy, rot_trt) %>% 
  summarise(rue = mean(rue, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = rue) %>% 
  janitor::clean_names() %>% 
  mutate(rue_ratio = x4y/x2y) %>% 
  filter(doy < 225, !is.na(rue_ratio)) %>% 
  ungroup() %>% 
  mutate(yearF = as.factor(year)) %>% 
  select(yearF, doy, rue_ratio)


d1 <- 
  dat_deriv %>% 
  left_join(prd_dat) %>% 
  filter(!is.na(mpreds)) %>%
  mutate(rel_gr = abs_gr/mpreds) %>% 
  rename("mass_gpl" = mpreds) %>% 
  left_join(rue_rat) %>% 
  pivot_longer(abs_gr:rue_ratio) %>% 
  mutate(name = factor(name, levels = c("mass_gpl", "abs_gr", "rel_gr", "rue_ratio"))) %>% 
  ggplot(aes(doy, value, color = rot_trt, group = name)) + 
  stat_summary(fun = "mean", geom = "line", aes(color = rot_trt, group = rot_trt)) +
  facet_grid(name~yearF, scales = "free")

d2 <- 
  mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  filter(harv_crop != "C3") %>% 
  filter(year %in% c(2013, 2014, 2018, 2019, 2020)) %>% 
  ggplot(aes(rot_trt, yield_Mgha, fill = rot_trt)) + 
  stat_summary(geom = "bar", color = "black") + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.5) +
  facet_grid(.~year)

library(patchwork)
d1/d2

ggsave("01_growth-analysis/fig_fe-growth-analysis_rue.png")


# yield components --------------------------------------------------------



# mixed model fitting -----------------------------------------------------


mdG <- groupedData(mass_gpl ~ doy | plot_id, data = md)


#--fit model to each group
lmodG <- nlsList(mass_gpl ~ SSlogis(doy, Asym, xmid, scal), data = mdG) 

plot(intervals(lmodG))

#--Asym and xmid could have random effects added

fmm1 <- nlme(lmodG, random = pdDiag(Asym + xmid ~ 1))

plot(fmm1)
plot(fmm1, id = 0.000001) #--fernando likes things to be under 2
plot(fmm1, id = 0.01) #--eek. 

head(coef(fmm1)) #--notice scal is the same for everything
intervals(fmm1) #--random effect intervals are reasonable

fxf1 <- fixef(fmm1) #--we need these if we want to add an effect of rotation

#--note this an update, it keeps the random effects, we are just adding a fixed effect
fmm2 <- update(fmm1, 
               fixed = list(Asym + xmid + scal ~ rot_trt),
               start = c(fxf1[1], 0, #--Asym
                         fxf1[2], 0, #--xmid
                         fxf1[3], 0)) #--scal

fxf2 <- fixef(fmm2) #--now we have a value for cc and cs

anova(fmm2) #--everything is not 0. not that informative
intervals(fmm2) #--still well constrained estimates
plot(fmm2, id = 0.01) #--outliers mean maybe we could model the residual variance better. Don't know how to do that.

#--could try having the variance increase with higher values
fmm3 <- update(fmm2, weights = varPower())
plot(fmm3, id = 0.01)

#--compare them ?not working grr
par(mfrow=c(2,1))
plot(fmm3, id = 0.01, main="varPower")
plot(fmm2, id = 0.01, main="no variance modelling")

#--FEM does something I don't quite get. 
# instead of modelling the variance, try adding two random effect levels?

fmm3a <- update(fmm2, random = list(plot_id = pdDiag(Asym + xmid + scal ~ 1),
                                    yearF = pdDiag(Asym + xmid + scal ~ 1)),
                groups = ~ yearF/plot_id)

## Fewer outliers, this is a better model
plot(fmm3a, id = 0.001)
 

## Parameter values and contrast among groups
emmeans(fmm3a, ~ rot_trt, param = "Asym")
emmeans(fmm3a, ~ rot_trt, param = "xmid")
emmeans(fmm3a, ~ rot_trt, param = "scal")

## Contrasts
contrast(emmeans(fmm3a, ~ rot_trt, param = "Asym"), "pairwise")
contrast(emmeans(fmm3a, ~ rot_trt, param = "xmid"), "pairwise")
contrast(emmeans(fmm3a, ~ rot_trt, param = "scal"), "pairwise")


# look at preds -----------------------------------------------------------

dat_doy <- 
  mdG %>% 
  select(plot_id, yearF, rot_trt) %>% 
  expand_grid(., doy = seq(130, 300, 5))

dat_doy$mass_gpl_pred <- predict(fmm3a, level = 0, newdata = dat_doy)

dat_doy %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(x = doy, y = mass_gpl_pred)) + 
  geom_line(aes(color = rot_trt), size = 3) 


fmm2.sim1 <- simulate_nlme(fmm3a, nsim = 100, psim = 1, level = 0)
mdG$mn.s <- apply(fmm2.sim1, 1, mean)
mdG$mxn.s <- apply(fmm2.sim1, 1, max)
mdG$mnn.s <- apply(fmm2.sim1, 1, min)


ggplot() + 
  #  geom_point(data = leachG, aes(x = nrate_kgha, y = leaching_kgha)) + 
  geom_line(data = mdG, aes(x = doy, 
                               y = prds, 
                               color = rot_trt), size = 2) +
  geom_ribbon(data = mdG, 
              mapping = aes(x = doy, 
                            ymin = mxn.s, 
                            ymax = mnn.s, fill = rot_trt), 
              alpha = 0.5)


