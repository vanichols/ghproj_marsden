## Analysis of corn root depth data
##
## Authors: Gina Nichols and Fernando Miguez
## Date: Oct 12th 2020
## Updated: Oct 13 2020 (added GDDs and plot elevation)
##          July 20 2021, trying to write a manuscript
##          cleaned up and renamed, just want emmeans for asym, ugh

library(tidyverse)
library(nlraa)
library(patchwork)
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

fig_full <- 
  rd %>% 
  ggplot(aes(dap, rootdepth_cm)) + 
  geom_point(size = 3, alpha = 0.5, color = "blue") + 
  facet_grid(.~year) + 
  labs(title = "Full dataset, beta-growth candidate")

print(fig_full)

#--only keep point before the max is reached

rd_max <- 
  rd %>% 
  filter(!(dap > 125 & year == 2020)) %>% 
  filter(!(dap > 100 & year == 2019)) 

fig_max <- 
  rd_max %>% 
  ggplot(aes(dap, rootdepth_cm, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5, color = "red") + 
  facet_grid(.~year)  + 
  labs(title = "Trimmed dataset, logistic candidate")

print(fig_max)

fig_full / fig_max

rd_max


## Set up NLME ##################

rd_max$yearrot.f <- as.factor(rd_max$year_rot)
rd_max$year.f <- as.factor(rd_max$year)
rd_max$rotation <- as.factor(rd_max$rot_trt)

## Typically we only have one observation per experimental unit
## it is no big deal but NLME sort of assumes this
#--we can average, done above

# note: can use dap OR cum_gdd as x-axis

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

## Comparing models
anova(fm2, fm3)
## We can keep the simpler model
## Do we need to model the variance?
fm4 <- update(fm3, weights = varPower())
## Compare models again
anova(fm3, fm4)
## Yes, this is a better model
plot(fm4)

## Rotation does not have much of an effect on scal
anova(fm4)

## This doesn't work
#fm5 <- update(fm3, weights = varFixed(~cum_gdd))


## Visualize at the population level
sim0 <- simulate_nlme(fm4, level = 0, nsim = 1e3)

rd_maxG$prd0 <- apply(sim0, 1, median)
rd_maxG$lwr0 <- apply(sim0, 1, quantile, probs = 0.05)
rd_maxG$upr0 <- apply(sim0, 1, quantile, probs = 0.95)

ggplot(rd_maxG, aes(x = cum_gdd, y = rootdepth_cm, color = rotation)) + 
  geom_point() + 
  geom_line(aes(y = prd0), size = 3) + 
  geom_ribbon(aes(ymin = lwr0, ymax = upr0, fill = rotation), alpha = 0.2)

## Test for asymptote
emmeans(fm4, ~rotation, param = "Asym")
contrast(emmeans(fm4, ~rotation, param = "Asym"), "pairwise")
plot(emmeans(fm4, ~rotation, param = "Asym"))

#--write results for figure
asym_res <- 
  emmeans(fm4, ~rotation, param = "Asym") %>% 
  broom::tidy() %>% 
  mutate(param = "Asym") 

asym_sig <-
  contrast(emmeans(fm4, ~rotation, param = "Asym"), "pairwise") %>% 
  broom::tidy() %>% 
  mutate(param = "Asym") 


emmeans(fm4, ~rotation, param = "xmid")
contrast(emmeans(fm4, ~rotation, param = "xmid"), "pairwise")
plot(emmeans(fm4, ~rotation, param = "xmid"))

xmid_res <- 
  emmeans(fm4, ~rotation, param = "xmid") %>% 
  broom::tidy() %>% 
  mutate(param = "xmid") 

xmid_sig <-
  contrast(emmeans(fm4, ~rotation, param = "xmid"), "pairwise") %>% 
  broom::tidy() %>% 
  mutate(param = "xmid") 

bind_rows(asym_res, xmid_res) %>%  
  write_csv("01_rootdepth/dat_nlraa-est.csv")

bind_rows(asym_sig, xmid_sig) %>%  
  write_csv("01_rootdepth/dat_nlraa-sig.csv")


## Simulation for an individual year
sim1 <- simulate_nlme(fm4, level = 1, nsim = 1e3)
## sim1 <- simulate_nlme(fm4, level = 1, nsim = 1e3, value = "data.frame")

rd_maxG$prd1 <- apply(sim1, 1, median)
rd_maxG$lwr1 <- apply(sim1, 1, quantile, probs = 0.05)
rd_maxG$upr1 <- apply(sim1, 1, quantile, probs = 0.95)

rd_maxG %>% 
  as_tibble() %>% 
  write_csv("01_rootdepth/dat_nlraa-preds-for-fig.csv")

ggplot(rd_maxG, aes(x = cum_gdd, y = rootdepth_cm, color = rotation)) + 
  facet_wrap(~ year.f) + 
  geom_point() + 
  geom_line(aes(y = prd1), size = 3) + 
  geom_ribbon(aes(ymin = lwr1, ymax = upr1, fill = rotation), alpha = 0.2)

## The reality is that 2018 is messy

