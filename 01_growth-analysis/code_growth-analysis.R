# try to do growth analysis
# started 1/15/2021
# updated:

#remotes::install_github("ashenoy-cmbi/grafify@*release", dependencies = T) 
library(maRsden)
library(tidyverse)
library(grafify)

#remotes::install_github("femiguez/nlraa")
library(nlraa)
library(janitor)

theme_set(theme_bw())

# data --------------------------------------------------------------------

mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  group_by(harv_crop) %>% 
  summarise(mn = mean(yield_Mgha))

library(saapsim)

saf_kgha_to_buac_corn(600)

dat <- 
  mrs_cornbio %>%
  left_join(mrs_plotkey) %>% 
  ungroup() 

dat %>% 
  ggplot(aes(doy, mass_gpl)) + 
  geom_point(aes(color = rot_trt),size = 3) + 
  scale_color_grafify() +
  facet_grid(.~year)

mod_dat <- 
  dat %>% 
  select(plot_id, doy, mass_gpl, year, rot_trt) %>% 
  mutate(yearF = as.factor(year),
         rot_trt = as.factor(rot_trt))

mod_dat %>%
  ggplot(aes(doy, mass_gpl, color = rot_trt)) + 
  geom_point(size = 4) + 
  scale_color_grafify() +
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
             alpha = 0.4, size = 4) +
  facet_grid(.~yearF) + 
  scale_color_grafify() +
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
  mutate(name = factor(name, levels = c("mass_gpl", "abs_gr", "rel_gr"))) %>%   ggplot(aes(doy, value, color = rot_trt, group = name)) + 
  stat_summary(fun = "mean", geom = "line", aes(color = rot_trt, group = rot_trt), size = 2) +
  scale_color_grafify() +
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
  scale_color_grafify() +
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
  scale_color_grafify() +
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
  filter(name != "rue_ratio") %>% 
  mutate(name = factor(name, levels = c("mass_gpl", "abs_gr", "rel_gr"))) %>% 
  ggplot(aes(doy, value, color = rot_trt, group = name)) + 
  stat_summary(fun = "mean", geom = "line", aes(color = rot_trt, group = rot_trt), size = 2) +
  scale_color_grafify() +
  
  facet_grid(name~yearF, scales = "free")

d2 <- 
  mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  filter(harv_crop != "C3") %>% 
  filter(year %in% c(2013, 2014, 2018, 2019, 2020)) %>% 
  ggplot(aes(rot_trt, yield_Mgha, fill = rot_trt)) + 
  stat_summary(geom = "bar", color = "black") + 
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.5) +
  scale_fill_grafify() +
  facet_grid(.~year)

library(patchwork)
d1/d2

ggsave("01_growth-analysis/fig_fe-growth-analysis_yields.png")


# yield components --------------------------------------------------------

mrs_krnl500 %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(rot_trt, krnl500_g, color = rot_trt)) + 
  stat_summary(geom = "point", size = 4) + 
  stat_summary(geom = "errorbar", width = 0.5) +
  #geom_jitter(size = 4, width = 0.2) +
  scale_color_grafify() +
  facet_grid(.~year, scales = "free") + 
  labs(x = NULL, y = "Weight of 500 kernals (g)") + 
  theme(axis.title = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.3)))

ggave("01_growth-analysis/fig_kernal-size.png")

mrs_earrows %>% 
  # group_by(year, plot_id) %>% 
  # summarise(rows_nu = mean(rows_nu, na.rm = T)) %>% 
  left_join(mrs_krnl500) %>% 
  pivot_longer(rows_nu:krnl500_g) %>% 
  left_join(mrs_plotkey) %>% 
  filter(name == "krnl500_g") %>% 
  ggplot(aes(rot_trt, value, color = rot_trt)) + 
  #stat_summary(geom = "point") + 
  #stat_summary(geom = "errorbar", width = 0.5) +
  geom_jitter() +
  scale_color_grafify() +
  facet_grid(name~year, scales = "free")
