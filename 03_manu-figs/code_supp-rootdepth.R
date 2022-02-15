## Make fig(s) for supp
##
## Date: aug 10 2021

rm(list = ls())
library(tidyverse)
library(patchwork)
library(maRsden)
library(nlraa)
library(emmeans)

source("03_manu-figs/palettes.R")

theme_set(theme_bw())

mghalab <- (expression(atop("Maize dry grain yield", paste("(Mg "~ha^-1*")"))))


# raw data ----------------------------------------------------------------


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
  mutate(dap = doy - dop,
         dataset = case_when(
           (dap > 125 & year == 2020) ~ "Removed",
           (dap > 100 & year == 2019) ~ "Removed",
           TRUE ~ "Included")
  )

rd %>% 
  ggplot(aes(dap, rootdepth_cm)) + 
  geom_point(size = 3, alpha = 0.5, 
             aes(color = dataset)) + 
  labs(x = "Days after planting",
       y = "Rooting depth (cm)",
       color = NULL) +
  scale_color_manual(values = c(grn1, ltrd2)) +
  facet_grid(.~year) + 
  labs(title = "Dataset for non-linear rooting depth analysis")

ggsave("03_manu-figs/sfig_rootdepth-dataset.png")

#--only keep point before the max is reached

rd_max <- 
  rd %>% 
  filter(!(dap > 125 & year == 2020)) %>% 
  filter(!(dap > 100 & year == 2019)) 



# use preds from nlraa fit ------------------------------------------------


pdat <- read_csv("01_rootdepth/dat_nlraa-preds-for-fig.csv")

pdat %>% 
  ggplot(aes(x = cum_gdd, y = rootdepth_cm, color = rotation)) + 
  facet_wrap(~ year.f) + 
  geom_point() + 
  geom_line(aes(y = prd1), size = 3) + 
  geom_ribbon(aes(ymin = lwr1, ymax = upr1, fill = rotation), alpha = 0.2) + 
  scale_fill_manual(values = c(pnk1, dkbl1)) +
  scale_color_manual(values = c(pnk1, dkbl1)) + 
  labs(x = "Cumulative GDDs",
       y = "Rooting depth (cm)")

ggsave("03_manu-figs/sfig_rootdepth-by-year.png")
## The reality is that 2018 is messy


# move to main manu (2/15/2022) -------------------------------------------
