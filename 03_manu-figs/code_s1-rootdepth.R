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


fig_rawdata <- 
  rd %>% 
  ggplot(aes(dap, rootdepth_cm)) + 
  geom_point(size = 3, alpha = 0.5, 
             aes(color = dataset)) + 
  labs(x = "Days after planting",
       y = "Rooting depth (cm)",
       color = NULL) +
  scale_color_manual(values = c("black", "gray80")) +
  facet_grid(.~year) + 
  labs(title = "Dataset for non-linear rooting depth analysis")


# no smoothing ------------------------------------------------------------

mghalab <- (expression(atop("Maize dry grain yield", paste("(Mg "~ha^-1*")"))))

pdat <- read_csv("01_rootdepth/dat_nlraa-preds-for-fig.csv")


fig_nosmooth <- 
  pdat %>%
  mutate(rotation = ifelse(rotation == "2y", "Short", "Extended"))  %>% 
  ggplot(aes(x = cum_gdd, y = rootdepth_cm, color = rotation)) + 
  facet_wrap(~ year.f) + 
  geom_point() + 
  geom_line(aes(y = prd1), size = 3) + 
  geom_ribbon(aes(ymin = lwr1, ymax = upr1, fill = rotation), alpha = 0.2) + 
  scale_fill_manual(values = c(dkbl1, pnk1)) +
  scale_color_manual(values = c(dkbl1,
                                pnk1)) + 
  labs(x = "Cumulative GDDs",
       y = "Rooting depth (cm)",
       title = "Non-linear fits without smoothing")



# put together ------------------------------------------------------------


fig_rawdata / fig_nosmooth

ggsave("03_manu-figs/s1_root-data-and-non-smooth-fit.png", width = 6.93, height = 7.2)



