# Gina
# show maximum root deptsh
# created: 2/15/2022
# updated: 2/23/2022 - made fig with smooth preds
#          8/12/2022 - add corn stages


rm(list=ls())
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(patchwork)
library(maRsden)

mrs_rootdepth


# fig stuff ---------------------------------------------------------------

source("03_manu-figs/palettes.R")

theme_set(theme_bw())


myth <- 
  theme(strip.text = element_text(size = rel(1.2)),
        strip.background = element_blank(),
        axis.text = element_text(size = rel(1.1)))

mghalab <- (expression(atop("Maize dry grain yield", paste("(Mg "~ha^-1*")"))))


# data --------------------------------------------------------------------

doy_to_cum_gdd <- 
  read_csv("01_rootdepth/td_rootdepth-elev-wea.csv") %>%
  select(year, doy, cum_gdd) %>% 
  distinct()

fig_dat <- 
  mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>%
  group_by(year, doy, date, rot_trt) %>% 
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T)) %>% 
  left_join(
    mrs_rootdepth %>% 
      left_join(mrs_plotkey) %>%
      group_by(year, doy, date, rot_trt) %>% 
      summarise(rootdepth_sd = sd(rootdepth_cm, na.rm =T),
                rootdepth_se = rootdepth_sd/sqrt(4-1))
  ) %>% 
  filter(doy < 240) %>% 
  mutate(rot_trt2 = ifelse(rot_trt == "2y", "Simple", "Complex")) %>% 
  left_join(doy_to_cum_gdd)

fig_dat_block <- 
  mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>%
  group_by(year, doy, date, rot_trt, block) %>% 
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T)) %>% 
  filter(doy < 240) %>% 
  mutate(rot_trt2 = ifelse(rot_trt == "2y", "Simple", "Complex")) %>% 
  left_join(doy_to_cum_gdd)


fig_preds <- 
  read_csv("01_rootdepth/dat_nlraa-preds-smooth-for-fig.csv")

# fig ---------------------------------------------------------------------

#--need to fix date axis

fig_dat %>% 
  ggplot(aes(date, rootdepth_cm)) + 
  geom_line(aes(color = rot_trt, linetype = rot_trt), size = 1.5) +
  geom_point(size = 4, aes(fill = rot_trt, pch = rot_trt)) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_shape_manual(values = c(22, 24),
                     labels = c("Simple 2-year", "Complex 4-year")) +
  facet_grid(.~ year, scales = "free") +
  myth +
  scale_y_reverse() +
  scale_x_date(labels = date_format("%b"),
               breaks = "1 month",
               expand = c(0.1, 0.1)) +
  labs(x = NULL,
       y = "Maximum rooting depth (cm)",
       fill = "Rotation",
       color = "Rotation",
       shape = "Rotation",
       linetype = "Rotation") + 
  theme(#legend.position = "top",
        #legend.direction = "horizontal",
    legend.position = c(0.15, 0.15),
        legend.background = element_rect(color = "black"),
        legend.title.align = 0.5,
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)))



ggsave("03_manu-figs/fig_rootdepth-by-year.png", width = 7)


# fig with preds ----------------------------------------------------------

ggplot() + 
  geom_line(data = fig_preds, 
            aes(cum_gdd, pred, color = rot_trt, linetype = rot_trt), 
            size = 1.5) +
  geom_point(
    data = fig_dat_block,
    aes(cum_gdd, rootdepth_cm, fill = rot_trt, pch = rot_trt),
             size = 1.5, alpha = 0.4, stroke = 0.5) +
  facet_grid(.~ year, scales = "free") +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_shape_manual(values = c(22, 24),
                     labels = c("Simple 2-year", "Complex 4-year")) +
  myth +
  scale_y_reverse() +
  labs(x = "\nCumulative growing degree days (base 10 deg C)",
       y = "Maximum maize rooting depth (cm)\n",
       fill = "Rotation",
       color = "Rotation",
       shape = "Rotation",
       linetype = "Rotation") + 
  theme(#legend.position = "top",
    #legend.direction = "horizontal",
    axis.title = element_text(size = rel(1.2)),
    legend.position = c(0.15, 0.15),
    legend.background = element_rect(color = "black"),
    legend.title.align = 0.5,
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))


ggsave("03_manu-figs/fig_rootdepth-by-year-fitted.png", width = 7)



# fig with corn stages ----------------------------------------------------

gdds_all <- 
  read_csv("01_gdds/td_gdds.csv")

gdd_phen <- 
  mrs_phen %>% 
  select(year, date, doy, pl_stage) %>% 
  distinct() %>% 
  arrange(year, date, doy, pl_stage) %>% 
  group_by(year, date, doy) %>% 
  slice(1) %>% 
  left_join(gdds_all) %>% 
  rename(cum_gdd = cum_gdds)


gdd_phen %>% 
  filter(year == 2020)

#--only keep some of the stages (too cluttered)

phen_for_fig <- 
  gdd_phen %>%
  mutate(pl_stage_fig = case_when(
    (year == 2018 & doy == 152) ~ "V5",
    (year == 2018 & doy == 176) ~ "V12",
    (year == 2018 & doy == 194) ~ "R2",#--change this to 194-197
    (year == 2019 & doy == 177) ~ "V4", 
    (year == 2019 & doy == 205) ~ "V12",
    (year == 2019 & doy == 232) ~ "R2",
    (year == 2020 & doy == 155) ~ "V5",
    (year == 2020 & doy == 184) ~ "V13",
    (year == 2020 & doy == 205) ~ "R2"
  )) %>% 
  filter(!is.na(pl_stage_fig))


ggplot() + 
  geom_line(data = fig_preds, 
            aes(cum_gdd, pred, color = rot_trt, size = rot_trt), 
            #size = 1.5
            ) +
  geom_point(
    data = fig_dat_block,
    aes(cum_gdd, rootdepth_cm, fill = rot_trt, pch = rot_trt),
    size = 1.5, alpha = 0.4, stroke = 0.5) +
  geom_text(
    data = phen_for_fig,
    aes(x = cum_gdd, 
        y = -10, 
        label = pl_stage_fig,
        fontface = "italic", 
        check_overlap = T),
    color = "gray50") +
  facet_grid(.~ year, scales = "free") +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  # scale_linetype_manual(values = c("solid", "solid"),
  #                       labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_size_manual(values = c(1, 1.5),
                        labels = c("Simple 2-year", "Complex 4-year")) +
  scale_shape_manual(values = c(22, 24),
                     labels = c("Simple 2-year", "Complex 4-year")) +
  myth +
  scale_y_reverse() +
  labs(x = "\nCumulative growing degree days (base 10 deg C)",
       y = "Maximum maize rooting depth (cm)\n",
       fill = "Rotation",
       color = "Rotation",
       size = "Rotation",
       shape = "Rotation",
       linetype = "Rotation") + 
  theme(#legend.position = "top",
    #legend.direction = "horizontal",
    axis.title = element_text(size = rel(1.2)),
    legend.position = c(0.15, 0.15),
    legend.background = element_rect(color = "black"),
    legend.title.align = 0.5,
    legend.text = element_text(size = rel(1)),
    legend.title = element_text(size = rel(1)))


ggsave("03_manu-figs/fig_rootdepth-by-year-fitted-phen.png", width = 7)
