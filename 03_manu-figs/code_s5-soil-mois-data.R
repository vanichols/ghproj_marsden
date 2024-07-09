#--show soil mois diffs using fem-gam fit
#--8//9/2021
#--1/24/2024 - add soil temperature


rm(list = ls())

library(maRsden)
library(tidyverse)
library(janitor)
library(patchwork)

source("03_manu-figs/palettes.R")

theme_set(theme_bw())

# data --------------------------------------------------------------------

#--what date is flowering?
dof <- 
  mrs_phen %>% 
  filter(year %in% c(2018, 2019)) %>% 
  select(year, doy, pl_stage) %>% 
  distinct() %>%
  filter(pl_stage != "VT") %>% 
  mutate(veg = str_sub(pl_stage, 1, 1),
         stg = parse_number(pl_stage)) %>% 
  filter(veg == "R") %>% 
  group_by(year) %>% 
  filter(doy == min(doy)) %>% 
  select(year, doy) %>% 
  rename("dof" = doy)

#--harvest? maturity? r3
r3 <- 
  mrs_phen %>% 
  filter(year %in% c(2018, 2019)) %>% 
  select(year, doy, pl_stage) %>% 
  distinct() %>%
  filter(pl_stage != "VT") %>% 
  mutate(veg = str_sub(pl_stage, 1, 1),
         stg = parse_number(pl_stage)) %>% 
  filter(veg == "R") %>% 
  group_by(year) %>% 
  filter(doy == max(doy)) %>% 
  select(year, doy) %>% 
  rename("day_r3" = doy)
  

dop <- 
  mrs_rootdepth %>%
  filter(year %in% c(2018, 2019)) %>% 
  ungroup() %>% 
  filter(rootdepth_cm == 0) %>% 
  select(year, doy) %>% 
  distinct() %>% 
  rename("dop" = doy)


phen <- 
  dop %>% 
  left_join(dof) %>% 
  left_join(r3) %>% 
  pivot_longer(dop:day_r3, names_to = "phen") %>% 
  filter(year != 2020)


# soil moisture -----------------------------------------------------------

sw <- 
  read_csv("01_soilsens/dat_soilsens-gam-fem.csv") 


fig_mois <- 
  sw %>% 
  mutate(depth_nice_label = ifelse(sensor_depth_cm == 15, "15 cm", "45 cm")) %>% 
  ggplot() + 
  geom_point(aes(x = doy, y = value, color = rot_trt),
             alpha = 0.1, pch = 19, size = 0.6) + 
  geom_line(aes(x = doy, y = Estimate, color = rot_trt), linewidth = 1.5) + 
  geom_ribbon(aes(x = doy, ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), 
              alpha = 0.4) + 
  #--planting etc.
  geom_vline(data = dop, aes(xintercept = dop), color = "black", linetype = "dotted") +
  geom_text(data = dop, aes(x = dop, y = 0.45, label = "Planting"),
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  geom_vline(data = dof, aes(xintercept = dof), color = "black", linetype = "dotted") +
  geom_text(data = dof, aes(x = dof, y = 0.45, label = "Silking"),
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  geom_vline(data = r3, aes(xintercept = day_r3), color = "black", linetype = "dotted") +
  geom_text(data = r3, aes(x = day_r3, y = 0.4, label = "Milk stage (R3)"),
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  scale_color_manual(values = c(ylw2, dkbl1),
                     labels = c("Short", "Extended")) + 
  scale_fill_manual(values = c(ylw2, dkbl1),
                    labels = c("Short", "Extended")) + 
  #  coord_cartesian(ylim = c(0.1, 0.45)) +
  scale_x_continuous(expand = expansion(add = c(15, 3))) +
  labs(x = "Day of year",
       y = "Soil moisture (vol. %)", 
       color = NULL,
       fill = NULL,
       title = "Soil Moisture") +
  guides(fill = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1)) +
  scale_y_continuous(label = label_percent(accuracy = 2)) +
  facet_grid(depth_nice_label~year, scales = "free") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank(),
        #legend.position = "top",
        legend.position = c(0.65, 0.05),
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.2)),
        strip.background = element_blank()) 

fig_mois
#ggsave("03_manu-figs/s5_soil-mois.png",  width = 6.93, height = 7.2)


# differences in soil temp ------------------------------------------------

st <- 
  read_csv("01_soilsens/dat_soilsens-gam-fem-soilT.csv") 

d_est <- 
  st %>% 
  #  filter(sensor_depth_cm == 15) %>% 
  select(year, doy, rot_trt, sensor_depth_cm, Estimate) %>% 
  distinct() %>% 
  pivot_wider(names_from = rot_trt, values_from = Estimate) %>% 
  janitor::clean_names() %>% 
  mutate(warmer_by = x4y - x2y) %>% 
  select(year, doy, sensor_depth_cm, warmer_by)

d_lo <- 
  st %>% 
  select(year, sensor_depth_cm, doy, rot_trt, Estimate, Q2.5) %>% 
  distinct() %>% 
  mutate(warmer_by_lo = Estimate - `Q2.5`) %>% 
  group_by(year, doy, sensor_depth_cm) %>% 
  summarise(warmer_by_lo = mean(warmer_by_lo))

d_hi <- 
  st %>% 
  #  filter(sensor_depth_cm == 15) %>% 
  select(year, doy, sensor_depth_cm, rot_trt, Estimate, Q97.5) %>% 
  distinct() %>% 
  mutate(warmer_by_hi = `Q97.5` - Estimate) %>% 
  group_by(year, sensor_depth_cm, doy) %>% 
  summarise(warmer_by_hi = mean(warmer_by_hi))


fig_tempdeg <- 
  d_est %>% 
  left_join(d_hi) %>% 
  mutate(depth_nice_label = ifelse(sensor_depth_cm == 15, "15 cm", "45 cm")) %>% 
  ggplot(aes(doy, warmer_by)) + 
  geom_line(color = rd2, size = 1.5) + 
  geom_ribbon(aes(x = doy, ymin = warmer_by - warmer_by_hi, ymax = warmer_by + warmer_by_hi), 
              fill = rd2, alpha = 0.4) + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  facet_grid(depth_nice_label ~ year) + 
  labs(x = "Day of Year",
       y = "Extended rotation soil\nis warmer by ___ deg C", 
       fill = NULL,
       title = "Soil Temperature") + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.2)),
        strip.background = element_blank()) 


# stacked -----------------------------------------------------------------

fig_mois/fig_tempdeg

ggsave("03_manu-figs/s5_soil-mois-and-temp.png",  width = 6.93, height = 10)
