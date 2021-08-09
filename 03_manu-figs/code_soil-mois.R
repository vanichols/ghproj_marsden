#--combine yields, growth rates, harvest indices
# created 8/9/2021


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


sigdifs <- 
  read_csv("01_soilsens/dat_soilsens-gam.csv") %>% 
  filter(type == "sqrt vals",
         depth_cm == "15cm") %>% 
  left_join(phen) %>% 
  filter(phen == "day_r3") %>% 
  filter(doy <= value) %>% 
  select(-phen, -value)
  
sigdifs %>% 
  ggplot(aes(doy, diff)) + 
  geom_line() + 
  facet_grid(.~year) 
  
sdry <- 
  sigdifs %>% 
  filter(lower > 0) %>% 
  mutate(st = "drier") %>% 
  filter(diff > 0.02)

swet <- 
  sigdifs %>% 
  filter(upper < 0) %>% 
  mutate(st = "wetter") %>% 
  filter(diff < 0.02)

#--make white tile for sig bars background

#--get min/max doy with data
mrs_soilsensors %>% 
  select(year, doy) %>% 
  distinct() %>% 
  group_by(year) %>% 
  filter(doy == min(doy))

r3

dumdat <- 
  tibble(doy = seq(139, 212, 1),
         year = 2018) %>% 
  bind_rows(
    tibble(doy = seq(158, 232, 1),
           year = 2019))


mrs_soilsensors %>% 
  filter(sensor_unit == "soilVWC",
         sensor_depth_cm == 15) %>%
  left_join(mrs_plotkey) %>% 
  left_join(r3) %>% 
  filter(doy <= day_r3) %>% 
  group_by(year, doy, rot_trt) %>% 
  summarise(value = mean(value, na.rm = T)) %>% 
  ggplot() + 
  geom_vline(data = dop, aes(xintercept = dop), color = "black", linetype = "dotted") +
  geom_text(data = dop, aes(x = dop, y = 0.45, label = "Planting"),
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  geom_vline(data = dof, aes(xintercept = dof), color = "black", linetype = "dotted") +
  geom_text(data = dof, aes(x = dof, y = 0.45, label = "Silking"),
            check_overlap = T, hjust = 0.5, fontface = "italic") +
  geom_line(aes(doy, value, color = rot_trt), size = 2) + 
  geom_tile(data = dumdat, 
            aes(x = doy, y = 0.1, height = 0.05), fill = "gray90") +
  # geom_tile(data = swet, 
  #           aes(x = doy, y = 0.1, height = 0.05), fill = ltbl1) +
  geom_tile(data = sdry, 
            aes(x = doy, y = 0.1, height = 0.05), fill = dkpr2) +
  scale_color_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple", "Complex")) + 
  coord_cartesian(ylim = c(0.1, 0.45)) +
  scale_x_continuous(expand = expansion(add = c(10, 3))) +
  labs(x = "Day of year",
       y = "Soil moisture (vol. %)", 
       color = "Rotation") +
  scale_y_continuous(label = label_percent(accuracy = 2)) +
  facet_grid(.~year, scales = "free") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_blank(), 
      #axis.ticks.x = element_blank(),
      legend.position = "top",
      legend.background = element_rect(color = "black"),
      legend.title = element_text(size = rel(1.2)),
      legend.text = element_text(size = rel(1.1)),
      strip.text = element_text(size = rel(1.2))) 
  
ggsave("03_manu-figs/fig_soil-mois.png", height = 4, width = 7.4)
