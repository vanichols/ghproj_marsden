#--weather years and yields
#--created sept 15 2021
#---currently using mesonet 2020 data that hasn't been qc'd by sotirios

rm(list = ls())
library(maRsden)
library(tidyverse)
library(janitor)
library(patchwork)
library(ggrepel)

source("03_manu-figs/palettes.R")

theme_set(theme_bw())

myth <- 
  theme(strip.text = element_text(size = rel(1.2)),
      strip.background = element_blank(),
      axis.text = element_text(size = rel(1.1)))

mghalab <- (expression(atop("Maize dry grain yield", paste("(Mg "~ha^-1*")"))))


# yield line graph --------------------------------------------------------

dat <- mrs_cornylds %>% filter(year > 2012)

fig_ylds <- 
  dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  ggplot(aes(year, yield_Mgha)) + 
  geom_line(aes(color = rot_trt, linetype = rot_trt), size = 1.5) +
  geom_point(size = 4, aes(fill = rot_trt, pch = rot_trt)) +
  scale_x_continuous(breaks = c(seq(from = 2004, to = 2020, by = 2))) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_shape_manual(values = c(22, 24),
                     labels = c("Simple 2-year", "Complex 4-year")) +
  labs(x = "Year",
       y = mghalab,
       fill = "Rotation",
       color = "Rotation",
       shape = "Rotation",
       linetype = "Rotation") + 
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.title.align = 0.5,
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)))


fig_ylds

# weather x-y fig ---------------------------------------------------------

#--growing season precip
ptot_gs <- 
  mrs_wea %>% 
  left_join(mrs_cornplant) %>% 
  filter(!is.na(plant_date)) %>% 
  filter(day >= plant_doy,
         day <= harv_doy) %>% 
  group_by(year)  %>% 
  summarise(tp = sum(rain_mm, na.rm = T)) 

ptot_gs_longterm <- 
  mrs_wea %>% 
  filter(day > 105,
         day < 288) %>% 
  group_by(year)  %>% 
  mutate(tp = sum(rain_mm)) %>% 
  ungroup() %>% 
  summarise(tp = mean(tp, na.rm = T)) %>% 
  pull(tp)

tav_gs <- 
  mrs_wea %>% 
  left_join(mrs_cornplant) %>% 
  filter(!is.na(plant_date)) %>% 
  filter(day >= plant_doy,
         day <= harv_doy) %>% 
  mutate(tav = (maxt_c + mint_c)/2) %>% 
  group_by(year)  %>% 
  summarise(tav = mean(tav, na.rm = T)) 

#--just say april 15 (doy 105) - oct 15 (doy288)

tav_gs_longterm <- 
  mrs_wea %>% 
  filter(day > 105, 
         day < 288) %>% 
  mutate(tav = (maxt_c + mint_c)/2) %>% 
  group_by(year)  %>% 
  summarise(tav = mean(tav, na.rm = T)) %>% 
  summarise(tav = mean(tav)) %>% 
  pull(tav)

yyrs <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
bioyrs <- c(2013, 2014, 2018, 2019, 2020)
ryrs <- c(2018, 2019, 2020)

ptot_gs_dat <- 
  ptot_gs %>% 
  filter(year > 2012) %>% 
  mutate(yield_dat = ifelse(year %in% yyrs, "Yield data", NA),
         root_dat = ifelse(year %in% ryrs, "root data", NA),
         bio_dat = ifelse(year %in% bioyrs, "growth analysis", NA)) %>% 
  unite(yield_dat, bio_dat, root_dat, col = "msmt", sep = ", ") %>% 
  mutate(msmt = str_squish(msmt),
         msmt = str_remove_all(msmt, "NA, "),
         msmt = str_remove_all(msmt, ", NA"))

yld_diffs <- 
  mrs_cornylds %>% 
  filter(year %in% yyrs) %>% 
  filter(rot_trt != "3y") %>% 
  group_by(year, rot_trt) %>% 
  summarise(yld = mean(yield_Mgha, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = yld) %>% 
  janitor::clean_names() %>% 
  mutate(yld_diff = x4y - x2y)

set.seed(125)
fig_wea <- 
  ptot_gs_dat %>%
  left_join(tav_gs) %>%
  left_join(yld_diffs) %>% 
  ggplot(aes(tp, tav)) +
  geom_hline(yintercept = tav_gs_longterm, linetype = "dashed", color = "gray70") +
  geom_vline(xintercept = ptot_gs_longterm, linetype = "dashed", color = "gray70") +
  geom_point(aes(fill = msmt, size = yld_diff), color = "black", pch = 21) +
  geom_text(aes(x = 400, y = 21.5, label = "Hot and dry"),
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text(aes(x = 750, y = 21.5, label = "Hot and wet"),
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text(aes(x = 400, y = 18, label = "Cool and dry"),
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text(aes(x = 750, y = 18, label = "Cool and wet"),
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text_repel(aes(label = year)) +
  scale_fill_manual(values = c("Yield data" = "gray60",
                                ltbl1, bl2)) + 
  guides(size = F) +
  labs(size = expression("Yield advantage of complex rotation ("~Mg~ha^-1*")"),
       #size = (expression(atop("Yield advantage\nof complex rotation", paste("(Mg "~ha^-1*")")))),
       color = "Measurement set",
       fill = "Measurement set",
       x = "Growing season precipitation (mm)",
       y = expression("Mean air temperature ("*~degree*C*")")) + 
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.title.align = 0.5,
        legend.title = element_text(size = rel(1)),
        legend.text = element_text(size = rel(1)),
        panel.grid = element_blank()) 


fig_wea

# pathcwork ---------------------------------------------------------------

library(patchwork)
set.seed(9)
fig_ylds + fig_wea + plot_layout(widths = c(1.5, 1))

ggsave("03_manu-figs/fig_yields-wea-gs.png", width = 9.15, height = 4.9)
