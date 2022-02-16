# Gina
# show maximum root deptsh
# created: 2/15/2022


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
  mutate(rot_trt2 = ifelse(rot_trt == "2y", "Simple", "Complex")) 



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
  scale_x_date(labels = date_format("%d-%b"),
               breaks = "1 month") +
  labs(x = "Date",
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
