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
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(color = "black"),
        legend.title.align = 0.5,
        legend.text = element_text(size = rel(1)),
        legend.title = element_text(size = rel(1)))



fig_dat %>%   
  ggplot(aes(doy, rootdepth_cm, color = rot_trt2, fill = rot_trt2)) + 
  geom_point(size = 4, pch  = 21) +
  
  scale_linetype_manual(values = c("solid", "dashed"))
geom_point(size = 3, alpha = 0.5, aes(pch = rot_trt2)) +
  geom_linerange(aes(x = doy, 
                     ymin = rootdepth_cm - rootdepth_se, 
                     ymax = rootdepth_cm + rootdepth_se )) +
  geom_line() +
  facet_grid(.~year, scales = "free") + 
  scale_y_reverse() +
  theme_bw() + 
  scale_fill_manual(values = c(pnk1, dkbl1)) +
  scale_color_manual(values = c(pnk1, dkbl1)) + 
  labs(x = "Day of Year", 
       y = "Maximum Maize\nRooting Depth\n(cm)",
       color = NULL,
       pch = NULL) + 
  theme(legend.position = "top",
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.text = element_text(size = rel(1.2)),
        strip.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)))

#ggsave("01_rootdepth/fig_max-rooting-depth-2018-2020.png", width = 7)
