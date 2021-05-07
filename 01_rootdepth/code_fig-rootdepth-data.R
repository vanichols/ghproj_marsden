# Gina
# combine root depth data, phenology, gdd, elevation
# created: 10/13/2020


rm(list=ls())
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(patchwork)
library(maRsden)

mrs_rootdepth


# look at it --------------------------------------------------------------

mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, rootdepth_cm, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5) + 
  facet_grid(.~year) + 
  scale_y_reverse()


mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>%
  group_by(year, doy, date, rot_trt) %>% 
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T)) %>% 
  filter(doy < 240) %>% 
  mutate(rot_trt2 = ifelse(rot_trt == "2y", "Maize following soybean", "Maize following alfalfa")) %>% 
  ggplot(aes(doy, rootdepth_cm, color = rot_trt2)) + 
  geom_point(size = 3, alpha = 0.5, aes(pch = rot_trt2)) +
  geom_line() +
  facet_grid(.~year, scales = "free") + 
  scale_y_reverse() +
  theme_bw() + 
  scale_color_manual(values = c("dodgerblue", "gold4")) + 
  labs(x = "Day of Year", 
       y = "Maximum Maize\nRooting Depth\n(cm)",
       color = NULL,
       pch = NULL) + 
  theme(legend.position = "top",
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.text = element_text(size = rel(1.2)),
        strip.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.2)))

ggsave("01_rootdepth/fig_max-rooting-depth-2018-2020.png", width = 7)
