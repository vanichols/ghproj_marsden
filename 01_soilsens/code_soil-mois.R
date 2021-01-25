# created: 11/23/2020
# purpose: visualize soil sensor data
# last updated:

library(maRsden)
library(janitor)
library(ggthemes)
library(cowplot)
library(grafify)

theme_set(theme_bw())

ss <- mrs_soilsensors

sw <- ss %>% filter(sensor_unit == "soilVWC")

sw %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, value, group = plot_id)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(sensor_depth_cm ~ year, scales = "free")

sw %>% 
  group_by(plot_id) %>% 
  mutate(value_sc = scale(value)) %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, value_sc, group = plot_id)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(sensor_depth_cm ~ year, scales = "free")

sw %>% 
  group_by(plot_id) %>% 
  mutate(maxval = max(value),
         val_pctmax = value/maxval) %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, val_pctmax, group = plot_id)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(sensor_depth_cm ~ year, scales = "free")
