# created: 11/23/2020
# purpose: visualize soil sensor data
# last updated:

library(maRsden)
library(janitor)
library(ggthemes)
library(cowplot)


theme_set(theme_bw())

pen <- mrs_penetrom
rd <- mrs_rootdepth
pk <- mrs_plotkey %>% filter(year %in% c(2018, 2019, 2020))


p1 <- 
  pen %>%
  left_join(pk) %>% 
  group_by(year, date, doy, depth_cm, harv_crop) %>% 
  summarise(resis_kpa = mean(resis_kpa, na.rm = T)) %>% 
  ggplot(aes(x = depth_cm, y = resis_kpa, color = harv_crop, group = interaction(harv_crop, date))) + 
  geom_vline(xintercept = 10, linetype = "dotted") +
  geom_vline(xintercept = 30, linetype = "dotted") +
  geom_rect(xmin = 10, xmax = 30, ymin = 0, ymax = 3000, fill = "blue4", alpha = 0.5) +
  geom_line() + 
  scale_x_reverse() +
  #coord_flip() +
  facet_grid(year~.)

p1

p2 <- 
  rd %>% 
  left_join(pk) %>% 
  group_by(year, date, doy, harv_crop) %>% 
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T)) %>% 
  ggplot(aes(doy, rootdepth_cm)) + 
  geom_line(aes(color = harv_crop)) +
  geom_hline(yintercept = 10, linetype = "dotted") +
  geom_hline(yintercept = 30, linetype = "dotted") +
  scale_y_reverse() +
  facet_grid(year~., scales = "free")

library(patchwork)
p2 + p1 + 
  plot_annotation(
    title = 'Penetration resistance and root depths',
    subtitle = 'Root differences continue past differences in penetration resistance',
    caption = 'Dotted lines indicate depth regions where C4 sig < C2'
    
  )


pen %>%
  left_join(pk) %>%
  group_by(year, date, depth_cm, harv_crop) %>%
  summarise(resis_kpa = mean(resis_kpa, na.rm = T)) %>%
  left_join(
    rd %>%
      left_join(pk) %>%
      group_by(year, date, doy, harv_crop) %>%
      summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T))
  )
