# created: 11/23/2020
# purpose: visualize soil sensor data
# last updated:

library(maRsden)
library(janitor)
library(ggthemes)
library(cowplot)


theme_set(theme_bw())

ss <- mrs_soilsensors
pk <- mrs_plotkey %>% filter(year == 2019)



fig_wt <- 
  ss %>%
  filter(sensor_unit == "wtdepth_mm") %>%
  left_join(pk) %>%
  ggplot(aes(date, value/10, group = plot_id)) + 
  geom_line(aes(color = plot_id), size = 3) + 
  guides(color = F) +
  scale_y_reverse(limits = c(300, 0)) + 
  labs(y = "Depth to Water Table (cm)",
       x = NULL,
       title = "Elevation Effects Water Table Depth")

#--what is the average difference across the season?
ss %>%
  filter(sensor_unit == "wtdepth_mm") %>%
  left_join(pk) %>%
  filter(doy<300, !doy %in% c(206, 232)) %>% #--no idea what happened these days
  select(year, doy, value, plot_id) %>% 
  pivot_wider(names_from = plot_id, values_from = value) %>%
  clean_names() %>% 
  mutate(dif = x2019_12 - x2019_43) %>% 
  summarise(mndif = mean(dif, na.rm = T)/10)

# 70 cm shallower on short end

#--where are these plots?
plot_map19 <- 
  mrs_plotcoords %>% 
  left_join(mrs_elevation %>% 
              select(plot, mean_elev_m, median)) %>%
  mutate(year = 2019,
         plot_id = paste(year, plot, sep = "_")) %>% 
  left_join(mrs_plotkey)


fig_plots <- 
  ggplot() + 
  geom_rect(data = plot_map19, 
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend, fill = mean_elev_m), 
            #fill = "gray", 
            color = "black") + 
  geom_rect(data = plot_map19 %>% filter(plot_id %in% c("2019_12", "2019_43")),
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend, color = plot_id), 
            fill = NA, size = 4) + 
  guides(color = F) +
  scale_fill_viridis_c() + 
  theme_map() + 
  theme(legend.position = "right") + 
  labs(fill = "Elevation (m)")

fig_plots  



ggdraw() +
  draw_plot(fig_wt) +
  draw_plot(fig_plots, x = 0.05, y = 0.1, width = 0.5, height = 0.3)

ggsave("01_soilsens/fig_wt-vs-elevation.png")
