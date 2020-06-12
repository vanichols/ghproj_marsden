library(maRsden)
library(tidyverse)

#---look at matt's root distribution data


a <- mrs_rootdist_ml %>% as_tibble() %>% 
  left_join(mrs_plotkey) %>% 
  mutate(depth_cm = as_factor(depth))


# growth relative to previous sampling ------------------------------------

a %>% 
  ggplot(aes(fct_rev(depth_cm), root_weights_g, group = block)) + 
  geom_line() + 
  facet_grid(rot_trt ~ days_after_planting) + 
  coord_flip()

a %>% 
  ggplot(aes(fct_rev(depth_cm), root_weights_g, color = rot_trt, group = interaction(days_after_planting, rot_trt))) +
  stat_summary(fun = "mean", geom = "line") +
  facet_grid(. ~ days_after_planting) + 
  coord_flip()

base <- 
  a %>% 
  filter(days_after_planting == 3) %>% 
  select(plot_id, depth, root_weights_g) %>% 
  rename(roots_3dap = root_weights_g)

a %>% 
  filter(days_after_planting != 3) %>%
  select(date, plot_id, block, rot_trt, days_after_planting, depth, root_weights_g) %>% 
  left_join(base) %>% 
  mutate(roots_scaled = root_weights_g - roots_3dap)


root_diffs <- 
  a %>% 
  mutate(dap = paste0("x_dap_", days_after_planting)) %>% 
  select(-date, -days_after_planting, 
         -total_soil_weight_g_4_subsamples_bulked, 
         -soil_volume_cm_3, -mass_volume_g_cm_3) %>% 
  pivot_wider(names_from = dap, values_from = root_weights_g) %>% 
  mutate(dap_3to39 = x_dap_39 - x_dap_3,
         dap_39to56 = x_dap_56 - x_dap_39,
         dap_56to74 = x_dap_74 - x_dap_56,
         dap_74to105 = x_dap_105 - x_dap_74) %>% 
  select(-starts_with("x_")) %>% 
  pivot_longer(dap_3to39:dap_74to105) %>% 
  mutate(#value = ifelse(value < 0, 0, value),
    name = factor(name, levels = c("dap_3to39", "dap_39to56", "dap_56to74", "dap_74to105")),
    depth = factor(depth))

root_diffs %>% 
  ggplot(aes(fct_rev(depth), value, group = interaction(rot_trt), color = rot_trt)) +
  geom_point(alpha = 0.5) +
  stat_summary(fun= "mean", geom = "line", size = 1
  ) +
  stat_summary(fun= "mean", geom = "point", size = 3
  ) +
  #geom_hline(yintercept = 1) +
  scale_y_log10() +
  coord_flip() + 
  facet_grid(.~name) + 
  labs(y = "log(New Roots Added)")


# mass balance ------------------------------------------------------------

b <- 
  a %>% 
  select(date, plot_id, days_after_planting, depth, mass_volume_g_cm_3, rot_trt) %>% 
  rename(dap = days_after_planting,
         roots_gcm3 = mass_volume_g_cm_3) %>% 
  filter(dap == 3) %>% 
  #--change to kg/ha
  mutate(roots_kgha = roots_gcm3 * 100^3)

c <- b %>% 
  group_by(rot_trt, plot_id) %>% 
  summarise(roots_kgha = sum(roots_kgha)) %>% 
  mutate(depth = "0-60cm") %>% 
  bind_rows(b) %>% 
  mutate(depth = factor(depth, levels = c("0-15cm", "15-30cm", "30-45cm", "45-60cm", "0-60cm"))) %>% 
  select(-roots_gcm3, -date, -dap)

c %>% 
  group_by(rot_trt, depth) %>% 
  summarise(roots_kgha = mean(roots_kgha)) %>% 
  ggplot(aes(fct_rev(depth), roots_kgha, fill = rot_trt)) + 
  geom_col(position = "dodge") + 
  coord_flip() + 
#  facet_grid(.~rot_trt) + 
  labs(x = NULL) +
  geom_vline(xintercept = 1.5) +
  scale_fill_manual(values = c("orange", "purple")) +
  labs(x = NULL,
       y = "Root Mass (kg/ha) at planting",
       title = "Root Mass Carryover",
       subtitle = "Amount of root material present at planting") + 
  theme(legend.position = "bottom")



#--read in root dist data to get rough %C and %N

#--corn = 1.39% N
#--soy = 2.08% N

d <- c %>% 
  mutate(rootN_pct = case_when(
    rot_trt == "2y" ~ 1.39,
    rot_trt == "4y" ~ 2.08),
    rootN_kgha = roots_kgha * rootN_pct/100)

d %>% 
  ggplot(aes(fct_rev(depth), rootN_kgha, fill = rot_trt)) + 
  geom_col(position = "dodge") + 
  coord_flip() + 
  geom_vline(xintercept = 1.5) +
  labs(x = NULL,
       y = "Root N (kg/ha) at planting",
       title = "Root N Carryover",
       subtitle = "Assuming corn roots are 1.4% N, alfalfa roots are 2% N") + 
  theme(legend.position = "bottom")


# microbial biomass -------------------------------------------------------

al <- tibble(
  rot_trt = c("4y", "2y"),
  MBN_ugNgsoil = c(50, 35))

al %>% 
  mutate(
