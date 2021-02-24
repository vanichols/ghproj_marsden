# started 7/29/2020
# updated: 2/24/2021
# goal: are there more green leaves in the 4 yr for longer?

library(tidyverse)
library(maRsden)


# data --------------------------------------------------------------------

data("mrs_phen")
data("mrs_plotkey")

d <- 
  mrs_phen %>% 
#--fill in some things that can be done using our brains
  mutate(
    devleaves_nu = case_when(
      (year == 2018 & pl_stage == "V4") ~ 4,
      (year == 2018 & pl_stage == "V6") ~ 6,
      #(year == 2018 & pl_stage == "V14") ~ 14,
      TRUE ~ devleaves_nu),
    grleaves_nu = case_when(
      (year == 2018 & pl_stage == "V4") ~ devleaves_nu,
      (year == 2018 & pl_stage == "V6") ~ devleaves_nu,
      #(year == 2018 & pl_stage == "V14") ~ devleaves_nu,
      TRUE ~ grleaves_nu)
  )

d %>% 
  filter(year == 2020) ->a


# look at it --------------------------------------------------------------

#--brown stacked on green. Is this right?
d %>% 
  left_join(mrs_plotkey) %>%
  mutate(dead = devleaves_nu - grleaves_nu) %>% 
  group_by(year, date, rot_trt) %>% 
  summarise(
    pot = mean(devleaves_nu, na.rm = T),
    gr = mean(grleaves_nu, na.rm = T),
    br = mean(dead, na.rm = T)) %>%  
  filter(!is.nan(br)) %>% 
  pivot_longer(pot:br) %>%
  filter(name !="pot") %>% 
  ggplot(aes(rot_trt, value, fill = name)) +
  geom_col(position = "stack", color = "black", aes(alpha = rot_trt)) + 
  labs(y = "Number of leaves",
       fill = "leaves") +
  scale_fill_manual(values = c("gr" = "green4",
                               "br" = "gold4")) +
  facet_grid(year~date)

ggsave("01_phen/fig_deadleaves.png")

#--start simpler 
d_simp <- 
  d %>% 
  left_join(mrs_plotkey) %>%
  filter(!is.na(grleaves_nu)) %>% 
  select(year, date, doy, rot_trt, block, plot_id, pl_stage, devleaves_nu, grleaves_nu) %>% 
  mutate(
    totleaves = devleaves_nu,
    green = grleaves_nu,
    dead = totleaves - green)


d_simp %>% 
  group_by(year, doy, rot_trt) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  ggplot() +
  geom_point(aes(doy, totleaves), color = "black") +
  geom_point(aes(doy, green), color = "green") +
  geom_point(aes(doy, dead), color = "brown") +
  facet_grid(rot_trt ~year)


d_simp %>% 
  group_by(year, doy, rot_trt) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  ggplot() +
  geom_ribbon(aes(doy, ymin = 0, ymax = totleaves), fill = "black") +
  geom_ribbon(aes(doy, ymin = 0, ymax = green), fill = "green") +
  geom_ribbon(aes(doy, ymin = 0, ymax = dead), fill = "brown") +
  facet_wrap(~year + rot_trt, scales = "free_x", ncol = 2)


d_simp %>% 
  group_by(year, doy, rot_trt) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  ggplot() +
  geom_ribbon(aes(doy, ymin = 0, ymax = totleaves), fill = "brown") +
  geom_ribbon(aes(doy, ymin = 0, ymax = green), fill = "green4") +
  facet_wrap(~year + rot_trt, scales = "free_x", ncol = 2)
