#--show soil mois diffs using fem-gam fit
#--8//9/2021

library(maRsden)
library(tidyverse)
library(janitor)
library(patchwork)

source("03_manu-figs/palettes.R")

theme_set(theme_bw())

mt <- 
  theme(legend.background = element_rect(color = "black"),
      legend.title = element_text(size = rel(1.2)),
      legend.text = element_text(size = rel(1.1)),
      strip.text = element_text(size = rel(1.2))) 




# data --------------------------------------------------------------------

pd <- 
  read_csv("01_penet/dat_penet-gam.csv") %>%
  mutate(year_num = str_sub(year, 2, -1)) %>% 
  arrange(year_num, doy) %>% 
  mutate(
    samp_nice = case_when(
      (doy == 130 & year == "Y2018") ~ "planting",
      (doy == 162 & year == "Y2019") ~ "planting",
      (doy == 197 & year == "Y2018") ~ "late season",
      (doy == 175 & year == "Y2020") ~ "late season",
      TRUE ~ "uhoh"),
    samp_nice2 = paste(year_num, samp_nice),
    samp_nice = fct_inorder(samp_nice),
    samp_nice2 = fct_inorder(samp_nice2))


pd %>% 
  select(year, doy, samp) %>% 
  distinct()

#--tillage is at 6 in, cultivator before planting
#--9 in, moldboard prev fall


pd %>% 
  select(samp_nice2) %>% 
  distinct()



# all tog? ----------------------------------------------------------------

#--facet grid
# pd %>% 
#   mutate(cult_val = 15.24,
#          mold_val = 22.86,
#          cult_lab = ifelse(year_num == 2018 & samp_nice == "planting", "Cultivation at planting", NA),
#          mold_lab = ifelse(year_num == 2018 & samp_nice == "planting", "Moldboard previous fall,\nExtended only", NA)) %>% 
#   ggplot() + 
#   geom_vline(aes(xintercept = cult_val), color = "gray80", size = 2) +
#   geom_text(aes(x = 13, y = 5, label = cult_lab),
#             fontface = "italic", check_overlap = T, hjust = 1) +
#   geom_vline(aes(xintercept = mold_val), color = "gray80", size = 2) +
#   geom_text(aes(x = 22.5, y = 5, label = mold_lab),
#             fontface = "italic", check_overlap = T, hjust = 1) +
#   geom_jitter(aes(x = depth_cm, y = resis_Mpa, color = rot_trt),
#               width = 0.5, alpha = 0.05, pch = 19, size = 0.6) + 
#   geom_line(aes(x = depth_cm, y = Estimate, color = rot_trt)) + 
#   geom_ribbon(aes(x = depth_cm, ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), 
#               alpha = 0.4) + 
#   scale_color_manual(values = c(ylw2, dkbl1),
#                      labels = c("Short", "Extended")) + 
#   scale_fill_manual(values = c(ylw2, dkbl1),
#                     labels = c("Short", "Extended")) + 
#   scale_x_reverse() +
#   labs(y = "Soil resistance (MPa)",
#        x = "Soil depth (cm)",
#        fill = "Rotation",
#        color = "Rotation") +
#   coord_flip() + 
#   facet_grid(year_num ~ samp_nice) + 
#   mt + 
#   theme(legend.position = "bottom")


#ggsave("03_manu-figs/fig_penet.png", width = 5.52, height = 9.6)


#--facet wrap
pd %>% 
  mutate(cult_val = 15.24,
         mold_val = 22.86,
         cult_lab = ifelse(year_num == 2018 & samp_nice == "planting", "Cultivation at planting", NA),
         mold_lab = ifelse(year_num == 2018 & samp_nice == "planting", "Moldboard previous fall,\nextended only", NA)) %>% 
  ggplot() + 
  geom_vline(aes(xintercept = cult_val), color = "gray80", size = 2) +
  geom_text(aes(x = 13, y = 5, label = cult_lab),
            fontface = "italic", check_overlap = T, hjust = 1) +
  geom_vline(aes(xintercept = mold_val), color = "gray80", size = 2) +
  geom_text(aes(x = 22.5, y = 5, label = mold_lab),
            fontface = "italic", check_overlap = T, hjust = 1) +
  geom_jitter(aes(x = depth_cm, y = resis_Mpa, color = rot_trt),
              width = 0.5, alpha = 0.05, pch = 19, size = 0.6) + 
  geom_line(aes(x = depth_cm, y = Estimate, color = rot_trt)) + 
  geom_ribbon(aes(x = depth_cm, ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), 
              alpha = 0.4) + 
  scale_color_manual(values = c(ylw2, dkbl1),
                     labels = c("Short", "Extended")) + 
  scale_fill_manual(values = c(ylw2, dkbl1),
                    labels = c("Short", "Extended")) + 
  scale_x_reverse() +
  labs(y = "Soil resistance (MPa)",
       x = "Soil depth (cm)",
       fill = "Rotation",
       color = "Rotation") +
  coord_flip() + 
  facet_wrap( ~ samp_nice2) + 
  mt + 
  theme(legend.position = "bottom",
        strip.background = element_blank())


ggsave("03_manu-figs/s4_penet.png", width = 5.9, height = 6.17)

