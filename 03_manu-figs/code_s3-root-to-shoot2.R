# try to do growth analysis
# goal: calc root to shoot
# started 8/19/2022
#--ran through/renamed: 1/19/2024
#--made into a range 1/22/2024

library(maRsden)
library(tidyverse)
library(janitor)

rm(list = ls())



# fig stuff ---------------------------------------------------------------

source("03_manu-figs/palettes.R")

theme_set(theme_bw())


myth <- 
  theme(strip.text = element_text(size = rel(1.2)),
        strip.background = element_blank(),
        axis.text = element_text(size = rel(1.1)))


# data --------------------------------------------------------------------

dat <-
  read_csv("01_growth-analysis/dat_growth-analysis.csv")

dat %>% 
  filter(yearF %in% c(2019, 2020)) %>% 
  ggplot(aes(doy, mass_gpl)) + 
  geom_point(aes(color = rot_trt)) + 
  facet_grid(.~yearF)

#--take the maximum value, doesn't separate by block
mx_bio <- 
  dat %>% 
  group_by(yearF, rot_trt) %>% 
  mutate(max_mass_gpl = max(mass_gpl)) %>% 
  select(yearF, rot_trt, max_mass_gpl) %>% 
  distinct()

#--get separate values per block? I didn't fit it by block. I guess I could. I did it. 
mx_bio <-
  read_csv("01_growth-analysis/dat_growth-anal-params-block.csv") %>% 
  filter(names == "Asym") %>% 
  rename(mass_gpl = x) %>% 
  select(yearF, block, rot_trt, mass_gpl) %>% 
  # #--hmm what was the stand count
  # # in 2019 it was 123,551 pl/ac on average. Use this.
  mutate(pl_ac = 123551,
         pl_ha = pl_ac*2.47,
         biokg_ha = mass_gpl * pl_ha / 1000) %>% 
  select(yearF, block, rot_trt, biokg_ha)


# root data ---------------------------------------------------------------

# rts_orig <- read_csv("01_rootdist-ml/dat_roots-added.csv")


# Average the first sampling, and the last sampling by depth 
#--Take difference to get 'minimum root production'
#--Last sampling value represents 'maximum root production'

#--get only the first and last sampling dates
te <- 
  mrs_rootdist_ml %>% 
  left_join(mrs_plotkey, relationship = "many-to-many") %>% 
  #--getting rid of plot by averaging
  group_by(year, rot_trt, dap, depth) %>% 
  summarise(roots_kgha = mean(roots_kgha, na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(dap_first = min(dap),
         dap_last = max(dap)) %>% 
  filter((dap == dap_first)|(dap == dap_last)) %>% 
  mutate(dap = ifelse(dap == dap_first, "first", "last"))

#--average them to get one value for each rotation, depth, and sampling point
#--keep year separate
te_avg <- 
  te %>%
  pivot_wider(names_from = dap, values_from = roots_kgha) %>% 
  #--calculate min and maxes
  mutate(roots_min = ifelse (last > first, last - first, 0),
         roots_max = last) %>% 
  group_by(year, rot_trt, depth) %>% 
  summarise(roots_min = mean(roots_min, na.rm = T),
            roots_max = mean(roots_max, na.rm = T))

rts <- 
  te_avg %>% 
  group_by(year, rot_trt) %>% 
  summarise(roots_min = sum(roots_min),
            roots_max = sum(roots_max)) %>% 
  mutate(depth = "Total (0-60 cm)") %>% 
  select(-depth) %>% 
  pivot_longer(roots_min:roots_max) %>% 
  rename(roots_added_kgha = name) 
  


# calculate ratio ---------------------------------------------------------

rs_ratio <- 
  rts %>%
  rename(yearF = year) %>% 
  left_join(mx_bio) %>%
  mutate(rs_ratio = value / biokg_ha, 
         yearF = as.factor(yearF)) 

rs_ratio %>% 
  summarise(mn = min(rs_ratio),
            mx = max(rs_ratio))

rs_ratio %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Short rotation", "Extended rotation"),
         roots_added_kgha = ifelse(roots_added_kgha == "roots_max", "100%", "0%")) %>% 
  ggplot(aes(as.factor(yearF), rs_ratio, color = rot_trt)) + 
  geom_jitter(aes(shape = roots_added_kgha), size = 3, width = 0.2) + 
  scale_color_manual(values = c(pnk1, dkbl1)) + 
  facet_grid(.~yearF, scales = "free") + 
  labs(x = NULL,
       y = "Root to shoot mass ratio",
       color = NULL,
       shape = "Background root decomposition assumption",
       title = "Ratio of maize root biomass from 0-60 cm\nto maximum aboveground maize biomass", 
       caption = )


ggsave("03_manu-figs/s3_root-to-shoot.png",  width = 6.93, height = 4.12)
  