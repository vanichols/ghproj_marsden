# try to do growth analysis
# goal: calc root to shoot
# started 8/19/2022

library(maRsden)
library(tidyverse)
library(janitor)

rm(list = ls())


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
  select(yearF, block, rot_trt, mass_gpl)


rts <- read_csv("01_rootdist-ml/dat_roots-added.csv")

#--hmm what was the stand count
# in 2019 it was 123,551 pl/ac on average. Can I just use this?

rs_ratio <- 
  rts %>% 
  rename(yearF = year) %>% 
  left_join(mx_bio) %>% 
  filter(!is.na(roots_added_kgha),
         roots_added_kgha > 0) %>% 
  mutate(pl_ac = 123551,
         pl_ha = pl_ac*2.47,
         kg_ha = mass_gpl * pl_ha / 1000,
         rs_ratio = roots_added_kgha / kg_ha, 
         yearF = as.factor(yearF)) 


rs_ratio %>% 
  ggplot(aes(as.factor(yearF), rs_ratio, color = rot_trt)) + 
  geom_point()


rs_ratio %>% 
  ggplot(aes(rot_trt, rs_ratio, color = rot_trt)) + 
  geom_point() +
  stat_summary(size = 2)rs_ratio

m1 <- lmer(rs_ratio ~ rot_trt*yearF + (1|yearF:block), data = rs_ratio)  

anova(m1)

emmeans(m1, specs = c("rot_trt"))
