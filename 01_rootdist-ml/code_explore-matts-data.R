# Gina
# created 7/21/2021
# notes: I'm getting less roots at the end of season compared to beg
#        matt said try doing top layers, or doing it by layer

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)

# mrs_rootdist_mlsum %>% 
#   left_join(mrs_plotkey) %>% 
#   select(date, dap, block, plot, rot_trt, harv_crop, roots_kgha) %>% 
#   arrange(date, block, rot_trt) %>% 
#   write_csv("01_rootdist-ml/dat_matt-compare.csv")

#--note, eliminate plot 22 on day XX

rm_sum <- 
  mrs_rootdist_mlsum %>% 
  group_by(year) %>% 
  mutate(min_dap = min(dap),
         max_dap = max(dap),
         minmax_x = ifelse(dap == min_dap, "beg", 
                           ifelse(dap == max_dap, "end", NA))) %>% 
  filter(!is.na(minmax_x)) %>% 
  left_join(mrs_plotkey)

mrs_rootdist_mlsum %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(dap, roots_kgha, group = plot_id)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~year)

rm_sum_added <- 
  rm_sum %>% 
  select(year, block, rot_trt, minmax_x, roots_kgha) %>% 
  pivot_wider(names_from = minmax_x, values_from = roots_kgha) %>% 
  mutate(roots_added_kgha = end - beg)

rm_sum_added %>% write_csv("01_rootdist-ml/dat_roots-added.csv")

rm_sum_added %>% 
  group_by(year, rot_trt) %>% 
  summarise(mean_roots_added_kgha = mean(roots_added_kgha, na.rm = T)) %>% 
  write_csv("01_rootdist-ml/dat_roots-added-means.csv")

#--lot of error
rm_sum_added %>% 
  ggplot(aes(rot_trt, roots_added_kgha)) + 
  stat_summary(geom = "bar") + 
  stat_summary() + 
  facet_grid(.~year)
  
#--in 2020 the 4 yr had less root stuff at the end of the season
rm_sum %>% 
  select(year, block, rot_trt, minmax_x, roots_kgha) %>%
  group_by(year, rot_trt, minmax_x) %>% 
  summarise(roots_kgha = mean(roots_kgha, na.rm = T)) %>%
  mutate(time = ifelse(minmax_x == "beg", 1, 2)) %>% 
  ggplot(aes(time, roots_kgha, color = rot_trt)) + 
  geom_point(size = 5) + 
  geom_line(size = 2) + 
  facet_grid(.~year)



# keep layer separate -----------------------------------------------------

rm_depth <- 
  mrs_rootdist_ml %>% 
  group_by(year) %>% 
  mutate(min_dap = min(dap),
         max_dap = max(dap),
         minmax_x = ifelse(dap == min_dap, "beg", 
                           ifelse(dap == max_dap, "end", NA))) %>% 
  filter(!is.na(minmax_x), 
         !is.na(roots_kgha)) %>% 
  left_join(mrs_plotkey) %>% 
  select(year, minmax_x, plot_id, depth, roots_kgha, block, plot, rot_trt) %>%  
  pivot_wider(names_from = minmax_x, values_from = roots_kgha) %>% 
  mutate(roots_added_kgha = end - beg,
         yearF = as.factor(year)) 


rm_depth %>% 
  ggplot(aes(depth, roots_added_kgha, fill = rot_trt)) + 
  geom_point() + 
  facet_grid(.~year) + 
  coord_flip()


rm_depth %>% 
  group_by(year, depth, rot_trt) %>% 
  summarise_if(is.numeric, mean, na.rm = T)

#--hmm. What if we do each point minus the baseline. 
rm_depth %>% 
  #select(-roots_added_kgha) %>% 
  mutate(depthF = fct_inorder(depth),
         depthF2 = fct_rev(depthF)) %>% 
  pivot_longer(beg:roots_added_kgha) %>% 
  ggplot(aes(depthF2, value, group = block)) + 
  geom_col(aes(fill = rot_trt), position = position_dodge()) + 
  geom_hline(yintercept = 0) +
  facet_grid(year+rot_trt~name) + 
  coord_flip()


# do every day minus baseline ---------------------------------------------

rm_firstday <- 
  mrs_rootdist_ml %>% 
  group_by(year) %>% 
  mutate(min_dap = min(dap)) %>% 
  filter(min_dap == dap,
         !is.na(roots_kgha)) %>% 
  select(year, depth, plot_id, roots_kgha) %>% 
  rename("beg_roots_kgha" = roots_kgha)

rm_alldays <- 
  mrs_rootdist_ml %>% 
  group_by(year) %>% 
  mutate(min_dap = min(dap)) %>% 
  filter(dap != min_dap) %>% 
  select(year, dap, depth, plot_id, roots_kgha) %>% 
  left_join(rm_firstday) %>% 
  mutate(roots_added_kgha = roots_kgha - beg_roots_kgha)

rm_alldays %>% 
  filter(year == 2019) %>% 
  mutate(depthF = fct_inorder(depth),
         depthF2 = fct_rev(depthF)) %>%
  left_join(mrs_plotkey) %>% 
  group_by(dap, year, rot_trt, depthF2) %>% 
  summarise(roots_added_kgha = mean(roots_added_kgha)) %>% 
  ggplot(aes(depthF2, roots_added_kgha)) + 
  geom_col(aes(fill = rot_trt), position = position_dodge()) + 
  geom_hline(yintercept = 0) +
  facet_grid(year+rot_trt~dap) + 
  coord_flip()


# matt stats --------------------------------------------------------------

#--what did he do?!
library(lme4)
library(lmerTest)
library(emmeans)

rm_depth

#--include depth as a thing
m1 <- lmer(value ~ yearF*depth*rot_trt*name + (1|block), 
           data = rm_depth %>% 
             select(-roots_added_kgha) %>% 
             pivot_longer(beg:end))
summary(m1)

anova(m1)

#--separated by year
emmeans(m1, specs = c("yearF", "rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~yearF)

#--not separated
emmeans(m1, specs = c("rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 


#--use the sum over all depths
m1 <- lmer(value ~ yearF*depth*rot_trt*name + (1|block), 
           data = rm_ %>% 
             select(-roots_added_kgha) %>% 
             pivot_longer(beg:end))
summary(m1)

anova(m1)

#--separated by year
emmeans(m1, specs = c("yearF", "rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~yearF)

#--not separated
emmeans(m1, specs = c("rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 
