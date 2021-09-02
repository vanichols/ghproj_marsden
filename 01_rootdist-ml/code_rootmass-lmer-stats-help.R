# Author: Gina Nichols
# created:  9/2/2021
# purpose: see if corn treatment affects change in roots, and if that effect differs by depth
# notes: It's ok if there are less roots at sampling period #2 compared to #1

rm(list=ls())
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)
library(emmeans)

#--note for self, creating simple dataset to share
library(maRsden)

# simp_dat <-
#   mrs_rootdist_ml %>%
#   group_by(year) %>%
#   mutate(min_dap = min(dap),
#          max_dap = max(dap),
#          samp_time = ifelse(dap == min_dap, "beg",
#                            ifelse(dap == max_dap, "end", NA))) %>%
#   filter(!is.na(samp_time)) %>%
#   left_join(mrs_plotkey) %>%
#   select(plot_id, year, plot, block, rot_trt, samp_time, depth, roots_kgha) %>%
#   distinct() %>% 
#   mutate(year = paste0("Y", year)) #--so year is read as a factor
# 
# simp_dat %>% write_csv("01_rootdist-ml/dat_simp-dat.csv")

#--read in simp_dat
simp_dat <- read_csv("01_rootdist-ml/dat_simp-dat.csv")

#--should I calculate difference between beg and end and do stats on that?
diff_dat <- 
  simp_dat %>% 
  pivot_wider(names_from = samp_time,
              values_from = roots_kgha) %>% 
  mutate(roots_added_kgha = end - beg)

# stats on roots added-------------------------------------------------------------------

m1 <- lmer(roots_added_kgha ~ year*depth*rot_trt + (1|block:year), 
           data = diff_dat)
anova(m1) #--depth x rot effect

em1 <- emmeans(m1, specs = c("rot_trt", "depth")) 

#--2y treatment added sig amount of roots in 0-15cm, but 4y did not
em1 %>% 
  broom::tidy() 

#--can I compare the 2y addition and 4y addition?
pairs(em1) %>% 
  broom::tidy() %>% 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("rot1", "d1"), sep = " ") %>% 
  separate(t2, into = c("rot2", "d2"), sep = " ") %>% 
  filter(rot1 != rot2,
         d1 == d2) 
#--p = 0.107; this does not match what my advisor found in JMP


# include sampling time ---------------------------------------------------

m2 <- lmer(roots_kgha ~ year*depth*rot_trt*samp_time + (1|block:year), 
           data = simp_dat)
anova(m2) #--depth x rot_trt x samp_time interaction

#--visualize it, can ignore year?
em2 <- emmeans(m2, specs = c("depth", "rot_trt", "samp_time")) 
em2b <- emmeans(m2, specs = c("depth", "rot_trt", "samp_time", "year")) 

em2 %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_wrap(~depth)

em2b %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(year~depth)

#--it seems like the 2y corn added more in the 0-15cm depth than the 4y in both years. 
#--In other depths they were the same
#--but in my model, it is not a significant effect


# try log transforming ----------------------------------------------------


m3 <- lmer(log(roots_kgha) ~ year*depth*rot_trt*samp_time + (1|block:year), 
           data = simp_dat)
anova(m3) #--rot_trt*samp_time, not depth...?

em3 <- emmeans(m3, specs = c("rot_trt", "depth", "samp_time")) 

#--everything is significant...
em3 %>% 
  broom::tidy() 

em3 %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~depth)

