# Gina
# created 7/21/2021
# notes: I'm getting less roots at the end of season compared to beg
#        matt said try doing top layers, or doing it by layer

rm(list=ls())
library(tidyverse)
library(lubridate)
library(patchwork)
library(maRsden)
library(lme4)
library(lmerTest)
library(emmeans)

#--note, eliminate plot 22 on day XX

#--there is something wrong?
radd <- 
  read_csv("01_rootdist-ml/dat_roots-added.csv") %>%
  select(year, block, rot_trt, beg, end) %>% 
  pivot_longer(beg:end) %>% 
  mutate(year = paste0("Y", year))

#--why is it singular fit?
m1 <- lmer(value ~ rot_trt*year*name + (1|block), data = radd)
m1a <- lm(value ~ rot_trt*year*name, data = radd)
anova(m1, m1a) #--wtf does that mean

anova(m1a)

#em1 <- emmeans(m1, specs = c("rot_trt", "name")) #--same as below
em1 <- emmeans(m1, specs = ~rot_trt:name)
contrast(em1, method = "pairwise")

em2 <- emmeans(m1, specs = ~name|rot_trt)

em2 %>% 
  broom::tidy() %>% 
  write_csv("01_rootdist-ml/dat_em-beg-end.csv")


contrast(em2, method = "pairwise") %>% 
  broom::tidy() %>% 
  write_csv("01_rootdist-ml/dat_em-change.csv")


# matt stats --------------------------------------------------------------

#--what did he do?!
library(lme4)
library(lmerTest)
library(emmeans)


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
  distinct() %>% 
  pivot_wider(names_from = minmax_x, values_from = roots_kgha) %>% 
  mutate(roots_added_kgha = end - beg,
         yearF = as.factor(year)) 



#--include depth as a thing
m2 <- lmer(value ~ yearF*depth*rot_trt*name + (1|block), 
           data = rm_depth %>% 
             select(-roots_added_kgha) %>% 
             pivot_longer(beg:end))
summary(m2)

anova(m2)

#--separated by year
emmeans(m2, specs = c("yearF", "rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~yearF)

#--not separated
emmeans(m2, specs = c("rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 


#--use the sum over all depths
m3 <- lmer(value ~ yearF*depth*rot_trt*name + (1|block), 
           data = rm_depth %>% 
             select(-roots_added_kgha) %>% 
             pivot_longer(beg:end))
summary(m3)

anova(m3)

#--separated by year
emmeans(m3, specs = c("yearF", "rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~yearF)

#--not separated
emmeans(m3, specs = c("rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 


#--not separated, without depth
emmeans(m1, specs = c("rot_trt", "name")) %>% 
  broom::tidy() %>% 
  mutate(name2 = ifelse(name == "beg", 1, 2)) %>% 
  ggplot(aes(name2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 
