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

rm_depth


# depth -------------------------------------------------------------------

#--include depth as a thing, use roots_added as resp var
m1 <- lmer(roots_added_kgha ~ yearF*depth*rot_trt + (1|block), 
           data = rm_depth)
anova(m1) #--depth x rot effect

#--added at each depth
em1 <- emmeans(m1, specs = c("rot_trt", "depth")) 

em1 %>% 
  broom::tidy() %>% 
  write_csv("01_rootdist-ml/dat_em-change-depth.csv")

pairs(em1) %>% 
  broom::tidy() %>% 
  separate(contrast, into = c("t1", "t2"), sep = " - ") %>% 
  separate(t1, into = c("rot1", "d1"), sep = " ") %>% 
  separate(t2, into = c("rot2", "d2"), sep = " ") %>% 
  filter(rot1 != rot2,
         d1 == d2) %>% 
  write_csv("01_rootdist-ml/dat_sig-change-depth.csv")

#--get an estimate ignoring depth
rm_tot <- 
  rm_depth %>% 
  group_by(year, block, rot_trt, yearF) %>% 
  summarise(roots_added_kgha = sum(roots_added_kgha, na.rm = T))

m2 <- lm(roots_added_kgha ~ rot_trt*yearF, data = rm_tot)
anova(m2)
em2 <- emmeans(m2, specs = c("rot_trt")) #--ugh. nothing is sig
em2 %>% 
  broom::tidy() %>% 
  write_csv("01_rootdist-ml/dat_sig-change-total.csv")


# depth, sample time ------------------------------------------------------

#--include depth as a thing, samp_time
m2 <- lmer(value ~ yearF*depth*rot_trt*samp_time + (1|block), 
           data = rm_depth %>% 
             select(-roots_added_kgha) %>% 
             pivot_longer(beg:end, names_to = "samp_time"))
summary(m2)

anova(m2)

#--separated by year
emmeans(m2, specs = c("yearF", "rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~yearF)

#--not separated
emmeans(m2, specs = c("rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 

#--separated by depth THIS IS THE WINNER
emmeans(m2, specs = c("depth", "rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~depth)

#--this one makes the most sense to me. 
emmeans(m2, specs = c("depth", "rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  write_csv("01_rootdist-ml/dat_em-beg-end-by-depth.csv")


# depth, samp time, log transform -----------------------------------------

#--include depth as a thing, samp_time, but use log transf
m2b <- lmer(log(value) ~ yearF*depth*rot_trt*samp_time + (1|block), 
           data = rm_depth %>% 
             select(-roots_added_kgha) %>% 
             pivot_longer(beg:end, names_to = "samp_time"))
anova(m2b)

#--separated by depth THIS IS THE WINNER
emmeans(m2b, specs = c("depth", "rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~depth)

emmeans(m2b, specs = c("depth", "rot_trt", "samp_time")) 



# depth, samp time, DIFF ---------------------------------------
m3 <- lmer(roots_added_kgha ~ yearF*depth*rot_trt + (1|block), 
            data = rm_depth)
anova(m3)

em_m3 <- emmeans(m3, specs = c("depth", "rot_trt")) 
  
contrast(em_m3, method = "pairwise") %>% 
  broom::tidy() %>% 
  separate(contrast, into = c("c1", "c2"), sep = " - ") %>% 
  separate(c1, into = c("d1", "r1"), sep = " ") %>% 
  separate(c2, into = c("d2", "r2"), sep = " ") %>% 
  filter(d1 == d2) %>% 
  select(term, d1, r1, r2, estimate, std.error, df, adj.p.value)

#--this is not what Matt is getting. He says it is because I am testing things against one error term, he is using another. 
m3b <- lmer(roots_added_kgha ~ yearF*depth*rot_trt + (1|block:yearF) + (1|block:rot_trt), 
           data = rm_depth)
anova(m3b)
em_m3b <- emmeans(m3b, specs = c("depth", "rot_trt")) 

contrast(em_m3b, method = "pairwise") %>% 
  broom::tidy() %>%
  separate(contrast, into = c("c1", "c2"), sep = " - ") %>% 
  separate(c1, into = c("d1", "r1"), sep = " ") %>% 
  separate(c2, into = c("d2", "r2"), sep = " ") %>% 
  filter(d1 == d2) %>% 
  select(term, d1, r1, r2, estimate, std.error, df, adj.p.value)



rm_depth %>% 
  arrange(year, block, rot_trt, depth)

#--use the sum over all depths?
m3 <- lmer(value ~ yearF*rot_trt*samp_time + (1|block), 
           data = rm_depth %>% 
             group_by(yearF, plot_id, block, rot_trt) %>% 
             summarise(beg = sum(beg, na.rm =T),
                       end = sum(end, na.rm = T)) %>% 
             pivot_longer(beg:end, names_to = "samp_time"))
summary(m3)

anova(m3)


#--separated by year
emmeans(m3, specs = c("yearF", "rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  facet_grid(.~yearF)

#--not separated
emmeans(m3, specs = c("rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 


#--not separated, without depth
emmeans(m1, specs = c("rot_trt", "samp_time")) %>% 
  broom::tidy() %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) 


#--there is something wrong?
radd <- 
  read_csv("01_rootdist-ml/dat_roots-added.csv") %>%
  select(year, block, rot_trt, beg, end) %>% 
  pivot_longer(beg:end, names_to = "samp_time") %>% 
  mutate(year = paste0("Y", year))

#--why is it singular fit?
m1 <- lmer(value ~ rot_trt*year*samp_time + (1|block), data = radd) #--wtf?
m1a <- lm(value ~ rot_trt*year*samp_time, data = radd)
anova(m1, m1a) #--wtf does that mean

anova(m1a) #--use the one that doesn't bark
#anova(m1) #-basically the same anyways

#em1 <- emmeans(m1, specs = c("rot_trt", "samp_time")) #--same as below
em1 <- emmeans(m1, specs = ~rot_trt:samp_time)
contrast(em1, method = "pairwise")

em2 <- emmeans(m1, specs = ~samp_time|rot_trt)

em2 %>% 
  broom::tidy() %>% 
  write_csv("01_rootdist-ml/dat_em-beg-end.csv")


contrast(em2, method = "pairwise") %>% 
  broom::tidy() %>% 
  write_csv("01_rootdist-ml/dat_em-change.csv")

