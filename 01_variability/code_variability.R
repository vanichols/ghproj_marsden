# try to do growth analysis
# started 1/15/2021
# updated:

library(maRsden)
library(tidyverse)

dat <- mrs_cornylds %>% left_join(mrs_plotkey) %>% mutate(yearF = as.factor(year))

dat %>% 
  group_by(year, rot_trt) %>% 
  summarise(yield_sd = sd(yield_Mgha)) %>% 
  ggplot(aes(year, yield_sd)) + 
  geom_point(aes(color = rot_trt))

library(tidysawyer2)

ia_yields_se %>% 
  ggplot(aes(year, sd_kgha)) + 
  geom_point(aes(color = site))

#--find se of the difference?

dat


library(lme4)
library(emmeans)

m1 <- lmer(yield_Mgha ~ rot_trt  + (1|yearF), data = dat)


em1 <- emmeans(m1, specs = pairwise ~ rot_trt)

em1$emmeans
em1$contrasts

confint(em1$contrasts)

#--get an estimate for each year + uncertainty

m2 <- lm(yield_Mgha ~ rot_trt*yearF, data = dat)


em1 <- emmeans(m2, specs = pairwise ~ rot_trt|yearF)

em1$emmeans

confint(em1$contrasts) %>% 
  as_tibble() %>% 
  separate(contrast, into = c("t1", "t2"), sep = "-") %>% 
  mutate_if(is.character, str_trim) %>% 
  filter(t1 != "3y",
         t2 != "3y") %>% 
  ggplot(aes(x = yearF, y = estimate)) + 
  geom_point() + 
  geom_linerange(aes(ymin = lower.CL, ymax = upper.CL))


dat %>% 
  group_by(year, rot_trt) %>% 
  summarise(yield_sd = sd(yield_Mgha)) %>% 
  filter(rot_trt %in% c("2y", "4y"))
  