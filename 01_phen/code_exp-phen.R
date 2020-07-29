# started 7/29/2020

library(tidyverse)
library(maRsden)


# data --------------------------------------------------------------------

data("mrs_phen")
data("mrs_plotkey")

d <- mrs_phen

d %>% 
  filter(year == 2020) ->a


# look at it --------------------------------------------------------------


#--this is stupid
d %>% 
  filter(year == 2020) %>% 
  left_join(mrs_plotkey) %>%
  mutate(deadleaves_nu = devleaves_nu - grleaves_nu) %>%
  select(year, date, rot_trt, plot, grleaves_nu, deadleaves_nu) %>% 
  pivot_longer(grleaves_nu:deadleaves_nu) %>% 
  ggplot(aes(date, value, group = interaction(plot, rot_trt))) + 
  geom_col(aes(fill = name), position = "dodge", width = 1) + 
  facet_wrap(~year, scales = "free") + 
  scale_alpha_discrete(range = c(0.35, 0.9))





d %>% 
  left_join(mrs_plotkey) %>%
  filter(year == 2020) %>% 
  mutate(deadleaves_nu = devleaves_nu - grleaves_nu,
         pct_dead = deadleaves_nu/devleaves_nu) %>%
  group_by(year, date, rot_trt) %>% 
  summarise(dead = mean(pct_dead, na.rm = T)) %>% 
  ggplot(aes(date, dead, group = rot_trt)) +
  geom_col(aes(fill = rot_trt), position = "dodge") + 
  scale_y_continuous(
                 labels = scales::percent_format()
    ) 

d %>% 
  left_join(mrs_plotkey) %>%
  filter(year == 2020) %>% 
  mutate(deadleaves_nu = devleaves_nu - grleaves_nu) %>%
  ggplot(aes(date, deadleaves_nu, group = rot_trt, color = rot_trt)) +
  stat_summary()


d %>% 
  left_join(mrs_plotkey) %>%
  filter(year == 2020) %>% 
  mutate(deadleaves_nu = devleaves_nu - grleaves_nu) %>%
  ggplot(aes(date, deadleaves_nu, group = rot_trt, color = rot_trt, fill = rot_trt)) +
  stat_summary(geom = "bar", position = "dodge")


#--green stacked on brown, something is wrong, they should add up to ~20
d %>% 
  left_join(mrs_plotkey) %>%
  filter(year == 2020) %>%
  mutate(dead = devleaves_nu - grleaves_nu) %>% 
  group_by(date, rot_trt) %>% 
  summarise(
    pot = mean(devleaves_nu, na.rm = T),
    gr = mean(grleaves_nu, na.rm = T),
            br = mean(dead, na.rm = T)) %>%  
  filter(!is.nan(br)) %>% 
  pivot_longer(pot:br) %>%
  filter(name !="pot") %>% 
  ggplot(aes(rot_trt, value, fill = name)) +
  geom_col(position = "stack", color = "black") + 
  # scale_fill_manual(values = c("gr" = "green4",
  #                              "br" = "gold2")) + 
  facet_grid(.~date)

d %>% 
  left_join(mrs_plotkey) %>%
  filter(year == 2020) %>% 
  filter(date > "2020-07-28") %>% 
  ggplot(aes(block, devleaves_nu, color = rot_trt)) + 
  geom_jitter() + 
  stat_summary(size = 3)

d %>% 
  left_join(mrs_plotkey) %>%
  filter(year == 2020) %>% 
  filter(date > "2020-07-28") %>% 
  ggplot(aes(rot_trt, devleaves_nu, color = rot_trt)) + 
  geom_jitter() + 
  stat_summary(size = 3)
