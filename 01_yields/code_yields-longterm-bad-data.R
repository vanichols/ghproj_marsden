# Gina
# visualize yields
# 10/13/2020
# notes: using bad data, need to get it straight from Matt, not sure if it's dry...


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)
library(ggforce)
library(ggridges)
library(GinaBooty)


dat <- read_csv("01_yields/td_corn-yields.csv")


dat %>% 
  filter(trt != "C3") %>% 
  group_by(trt) %>% 
  summarise(yld = mean(yld_Mgha, na.rm = T))
dat %>% 
  filter(trt != "C3") %>% 
  group_by(trt) %>% 
  summarise(yld = mean(yld_buac, na.rm = T))


dat %>% 
  filter(trt != "C3") %>% 
  ggplot(aes(trt, yld_buac)) +
  stat_summary() + 
  coord_cartesian(ylim = c(0, 210))



# donuts ------------------------------------------------------------------

ylds <- 
  mrs_cornylds %>% 
  group_by(harv_crop) %>% 
  summarise(yld_Mgha = mean(yld_Mgha)/10) %>%
  pivot_wider(names_from = harv_crop, values_from = yld_Mgha)

ggplot() + 
  geom_circle(data = ylds, 
              aes(x0 = 0, y0 = 0, r=C4), fill = "green4") + 
  geom_circle(data = ylds, 
              aes(x0 = 0, y0 = 0, r=C2), fill = "white") + 
  coord_fixed() + 
  theme_no_axes()


ggplot() + 
  geom_circle(data = ylds, 
              aes(x0 = 0, y0 = 0, r=C3), fill = "purple") + 
  geom_circle(data = ylds, 
              aes(x0 = 0, y0 = 0, r=C2), fill = "white") + 
  coord_fixed() + 
  theme_no_axes()

ylds %>%
  ggplot() + 
  geom_circle(aes(x0 = 0, y0 = 0, r=C4), fill = "purple") + 
  geom_circle(aes(x0 = 0, y0 = 0, r=C3), fill = "green4") + 
  geom_circle(aes(x0 = 0, y0 = 0, r=C2), fill = "orange") + 
  coord_fixed() + 
  theme_no_axes()



mrs_cornylds %>% 
  filter(!is.na(plot)) %>%
  group_by(harv_crop) %>% 
  summarise(minyld = min(yld_Mgha),
         maxyld = max(yld_Mgha)) %>% 
  ggplot() + 
  geom_circle(aes(x0 = 0, y0 = 0, r=maxyld), fill = "purple") + 
  geom_circle(aes(x0 = 0, y0 = 0, r=minyld), fill = "white") + 
  coord_fixed() +
  facet_grid(.~harv_crop) 


# ggplot() + geom_arc_bar(aes(
#   x0 = 0, y0 = 0, r0 = r0, r = 1, amount = amount,
#   fill = state, explode = focus
# )




# nate's gini  ------------------------------------------------------------

gini <-
  mrs_cornylds %>% 
  filter(!is.na(plot)) %>% 
  group_by(year, harv_crop) %>% 
  summarise(yld_Mgha = mean(yld_Mgha)) %>% 
  group_by(harv_crop) %>% 
  arrange(yld_Mgha) %>% 
  group_by(harv_crop) %>% 
  mutate(tot_Mgha = sum(yld_Mgha),
         pct_tot = yld_Mgha/tot_Mgha,
         cum_pct = cumsum(pct_tot),
         n = n(),
         n = cumsum(1/n)) %>% 
  select(harv_crop, cum_pct, n) %>% 
  ungroup()


gini %>% 
  add_row(harv_crop = c("C2", "C3", "C4"),
          n = c(0,0,0), 
          cum_pct = c(0,0,0)) %>% 
  filter(harv_crop != "C3") %>% 
  ggplot(aes(n, cum_pct)) + 
  #geom_point(aes(color = harv_crop), size = 4) +
  geom_density_line(stat ="identity", alpha=0.6, aes(fill = harv_crop)) +
  geom_abline() + 
  facet_grid(.~harv_crop) +
  theme_minimal() + 
  coord_fixed()


