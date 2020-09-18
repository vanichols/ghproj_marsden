# Gina
# visualize yields
# 9/17/2020


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)
library(ggforce)


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



# If you got values for a pie chart, use stat_pie
states <- c(
  'eaten', "eaten but said you didn\'t", 'cat took it', 'for tonight',
  'will decompose slowly')

pie <- data.frame(
  state = factor(rep(states, 2), levels = states),
  type = rep(c('Pie', 'Donut'), each = 5),
  r0 = rep(c(0, 0.8), each = 5),
  focus = rep(c(0.2, 0, 0, 0, 0), 2),
  amount = c(4, 3, 1, 1.5, 6, 6, 1, 2, 3, 2),
  stringsAsFactors = FALSE)

pie %>% 
  filter(type == "Pie")

# Look at the cakes
ggplot() + 
  geom_arc_bar(aes(
    x0 = 0, 
    y0 = 0, 
    r0 = r0, 
    r = 1, 
    amount = amount,
    fill = state, explode = focus),data = pie, stat = 'pie') +
  facet_wrap(~type, ncol = 1) +
  coord_fixed() +
  theme_no_axes() +
  scale_fill_brewer('', type = 'qual')
    
    
    ggplot() +
      geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = r), data = circles)
