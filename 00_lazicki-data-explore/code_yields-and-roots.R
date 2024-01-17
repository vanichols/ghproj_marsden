# Gina
# created 1/16/2024
# see if lazicki data has higher yields and more roots in top layers in corn


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(maRsden)

source("03_manu-figs/palettes.R")


# theme ---------------------------------------------------------------------

theme_set(theme_bw())

my_th1 <- theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                #axis.text.x = element_blank(), 
                #axis.ticks.x = element_blank(),
                #legend.position = "top",
                legend.position = c(0.8, 0.2),
                legend.background = element_rect(color = "black"),
                legend.title = element_text(size = rel(1.1)),
                legend.text = element_text(size = rel(1.1)),
                strip.text = element_text(size = rel(1.1)),
                strip.background = element_blank())


# data ---------------------------------------------------------------------



draw <- 
  read_excel("00_lazicki-data-explore/lazicki-root-data.xlsx", na = ".") %>% 
  filter(!is.na(lengthpcm3soil))


# corn --------------------------------------------------------------------

#--simplify
d <- 
  draw %>% 
  filter(
    crop %in% c("C"),
    system %in% c(2,4)) %>% 
  mutate(depth = ifelse(depth == 0, "0-10cm", "10-20cm"),
         rot_trt = ifelse(system == 2, "2y", "4y"),
         year = 2009) %>%  
  select(year, date, rot_trt, block, depth, lengthpcm3soil)


y <- 
  mrs_cornylds %>% 
  filter(rot_trt != "3y") %>% 
  group_by(year, rot_trt, harv_crop) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T))


# fig ---------------------------------------------------------------------

d %>% 
  left_join(y) %>%
  mutate(rot_trt = ifelse(rot_trt == "2y", 
                          paste0("Short, ", round(yield_Mgha, 1), " Mg/ha yield"),
                          paste0("Extended, ", round(yield_Mgha, 1), " Mg/ha yield")),
         date = ifelse(date == 3, "Summer", "Fall")) %>% 
  filter(date != "Summer") %>% 
  ggplot(aes(fct_rev(depth), lengthpcm3soil, fill = rot_trt)) + 
  geom_boxplot(size = 1) + 
  scale_fill_manual(values = c(dkbl1, pnk1)) +
  coord_flip() + 
  labs(x = "Soil Depth", y = "Length of root per cm3 soil",
       fill = "Rotation, maize yield",
       caption = "Data adapted from Lazicki et al. 2016",
       title = str_wrap("Maize grown in short rotation has higher root 
                        density in top 10 cm of soil and lower yields compared to extended rotation", width = 60))

ggsave("03_manu-figs/fig_lazicki2016.png")

#--goddamn it I have to do stats
yb <- 
  mrs_cornylds %>% 
  filter(rot_trt != "3y",
         year == 2009) %>% 
  group_by(year, block, rot_trt) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T))

yb

m <- lm(yield_Mgha ~ rot_trt, yb)
anova(m)
