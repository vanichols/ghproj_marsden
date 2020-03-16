#############################
##
## Date created: March 7 2019
## Author: Gina
## Purpose: Do stability analysis like Martin et al. (2017)
##
## Date last modified: March 8 2019 (different x axis)
##                     April 12
## 
## INPUTS: _data_raw/dat_cornyld_2003-17.csv
##         
## OUPUTS: _figs/corn-ylds-by-trt-pretty.png
##         _figs/corn-ylds-by-trt.png
##
##############################


##

rm(list=ls())
library(tidyverse)
library(broom)
library(lubridate)
library(corrplot)
library(here)

setwd(here())


# Read files --------------------------------------------------------------

cyld <- read_csv("_data/_tidy/td_corn-yields.csv")

# Wrangle -----------------------------------------------------------------

# sys mean vs grand mean
cyld %>%
  group_by(year) %>%
  mutate(ym_yld = mean(yld_Mgha)) %>%
  ggplot(aes(ym_yld, yld_Mgha)) + 
  geom_point(aes(color = trt), alpha = 0.2) + 
  geom_abline() + 
  geom_smooth(aes(color = trt), se = F, method = "lm", size = 1.5)



# sys mean vs 2-year mean
cyld %>%
  select(-yld_buac) %>%
  spread(trt, yld_Mgha) %>%
  gather(C3:C4, key = trt, value = Mg_ha) %>%
  mutate(more = ifelse(Mg_ha>C2, 1, 0)) %>%
    group_by(block) %>%
    mutate(n = sum(more) / n() * 100,
           n = round(n, 0)) %>%
  ggplot() + 
  geom_point(aes(C2, Mg_ha, color = trt), size = 3) + 
  geom_abline() + 
  
  geom_text(aes(x = 10, y = 15, label = paste0(n, " %"))) +
  
  facet_grid(.~block) + 
  theme_bw() + 
  scale_color_manual(values = c("orange", "red")) + 
  labs(title = "Diversity Does Best in Block 1 and Worst in Block 4\n(i.e. WT gradient?)",
       x = "2Y-rot Corn Yield")


ggsave("_figs/div-benefit-varies-by-block.png")


# Overall yields

cyld %>%
  gather(yld_buac:yld_Mgha, key = yldtype, value = yld) %>%
  mutate(trt_nice = recode(trt, 
                           C2 = "2Y-rot Maize",
                           C3 = "3Y-rot Maize",
                           C4 = "4Y-rot Maize"),
         yldtype_nice = recode(yldtype,
                               yld_buac = "Bu/ac (15.5% mois)",
                               yld_Mgha = "Dry Mg/ha")) %>%
  ggplot(aes(trt_nice, yld)) + 
  stat_summary(fun.data = "mean_se", size = 3) + 
  labs(x = NULL,
       y = "Maize Yield") + 
  theme_bw() + 
  facet_grid(yldtype_nice~., scales = "free")

ggsave("_figs/corn-ylds-by-trt.png", width = 3.5)


cyld %>%
  mutate(conv = yld_buac / yld_Mgha) %>%
  #gather(yld_buac:yld_Mgha, key = yldtype, value = yld) %>%
  mutate(trt_nice = recode(trt, 
                           C2 = "2Y-rot Maize",
                           C3 = "3Y-rot Maize",
                           C4 = "4Y-rot Maize")
         # yldtype_nice = recode(yldtype,
         #                       yld_buac = "Bu/ac (15.5% mois)",
         #                       yld_Mgha = "Dry Mg/ha")
         ) %>%
  
  ggplot(aes(trt_nice, yld_Mgha)) + 
  geom_jitter(aes(color = year), width = 0.2) +
  stat_summary(fun.data = "mean_se", color = "black", geom = "pointrange", size = 1.5) + 
  #stat_summary(fun.data = "mean_se", size = 1, color = "red") + 
  
  scale_y_continuous(sec.axis = sec_axis(~.*15.9, name = "Bushels / Acre")) +
  labs(x = NULL,
       y = "Dry Mg / Hectare") + 
  theme_bw() + 
 # facet_grid(~block) + 
  guides(colour = guide_colourbar(barwidth=20)) +
  labs(color = NULL) +
  
  scale_color_viridis_c(
                        limits = c(2003, 2018), 
                        breaks = c(2003, 2006, 2009, 2012, 2015, 2018)) + 
  
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


ggsave("_figs/corn-ylds-by-trt-pretty.png", width = 4.5)

# Are slopes different by trt? --------------------------------------------

mdl1 <- cyld %>%
  mutate(yld_Mgha = Mg_ha,
         divYN = ifelse(trt == 'C2', 'N', 'Y')) %>%
  group_by(year) %>%
  mutate(ym_yld = mean(yld_Mgha)) 

# No. 
anova(lm(yld_Mgha ~ ym_yld*divYN, data = mdl1))


# Jarad-style graph -------------------------------------------------------

cyld %>%
  mutate(trt = recode(trt,
                      C2 = "2-year",
                      C3 = "3-year",
                      C4 = "4-year")) %>%
  ggplot(aes(year, yld_Mgha, color = trt)) + 
  #geom_point(aes(color = trt)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  labs(x = NULL,
       y = "Maize Yield (dry Mg / ha)",
       color = "Rotation Length") +
  theme_bw() + 
  guides(title = "Rotation Length") +
  theme(legend.justification = c(0,0),
        legend.position = c(0.01,0.01),
        legend.background = element_rect(color = "black")) +
  scale_color_manual(values = c("lightblue", "dodgerblue2", "black"))

ggsave("_figs/corn-ylds-trt-year.png")
