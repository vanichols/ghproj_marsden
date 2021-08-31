#--make fig of yields over time
# created 7/30/2021

rm(list = ls())
library(maRsden)
library(tidyverse)
library(janitor)
library(patchwork)

source("03_talk-figs/talk-palette3.R")

theme_set(theme_bw())

mghalab <- (expression(atop("Maize dry grain yield", paste("(Mg "~ha^-1*")"))))

# data --------------------------------------------------------------------

dat <- mrs_cornylds %>% filter(year > 2003)

# fig ---------------------------------------------------------------------

dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  pivot_wider(names_from = rot_trt, values_from = yield_Mgha) %>% 
  clean_names() %>% 
  mutate(segcol = ifelse(x4y - x2y > 0, "A", "B")) %>% 
  ggplot() + 
  geom_segment(aes(y = x2y, yend = x4y,
                   x = year, xend = year, 
                   linetype = segcol)) + 
  geom_point(aes(year, x2y), fill = clr_rot, size = 4, pch = 21) + 
  geom_point(aes(year, x4y), fill = clr_div, size = 4, pch = 21) +
  scale_linetype_manual(values = c("solid", "dashed"))
  


# line graph ----------------------------------------------------------

p_line <- 
  dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  ggplot(aes(year, yield_Mgha)) + 
  geom_line(aes(color = rot_trt, linetype = rot_trt)) +
  geom_point(pch = 21, size = 3, aes(fill = rot_trt)) +
  scale_color_manual(values = c(clr_rot, clr_div),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(clr_rot, clr_div),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  labs(x = "Year",
       y = myyieldlab,
       fill = "Rotation",
       color = "Rotation",
       linetype = "Rotation") + 
  theme(legend.position = c(0.1, 0.1),
        legend.justification = c(0,0),
        legend.background = element_blank()) + 
  scale_y_continuous(limits = c(0, 13))


p_line


# line with means labeled -------------------------------------------------

mns <- 
  dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  group_by(rot_trt) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple 2-year", "Complex 4-year")) 

yldsmodmns

p_line2 <- 
  dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  ggplot(aes(year, yield_Mgha)) + 
    geom_hline(data = mns %>% filter(rot_trt == "Simple 2-year"), color = clr_rot, alpha = 0.5, linetype = "dashed",
               aes(yintercept = yield_Mgha)) +
    geom_hline(data = mns %>% filter(rot_trt != "Simple 2-year"), color = clr_div, alpha = 0.5,
               aes(yintercept = yield_Mgha)) +
    geom_line(aes(color = rot_trt, linetype = rot_trt), size = 1.5) +
  geom_point(pch = 21, size = 4, aes(fill = rot_trt)) +
    scale_color_manual(values = c(clr_rot, clr_div),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(clr_rot, clr_div),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  labs(x = "Year",
       y = myyieldlab,
       fill = "Rotation",
       color = "Rotation",
       linetype = "Rotation") + 
  theme(legend.position = "top",
        legend.direction = "horizontal")

p_line2


ggsave("03_manu-figs/2004-2020-yields.png", width = 9.45, height = 5.03)

# line with means labeled for 2013-2020 only-------------------------------------------------

yldsmodmns <-  
  read_csv("01_yields/dat_ylds13-lmer-est.csv") %>%
  rename("value" = estimate) %>% 
  mutate(name = "Grain yield\n2013-2020",
         rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = fct_rev(rot_trt))


p_line3 <- 
  dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  ggplot(aes(year, yield_Mgha)) + 
    geom_rect(aes(xmin = 2013, xmax = 2020,
                  ymin = 7.5, ymax = 12.5),
              fill = "gray90") +
    geom_segment(data = yldsmodmns %>% filter(rot_trt == "Simple"), 
               color = clr_rot, alpha = 0.5, linetype = "dashed",
             aes(x = 2013, xend = 2020,
                 y = value, yend = value)) +
    geom_segment(data = yldsmodmns %>% filter(rot_trt != "Simple"), 
                 color = clr_div, alpha = 0.5, linetype = "solid",
                 aes(x = 2013, xend = 2020,
                     y = value, yend = value)) +
    geom_line(aes(color = rot_trt, linetype = rot_trt), size = 1.5) +
  geom_point(pch = 21, size = 4, aes(fill = rot_trt)) +
scale_x_continuous(breaks = c(seq(from = 2004, to = 2020, by = 2))) +
  scale_color_manual(values = c(clr_rot, clr_div),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(clr_rot, clr_div),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  labs(x = NULL,
       y = mghalab,
       fill = "Rotation",
       color = "Rotation",
       linetype = "Rotation") + 
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.text = element_text(size = rel(1.3)),
        legend.title = element_text(size = rel(1.5))) + 
  bigtheme

p_line3


ggsave("03_talk-figs/2004-2020-yields-gray.png", width = 9.45, height = 5.03)

