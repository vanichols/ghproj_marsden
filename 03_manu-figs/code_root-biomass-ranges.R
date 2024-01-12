# Gina
# created 7/21/2021
# notes: I'm getting less roots at the end of season compared to beg
#        matt said try doing top layers, or doing it by layer
# updated: 7/31/2023, doing back of the envelope calcs
# 1/12/2024 make figure w/ranges


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)

source("03_manu-figs/palettes.R")


# theme -------------------------------------------------------------------

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

my_ylab <- (expression(atop("Range in maize root production", paste("assuming 0% (left) to 100% (right) background root decay (kg "~ha^-1*")"))))

my_ylab2 <- (expression(atop("Possible range in maize root production", paste("(kg "~ha^-1*")"))))


# data --------------------------------------------------------------------

#--note, eliminate 2020 plot 22 on last day (NAs)?

mrs_rootdist_ml %>% 
  filter(plot_id == "2020_22",
         dap == max(dap))


# Average the first sampling, and the last sampling by depth 
# (one value per rot per year for ea sampling)
#--year wasn't sig, so can justify combining year 
#--Take difference to get 'minimum root production'
#--Last sampling value represents 'maximum root production'

#--get only the first and last sampling dates
te <- 
  mrs_rootdist_ml %>% 
  left_join(mrs_plotkey, relationship = "many-to-many") %>% 
  #--getting rid of plot by averaging
  group_by(year, rot_trt, dap, depth) %>% 
  summarise(roots_kgha = mean(roots_kgha, na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(dap_first = min(dap),
         dap_last = max(dap)) %>% 
  filter((dap == dap_first)|(dap == dap_last)) %>% 
  mutate(dap = ifelse(dap == dap_first, "first", "last"))

#--average them to get one value for each rotation, depth, and sampling point
te_avg <- 
  te %>%
  pivot_wider(names_from = dap, values_from = roots_kgha) %>% 
  #--calculate min and maxes
  mutate(roots_min = ifelse (last > first, last - first, 0),
         roots_max = last) %>% 
  group_by(rot_trt, depth) %>% 
  summarise(roots_min = mean(roots_min, na.rm = T),
            roots_max = mean(roots_max, na.rm = T))

te_tot <- 
  te_avg %>% 
  group_by(rot_trt) %>% 
  summarise(roots_min = sum(roots_min),
            roots_max = sum(roots_max)) %>% 
  mutate(depth = "Total (0-60 cm)")


te_all <- 
  te_avg %>% 
  bind_rows(te_tot) %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = factor(rot_trt, levels = c("Simple", "Complex")),
         depthF = fct_inorder(depth))
  


# exp fig -----------------------------------------------------------------

te_avg %>% 
  mutate(depthF = fct_inorder(depth),
         depthFr = fct_rev(depthF)) %>% 
  ggplot(aes(x = depthFr)) + 
  geom_linerange(aes(ymin = roots_min, ymax = roots_max, color = rot_trt), 
                 position = position_dodge(width = 0.2), 
                 size = 3) + 
  coord_flip() +
  labs(x = NULL,
       y = "Range in possible maize root production (kg ha-1)")


# faceted depths ----------------------------------------------------------

te_all %>% 
  ggplot(aes(x = rot_trt)) + 
  geom_linerange(aes(ymin = roots_min, ymax = roots_max, color = rot_trt), 
                 position = position_dodge(width = 0.2), 
                 size = 3) + 
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple", "Complex")) + 
  guides(fill = F, color = F) +
  labs(x = NULL,
       y = my_ylab,
       color = "Rotation",
       fill = "Rotation") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(.~depth) + 
  my_th
  
#--try patchworking the depth one with the total one


# patchworked -------------------------------------------------------------


f1 <- 
  te_avg %>% 
  mutate(depthF = fct_inorder(depth),
         depthFr = fct_rev(depthF),
         depth = "By Depth") %>% 
  ggplot(aes(x = depthFr)) + 
  # geom_point(aes(y = roots_max, color = rot_trt), pch = 16,
  #            position = position_dodge(width = 0.2),
  #            size = 3) +
  geom_linerange(aes(ymin = roots_min, ymax = roots_max, color = rot_trt), 
                 position = position_dodge(width = 0.2), 
                 size = 3) + 
  # geom_point(aes(y = roots_min, color = rot_trt), pch = 16,
  #            position = position_dodge(width = 0.2),
  #            size = 3) +
  geom_text(aes(x = 4.35, y = 100), color = "gray70", size = 2.5,
            label = "No background\nroot decomposition", check_overlap = T, fontface = "italic") +
  geom_segment(aes(x = 4.15, y = 88, xend = 4.05, yend = 102), 
               color = "gray", arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  geom_text(aes(x = 4.35, y = 350), color = "gray70", size = 2.5,
            label = "100% background\nroot decomposition", check_overlap = T, fontface = "italic") +
  geom_segment(aes(x = 4.15, y = 350, xend = 4.05, yend = 320), 
               color = "gray", arrow = arrow(length = unit(0.1, "cm"), type = "closed")) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) + 
  guides(color = "none") +
  coord_flip() +
  facet_grid(.~depth) +
  labs(x = NULL,
       y = my_ylab2,
       color = "Rotation") + 
    my_th1


#--total biomass
f2 <- 
  te_all %>%
  filter(depthF == "Total (0-60 cm)") %>% 
  ggplot(aes(x = rot_trt, color = rot_trt)) +
  # geom_point(aes(y = roots_min), pch = 16,
  #            position = position_dodge(width = 0.2),
  #            size = 3) +
  # geom_point(aes(y = roots_max), pch = 16,
  #            position = position_dodge(width = 0.2),
  #            size = 3) +
  geom_linerange(aes(ymin = roots_min, ymax = roots_max, 
                     color = rot_trt), 
                 position = position_dodge(width = 0.2), 
                 size = 4) + 
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) + 
  guides(fill = "none", color = "none") +
  labs(x = NULL,
       y = my_ylab2,
       color = "Rotation",
       fill = "Rotation") +
  facet_grid(.~depth) + 
  my_th1


f1 + f2 + 
  plot_layout(widths = c(2, 1)) #+
  # plot_annotation(
  #  caption = 'Range represents 0-100% assumed background root decomposition over growing season'
  # )

ggsave("03_manu-figs/fig_rootmass-ranges.png", width = 6.93, height = 4.12)



# exp with points, don't like ---------------------------------------------------------

  
te_avg %>%
  mutate(depthF = fct_inorder(depth),
         depthFr = fct_rev(depthF)) %>%
  ggplot(aes(x = depthFr, color = rot_trt)) +
  geom_point(aes(y = roots_min), pch = 16,
             position = position_dodge(width = 0.2),
             size = 3) +
  geom_point(aes(y = roots_max), pch = 16,
             position = position_dodge(width = 0.2),
             size = 3) +
  geom_linerange(
    aes(ymin = roots_min, ymax = roots_max, color = rot_trt),
    position = position_dodge(width = 0.2),
    size = 3) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) +
  coord_flip() +
  labs(x = "Soil depth range (cm)",
       y = my_ylab,
       color = "Rotation") +
  my_th1

  
  
#--old fig for example

dat %>% 
  bind_rows(dat_tot) %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = factor(rot_trt, levels = c("Simple", "Complex")),
         depth = fct_inorder(depth)) %>% 
  ggplot(aes(rot_trt, mean)) + 
  geom_col(aes(fill = rot_trt), color= "black") +
  geom_linerange(aes(x = rot_trt, ymin = mean - se, 
                     ymax = mean + se)) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple", "Complex")) + 
  guides(fill = F, color = F) +
  labs(x = NULL,
       y = my_ylab,
       color = "Rotation",
       fill = "Rotation") +
  facet_grid(.~depth) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = rel(1.1)),
        legend.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.1)),
        strip.background = element_blank())

