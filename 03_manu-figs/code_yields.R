#--make fig of yields over time
# created 7/30/2021


library(maRsden)
library(tidyverse)
library(janitor)
library(patchwork)

source("03_manu-figs/palettes.R")

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
  geom_point(aes(year, x2y), fill = pnk1, size = 4, pch = 21) + 
  geom_point(aes(year, x4y), fill = dkbl1, size = 4, pch = 21) +
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
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
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
    geom_hline(data = mns %>% filter(rot_trt == "Simple 2-year"), color = pnk1, alpha = 0.5, linetype = "dashed",
               aes(yintercept = yield_Mgha)) +
    geom_hline(data = mns %>% filter(rot_trt != "Simple 2-year"), color = dkbl1, alpha = 0.5,
               aes(yintercept = yield_Mgha)) +
    geom_line(aes(color = rot_trt, linetype = rot_trt), size = 1.5) +
  geom_point(pch = 21, size = 4, aes(fill = rot_trt)) +
    scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
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
               color = pnk1, alpha = 0.5, linetype = "dashed",
             aes(x = 2013, xend = 2020,
                 y = value, yend = value)) +
    geom_segment(data = yldsmodmns %>% filter(rot_trt != "Simple"), 
                 color = dkbl1, alpha = 0.5, linetype = "solid",
                 aes(x = 2013, xend = 2020,
                     y = value, yend = value)) +
    geom_line(aes(color = rot_trt, linetype = rot_trt), size = 1.5) +
  geom_point(pch = 21, size = 4, aes(fill = rot_trt)) +
scale_x_continuous(breaks = c(seq(from = 2004, to = 2020, by = 2))) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  labs(x = "Year",
       y = mghalab,
       fill = "Rotation",
       color = "Rotation",
       linetype = "Rotation") + 
  theme(legend.position = "top",
        legend.direction = "horizontal")

p_line3


ggsave("03_manu-figs/2004-2020-yields-gray.png", width = 9.45, height = 5.03)


# 2013-2020 yuields-------------------------------------------------


yldsmodmns <-  
  read_csv("01_yields/dat_ylds13-lmer-est.csv") %>%
  rename("value" = estimate) %>% 
  mutate(name = "Grain yield\n2013-2020",
         rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = fct_rev(rot_trt))


p_line4 <- 
  dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  filter(year > 2012) %>% 
  ggplot(aes(year, yield_Mgha)) + 
  geom_line(aes(color = rot_trt, linetype = rot_trt), size = 1.5) +
  geom_point(size = 4, aes(fill = rot_trt, pch = rot_trt)) +
  scale_x_continuous(breaks = c(seq(from = 2004, to = 2020, by = 2))) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_linetype_manual(values = c("dashed", "solid"),
                        labels = c("Simple 2-year", "Complex 4-year")) + 
  scale_shape_manual(values = c(22, 24),
                     labels = c("Simple 2-year", "Complex 4-year")) +
  labs(x = "Year",
       y = mghalab,
       fill = "Rotation",
       color = "Rotation",
       shape = "Rotation",
       linetype = "Rotation") + 
  theme(legend.position = c(0.9, 0.1),
        legend.justification = c(1, 0),
        axis.text = element_text(size = rel(1.1)),
        legend.background = element_rect(color = "black"))

p_line4


ggsave("03_manu-figs/2012-2020-yields.png", width = 7.17, height = 4.22)


# bar chart of mean ----------------------------------------------------------


p_bar <- 
  dat %>% 
  left_join(mrs_plotkey) %>% 
  group_by(rot_trt, year) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  filter(rot_trt != "3y") %>% 
  group_by(rot_trt) %>% 
  summarise(yield_Mgha = mean(yield_Mgha, na.rm = T)) %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple 2-year", "Complex 4-year")) %>% 
  ggplot(aes(rot_trt, yield_Mgha)) + 
  geom_col(aes(fill = rot_trt), width = 0.25, color = "black", size = 1) +
  geom_text(aes(x = rot_trt, y = yield_Mgha + 0.5, label = paste(round(yield_Mgha, 1))),
            fontface = "italic") +
  guides(fill = F) +
  scale_fill_manual(values = c(dkbl1, pnk1),
                    labels = c("Simple 2-year", "Complex 4-year")) + 
  labs(x = NULL,
       y = mymeanyieldlab) + 
  scale_y_continuous(limits = c(0, 13))

p_bar



# together ----------------------------------------------------------------


p_line + p_bar + plot_layout(widths = c(2, 1))


p_line + inset_element(p_bar, 0.52, 0, 1, 0.52)
