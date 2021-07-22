#--combine yields, growth rates, harvest indices, root depth, etc.
# created 7/21/2021


library(maRsden)
library(tidyverse)
library(grafify)
library(janitor)
library(patchwork)

source("02_integrate/palettes.R")

theme_set(theme_bw())

# data --------------------------------------------------------------------

all_years <- tibble(year = c(2013, 2014, 2018, 2019, 2020))
all_trts <- tibble(rot_trt = c("2y", "4y"))

#--bm over time, growth over time
gr <- read_csv("01_growth-analysis/dat_growth-analysis.csv") %>% 
  pivot_longer(mass_gpl:rel_gr) %>% 
  mutate(name = recode(name,  
                       mass_gpl = "Plant biomass",
                       abs_gr = "Growth rate")) %>% 
  filter(name != "rel_gr") %>% 
  rename("year" = yearF)

#--harvest index
hi <- read_csv("01_growth-analysis/dat_harvest-indices.csv") %>% 
  left_join(mrs_plotkey) %>% 
  select(year, rot_trt, block, hi) %>% 
  rename("value" = hi) %>% 
  mutate(name = "Harvest index")

#--grain weights
yc <- mrs_krnl500 %>% 
  left_join(mrs_plotkey) %>% 
  select(year, rot_trt, block, krnl500_g) %>% 
  rename("value" = krnl500_g) %>% 
  mutate(name = "Weight of 500 kernals")


#--yields
ylds <-  
  mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  filter(harv_crop != "C3") %>% 
  filter(year %in% c(2013, 2014, 2018, 2019, 2020)) %>% 
  select(year, rot_trt, block, yield_Mgha) %>% 
  rename("value" = yield_Mgha) %>% 
  mutate(name = "Grain yield")


#--maximum root depth
rd <- 
  read_csv("01_rootdepth/dat_nls-parameters-eu.csv") %>%
  filter(param == "Asym") %>%
  mutate(name = "Maximum rooting depth") %>% 
  select(-param)

#--roots added
radd <- read_csv("01_rootdist-ml/dat_roots-added.csv") %>% 
  select(year, rot_trt, block, roots_added) %>% 
  rename("value" = roots_added) %>% 
  mutate(name = "Roots added over season")




# ind figs ----------------------------------------------------------------

star_dat_yld <- 
  all_years %>% 
  mutate(star = c("*** ", 
                  "*   ",
                  "*** ", 
                  " ", 
                  "*   "),
         rot_trt = "4y")

f_ylds <- 
  ylds %>% 
  ggplot(aes(rot_trt, value, fill = rot_trt, color = rot_trt)) + 
  stat_summary(geom = "bar", width = 0.5) + 
  #stat_summary(geom = "linerange") +
  geom_text(data = star_dat_yld, 
            aes(x = rot_trt, y = 13, label = star),
            size = 8,
            hjust = 1) +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 10)) + 
  scale_fill_manual(values = c(pnk1, dkbl1)) + 
  scale_color_manual(values = c(pnk1, dkbl1)) + 
  scale_y_continuous(limits = c(0, 16)) +
  labs(x = NULL,
       y = "Mg ha-1") +
  theme(axis.text.x = element_blank()) + 
  guides(fill = F, color = F)

f_ylds

f_bm <- 
  gr %>%
  filter(name == "Plant biomass") %>% 
  ggplot(aes(doy, value, color = rot_trt)) + 
  geom_line() +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 10)) + 
  scale_color_manual(values = c(pnk1, dkbl1)) +
  labs(x = NULL,
       y = "grams\nplant-1") +
  theme(strip.text.x = element_blank(),
        axis.text.x = element_blank())+ 
  guides(fill = F, color = F)

f_bm


f_gr <- 
  gr %>%
  filter(name == "Growth rate") %>% 
  ggplot(aes(doy, value, color = rot_trt)) + 
  geom_line() +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 10)) + 
  scale_color_manual(values = c(pnk1, dkbl1)) +
  labs(x = "Day of year", 
       y = "grams\nplant-1 day-1") +
  theme(strip.text.x = element_blank()) + 
  guides(fill = F, color = F)

f_gr

f_hi <- 
  hi %>% 
  ggplot(aes(rot_trt, value, fill = rot_trt)) + 
  stat_summary(geom = "bar") + 
  #stat_summary(geom = "linerange") +
  facet_grid(name ~ year, labeller = label_wrap_gen()) + 
  scale_fill_manual(values = c(pnk1, dkbl1)) + 
  guides(fill = F, color = F)


star_dat_yc <- 
  all_years %>% 
  mutate(star = c(" ", 
                  " ",
                  "*** ", 
                  " ", 
                  " "),
         rot_trt = "4y")


f_yc <-
  all_years %>%
  crossing(all_trts) %>% 
  left_join(yc) %>%
  fill(name, .direction = "up") %>% 
  ggplot(aes(rot_trt, value, color = rot_trt)) + 
  stat_summary(geom = "point", pch = 17, size = 4) + 
  geom_text(data = star_dat_yc, 
            aes(x = rot_trt, y = 190, label = star),
            size = 9,
            hjust = 1) +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 15)) + 
  scale_color_manual(values = c(pnk1, dkbl1)) + 
  scale_y_continuous(limits = c(140, 200)) +
  labs(x = NULL,
       y = "g") +
  theme(axis.text.x = element_blank(),
        strip.text.x = element_blank()) + 
  guides(fill = F, color = F)

f_yc

star_dat_rd <- 
  all_years %>% 
  mutate(star = c("  ", 
                  " ",
                  "*  ", 
                  "*  ", 
                  "*  "),
         rot_trt = "4y")


f_rd <- 
  all_years %>%
  crossing(all_trts) %>% 
  left_join(rd) %>%
  fill(name, .direction = "up") %>% 
  ggplot(aes(rot_trt, value, color = rot_trt)) + 
  geom_point(size = 2) +
  geom_segment(aes(x = rot_trt, xend = rot_trt, y = 0, yend = value)) +
  geom_text(data = star_dat_rd, 
            aes(x = rot_trt, y = -7, label = star),
            size = 9,
            hjust = 1) +
  scale_y_reverse(limits = c(140, -20)) +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 15)) + 
  scale_color_manual(values = c(pnk1, dkbl1)) +
  labs(x = NULL,
       y = "cm") +
  theme(axis.text.x = element_blank(),
        strip.text.x = element_blank()) + 
  guides(fill = F, color = F)

f_rd

f_radd <- 
  all_years %>%
  crossing(all_trts) %>% 
  left_join(radd) %>%
  fill(name, .direction = "up") %>% 
  ggplot(aes(rot_trt, value, fill = rot_trt, color= rot_trt)) + 
  stat_summary(geom = "point", pch = 15, size = 4) + 
#  stat_summary(geom = "linerange") +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 15)) + 
  scale_fill_manual(values = c(pnk1, dkbl1)) + 
  scale_color_manual(values = c(pnk1, dkbl1)) + 
  labs(x = NULL,
       y = "kg ha-1") +
  theme(axis.text.x = element_blank(),
        strip.text.x = element_blank()) + 
  guides(fill = F, color = F)


f_radd

# patchwork... ------------------------------------------------------------

f_ylds / f_bm / f_gr / f_yc / f_rd / f_radd

ggsave("02_integrate/fig_all.png")
