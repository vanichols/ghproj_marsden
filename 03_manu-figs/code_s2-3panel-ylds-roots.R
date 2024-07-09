#--combine yields, growth rates, harvest indices
# created 8/6/2021


library(maRsden)
library(tidyverse)
library(janitor)
library(patchwork)

source("03_manu-figs/palettes.R")

theme_set(theme_bw())

# data --------------------------------------------------------------------

all_years <- tibble(year = c(2013, 2014, 2018, 2019, 2020))
all_trts <- tibble(rot_trt = c("2y", "4y"))

#--bm over time, growth over time
gr <- 
  read_csv("01_growth-analysis/dat_growth-analysis.csv") %>% 
  pivot_longer(mass_gpl:rel_gr) %>% 
  mutate(name = case_when(
    name == "mass_gpl" ~ "Plant biomass",
    name == "abs_gr" ~ "Growth rate",
    TRUE ~ name)) %>% 
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
  distinct() %>% 
  filter(year %in% c(2013, 2014, 2018, 2019, 2020)) %>% 
  select(year, rot_trt, block, yield_Mgha) %>% 
  rename("value" = yield_Mgha) %>% 
  mutate(name = "Maize grain yield")



# ylds --------------------------------------------------------------------
yldlab <- (expression("Mg "~ha^-1))


f_ylds <- 
  ylds %>% 
  mutate(thing = "A") %>% 
  pivot_wider(names_from = rot_trt, values_from = value) %>% 
  clean_names() %>% 
  group_by(year, name, thing) %>% 
  summarise_if(is.numeric, mean, na.rm = T) %>% 
  ggplot() + 
  geom_segment(aes(x = thing, xend = thing,
                   y = x2y, yend = x4y)) +
  geom_point(aes(thing, x4y), color = dkbl1, size = 3) + 
  geom_point(aes(thing, x2y), color = rd2, size = 3) +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 15)) + 
  scale_fill_manual(values = c(rd2, dkbl1),
                    labels = c("Short", "Extended")) + 
  scale_color_manual(values = c(rd2, dkbl1)) + 
  #  scale_y_continuous(limits = c(0, 16)) +
  scale_y_continuous(expand = expansion(add = 0.5)) +
  labs(x = NULL,
       y = yldlab,
       fill = "Rotation") +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(color = "black")) + 
  guides(color = "none") 


f_ylds


# biomass -----------------------------------------------------------------
#bmlab <- expression("grams"~plant^-1)
bmlab <- (expression(atop("grams", paste(plant^-1))))

#yldlab2 <- (expression("Maize grain yield ("~Mg~ha^-1*")"))

f_bm <- 
  gr %>%
  filter(name == "Plant biomass") %>% 
  ggplot(aes(doy, value, color = rot_trt)) + 
  geom_line() +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 10)) + 
  scale_color_manual(values = c(rd2, dkbl1)) +
  labs(x = NULL,
       y = bmlab) +
  theme(strip.text.x = element_blank(),
        axis.text.x = element_blank())+ 
  guides(fill = F, color = F)

f_bm


# growth rate -------------------------------------------------------------
grlab <- (expression(atop("grams", paste(~plant^-1~day^-1))))
#grlab <- expression("grams"~plant^-1~day^-1)

f_gr <- 
  gr %>%
  filter(name == "Growth rate") %>% 
  ggplot(aes(doy, value, color = rot_trt)) + 
  geom_line() +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 10)) + 
  scale_color_manual(values = c(rd2, dkbl1)) +
  labs(x = "Day of year", 
       y = grlab) +
  theme(strip.text.x = element_blank()) + 
  guides(fill = F, color = F)

f_gr


# harvest index -----------------------------------------------------------
hilab <- (expression(atop("grams grain", paste("(grams biomass)"^-1))))


f_hi <- 
  hi %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Short", "Extended"),
         rot_trt = as.factor(rot_trt),
         rot_trt = fct_rev(rot_trt)) %>% 
  ggplot(aes(rot_trt, value, color = rot_trt)) + 
  stat_summary(geom = "point", pch = 17, size = 4) + 
  #stat_summary(geom = "linerange") +
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 10)) + 
  scale_color_manual(values = c("Short" = rd2,
                               "Extended" = dkbl1)) + 
  guides(color = F) + 
  labs(x = NULL,
       y = hilab,
       color = "Rotation") + 
#  scale_y_continuous(limits = c(0, 1)) +
  theme(axis.text.x = element_blank(),
        strip.text.x = element_blank(),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)))
  

f_hi

# grain size --------------------------------------------------------------

f_yc <-
  all_years %>%
  crossing(all_trts) %>% 
  left_join(yc) %>%
  fill(name, .direction = "up") %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Short", "Extended"),
         rot_trt = as.factor(rot_trt),
         rot_trt = fct_rev(rot_trt)) %>% 
  ggplot(aes(rot_trt, value, color = rot_trt)) + 
  stat_summary(geom = "point", pch = 15, size = 4) + 
  facet_grid(name ~ year, labeller = label_wrap_gen(width = 15)) + 
  scale_color_manual(values = c(rd2, dkbl1)) + 
  scale_y_continuous(limits = c(140, 200)) +
  labs(x = NULL,
       y = "grams") +
  theme(strip.text.x = element_blank()) + 
  guides(color = F)

f_yc



# patchwork... ------------------------------------------------------------

f_ylds / f_bm / f_gr / f_hi + f_yc + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("03_manu-figs/s2_multi-panel.png", width = 6.93, height = 7.2)
