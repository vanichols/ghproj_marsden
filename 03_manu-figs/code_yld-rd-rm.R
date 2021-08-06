#--combine yields, root depth, root mass
# created 8/6/2021


library(maRsden)
library(tidyverse)
library(janitor)
library(patchwork)

source("03_manu-figs/palettes.R")

theme_set(theme_bw())

myth <- 
  theme(strip.text = element_text(size = rel(1.2)),
      strip.background = element_blank(),
      axis.text = element_text(size = rel(1.1)))

# data --------------------------------------------------------------------

my_years <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
my_years2 <- c(2013, 2014, 2018, 2019, 2020)
all_years <- tibble(year = my_years2)
all_trts <- tibble(rot_trt = c("2y", "4y"))



# show means --------------------------------------------------------------
#--could use raw data, or modelled data, not sure which is 'best'

#--yields
ylds <-  
  mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  filter(harv_crop != "C3") %>% 
  filter(between(year, 2013, 2020)) %>%
  group_by(rot_trt) %>% 
  summarise(value = mean(yield_Mgha, na.rm = T)) %>% 
  mutate(name = "Grain yield\n2013-2020",
         rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = fct_rev(rot_trt))

yldsmod <-  
  read_csv("01_yields/dat_ylds-lmer-est.csv") %>%
  rename("value" = estimate) %>% 
  mutate(name = "Grain yield\n2013-2020",
         rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = fct_rev(rot_trt))

  
#--maximum root depth

rd <- 
  read_csv("01_rootdepth/dat_nls-parameters-eu.csv") %>%
  filter(param == "Asym") %>%
  group_by(rot_trt) %>% 
  summarise(value = mean(value)) %>% 
  mutate(name = "Maximum rooting depth\n2018-2020",
         rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = fct_rev(rot_trt))

rdmod <- 
  read_csv("01_rootdepth/dat_nlraa-est.csv") %>%
  filter(param == "Asym") %>%
  rename("value" = estimate,
         "rot_trt" = rotation) %>% 
  mutate(name = "Maximum rooting depth\n2018-2020",
         rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = fct_rev(rot_trt))

#--roots added
radd <- 
  read_csv("01_rootdist-ml/dat_roots-added.csv") %>%
  select(year, block, rot_trt, beg, end) %>% 
  pivot_longer(beg:end) %>% 
  group_by(rot_trt, name) %>% 
  summarise(value = mean(value, na.rm = T)) %>%
  mutate(
    tp = ifelse(name == "beg", 1, 2),
    name = "Root mass\n2019-2020",
         rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = fct_rev(rot_trt))

raddmod <- 
  read_csv("01_rootdist-ml/dat_em-beg-end.csv") %>% 
  rename("value" = estimate) %>% 
  mutate(
    tp = ifelse(name == "beg", 1, 2),
    name = "Root mass\n2019-2020",
    rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
    rot_trt = fct_rev(rot_trt))


rchange <- 
  read_csv("01_rootdist-ml/dat_em-change.csv") %>% 
  mutate(
    name = "Root mass\n2019-2020",
    rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
    rot_trt = fct_rev(rot_trt)) %>% 
  left_join(
    raddmod %>% 
      group_by(rot_trt) %>% 
      summarise(ytext = mean(value))
    ) %>%
  select(rot_trt, contrast, estimate, name, ytext) %>% 
  mutate(lab = paste0("italic(+", abs(round(estimate, 0)), "~kg~", "ha^-1)"),
         xtext = c(2, 1.3))
         



# yield -------------------------------------------------------------------


yldlab <- (expression(atop("Maize grain yield", paste("(Mg "~ha^-1*")"))))
yldlab2 <- (expression("Maize grain yield ("~Mg~ha^-1*")"))

f_yld <- 
  yldsmod %>% 
  mutate(lab = paste0("italic(", round(value, 0), "~Mg~", "ha^-1)")) %>% 
  ggplot(aes(rot_trt, value)) + 
  geom_col(aes(fill = rot_trt), color = "black", width = 0.5) + 
  scale_fill_manual(values = c("Simple" = pnk1, 
                               "Complex" = dkbl1)) + 
  geom_text(aes(x = rot_trt, y = value + 1, 
                label = lab),
            parse = T) +
  guides(fill = F) + 
  labs(x = NULL,
       y = yldlab2) + 
  facet_grid(.~name) + 
  myth
  
f_yld


# root depth --------------------------------------------------------------


rdlab <- (expression(atop("Maximum rooting depth", paste("(cm)"))))
rdlab2 <- "Maximum rooting depth (cm)"

f_rd <- 
  rdmod %>%
  ggplot(aes(rot_trt, value)) +
  geom_point(aes(fill = rot_trt), size = 12, pch = 23) +
  geom_segment(aes(x = rot_trt, xend = rot_trt, y = 0,
                   yend = value), 
               arrow = arrow(length = unit(0.1, "inches"),
                             type = "closed")) +
  geom_text(aes(x = rot_trt, y = value + 15, 
                label = paste0("italic(", round(value, 0), "~cm)")),
            #fontface = "italic"
            parse = T) +
  geom_hline(yintercept = 0) +
  scale_y_reverse(limits = c(140, -10))  + 
  scale_fill_manual(values = c("Simple" = pnk1, 
                               "Complex" = dkbl1)) + 
  guides(fill = F) + 
  labs(x = NULL,
       y = rdlab2)+ 
  facet_grid(.~name) + 
  myth

f_rd


# root mass ---------------------------------------------------------------


raddlab <- (expression(atop("Root material 0-60 cm", paste("(kg "~ha^-1*")"))))
raddlab2 <- (expression("Root material 0-60 cm (kg "~ha^-1*")"))

f_radd <- 
  raddmod %>% 
  ggplot(aes(tp, value, color = rot_trt)) + 
  geom_point(size = 4) + 
  geom_line() + 
  geom_text(data = rchange, 
            aes(x = xtext, 
                y = ytext + 75, 
                label = lab),
            parse = T) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Planting", "Maturity"),
                     limits = c(0.75, 2.25)) + 
  scale_color_manual(values = c("Simple" = pnk1, 
                               "Complex" = dkbl1)) + 
  labs(x = NULL,
       y = raddlab2,
       color = "Rotation") + 
  theme(legend.position = c(0.1, 0.95),
        legend.justification = c(0, 1),
        legend.title = element_text(size = rel(1.1)),
        legend.text = element_text(size = rel(1.1))) + 
  facet_grid(.~name) + 
  myth

f_radd


# patchwork... ------------------------------------------------------------

f_yld + f_rd + f_radd + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("03_manu-figs/fig_yld-rd-rm.png", width = 10.1, height = 6.1)
