#############################
##
## Created: Oct 31 2019
## Last modified: Nov 2 2019
##
## Purpose: Make graphs for ASA 2019
##
## INPUTS:
##
## OUPUTS:
##############################

rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(ggpubr)
library(ggthemes)




# data --------------------------------------------------------------------

# 2019 key
key18 <- read_csv("_data/tidy/td_year-plot-trt-key.csv") %>%
  filter(year %in% c(2018)) %>%
  mutate(block = str_sub(plot, 1, 1))

key19 <- read_csv("_data/tidy/td_year-plot-trt-key.csv") %>%
  filter(year %in% c(2019)) %>%
  mutate(block = str_sub(plot, 1, 1))


# 2019 root depth
rtd19 <- read_csv("_theme-2019data/_data/tidy/td_maxroot.csv")
rtd18 <- read_csv("_theme-2018data/_data/tidy/td-rootdepth.csv") %>% 
  select(date, plot, trt, rootdepth_cm) %>% 
  group_by(date, trt, plot) %>% 
  summarise(mrd_cm = mean(rootdepth_cm)) %>% 
  filter(mrd_cm != 0)

# combine root depth years
rtd <- 
  rtd19 %>%
  left_join(key19) %>%
  select(date, trt, mrd_cm, plot) %>% 
  bind_rows(rtd18) %>% 
  mutate(year = year(date)) %>% 
  filter(trt %in% c("C2", "C4")) %>% 
  add_row(date = "2019-06-01", trt = "C4", mrd_cm = 0, plot = 12, year = 2019) %>% 
  add_row(date = "2019-06-01", trt = "C2", mrd_cm = 0, plot = 19, year = 2019)
  

# 2018 penetrometer
pen18 <- read_csv("_theme-2018data/_data/tidy/td-penetrometer.csv")

# 2019 penetrometer
pen <- read_csv("_theme-2019data/_data/tidy/td_penetrometer19.csv")

# 2018 residue
roo <- read_csv("_theme-2018data/_data/tidy/td-residue.csv") %>%
  mutate(residue_Mgha = residue_g * 0.01)

# 2018 nutrients
noo <- read_csv("_theme-2018data/_data/tidy/td-nutrients.csv") %>%
  mutate_if(is.character, as.factor)

# all yields
crn <-
  read_csv("_theme-explore-prev-data/_data/_tidy/td_corn-yields.csv") %>%
  arrange(year)


soiln19 <- 
  read_csv("_theme-2019data/_data/tidy/td-soiln19.csv") %>% 
  left_join(key19)
  
  
soiln18 <- 
  read_csv("_theme-2018data/_data/tidy/td-soiln18.csv") %>% 
  clean_names() %>% 
  mutate(totN_kgha = no3_kgha + nh4_kgha) %>%
  left_join(key18) %>% 
  select(date_2, plot, depth, treatment, replication, totN_kgha) %>% 
  rename("date" = date_2)



sensan <- read_csv("../proj_FACTS-sensanl/_data/_tidy/td_sens-res-corn.csv") %>%
  mutate(variable_nice = recode(variable_nice, 
                                `Initial Soil Nitrogen` = "Initial Soil Mineral N")) %>% 
  filter(resp == "HarvestedYield") %>%
  mutate(resp = recode(resp,
                       HarvestedYield = "Maize Grain Yield"),
         variable_nice = recode(variable_nice,
                                `Soil Depth` = "Maximum Rooting Depth")) %>%
  mutate(mycols = case_when(
    
    str_detect(variable_nice, "Maximum") ~ "A",
    str_detect(variable_nice, "Initial Soil") ~ "A",
    str_detect(variable_nice, "N Rate") ~ "A",
    
    
    str_detect(variable_nice, "Remaining Root Amount") ~ "A",
    str_detect(variable_nice, "Initial Soil Water") ~ "A",
    str_detect(variable_nice, "Tillage") ~ "A",
    str_detect(variable_nice, "Remaining Root CN") ~ "A",
    str_detect(variable_nice, "Residue CN Ratio") ~ "A",
    str_detect(variable_nice, "Residue Amount") ~ "A",
    str_detect(variable_nice, "Soil Water-holding") ~ "A",
    str_detect(variable_nice, "Soil Carbon Content") ~ "A",
    TRUE ~ "B")
  )

# create pallettes and labels ---------------------------------------------

#mypal <- c("green4", "olivedrab1")
mypal <- c("darkmagenta", "green4")
yldlab <- expression(Maize ~ Yield ~ (Mg ~ ha ^ -1))
reslab = expression(Surface ~ Residue ~ (Mg ~ ha ^ -1))

mynlab = expression(Total~Mineral~Nitrogen~(mg~kg ^ -1))
mynlab2 = expression(Total~Mineral~Nitrogen~(N)~At~Planting~(kg~N~ha ^ -1))

myfertlab = expression(System~Nitrogen~Addition~(kg~ha ^ -1 ~ yr ^ -1))

mytheme <- theme(
  legend.justification = c(0, 0),
  legend.position = c(0.1, 0.1),
  legend.text = element_text(size = rel(1.3)),
  legend.background = element_rect(color = "black"),
  axis.title = element_text(size = rel(1.5)),
  axis.text = element_text(size = rel(1.3))
)


# figs --------------------------------------------------------------------
# penetrometer ------------------------------------------------------------


pen %>%
  group_by(depth_cm, plot, trt) %>%
  summarise(resis_kpa = mean(resis_kpa)) %>%
  mutate(trtdesc = ifelse(trt == "C4",
                          "Following Alfalfa",
                          "Following Soybean")) %>%
  ggplot(aes(depth_cm, resis_kpa, group = as.factor(plot))) +
  geom_line(aes(color = trtdesc), size = 3) +
  geom_point() +
  scale_x_reverse() +
  geom_vline(xintercept = 0,
             color = "brown",
             size = 1.5) +
  geom_text(
    x = -1,
    y = 1100,
    label = "Soil Surface",
    hjust = 1,
    vjust = 1,
    color = "brown"
  ) +
  coord_flip() +
  labs(color = NULL, x = "Depth (cm)", y = "Resistance (kPa)") +
  scale_color_manual(values = mypal) +
  theme_bw() +
  # theme(legend.position = "top",
  #       legend.text = element_text(size = rel(1.2))) + 
  theme(
    legend.justification = c(0, 0),
    legend.position = c(0.1, 0.1),
    legend.text = element_text(size = rel(1.3)),
    axis.title = element_text(size = rel(1.3)),
    axis.text = element_text(size = rel(1.2))
  )


ggsave("_figs/fig_ASA-penetrometer.png",
       height = 8,
       width = 5)

pen18 %>%
  clean_names() %>% 
  group_by(depth_cm, plot, trt, date) %>%
  summarise(resis_kpa = mean(resis_k_pa)) %>%
  mutate(trtdesc = ifelse(trt == "C4",
                          "Following Alfalfa",
                          "Following Soybean"),
         strptxt = ifelse(date == "2018-05-10", "At Planting", "At Flowering"),
         strptxt = factor(strptxt, levels = c("At Planting", "At Flowering"))) %>%
  ggplot(aes(depth_cm, resis_kpa, group = as.factor(plot))) +
  geom_line(aes(color = trtdesc), size = 3) +
  geom_point() +
  scale_x_reverse() +
  geom_vline(xintercept = 0,
             color = "saddlebrown",
             size = 1.5) +
  # geom_text(
  #   x = 1,
  #   y = 1100,
  #   label = "Soil Surface",
  #   hjust = 1,
  #   vjust = 1,
  #   color = "brown"
  # ) +
  coord_flip(xlim = c(0, 38)) +
  labs(color = NULL, x = "Depth (cm)", y = "Resistance (kPa)") +
  scale_color_manual(values = mypal) +
  facet_grid(. ~ strptxt) +
  theme_bw() +
  # theme(legend.position = "top",
  #       legend.text = element_text(size = rel(1.2))) + 
  mytheme + 
  theme(
    legend.justification = c(0.5, 0.5),
    #legend.position = c(0.05, 0.05),
    legend.position = "top",
    legend.text = element_text(size = rel(1.3)),
    strip.text = element_text(size = rel(1.3)))


ggsave("_figs/fig_ASA-penetrometer2.png",
       height = 8,
       width = 10)


# soilN 2018 ---------------------------------------------------------------------

soiln18 %>% 
  mutate(depth_cm = recode(depth, 
                           '01' = '0-30 cm',
                           '12' = '30-60 cm',
                           '23' = '60-90 cm'),
         depth_cm = factor(depth_cm, levels = (c("0-30 cm", "30-60 cm", "60-90 cm")))) %>% 
  
  mutate(trtdesc = ifelse(trt == "4yr",
                          "Following Alfalfa",
                          "Following Soybean")) %>% 
  
  mutate(month = month(date, label = T)) %>% 
  
  ggplot(aes(x = depth_cm, y = totN_mgkg, fill = trtdesc), color = "black")  +
  stat_summary(fun.data = mean_se, position = position_dodge(0.95), geom = "errorbar", width = 0.5) + 
  stat_summary(fun.y = mean, position = position_dodge(0.95), geom = "bar", color = "black", size = 1.1) +
  labs(fill = NULL, y = mynlab) +
  coord_flip() +
  facet_grid(depth_cm ~ month, scales = "free_y") + 
  scale_fill_manual(values = mypal) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = rel(1.1)),
        legend.position = "top",
        legend.text = element_text(size = rel(1.2)),
        strip.text = element_text(size = rel(1.2)))
  
ggsave("_figs/fig_ASA-soilN18.png")

# initial soil N both years ----------------------------------------------------------

init18 <- 
  soiln18 %>% 
  filter(date < "2018-05-17") %>% 
  mutate(depth_cm = recode(depth, 
                           '0-30' = '0-30 cm',
                           '30-60' = '30-60 cm',
                           '60-90' = '60-90 cm'),
         depth_cm = factor(depth_cm, levels = (c("0-30 cm", "30-60 cm", "60-90 cm")))) %>% 
  
  mutate(trtdesc = ifelse(treatment == "4yr",
                          "Following Alfalfa",
                          "Following Soybean"),
         year = year(date)) %>% 
  select(year, trtdesc, depth_cm, totN_kgha) %>% 
  mutate(timing = "At Planting")

init19 <- 
  soiln19 %>%
  mutate(totN_kgha = no3_kgha + nh4_kgha) %>% 
  #filter(totN_mgkg < 30) %>% 
  mutate(trtdesc = ifelse(system == "4yr",
                          "Following Alfalfa",
                          "Following Soybean"),
         depth_cm = recode(depth, 
                           '0-30' = '0-30 cm',
                           '30-60' = '30-60 cm'),
         depth_cm = factor(depth_cm, levels = (c("0-30 cm", "30-60 cm")))) %>% 
  select(year, trtdesc, depth_cm, totN_kgha) %>% 
  mutate(timing = "At Planting")

initN <- bind_rows(init18, init19) %>% 
  mutate(depth_cm = factor(depth_cm, levels = (c("0-30 cm", "30-60 cm", "60-90 cm")))) 


initN %>% 
  ggplot(aes(trtdesc, totN_kgha)) + 
  stat_summary(fun.data = mean_se, 
               position = position_dodge(0.95), 
               geom = "errorbar", width = 0.5) + 
  stat_summary(fun.y = mean, 
               position = position_dodge(0.95), 
               geom = "bar", color = "black", size = 1.1, 
               aes(fill = trtdesc)) +
  labs(fill = NULL, y = mynlab2) +
  coord_flip() +
  facet_grid(depth_cm ~ year) + 
  scale_fill_manual(values = mypal) + 
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = rel(1.2)),
        legend.position = "top",
        legend.text = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(1.3)))
  

ggsave("_figs/fig_ASA-initN.png")

  


# residue -----------------------------------------------------------------


roo %>%
  mutate(trtdesc = ifelse(trt == "C4",
                          "Following Alfalfa",
                          "Following Soybean")) %>%
  ggplot(aes(trtdesc, residue_Mgha)) +
  stat_summary(fun.data = "mean_se", aes(color = trtdesc), size = 4) +
  labs(#title = "Marsden\nCorn plot residue at planting",
    x = "",
    y = reslab,
    color = "Treatment") +
  scale_color_manual(values = mypal) +
  guides(color = F) +
  theme_bw() +
  theme(
    legend.justification = c(0, 0),
    legend.position = c(0.1, 0.1),
    legend.text = element_text(size = rel(1.3)),
    axis.title = element_text(size = rel(1.3)),
    axis.text = element_text(size = rel(1.2))
  )


ggsave("_figs/fig_ASA-res.png", width = 4, height = 6)


# micronutrients ----------------------------------------------------------


noo %>%
  mutate(trtdesc = ifelse(trt == "C4",
                          "Following Alfalfa",
                          "Following Soybean")) %>%
  filter(msmt %in% c("B", "CA", "CU", "FE", "K", "MG", "MN", "Na", "S", "ZN",
                     "CEC", "OM")) %>%
  filter(trt != "C3") %>% 
  mutate(depth_cm = factor(depth_cm, levels = c("60-90", "30-60", "0-30"))) %>% 
  ggplot(aes(depth_cm, value)) +
  stat_summary(
    fun.data = "mean_se",
    geom = "errorbar",
    width = 0.2, size = 2,
    aes(color = trtdesc)
  ) +
  #stat_summary(fun.y = "mean", geom = "point", aes(color = trtdesc), size = 2) +
  labs(title = "Extensive Soil Test At Planting (0-90 cm)",
       x = "",
       y = "",
      color = NULL) +
  
  scale_color_manual(values = (mypal)) +
  coord_flip() +
  facet_wrap( ~ msmt, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom") + 
  theme(
   # legend.justification = c(0, 0),
    #legend.position = c(0.1, 0.1),
    legend.text = element_text(size = rel(1.3)),
    axis.title = element_text(size = rel(1.3)),
    axis.text.y = element_text(size = rel(1.2))
  )


ggsave("_figs/fig_ASA-micros.png",
       height = 6,
       width = 6)

##



# yields ------------------------------------------------------------------



crn %>%
  group_by(year, trt) %>%
  summarise(yld = mean(yld_Mgha)) %>%
  filter(trt != "C3", year > 2004) %>%
  mutate(trtdesc = ifelse(trt == "C4",
                          "Following Alfalfa",
                          "Following Soybean")) %>%
  
  ggplot(aes(year, yld)) +
  geom_line(aes(linetype = trtdesc)) +
  geom_point(aes(color = trtdesc), size = 5) +
  guides(linetype = F) +
  scale_color_manual(values = mypal) +
  theme_bw() +
  labs(color = NULL, y = yldlab, x = NULL) +
  mytheme

ggsave("_figs/fig_ASA-maize-ylds.png",
       height = 4,
       width = 6)




# root depth --------------------------------------------------------------

## Need to add yields to these
##


yldbox <- tibble(year = 2018,
                 yldC2 = 9.9,
                 yldC4 = 11.9)

rtd %>%
  group_by(year, trt, date) %>% 
  summarise(rd = mean(mrd_cm)) %>% 
  mutate(trtdesc = ifelse(trt == "C4",
                          "Following Alfalfa",
                          "Following Soybean")) %>%
  ungroup() %>% 
  add_row(year = 2018, trt = "C2", date = "2018-09-15", rd = NA, trtdesc = "Following Soybean") %>% 
  ggplot(aes(date, rd, group = trtdesc)) + 
  geom_line(aes(linetype = trtdesc)) +
  geom_point(aes(color = trtdesc), size = 5) +
  
#  geom_text(x = as_date("2018-06-01"), y = 95, label = "9.9") +
  
  guides(linetype = F) +
  scale_y_reverse() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_manual(values = mypal) +
  labs(color = NULL,
       x = NULL,
       y = "Maximum Maize\nRoot Depth (cm)") +
  facet_grid(.~year, scales = "free") +
  theme_bw() + 
  mytheme +
  theme(
    legend.position = "top",
    legend.justification = c(.5,.5),
    strip.text = element_text(size = rel(1.2)))


ggsave("_figs/fig_ASA-root-depth.png", width = 7, height = 5)



# Fake N inputs -----------------------------------------------------------

faken <- tibble(sysID = c("2-yr", "4-yr"),
       fertN= c(80, 15),
       manN = c(0, 30)) %>% 
  gather(fertN:manN, key = fert, value = amt) %>% 
  mutate(fertnice = recode(fert,
                           "fertN" = "Mineral Fertilizer",
                           "manN" = "Manure"))

faken %>% 
  ggplot(aes(sysID, amt)) + 
  geom_bar(aes(fill = fertnice), stat = "identity", color = "black", size = 1.1) + 
  scale_fill_manual(values = c("chocolate4", "lightblue")) + 
  labs(fill = NULL, x = NULL, y = myfertlab) +
  theme_bw() +
  mytheme + 
  theme(legend.position = c(1, 1),
        legend.justification = c(1,1),
        legend.text = element_text(size = rel(1.6)))

ggsave("_figs/fig_ASA-N-add.png", width = 5, height = 8)




# sens anala --------------------------------------------------------------------

bartheme <- theme_pubclean() +
  theme(
    #axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, color = colvec),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.background = element_blank(),
    legend.position = c(.7, .7),
    legend.title = element_text(color = "white"),
    panel.spacing = unit(0.2, "lines"),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    panel.grid.major.y = element_line( size = 0.1, color = "gray80", linetype = "dashed"),
    panel.grid.minor.y = element_line( size = 0.1, color = "gray80", linetype = "dashed"),
    
    strip.background = element_blank(),
    strip.text = element_text(size = rel(1.1))
  )

#--colored
sensan %>% 
  ggplot(aes(x = reorder(variable_nice, rx), y = rx)) +
  geom_col(aes(fill = mycols), color = "grey75") +
  theme_base() +
  labs(x = NULL,
       y = "Sensitivity Index") +
  geom_text(aes(
    x = variable_nice,
    y = 0.00,
    label = variable_nice,
    angle = 0,
    hjust = 0,
    color = mycols
  )) +
  coord_flip() +
  guides(fill = F, color = F) +
  #scale_fill_manual(values = c("deepskyblue", "grey75", "red")) +
  facet_grid(.~resp) +
  bartheme + 
  scale_fill_manual(values = c("gold", "gray")) +
  scale_color_manual(values = c("black", "gray40")) +
  theme(legend.position = c(0.5, 0.25),
        legend.background = element_blank())

ggsave("_figs/fig_ASA-sensanal.png", width = 4, height = 8)

#--top 5
sensan %>%
  ggplot(aes(x = reorder(variable_nice, rx), y = rx)) +
  geom_col(fill = "gold", color = "grey75") +
  theme_base() +
  labs(x = NULL,
       y = "Sensitivity Index") +
  geom_text(aes(
    x = variable_nice,
    y = 0.005,
    label = variable_nice,
    angle = 0,
    hjust = 0),
    size = 6
    #color = mycols
  ) +
  coord_flip(xlim = c(23, 28)) +
  guides(fill = F, color = F) +
  #scale_fill_manual(values = c("deepskyblue", "grey75", "red")) +
  facet_grid(.~resp) +
  bartheme + 
  #scale_fill_manual(values = c("gold", "gray")) +
  #scale_color_manual(values = c("black", "gray40")) +
  theme(legend.position = c(0.5, 0.25),
        legend.background = element_blank())

ggsave("_figs/fig_ASA-sensanal2.png", width = 4, height = 8)

#--one color
sensan %>%
  ggplot(aes(x = reorder(variable_nice, rx), y = rx)) +
  geom_col(fill = "gold", color = "grey75") +
  theme_base() +
  labs(x = NULL,
       y = "Sensitivity Index") +
  geom_text(aes(
    x = variable_nice,
    y = 0.00,
    label = variable_nice,
    angle = 0,
    hjust = 0)
    #size = 3
    #color = mycols
  ) +
  coord_flip() +
  guides(fill = F, color = F) +
  #scale_fill_manual(values = c("deepskyblue", "grey75", "red")) +
  facet_grid(.~resp) +
  bartheme + 
  #scale_fill_manual(values = c("gold", "gray")) +
  #scale_color_manual(values = c("black", "gray40")) +
  theme(legend.position = c(0.5, 0.25),
        legend.background = element_blank())

ggsave("_figs/fig_ASA-sensanal3.png", width = 4, height = 8)

