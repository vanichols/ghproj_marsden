# Gina
# qc rootdepth data, combine with phenology
# 6/12/2020


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)

mrs_rootdepth


# look at it --------------------------------------------------------------

mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, rootdepth_cm, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5) + 
  facet_grid(.~year) + 
  scale_y_reverse()

#---add phenology

phen_avg <- 
  mrs_phen %>% 
  select(year, date, doy, plot_id, pl_stage) %>%
  mutate(
    #--replace the one VT with R1
    pl_stage = ifelse(pl_stage == "VT", "R1", pl_stage),
    #--wyatt forgot to do 2019_21 on day 213, just make it R1
    pl_stage = ifelse( (plot_id == "2019_21" & doy == 213), "R1", pl_stage)) %>% 
  #--separate out number so I can average it
  mutate(class = str_sub(pl_stage, 1, 1),
         stage = parse_number(pl_stage),
         stage = ifelse(is.nan(stage), NA, stage)) %>%
  fill(class) %>% 
  #--average stage by plot
  group_by(year, date, doy, plot_id, class) %>% 
  summarise(stage = round(mean(stage, na.rm = T), 0)) %>% 
  filter(!is.nan(stage)) %>% 
  unite(class, stage, col = "pl_stage", sep = "") %>% 
  ungroup()

#--tricky, phen and roots might not have been done same day
phen_dum <-
  phen_avg %>% 
# make dummy doy2 to merge w/roots
  mutate(doy_tmp = case_when(
    year == 2019 & doy == 190 ~ 189,
    year == 2019 & doy == 205 ~ 203,
    year == 2019 & doy == 213 ~ 212,
    year == 2019 & doy == 232 ~ 231,
    year == 2020 & doy == 163 ~ 164, #--did phen day before roots
    TRUE ~ doy)) %>%
  select(-doy, -date) 


rootphen <- 
  mrs_rootdepth %>% 
  left_join(phen_dum, by = c("doy" = "doy_tmp", "year", "plot_id")) %>% 
  filter(rootdepth_cm != 0) %>% 
  #--no phen 9/17/2019, say R6
  mutate(pl_stage = ifelse((doy == 260 & year == 2019), "R6", pl_stage))

write_csv(rootphen, "01_rootdepth/td_rootdepth-phen.csv")



library(ggrepel)

rootphen %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, rootdepth_cm, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5) + 
  geom_label_repel(data = . %>% select(year, doy, rot_trt, pl_stage) %>% distinct(),
            aes(y = 0, label = pl_stage)) +
  facet_grid(.~year) + 
  scale_y_reverse()


ggsave("01_rootdepth/fig_rootdepth.png")


# ##===================================================================##
# ##   Look at it
# ##===================================================================##
# 
# blockfig<- 
#   ggplot(roo, aes(date, rootdepth_cm, fill = trt)) + 
#   geom_jitter(size = 4, width = 0.5, pch = 21, alpha = 0.5) + 
#   scale_y_reverse() + 
#   facet_wrap(.~block, ncol = 2) + 
#   scale_fill_manual(values = c("darkgoldenrod1", "cyan3")) +
#   labs(x = "",
#        y = "Root Depth [cm]\n",
#        title = "Marsden\n Corn root depth by block",
#        fill = "Treatment") + 
#   theme_bw() + 
#   theme(legend.position = c(0, 0), 
#         legend.justification = c(0, 0), 
#         legend.background = element_rect(color = "black"),
#         legend.box.margin=margin(c(10,10,10,10)))
# 
# 
# #ggsave("../_figs/MARSfig_rootdepth-by-block.png", width = 4, height = 5, units = "in")
# 
# 
# sumfig <- 
#   ggplot(dat, aes(date, rootdepth_cm)) + 
#   #stat_summary(fun.y="mean", geom="line", size = 2, aes(color = trt)) +
#   stat_summary(fun.data = "mean_se", geom = "crossbar",  aes(color = trt)) +
#   
#   geom_text(data = anns, 
#             aes(x = date, maxval, label = stage), fontface = "italic", hjust = 0) +
#   
#   labs(title = "Marsden\nCorn root depth", 
#        x = "", 
#        y = "In Row Root Depth [cm]", 
#        color = "Treatment") +
# 
#   scale_y_reverse() + 
#   scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
#   
#   theme_bw() +
#   theme(legend.position = c(0, 0), 
#         legend.justification = c(0, 0), 
#         legend.background = element_rect(color = "black"),
#         legend.box.margin=margin(c(10,10,10,10)))
#   
#   
# #ggsave("../_figs/MARSfig_rootdepth.png", width = 4, height = 5, units = "in")
# 
# blockfig | sumfig
# ggsave("../_figs/MARSfig_rootdepth.png", width = 8, height = 5, units = "in")
