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


#---need to do phenology

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

phen_avg %>% 
# make dummy doy2 to merge w/roots
  mutate(doy_tmp = case_when(
    year == 2019 & doy == 190 ~ 189,
    year == 2019 & doy == 205 ~ 203,
    year == 2019 & doy == 213 ~ 212,
    year == 2019 & doy == 232 ~ 231,
    year == 2020 & doy == 163 ~ 164,
    TRUE ~ doy)) %>%
  select(-doy, -date) %>% 
  left_join(mrs_rootdepth, by = c("doy_tmp" = "doy", "year", "plot_id"))

#--combine root data w/phen
rdphen19 <-
  rd19 %>%
  left_join(phenroots, by = c("doy" = "doy_tmp",
                              "year", "plot_id")) %>%
  #--no phen 9/17, say R6
  mutate(pl_stage = ifelse(doy == 260, "R6", pl_stage))

#--make a dummy planting (6/3/2020) data point w/rootdepth = 0
plant19 <-
  mrs_plotkey %>%
  filter(year == 2019, rot_trt %in% c("C4", "C2")) %>%
  select(plot_id) %>%
  unique() %>%
  mutate(date = ymd("2019-06-03"),
         year = year(date),
         doy = yday(date),
         pl_stage = "planting",
         rootdepth_cm = 0)

rd19phen2 <-
  rdphen19 %>%
  bind_rows(plant19)



# 2020 data ---------------------------------------------------------------

#--corn was planted 4/23/2020
plant20 <-
  mrs_plotkey %>%
  filter(year == 2020, rot_trt %in% c("C4", "C2")) %>%
  select(plot_id) %>%
  unique() %>%
  mutate(date = ymd("2020-04-23"),
         year = year(date),
         doy = yday(date),
         pl_stage = "planting",
         rootdepth_cm = 0)


rd20raw <-
  tibble(files = list.files(myrootdir)) %>%
  mutate(path = paste0(myrootdir, files)) %>%
  # keep only maxrootdepth ones
  filter(grepl('maxrootdepth', files)) %>%
  filter(grepl('2020', files)) %>%
  filter(!grepl("20200522", files)) %>% #--this first day was a bust
  # make sure I don't get .txt files by mistake
  filter(grepl('.xlsx', files)) %>%
  # read each file
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot) %>%
  mutate(mrd_cm = ifelse(is.na(maxrootdepth_cm), maxrootdepth_in*2.54, maxrootdepth_cm)) %>%
  group_by(date, plot) %>%
  summarise(mrd_cm = mean(mrd_cm, na.rm = T)) %>%
  ungroup() %>%
  mutate(date = as_date(date))

# combine 2018 and 2019 data ----------------------------------------------

mrs_rootdepth <-
  bind_rows(rd18, rd19phen2) %>%
  arrange(year, date, doy, plot_id)

mrs_rootdepth %>% write_csv("data-raw/rootdepth/rootdepth.csv")
usethis::use_data(mrs_rootdepth, overwrite = T)



# data --------------------------------------------------------------------

rd <- mrs_rootdepth %>% as_tibble()


dat <- 
  roo %>%
  # Average sub-plot measurements to get a value for each block
  group_by(date, block, trt, stage) %>%
  summarise(rootdepth_cm = mean(rootdepth_cm, na.rm = T))

anns <- dat %>% 
  group_by(date, stage) %>%
  summarise(maxval = max(rootdepth_cm, na.rm = T) + 2)

##===================================================================##
##   Look at it
##===================================================================##

blockfig<- 
  ggplot(roo, aes(date, rootdepth_cm, fill = trt)) + 
  geom_jitter(size = 4, width = 0.5, pch = 21, alpha = 0.5) + 
  scale_y_reverse() + 
  facet_wrap(.~block, ncol = 2) + 
  scale_fill_manual(values = c("darkgoldenrod1", "cyan3")) +
  labs(x = "",
       y = "Root Depth [cm]\n",
       title = "Marsden\n Corn root depth by block",
       fill = "Treatment") + 
  theme_bw() + 
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10)))


#ggsave("../_figs/MARSfig_rootdepth-by-block.png", width = 4, height = 5, units = "in")


sumfig <- 
  ggplot(dat, aes(date, rootdepth_cm)) + 
  #stat_summary(fun.y="mean", geom="line", size = 2, aes(color = trt)) +
  stat_summary(fun.data = "mean_se", geom = "crossbar",  aes(color = trt)) +
  
  geom_text(data = anns, 
            aes(x = date, maxval, label = stage), fontface = "italic", hjust = 0) +
  
  labs(title = "Marsden\nCorn root depth", 
       x = "", 
       y = "In Row Root Depth [cm]", 
       color = "Treatment") +

  scale_y_reverse() + 
  scale_color_manual(values = c("darkgoldenrod1", "cyan3")) +
  
  theme_bw() +
  theme(legend.position = c(0, 0), 
        legend.justification = c(0, 0), 
        legend.background = element_rect(color = "black"),
        legend.box.margin=margin(c(10,10,10,10)))
  
  
#ggsave("../_figs/MARSfig_rootdepth.png", width = 4, height = 5, units = "in")

blockfig | sumfig
ggsave("../_figs/MARSfig_rootdepth.png", width = 8, height = 5, units = "in")
