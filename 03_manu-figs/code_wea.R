#--weather years
#--created aug 7 2021
#---currently using mesonet 2020 data that hasn't been qc'd by sotirios

rm(list = ls())
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



# precip ------------------------------------------------------------------

#--cumultive precip

#--total precip
ptot <- 
  mrs_wea %>% 
  filter(day < 366) %>% 
  group_by(year)  %>% 
  summarise(tp = sum(rain_mm, na.rm = T)) 

#--long term mean total
ptot_longterm <- 
  ptot %>% 
  summarise(tp = mean(tp)) %>% 
  pull(tp)

pcum_longterm <- 
  mrs_wea %>% 
  filter(day < 366) %>% 
  group_by(year)  %>% 
  mutate(cp = cumsum(rain_mm)) %>% 
  group_by(day) %>% 
  summarise(cp = mean(cp, na.rm = T)) 

# temperature -------------------------------------------------------------

#--avg temp
tav <- 
  mrs_wea %>% 
  filter(day < 366) %>% 
  mutate(tav = (maxt_c + mint_c)/2) %>% 
  group_by(year)  %>% 
  summarise(tav = mean(tav, na.rm = T)) 

tav_longterm <- 
  tav %>% 
  summarise(tav = mean(tav)) %>% 
  pull(tav)


# my years ----------------------------------------------------------------
yyrs <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
bioyrs <- c(2013, 2014, 2018, 2019, 2020)
ryrs <- c(2018, 2019, 2020)





# weather for different data years ----------------------------------------

ptot_dat <- 
  ptot %>% 
  filter(year > 2012) %>% 
  mutate(yield_dat = ifelse(year %in% yyrs, "Yield data", NA),
         root_dat = ifelse(year %in% ryrs, "root data", NA),
         bio_dat = ifelse(year %in% bioyrs, "growth analysis", NA)) %>% 
  unite(yield_dat, bio_dat, root_dat, col = "msmt", sep = ", ") %>% 
  mutate(msmt = str_squish(msmt),
         msmt = str_remove_all(msmt, "NA, "),
         msmt = str_remove_all(msmt, ", NA"))



# xy plot -----------------------------------------------------------------
library(ggrepel)

yld_diffs <- 
  mrs_cornylds %>% 
  filter(year %in% yyrs) %>% 
  filter(rot_trt != "3y") %>% 
  group_by(year, rot_trt) %>% 
  summarise(yld = mean(yield_Mgha, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = yld) %>% 
  janitor::clean_names() %>% 
  mutate(yld_diff = x4y - x2y)

ptot_dat %>%
  left_join(tav) %>%
  left_join(yld_diffs) %>% 
  ggplot(aes(tp, tav)) +
  geom_hline(yintercept = tav_longterm, linetype = "dashed") +
  geom_vline(xintercept = ptot_longterm, linetype = "dashed") +
  geom_point(aes(color = msmt, size = yld_diff)) +
  geom_text(aes(x = 610, y = 10.85, label = "Hot and dry"), 
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text(aes(x = 1200, y = 8.4, label = "Cool and wet"), 
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text_repel(aes(label = year)) +
  scale_color_manual(values = c("Yield data" = "gray60",
                                ltrd2, dkpnk1)) + 
  labs(size = expression("Yield advantage of complex rotation ("~Mg~ha^-1*")"),
       #size = (expression(atop("Yield advantage\nof complex rotation", paste("(Mg "~ha^-1*")")))),
         color = "Measurement set",
       x = "Total precipitation (mm)",
       y = expression("Mean air temperature ("*~degree*C*")")) + 
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.title.align = 0.5,
        legend.title = element_text(size = rel(1))) + 
  guides(size=guide_legend(direction='horizontal',
                           title.position = "top"))

ggsave("03_manu-figs/fig_wea.png", width = 7.39, height = 5.67)



# get rid of size legend --------------------------------------------------

ptot_dat %>%
  left_join(tav) %>%
  left_join(yld_diffs) %>% 
  ggplot(aes(tp, tav)) +
  geom_hline(yintercept = tav_longterm, linetype = "dashed") +
  geom_vline(xintercept = ptot_longterm, linetype = "dashed") +
  geom_point(aes(color = msmt, size = yld_diff)) +
  geom_text(aes(x = 675, y = 10.85, label = "Hot and dry"), 
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text(aes(x = 1150, y = 8.4, label = "Cool and wet"), 
            color = "gray70", fontface = "italic", check_overlap = T) +
  geom_text_repel(aes(label = year)) +
  scale_color_manual(values = c("Yield data" = "gray60",
                                ltrd2, dkpnk1)) + 
  guides(size = F) +
  labs(size = expression("Yield advantage of complex rotation ("~Mg~ha^-1*")"),
       #size = (expression(atop("Yield advantage\nof complex rotation", paste("(Mg "~ha^-1*")")))),
       color = "Measurement set",
       x = "Total precipitation (mm)",
       y = expression("Mean air temperature ("*~degree*C*")")) + 
  theme(legend.position = "top",
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.title.align = 0.5,
        legend.title = element_text(size = rel(1))) #+ 
  # guides(size=guide_legend(direction='horizontal',
  #                          title.position = "top"))

ggsave("03_manu-figs/fig_wea.png", width = 3.7, height = 4.7)




# old ---------------------------------------------------------------------


#--cum
pcum_y <- 
  pcum_lt %>% filter(between(year, 2013, 2020)) %>% 
  mutate(cat = "Yield data")

pcum_r <- 
  pcum_lt %>% filter(year %in% ryrs) %>% 
  mutate(cat = "Root data")

pcum_bm <- 
  pcum_lt %>% filter(year %in% bioyrs) %>% 
  mutate(cat = "Growth analysis")

#--tot
pt_y <- 
  pt_lt %>% filter(between(year, 2013, 2020)) %>% 
  mutate(cat = "Yield data")

pt_r <- 
  pt_lt %>% filter(year %in% ryrs) %>% 
  mutate(cat = "Root data")

pt_bm <- 
  pt_lt %>% filter(year %in% bioyrs) %>% 
  mutate(cat = "Growth analysis")


#--avg
tav_y <- 
  tav_lt %>% filter(between(year, 2013, 2020)) %>% 
  mutate(cat = "Yield data")

tav_r <- 
  tav_lt %>% filter(year %in% ryrs) %>% 
  mutate(cat = "Root data")

tav_bm <- 
  tav_lt %>% filter(year %in% bioyrs) %>% 
  mutate(cat = "Growth analysis")


tav_y


pt_y %>% 
  rename("yld" = cat) %>% 
  left_join(pt_r %>% rename("roots" = cat)) %>% 
  left_join(pt_bm %>% rename("biomass" = cat)) %>% 
  left_join(tav_y %>% select(-cat) %>% distinct()) %>% 
  mutate_if(is.character, ~replace_na(., "none")) %>% 
  ggplot(aes(tp, tav)) + 
  geom_point(aes(stroke = roots, color = biomass), pch = 21) + 
  geom_hline(yintercept = tav_longterm) + 
  geom_vline(xintercept = pt_longterm)
  
  
  
  
#--do a x-y precip-temp figure instead?



#--one approach  
  p_dat %>% 
  mutate(biodat = ifelse(year %in% bioyrs, "Growth analysis", "Yield only"),
         rootdat = ifelse(year %in% ryrs, "Roots", "not")) %>% 
  ggplot(aes(day, cp)) + 
  geom_line(aes(color = biodat, linetype = rootdat, group = year)) + 
  geom_line(data = p_lt, 
            aes(x = day, y = cp), color = "black", size = 2) + 
  scale_color_manual(values = c("Growth analysis" = grn1,
                                "Yield only" = "gray70")) + 
  facet_grid(.~rootdat)


mrs_wea %>% pull(year) %>% unique()
 library(tidysawyer2)

ilia_wea %>% 
  filter(site == "ames") %>% 
  pull(year) %>% unique()
