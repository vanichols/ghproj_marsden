#--weather years
#--created aug 7 2021


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


# long term wea calcs -----------------------------------------------------

#--cumultive precip
pcum_lt <- 
  mrs_wea %>% 
  filter(day < 366) %>% 
  group_by(year)  %>% 
  mutate(cp = cumsum(rain_mm)) %>% 
  group_by(day) %>% 
  summarise(cp = mean(cp, na.rm = T)) 

#--total precip
pt_lt <- 
  mrs_wea %>% 
  filter(day < 366) %>% 
  group_by(year)  %>% 
  summarise(tp = sum(rain_mm, na.rm = T)) 


pt_longterm <- 
  pt_lt %>% 
  summarise(tp = mean(tp)) %>% 
  pull(tp)

#--avg temp
tav_lt <- 
  mrs_wea %>% 
  filter(day < 366) %>% 
  mutate(tav = (maxt_c + mint_c)/2) %>% 
  group_by(year)  %>% 
  summarise(tav = mean(tav, na.rm = T)) 

tav_longterm <- 
  tav_lt %>% 
  summarise(tav = mean(tav)) %>% 
  pull(tav)
# my years ----------------------------------------------------------------
yyrs <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
bioyrs <- c(2013, 2014, 2018, 2019, 2020)
ryrs <- c(2018, 2019, 2020)





# precip ------------------------------------------------------------------

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
