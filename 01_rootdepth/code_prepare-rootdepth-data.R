# Gina
# combine root depth data, phenology, gdd, elevation
# created: 10/13/2020


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)
library(GinaBooty)

mrs_rootdepth


# look at it --------------------------------------------------------------

mrs_rootdepth %>% 
  left_join(mrs_plotkey) %>% 
  ggplot(aes(doy, rootdepth_cm, color = rot_trt)) + 
  geom_point(size = 3, alpha = 0.5) + 
  facet_grid(.~year) + 
  scale_y_reverse()


# add pehnology -----------------------------------------------------------

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


# add gdd -----------------------------------------------------------------

#--right now don't have 2020 wea data in package. supplement w/nwcoop
wea20 <- read_csv("01_rootdepth/nwscoop-incomplete-2020-weather.csv") %>% 
  rename(date = day,
         day = doy,
         maxt_c = highc,
         mint_c = lowc) %>% 
  mutate(year = year(date)) %>% 
  select(-station, -station_name)

mrs_wea2 <- 
  mrs_wea %>% 
  bind_rows(wea20)

#--want dates of planting for each year
dap_dat <- 
  mrs_rootdepth %>% 
  ungroup() %>% 
  select(-plot_id, -subrep_id, -doy) %>% 
  filter(rootdepth_cm == 0) %>% 
  distinct() %>% 
  mutate(date = date + 1) %>% #--start accumulating gdds after planting
  mutate(dap = "dap") %>% 
  select(-rootdepth_cm)

wea_dap <- 
  mrs_wea2 %>% 
  left_join(dap_dat) %>% 
  group_by(year) %>% 
  fill(dap) %>% 
  filter(!is.na(dap)) %>% 
  ungroup() %>% 
  select(-radn_MJm2, -rain_mm, -dap)

wea_gdd <- 
  wea_dap %>% 
  mutate(gdd = (maxt_c + mint_c)/2 - 10,
         gdd = ifelse(gdd < 0, 0, gdd)) %>% 
  group_by(year) %>% 
  mutate(cum_gdd = cumsum(gdd)) %>% 
  ungroup() %>% 
  select(-year, -day, -maxt_c, -mint_c, -gdd)

rootwea <- 
  mrs_rootdepth %>% 
  left_join(wea_gdd) %>% 
  mutate(cum_gdd = ifelse(is.na(cum_gdd), 0, cum_gdd))

rootwea %>% write_csv("01_rootdepth/td_rootdepth-wea.csv")

rootwea %>% 
  ggplot(aes(date, cum_gdd)) + 
  geom_point() + 
  geom_line()
  

# add elevation -----------------------------------------------------------

dat_elev <- 
  mrs_elevation %>% 
  select(plot, mean_elev_m) %>% 
  mutate(plot = as.character(plot))

rootelev <- 
  mrs_rootdepth %>% 
  separate(plot_id, into = c("year", "plot"), remove = F) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(dat_elev) %>% 
  select(-plot)

rootelev %>% write_csv("01_rootdepth/td_rootdepth-elev.csv")


# elev and wea ------------------------------------------------------------

rootelevwea <- 
  rootelev %>% 
  left_join(rootwea) %>% 
  left_join(mrs_plotkey) %>% 
  select(date, year, doy, cum_gdd, 
         rot_trt, block,
         plot_id, mean_elev_m, subrep_id, 
         rootdepth_cm) 
  
rootelevwea %>% write_csv("01_rootdepth/td_rootdepth-elev-wea.csv")
rootelevwea %>% write_csv("../../_notmy_repos/stat_rethink/gina-proj/td_rootdepth-elev-wea.csv")
