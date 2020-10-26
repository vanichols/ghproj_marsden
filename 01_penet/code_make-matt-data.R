# make matt data
# started 10/26
# updated:

library(maRsden)
library(tidyverse)


mrs_penetrom %>% 
  left_join(mrs_plotkey) %>% 
  select(year, doy, block, plot_id, rep_id, rot_trt, harv_crop, depth_cm, resis_kpa) %>% 
  group_by(year, doy, block, plot_id, rot_trt, harv_crop, depth_cm) %>% 
  summarise(resis_kpa = mean(resis_kpa)) %>% 
  write_csv("01_penet/ml_allplotavgpenet.csv")

    

mrs_penetrom %>% 
  ungroup() %>% 
  select(date, doy) %>% 
  distinct()

dat18_may <- 
  mrs_penetrom %>% 
  filter(year == 2018) %>% 
  filter(doy == 130) %>% 
  left_join(mrs_plotkey) %>% 
  ungroup() %>% 
  select(year, doy, block, plot_id, rep_id, rot_trt, harv_crop, depth_cm, resis_kpa)

write_csv(dat18_may, "01_penet/ml_2018maypenet.csv")

dat18_july <- 
  mrs_penetrom %>% 
  filter(year == 2018) %>% 
  filter(doy == 197) %>% 
  left_join(mrs_plotkey) %>% 
  ungroup() %>% 
  select(year, doy, block, plot_id, rep_id, rot_trt, harv_crop, depth_cm, resis_kpa)

write_csv(dat18_july, "01_penet/ml_2018julpenet.csv")

dat19_june <- 
  mrs_penetrom %>% 
  filter(year == 2019) %>% 
  filter(doy == 162) %>% 
  left_join(mrs_plotkey) %>% 
  ungroup() %>% 
  select(year, doy, block, plot_id, rep_id, rot_trt, harv_crop, depth_cm, resis_kpa)

write_csv(dat19_june, "01_penet/ml_2019junpenet.csv")


dat20_june <- 
  mrs_penetrom %>% 
  filter(year == 2020) %>% 
  filter(doy == 175) %>% 
  left_join(mrs_plotkey) %>% 
  ungroup() %>% 
  select(year, doy, block, plot_id, rep_id, rot_trt, harv_crop, depth_cm, resis_kpa)

write_csv(dat20_june, "01_penet/ml_2020junpenet.csv")

