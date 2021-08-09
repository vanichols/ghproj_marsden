# Trying to use fernando + miranda's advice (block as random?)
# started 7/20/21, havent' worked throgh entirely
# 8/9/2021 - transferring over knowledge from soil moisture that fem helped me with. 

library(tidyverse)
library(maRsden)
library(mgcv)
#remotes::install_github("femiguez/nlraa")



# look at data ------------------------------------------------------------


myd <- 
  mrs_penetrom %>% 
  left_join(mrs_plotkey) %>% 
  #filter(year != "2020") %>%
  mutate(resis_Mpa = resis_kpa/1000,
         year = paste0("Y", year),
         eu = paste(year, doy, block, rot_trt)) %>% #--so it's always a charater
  select(year, doy, eu, block, rot_trt, plot_id, rep_id, depth_cm, resis_Mpa) %>% 
  arrange(block, plot_id, rep_id, depth_cm) %>% 
  mutate_if(is.character, as.factor) %>% 
  ungroup()


#--should probably just fit each year separately
myd %>% 
  ggplot(aes(depth_cm, resis_Mpa)) + 
  geom_line(aes(color = rot_trt, group = rep_id)) + 
  facet_grid(doy~year)
  

# make separate data for each year/doy ------------------------------------

d18a <-
  myd %>% 
  filter(year == "Y2018",
         doy == 130)

d18b <-
  myd %>% 
  filter(year == "Y2018",
         doy == 197)


d19 <-
  myd %>% 
  filter(year == "Y2019")


d20 <-
  myd %>% 
  filter(year == "Y2020")



# gam ---------------------------------------------------------------------

#--number of knots? we have 19 depths
d18a %>% 
  select(depth_cm) %>% 
  distinct()

mod_v0 <- mgcv::gam(resis_Mpa ~ s(depth_cm, by = rot_trt, bs = "cr", k = 8) + rot_trt,
                    data = d18a, method = "REML")
mod_v1 <- mgcv::gam(resis_Mpa ~ s(depth_cm, by = rot_trt, bs = "cr", k = 5) + rot_trt,
                    data = d18a, method = "REML")

nlraa::IC_tab(mod_v0, mod_v1, criteria = "BIC") #--model 1 is better


mod_v1_prd <- predict(mod_v1, se.fit = TRUE)

d18a_res <- cbind(d18a, 
                  Estimate = mod_v1_prd$fit, 
                  Q2.5 = mod_v1_prd$fit - 1.96 * mod_v1_prd$se.fit,
                    Q97.5 = mod_v1_prd$fit + 1.96 * mod_v1_prd$se.fit)

## GAM fit with 95% confidence bands
ggplot(data = d18a_res, 
       aes(x = depth_cm, y = resis_Mpa, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3)

## How does the model change when we incorporate the random effect of block?
mod_v2 <- mgcv::gam(resis_Mpa ~ s(depth_cm, by = rot_trt, bs = "cr", k = 5) + rot_trt + s(block, bs = "re"),
                    data = d18a, method = "REML")

mod_v2_prd <- predict(mod_v2, se.fit = TRUE, exclude = "s(block)")
d18a_res2 <- cbind(d18a, 
                  Estimate = mod_v2_prd$fit, 
                  Q2.5 = mod_v2_prd$fit - 1.96 * mod_v2_prd$se.fit,
                  Q97.5 = mod_v2_prd$fit + 1.96 * mod_v2_prd$se.fit)

ggplot(data = d18a_res2, 
       aes(x = depth_cm, y = resis_Mpa, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3)


## What does a GAMM model look like?
mod_v3 <- mgcv::gamm(resis_Mpa ~ s(depth_cm, by = rot_trt, bs = "cr", k = 5) + rot_trt,
                     random = list(block = ~ 1),
                     data = d18a, method = "REML")


mod_v3_prd <- predict(mod_v3$gam, se.fit = TRUE)
d18a_res3 <- cbind(d18a, 
                   Estimate = mod_v3_prd$fit, 
                   Q2.5 = mod_v3_prd$fit - 1.96 * mod_v3_prd$se.fit,
                   Q97.5 = mod_v3_prd$fit + 1.96 * mod_v3_prd$se.fit) %>% 
  as_tibble()

ggplot(data = d18a_res3, 
       aes(x = depth_cm, y = resis_Mpa, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3) + 
  facet_grid(year~doy)


# 2018 2nd sampling -------------------------------------------------------


## What does a GAMM model look like?
mod_v4 <- mgcv::gamm(resis_Mpa ~ s(depth_cm, by = rot_trt, bs = "cr", k = 5) + rot_trt,
                     random = list(block = ~ 1),
                     data = d18b, method = "REML")


mod_v4_prd <- predict(mod_v4$gam, se.fit = TRUE)
d18b_res4 <- cbind(d18b, 
                   Estimate = mod_v4_prd$fit, 
                   Q2.5 = mod_v4_prd$fit - 1.96 * mod_v4_prd$se.fit,
                   Q97.5 = mod_v4_prd$fit + 1.96 * mod_v4_prd$se.fit) %>% 
  as_tibble()

ggplot(data = d18b_res4, 
       aes(x = depth_cm, y = resis_Mpa, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3) +
  facet_grid(year~doy)



# 2019 -------------------------------------------------------

mod_v5 <- mgcv::gamm(resis_Mpa ~ s(depth_cm, by = rot_trt, bs = "cr", k = 5) + rot_trt,
                     random = list(block = ~ 1),
                     data = d19, method = "REML")


mod_v5_prd <- predict(mod_v5$gam, se.fit = TRUE)
d19_res5 <- cbind(d19, 
                   Estimate = mod_v5_prd$fit, 
                   Q2.5 = mod_v5_prd$fit - 1.96 * mod_v5_prd$se.fit,
                   Q97.5 = mod_v5_prd$fit + 1.96 * mod_v5_prd$se.fit)%>% 
  as_tibble()


ggplot(data = d19_res5, 
       aes(x = depth_cm, y = resis_Mpa, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3) +
  facet_grid(year~doy)



# 2020 -------------------------------------------------------

mod_v7 <- mgcv::gamm(resis_Mpa ~ s(depth_cm, by = rot_trt, bs = "cr", k = 5) + rot_trt,
                     random = list(block = ~ 1),
                     data = d20, method = "REML")


mod_v7_prd <- predict(mod_v7$gam, se.fit = TRUE)
d20_res7 <- cbind(d20, 
                  Estimate = mod_v7_prd$fit, 
                  Q2.5 = mod_v7_prd$fit - 1.96 * mod_v7_prd$se.fit,
                  Q97.5 = mod_v7_prd$fit + 1.96 * mod_v7_prd$se.fit) %>% 
  as_tibble()

ggplot(data = d20_res7, 
       aes(x = depth_cm, y = resis_Mpa, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3)



# write results -----------------------------------------------------------

res <- 
  d18a_res3 %>% 
  bind_rows(d18b_res4) %>% 
  bind_rows(d19_res5) %>% 
  bind_rows(d20_res6) %>% 
  mutate(samp = paste(year, doy)) %>% 
  mutate(
    year_num = str_sub(year, 2, -1),
    samp_nice = case_when(
      doy == 130 ~ "Planting",
      doy == 162 ~ "Planting",
      doy == 197 ~ "Late season",
      doy == 175 ~ "Late season",
      TRUE ~ "other"),
    samp_nice2 = paste(year_num, samp_nice)) 

ggplot(data = res, 
       aes(x = depth_cm, y = resis_Mpa, color = rot_trt)) + 
  geom_point(alpha = 0.2) + 
  geom_line(aes(y = Estimate)) + 
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5, fill = rot_trt, color = NULL), alpha = 0.3) + 
  facet_wrap(~samp)

res %>% write_csv("01_penet/dat_penet-gam.csv")



# average difference above 30 cm? -----------------------------------------

res %>% 
  filter(depth_cm <= 30) %>% 
  group_by(samp_nice, rot_trt) %>% 
  summarise(mest = mean(Estimate, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = mest) %>% 
  janitor::clean_names() %>% 
  mutate(diff = x2y - x4y,
         diff/x4y)

res %>% 
  filter(depth_cm > 30) %>% 
  group_by(samp_nice, rot_trt) %>% 
  summarise(mest = mean(Estimate, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = mest) %>% 
  janitor::clean_names() %>% 
  mutate(diff = x2y - x4y,
         diff/x4y)

