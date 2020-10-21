# Gina
# does average elevation of rot treatments vary by year?
# 10/20/2020
# notes: need to add more years to plotkey in maRsden package. 


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(patchwork)
library(maRsden)
library(GinaBooty)


mrs_plotkey
mrs_elevation

#--is mean or median elevation best?
# they are the same
mrs_elevation %>% 
  ggplot(aes(mean_elev_m, median)) + 
  geom_point()


#--look at location of plots in a given year
plot_map18 <- 
  mrs_plotcoords %>% 
  left_join(mrs_elevation %>% 
              select(plot, mean_elev_m, median)) %>%
  mutate(year = 2018,
         plot_id = paste(year, plot, sep = "_")) %>% 
  left_join(mrs_plotkey)


ggplot() + 
  geom_rect(data = plot_map18, 
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend, fill = mean_elev_m), 
            #fill = "gray", 
            color = "black") + 
  geom_rect(data = plot_map18 %>% filter(harv_crop %in% c("C2", "C4")),
            aes(xmin = x, xmax = xend, ymin = y, ymax = yend, color = harv_crop), 
            fill = NA, size = 4) + 
  scale_fill_viridis_c()


#--get elevation for each plot in each year
elev <- 
  mrs_plotkey %>% 
  left_join(mrs_elevation %>% 
              select(plot, mean_elev_m, median))

#--get mean elevation by treatment
elev_trt <- 
  elev %>% 
  filter(harv_crop %in% c("C2", "C3", "C4")) %>% 
  group_by(year, rot_trt, harv_crop) %>% 
  summarise(mean_elev = mean(mean_elev_m)) %>% 
  ungroup()
  
# viz ---------------------------------------------------------------------

elev_trt %>% 
  ggplot(aes(rot_trt, mean_elev, color = rot_trt)) +
  geom_point(size = 4) + 
  facet_wrap(~year)

# difference in elev ------------------------------------------------------

elev_dev <- 
  elev_trt %>%
  select(-rot_trt) %>% 
  pivot_wider(names_from = harv_crop, values_from = mean_elev) %>% 
  mutate(devC3 = C3 - C2,
         devC4 = C4 - C2) %>% 
  select(-(C2:C4)) %>% 
  pivot_longer(devC3:devC4) %>% 
  rename(elev_dev = value)

elev_dev %>% 
  ggplot(aes(year, elev_dev, fill = name)) + 
  geom_col(position = position_dodge2()) + 
  ggtitle("Deviation from 2yr elevation")


# compare elev diffs to yield diffs ---------------------------------------

yld_dev <- 
  mrs_cornylds %>% 
  left_join(mrs_plotkey) %>%
  group_by(year, rot_trt) %>% 
  summarise(yield = mean(yield_Mgha, na.rm = T)) %>% 
  pivot_wider(names_from = rot_trt, values_from = yield) %>% 
  mutate(devC3 = `3y` - `4y`,
         devC4 = `4y` - `2y`) %>% 
  select(year, devC3:devC4) %>% 
  pivot_longer(devC3:devC4) %>% 
  rename(yield_dev = value)

#--with more years this relationship might be more robust
yld_dev %>% 
  left_join(elev_dev) %>% 
  ggplot(aes(elev_dev, yield_dev, color = name)) + 
  geom_point(size = 3) + 
  geom_label(aes(label = year)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  ggtitle("Biggest corn yield bumps when average plot elevations are lower than control")


mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  left_join(elev_m) %>% 
  ggplot(aes(mean_elev, yield_Mgha, color = rot_trt)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("Elevation not related to rot effect")

mrs_cornylds %>% 
  left_join(mrs_plotkey) %>% 
  left_join(elev_m) %>% 
  ggplot(aes(mean_elev, yield_Mgha)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_smooth(method = "lm") + 
  facet_grid(.~year) +
  ggtitle("Elevation not related to yields")
#--were the 4yr plots at a lower eleveation in 2018?!?


#--was the biggest yield bump for C4 in 2018?
mrs_cornylds %>% 
  left_join(mrs_plotkey) %>%
  ggplot(aes(rot_trt, yield_Mgha)) + 
  stat_summary() + 
  facet_grid(.~year)
  
#--I need to look at what the water table data said

  

# stats approach ----------------------------------------------------------

dat <- 
  elev_m  %>% 
  left_join(  
    mrs_cornylds %>% 
                left_join(mrs_plotkey)
  ) %>% 
  mutate(yearF = as.factor(year))


library(lme4)
library(lmerTest)
m1 <- lmer(yield_Mgha ~ rot_trt + (1|yearF) + (1|year:block), data = dat)
summary(m1)
anova(m1)

m2 <- lmer(yield_Mgha ~ rot_trt + mean_elev + (1|yearF), data = dat)
summary(m2)
anova(m2)
