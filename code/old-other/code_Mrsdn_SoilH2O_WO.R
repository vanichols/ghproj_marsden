#############################
##
## March 26 2018
## Look at Will's moisture data 
## 
## 
##
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(gridExtra)
library(egg)

####==SETS WORKING DIRECTORY TO WHEREVER ACTIVE DOCUMENT IS KEPT===####
#
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

####========Read in Data==========####
##

soil <- read_csv("../_data_raw/dat_soilH2O_2013-14.csv", skip=1)
yield <- read_csv("../_data_raw/dat_cornyld_2003-17.csv", skip = 4)
weather <- read_csv("../_data_raw/dat_wthr_1987-2017_Marsden.csv", skip = 0)
  
####========Process Data==========####
##

##------Soil moist-----------
##
sm <- soil %>%
  mutate(lubedate = mdy(date), 
         year = year(lubedate), 
         doy = yday(lubedate)) %>%
  rename("1_0-10" = `0-10cm_soilH2O_g.g`,
         "2_10-30" = `10-30cm_soilH2O_g.g`,
         "3_0-30" = `0-30cm_soilH2O_g.g`) %>%
  mutate(system = replace(system, system == 2, "C2"), 
         system = replace(system, system == 3, "C3"),
         system = replace(system, system == 4, "C4")) %>%
  # Change plots to blocks
  mutate(block = replace(block, plot %in% c(13, 11, 18, 14, 16, 19), "B1"),
         block = replace(block, plot %in% c(24, 23, 28, 25, 27, 29), "B2"),
         block = replace(block, plot %in% c(34, 38, 37, 32, 33, 35), "B3"),
         block = replace(block, plot %in% c(44, 48, 46, 42, 45, 49), "B4")) %>%
  # Make 'trt' col
  rename(trt = system) %>%
  gather(depth, VWC, -(1:6), -(10:12))


# Averaged by block
sm2 <- sm %>%
  group_by(year, depth, doy, trt) %>%
  summarize(VWCm = mean(VWC, na.rm = T))


##------Yields-----------
##
yld <- yield %>%
  filter(year %in% c("2013", "2014")) 

# Averaged by block
yld2 <- yld %>%
  group_by(year, trt) %>%
  summarise(bu_ac_mean = mean(bu_ac, na.rm = T))


##------Weather-----------
##
wea <- weather %>%
  mutate(day = as.character(day),
         # NOTE: dates are in yyyy/mm/dd format
         lubedate = ymd(day),
         mo  = month(lubedate),
         year = year(lubedate),
         doy = yday(lubedate)) %>%
  filter(year %in% c("2013", "2014")) 



####==============Plots=============####

# Boxplot of yields
ggplot(yld, aes(trt, Mg_ha, fill = trt)) +
  geom_boxplot() +
  facet_grid(year~.) +
  guides(fill = F) + 
  xlab("") + 
  ylab("Yield [Mg ha-1]") + 
  theme_bw()

ggplot(filter(sm2, depth != "3_0-30"), aes(doy, VWCm, color = trt, group = trt)) + 
  geom_point() +
  geom_line(size = 1.5) + 
  facet_grid(year~depth) +
  guides(color = F) + 
  geom_vline(xintercept = 182, linetype = "dashed") + 
  xlab("") + 
  ylab("Water Content [g g-1]")

# Average soil moisture compared to yields
#ggarrange(p3, p2, widths = 2:1)

# What does soil moisture look like per block?
#
# 0-10cm
ggplot(filter(sm, depth == "1_0-10"), 
       aes(doy, VWC, color = trt, shape = block)) + 
  geom_point() +
  geom_line(aes(group = trt)) + 
  geom_vline(xintercept = 182, linetype = "dashed") + 
  facet_grid(year~block) + 
  theme_bw()

# Here, it looks like when it rains (get data!)
#  moisture in C4 spikes more


##------------Plot moisture with reference to C2 mois-----
##

smref <- sm %>%
  select(-plot, -weird_date, -date, -lubedate) %>%
  spread(trt, VWC) %>%
  mutate(C3ref = C3-C2,
         C4ref = C4-C2) %>%
  select(-C2, -C3, -C4) %>%
  gather(refC2, VWC, -(1:4))

# 0-10cm
ggplot(filter(smref, depth == "1_0-10"), aes(doy, VWC, color = refC2)) + 
  geom_vline(xintercept = 182, linetype = "dashed") + 
  geom_hline(yintercept = 0, size = 2) + 
  geom_point(size = 2) +
  geom_line() +
  #geom_line(aes(group = trt)) + 
  facet_grid(year~block) + 
  theme_bw() + 
  ggtitle("Difference btwn C3/C4 soil moisture\nvs C2 at 0-10cm")

# 10-30cm
ggplot(filter(smref, depth == "2_10-30"), aes(doy, VWC, color = refC2)) + 
  geom_vline(xintercept = 182, linetype = "dashed") + 
  geom_hline(yintercept = 0, size = 2) + 
  geom_point(size = 2) +
  geom_line() +
  #geom_line(aes(group = trt)) + 
  facet_grid(year~block) + 
  theme_bw() + 
  ggtitle("Difference btwn C3/C4 soil moisture\nand C2 at 10-30cm")

##----------Just C4---------

## Build label data frame
##
yld_labs <- tribble(
  ~block, ~year,  ~VWC, ~doy, ~bubump,     ~refC2,
  "B1",    2013,   0.1,  125,    168-156,    "C4ref",
  "B2",   2013,   0.1,  125,    180-151,     "C4ref",
  "B3",   2013,   0.1,  125,    160-153,     "C4ref",
  "B4",   2013,   0.1,  125,    162-128,     "C4ref",
  
  "B1",   2014,   0.1,  125,    212-194,     "C4ref",  
  "B2",   2014,   0.1,  125,    203-208,     "C4ref",
  "B3",   2014,   0.1,  125,    223-198,     "C4ref",
  "B4",   2014,   0.1,  125,    210-209,     "C4ref"
) %>%
  mutate(bubump = as.character(bubump))


## Both depths
## 
ggplot(filter(smref, refC2 == "C4ref", depth != "3_0-30"), aes(doy, VWC, color = depth)) + 
  geom_vline(xintercept = 182, linetype = "dashed") + 
  geom_hline(yintercept = 0, size = 2) + 
  geom_point(size = 4, shape = 18) +
  geom_line() +
  geom_text(data=yld_labs, mapping = aes(x=doy, y=VWC, label=bubump, group = NULL), 
            fontface = "italic", color = "black") + 
  #geom_line(aes(group = trt)) + 
  facet_grid(year~block) + 
  theme_bw() + 
  ggtitle("Difference btwn C4 soil moisture\nvs C2")

## 0-10cm
##
ggplot(filter(smref, depth == "1_0-10", refC2 == "C4ref"), aes(doy, VWC, color = refC2)) + 
  geom_vline(xintercept = 182, linetype = "dashed") + 
  geom_hline(yintercept = 0, size = 2) + 
  geom_point(size = 4) +
  geom_line() + 
  geom_text(data=yld_labs, mapping = aes(x=doy, y=VWC, label=bubump, group = NULL), 
            fontface = "italic", color = "black") + 
  facet_grid(year~block) + 
  theme_bw() + 
  ggtitle("Difference btwn C4 soil moisture\nand C2 at 0-10cm")


## 10-30cm
##
ggplot(filter(smref, depth == "2_10-30", refC2 == "C4ref"), aes(doy, VWC, color = refC2)) + 
  geom_vline(xintercept = 182, linetype = "dashed") + 
  geom_hline(yintercept = 0, size = 2) + 
  geom_point(size = 4) +
  geom_line() + 
  geom_text(data=yld_labs, mapping = aes(x=doy, y=VWC, label=bubump, group = NULL), 
            fontface = "italic", color = "black") + 
  facet_grid(year~block) + 
  theme_bw() + 
  ggtitle("Difference btwn C4 soil moisture\nand C2 at 10-30cm")

# Rain
ggplot(wea, aes(doy, precipmm)) + 
  geom_col() +
  facet_grid(year~.)

# Rain + mois

yld_labs2 <- tribble(
  ~year,  ~VWC, ~doy, ~bubump,
  2013,   50,  125,    "C4 = 168 bu/ac\nC2 = 147 bu/ac",
  2014,   50,  125,    "C4 = 212 bu/ac\nC2 = 202 bu/ac") %>%
  mutate(bubump = as.character(bubump))

## Rain + mois 0-30cm
#
ggplot(filter(sm2, trt != "C3", depth == "3_0-30"), aes(x=doy, y=VWCm*100, color = trt)) +
  geom_col(data=wea, aes(doy, precipmm), color = "gray", fill = "gray") + 
  geom_point()+
  geom_line(aes(group = trt)) +
  
  geom_vline(xintercept = 182, linetype = "dashed") +
  geom_text(data=yld_labs2, mapping = aes(x=doy, y=VWC, label=bubump, group = NULL), 
            fontface = "italic", color = "black") + 
  
  facet_grid(year~.) + 
  ylab("VWC [%] or Precip [mm]") + 
  coord_cartesian(xlim= c(100, 300))

## Rain + mois 10-30cm
#
ggplot(filter(sm2, trt != "C3", depth == "2_10-30"), aes(x=doy, y=VWCm*100, color = trt)) +
  geom_col(data=wea, aes(doy, precipmm), color = "gray", fill = "gray") + 
  geom_point()+
  geom_line(aes(group = trt)) +
  
  geom_vline(xintercept = 182, linetype = "dashed") +
  geom_text(data=yld_labs2, mapping = aes(x=doy, y=VWC, label=bubump, group = NULL), 
            fontface = "italic", color = "black") + 
  
  facet_grid(year~.) + 
  ylab("VWC [%] or Precip [mm]") + 
  coord_cartesian(xlim= c(100, 300))

## Rain + mois 0-10cm
#
ggplot(filter(sm2, trt != "C3", depth == "1_0-10"), aes(x=doy, y=VWCm*100, color = trt)) +
  geom_col(data=wea, aes(doy, precipmm), color = "gray", fill = "gray") + 
  geom_point()+
  geom_line(aes(group = trt)) +
  
  geom_vline(xintercept = 182, linetype = "dashed") +
  geom_text(data=yld_labs2, mapping = aes(x=doy, y=VWC, label=bubump, group = NULL), 
            fontface = "italic", color = "black") + 
  
  facet_grid(year~.) + 
  ylab("VWC [%] or Precip [mm]") + 
  coord_cartesian(xlim= c(100, 300))


## Rain + mois all depths
#
ggplot(filter(sm2, trt != "C3", depth != "3_0-30"), aes(x=doy, y=VWCm*100, color = trt)) +
  geom_col(data=wea, aes(doy, precipmm), color = "gray", fill = "gray") + 
  geom_point(aes(shape = depth))+
  geom_line(aes(linetype = depth, color = trt)) +
  
  geom_vline(xintercept = 182, linetype = "dashed") +
  geom_text(data=yld_labs2, mapping = aes(x=doy, y=VWC, label=bubump, group = NULL), 
            fontface = "italic", color = "black") + 
  
  facet_grid(year~depth) + 
  ylab("VWC [%] or Precip [mm]") + 
  coord_cartesian(xlim= c(100, 300)) + 
  theme_bw()
