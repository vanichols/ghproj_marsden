#############################
##
## Feb 20 2018
## Look at Marsden Farm weather (from https://mesonet.agron.iastate.edu/request/coop/fe.phtml)
## Compare to 2003-2017 yields (corn)
##
## INPUTS: _data_raw/dat_NASS_cntyylds_1986-2016.csv
##         _data_raw/dat_cornyld_2003-17.csv
##         _data_raw/dat_wthr_1987-2017_Marsden.csv
##         _data_raw/dat_soyyld_2003-17.csv
##
## OUPUTS: _data_prcsd/datp_cornyld_2003-17.csv
##         _data_prcsd/datp_soyyld_2003-17.csv
##

##############################


##

rm(list=ls())
library(tidyverse)
library(lubridate)
library(corrplot)
library(PerformanceAnalytics)
library(lme4)
library(ggplot2)
library(rlme)
library(rstudioapi)

####==SETS WORKING DIRECTORY TO WHEREVER ACTIVE DOCUMENT IS KEPT===####
#
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

####==========READ IN FILES==============######

ca <- read.csv("../_data_raw/dat_NASS_cntyylds_1986-2016.csv", skip=0, header=TRUE)
cyld <- read.csv("../_data_raw/dat_cornyld_2003-17.csv", skip=4, header=TRUE)
wLT <- read.csv("../_data_raw/dat_wthr_1987-2017_Marsden.csv", skip=0, header=TRUE)
syld <- read.csv("../_data_raw/dat_soyyld_2003-17.csv", skip=4, header=TRUE)


####==========MY FUNCTIONS?====================##
# 
# lm_eqn <- function(m){
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 2), 
#                         b = format(coef(m)[2], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 2)))
#   as.character(as.expression(eq));                 
# }


####==MANIPULATE CORN and SOY YIELD FILES==============####
#

####====CORN=========####
#
cyld %>%
  #group_by(year, trt) %>%
  #summarise(bu_ac = mean(bu_ac, na.rm=T)) %>%
  mutate(trt = recode(trt, "C2" = "2yr", 
                      "C3" = "3yr",
                      "C4" = "4yr")) %>%
  ggplot(aes(year, bu_ac, group = trt)) + 
  stat_summary(aes(color = trt), size = 1) + 
  #geom_line(aes(color = trt)) +
  theme_bw() + 
  labs(y = "Maize Yield\n[Bushels per acre]", x=NULL, color = "Rotation Length") + 
  theme(legend.justification = c(0.1,0.1), 
        legend.position = c(0.1, 0.1), 
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1))

ggsave("../_figs/MARS_cornyields.png")

#----------Boone county averages
boone <- ca  %>%
  # 56 lbs/bu, 0.45 kg/lb, 2.47 ac/ha, 15.5% moisture
  mutate(coMg_ha = Value * 56 * .4536 * 2.471 * (1-.155) / 1000 , year = Year) %>%
  select(year, County, coMg_ha) %>%
  mutate(LTMg_ha = mean(coMg_ha), co_indx = coMg_ha - LTMg_ha)

#--------Marsden yield data

cy <- cyld %>% 
  mutate(year  = as.factor(year),
         gyear = year,
         # Convert 15.5% mois Mg_ha to dry wt
         dMg_ha = (1-.155) * Mg_ha)

ggplot(cy, aes(x=year, y=dMg_ha, color=trt)) +geom_point(size = 2, aes(shape = as.factor(block)))

ggplot(cy, aes(x=trt, y=dMg_ha, color=trt)) +
  geom_boxplot()


# When does block 4 do well compared to block 1? No pattern.
ggplot(filter(cy, block %in% c("1", "2")), 
              aes(x=year, y=dMg_ha, color=as.factor(block))) +
  geom_point(size = 3, aes(shape = as.factor(block)))


# Average trt by year
cy_yr <- cy %>% 
  group_by(year, trt) %>% 
  summarise(mMg_ha = mean(dMg_ha))


# Calc differences and spreads
cy_difs <- cy_yr %>%
  # find the winner each year and range of ylds
  group_by(year) %>% 
  mutate(winner = trt[which.max(mMg_ha)], sprd = (max(mMg_ha) - min(mMg_ha))) %>% 
  # Give each trt it's own column
  spread(trt, mMg_ha) %>% 
  mutate(dif_4v2 = C4-C2,
         dif_3v2 = C3-C2,
         dif_4v3 = C4-C3,
         pdif_4v2 = (C4-C2) / C2 * 100, 
         pdif_3v2 = (C3-C2) / C2 * 100,
         pdif_4v3 = (C4-C3) / C3 * 100)%>%
  # Get C2, C3, C4 into one column w/yield as a col
  gather(trt, dMg_ha, -year, -winner, -sprd, -(dif_4v2:pdif_4v3)) %>%
  mutate(crop = "maize")



write.csv(cy_difs, file = paste("../_data_prcsd/datp_cornyld_2003-17.csv", sep = ""), row.names = FALSE) 


####====SOY=========####
#

#--------Marsden yield data

sy <- syld %>% 
  mutate(year  = as.factor(year),
         gyear = year,
         # Convert 13% mois Mg_ha to dry wt
         dMg_ha = (1-.13) * Mg_ha)
#ggplot(sy, aes(x=year, y=dMg_ha, color=trt)) +geom_point(size = 2, aes(shape = as.factor(block)))

ggplot(sy, aes(x=trt, y=dMg_ha, color=trt)) +
  geom_boxplot()

# Average trt by year
sy_yr <- sy %>% 
  group_by(year, trt) %>% 
  summarise(mMg_ha = mean(dMg_ha))


# Calc differences and spreads
sy_difs <- sy_yr %>%
  # find the winner each year and range of ylds
  group_by(year) %>% 
  mutate(winner = trt[which.max(mMg_ha)], sprd = (max(mMg_ha) - min(mMg_ha))) %>% 
  # Give each trt it's own column
  spread(trt, mMg_ha) %>% 
  mutate(dif_4v2 = S4-S2,
         dif_3v2 = S3-S2,
         dif_4v3 = S4-S3,
         pdif_4v2 = (S4-S2) / S2 * 100, 
         pdif_3v2 = (S3-S2) / S2 * 100,
         pdif_4v3 = (S4-S3) / S3 * 100)%>%
  # Get S2, S3, S4 into one column w/yield as a col
  gather(trt, dMg_ha, -year, -winner, -sprd, -(dif_4v2:pdif_4v3)) %>%
  mutate(crop = "soybean")

write.csv(sy_difs, file = paste("../_data_prcsd/datp_soyyld_2003-17.csv", sep = ""), row.names = FALSE) 


##---------Explore corn yield data

## Large range in yields, is system 3 or/and 4 statistically better over the long term?
ggplot(cy, aes(x=trt, y=dMg_ha, color=year)) +geom_point()
ggplot(cy, aes(x=trt, y=dMg_ha)) +geom_boxplot()

# Very simple ANOVA
a1 <- aov(data = cy, dMg_ha~ trt)
summary(a1)
# 
# # Linear Mixed Model
# l1 <- lme(data = cy, dMg_ha~ trt*year, random = ~1|block)
# summary(l1)
# anova(l1)
# 
# l1 <- lme(data = cy, dMg_ha~ trt*block, random = ~1|year)
# summary(l1)
# anova(l1)

##================LOOK AT YIELD SPRDS OF SOYBEAN AND CORN OVER YEARS=====####

## THERE IS SOMETHING WRONG.....

yall <- cy_difs %>% bind_rows(sy_difs)
  
ggplot(filter(yall, crop == "maize", trt != "C2"), aes(x= year, y = dMg_ha, group = trt)) + 
  geom_point(size = 4, aes(color = trt)) + 
  geom_line(aes(color = trt)) + 
  geom_col(data=filter(yall, crop == "maize"), position = "dodge", 
           aes(x = year, y = dif_4v2, fill = dif_4v2)) +
  scale_fill_gradient(low = "black", high = "green2") +
#  theme_bw() +
  scale_color_manual(values = c("black", 'green2'))
#+ 
  #facet_grid (crop~., scales = "free_y" )
    
# I bet soybean yields are related to precip


####==LONG TERM WEATHER AND CALCULATIONS=============##########################
##

wea <- wLT %>%
  mutate(day = as.character(day),
         # NOTE: dates are in yyyy/mm/dd format
         lubedate = ymd(day),
         mo  = month(lubedate),
         year = year(lubedate),
         doy = yday(lubedate))


####==Calcs CALENDAR YEARS=============####
#

#--------------------LT by month
#
wea_mLT <- wea %>% 
  # Get avg daily T
  mutate(mTday_C = (highc + lowc)/2) %>% 
  # Find LT mean T and tot P for each month in each year
  group_by(mo, year) %>% 
  summarise(mT_C = mean(mTday_C), mP_mm = sum(precipmm)) %>%
  # Peel it back and average over years
  summarise(mLTT_C = mean(mT_C), mLTP_mm = mean(mP_mm))

# Exp+LT by month
# emLT; e = experimental years, m = monthly, LT = includes LT data
wea_emLT <- wea %>% 
  # Daily avg T
  mutate(dT_C = (highc + lowc)/2) %>% 
  # Find avg for each month in each year
  group_by(year, mo) %>% 
  summarise(mT_C = mean(dT_C), mP_mm = sum(precipmm))%>% 
  # Reduce to just experimental years
  filter(year %in% 2003:2017)  %>%
  # Add mLT values
  left_join(wea_mLT, by  = "mo")

# Graphically compare LT
#  Precip SOMETHING IS WRONG?
ggplot(wea_mLT, aes(x=as.factor(mo), y=mLTP_mm)) +
  geom_col(alpha = 0.5) +
  geom_point(data = wea_emLT, aes(x=as.factor(mo), y = mP_mm, color = year)) +
  geom_line(data = wea_emLT, aes(x=as.factor(mo), y = mP_mm, color = year, group = year), size = 1.1)

#  Temperature LOOKS OK.
ggplot(wea_mLT, aes(x=as.factor(mo), y=mLTT_C)) +
  geom_col(alpha = 0.5) +
  geom_point(data=wea_emLT, aes(x=as.factor(mo), y = mT_C, color = year)) +
  geom_line(data=wea_emLT, aes(x=as.factor(mo), y = mT_C, color = year, group = year), size = 1.1)

  

# ------------------------------LT Overall (just one value for T and P)
wea_LT <- wea %>% 
  # Find avg daily temp 
  mutate(dT_C = (highc + lowc)/2) %>% 
  # Calculate for each year
  group_by(year) %>%
  summarise(yLTT_C = mean(dT_C), yLTP_mm = sum(precipmm)) %>%
  # Summarise over the 30 years
  summarise(LTT_C = mean(yLTT_C), LTP_mm = mean(yLTP_mm))


# Exp+LToverall by year
# eyLT; e = experimental years, y = monthly, LT = includes LT data
wea_eyLT <- wea %>% 
  mutate(dT_C = (highc + lowc)/2) %>% 
  # Find mean T and tot P for each year
  group_by(year) %>% 
  summarise(yT_C = mean(dT_C), yP_mm = sum(precipmm)) %>%
  # Keep only experimental yrs
  filter(year %in% 2003:2017) %>%
  # Add LT values
  mutate(LTT_C = wea_LT$LTT_C, LTP_mm = wea_LT$LTP_mm)
  

#---------Graphically look at years compared to LT values
#

LTT <- wea_LT $LTT_C
LTP <- wea_LT $LTP_mm

ggplot(wea_eyLT, aes(x=yT_C, y=yP_mm, color = as.factor(year))) +
  geom_point(alpha = 0.5, size = 4) +
  geom_hline(yintercept = LTP) +
  geom_vline(xintercept = LTT)

####==Calcs GROWING YEARS=============####
#

##----------Create a 'growing year' that goes from Oct - Sept (Matt's idea)
#

wea_g <- wea %>% 
  select(doy, gdd_50_86, highc, lowc, precipmm, mo, year) %>%
  mutate(gyear = ifelse(mo >9, year +1, year)) 

#--------------------LT by month
#
wea_mgLT <- wea_g %>% 
  # Get avg daily T
  mutate(mTday_C = (highc + lowc)/2) %>% 
  # Find LT mean T and tot P for each month in each gyear
  group_by(mo, gyear) %>% 
  summarise(mT_C = mean(mTday_C), mP_mm = sum(precipmm)) %>%
  # Peel it back and average over gyears
  summarise(mLTT_C = mean(mT_C), mLTP_mm = mean(mP_mm))

# Exp+LT by month
# emLT; e = experimental gyears, m = monthly, LT = includes LT data
wea_emgLT <- wea_g %>% 
  # Daily avg T
  mutate(dT_C = (highc + lowc)/2) %>% 
  # Find avg for each month in each gyear
  group_by(gyear, mo) %>% 
  summarise(mT_C = mean(dT_C), mP_mm = sum(precipmm))%>% 
  # Reduce to just experimental gyears
  filter(gyear %in% 2003:2017)  %>%
  # Add mLT values
  left_join(wea_mgLT, by  = "mo")

# Graphically compare LT
#  Precip SOMETHING IS WRONG?
ggplot(wea_mgLT, aes(x=as.factor(mo), y=mLTP_mm)) +
  geom_col(alpha = 0.5) +
  geom_point(data = wea_emgLT, aes(x=as.factor(mo), y = mP_mm, color = gyear)) +
  geom_line(data = wea_emgLT, aes(x=as.factor(mo), y = mP_mm, color = gyear, group = gyear), size = 1.1)

#  Temperature LOOKS OK.
ggplot(wea_mgLT, aes(x=as.factor(mo), y=mLTT_C)) +
  geom_col(alpha = 0.5) +
  geom_point(data=wea_emgLT, aes(x=as.factor(mo), y = mT_C, color = gyear)) +
  geom_line(data=wea_emgLT, aes(x=as.factor(mo), y = mT_C, color = gyear, group = gyear), size = 1.1)

  

# ------------------------------LT Overall (just one value for T and P)
wea_gLT <- wea_g %>% 
  # Find avg daily temp 
  mutate(dT_C = (highc + lowc)/2) %>% 
  # Calculate for each gyear
  group_by(gyear) %>%
  summarise(yLTT_C = mean(dT_C), yLTP_mm = sum(precipmm)) %>%
  # Summarise over the 30 gyears
  summarise(LTT_C = mean(yLTT_C), LTP_mm = mean(yLTP_mm))


# Exp+LToverall by gyear
# eyLT; e = experimental gyears, y = monthly, LT = includes LT data
wea_eygLT <- wea_g %>% 
  mutate(dT_C = (highc + lowc)/2) %>% 
  # Find mean T and tot P for each gyear
  group_by(gyear) %>% 
  summarise(yT_C = mean(dT_C), yP_mm = sum(precipmm)) %>%
  # Keep only experimental yrs
  filter(gyear %in% 2003:2017) %>%
  # Add LT values
  mutate(LTT_C = wea_LT$LTT_C, LTP_mm = wea_LT$LTP_mm)
  

#---------Graphically look at gyears compared to LT values
#

## THESE SHOULD BE CHANGING AND THEY ARE NOT
gLTT <- wea_gLT $LTT_C
gLTP <- wea_gLT $LTP_mm

ggplot(wea_eygLT, aes(x=yT_C, y=yP_mm, color = as.factor(gyear))) +
  geom_point(alpha = 0.5, size = 4) +
  geom_hline(yintercept = gLTP) +
  geom_vline(xintercept = gLTT)


####===YIELD SPREADS VS SIMPLE WEATHER PATTERNS=============####
##

# Spread vs yearly precip
#
wyall <- wea_eyLT %>%
  mutate(year = as.factor(year)) %>%
  right_join(yall, by = "year")
ggplot(wyall, aes(x = yP_mm, y = sprd)) + 
  geom_point(aes(color = year), size = 4) + 
  geom_smooth(method = "lm") +
  facet_grid(. ~crop, scales = "free_y") + 
  ylab("Difference Btwn Best Yielding Diverse System and 2-yr System") +
  xlab("Calendar Year Precip [mm]")


# Spread vs precip tot in April - Aug (SDS, Leandro et al. 2013)
#
spyall <- wea_emLT %>%
  filter(mo %in% c(3, 4, 5, 6, 7, 8)) %>%
  group_by(year) %>%
  summarise(spP_mm = sum(mP_mm)) %>% 
  mutate(year = as.factor(year)) %>%
  right_join(yall, by ="year")

# Put equations on it?
mym <- lm(sprd~spP_mm, spyall[spyall$crop == "maize",])
mys <- lm(sprd~spP_mm, spyall[spyall$crop == "soybean",])

ggplot(spyall, aes(x = spP_mm, y = sprd, color = year, group = crop)) +
  geom_point(size = 4) + 
  geom_smooth(method = "lm") + 
  facet_grid(.~crop, scales = "free_y") + 
  geom_text(x = 800, y = 1.2, 
           label = lm_eqn(mys), parse = TRUE, color = "green4") + 
  geom_text(x = 800, y = 1.4, 
            label = lm_eqn(mym), parse = TRUE, color = "gold")




####==CREATE INDEXES TO CLASSIFY YEARS (weather, co yld avgs, etc) ==================####
#

#---------'Hot and dry' index ('hd')
#----Each month, how much hotter and drier was it than LT average
#---- (across entire year)
#
wea_hd <- wea_emLT %>%
  # find deviation from LT average
  mutate(devT_C = mT_C - mLTT_C, pdevT_C = devT_C / mLTT_C,
         devP_mm = mP_mm -mLTP_mm, pdevP_mm = devP_mm / mLTP_mm) %>%
  # calculate a 'hot and dry' index
  #  -Pdev (drier = more +), Tdev (hotter = more +)
  mutate(hd_indx_mo = -pdevP_mm + pdevT_C) %>%
  # group by year, add hd_indx for each month
  group_by(year) %>%
  summarise(hd_indx = sum(hd_indx_mo))

#---------Do a subset of the year
#
# April - June (spring; sp)
wea_hdsp <- wea_emLT %>%
  # find deviation from LT average
  mutate(devT_C = mT_C - mLTT_C, pdevT_C = devT_C / mLTT_C,
         devP_mm = mP_mm -mLTP_mm, pdevP_mm = devP_mm / mLTP_mm) %>%
  # subset to April, May, June
  filter(mo %in% c(4, 5, 6)) %>%
  # calculate a 'hot and dry' index
  #  -Pdev (drier = more +), Tdev (hotter = more +)
  mutate(hdsp_indx_mo = -pdevP_mm + pdevT_C) %>%
  # group by year, add hd_indx for each month
  group_by(year) %>%
  summarise(hdsp_indx = sum(hdsp_indx_mo))


wea_id <- wea_hd %>% left_join(wea_hdsp, by = "year")


#----------County yield data vs hd_indx
#
boone$year <- as.character(boone$year)
wea_id$year <- as.character(wea_id$year)
idx <- wea_id %>% left_join(boone, by = "year")

# They should be inversely related. They are not.
ggplot(idx, aes(x = co_indx, y = hd_indx)) + geom_point(size = 2, color = "red") + 
  geom_point(data=idx, aes(x = co_indx, y = hdsp_indx), size = 2, color = "blue")

# Is hd_indx or hdsp_indx accurately representing years?
# 2012 is high, severe drought year by yr index, not so bad by sp index
# 2006 is classified differently by each index
ggplot(idx, aes(x = year, y = co_indx)) + geom_point(size = 4) + 
  geom_point(data=idx, aes(x = year, y = hd_indx), size = 2, color = "red") +
  geom_point(data=idx, aes(x = year, y = hdsp_indx), size = 2, color = "blue")
  
# They don't follow each other. Probably using the county average deviation is a better tactic?

####=========GROWING YEAR==========####
#
wea_eygLT$gyear <- as.character(wea_eygLT$gyear)
idx$gyear <- idx$year
cy_difs$gyear <- cy_difs$year

dat_eygLTid <- wea_eygLT %>% left_join(idx, by = "gyear") %>% left_join(cy_difs, by = "gyear")

####=========CALENDAR YEAR==========####
wea_eyLT$year <- as.character(wea_eyLT$year)
idx$year <- idx$year
cy_difs$year <- cy_difs$year

dat_eyLTid <- wea_eyLT %>% left_join(idx, by = "year") %>% left_join(cy_difs, by = "year")


# co_indx? No
ggplot(dat_eygLTid, aes(x=yT_C, y=yP_mm, size = co_indx)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = gLTP) +
  geom_vline(xintercept = gLTT)

# Note that the points move a lot based on 'year' definition, LT averages don't
# dif_4v3? No
ggplot(dat_eygLTid, aes(x=yT_C, y=yP_mm, size = dif_4v3)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = gLTP) +
  geom_vline(xintercept = gLTT) +
  # Calendar year
  geom_hline(yintercept = LTP, color = "red") +
  geom_vline(xintercept = LTT, color = "red") + 
  geom_point(data = dat_eyLTid, aes(x = yT_C, y = yP_mm, size = dif_4v3), alpha = 0.5, color = "red")

# sprd? No
ggplot(dat_eyLTid, aes(x=yT_C, y=yP_mm, size = sprd)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = gLTP) +
  geom_vline(xintercept = gLTT)


####==COMPARE YIELDS TO INDEXES ==================####

#-------Are cy yields related to hd_indx or county yields?
#

## Merge yrly yields w/ wea_hd
idx$year <- as.character(idx$year)
cy_yr$year <- as.character(cy_yr$year)
dat_idx <- idx %>% 
  left_join(cy_yr, by = "year")

# Are exp yields rltd to county yields?
#  Yes, as the county does better than average, exp yields also inc.
ggplot(dat_idx, aes(x = co_indx, y = mMg_ha)) + 
  geom_point(size = 2, aes(color = year)) + 
  geom_smooth()


####==COMPARE MARSDEN SPREADS, DIFFERENCES, ETc. TO INDEXES ==================####
#

cy_difs$year <- as.character(cy_difs$year)
dat_difx <- idx %>% 
  left_join(cy_difs, by = "year")

# Who wins most of the time? C4 = 9, C3 = 5
ggplot(dat_difx, aes(x=winner)) + geom_histogram(stat = "count")

# Is the sprd within a year related to the dif btwn C3 and C4? No. Be careful what you are asking.
ggplot(dat_difx, aes(x = sprd, y = pdif_4v3)) + geom_point()


# Is who wins rltd to co_indx? No, but
#  when C4 wins, it wins by a lot. When C3 wins, it's a squeaker.
#  Note : 0.4 Mg is ~6 bushels
#ggplot(dat_difx, aes(x = winner, y = co_indx)) + geom_point(alpha = 0.5, aes(size = sprd))
ggplot(dat_difx, aes(x = winner, y = co_indx)) + geom_point(alpha = 0.5, aes(size = pdif_4v3))
ggplot(dat_difx, aes(x = winner, y = co_indx)) + geom_point(alpha = 0.5, aes(size = dif_4v3))

# When the county average is high, is there less spread (i.e. is gap closed in good years)? No. 
#  In good years, the spread is higher. Except...
#  2013 is such a stinker. The county yields were low but Marsden had a big spread.
ggplot(dat_difx, aes(x = co_indx, y = sprd)) + geom_point(size = 4, aes(color = year))
ggplot(dat_difx, aes(x = coMg_ha, y = sprd)) + geom_point(size = 4, aes(color = year))

# Are Marsden yields rltd to spread? No.
ggplot(dat_difx, aes(x = C2, y = sprd)) + geom_point(size = 4, color = "red") +
  geom_point(data=dat_difx, aes(x=C3, y=sprd), size = 4, color = "green3" ) +
  geom_point(data=dat_difx, aes(x=C4, y=sprd), size = 4, color = "blue" ) 

# My indexes are even worse.
ggplot(dat_difx, aes(x = hd_indx, y = sprd)) + geom_point(size = 4, aes(color = year))
ggplot(dat_difx, aes(x = hdsp_indx, y = sprd)) + geom_point(size = 4, aes(color = year))

# Is the pdif_4v3 related to hd_indx? No.
ggplot(dat_difx, aes(x=hd_indx, y = pdif_4v3)) + geom_point()

# It changes the index by a lot
dat_xx <- dat_difx %>% right_join(dat_idx, by = "year") %>% select(year, hd_indx, hdx_indx)
ggplot(dat_xx, aes(x=hd_indx, y = hdx_indx)) + geom_point(size = 2, color = "red")

# This still looks terrible.
ggplot(dat_idxx, aes(x=hdx_indx, y = pdif_4v3)) + geom_point(size = 2, color = "red") + 
  geom_point(data = dat_idxdif, aes(x=hd_indx, y = pdif_4v3), size = 2)

ggplot(dat_idxx, aes(x=winner, y = hdx_indx)) + geom_point(size = 2, color = "red") + 
  geom_point(data = dat_idxdif, aes(x=winner, y = hd_indx), size = 2)


##-------Is hd_indx related to county corn yields?
#





####==Correlations of yield diff with monthly....============####
#
#

#-------------Precip?
p_mo <- wea_emLT %>% select(year, mo, mP_mm) %>% spread(mo, mP_mm) %>%
  right_join(cy_difs, by = "year") 
p_mo$year <- as.numeric(p_mo$year)

p_mo_corr <- cor(p_mo)
corrplot(p_mo_corr, method = "circle", type = "upper")
# Nothing.

#-------------Temp?
t_mo <- wea_ym %>% select(year, mo, mT_C) %>% spread(mo, mT_C) %>%
  right_join(cy_difs, by = "year") 
t_mo$year <- as.numeric(t_mo$year)

t_mo_corr <- cor(t_mo)
corrplot(t_mo_corr, method = "circle", type = "upper")
# Nothing



####=============Yields vs precip, Ricker curve==========####
# Maybe I could fit a 'yield max' curve and see if systems have different 'max' values? Ala Ranae. The Ricker curve!
################ Possible fun thing here!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Combine cy_yr with wea_eyLT
drick <- cy_yr %>% 
  left_join(wea_eyLT, by = "year")
ggplot(drick, aes(x = yP_mm, y = mMg_ha, color = trt)) + 
  geom_point(size = 4)


drick <- cy_yr %>% 
  mutate(gyear = year) %>%
  left_join(wea_eygLT, by = "gyear")
ggplot(drickG, aes(x = yP_mm, y = mMg_ha, color = trt)) + 
  geom_point(size = 4)


# Make separate dataframes for each rotation
drick_c4 <- filter(drick, trt == "C4")
drick_c3 <- filter(drick, trt == "C3")
drick_c2 <- filter(drick, trt == "C2")

# Define a Ricker curve to predict the yield based on yearly precip
rick<-function(x, a1, a2){
  a1 * x * exp(-x/a2)
  }

##--------Seperate one for each system
#

# C4==============================
#  Get initial guesses
x <- drick_c4$yP_mm
a1 <- 0.03
a2 <-  900

pred<- rick(x, a1, a2)
predD <- as.data.frame(pred)
predD$WI <- drick_c4$yP_mm

obfun_c4<- function(coef, x){
  sim <- rick(x=x, a1=coef[1], a2=coef[2])
  #obs <- rot$WUEs
  obs = drick_c4$mMg_ha
  rss <- sum((obs - sim)^2)
  rss
}


plot(mMg_ha~yP_mm, data = drick_c4)
points(predD$WI, predD$pred, type="p", col="red") 

op.c4 <- optim( par = c(0.03, 950), fn = obfun_c4, x = drick_c4$yP_mm) 
op.c4_params <- data.frame(sys = "c4", a1 = op.c4$par[1], a2 = op.c4$par[2] )
points(x, rick(x, op.c4$par[1], op.c4$par[2]),col=3)


newpred<-rick(x, op.c4$par[1], op.c4$par[2]) 
newpredD<-as.data.frame(newpred)
newpredD$WI<-drick_c4$yP_mm
op.c4_params$max <- max(newpredD$newpred)

# C3==============================
#  Get initial guesses
x <- drick_c3$yP_mm
a1 <- 0.03
a2 <-  900

pred<- rick(x, a1, a2)
predD <- as.data.frame(pred)
predD$WI <- drick_c3$yP_mm

plot(mMg_ha~yP_mm, data = drick_c3)
points(predD$WI, predD$pred, type="p", col="red") 

obfun_c3<- function(coef, x){
  sim <- rick(x=x, a1=coef[1], a2=coef[2])
  #obs <- rot$WUEs
  obs = drick_c3$mMg_ha
  rss <- sum((obs - sim)^2)
  rss
}


op.c3 <- optim( par = c(0.03, 950), fn = obfun_c3, x = drick_c3$yP_mm) 
op.c3_params <- data.frame(sys = "c3", a1 = op.c3$par[1], a2 = op.c3$par[2] )

points(x, rick(x, op.c3$par[1], op.c3$par[2]),col=3)

newpred<-rick(x, op.c3$par[1], op.c3$par[2]) 
newpredD<-as.data.frame(newpred)
newpredD$WI<-drick_c3$yP_mm
op.c3_params$max <- max(newpredD$newpred)


# C2==============================
#  Get initial guesses
x <- drick_c2$yP_mm
a1 <- 0.03
a2 <-  900

pred<- rick(x, a1, a2)
predD <- as.data.frame(pred)
predD$WI <- drick_c2$yP_mm

plot(mMg_ha~yP_mm, data = drick_c2)
points(predD$WI, predD$pred, type="p", col="red") 

obfun_c2<- function(coef, x){
  sim <- rick(x=x, a1=coef[1], a2=coef[2])
  #obs <- rot$WUEs
  obs = drick_c2$mMg_ha
  rss <- sum((obs - sim)^2)
  rss
}

op.c2 <- optim( par = c(0.03, 950), fn = obfun_c2, x = drick_c2$yP_mm) 
op.c2_params <- data.frame(sys = "c2", a1 = op.c2$par[1], a2 = op.c2$par[2] )
points(x, rick(x, op.c2$par[1], op.c2$par[2]),col=3)

newpred<-rick(x, op.c2$par[1], op.c2$par[2]) 
newpredD<-as.data.frame(newpred)
newpredD$WI<-drick_c2$yP_mm
op.c2_params$max <- max(newpredD$newpred)

opall <- bind_rows(op.c4_params, op.c3_params, op.c2_params)
opall

rct <- data.frame(x = seq(600, 1400, 25))
rc4 <- rct %>% 
  mutate(y = rick(x=x, a1 = opall[1,2], a2 = opall[1,3]))
rc3 <- rct %>% 
  mutate(y = rick(x=x, a1 = opall[2,2], a2 = opall[2,3]))
rc2 <- rct %>% 
  mutate(y = rick(x=x, a1 = opall[3,2], a2 = opall[3,3]))


ggplot(drick, aes(x = yP_mm, y = mMg_ha) ) + 
  geom_point(size = 4, aes (color = trt) ) + 
  geom_line(data  = rc4, aes(x, y), color = "blue") +
  geom_line(data  = rc3, aes(x, y), color = "green3") +
  geom_line(data  = rc2, aes(x, y), color = "red")
         

