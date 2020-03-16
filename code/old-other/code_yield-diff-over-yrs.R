#########################
#
# Date of creation: Oct 31 2019
# Date last modified: 
# Author: Gina Nichols (virginia.nichols@gmail.com)
#
# Purpose: Fit mixed linear models to all response variables and all modifiers
#            as a cumulative year of pub
#          
# Inputs: 
#
# Outputs:
#
# NOTE: See what estimates look like over years
#
#########################

# Clear env, load packages, set wd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom)


#source("00_weeds_functions.R")


#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Read in data -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~

#dat <- read_csv("data-tidy-PIVI.csv")
datraw <- read_csv("_theme-explore-prev-data/_data/_tidy/td_corn-yields.csv") %>% 
  filter(trt %in% c("C2", "C4")) %>% 
  arrange(year)

#~~~~~~~~~~~~~~~~~~~~~~~~
#
# Loop through adding next year's studies -----------------------------------
#
#~~~~~~~~~~~~~~~~~~~~~~~~~


dat <- datraw %>% 
  mutate(year_id = group_indices(., year)) %>%
  select(year_id, everything(), -yld_buac)

F.res <- lmer(yld_Mgha ~ trt*block + (1|year), data = dat)
tidy(fixef(F.res))
#F.res <- lmer(yi ~ 1 + (1|study_id), 
#              data = mydata, 
#              weights = wgt) 

contest(F.res, L = 1, joint = F, level = 0.95)
# Extract lmer results using contest from lmerTest package
F.tidy <- contest(F.res, L = 1, joint = F, level = 0.99) 

# Fix names
names(F.tidy) <- c("est", "se", "df", "t", "ci_99low", "ci_99up", "p_val")

F.tidy <- F.tidy %>%
  mutate(mod = resp,
         desc = "intercept") %>%
  select(mod, desc, everything())





denres <- tibble(year = NA,
                 mod = NA,
                 est = NA,
                 se = NA,
                 ci_99low = NA,
                 ci_99up = NA,
                 p_val = NA)

for (i in 2:max(den$year_id)) {

  dome <- den %>%
    filter(year_id %in% c(1:i))
  
  year.tmp <- max(dome$year)
  
  res.tmp <- RunModelNoModsFun(dome, "den") %>%
    select(mod, est, se, ci_99low, ci_99up, p_val) %>%
    mutate(year = year.tmp)
  
  denres <- bind_rows(denres, res.tmp)
  
  
}

denres2 <- denres %>%
  filter(!is.na(year),
         year !=1992) 

denres2 %>%
  ggplot(aes(year, est)) + 
  geom_pointrange(aes(ymin = ci_99low, ymax = ci_99up))


# biomass -----------------------------------------------------------------


bio <- dat %>% 
  select(-denLRR) %>%
  filter(!is.na(bioLRR)) %>%
  mutate(yi = bioLRR) %>%
  mutate(year = str_sub(reference, -4, -1)) %>%
  select(year, everything()) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(year) %>%
  mutate(year_id = group_indices(., year)) %>%
  select(year_id, everything())


biores <- tibble(year = NA,
                 mod = NA,
                 est = NA,
                 se = NA,
                 ci_99low = NA,
                 ci_99up = NA,
                 p_val = NA)

for (i in 1:max(bio$year_id)) {
  
  dome <- bio %>%
    filter(year_id %in% c(1:i))
  
  year.tmp <- max(dome$year)
  
  res.tmp <- RunModelNoModsFun(dome, "bio") %>%
    select(mod, est, se, ci_99low, ci_99up, p_val) %>%
    mutate(year = year.tmp)
  
  biores <- bind_rows(biores, res.tmp)
  
  
}

biores2 <- biores %>%
  filter(!is.na(year),
         year > 1997)

biores2 %>%
  ggplot(aes(year, est)) + 
  geom_pointrange(aes(ymin = ci_99low, ymax = ci_99up))

# write that shit ---------------------------------------------------------

myres <- bind_rows(denres2, biores2) 

myres %>%
  write_csv("stats_est-over-time.csv")

