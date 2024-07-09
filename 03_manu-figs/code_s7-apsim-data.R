#--comes from ghproj_ccgap repo, manu figs folder
#--added here 1/29/2024
#--show range in impact of adjusting root front velocity

rm(list = ls())
library(tidyverse)
library(saapsim) #--has some functions
library(tidysawyer2) #--has sawyer data
library(janitor)

source("03_manu-figs/palettes.R")

oats_nice <- read_csv("03_manu-figs/oats_nice.csv")

gaps_filt <- read_csv("03_manu-figs/dat_tidy-hand-oats.csv")

#--this is worthless but don't want to worry about adjusting code
cis <- read_csv("03_manu-figs/dat_gap-cis.csv") %>%
  filter(site == "ames")

new_exp <- 
  gaps_filt %>% 
  select(dtype, oat_nu, year, oat_what, category, notes) %>% 
  filter(dtype == "exp_gap") %>% 
  left_join(
    cis %>% 
      select(year, gap_kgha, gap_hi, gap_lo)
  )

gaps_filt_cis <- 
  gaps_filt %>% 
  filter(dtype != "exp_gap") %>% 
  bind_rows(new_exp)

# windmill ----------------------------------------------------------------

yrs_ord <- 
  gaps_filt_cis %>% 
  filter(oat_what == "exp gap", category == "1 factor") %>% 
  arrange(-gap_kgha) %>% 
  select(year) %>% 
  pull()

gaps_filt_wind <- 
  gaps_filt_cis %>%
  left_join(oats_nice) %>% 
  arrange(oat_what_order) %>% 
  mutate(year = factor(year, levels = yrs_ord),
         oat_what = fct_inorder(oat_what),
         oat_what_nice = fct_inorder(oat_what_nice)) %>% 
  group_by(dtype, oat_what, category) %>% 
  mutate(mngap = mean(gap_kgha, na.rm = T))

wind_theme_V <-    theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

wind_theme_H <-    theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())



# 1 and 5 together --------------------------------------------------------

f1 <- 
  gaps_filt_wind %>%
  filter(category %in% c("1 factor"), oat_what != "dec RFV10%") 

f5 <- 
  gaps_filt_wind %>%
  filter(category %in% c("5 factor"), oat_what != "dec RFV10%") %>%
  filter(!grepl("gap", oat_what))


#--kl, rfv, k#, rue, emerg, plant pop

f_dat <- 
  f1 %>% 
  bind_rows(f5)  %>% 
  mutate(oat_scen1 = oat_what_order - 2,
         oat_scen_lab = ifelse(oat_scen1 <= 0, paste(oat_what_nice),
                               ifelse(oat_scen1 == 16, "Combine Scenarios 1, 3, 4, 5, 6",
                                      paste0("Scenario ", oat_scen1)))
  ) 

f_dat_oat_labs <- f_dat %>% pull(oat_scen_lab) %>% unique()
#f_dat_oat_labs <- f_dat %>% pull(oat_what) %>% unique()

f_dat %>% 
  filter(oat_nu == 0) %>% 
  arrange(gap_kgha) %>% 
  filter(gap_kgha == min(gap_kgha)|gap_kgha == max(gap_kgha))

f_dat %>% 
  ungroup() %>% 
  mutate(oat_scen_lab = factor(oat_scen_lab, levels = f_dat_oat_labs)) %>% 
  mutate(
    #year = factor(year, levels = yrs_ord),
    col1 = case_when(
      oat_what == "exp gap" ~ "A",
      oat_what == "current apsim gap" ~ "B",
      oat_what == "late emergence" ~ "C",
      TRUE ~ "D")
  ) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = col1, color = col1),
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) +
  geom_linerange(aes(ymin = gap_lo, ymax = gap_hi), color = "gray40") +
  facet_grid(.~oat_scen_lab, labeller = label_wrap_gen(width = 10)) + 
  guides(fill = F, color = F) +
  scale_x_discrete(breaks = c(NA, NA)) +
  scale_fill_manual(values = c("C" = dkbl1, "B" = grn1, "A" = ylw1, "D" = dkpnk1)) +
  scale_color_manual(values = c("C" = dkbl1, "B" = grn1, "A" = ylw1, "D" = dkpnk1)) +
  geom_hline(aes(yintercept = mngap), size = 1, type = "dashed") +
  labs(#title = "Ames",
    x = "Year, ordered by largest to smallest observed penalty",
    y = (expression(atop("Continuous maize penalty at site IA-4", paste("(kg "~ha^-1*")"))))) + 
  wind_theme_H


# root front velocity -----------------------------------------------------

# scenario 4 and 22 are root front velocity (inc 10% and 50% resp)

gaps_filt %>% 
  ungroup() %>% 
  select(oat_nu, year, gap_kgha) %>% 
  filter(oat_nu %in% c(4, 22),
         !is.na(year)) %>%  
  mutate(oat_lab = ifelse(oat_nu == 4, "Increase RFV 10%", "Increase RFV 50%"),
         year = fct_inorder(as.factor(year))) %>% 
  ggplot(aes(year, gap_kgha)) + 
  geom_bar(aes(fill = oat_lab), color = "black",
           position = "dodge", 
           stat = "identity", 
           #color = "black"
  ) +
  #facet_grid(.~oat_lab, labeller = label_wrap_gen(width = 10)) + 
  #guides(fill = F, color = F) +
  scale_fill_manual(values = c(ylw1, dkpr2)) +
  labs(#title = "Ames",
    x = "Year",
    y = (expression(atop("Increase in maize yield", paste("(kg "~ha^-1*")")))),
    color = NULL,
    fill = NULL,
    caption = "RFV = Root front velocity",
    title = "APSIM (Keating et al. 2003) simulations of maize yield\nin Ames, Iowa USA (Archontoulis et al. 2020)", label_wrap_gen(width = 10)) +
  theme(legend.position = c(0.8, 0.8))

ggsave("03_manu-figs/s7_apsim.png", width = 7, height = 4)
