## Make fig(s) for supp
##
## Date: aug 10 2021
## updated: sept 15 2021 to use matt's JMP results

rm(list = ls())
library(tidyverse)
library(patchwork)
library(maRsden)

source("03_manu-figs/palettes.R")

theme_set(theme_bw())

my_ylab <- (expression(atop("Root mass at maize maturity", paste("relative to root mass at planting (kg "~ha^-1*")"))))

# matt's results ----------------------------------------------------------

dat <- 
  read_csv("01_rootdist-ml/manual_mattJMPres-diff.csv") 

dat_tot <- 
  dat %>% 
  #--can I do this?
  group_by(rot_trt) %>% 
  summarise(mean = sum(mean),
            se = mean(se)) %>% 
  mutate(depth = "Total (0-60 cm)")

dat %>% 
  bind_rows(dat_tot) %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = factor(rot_trt, levels = c("Simple", "Complex")),
         depth = fct_inorder(depth)) %>% 
  ggplot(aes(rot_trt, mean)) + 
  geom_col(aes(fill = rot_trt), color= "black") +
  geom_linerange(aes(x = rot_trt, ymin = mean - se, 
                     ymax = mean + se)) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple", "Complex")) + 
  guides(fill = F, color = F) +
  labs(x = NULL,
       y = my_ylab,
       color = "Rotation",
       fill = "Rotation") +
  facet_grid(.~depth) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = rel(1.1)),
        legend.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.1)),
        strip.background = element_blank())

ggsave("03_manu-figs/fig_rootmass-JMP.png", width = 6.93, height = 4.12)





# old, don't use, JMP wins ------------------------------------------------


rad <- read_csv("01_rootdist-ml/dat_em-change-depth.csv")
rad_tot <- read_csv("01_rootdist-ml/dat_sig-change-total.csv") %>% 
  mutate(depth = "Total\n0-60cm")

bind_rows(rad_tot, rad) %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
       rot_trt = factor(rot_trt, levels = c("Simple", "Complex")),
       depth = fct_inorder(depth)) %>% 
  ggplot(aes(rot_trt, estimate)) + 
  geom_col(aes(fill = rot_trt), color= "black") +
  geom_linerange(aes(x = rot_trt, ymin = estimate - std.error, 
                     ymax = estimate + std.error)) +
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple", "Complex")) + 
  guides(fill = F, color = F) +
  labs(x = NULL,
       y = expression("Root mass added ("~kg~ha^-1*")"), 
       color = "Rotation",
       fill = "Rotation") +
  facet_grid(.~depth) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.2)),
        strip.background = element_blank())

ggsave("03_manu-figs/fig_rootmass.png", width = 7.98, height = 5.11)


# calc diff ---------------------------------------------------------------


rm <- read_csv("01_rootdist-ml/dat_em-beg-end-by-depth.csv")

rm_added <- 
  rm %>% 
  select(depth, rot_trt, samp_time, estimate) %>% 
  pivot_wider(names_from = samp_time, values_from = estimate) %>% 
  mutate(radd = end - beg)

rm_added_tot <- 
  rm_added %>% 
  group_by(rot_trt) %>% 
  summarise(radd = sum(radd)) %>% 
  mutate(depth = "Total Profile\n0-60cm") %>% 
  bind_rows(rm_added) %>% 
  mutate(depth = fct_inorder(depth))

rm_added_tot %>% 
  select(rot_trt, radd, depth) %>% 
  pivot_wider(names_from = depth, values_from = radd) %>% 
  janitor::clean_names() %>% 
  mutate(ptop = x0_15cm/total_profile_0_60cm)

rm_added_tot %>% 
  mutate(rot_trt = ifelse(rot_trt == "2y", "Simple", "Complex"),
         rot_trt = factor(rot_trt, levels = c("Simple", "Complex"))) %>% 
  ggplot(aes(rot_trt, radd)) + 
  geom_col(aes(fill = rot_trt), color= "black") + 
  scale_color_manual(values = c(pnk1, dkbl1),
                     labels = c("Simple", "Complex")) + 
  scale_fill_manual(values = c(pnk1, dkbl1),
                    labels = c("Simple", "Complex")) + 
  guides(fill = F, color = F) +
  labs(x = NULL,
       y = expression("Root mass added ("~kg~ha^-1*")"), 
       color = "Rotation",
       fill = "Rotation") +
  facet_grid(.~depth) + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.background = element_rect(color = "black"),
        legend.title = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.1)),
        strip.text = element_text(size = rel(1.2)),
        strip.background = element_blank())

ggsave("03_manu-figs/fig_rootmass.png", width = 7.98, height = 5.11)

rm %>% 
  mutate(samp_time2 = ifelse(samp_time == "beg", 1, 2)) %>% 
  ggplot(aes(samp_time2, estimate)) + 
  geom_point(aes(color = rot_trt)) + 
  geom_line(aes(color = rot_trt)) + 
  scale_color_manual(values = c(pnk1, dkbl1)) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("Planting", "Maturity"),
                     expand = expansion(add = c(0.5, 0.5))) +
  theme(panel.grid.minor.x = element_blank()) +
  facet_grid(depth~., )
