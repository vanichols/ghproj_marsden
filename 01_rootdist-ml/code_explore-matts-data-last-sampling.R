library(tidyverse)

d <- read_csv("000_manu/FCR-resubmission/dat_roots-lastsampledoy-4matt.csv")


d %>%
  mutate(depth = as.factor(depth),
         depth= fct_rev(depth))  %>% 
  ggplot(aes(depth, roots_kgha, fill = rot_trt)) + 
  geom_boxplot() + 
  coord_flip() +
  labs(title = "Root data at end of season, all years")
