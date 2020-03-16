#############################
##
## Created: April 2 2019
## Last modified: Oct 31 2019 (diff in yields over time? didn't work)
##
## Purpose: Make graphs of yields
##
## INPUTS: td_corn-yields
##
## OUPUTS: corn-ylds-by-trt-pretty.png, div-benefit-varies-by-block.png, etc.  
##
#3
##############################


##

rm(list=ls())
library(tidyverse)
library(lubridate)
library(broom)
library(emme)

crn <- read_csv("_theme-explore-prev-data/_data/_tidy/td_corn-yields.csv") %>% 
  arrange(year)

mypal <- c("green4", "olivedrab1")
myylab <- expression(Maize~Diversity~(Mg~ha^-1))

crn %>% 
  group_by(year, trt) %>% 
  summarise(yld = mean(yld_Mgha)) %>% 
  filter(trt != "C3", year > 2004) %>% 
  mutate(trtdesc = ifelse(trt == "C4", 
                          "Following Alfalfa",
                          "Following Soybean")) %>% 
  
  ggplot(aes(year, yld)) + 
  geom_line(aes(color = trtdesc), size = 4) +
  geom_point() + 
  scale_color_manual(values = mypal) +
  theme_bw() + 
  labs(color = NULL, y = myylab, x = NULL) +
  theme(legend.justification = c(0, 0),
        legend.position = c(0.1,0.1),
        legend.text = element_text(size = rel(1.3)),
        axis.title = element_text(size = rel(1.3)),
        axis.text = element_text(size = rel(1.2)))

ggsave("_figs/fig_ASA-maize-ylds.png", height = 4, width = 6)


dome <- 
  crn %>% 
  select(-yld_buac) %>% 
  nest(data = c(trt, block, yld_Mgha)) %>% 
  mutate(thing = 1:n()) %>% 
  unnest(cols = c(data))

myis <- dome %>% pull(thing) %>% unique()

fun_lm <- function(fdata = data){
  
  fres <- tidy(summary(lm(yld_Mgha ~ trt*block, data = fdata))) %>% 
    select(term, estimate, std.error) 
  
  return(fres)
  
}


res1 <- 
  dome %>% 
  select(-thing) %>% 
  nest(data = c(year, trt, block, yld_Mgha)) %>%
  mutate(mod = data %>% map(fun_lm)) %>% 
  unnest(mod) %>% 
  select(-data) %>% 
  mutate(thing = (length(myis) + 1))


for (i in 1:length(myis)){
  
  #i <- 1
  
  lres <- 
    dome %>% 
    filter(thing <= i) %>% 
    select(-thing) %>% 
    nest(data = c(year, trt, block, yld_Mgha)) %>%
    mutate(mod = data %>% map(fun_lm)) %>% 
    unnest(mod) %>% 
    select(-data) %>% 
    mutate(thing = i)
    
  res1 <- bind_rows(res1, lres)
  
  i <- i +1
  
}



stders <- 
  res1 %>% 
  filter(!grepl("block", term)) %>% 
  mutate(term = str_replace(term, "\\(Intercept\\)", "trtC2")) %>%
  select(term, thing, std.error) %>% 
  spread(key = term, value = std.error) %>% 
  gather(trtC2:trtC4, key = "trt", value = "stderr")


mns <- 
res1 %>% 
  filter(!grepl("block", term)) %>% 
  mutate(term = str_replace(term, "\\(Intercept\\)", "trtC2")) %>%
  select(term, thing, estimate) %>% 
  spread(key = term, value = estimate) %>% 
  mutate(trtC3 = trtC2 + trtC3,
         trtC4 = trtC2 + trtC4) %>% 
  gather(trtC2:trtC4, key = "trt", value = "mean")

mns %>% left_join(stders) %>% 
  mutate(ymin = mean-stderr, ymax = mean+stderr) %>% 
  ggplot(aes(thing, mean)) + 
  geom_point(aes(color = trt)) + 
  geom_linerange(aes(thing, ymin = ymin, ymax = ymax, color = trt))



