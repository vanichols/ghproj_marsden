library(tidyverse)


# 6/19/2018 calibration ---------------------------------------------------

# My data (this is the only place it is kept, no excel file)
lai <- tribble(
  ~act_cm2, ~meas_cm2,
  50  , 49.8,
  190,  141,
  241,  180.5,
  380,  278)


ggplot(lai, aes(meas_cm2, act_cm2)) + 
  geom_point(color = "red", size = 5) + 
  geom_abline() + 
  geom_smooth(method = "lm", se = F) +
  xlim(0, 400) + 
  ylim(0, 400)

# Fit a linear model and force it through 0.0
# I should see if it ever over-estimates though, maybe it shouldn't go through 0,0
lm(lai2$newact ~ 0+lai2$newmeas)

1.4

(278-10)*1.4 + 10

278*1.46

141*1.4
