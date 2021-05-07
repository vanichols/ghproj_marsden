# Data processing

#--should I scale the soil moisture data?

library(magrittr)
library(maRsden)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(lemon) # chunk option `render = lemon_print` makes tables prettier


rawdat <- 
  mrs_soilsensors %>% 
  filter(sensor_unit == "soilVWC") %>% 
  left_join(mrs_plotkey) %>%
  select(year, doy, block, rot_trt, plot_id, sensor_depth_cm, value) %>% 
  arrange(block, plot_id, sensor_depth_cm)


sw15 <- 
  rawdat %>% 
  filter(sensor_depth_cm == 15) %>% 
  select(-sensor_depth_cm) %>% 
  # make new factor variables and convert old trt/block variables into factors
  mutate(
  plot_id = as.factor(plot_id),
      block = as.factor(block),
    rot_trt = as.factor(rot_trt)
  )

  
# sorry, I couldn't figure out how to do this in mutate() without gnarly warnings
# myd$year_doy <- as.factor(paste(myd$year, myd$doy, sep = "_"))
# myd$trt_block_yr <- as.factor(paste(myd$rot_trt, myd$block, myd$year, myd$doy, sep = "_"))
# myd$trt_yr_doy <- as.factor(paste(myd$rot_trt, myd$year, myd$doy, sep = "_"))
# myd$block <- as.factor(myd$block)
# myd$rot_trt <- as.factor(myd$rot_trt)
# 
# head(myd, 12)

#--I have 2 years of data
#--each year, there are 4 blocks, each with 2 treatments

# may want to account for interaction w/ year? looks like pattern changes over time
ggplot(data = sw15) + 
  geom_line(aes(x = doy, y = value, group = plot_id)) +
  facet_wrap( ~ year + rot_trt)

# Fit and check GAM
# The general format of a gam() call is gam(y ~ s(x1, by = x2, k = 15) + x2, data = data), 
# where x terms within s() control the shape of the wiggly curve(s), 
# x terms outside of s() control the intercept(s) (if desired), and k controls the number of knots. 
# Also, I think you need x1 to be continuous and x2 to be a factor/category, but I admittedly had trouble figuring this out...

library(mgcv)
#--try just doing 2018 first
#--lots of variance at doy > 260
saapsim::saf_doy_to_date(260)

sw15_18 <- 
  sw15 %>% 
  filter(year == 2018, doy < 260) %>% 
  filter(!is.na(value))

mod <- gam(value ~ s(doy, by = plot_id, bs = "cr", k = 70) + plot_id,
           data = sw15_18, method = "REML")

plot(mod, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod)

#To [check a GAM with `gam.check()`](https://rdrr.io/cran/mgcv/man/gam.check.html), we look for a few things:
  
#   - If `edf` is too close to `k'`, we may need more knots
# - `k-index` is the estimate divided by the residual variance. 
# The further below 1 this is, the more likely it is that there is missed pattern left in the residuals.
# - The p-value for the `k-index` is computed by simulation: the residuals are randomly re-shuffled 
# `k.rep` times to obtain the null distribution of the differencing variance estimator, if there is no pattern in the residuals.
# - If the p-value is too close to zero, there is a significant pattern in the residuals that should be addressed.
# 
# In this case, the `edf` values are not too close to 7, and the `k-index` is close to 1, but the p-values are a bit low for my taste. This is likely due to the heteroskedastic residuals (i.e., megaphone shape), because the curves are all close to zero at zero depth but spread out a lot for higher depths.
# 

# view the 24 fitted curves to visually inspect differences
sw15_18$p <- predict(mod)

# 
ggplot(data = sw15_18) +
  geom_line(aes(x = doy, y = p, color = rot_trt, group = plot_id)) + # note p^2
  facet_wrap( ~ year, ncol = 2) 
# 
# ## View residuals by group
# myd$resid <- mod$residuals
# 
# ggplot(data = myd) + 
#   geom_line(aes(x = depth_cm, y = resid, group = rep_id, color = year_doy)) +
#   facet_wrap( ~ trt_block_yr, ncol = length(unique(myd$year_doy)))
# ## View data with fitted curves by group
# 
# ggplot(data = myd) + 
#   geom_line(aes(x = depth_cm, y = resis_Mpa, group = rep_id, color = year_doy)) +
#   geom_line(aes(x = depth_cm, y = p^2, group = rep_id), color = "black") + # note p^2
#   facet_wrap( ~ trt_block_yr, ncol = length(unique(myd$year_doy)))


# Remove year that is most unlike the others and (re)refit

myd <- filter(myd, year != 2020) # modifying in place because I'm lazy :(
myd$trt_block_yr <- as.factor(as.character(myd$trt_block_yr))
myd$trt_yr_doy <- as.factor(as.character(myd$trt_yr_doy))

mod <- gam(sqrt(resis_Mpa) ~ s(depth_cm, by = trt_block_yr, bs = "cr", k = 8) + trt_block_yr, 
           data = myd, method = "REML")

# plot(mod, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod)

#I'm happy with the `edf`, `k-index`, and p-values here, so we can visualize the model and residuals, and then use the model for inference.

#(If you don't want to throw away the 2020 data entirely, you could fit another GAM to just those data.)

## View fitted model by group

# view the 24 fitted curves to visually inspect differences
myd$p <- predict(mod)

# Q: is there a way to predict on the level of year_doy_trt? I think the term is marginal over blocks?

ggplot(data = myd) +
  geom_line(aes(x = depth_cm, y = p^2, color = year_doy, group = year_doy)) + # note p^2
  facet_wrap( ~ block + rot_trt, ncol = 2) +
  guides(color = FALSE) + 
  labs(y = "resis_MPa")


ggplot(data = myd) +
  geom_line(aes(x = depth_cm, y = p^2, group = trt_block_yr, color = rot_trt)) + # note p^2
  facet_wrap( ~ year_doy, ncol = 2) +
  labs(y = "resis_MPa")


# Try w/o intercept
mod2 <- gam(sqrt(resis_Mpa) ~ s(depth_cm, by = trt_block_yr, bs = "cr", k = 8), 
           data = myd, method = "REML")

# plot(mod, residuals = TRUE, shade = TRUE)
par(mar = c(4, 4, 3, 0))
mgcv::gam.check(mod2)
# Oh very bad. 

## View residuals by group

myd$resid <- mod$residuals

ggplot(data = myd) + 
  geom_point(aes(x = depth_cm, y = resid, group = rep_id, color = year_doy)) +
  facet_wrap( ~ trt_block_yr, ncol = length(unique(myd$year_doy)))


## View data with fitted curves by group

ggplot(data = myd) + 
  geom_line(aes(x = depth_cm, y = resis_Mpa, group = rep_id, color = year_doy)) +
  geom_line(aes(x = depth_cm, y = p^2, group = rep_id), color = "black") + # note p^2
  facet_wrap( ~ trt_block_yr, ncol = length(unique(myd$year_doy)))

# Differences between treatments

#To test for differences between treatments, we'll use the following function from [https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/](https://fromthebottomoftheheap.net/2017/10/10/difference-splines-i/):

smooth_diff <- function(model, newdata, f1, f2, var, alpha = 0.05,
                        unconditional = FALSE) {
    xp <- predict(model, newdata = newdata, type = 'lpmatrix')
    c1 <- grepl(f1, colnames(xp))
    c2 <- grepl(f2, colnames(xp))
    r1 <- newdata[[var]] == f1
    r2 <- newdata[[var]] == f2
    ## difference rows of xp for data from comparison
    X <- xp[r1, ] - xp[r2, ]
    ## zero out cols of X related to splines for other lochs
    X[, ! (c1 | c2)] <- 0
    ## zero out the parametric cols
    X[, !grepl('^s\\(', colnames(xp))] <- 0
    dif <- X %*% coef(model)
    se <- sqrt(rowSums((X %*% vcov(model, unconditional = unconditional)) * X))
    crit <- qt(alpha/2, df.residual(model), lower.tail = FALSE)
    upr <- dif + (crit * se)
    lwr <- dif - (crit * se)
    data.frame(pair = paste(f1, f2, sep = '-'),
               diff = dif,
               se = se,
               upper = upr,
               lower = lwr)
}

#We want to compare the differences between treatments, within blocks and years. First, we set up a data frame that pairs the variable levels between treatments, within blocks and year/doy values.

#--can we just do within year_doy?

# get all combinations of block and year/doy
base_str <- as.vector(sapply(unique(myd$block), function(s) {
  paste(s, unique(myd$year_doy), sep = "_")
  }))

# get all combinations of year/doy
base_str2 <- as.vector(unique(myd$year_doy))


# add trt to the front of each base str, yielding pairwise df
var_df <- data.frame(var1 = paste("2y", base_str, sep = "_"),
                     var2 = paste("4y", base_str, sep = "_"),
                     stringsAsFactors = FALSE)

head(var_df)

var_df2 <- data.frame(var1 = paste("2y", base_str2, sep = "_"),
                     var2 = paste("4y", base_str2, sep = "_"),
                     stringsAsFactors = FALSE)

var_df2

#Next, we create a data frame for prediction that contains the depth values and levels of `trt_block_yr`.

newdata <- expand.grid(depth_cm = unique(myd$depth_cm),
                        trt_block_yr = levels(myd$trt_block_yr))

#--make depth smoother/more consistent?
newdata2 <- expand.grid(depth_cm = seq(0, 45, by = 1),
                       trt_block_yr = levels(myd$trt_block_yr))

#Create one for prediction that contains the depth values and levels of `trt_yr_doy` (not working in fxn).

newdata3 <- expand.grid(depth_cm = seq(0, 45, by = 1),
                        trt_yr_doy = levels(myd$trt_yr_doy))
head(newdata3)

#-test
f1.test <- var_df[1, 1]
f2.test <- var_df[1, 2]

d.test <- smooth_diff(mod, newdata, 
                      f1 = f1.test,
                      f2 = f2.test,
                      var = "trt_block_year")

#--ok, doesn't work, work through function itself...

xp <- predict(mod, newdata, type = 'lpmatrix')
c1 <- grepl("2y_b1_2018_130", colnames(xp))
c2 <- grepl("4y_b1_2018_130", colnames(xp))
r1 <- newdata[["trt_block_year"]] == "2y_b1_2018_130"
r2 <- newdata[["trt_block_year"]] == "4y_b1_2018_130"
## difference rows of xp for data from comparison
X <- xp[r1, ] - xp[r2, ]
## zero out cols of X related to splines for other lochs
X[, ! (c1 | c2)] <- 0
## zero out the parametric cols
X[, !grepl('^s\\(', colnames(xp))] <- 0
dif <- X %*% coef(mod)
se <- sqrt(rowSums((X %*% vcov(mod, unconditional = FALSE)) * X)) #--? doesn't work?
crit <- qt(0.05/2, df.residual(mod), lower.tail = FALSE)
upr <- dif + (crit * se)
lwr <- dif - (crit * se)
data.frame(pair = paste(f1, f2, sep = '-'),
           diff = dif,
           se = se,
           upper = upr,
           lower = lwr)



# but this works. Ugh what am I missing. 
out <- purrr::map_dfr(1:nrow(var_df), function(i) {
  d <- smooth_diff(mod, newdata, 
              f1 = var_df[i,1], f2 = var_df[i,2], 
              var = "trt_block_yr")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})

comp <- cbind(depth_cm = unique(myd$depth_cm), out) # add depth values
comp$pair <- as.factor(comp$pair) # make this a factor again for ggplot2
head(comp)

out2 <- purrr::map_dfr(1:nrow(var_df), function(i) {
  d <- smooth_diff(mod, newdata2, 
                   f1 = var_df[i,1], f2 = var_df[i,2], 
                   var = "trt_block_yr")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})

comp2 <- cbind(depth_cm = seq(0, 45, by = 1), out2) # add depth values
comp2$pair <- as.factor(comp2$pair) # make this a factor again for ggplot2
head(comp2)

#--try w/o block?
out3 <- purrr::map_dfr(1:nrow(var_df3), function(i) {
  d <- smooth_diff(mod, newdata2, 
                   f1 = var_df3[i,1], f2 = var_df3[i,2], 
                   var = "trt__yr")
  d$pair <- as.character(d$pair) # prevent map_dfr from combining factors
  return(d)
})



#First, remember that the model is predicting sqrt(resis_Mpa), so we should transform the predictions and confidence intervals back to their original scale.

comp2$diff2 <- comp2$diff^2
comp2$upper2 <- comp2$upper^2
comp2$lower2 <- comp2$lower^2

#Now, we can plot the difference between treatments, with associated confidence bands, for each block and year/doy combination. Any depth values where the shading does not cross the dashed red line (at diff = 0) are points where the treatment resistances differ significantly.

ggplot(comp, aes(x = depth_cm, y = diff2, group = pair)) +
    geom_ribbon(aes(ymin = lower2, ymax = upper2), alpha = 0.2) +
    geom_line() +
    geom_hline(aes(yintercept = 0), colour="#990000", linetype="dashed") +
    facet_wrap(~ pair, ncol = 2) +
    labs(x = NULL, y = 'Difference in resis_Mpa trend') +
  ggtitle("Difference in resis_Mpa trend", subtitle = "Original scale")


ggplot(comp, aes(x = depth_cm, y = diff, group = pair)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line() +
    geom_hline(aes(yintercept = 0), colour="#990000", linetype="dashed") +
    facet_wrap(~ pair, ncol = 2) +
    labs(x = NULL, y = 'Difference in resis_Mpa trend') +
  ggtitle("Difference in resis_Mpa trend", subtitle = "Sqrt scale")


# play a fanfare noise when everything is done running
beepr::beep(3)


#--get a list of depths that have sig diffs

comp2 %>% 
  as_tibble() %>%
  mutate(t2yH = ifelse(lower > 0, 1, 0),
         t4yH = ifelse(upper < 0, 1, 0),
         trtsig = t2yH + t4yH) %>%
  mutate(diffneg = ifelse(diff < 0, -1, 1),
         diff2 = diff2*diffneg) %>% 
  filter(trtsig != 0) %>% 
  separate(pair, into = c("p1", "p2"), sep = "-") %>% 
  separate(p1, into = c("trt", "block", "year", "doy")) %>% 
  select(block, year, doy, depth_cm, diff, diff2) %>% 
  group_by(year, doy, depth_cm) %>% 
  summarise(diff2 = mean(diff2)) %>% 
  ggplot(aes(diff2, depth_cm)) + 
  geom_hline(yintercept = 10, linetype = "dashed") +
  geom_hline(yintercept = 30, linetype = "dashed") +
  geom_point(aes(color = interaction(year, doy))) + 
  scale_y_reverse() 


#
comp %>% 
  as_tibble() %>% 
  mutate(t2yH = ifelse(lower > 0, 1, 0),
         t4yH = ifelse(upper < 0, -1, 0)) %>% 
  separate(pair, into = c("p1", "p2"), sep = "-") %>% 
  separate(p1, into = c("trt", "block", "year", "doy")) %>% 
  group_by(depth_cm, year, doy) %>% 
  summarise(t2yH = sum(t2yH),
            t4yH = sum(t4yH),
            trt = t2yH + t4yH,
         trt.col = ifelse(trt >0, "happymatt", "sadmatt")) %>% 
  ggplot() + 
  # geom_col(aes(x = depth_cm, y = t2yH), fill = "green4") +
  # geom_col(aes(x = depth_cm, y = t4yH), fill = "red4") +
  geom_col(aes(x = depth_cm, y = trt, fill = trt.col)) +
  coord_flip() + 
  scale_x_reverse() + 
  facet_wrap(~year + doy)

#-can I add 'windows' where the differences are significant?

ggplot(data = myd) +
  geom_line(aes(x = depth_cm, y = p^2, group = trt_block_yr, color = rot_trt)) + # note p^2
  facet_wrap( ~ year_doy, ncol = 2) 

