# try to do gam on penetration data
# started 10/16
# updated: 10/19 (make knots more reasonable)

library(rethinking)
library(maRsden)
library(dplyr)
library(tidyr)
dat <- 
  mrs_penetrom %>% 
  filter(year == 2018) %>% 
  filter(doy == 130) %>% 
  left_join(mrs_plotkey) %>% 
  ungroup() %>% 
  select(block, plot_id, rot_trt, depth_cm, resis_kpa)

#  resis ~ s(depth) + rot_trt (?)

# work through this...
# https://m-clark.github.io/generalized-additive-models/application.html

# knots (divides the data into sections)
# basis (the function of x to be used, ex polynomial spline basis)

data("cherry_blossoms")
datraw <- cherry_blossoms

dat <- datraw %>% 
  filter(!is.na(doy))

summary(datraw)
summary(dat)


# breate knot list --------------------------------------------------------


num_knots <- 15
#--could split them evenly? not sure how to do that
#split(dat, ceiling(seq_along(dat)/20))
# cut_interval() makes n groups with equal range, 
# cut_number() makes n groups with (approximately) equal numbers of observations
split(dat$year, cut_number(dat$year, num_knots)) 

cut_interval(rexp(100, 5), 10)
cut_number(rexp(100, 5), 10)
#--I give up

#--could use quantiles, as data isn't equally distributed
hist(dat$year) #--less observations in the way back time

knot_list <- quantile(dat$year, probs = seq(0, 1, length.out = num_knots))
knot_list
length(knot_list)

library(splines)
B1 <- bs(x = dat$year,
         knots = knot_list[-c(1, num_knots)], #--no idea why you have to do this
         degree = 1, #four functions combine within each interval between knots
         intercept = T)

#--what is he doing to the knot list?!
knot_list
a <- c(1, 3, 4, 8)
a[-c(1,4)]
a[c(-1,-4)]
a[-1] #--removes the first
a[-c(1, 15)]

knot_list[-c(1, num_knots)] #--ohhhh he's removing the first and last knots. 

B1 <- bs(x = dat$year,
         knots = knot_list[-c(1, num_knots)], #--no idea why you have to do this
         degree = 1, #defines number of functions to combine within each interval between knots, 1 means 2 funcs
         intercept = T) #--not sure what this does

B1

# plot( NULL, xlim = range(dat$year), ylim = c(0, 1), xlab = "year", ylab = "basis")
# for (i in 1:ncol(B1) ) lines (dat$year, B1[,i])

B1 %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(year = dat$year) %>%
  select(year, everything()) %>% 
  pivot_longer(x1:x15) %>% 
  ggplot(aes(year, value, group = name)) + 
  geom_line()

#--try with interept as F
B1f <- bs(x = dat$year,
         knots = knot_list[-c(1, num_knots)], #--no idea why you have to do this
         degree = 1, #defines number of functions to combine within each interval between knots, 1 means 2 funcs
         intercept = F) #--not sure what this does

# plot( NULL, xlim = range(dat$year), ylim = c(0, 1), xlab = "year", ylab = "basis")
# for (i in 1:ncol(B1) ) lines (dat$year, B1[,i])

B1f %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(year = dat$year) %>%
  select(year, everything()) %>% 
  pivot_longer(x1:x14) %>% 
  ggplot(aes(year, value, group = name)) + 
  geom_line()

#--oh if there is an intercept, then the thing doesn't have to start at 0. 
# in my case with penetrometer data, I probably want it to start at 0. 


B3 <- bs(x = dat$year,
        knots = knot_list[-c(1, num_knots)], #--no idea why you have to do this
        degree = 3, #four functions combine within each interval between knots
        intercept = T)

B3 %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(year = dat$year) %>%
  select(year, everything()) %>% 
  pivot_longer(x1:x17) %>% 
  ggplot(aes(year, value, group = name, color = name)) + 
  geom_line() + 
  geom_vline(aes(xintercept = year), data = tibble(year = as.numeric(knot_list))) +
  guides(color = F) 


# degree of 1 -------------------------------------------------------------

#--now he fits a quap
mB1 <- 
  quap(
    alist(
      D ~ dnorm( mu, sigma),
      mu <- a + B %*% w,
      a ~ dnorm(100, 10),
      w ~ dnorm(0, 10),
      sigma ~ dexp(1)
    ), data = list( D = dat$doy, B = B1 ), 
    start = list(w = rep(0, ncol(B1))))

precis(mB1, depth = 2) #--this shows our weights
post <- extract.samples(mB1) #--I still struggle with what this actually is
w <- apply(post$w, 2, mean) #--2 means column-wise. Getting a mean value for each w

plot( NULL, xlim = range(dat$year), ylim = c(-6, 6), 
      xlab = "year", ylab = "basis * wgt")
for ( i in 1:ncol(B1)) lines (dat$year, w[i]*B1[,i] )

mu <- link(mB1) #--again, how is this different from extract.samples??
mu_PI <- apply(mu, 2, PI, 0.97)
plot(dat$year, dat$doy, col = col.alpha(rangi2, 0.3), pch  =16)
shade(mu_PI, dat$year, col = col.alpha("black", 0.5))

# degree of 3 -------------------------------------------------------------

#--now he fits a quap
mB3 <- 
  quap(
    alist(
      D ~ dnorm( mu, sigma),
      mu <- a + B %*% w,
      a ~ dnorm(100, 10),
      w ~ dnorm(0, 10),
      sigma ~ dexp(1)
    ), data = list( D = dat$doy, B = B3 ), 
    start = list(w = rep(0, ncol(B3))))

post <- extract.samples(mB3) #--I still struggle with what this actually is
w <- apply(post$w, 2, mean) #--2 means column-wise. Getting a mean value for each w

plot( NULL, xlim = range(dat$year), ylim = c(-6, 6), 
      xlab = "year", ylab = "basis * wgt")
for ( i in 1:ncol(B3)) lines (dat$year, w[i]*B3[,i] )

mu <- link(mB3) #--again, how is this different from extract.samples??
mu_PI <- apply(mu, 2, PI, 0.97)
plot(dat$year, dat$doy, col = col.alpha(rangi2, 0.3), pch  =16)
shade(mu_PI, dat$year, col = col.alpha("black", 0.5))


# try with penetration data from one treatment and one year ---------------

#--NOT WORKING. I have bad priors, I think
# w should not be normally distributed. It always needs to be positive. 
#--I guess I could center and scale the resistances?
# Scaling didn't work either. 
# maybe I need more data, shouldn't average by plot. Need to fix that in the package. 

md <- 
  mrs_penetrom %>% 
  left_join(mrs_plotkey) %>% 
  filter(year == "2018", rot_trt == "2y", doy == 130) %>% 
  mutate(resis_Mpa = resis_kpa/1000,
         resis_sc = scale(resis_kpa))

plot(md$depth_cm, md$resis_sc, col = col.alpha(rangi2, 0.3), pch  =16)

ggplot() + 
  geom_point(data = md, aes(x = depth_cm, y = resis_Mpa)) 

#--create knot list, dividing into 6 depths seems reasonable
hist(md$depth_cm)
num_knots <- 4
knot_list <- quantile(md$depth_cm, probs = seq(0, 1, length.out = num_knots))
knot_list
length(knot_list)

#--build basis functions, 0 intercept if not scaled, T if it is scaled

B3pen <- bs(x = md$depth_cm,
          knots = knot_list[-c(1, num_knots)], #--remove first and last knots
          degree = 3, #defines number of functions to combine within each interval between knots, 1 means 2 funcs
          intercept = F) 

B3pen %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  mutate(depth_cm = md$depth_cm) %>%
  select(depth_cm, everything()) %>% 
  pivot_longer(x1:x5) %>% 
  ggplot(aes(depth_cm, value, group = name, color = name)) + 
  geom_line() + 
  guides(color = F) 

#--now fit a quap
mB3m <- 
  quap(
    alist(
      D ~ dnorm( mu, sigma),
      mu <- B %*% w, #--add a if intercept (scaled)
      #a ~ dnorm(-2.5, 1),
      w ~ dnorm(0, 10),
      sigma ~ dexp(1)
    ), data = list( D = md$resis_Mpa, B = B3pen ), 
    start = list(w = rep(0, ncol(B3pen))))

post <- extract.samples(mB3m) #--I still struggle with what this actually is
w <- apply(post$w, 2, mean) #--2 means column-wise. Getting a mean value for each w

plot( NULL, xlim = range(md$depth_cm), ylim = c(0, 1), 
      xlab = "depth_cm", ylab = "basis * wgt")
for ( i in 1:ncol(B3pen)) lines (md$depth_cm, w[i]*B3pen[,i] )

mu_pen <- link(mB3m) #--again, how is this different from extract.samples??
mu_pen_PI <- apply(mu_pen, 2, PI, 0.97)
plot(md$depth_cm, md$resis_Mpa, col = col.alpha(rangi2, 0.3), pch  =16)
shade(mu_pen_PI, md$depth_cm, col = col.alpha("black", 0.5))
plot(mu_pen, md$depth_cm)

