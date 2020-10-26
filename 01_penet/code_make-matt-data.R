# try to do gam on penetration data
# started 10/16
# updated:

library(maRsden)
library(tidyverse)
library(mgcv)

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


dat %>% 
  ggplot(aes(depth_cm, resis_kpa, color = plot_id)) + 
  geom_line()

library(mgcv)
data(trees)

trees %>% 
  as_tibble()

#--here they are smoothing both height and girth relatinoships
#--why a gamma family? why a log link?
ct1<-gam(Volume~s(Height)+s(Girth),  family=Gamma(link=log),data=trees, method="REML")

# the degree of smoothness of the functions is chosen by gcv.
ct1
summary(ct1)
#To visualize gam fitting:              
plot(ct1,residuals=TRUE)  
# The plots show the estimated effects as solid lines/curves, with 95% confidence limits
#(strict Bayesian credible intervals) shown as dashed lines.

#The default basis for gam function in package mgcv uses thin plate regression splines, 
# which have some appealing properties, but can be computationally costly for large data sets.  
# For faster computing, we may use penalized cubic regression splines as the basis for fitting GAM, 
# by adding  one additional argument in "s"

ct2<-gam(Volume~s(Height,bs="cr")+s(Girth, bs="cr"),  family=Gamma(link=log),data=trees)
 
# The number of basis k used can be specified within  "s" part, the default value is 10.  For instance, if we want to reset k=5:
  > ct3<-gam(Volume~s(Height, bs="cr", k=5)+s(Girth, bs="cr",k=5),  family=Gamma(link=log),data=trees)
38.
39.                              One of the advantages of the gam function package mgcv is that it can automatically select the degree of smoothness under a certain criterion.  
This can be specified in "method" argument.   Smoothing parameters are chosen to minimize the GCV, UBRE/AIC, GACV or REML scores for the model. The default method is GCV.  
> ct4<-gam(Volume~s(Height, bs="cr", k=5)+s(Girth, bs="cr",k=5),  family=Gamma(link=log),data=trees,method="REML")


40.                             Diagnosis:  
  >gam.check(ct3)
This function plots 4 standard diagnostic plots, and some other convergence diagnostics.  
Check   
>?gam    