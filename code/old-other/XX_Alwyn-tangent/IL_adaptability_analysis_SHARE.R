
## Adaptability analyses
########################

# This file provides a compressed/tidied version of the analyses and derived variables
# put together by Adam Davis
# The final .csv output file is needed for the SEM buffering analysis

library(lattice)
library(nlme)
library(plyr)
library(dplyr)
library(ggplot2)
library(psych)
library(broom)

####==SETS WORKING DIRECTORY TO WHEREVER ACTIVE DOCUMENT IS KEPT===####
#
path <- rstudioapi::getActiveDocumentContext()$path
Encoding(path) <- "UTF-8"
setwd(dirname(path))

## Set working directory
# setwd()

ilmz <- read.table("IL_corn_aa_2000 to 2014.txt",header=T)

sapply(ilmz,class) #are variable classes correct?

EI.by.cty<-aggregate(ilmz[7], ilmz[4], FUN=mean) #calculate county-level means
EI.by.year<-aggregate(ilmz[7], ilmz[2], FUN=mean) #calculate year-level means

#Add EI by year (15 years)
for (i in 1:length(unique(EI.by.year$Year))){
  ilmz[ilmz$Year==EI.by.year[i,1],8]<-EI.by.year[i,2]
}

#Add EI by county (102 counties)
for (i in 1:length(unique(EI.by.cty$County))){
  ilmz[ilmz$County==EI.by.cty[i,1],9]<-EI.by.cty[i,2]
}

names(ilmz)[8:9]<-c("EI.yr.MgHa","EI.cty.MgHa")

###########################################################################
###########################################################################

## Visualize range in yearly behaviors, by county
attach(ilmz)
mz<-groupedData(Corn.Mg.ha~EI.yr.MgHa|Cty.num,
                data=ilmz, labels=list(x="Environmental index by year (Mg/ha)", y = "Maize yield by county (Mg/ha)"))

plot(mz,as.table=TRUE,layout = c(12, 7), aspect=0.6) 
detach(ilmz)

###########################################################################
###########################################################################

attach(ilmz)
mz<-groupedData(Corn.Mg.ha~EI.yr.MgHa|Cty.num,
                data=ilmz,labels=list(x="Environmental index by year (Mg/ha)", y = "Maize yield by county (Mg/ha)"))
detach(ilmz)

# No random effects, constant slope
fm1.lm<-gls(Corn.Mg.ha ~ EI.yr.MgHa, data=mz, method='ML')
summary(fm1.lm)
anova(fm1.lm)

# Random shifts by yield dist, constant slope
fm2.lme<-lme(Corn.Mg.ha ~ EI.yr.MgHa, data=mz, random = ~ 1|County, method='ML')
summary(fm2.lme)
anova(fm2.lme)

# Random shift to intercept & slope
fm3.lme<-lme(Corn.Mg.ha ~ EI.yr.MgHa, data=mz, random = ~ EI.yr.MgHa|County, method='ML')
summary(fm3.lme)
anova(fm3.lme)

anova(fm1.lm, fm2.lme, fm3.lme)
# Nothing to choose between random intercept and random intercept-slopes models
# Random intercept model is most parsimonious
# But a key part of our analysis is investigation of slope shifts
# So stick with random intercept-slope model (fm3.lme)

## Use augment() from broom package to make plot straight from model
augment(fm3.lme) %>%
  ggplot(aes(x=EI.yr.MgHa, y=Corn.Mg.ha, group=County)) +
  geom_line(aes(x=EI.yr.MgHa, y=.fitted), col="grey50") +
  geom_line(aes(x=EI.yr.MgHa, y=.fixed), col="black", lwd=2)

## To explore the fitted values (BLUPs)
augment(fm3.lme)

###########################################################################
###########################################################################

## END