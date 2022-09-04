#UK BB data set- MC phase ~ fasting duration

#---------------------------------------------------------------------------------------------------------
#load and arrange data 

rm(list= ls())

setwd("P:/C3_Integrative_Physiology_Group/PeoplesData/Kirstin/Projects/UoS/MC and metabolites_BioBank/Analysis/data/filtered")

library(tidyverse)
library(ggplot2)
library(lme4)
library(mgcv)
library('tidymv')

data <- read.csv("filtered.data_20220627")
data <- data  %>% filter(n_74_0_0 >=4,age <51) %>% transform(n_74_0_0= as.integer(n_74_0_0)) %>% drop_na(n_74_0_0)

#---------------------------------------------------------------------------------------------------------
mod1 <- gam(n_74_0_0 ~s(MC.status, bs="cc"), data=data, family='quasipoisson')
plot_smooths(model= mod1, series= MC.status)
summary(mod1)