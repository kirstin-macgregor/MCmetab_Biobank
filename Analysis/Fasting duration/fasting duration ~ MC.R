#UK BB data set- MC phase ~ fasting duration

#---------------------------------------------------------------------------------------------------------
#load and arrange data 

rm(list= ls())

setwd("/Users/kirstinMac/Desktop/OneDrive - University of Stirling/PhD/Studies/BioBank/Data")

library(tidyverse)
library(ggplot2)
library(lme4)
library(mgcv)
library('tidymv')

data <- read.csv("filtered.data_05.08.21.csv")
data <- data  %>% filter(n_74_0_0 >=4,age <51) %>% transform(n_74_0_0= as.integer(n_74_0_0)) %>% drop_na(n_74_0_0)

#---------------------------------------------------------------------------------------------------------
mod1 <- gam(n_74_0_0 ~s(MC.status, bs="cc"), data=data, family='quasipoisson')
plot_smooths(model= mod1, series= MC.status)
summary(mod1)