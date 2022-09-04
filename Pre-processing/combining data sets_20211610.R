#UK BB data set- merging data files.
library(tidyverse)
library(haven) #read dta files
rm(list= ls())
setwd("/Users/kirstinMac/Desktop/OneDrive - University of Stirling/PhD/Studies/BioBank/Data/Original data files")

#full data file
data.full <- read.csv('Data_UKB7155_Kirstin_Stirling_Uni.csv')
data.full <-  transform(data.full, eid = as.numeric(eid)) # check eid is as numeric.

#inflammatory markers file
data.inflam <- read_dta("Data_inflam.dta")
data.inflam <-  transform(data.inflam, eid = as.numeric(eid)) # check eid is as numeric.

#check eid for data.full and data.inflam are the same.
data.full.eid <- select(data.full, 'eid') #make new data frame with data.full eid column
data.inflam.eid <- select(data.inflam, 'eid') #make new data frame with data.inflam eid column
data.eids1 <- rbind(data.full.eid, data.inflam.eid) 
sum(duplicated(data.eids1$eid)) # 502,487 duplicate eid values

# merge data sets 
data.joined <- left_join(data.full, data.inflam, by='eid')

#fasting time file
data.fastingtime <- read_dta("Data_fasting time.dta")
data.fastingtime <- data.fastingtime %>% rename('eid'= 'n_eid') #re-name participant ID column so it is the same as the full data file.
data.fastingtime <-  transform(data.fastingtime, eid = as.numeric(eid)) # convert eid to numeric so it's the same as the full data file.
data.fastingtime <- data.fastingtime %>% rename(eid3 = eid)

#check eid for data.full and data.fastingtime are the same.
data.full.eid <- select(data.full, 'eid3') #make new data frame with data.full eid column
data.fastingtime.eid <- select(data.fastingtime, 'eid3') #make new data frame with data.inflam eid column
data.eids2 <- rbind(data.full.eid, data.fastingtime.eid) 
sum(duplicated(data.eids2$eid3)) # 502,473 duplicate eid values

# merge data sets
data.joined <- left_join(data.joined, data.fastingtime, by='eid3') 


write.csv(data.joined, 'data.full_10.06.21.csv')





