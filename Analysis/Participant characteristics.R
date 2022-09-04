#UK BB data set- participant characteristics
#-------------------------------------------------------------------------------------------------------

rm(list= ls())
setwd("/Users/kirstinMac/Desktop/OneDrive - University of Stirling/PhD/Studies/BioBank/Data")
library(tidyverse)
library(ggplot2)
library(lme4)
library(mgcv)
library('tidymv')

#-------------------------------------------------------------------------------------------------------
#load and arrange data 
data <- read.csv("filtered.data_12.08.21.csv")
data <- data %>% transform(bmi_cat4 = as.factor(bmi_cat4 ),
                           n_22035_0_0 = as.factor(n_22035_0_0),
                           n_22036_0_0 = as.factor(n_22036_0_0),
                           n_22032_0_0 = as.factor(n_22032_0_0),
                           n_21050_0_0 = as.factor(n_21050_0_0),
                           lightPA_yesterday = as.factor(lightPA_yesterday),
                           moderatePA_yesterday = as.factor(moderatePA_yesterday),
                           #HBA1c.Cat = as.factor(HBA1c.Cat),
                           HBA1c.tertiles=as.factor(HBA1c.tertiles),
                           Fitness_METs_c.tertiles=as.factor(Fitness_METs_c.tertiles),
                           muslcle.mass.perc.tertiles= as.factor(muslcle.mass.perc.tertiles),
                           fat.mass.perc.tertiles= as.factor(fat.mass.perc.tertiles),
                           MPA.tertiles= as.factor(MPA.tertiles),
                           VPA.tertiles= as.factor(VPA.tertiles),
                           WPA.tertiles= as.factor(WPA.tertiles),
                           METsummed.tertiles= as.factor(METsummed.tertiles),
                           Bodyfatp.tertiles= as.factor(Bodyfatp.tertiles),
                           ffm.tertiles= as.factor(ffm.tertiles),
                           fm.tertiles= as.factor(fm.tertiles),
                           ffmi.tertiles= as.factor(ffmi.tertiles),
                           fmi.tertiles= as.factor(fmi.tertiles),
                           handgripkg.tertiles= as.factor(handgripkg.tertiles),
                           Timevigorousphysactivity = as.factor(Timevigorousphysactivity),
                           ethnicity_cat= as.factor(ethnicity_cat),
                           age= as.numeric(age)) %>% 
  filter(n_74_0_0 >=4, age <51)

#-------------------------------------------------------------------------------------------------------
#mean ± sd continuous variables.

mean_all <- data %>%summarise(age  =mean(age, na.rm=TRUE),
                              weight_kg = mean(weight_kg, na.rm=TRUE),
                              height =mean(height, na.rm=TRUE),
                              bmi = mean(bmi, na.rm=TRUE),
                              bodyfat_kg = mean(bodyfat_kg, na.rm=TRUE),
                              bodyfat_p = mean(bodyfat_p, na.rm=TRUE),
                              FMI=mean(FMI, na.rm=TRUE),
                              bodyfatfreemass = mean(bodyfatfreemass, na.rm=TRUE),
                              SMusclemasspct_c= mean(SMusclemasspct_c, na.rm=TRUE),
                              FFMI=mean(FFMI, na.rm=TRUE),
                              HbA1c_0 = mean(HbA1c_0, na.rm=TRUE),
                              n_22037_0_0 = mean(n_22037_0_0, na.rm=TRUE),
                              n_22038_0_0 = mean(n_22038_0_0, na.rm=TRUE),
                              n_22039_0_0=  mean(n_22039_0_0, na.rm=TRUE),
                              MET.summed = mean(MET.summed, na.rm=TRUE),
                              handgripkg = mean(handgripkg, na.rm=TRUE),
                              Fitness_METs_c = mean(Fitness_METs_c, na.rm=TRUE),
                              n_30800_0_0 = mean(n_30800_0_0, na.rm=TRUE),
                              n_30770_0_0 = mean(n_30770_0_0, na.rm=TRUE),
                              n_30830_0_0 = mean(n_30830_0_0, na.rm=TRUE),
                              CRP_0 = mean(CRP_0, na.rm=TRUE),
                              n_30740_0_0 = mean(n_30740_0_0, na.rm=TRUE),
                              TG_0 = mean(TG_0, na.rm=TRUE),
                              tyG.index2 = mean(tyG.index2, na.rm=TRUE),
                              Tchol_0=mean(Tchol_0, na.rm=TRUE),
                              HDL_0 = mean(HDL_0, na.rm=TRUE),
                              ldl_0 = mean(ldl_0, na.rm=TRUE),
                              TC.HDL=  mean(TC.HDL, na.rm=TRUE),
                              deprivationindex= mean(deprivationindex, na.rm=TRUE),
                              MClength= mean(n_3710_0_0, na.rm=TRUE))

sd_all <- data %>%summarise(age  =sd(age, na.rm=TRUE),
                            weight_kg = sd(weight_kg, na.rm=TRUE),
                            height =sd(height, na.rm=TRUE),
                            bmi = sd(bmi, na.rm=TRUE),
                            bodyfat_kg = sd(bodyfat_kg, na.rm=TRUE),
                            bodyfat_p = sd(bodyfat_p, na.rm=TRUE),
                            FMI=sd(FMI, na.rm=TRUE),
                            bodyfatfreemass = sd(bodyfatfreemass, na.rm=TRUE),
                            SMusclemasspct_c= sd(SMusclemasspct_c, na.rm=TRUE),
                            FFMI=sd(FFMI, na.rm=TRUE),
                            HbA1c_0 = sd(HbA1c_0, na.rm=TRUE),
                            n_22037_0_0 = sd(n_22037_0_0, na.rm=TRUE),
                            n_22038_0_0 = sd(n_22038_0_0, na.rm=TRUE),
                            n_22039_0_0=  sd(n_22039_0_0, na.rm=TRUE),
                            MET.summed = sd(MET.summed, na.rm=TRUE),
                            handgripkg = sd(handgripkg, na.rm=TRUE),
                            Fitness_METs_c = sd(Fitness_METs_c, na.rm=TRUE),
                            n_30800_0_0 = sd(n_30800_0_0, na.rm=TRUE),
                            n_30770_0_0 = sd(n_30770_0_0, na.rm=TRUE),
                            n_30830_0_0 = sd(n_30830_0_0, na.rm=TRUE),
                            CRP_0 = sd(CRP_0, na.rm=TRUE),
                            n_30740_0_0 = sd(n_30740_0_0, na.rm=TRUE),
                            TG_0 = sd(TG_0, na.rm=TRUE),
                            tyG.index2 = sd(tyG.index2, na.rm=TRUE),
                            Tchol_0=sd(Tchol_0, na.rm=TRUE),
                            HDL_0 = sd(HDL_0, na.rm=TRUE),
                            ldl_0 = sd(ldl_0, na.rm=TRUE),
                            TC.HDL=  sd(TC.HDL, na.rm=TRUE),
                            deprivationindex= sd(deprivationindex, na.rm=TRUE),
                            MClength= sd(n_3710_0_0, na.rm=TRUE))



summary_all <- rbind(mean_all, sd_all)

#-------------------------------------------------------------------------------------------------------
#random stuff
ages <- data %>% summarise(min.age = min(age, na.rm=TRUE),
                  max.age= max(age, na.rm = TRUE))

#N values
counts_ <- data[!is.na(data$TC.HDL), ] 
counts_ <- counts_ %>% as_tibble()  %>% count(n_30740_0_0)
summarise_count <- counts_ %>% summarise(total= sum(n))
summarise_count

#-------------------------------------------------------------------------------------------------------
# percentage in each category

ethnicity_cat_count <- data %>% as_tibble() %>% count(ethnicity_cat)
ethnicity_cat_total <- as.numeric(summarise(ethnicity_cat_count, total.ethnicity_cat = sum(n)))
ethnicity_cat_count <- ethnicity_cat_count %>% mutate(percentage.total = (n/ethnicity_cat_total)*100)

bmi_cat4_count <- data %>% as_tibble() %>% count(bmi_cat4)
bmi_cat4_total <- as.numeric(summarise(bmi_cat4_count, total.bmi_cat4 = sum(n)))
bmi_cat4_count <- bmi_cat4_count %>% mutate(percentage.total = (n/bmi_cat4_total)*100)

n_21050_0_0_count <- data %>% as_tibble() %>% count(n_21050_0_0)
n_21050_0_0_total <- as.numeric(summarise(n_21050_0_0_count, total.n_21050_0_0 = sum(n)))
n_21050_0_0_count <- n_21050_0_0_count %>% mutate(percentage.total = (n/n_21050_0_0_total)*100)

n_21026_0_0_count <- data %>% as_tibble() %>% count(n_21026_0_0)
n_21026_0_0_total <- as.numeric(summarise(n_21026_0_0_count, total.n_21026_0_0 = sum(n)))
n_21026_0_0_count <- n_21026_0_0_count %>% mutate(percentage.total = (n/n_21026_0_0_total)*100)

n_3720_0_0_count <- data %>% as_tibble() %>% count(n_3720_0_0)
n_3720_0_0_total <- as.numeric(summarise(n_3720_0_0_count, total.n_3720_0_0 = sum(n)))
n_3720_0_0_count <- n_3720_0_0_count %>% mutate(percentage.total = (n/n_3720_0_0_total)*100)
data %>% as_tibble() %>% count(n_3720_0_0)

n_22032_0_0_count <- data %>% filter(n_22032_0_0 == "0" |n_22032_0_0 == "1" | n_22032_0_0 == "2")
n_22032_0_0_count <- n_22032_0_0_count %>% as_tibble() %>% count(n_22032_0_0)
n_22032_0_0_total <- as.numeric(summarise(n_22032_0_0_count, total.n_22032_0_0 = sum(n)))
n_22032_0_0_count <- n_22032_0_0_count %>% mutate(percentage.total = (n/n_22032_0_0_total)*100)
data %>% as_tibble() %>% count(n_22032_0_0)

HBA1c.Cat_count <- data %>% filter(HBA1c.Cat == "0" |HBA1c.Cat == "1" | HBA1c.Cat == "2", HBA1c.Cat == "3")
HBA1c.Cat_count <- HBA1c.Cat_count %>% as_tibble() %>% count(HBA1c.Cat)
HBA1c.Cat_total <- as.numeric(summarise(HBA1c.Cat_count, total.HBA1c.Cat = sum(n)))
HBA1c.Cat_count <- HBA1c.Cat_count %>% mutate(percentage.total = (n/HBA1c.Cat_total)*100)
data %>% as_tibble() %>% count(HBA1c.Cat)

#-------------------------------------------------------------------------------------------------------
#by different categories:

summary_bmi <- data %>% group_by(bmi_cat4) %>% summarise(tyG.index  =mean(tyG.index2, na.rm=TRUE),
                                                         glucose = mean(n_30740_0_0, na.rm=TRUE),
                                                         HBA1c = mean(HbA1c_0, na.rm=TRUE),
                                                         HDL = mean(HDL_0, na.rm=TRUE),
                                                         LDL = mean(ldl_0, na.rm=TRUE),
                                                         Chol.ratio = mean(TC.HDL, na.rm=TRUE),
                                                         TGL = mean(TG_0, na.rm=TRUE),
                                                         BMI=  mean(bmi, na.rm=TRUE),
                                                         CRP = mean(CRP_0, na.rm=TRUE),
                                                         IGF1 = mean(n_30770_0_0, na.rm=TRUE),
                                                         SHBG=  mean(n_30830_0_0, na.rm=TRUE))

summary_IPAQcat <- data %>% group_by(n_22032_0_0) %>% summarise(tyG.index  =mean(tyG.index2, na.rm=TRUE),
                                                                glucose = mean(n_30740_0_0, na.rm=TRUE),
                                                                HBA1c = mean(HbA1c_0, na.rm=TRUE),
                                                                HDL = mean(HDL_0, na.rm=TRUE),
                                                                LDL = mean(ldl_0, na.rm=TRUE),
                                                                Chol.ratio = mean(TC.HDL, na.rm=TRUE),
                                                                TGL = mean(TG_0, na.rm=TRUE),
                                                                BMI=  mean(bmi, na.rm=TRUE),
                                                                CRP = mean(CRP_0, na.rm=TRUE),
                                                                IGF1 = mean(n_30770_0_0, na.rm=TRUE),
                                                                SHBG=  mean(n_30830_0_0, na.rm=TRUE))                                                 

#-------------------------------------------------------------------------------------------------------
#by different categories:
#n_30800_0_0
#CRP_0
#n_30770_0_0

#n_30740_0_0
#Tchol_0
#HDL_0
#ldl_0
#TC.HDL
#TG_0
#tyG.index2

data1 <- data %>% filter(!is.na(ethnicity_cat),
                         !is.na(deprivationindex),
                         !is.na(age))

data2 <- data1 %>% filter(!is.na(n_30740_0_0))


#FM %
count <- data2 %>% filter(fat.mass.perc.tertiles == "1" | fat.mass.perc.tertiles == "2" | fat.mass.perc.tertiles == "3" | fat.mass.perc.tertiles =="4")
count2 <- count %>% as_tibble() %>% count(fat.mass.perc.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Fat mass (%)',
                            Variable= 'tyG.index2')
#combined_counts <- count2
combined_counts <- rbind(combined_counts, count2)

#Skeletal muscle mass %
count <- data2 %>% filter(muslcle.mass.perc.tertiles == "1" | muslcle.mass.perc.tertiles == "2" | muslcle.mass.perc.tertiles == "3" | muslcle.mass.perc.tertiles=="4")
count2 <- count %>% as_tibble() %>% count(muslcle.mass.perc.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Skeletal muscle mass (%)',
                            Variable= 'tyG.index2')
combined_counts <- rbind(combined_counts, count2)

#IPAQ
count <- data2 %>% filter(n_22032_0_0 == "0" |n_22032_0_0 == "1" | n_22032_0_0 == "2")
count2 <- count %>% as_tibble() %>% count(n_22032_0_0)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'IPAQ',
                            Variable= 'tyG.index2')
combined_counts <- rbind(combined_counts, count2)

#HBA1c
count <- data2 %>% filter(HBA1c.Cat == "1" | HBA1c.Cat == "2" | HBA1c.Cat == "3")
count2 <- count %>% as_tibble() %>% count(HBA1c.Cat)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'HBA1c (mmol/mol)',
                            Variable= 'tyG.index2')
combined_counts <- rbind(combined_counts, count2)

#CRF tertiles (%)
count <- data2 %>% filter(Fitness_METs_c.tertiles == "1" | Fitness_METs_c.tertiles == "2" | Fitness_METs_c.tertiles == "3" | Fitness_METs_c.tertiles=="4")
count2 <- count %>% as_tibble() %>% count(handgripkg.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Cardiorespiratory fitness (METs)',
                            Variable= 'tyG.index2')
combined_counts <- rbind(combined_counts, count2)

#handgripkg tertiles %
count <- data2 %>% filter(handgripkg.tertiles == "1" | handgripkg.tertiles == "2" | handgripkg.tertiles == "3" | handgripkg.tertiles=="4")
count2 <- count %>% as_tibble() %>% count(handgripkg.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Grip strength (kg)',
                            Variable= 'tyG.index2')
combined_counts <- rbind(combined_counts, count2)

#-------------------------------------------------------------------------------------------------------
# in MC symptom excluded cohort



data1 <- data %>% filter(!is.na(ethnicity_cat),
                         !is.na(deprivationindex),
                         !is.na(age))

data.n_21050_0_0 <- data1 %>% filter(n_21050_0_0 == "-601" | n_21050_0_0 == "-602")
data.n_21026_0_0 <- data1 %>% filter(n_21026_0_0 == "1")
Data.MCsymptoms <- rbind(data.n_21050_0_0, data.n_21026_0_0)
data1<- anti_join(data1, Data.MCsymptoms, by = 'eid')

#by different categories:
#n_30800_0_0
#CRP_0
#n_30770_0_0

#n_30740_0_0
#Tchol_0
#HDL_0
#ldl_0
#TC.HDL
#TG_0
#tyG.index2

data2 <- data1 %>% filter(!is.na(tyG.index2))



#FM %
count <- data2 %>% filter(fat.mass.perc.tertiles == "1" | fat.mass.perc.tertiles == "2" | fat.mass.perc.tertiles == "3" | fat.mass.perc.tertiles =="4")
count2 <- count %>% as_tibble() %>% count(fat.mass.perc.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Fat mass (%)',
                            Variable= 'n_30740_0_0')
#combined_counts <- count2
combined_counts <- rbind(combined_counts, count2)

#Skeletal muscle mass %
count <- data2 %>% filter(muslcle.mass.perc.tertiles == "1" | muslcle.mass.perc.tertiles == "2" | muslcle.mass.perc.tertiles == "3" | muslcle.mass.perc.tertiles=="4")
count2 <- count %>% as_tibble() %>% count(muslcle.mass.perc.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Skeletal muscle mass (%)',
                            Variable= 'n_30740_0_0')
combined_counts <- rbind(combined_counts, count2)

#IPAQ
count <- data2 %>% filter(n_22032_0_0 == "0" |n_22032_0_0 == "1" | n_22032_0_0 == "2")
count2 <- count %>% as_tibble() %>% count(n_22032_0_0)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'IPAQ',
                            Variable= 'n_30740_0_0')
combined_counts <- rbind(combined_counts, count2)

#HBA1c
count <- data2 %>% filter(HBA1c.Cat == "1" | HBA1c.Cat == "2" | HBA1c.Cat == "3")
count2 <- count %>% as_tibble() %>% count(HBA1c.Cat)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'HBA1c (mmol/mol)',
                            Variable= 'n_30740_0_0')
combined_counts <- rbind(combined_counts, count2)

#CRF tertiles (%)
count <- data2 %>% filter(Fitness_METs_c.tertiles == "1" | Fitness_METs_c.tertiles == "2" | Fitness_METs_c.tertiles == "3" | Fitness_METs_c.tertiles=="4")
count2 <- count %>% as_tibble() %>% count(handgripkg.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Cardiorespiratory fitness (METs)',
                            Variable= 'n_30740_0_0')
combined_counts <- rbind(combined_counts, count2)

#handgripkg tertiles %
count <- data2 %>% filter(handgripkg.tertiles == "1" | handgripkg.tertiles == "2" | handgripkg.tertiles == "3" | handgripkg.tertiles=="4")
count2 <- count %>% as_tibble() %>% count(handgripkg.tertiles)
total <- as.numeric(summarise(count2, total = sum(n)))
count2 <- count2 %>% mutate(percentage.total = (n/total)*100)
colnames(count2) <- c( 'cat', 'N', 'perc')
count2 <- count2 %>% mutate(Covariate= 'Grip strength (kg)',
                            Variable= 'n_30740_0_0')
combined_counts <- rbind(combined_counts, count2)