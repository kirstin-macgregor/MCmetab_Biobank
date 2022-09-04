#UK BB data set- data familiarization
rm(list= ls())
setwd("P:/C3_Integrative_Physiology_Group/PeoplesData/Kirstin/Projects/UoS/MC and metabolites_BioBank/Analysis/data/filtered")
library(tidyverse)
library(plyr)
library(ggplot2)
library('ggthemes')
library(ggpubr)
library(reshape2)
library(broom)
library(magrittr)
library(mgcv)
library(lme4)
library(emmeans)

data <- read.csv("filtered.data_20220627.csv")

#select all feeding and metabolite columns
feeding.data <- data %>% filter(age <51) %>% select(c('eid', 'n_30740_0_0', 'TG_0', 'Tchol_0', 'HDL_0','ldl_0', 'HbA1c_0',
                                  'n_74_0_0', 'n_74_1_0', 'n_74_2_0', 'n_74_3_0', 'ts_3166_0_0', 'ts_3166_0_1', 'ts_3166_0_2')) 
#filter by fasting duration
#feeding.data <- feeding.data %>% filter(n_74_0_0 >3)
feeding.data <- feeding.data %>% mutate(fasting.cat = case_when(n_74_0_0 ==0 ~ "hr.0",
                                                  n_74_0_0 ==1 ~ "hr.1",
                                                  n_74_0_0 ==2 ~ "hr.2",
                                                  n_74_0_0 ==3 ~ "hr.3",
                                                  n_74_0_0 ==4 ~ "hr.4",
                                                  n_74_0_0 ==5 ~ "hr.5",
                                                  n_74_0_0 ==6 ~ "hr.6",
                                                  n_74_0_0 ==7 ~ "hr.7",
                                                  n_74_0_0 >8 ~ "hr.>8",
                                                  TRUE~'0'
                                                  ))
feeding.data <-feeding.data %>% filter(fasting.cat !=0)

feeding.data$n_74_0_0 <- paste0("hr.", feeding.data$n_74_0_0)
feeding.data <- feeding.data %>% dplyr::rename('Glucose' = 'n_30740_0_0',
                                        'TGL' = 'TG_0',
                                        'TotalCholesterol' = 'Tchol_0',
                                        'HDL' = 'HDL_0', 
                                        'LDL' = 'ldl_0')

feeding.data$n_74_0_0 <- as.factor(feeding.data$n_74_0_0)
feeding.data$n_74_0_0 <- factor(feeding.data$n_74_0_0, levels= c("hr.0", "hr.1", "hr.2", "hr.3", "hr.4", "hr.5", "hr.6", "hr.7", "hr.8", "hr.9", "hr.10", 
                  "hr.11", "hr.12", "hr.13", "hr.14", "hr.15", "hr.16", "hr.17", "hr.18", "hr.19", 
                  "hr.20", "hr.21", "hr.22", "hr.23", "hr.24"))

feeding.data$fasting.cat <- factor(feeding.data$fasting.cat, levels= c("hr.0", "hr.1", "hr.2", "hr.3", 
                                                                       "hr.4", "hr.5", "hr.6", "hr.7", "hr.>8"))

#arrange data for ggplot
feeding.data <- pivot_longer(feeding.data, cols= Glucose:LDL, names_to='Variable', values_to= 'Conc')

#make summary
feeding.data.summary2 <-  feeding.data %>%  dplyr::group_by(Variable, fasting.cat) %>% dplyr::summarise(mean=mean(Conc, na.rm = TRUE),
                                                                                                    sd1= sd(Conc, na.rm = TRUE))
feeding.data.summary.gl <- feeding.data.summary2 %>% filter(Variable== 'Glucose') %>% 
  dplyr::mutate( diff.from.8hr.IU =  (4.837185-mean),
    diff.from.8hr.perc= (mean/4.837185)*100)
feeding.data.summary.HDL <- feeding.data.summary2 %>% filter(Variable== 'HDL') %>% 
  dplyr::mutate( diff.from.8hr.IU =  (1.503522-mean),
                 diff.from.8hr.perc= (mean/1.503522)*100)
feeding.data.summary.LDL <- feeding.data.summary2 %>% filter(Variable== 'LDL') %>% 
  dplyr::mutate( diff.from.8hr.IU =  (3.349819-mean),
                 diff.from.8hr.perc= (mean/3.349819)*100)
feeding.data.summary.TGL <- feeding.data.summary2 %>% filter(Variable== 'TGL') %>% 
  dplyr::mutate( diff.from.8hr.IU =  (1.103603-mean),
                 diff.from.8hr.perc= (mean/1.103603)*100)
feeding.data.summary.TC <- feeding.data.summary2 %>% filter(Variable== 'TotalCholesterol') %>% 
  dplyr::mutate( diff.from.8hr.IU =  (5.386174-mean),
                 diff.from.8hr.perc= (mean/5.386174)*100)
feeding.data.summary2 <- rbind(feeding.data.summary.gl, feeding.data.summary.HDL, feeding.data.summary.LDL,
                                 feeding.data.summary.TGL, feeding.data.summary.TC)
#write.csv(feeding.data.summary2, 'feeding.data.summary.csv')


#boxplot each variable by fasting duration
boxplot <- ggplot(feeding.data)+
  geom_boxplot(aes(y=Conc, x=fasting.cat, group=fasting.cat))+
  #geom_boxplot(aes(y=Conc, x=n_74_0_0, group=n_74_0_0))+
  xlab('Fasting duration (hr)')+
  ylab('Concentration (IU)')+
  ggtitle('Boplot: concentration ~ fasting duration')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background =element_rect(fill="grey"),
        panel.background = element_blank())+
  facet_wrap(~Variable, scales = "free")
boxplot

#point and line (mean ± sd) by fasting duration 
pointplot <- ggplot(feeding.data.summary2)+
  geom_point(aes(y=mean, x=fasting.cat))+
  geom_errorbar(aes(x=fasting.cat, ymin=mean-sd1, ymax=mean+sd1))+
  xlab('Fasting duration (hr)')+
  scale_y_continuous(limits=c(0,7))+
  ylab('Concentration (IU)')+
  ggtitle('mean ± 1SD: concentration ~ fasting duration')+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background =element_rect(fill="grey"),
        panel.background = element_blank())+
  facet_wrap(~Variable, scales = "free")
pointplot

#categories of fasting duration, to compare all hr to >8 hr 
T.0 =  c(1, 0, 0, 0, 0, 0, 0, 0, 0)
T.1 =  c(0, 1, 0, 0, 0, 0, 0, 0, 0)
T.2 =  c(0, 0, 1, 0, 0, 0, 0, 0, 0)
T.3 =  c(0, 0, 0, 1, 0, 0, 0, 0, 0)
T.4 =  c(0, 0, 0, 0, 1, 0, 0, 0, 0)
T.5 =  c(0, 0, 0, 0, 0, 1, 0, 0, 0)
T.6 =  c(0, 0, 0, 0, 0, 0, 1, 0, 0)
T.7 =  c(0, 0, 0, 0, 0, 0, 0, 1, 0)
T.above8 =  c(0, 0, 0, 0, 0, 0, 0, 0, 1)

#Glucose
glucose.data <- feeding.data %>% ungroup() %>% filter(Variable== 'Glucose') %>% select(eid, fasting.cat, Conc) %>% drop_na() %>% 
  transform(fasting.cat = as.factor(fasting.cat)) 
glucose.lmm <- lm(Conc ~  fasting.cat , data= glucose.data)
anova(glucose.lmm)
glucose.emm <- emmeans(glucose.lmm, specs= ~fasting.cat)
glucose.pairs <- pairs(glucose.emm)
glucose.plot <- plot(glucose.emm)
glucose.pwpp <- pwpp(glucose.emm)
glucose.Tcomp.vsfasting = contrast(glucose.emm, method = list("0 - fasting" = T.0 - T.above8,
                                        "1 - fasting" = T.1 - T.above8,
                                        "2 - fasting" = T.2 - T.above8,
                                        "3 - fasting" = T.3 - T.above8,
                                        "4 - fasting" = T.4 - T.above8,
                                        "5 - fasting" = T.5 - T.above8,
                                        "6 - fasting" = T.6 - T.above8,
                                        "7 - fasting" = T.7 - T.above8),
                         adjust = "mvt")
glucose.Tcomp.vsfasting
glucose.Tcomp.vsprevhr = contrast(glucose.emm, method = list("0 - 1" = T.0 - T.1,
                                                              "1 - 2" = T.1 - T.2,
                                                              "2 - 3" = T.2 - T.3,
                                                              "3 - 4" = T.3 - T.4,
                                                              "4 - 5" = T.4 - T.5,
                                                              "5 - 6" = T.5 - T.6,
                                                              "6 - 7" = T.6 - T.7,
                                                              "7 - 8" = T.7 - T.above8),
                                   adjust = "mvt")
glucose.Tcomp.vsprevhr

glucose.plot.hrcomp <- plot(glucose.Tcomp)
glucose.pwpp.hrcomp <- pwpp(glucose.Tcomp)

#HDL
HDL.data <- feeding.data %>% ungroup() %>% filter(Variable== 'HDL') %>% select(eid, fasting.cat, Conc) %>% drop_na() %>% 
  transform(fasting.cat = as.character(fasting.cat)) 
HDL.lmm <- lm(Conc ~  fasting.cat , data= HDL.data)
anova(HDL.lmm)
HDL.emm <- emmeans(HDL.lmm, "fasting.cat")
HDL.pairs <- pairs(HDL.emm)
HDL.plot <-  plot(HDL.emm)
HDL.pwpp <- pwpp(HDL.emm)
HDL.Tcomp.vsfasting = contrast(HDL.emm, method = list("0 - fasting" = T.0 - T.above8,
                                                           "1 - fasting" = T.1 - T.above8,
                                                           "2 - fasting" = T.2 - T.above8,
                                                           "3 - fasting" = T.3 - T.above8,
                                                           "4 - fasting" = T.4 - T.above8,
                                                           "5 - fasting" = T.5 - T.above8,
                                                           "6 - fasting" = T.6 - T.above8,
                                                           "7 - fasting" = T.7 - T.above8),
                                            adjust = "mvt")
HDL.Tcomp.vsfasting
HDL.Tcomp.vsprevhr = contrast(HDL.emm, method = list("0 - 1" = T.0 - T.1,
                                                             "1 - 2" = T.1 - T.2,
                                                             "2 - 3" = T.2 - T.3,
                                                             "3 - 4" = T.3 - T.4,
                                                             "4 - 5" = T.4 - T.5,
                                                             "5 - 6" = T.5 - T.6,
                                                             "6 - 7" = T.6 - T.7,
                                                             "7 - 8" = T.7 - T.above8),
                                  adjust = "mvt")
HDL.Tcomp.vsprevhr
HDL.plot.hrcomp <-  plot(HDL.Tcomp)
HDL.pwpp.hrcomp <- pwpp(HDL.Tcomp)

#LDL
LDL.data <- feeding.data %>% ungroup() %>% filter(Variable== 'LDL') %>% select(eid, fasting.cat, Conc) %>% drop_na() %>% 
  transform(fasting.cat = as.character(fasting.cat)) 
LDL.lmm <- lm(Conc ~  fasting.cat , data= LDL.data)
anova(LDL.lmm)
LDL.emm <- emmeans(LDL.lmm, "fasting.cat")
LDL.pairs <-pairs(LDL.emm)
LDL.plot <- plot(LDL.emm)
LDL.pwpp <- pwpp(LDL.emm)
LDL.plot
LDL.Tcomp.vsfasting = contrast(LDL.emm, method = list("0 - fasting" = T.0 - T.above8,
                                                     "1 - fasting" = T.1 - T.above8,
                                                     "2 - fasting" = T.2 - T.above8,
                                                     "3 - fasting" = T.3 - T.above8,
                                                     "4 - fasting" = T.4 - T.above8,
                                                     "5 - fasting" = T.5 - T.above8,
                                                     "6 - fasting" = T.6 - T.above8,
                                                     "7 - fasting" = T.7 - T.above8),
                     adjust = "mvt")
LDL.Tcomp.vsfasting
LDL.Tcomp.vsprevhr = contrast(LDL.emm, method = list("0 - 1" = T.0 - T.1,
                                                     "1 - 2" = T.1 - T.2,
                                                     "2 - 3" = T.2 - T.3,
                                                     "3 - 4" = T.3 - T.4,
                                                     "4 - 5" = T.4 - T.5,
                                                     "5 - 6" = T.5 - T.6,
                                                     "6 - 7" = T.6 - T.7,
                                                     "7 - 8" = T.7 - T.above8),
                              adjust = "mvt")
LDL.Tcomp.vsprevhr
LDL.plot.hrcomp <- plot(LDL.Tcomp)
LDL.pwpp.hrcomp <- pwpp(LDL.Tcomp)

#TGL
TGL.data <- feeding.data %>% ungroup() %>% filter(Variable== 'TGL') %>% select(eid, fasting.cat, Conc) %>% drop_na() %>% 
  transform(fasting.cat = as.factor(fasting.cat)) 
TGL.lmm <- lm(Conc ~  fasting.cat , data= TGL.data)
anova(TGL.lmm)
TGL.emm <- emmeans(TGL.lmm, "fasting.cat")
TGL.pairs <- pairs(TGL.emm)
TGL.plot <- plot(TGL.emm)
TGL.pwpp <- pwpp(TGL.emm)
TGL.Tcomp.vsfasting = contrast(TGL.emm, method = list("0 - fasting" = T.0 - T.above8,
                                            "1 - fasting" = T.1 - T.above8,
                                            "2 - fasting" = T.2 - T.above8,
                                            "3 - fasting" = T.3 - T.above8,
                                            "4 - fasting" = T.4 - T.above8,
                                            "5 - fasting" = T.5 - T.above8,
                                            "6 - fasting" = T.6 - T.above8,
                                            "7 - fasting" = T.7 - T.above8),
                     adjust = "mvt")
TGL.Tcomp.vsfasting
TGL.Tcomp.vsprevhr = contrast(TGL.emm, method = list("0 - 1" = T.0 - T.1,
                                                     "1 - 2" = T.1 - T.2,
                                                     "2 - 3" = T.2 - T.3,
                                                     "3 - 4" = T.3 - T.4,
                                                     "4 - 5" = T.4 - T.5,
                                                     "5 - 6" = T.5 - T.6,
                                                     "6 - 7" = T.6 - T.7,
                                                     "7 - 8" = T.7 - T.above8),
                              adjust = "mvt")
TGL.Tcomp.vsprevhr
TGL.plot.hrcomp <- plot(TGL.Tcomp)
TGL.pwpp.hrcomp <- pwpp(TGL.Tcomp)

#TC
TC.data <- feeding.data %>% ungroup() %>% filter(Variable== 'TotalCholesterol') %>% 
  select(eid, fasting.cat, Conc) %>% drop_na() %>% 
  transform(fasting.cat = as.character(fasting.cat))
TC.lmm <- lm(Conc ~  fasting.cat , data= TC.data)
anova(TC.lmm)
TC.emm <- emmeans(TC.lmm, "fasting.cat")
TC.pairs <- pairs(TC.emm)
TC.plot <- plot(TC.emm)
TC.pwpp <- pwpp(TC.emm)
TC.Tcomp.vsfasting = contrast(TGL.emm, method = list("0 - fasting" = T.0 - T.above8,
                                            "1 - fasting" = T.1 - T.above8,
                                            "2 - fasting" = T.2 - T.above8,
                                            "3 - fasting" = T.3 - T.above8,
                                            "4 - fasting" = T.4 - T.above8,
                                            "5 - fasting" = T.5 - T.above8,
                                            "6 - fasting" = T.6 - T.above8,
                                            "7 - fasting" = T.7 - T.above8),
                     adjust = "mvt")
TC.Tcomp.vsfasting
TC.Tcomp.vsprevhr = contrast(TC.emm, method = list("0 - 1" = T.0 - T.1,
                                                     "1 - 2" = T.1 - T.2,
                                                     "2 - 3" = T.2 - T.3,
                                                     "3 - 4" = T.3 - T.4,
                                                     "4 - 5" = T.4 - T.5,
                                                     "5 - 6" = T.5 - T.6,
                                                     "6 - 7" = T.6 - T.7,
                                                     "7 - 8" = T.7 - T.above8),
                              adjust = "mvt")
TC.Tcomp.vsprevhr
TC.plot.hrcomp <- plot(TC.Tcomp)
TC.pwpp.hrcomp <- pwpp(TC.Tcomp)

#HBA1c ~ glucose
HbA1c.data <- feeding.data %>% filter(HbA1c_0<60,
                                      Glucose <10)
HBA1c.lm <- lm(HbA1c_0 ~ Glucose + n_74_0_0, data=HbA1c.data)
summary(HBA1c.lm)



HBA1c.plot <- ggplot(HbA1c.data, aes(x=HbA1c_0, y=Glucose))+
  geom_point()+
  geom_smooth(method='lm')
HBA1c.plot


plot.all <- ggarrange(glucose.plot, HDL.plot, LDL.plot, TGL.plot, TC.plot,
                      labels=c("Glucose", "HDL", "LDL", "Triglyceride", "Total cholesterol"),
                      ncol=2, nrow=3)
plot.all

pwpp.all <- ggarrange(glucose.pwpp, HDL.pwpp, LDL.pwpp, TGL.pwpp, TC.pwpp,
                      labels=c("Glucose", "HDL", "LDL", "Triglyceride", "Total cholesterol"),
                      ncol=2, nrow=3)
pwpp.all

anova <- rbind(Glucose.anova, TGL.anova, HDL.anova, LDL.anova, TC.anova)
anova.rownames <- (c('glucose1', 'glucose2', 'HDL1', 'HDL2', 'LDL1', 'LDL2', 'TGL1', 'TGL2', 'TC1', 'TC2'))
rownames(anova)<- anova.rownames

glucose.Tcomp
TGL.Tcomp
HDL.Tcomp
LDL.Tcomp
TC.Tcomp
