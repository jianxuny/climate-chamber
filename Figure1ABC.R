rm(list = ls())
library(tidyverse)
library(ggplot2)
library(corrr)
library(ggsci)
library(ggpattern)
library(patchwork)
library(ggpubr)
library(rstatix) # pipe-friendly R functions for easy statistical analyses
library(coin)
library(afex) # used for anova
library(knitr) # make tables
library(emmeans) # make comparison
library(psych) # get descriptive statistics that include SD and SE
library(outliers)
#library(anomalize)

# EXTRACT ID, TEMP CONDITION 
ecgcsvfile <- list.files("D:/project/climate_chamber_project/biopac_file/process_output/ecg_csv_fall")

ecgcsvfile <- gsub(".csv","", ecgcsvfile)
ID <- substr(ecgcsvfile, start = 1, stop = 13)
exp_temp <- gsub("(\\w{13}-)(.*)", "\\2", ecgcsvfile)
filecsv <- tibble(ecgcsvfile, ID, exp_temp)


# DATA EXPLORATION
ecgcsv <- read.csv("D:/project/climate_chamber_project/biopac_file/process_output/ecg_fall.csv", 
                   header = T, stringsAsFactor = F) %>% select(-1) %>% rename(exp_temp = temp)
subjinfo <- read.csv("D:/project/climate_chamber_project/questionnaire/SubjInfo.csv")
# POINTRANGE
ecgcsv %>%
  gather(measure, value, -ID, -exp_temp) %>%
  group_by(exp_temp, measure) %>%
  summarise(mean = mean(value, na.rm = T), sd = sd(value, na.rm = T)) %>%
  filter(measure %in% c("bpm")) %>%
  ggplot(aes(x = exp_temp, y = mean, group = measure)) +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width= 0.1, color = "black")+
  geom_line(color = "indianred2", lwd = 1.2)+
  geom_point(size = 2.5, fill = "cornflowerblue", pch = 21, color = "black", stroke = 1.2, alpha = 1) +
  #geom_pointrange(aes(ymin = mean-sd, ymax = mean+sd), color = "#3C5488B2", size = 1, alpha = 1, fatten = 4, ) +
  theme_bw() +
  theme(panel.grid =element_blank() )+
  scale_y_continuous(limits = c(50,100), breaks = c(50,60,70,80,90,100))+
  ylab("Heart Rate (bpm)")+
  xlab("Temperature (\u00B0C)") -> bpm

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/bpm.jpg",
       plot = bpm, height = 2.8, width = 3, dpi = 600)



### === BOXPLOT+HISTOGRAM: BPM === ###
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(bpm,ID,exp_temp,bpm_outlier) %>%
  filter(bpm_outlier!='yes') %>%
  left_join(., subjinfo, by = "ID") -> bpm_csv

ggplot(data = bpm_csv)  +
  geom_histogram(aes(x = bpm, fill = "#DC0000FF"), binwidth = 3) +
  facet_wrap(~exp_temp) +
  #scale_x_continuous(limits = c(30,165), breaks = c(40,60,80,100,120,140,160))+
  scale_y_continuous(limits = c(0,12.5), breaks = c(0,3,6,9,12)) +
  scale_fill_npg(name = "Outliers")+
  xlab(NULL)+
  theme_bw() +
  theme(panel.grid =element_blank()) +
  guides(fill = F)-> bpm_histo

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/bpm_histo.jpg",
       plot = bpm_histo, height = 2, width = 4, dpi = 600)

# BOXPLOT
ggplot(data = bpm_csv)  +
  geom_boxplot(aes(x = exp_temp, y = bpm),
               width = 0.6, alpha = 0.6, outlier.shape = NA, fill = "#DC0000FF") +
  #scale_fill_npg(name = "Outliers")+
  scale_y_continuous(limits = c(40,120), breaks = c(40,60,80,100,120)) +
  theme_bw() +
  theme(panel.grid =element_blank())+
  xlab(NULL)+
  ylab(NULL)  -> bpm_boxp
ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/bpm_boxp.jpg",
       plot = bpm_boxp, height = 2.5, width = 2.7, dpi = 600)

# Normality test
bpm_csv %>%
  group_by(exp_temp)%>%
  rstatix::shapiro_test(bpm)

# ANOVA test
bpm_csv %>%
  rstatix::anova_test(dv = bpm, wid = ID, within = exp_temp,
                      between = c(hometown,gender)) 

# GEOBACKGROUND
bpm_csv %>%
  group_by(exp_temp)%>%
  t_test(bpm ~ hometown)
  rstatix::wilcox_test(bpm ~ hometown, conf.level = 0.95)

#GENDER
bpm_csv %>%
  group_by(exp_temp)%>%
  rstatix::wilcox_test(bpm ~ gender)
  t_test(bpm ~ gender)
  
### === BOXPLOT+HISTOGRAM: RMSSD === ###
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(rmssd,ID,exp_temp,rmssd_outlier) %>%
  left_join(., subjinfo, by = "ID") -> rmssd_csv

  filter(rmssd_outlier!='yes') -> rmssd_csv
  
ggplot(data = rmssd_csv)  +
  geom_histogram(aes(x = rmssd), fill = "#3C5488FF", binwidth = 5) +
  facet_wrap(~exp_temp) +
  #scale_x_continuous(limits = c(30,165), breaks = c(40,60,80,100,120,140,160))+
  scale_fill_npg(name = "Outliers")+
  xlab(NULL)+
  theme_bw() +
  guides(fill = F) +
  theme(panel.grid =element_blank()) -> rmssd_histo

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/rmssd_histo.jpg",
       plot = rmssd_histo, height = 2, width = 4, dpi = 600)

# BOXPLOT
ggplot(data = rmssd_csv)  +
  geom_boxplot(aes(x = exp_temp, y = rmssd),
               width = 0.6, alpha = 0.6, outlier.shape = NA, fill = "#3C5488FF") +
  #scale_fill_npg(name = "Outliers")+
  scale_y_continuous(limits = c(0,100), breaks = c(0,20,40,60,80,100)) +
  xlab(NULL)+
  ylab(NULL)+
  theme_bw() +
  theme(panel.grid = element_blank()) -> rmssd_boxp

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/rmssd_boxp.jpg",
       plot = rmssd_boxp, height = 2.5, width = 2.7, dpi = 600)

# Normality test
rmssd_csv %>%
  group_by(exp_temp)%>%
  rstatix::shapiro_test(rmssd)

# Friedman test
rmssd_csv %>%
  select(rmssd,ID,exp_temp,gender,hometown) %>%
  spread(exp_temp,rmssd) %>%
  drop_na() %>%
  gather(exp_temp, rmssd, -ID, -gender, -hometown) %>%
  rstatix::friedman_test(rmssd~exp_temp|ID)

rmssd_csv %>%
  filter(gender != 'f')%>%
  ggplot(aes(x = exp_temp, y = rmssd, fill = factor(hometown)))+
  geom_boxplot()

# BY GENDER AND 
rmssd_csv %>%
  group_by(exp_temp)%>%
  rstatix::wilcox_test(rmssd ~ gender, conf.level = 0.95)

rmssd_csv %>%
  group_by(exp_temp) %>%
  rstatix::wilcox_test(rmssd ~ hometown)
  t_test(rmssd ~ hometown)
  rstatix::wilcox_test(rmssd ~ hometown, conf.level = 0.95)

### === BOXPLOT+HISTOGRAM: SDNN === ###
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(sdnn,ID,exp_temp,sdnn_outlier) %>%
  filter(sdnn_outlier != 'yes') %>%
  left_join(., subjinfo, by = "ID") -> sdnn_csv

ggplot(data = sdnn_csv)  +
  geom_histogram(aes(x = sdnn), fill = "#7E6148B2", binwidth = 4) +
  facet_wrap(~exp_temp) +
  #scale_x_continuous(limits = c(30,165), breaks = c(40,60,80,100,120,140,160))+
  scale_fill_npg(name = "Outliers")+
  xlab(NULL)+
  theme_bw() +
  guides(fill = F) +
  theme(panel.grid =element_blank()) -> sdnn_histo

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/sdnn_histo.jpg",
       plot = sdnn_histo, height = 2, width = 4, dpi = 600)

ggplot(data = sdnn_csv)  +
  geom_boxplot(aes(x = exp_temp, y = sdnn),
               width = 0.6, alpha = 0.8, outlier.shape = NA, fill = "#00A087FF") +
  #scale_fill_npg(name = "Outliers")+
  scale_y_continuous(limits = c(0,100), breaks = c(0,50,100)) +
  xlab(NULL)+
  ylab("SDNN (ms)")+
  theme_bw() +
  theme(panel.grid = element_blank()) -> sdnn_boxp

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/sdnn_boxp.jpg",
       plot = sdnn_boxp, height = 2.5, width = 2.7, dpi = 600)

# Friedman test
sdnn_csv %>%
  select(sdnn,ID,exp_temp,gender,hometown) %>%
  spread(exp_temp, sdnn) %>%
  drop_na() %>%
  gather(exp_temp, sdnn, -ID, -gender, -hometown) %>%
  rstatix::friedman_test(sdnn ~ exp_temp|ID)

### === BOXPLOT+HISTOGRAM: LF/HF === ###
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(lf.hf,ID,exp_temp,lf.hf_outlier) %>%
  left_join(., subjinfo, by = "ID") -> lfhf_csv
  filter(lf.hf_outlier!='yes')-> lfhf_csv

ggplot(data = lfhf_csv)  +
  geom_histogram(aes(x = lf.hf), fill = "#E64B35B2", binwidth = 0.5) +
  facet_wrap(~exp_temp) +
  #scale_x_continuous(limits = c(30,165), breaks = c(40,60,80,100,120,140,160))+
  scale_fill_npg(name = "Outliers")+
  xlab(NULL)+
  theme_bw() +
  theme(panel.grid =element_blank()) -> lfhf_histo

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/lfhf_histo.jpg",
       plot = lfhf_histo, height = 2, width = 4, dpi = 600)


ggplot(data = lfhf_csv)  +
  geom_boxplot(aes(x = exp_temp, y = lf.hf),
               width = 0.6, alpha = 0.8, outlier.shape = NA, fill = "#3C5488FF") +
  #scale_fill_npg(name = "Outliers")+
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10)) +
  xlab(NULL)+
  ylab("LF/HF (%)")+
  theme_bw() +
  theme(panel.grid = element_blank()) -> lfhf_boxp

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/lfhf_boxp.jpg",
       plot = lfhf_boxp, height = 2.5, width = 2.7, dpi = 600)


# Friedman test
lfhf_csv %>%
  select(lf.hf,ID,exp_temp,gender,hometown) %>%
  spread(exp_temp, lf.hf) %>%
  drop_na() %>%
  gather(exp_temp, lf.hf, -ID, -gender, -hometown) %>%
  rstatix::friedman_test(lf.hf ~ exp_temp|ID)


# by gender
lfhf_csv %>%
  group_by(exp_temp)%>%
  rstatix::wilcox_test(lf.hf ~ gender, conf.level = 0.95)

# by hometown
lfhf_csv %>%
  group_by(exp_temp)%>%
  rstatix::wilcox_test(lf.hf ~ hometown, conf.level = 0.95)

# Normality test
lfhf_csv %>%
  group_by(exp_temp)%>%
  rstatix::shapiro_test(lf.hf)
### === BOXPLOT+HISTOGRAM: PNN50 === ###
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(pnn50,ID,exp_temp,pnn50_outlier) %>%
  left_join(., subjinfo, by = "ID") %>%
  filter(pnn50_outlier!='yes') -> pnn50_csv

ggplot(data = pnn50_csv)  +
  geom_histogram(aes(x = pnn50), fill = "#00A087FF", binwidth = 0.02) +
  facet_wrap(~exp_temp) +
  #scale_x_continuous(limits = c(30,165), breaks = c(40,60,80,100,120,140,160))+
  scale_fill_npg(name = "Outliers")+
  xlab(NULL)+
  scale_x_continuous(labels = scales::percent)+
  theme_bw() +
  theme(panel.grid =element_blank()) -> pnn50_histo

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/pnn50_histo.jpg",
       plot = pnn50_histo, height = 2, width = 4, dpi = 600)

ggplot(data = pnn50_csv)  +
  geom_boxplot(aes(x = exp_temp, y = pnn50),
               width = 0.6, alpha = 0.6, outlier.shape = NA, fill = "#00A087FF") +
  #scale_fill_npg(name = "Outliers")+
  scale_y_continuous(limits = c(0,0.7), breaks = c(0,0.2,0.4,0.6),labels = scales::percent) +
  xlab(NULL)+
  ylab(NULL)+
  theme_bw() +
  theme(panel.grid = element_blank()) -> pnn50_boxp

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/pnn50_boxp.jpg",
       plot = pnn50_boxp, height = 2.5, width = 2.7, dpi = 600)

pnn50_csv %>%
  group_by(exp_temp)%>%
  rstatix::shapiro_test(pnn50)

# Friedman test
pnn50_csv %>%
  select(pnn50,ID,exp_temp,gender,hometown) %>%
  spread(exp_temp,pnn50) %>%
  drop_na() %>%
  gather(exp_temp, pnn50, -ID, -gender, -hometown) %>%
  rstatix::friedman_test(pnn50~exp_temp|ID)

# by gender
pnn50_csv %>%
  group_by(exp_temp)%>%
  rstatix::wilcox_test(pnn50 ~ gender, conf.level = 0.95)

# by hometown
pnn50_csv %>%
  group_by(exp_temp)%>%
  t_test(pnn50 ~ hometown)
  rstatix::wilcox_test(pnn50 ~ hometown, conf.level = 0.95)


# ANOVA TEST TO SEE GENDER AND HOMETOWN EFFECT
# https://www.datanovia.com/en/lessons/mixed-anova-in-r/
# BY GENDER AND BY HOMETOWN
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(bpm,lf.hf,pnn50,rmssd,sdnn,exp_temp,ID) %>%
  gather(measure, value, -exp_temp,-ID) %>%
  left_join(., subjinfo, by = "ID") %>%
  mutate(measure = factor(measure, levels = c("bpm","rmssd","pnn50","sdnn","lf.hf"), 
                           labels = c("HR (bpm)","RMSSD (ms)","pNN50 (%)","SDNN (ms)","LF/HF (%)"))) %>%
  mutate(hometown = factor(hometown, levels = c("n","s"),
                           labels = c("North", "South"))) %>%
  ggplot(aes(x = exp_temp, y = value, fill = hometown))+
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.1)+
  xlab(NULL)+
  ylab(NULL)+
  guides(fill=guide_legend(title = "Geographic\nBackground"))+
  theme_bw() +
  scale_fill_npg()+
  theme(panel.grid =element_blank())+
  facet_wrap(~measure, scales = 'free') -> SI_Geographic

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/SI_geo.jpg",
       plot = SI_Geographic, height = 5, width = 8, dpi = 600)

ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(bpm,lf.hf,pnn50,rmssd,sdnn,exp_temp,ID) %>%
  gather(measure, value, -exp_temp,-ID) %>%
  left_join(., subjinfo, by = "ID") %>%
  mutate(measure = factor(measure, levels = c("bpm","rmssd","pnn50","sdnn","lf.hf"), 
                          labels = c("HR (bpm)","RMSSD (ms)","pNN50 (%)","SDNN (ms)","LF/HF (%)"))) %>%
  mutate(gender = factor(gender, levels = c("f","m"),
                           labels = c("Female", "Male"))) %>%
  ggplot(aes(x = exp_temp, y = value, fill = gender))+
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.1)+
  xlab(NULL)+
  ylab(NULL)+
  guides(fill=guide_legend(title = "Gender"))+
  theme_bw() +
  scale_fill_npg()+
  theme(panel.grid =element_blank())+
  facet_wrap(~measure, scales = 'free') -> SI_Gender

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure2_ECG/SI_gender.jpg",
       plot = SI_Gender, height = 5, width = 8, dpi = 600)
