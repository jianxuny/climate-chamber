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
library(gghalves)

setwd("D:/project/climate_chamber_project/questionnaire")
recall <- read.csv(file = "Recall_fall.csv", header = T, stringsAsFactor = F)
PsychoDis <- read.csv(file = "Perception_fall.csv", header = T, stringsAsFactor = F)
thermal_perception <- read.csv("D:/project/climate_chamber_project/questionnaire/Thermal_Selfreport.csv", 
                               header = T, stringsAsFactors = F)

# normality test




# regression
recall %>%
  select(ID, matches("memo_.*"), matches("qihou_[123]")) %>%
  rename(exp_temp = memo_days, sub_temp = memo_temp, sub_comfort = memo_comfort, sub_accept = memo_accept) %>%
  as_tibble() %>% mutate(exp_temp = factor(exp_temp)) -> recall_new

thermal_perception %>%
  select(-name) %>%
  mutate(cog = (thought+concentration+tiredness+modd+alert)/5) %>%
  select(exp_temp, ID, sub_temp, sub_comfort, sub_accept, air_accept, cog) %>%
  filter(exp_temp == 40) %>%
  mutate(exp_temp = 0) %>%
  mutate(exp_temp = factor(exp_temp)) %>%
  select(ID, exp_temp, sub_temp, sub_comfort, sub_accept) -> thermal_exp


PsychoDis %>%
  as_tibble() %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("qihou_[123]")) %>%
  filter(exp_temp == 2) %>%
  mutate(exp_temp = 0) %>%
  mutate(exp_temp = factor(exp_temp)) -> psydis_exp

psydis_exp %>%
  left_join(x = thermal_exp, y = ., by = c("ID", "exp_temp")) %>%
  as_tibble() %>%
  rbind(., recall_new) -> recall_new


recall_new %>%
  arrange(exp_temp,ID) %>%
  filter(ID != "20190509AM03F") %>%
  gather(measure, value, -ID, -exp_temp) %>%
  group_by(measure) %>%
  #t_test(qihou_1~exp_temp) %>%
  rstatix::wilcox_test(value ~ exp_temp, paired = T,  conf.level = 0.95) %>%
  View()
#   
recall_new %>%
  gather(measure, value, -ID, -exp_temp) %>%
  mutate(measure = factor(measure, 
                           levels = c("sub_temp","sub_comfort","sub_accept","qihou_3","qihou_2","qihou_1"), 
                           labels = c("Temperature Sensation","Temperature Comfort","Temperature Acceptability",
                                      "Climate Change Belief","Geographical Distance","Temporal Distance"))) %>%
  ggplot(aes(x = exp_temp, y = value)) +
  geom_half_violin(aes(fill = exp_temp), position = position_nudge(x = 0.15), side = "r", alpha = 0.4, color = NA) +
  geom_boxplot(aes(fill = exp_temp),width = 0.3, alpha = 0.8, outlier.alpha = 0.6, position = position_nudge(x = -0.15)) +
  #scale_fill_uchicago(name = "Follow-up Survey Time")+
  scale_fill_manual(values = c("#E31A1C", "#084594", "#2171B5","#4292C6","#6BAED6","#9ECAE1"))+
  ylab("Self-reported Scores")+
  xlab(NULL)+
  geom_smooth(aes(x = exp_temp, y = value), color = "blue", method = 'linear')+
  theme_bw()+
  theme(legend.title = element_text(size = 10),
        legend.position = "bottom",
        axis.title.y = element_text(size = 10))+
    facet_wrap(~measure, scales = "free") -> recallplot

recallplot

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure8_Recall/Recall.jpg",
       plot = recallplot, height = 5, width = 7.5, dpi = 600)



