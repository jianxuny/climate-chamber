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
library(ARTool)

setwd("E:/project/2021_Climate_Chamber/questionnaire")
PsychoDis <- read.csv(file = "Perception_fall.csv", header = T, stringsAsFactor = F)

# ============================================================================-
# ===================            HISTOGRAM               =====================
# ============================================================================-

PsychoDis %>%
  as_tibble() %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, 
         matches("qihou_[123]"), matches("yimiao_[234]")) %>%
  mutate(exp_temp = factor(exp_temp)) %>%
  gather(key = question, value = answer, -ID, -exp_temp) %>%
  filter(exp_temp == 1) %>%
  ggplot() +
  geom_histogram(aes(x = answer), binwidth = 2) +
  facet_wrap(~question, nrow = 4)


# ===========================================================================-
# Non-paramatric ANOVA test
# Hopefully not significant for gender and hometown
# ===========================================================================-
subjinfo <- read.csv("D:/project/climate_chamber_project/questionnaire/SubjInfo.csv")
PsychoDis %>%
  as_tibble() %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("gaokao_[234]"),matches("aihang_[235]"),
         matches("qihou_[123456]"), matches("yimiao_[234]")) %>%
  left_join(., subjinfo, by = "ID") %>%
  filter(!is.na(gender)) %>%
  gather(measure, value, -exp_temp, -ID, -gender, -hometown) %>%
  filter(measure == 'qihou_1') %>%
  art(value ~ factor(exp_temp)*factor(gender) + (1|ID), data=.) -> m

anova(m)  
  
# ============================================================================-
# ===================            normality test               =====================
# ============================================================================-

PsychoDis %>%
  as_tibble() %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("gaokao_[234]"),matches("aihang_[235]"),
         matches("qihou_[123456]"), matches("yimiao_[234]")) %>%
  filter(exp_temp == 3) %>%
  shapiro_test(qihou_3)

PsychoDis %>%
  as_tibble() %>%
  filter(exp_stag == 1) %>%
  pull(qihou_2) 
  
grubbs.test(PsychoDis$qihou_1)

# ============================================================================-
# ===================            wilxon test               =====================
# ============================================================================-

subjinfo <- read.csv("E:/project/2021_Climate_Chamber/questionnaire/SubjInfo.csv")
PsychoDis %>%
  as_tibble() %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("gaokao_[234]"),matches("aihang_[235]"),
         matches("qihou_[123]"), matches("yimiao_[234]")) %>%
  left_join(., subjinfo, by = "ID") %>%
  #filter(exp_temp != 3) %>%
  mutate(exp_temp = factor(exp_temp)) %>%
  gather(key = question, value = answer, -ID, -exp_temp, -gender, -hometown) %>%
  arrange(exp_temp,ID) %>%
  group_by(question, exp_temp) %>%
  rstatix::wilcox_test(answer ~ hometown) %>%View()

PsychoDis %>%
  as_tibble() %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("gaokao_[234]"),matches("aihang_[235]"),
         matches("qihou_[123]"), matches("yimiao_[234]")) %>%
  #select(ID, exp_temp, matches("qihou_[123]")) %>%
  left_join(., subjinfo, by = "ID") %>%
  #filter(exp_temp != 3) %>%
  mutate(exp_temp = factor(exp_temp)) %>%
  gather(key = question, value = answer, -ID, -exp_temp, -gender, -hometown) %>%
  group_by(question) %>%
  rstatix::wilcox_test(answer ~ exp_temp, paired = T) %>% write.csv('D:/project/climate_chamber_project/Manuscript/Figures/Figure1_PsychoDis/wlcoxtest.csv')
  

# ============================================================================-
# ===================              BOXPLOT               =====================
# ============================================================================-
PsychoDis %>%
  as_tibble() %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("gaokao_[234]"),matches("aihang_[235]"),
         matches("qihou_[123456]"), matches("yimiao_[234]")) %>%
  left_join(., subjinfo, by = "ID") %>%
  #filter(exp_temp != 3) %>%
  mutate(exp_temp = factor(exp_temp)) %>%
  gather(key = question, value = answer, -ID, -exp_temp, -gender, -hometown) %>%
  drop_na()
  ggplot()+
  geom_boxplot(aes(x = exp_temp, y = answer, fill = gender)) +
  facet_wrap(~question)

  
# ALL QUESTION
PsychoDis %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("gaokao_[234]"), matches("aihang_[235]"),
         matches("qihou_[123456]"), matches("yimiao_[234]")) %>%
  mutate(exp_temp = factor(exp_temp)) %>%
  gather(key = question, value = answer, -ID, -exp_temp) %>%
  ggplot()+
  geom_boxplot(aes(x = exp_temp, y = answer)) +
  facet_wrap(~question)

# Q1
sig_df <- data.frame(question = c("Temporal Distance", "Temporal Distance","Climate Change Belief"),
                     start    = c("Before", "Before", "Before"), 
                     end      = c("During","After","During"),
                     y        = c(105, 115, 110),
                     label    = c("**", "**", "*"))

PsychoDis %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("qihou_[123]")) %>%
  gather(question, answer, -ID, -exp_temp) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("1","2","3"), labels = c("Before","During","After"))) %>%
  mutate(question = factor(question, levels = c("qihou_3","qihou_2","qihou_1"), 
                           labels = c("Climate Change Belief","Geographical Distance","Temporal Distance"))) %>%
  ggplot(aes(x = exp_temp, y = answer)) +
  geom_half_violin(aes(fill = exp_temp), position = position_nudge(x = 0.15), side = "r", alpha = 0.2, color = NA) +
  geom_boxplot(aes(fill = exp_temp),width = 0.3, alpha = 0.9, outlier.alpha = 0.6, position = position_nudge(x = -0.15)) +
  #scale_fill_npg(name = "Experiment Stage")+
  scale_fill_manual(values = c("#2166AC", "#E31A1C", "#6BAED6"))+
  #scale_fill_brewer(palette = "RdYIBu", name = "Temperature")+
  ylab("Self-reported Scores")+
  xlab(NULL)+
  scale_y_continuous(limits = c(0,115), breaks = c(0,20,40,60,80,100))+
  theme_bw()+
  theme(legend.title = element_text(size = 10),
        legend.position = "right",
        axis.title.y = element_text(size = 10))+
  facet_wrap(~question) +
  geom_signif(manual = T, data = sig_df, 
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              position = position_nudge(x = -0.15),
              textsize = 5, vjust = 0.6, extend_line = 0) -> PsychoDisPlot_fall

ggsave(filename = "D:/project/climate_chamber_project/Manuscript/Figures/Figure1_PsychoDis/PsychoDisPlot_fall.jpg",
       plot = PsychoDisPlot_fall, height = 2, width = 7, dpi = 600)


