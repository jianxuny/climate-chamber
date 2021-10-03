rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(ggplot2)
library(openxlsx)
library(rstatix)
# CLEAN DATA
# REFERENCE:A new weighting system for mean surface temperature of the human body
# we have missing values...
# sktmean <- function(Tbelly, Tthigh, Tarm, Tleg, Thead, Tfoot, Thand){
#   Tms = 0.35*Tbelly + 0.19*Tthigh + 0.14*Tarm +
#     0.13*Tleg + 0.07*Thead + 0.07*Tfoot + 0.05*Thand
#   Tms
# }
sktemp <- read.csv("E:/project/2021_Climate_Chamber/output_file/sktemp_l.csv", header = T, stringsAsFactors = F)
subjinfo <- read.csv("E:/project/2021_Climate_Chamber/questionnaire/SubjInfo.csv")

# --------------------------
#         FIGURE 2D
# --------------------------

# CALCULATE GRAND AVERAGE LINE
sktemp %>%
  spread(pos,value) %>%
  rowwise() %>%
  mutate(avgtemp = mean(c(belly, calf, foot, forearm, forehead, hands, thighs), na.rm = T)) %>%
  ungroup() %>%
  select(time, timesq, ID, avgtemp) %>%
  filter(timesq >= 10 & timesq <= 100) %>%
  mutate(timesq = (timesq - 10)*2) %>% # first 120 points (4h)
  filter(ID != "20190509AM02F") %>%
  left_join(., subjinfo, by = "ID") %>%
  filter(!is.na(gender)) %>%
  group_by(timesq) %>%
  summarise(avg = mean(avgtemp)) -> sktemp_avg

# MAKE FIGURE_2
sktemp %>%
  spread(pos,value) %>%
  rowwise() %>%
  mutate(avgtemp = mean(c(belly, calf, foot, forearm, forehead, hands, thighs), na.rm = T)) %>%
  ungroup() %>%
  select(time, timesq, ID, avgtemp) %>%
  filter(timesq >= 10 & timesq <= 100) %>%
  mutate(timesq = (timesq - 10)*2) %>% # first 120 points (4h)
  filter(ID != "20190509AM02F") %>%
  ggplot(data = .) +
  #geom_vline(xintercept = 10)+
  annotate("rect",xmin=20,xmax=40,ymin=-Inf,ymax=Inf,alpha=0.2,fill="red")+
  annotate("rect",xmin=40,xmax=60,ymin=-Inf,ymax=Inf,alpha=0.3,fill="red")+
  annotate("rect",xmin=60,xmax=80,ymin=-Inf,ymax=Inf,alpha=0.4,fill="red")+
  annotate("rect",xmin=80,xmax=100,ymin=-Inf,ymax=Inf,alpha=0.5,fill="red")+
  annotate("rect",xmin=100,xmax=120,ymin=-Inf,ymax=Inf,alpha=0.6,fill="red")+
  annotate("rect",xmin=120,xmax=140,ymin=-Inf,ymax=Inf,alpha=0.7,fill="red")+
  annotate("rect",xmin=140,xmax=160,ymin=-Inf,ymax=Inf,alpha=0.2,fill="red")+
  geom_point(aes(x = timesq, y = avgtemp), color = "cornflowerblue", alpha = 0.1) +
  #geom_smooth(aes(timesq, avgtemp), span = 0.3, se = T, color = "blue") + 
  geom_point(aes(x = timesq, y = avg), color = "blue", data = sktemp_avg) +
  geom_line(aes(x = timesq, y = avg), color = "blue", data = sktemp_avg)+
  annotate(geom = "text", x = 22, y = 39.5, label = "Room Temperature:", hjust = 0, vjust = 0, size = 2.5)+
  annotate(geom = "text", x = 22, y = 39, label = "25\u00B0C", hjust = 0, vjust = 0, size = 2)+
  annotate(geom = "text", x = 42, y = 39, label = "30\u00B0C", hjust = 0, vjust = 0, size = 2)+
  annotate(geom = "text", x = 62, y = 39, label = "30-35", hjust = 0, vjust = 0, size = 2)+
  annotate(geom = "text", x = 82, y = 39, label = "35\u00B0C", hjust = 0, vjust = 0, size = 2)+
  annotate(geom = "text", x = 102, y = 39, label = "35-40", hjust = 0, vjust = 0, size = 2)+
  annotate(geom = "text", x = 122, y = 39, label = "40\u00B0C", hjust = 0, vjust = 0, size = 2)+
  annotate(geom = "text", x = 142, y = 39, label = "25\u00B0C", hjust = 0, vjust = 0, size = 2)+
  theme_bw() +
  theme(panel.grid =element_blank())+
  #ylab("Mean Skin Temperature (\u00B0C)")+
  #xlab("Time (min)")+
  xlab(NULL)+
  ylab(NULL)+
  scale_x_continuous(limits = c(0,180), breaks = c(0,20,40,60,80,100,120,140,160,180))+
  scale_y_continuous(limits = c(30,40), breaks = c(30,32,34,36,38,40))-> sktemp_plot

sktemp_plot

ggsave(filename = "E:/project/2021_Climate_Chamber/Manuscript/Figures/Figure4_SkinTemp/sktemp.jpg",
       plot = sktemp_plot, height = 2.5, width = 2.7, dpi = 600)


# --------------------------
#         FIGURE S6
# --------------------------

# BY GENDER
sktemp %>%
  spread(pos,value) %>%
  rowwise() %>%
  mutate(avgtemp = mean(c(belly, calf, foot, forearm, forehead, hands, thighs), na.rm = T)) %>%
  ungroup() %>%
  select(time, timesq, ID, avgtemp) %>%
  filter(timesq >= 10 & timesq <= 100) %>%
  mutate(timesq = (timesq - 10)*2) %>% # first 120 points (4h)
  filter(ID != "20190509AM02F") %>%
  left_join(., subjinfo, by = "ID") %>%
  filter(!is.na(gender)) %>% 
  group_by(timesq, gender) %>%
  summarise(avg = mean(avgtemp)) -> by_gender

ggplot() +
  geom_line(data = by_gender, aes(x = timesq, y = avg, color = gender), size = 0.5) +
  scale_color_manual(values = c('#d53e4f','#3288bd'),labels = c('Females','Males')) +
  theme(panel.grid = element_blank())+
  #ylab("Mean Skin Temperature (\u00B0C)")+
  #xlab("Time (min)")+
  xlab(NULL)+
  ylab(NULL)+
  labs(color = 'Gender') +
  scale_x_continuous(limits = c(0,180), breaks = c(0,20,40,60,80,100,120,140,160,180))+
  scale_y_continuous(limits = c(30,38), breaks = c(30,32,34,36,38)) +
  theme_bw() -> figure_gender
figure_gender

ggsave(filename = "E:/project/2021_Climate_Chamber/Manuscript/Figures/Figure4_SkinTemp/by_gender.jpg",
       plot = figure_gender, height = 3, width = 4, dpi = 600)


# BY HOMETOWN
sktemp %>%
  spread(pos,value) %>%
  rowwise() %>%
  mutate(avgtemp = mean(c(belly, calf, foot, forearm, forehead, hands, thighs), na.rm = T)) %>%
  ungroup() %>%
  select(time, timesq, ID, avgtemp) %>%
  filter(timesq >= 10 & timesq <= 100) %>%
  mutate(timesq = (timesq - 10)*2) %>% # first 120 points (4h)
  filter(ID != "20190509AM02F") %>%
  left_join(., subjinfo, by = "ID") %>%
  filter(!is.na(gender)) %>% 
  group_by(timesq, hometown) %>%
  summarise(avg = mean(avgtemp)) -> by_home


ggplot() +
  geom_line(data = by_home, aes(x = timesq, y = avg, color = hometown), size = 0.5) +
  scale_color_manual(values = c('#8c510a','#01665e'),labels = c('Northern','Southern')) +
  theme(panel.grid = element_blank())+
  #ylab("Mean Skin Temperature (\u00B0C)")+
  #xlab("Time (min)")+
  xlab(NULL)+
  ylab(NULL)+
  labs(color = 'Geographical\nBackground') +
  scale_x_continuous(limits = c(0,180), breaks = c(0,20,40,60,80,100,120,140,160,180))+
  scale_y_continuous(limits = c(30,38), breaks = c(30,32,34,36,38)) +
  theme_bw() -> figure_home
figure_home

ggsave(filename = "E:/project/2021_Climate_Chamber/Manuscript/Figures/Figure4_SkinTemp/by_home.jpg",
       plot = figure_home, height = 3, width = 4.1, dpi = 600)


pairwise_t_test(data = by_gender, avg~gender)

by_gender %>%
  ungroup() %>%
  rstatix::pairwise_t_test(data = ., avg ~ gender)

by_home %>%
  ungroup() %>%
  rstatix::pairwise_t_test(data = ., avg ~ hometown)


# --------------------------
#         Bar plots
# --------------------------

  # FIGURE 2
sktemp %>%
  spread(pos,value) %>%
  rowwise() %>%
  mutate(avgtemp = mean(c(belly, calf, foot, forearm, forehead, hands, thighs), na.rm = T)) %>%
  ungroup() %>%
  select(time, timesq, ID, avgtemp) %>%
  filter(timesq >= 10 & timesq <= 100) %>%
  mutate(timesq = (timesq - 10)*2) %>% # first 120 points (4h)
  filter(ID != "20190509AM02F") %>%
  mutate(exp_temp = case_when(timesq > 30 & timesq <= 40 ~ "25C",
                              timesq > 50 & timesq <= 60 ~ "30C",
                              timesq > 90 & timesq <= 100 ~ "35C",
                              timesq > 130 & timesq <= 140 ~ "45C",
                              )) %>%
  drop_na() %>%
  group_by(exp_temp) %>%
  summarise(temp_avg = mean(avgtemp),
            sd = sd(avgtemp)) %>%
  #left_join(., subjinfo, by = "ID") %>%
  drop_na() -> skt_bytemp

skt_bytemp %>%
  group_by(exp_temp) %>%
  spread(exp_temp,temp_avg) %>%
  drop_na() %>%
  gather(exp_temp, temp_avg, -ID, -gender, -hometown) %>%
  group_by(exp_temp) %>%
  wilcox_test(temp_avg ~ gender)
  rstatix::anova_test(dv = temp_avg, between = gender, within = exp_temp)

ggplot(data = skt_bytemp)  +
  geom_histogram(aes( x = temp_avg)) +
  facet_wrap(~exp_temp)
  geom_boxplot(aes(x = exp_temp, y = temp_avg, fill = hometown),
               width = 0.6, alpha = 0.8, outlier.shape = NA) +
  #scale_fill_npg(name = "Outliers")+
  #scale_y_continuous(limits = c(40,120), breaks = c(40,60,80,100,120)) +
  xlab(NULL)+
  ylab("Heart Rate (bpm)")+
  theme_bw() +
  theme(panel.grid =element_blank())
