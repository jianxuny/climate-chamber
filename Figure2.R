rm(ls = list())
library(tidyverse)
library(ggplot2)
library(correlation)

# DATA OUTPUT PATH
plot_path <- "D:/project/climate_chamber_project/Manuscript/Figures/Figure9_Discussion"

# Import data
ecgcsv <- read.csv("D:/project/climate_chamber_project/biopac_file/process_output/ecg_fall.csv", 
                   header = T, stringsAsFactor = F) %>% select(-1) %>% rename(exp_temp = temp)
subjinfo <- read.csv("D:/project/climate_chamber_project/questionnaire/SubjInfo.csv")
PsychoDis <- read.csv(file = "D:/project/climate_chamber_project/questionnaire/Perception_fall.csv", header = T, stringsAsFactor = F)
PsychoDis %>%
  rename(ID = id, exp_temp = exp_stag) %>%
  select(ID, exp_temp, matches("qihou_[123]")) %>%
  filter(exp_temp != 3) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("1","2"), labels = c("25°C","40°C"))) -> PsychoDis
  

thermal_perception <- read.csv("D:/project/climate_chamber_project/questionnaire/Thermal_Selfreport.csv", 
                               header = T, stringsAsFactors = F) %>%
  select(-name) %>%
  mutate(cog = (thought+concentration+tiredness+modd+alert)/5) %>%
  select(exp_temp, ID, sub_temp, sub_comfort, sub_accept, air_accept, cog) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C")))

# ------------
# SDNN < 50 ms
# ------------
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(bpm,rmssd,sdnn,pnn50,lf.hf,ID,exp_temp) %>% 
  #filter(sdnn < 60) %>%
  group_by(exp_temp) %>%
  summarise(n = sum(sdnn < 40)/n()) %>%
  ggplot() +
  geom_col(aes(x = exp_temp, y = n, fill = exp_temp)) +
  theme_bw()+
  theme(panel.grid =element_blank(),
        legend.position = 'none') +
  scale_y_continuous(labels = scales::percent) +
  ylab("Proportion of Participants with SDNN < 40 ms") +
  xlab("Temperature Stage") +
  scale_fill_brewer(palette = "OrRd", name = "Temperature") -> p1

ggsave(filename = paste0(plot_path, '/barplot.jpg'),
       plot = p1, height = 3, width = 3, dpi = 600)



# -----------
# Correlation
# -----------
ecgcsv %>%
  as_tibble() %>%
  filter(exp_temp %in% c('25','30','35','40')) %>%
  mutate(exp_temp = factor(exp_temp, levels = c("25","30","35","40"), 
                           labels = c("25°C","30°C","35°C","40°C"))) %>%
  select(bpm,rmssd,sdnn,pnn50,lf.hf,ID,exp_temp,sdnn_outlier,pnn50_outlier) %>% 
  left_join(., thermal_perception, by = c("ID", "exp_temp") ) %>%
  filter(!is.na(sub_comfort)) %>%
  left_join(., y = PsychoDis, by = c("ID","exp_temp")) -> plotdf


################## CORRELATION TABLE ######################
plotdf %>%
  filter(!sdnn_outlier == 'yes') %>%
  select(ID, lf.hf, qihou_2) %>%
  correlation(partial = TRUE, multilevel = TRUE, method = "spearman")

  
# [1] -- SUB_ACCEPT/COMFORT VS SDNN
# ACCEPT
plotdf %>%
  filter(!sdnn_outlier == 'yes') %>%
  filter(sdnn < 150) %>%
  ggplot(data = .) +
  geom_point(aes(x = sub_accept, y = sdnn, color = exp_temp)) +
  geom_smooth(aes(x = sub_accept, y = sdnn), method = 'lm') +
  scale_color_brewer(palette = "OrRd", name = "Temperature") +
  theme_bw()+
  theme(panel.grid =element_blank()) +
  annotate("text", x = -25, y = 120, label = "Multilevel Correlation\nrho: 0.25***")
# COEFFICIENT
plotdf %>%
  filter(!sdnn_outlier == 'yes') %>%
  select(ID, sdnn, sub_accept) %>%
  correlation(partial = TRUE, multilevel = TRUE, method = "spearman")

# COMFORT
plotdf %>%
  filter(!sdnn_outlier == 'yes') %>%
  filter(sdnn < 150) %>%
  ggplot(data = .) +
  geom_point(aes(x = sub_comfort, y = sdnn, color = exp_temp)) +
  geom_smooth(aes(x = sub_comfort, y = sdnn), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "OrRd", name = "Temperature") +
  theme_bw()+
  theme(panel.grid =element_blank()) +
  annotate("text", x = 20, y = 100, label = "Multilevel Correlation: 0.31")+
  ylab("SDNN (ms)") +
  xlab("Temperature Comfort") -> p2


ggsave(filename = paste0(plot_path, '/correlation.jpg'),
       plot = p2, height = 4, width = 5.5, dpi = 600)
# COEFFICIENT
plotdf %>%
  filter(!sdnn_outlier == 'yes') %>%
  filter(sdnn < 150) %>%
  select(ID, sdnn, sub_accept) %>%
  correlation(partial = TRUE, multilevel = TRUE, method = "pearson")

### [1] -- SUB_ACCEPT/COMFORT/PERCEPTION VS HR ###
# -1- SUB_PRECEPTION
plotdf %>%
  #filter(!sdnn_outlier == 'yes') %>%
  filter(bpm < 140) %>%
  #filter(exp_temp != "25°C") %>%
  ggplot(data = .) +
  geom_point(aes(x = sub_temp, y = bpm, color = exp_temp)) +
  geom_smooth(aes(x = sub_temp, y = bpm), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "OrRd", name = "Temperature") +
  theme_bw()+
  theme(panel.grid =element_blank()) +
  annotate("text", x = 5, y = 105, label = "Spearman Correlation\n??: 0.71***", size = 3)+
  ylab("Heart Rate (bpm)") +
  xlab("Temperature Sensation") -> bpm_ts
ggsave(filename = paste0(plot_path, '/temp_sensation.jpg'),
       plot = bpm_ts, height = 2.2, width = 3.55, dpi = 600)
# COEFFICIENT
plotdf %>%
  filter(bpm < 140) %>%
  #filter(!sdnn_outlier == 'yes') %>%
  select(ID, bpm, sub_temp) %>%
  correlation(partial = TRUE, multilevel = TRUE, method = "spearman")

# -2- SUB_COMFORT
plotdf %>%
  #filter(!sdnn_outlier == 'yes') %>%
  filter(bpm < 140) %>%
  #filter(exp_temp != "25°C") %>%
  ggplot(data = .) +
  geom_point(aes(x = sub_comfort, y = bpm, color = exp_temp)) +
  geom_smooth(aes(x = sub_comfort, y = bpm), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "OrRd", name = "Temperature") +
  theme_bw()+
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x = 18, y = 105, label = "Spearman Correlation\n??: -0.64***",size = 3)+
  ylab(NULL) +
  xlab("Temperature Comfort") -> bpm_tc
ggsave(filename = paste0(plot_path, '/temp_comfort.jpg'),
       plot = bpm_tc, height = 2.2, width = 3.2, dpi = 600)

# COEFFICIENT
plotdf %>%
  filter(bpm < 140) %>%
  #filter(!sdnn_outlier == 'yes') %>%
  select(ID, bpm, sub_comfort) %>%
  correlation(partial = TRUE, multilevel = TRUE, method = "spearman")

# -3- SUB_ACCEPT
plotdf %>%
  #filter(!sdnn_outlier == 'yes') %>%
  filter(bpm < 140) %>%
  #filter(exp_temp != "25°C") %>%
  ggplot(data = .) +
  geom_point(aes(x = sub_accept, y = bpm, color = exp_temp)) +
  geom_smooth(aes(x = sub_accept, y = bpm), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "OrRd", name = "Temperature") +
  theme_bw()+
  theme(panel.grid =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x = 18, y = 105, label = "Spearman Correlation\n??: -0.54***",size = 3)+
  ylab(NULL) +
  xlab("Temperature Acceptability") -> bpm_ta
ggsave(filename = paste0(plot_path, '/temp_accept.jpg'),
       plot = bpm_ta, height = 2.2, width = 3.2, dpi = 600)

# COEFFICIENT
plotdf %>%
  filter(bpm < 140) %>%
  #filter(!sdnn_outlier == 'yes') %>%
  select(ID, bpm, sub_accept) %>%
  correlation(partial = TRUE, multilevel = TRUE, method = "spearman")


# -4- AIR QUALITY
plotdf %>%
  #filter(!sdnn_outlier == 'yes') %>%
  filter(bpm < 140) %>%
  #filter(exp_temp != "25°C") %>%
  ggplot(data = .) +
  geom_point(aes(x = air_accept, y = bpm, color = exp_temp)) +
  geom_smooth(aes(x = air_accept, y = bpm), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "OrRd", name = "Temperature") +
  theme_bw()+
  theme(panel.grid =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x = 16, y = 105, label = "Spearman Correlation\n??: -0.43***",size = 3)+
  ylab(NULL) +
  xlab("Air Quality Acceptablility") -> bpm_ta
ggsave(filename = paste0(plot_path, '/air_accept.jpg'),
       plot = bpm_ta, height = 2.2, width = 3.2, dpi = 600)

# COEFFICIENT
plotdf %>%
  filter(bpm < 140) %>%
  #filter(!sdnn_outlier == 'yes') %>%
  select(ID, bpm, air_accept) %>%
  correlation(partial = TRUE, multilevel = TRUE, method = "spearman")






### [2] -- Climate Change Perception VS HR ###
# -1- Climate change belief
plotdf %>%
  filter(bpm < 140) %>%
  filter(exp_temp != "30°C", exp_temp != "35°C") %>%
  ggplot(data = .) +
  geom_point(aes(x = qihou_3, y = bpm, color = exp_temp)) +
  geom_smooth(aes(x = qihou_3, y = bpm), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "Set1", name = "Temperature", direction = -1) +
  theme_bw()+
  theme(panel.grid =element_blank()) +
  annotate("text", x = 46, y = 105, label = "Spearman Correlation\n??: 0.29**", size = 3)+
  ylab("Heart Rate (bpm)") +
  xlab("Climate Change Belief") -> bpm_ccb
ggsave(filename = paste0(plot_path, '/cc_belief.jpg'),
       plot = bpm_ccb, height = 2.2, width = 3.6, dpi = 600)

plotdf %>%
  filter(bpm < 140) %>%
  select(ID, bpm, qihou_3) %>%
  correlation(partial = TRUE, multilevel = T, method = "spearman")

# -2- temporal pd
plotdf %>%
  filter(bpm < 140) %>%
  filter(exp_temp != "30°C", exp_temp != "35°C") %>%
  ggplot(data = .) +
  geom_point(aes(x = qihou_1, y = bpm, color = exp_temp)) +
  geom_smooth(aes(x = qihou_1, y = bpm), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "Set1", name = "Temperature", direction = -1) +
  theme_bw()+
  theme(panel.grid =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x = 65, y = 105, label = "Spearman Correlation\n??: -0.18", size = 3)+
  ylab(NULL) +
  xlab("Temporal Distance") -> bpm_tpd
ggsave(filename = paste0(plot_path, '/cc_tpd.jpg'),
       plot = bpm_tpd, height = 2.2, width = 3.15, dpi = 600)

plotdf %>%
  filter(bpm < 140) %>%
  select(ID, bpm, qihou_1) %>%
  correlation(partial = TRUE, multilevel = T, method = "spearman")

# -3- spatial pd
plotdf %>%
  filter(bpm < 140) %>%
  filter(exp_temp != "30°C", exp_temp != "35°C") %>%
  ggplot(data = .) +
  geom_point(aes(x = qihou_2, y = bpm, color = exp_temp)) +
  geom_smooth(aes(x = qihou_2, y = bpm), method = 'lm', color = "#3C5488B2")+
  scale_color_brewer(palette = "Set1", name = "Temperature", direction = -1) +
  theme_bw()+
  theme(panel.grid =element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text", x = 60, y = 105, label = "Spearman Correlation\n??: -0.06", size = 3)+
  ylab(NULL) +
  xlab("Geographical Distance") -> bpm_spd
ggsave(filename = paste0(plot_path, '/cc_spd.jpg'),
       plot = bpm_spd, height = 2.2, width = 3.15, dpi = 600)
plotdf %>%
  filter(bpm < 140) %>%
  select(ID, bpm, qihou_2) %>%
  correlation(partial = TRUE, multilevel = T, method = "spearman")
