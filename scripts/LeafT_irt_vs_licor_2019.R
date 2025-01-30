# New column of datetime for diurnals 2019 
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_2019_leaf_temp_irt_vs_licor  <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-") %>%
  filter(!BH_Block == "B1_R2") %>%
  select(date, time, day, BH_Vine, BH_Block, pixel_number, Tleaf, leaf_temp_C, round, treatment)

diurnals_2019_leaf_temp_irt_vs_licor  <- diurnals_2019_leaf_temp_irt_vs_licor  [-106, ]
diurnals_2019_leaf_temp_irt_vs_licor %>%
  group_by(date) %>%
  tally()

str(diurnals_2019_leaf_temp_irt_vs_licor)


diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C<- format(diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C)

diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C<-as.numeric(diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C)



str(diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C)

write.csv(diurnals_2019_leaf_temp_irt_vs_licor,"data_output/diurnals_2019_leaf_temp_irt_vs_licor.csv")


leafT_irt_vs_licor2019<- ggscatter(diurnals_2019_leaf_temp_irt_vs_licor, x = "Tleaf", y = "leaf_temp_C", 
                                   add = "reg.line", conf.int = TRUE, 
                                   cor.coef = TRUE, cor.method = "pearson",
                                   xlab = "LI-COR Temperature (ºC)", ylab = "IRT Temperature (ºC)") +
  scale_y_continuous(breaks=seq(15,50,5), limits = c (14,50)) +
  scale_x_continuous(breaks=seq(15,50,5), limits = c (14,50)) +
  theme_classic() +
  geom_point(alpha = 0.1, color = "pink" ) +
  stat_regline_equation(label.y = 45)  +
  theme(axis.title.y = element_text(size=18, family = "serif")) +
  theme(axis.title.x = element_text(size=18, family = "serif"))   +
  theme(axis.text.x = element_text(size =16))+
  theme(axis.text.y = element_text(size =16)) 

cor(diurnals_2019_leaf_temp_irt_vs_licor$Tleaf, diurnals_2019_leaf_temp_irt_vs_licor$leaf_temp_C, method = "pearson")


ggsave(leafT_irt_vs_licor2019 , filename = "figures/leafT_irt_vs_licor2019.pdf", device = cairo_pdf, width = 10, height = 8)

####ALL LICOR AND IRT TEMPS FOR 2019-2020-2021

#####2020 data 

diurnals_borden_hills_2020 <-read.csv("data_output/data_physiology_all_complete_BH_2020.csv", header = TRUE)

str(diurnals_borden_hills_2020)

diurnals_2020_A_vs_time <- diurnals_borden_hills_2020 %>%
  mutate(time = hhmmss)%>%
  select(-hhmmss)

diurnals_2020_A_vs_time$time<-hms(diurnals_2020_A_vs_time$time)
diurnals_2020_A_vs_time$date<- ymd(diurnals_2020_A_vs_time$date)

str(diurnals_2020_A_vs_time$date)
str(diurnals_2020_A_vs_time$time)

diurnals_2020_A_vs_time$datetime <- paste(diurnals_2020_A_vs_time$date, " ", diurnals_2020_A_vs_time$time, sep = "")

str(diurnals_2020_A_vs_time$datetime)

glimpse(diurnals_2020_A_vs_time) 

diurnals_2020_A_vs_time$datetime <- ymd_hms(diurnals_2020_A_vs_time$datetime,  tz = "UTC")
str(diurnals_2020_A_vs_time$datetime)

diurnals_2020_A_vs_time$round<-format(diurnals_2020_A_vs_time$round)
diurnals_2020_A_vs_time$round<-as.numeric(as.factor(diurnals_2020_A_vs_time$round))

str(diurnals_2020_A_vs_time$round)
str(diurnals_2020_A_vs_time)

tz(diurnals_2020_A_vs_time$datetime)

tz(diurnals_2020_A_vs_time$time)

str(diurnals_2020_A_vs_time$datetime)

str(diurnals_2020_A_vs_time)


####2021 data 


data_physiology_complete_BH_2021_middays<-read.csv("data_output/data_physiology_complete_BH_2021_middays.csv", header = TRUE)


data_physiology_complete_BH_2021_middays$date<- ymd(data_physiology_complete_BH_2021_middays$date)
str(data_physiology_complete_BH_2021_middays$date)


diurnals_2019_leaf_temp_irt_vs_licor<-diurnals_2019_leaf_temp_irt_vs_licor%>%
  mutate(leaf_temp = leaf_temp_C)%>%
  select(-leaf_temp_C, -BH_Vine, -BH_Block, -pixel_number,-round)
str(diurnals_2019_leaf_temp_irt_vs_licor)

diurnals_2020_leaf_temp_irt_vs_licor  <- diurnals_2020_A_vs_time %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp)) %>%
  filter(!leaf_temp == "-") %>%
  select(date,time, day, Tleaf, leaf_temp,  treatment)

str(diurnals_2020_leaf_temp_irt_vs_licor)

str(data_physiology_complete_BH_2021_middays)
diurnals_2021_leaf_temp_irt_vs_licor  <- data_physiology_complete_BH_2021_middays %>% mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp)) %>%
  filter(!leaf_temp == "-") %>%
  select(date,time, day, Tleaf, leaf_temp, treatment)

str(diurnals_2021_leaf_temp_irt_vs_licor)

BH_leaf_temp_irt_vs_licor_all_years<-rbind(diurnals_2021_leaf_temp_irt_vs_licor,diurnals_2020_leaf_temp_irt_vs_licor, diurnals_2019_leaf_temp_irt_vs_licor)


BH_leaf_temp_irt_vs_licor_all_years_plot<- ggscatter(BH_leaf_temp_irt_vs_licor_all_years, x = "Tleaf", y = "leaf_temp", 
                                                     add = "reg.line", conf.int = TRUE, 
                                                     cor.coef = TRUE, cor.method = "pearson",
                                                     xlab = "LI-COR Temperature (ºC)", ylab = "IRT Temperature (ºC)") +
  scale_y_continuous(breaks=seq(15,50,5), limits = c (14,50)) +
  scale_x_continuous(breaks=seq(15,50,5), limits = c (14,50)) +
  theme_classic() +
  geom_point(alpha = 0.1, color = "pink" ) +
  stat_regline_equation(label.y = 45)  +
  theme(axis.title.y = element_text(size=18, family = "serif")) +
  theme(axis.title.x = element_text(size=18, family = "serif"))   +
  theme(axis.text.x = element_text(size =16))+
  theme(axis.text.y = element_text(size =16)) 

cor(BH_leaf_temp_irt_vs_licor_all_years$Tleaf, BH_leaf_temp_irt_vs_licor_all_years$leaf_temp, method = "pearson")

plot(diurnals_2021_leaf_temp_irt_vs_licor_plot)

ggsave(BH_leaf_temp_irt_vs_licor_all_years_plot , filename = "figures/BH_leaf_temp_irt_vs_licor_all_years_plot.jpg",  width = 10, height = 8,dpi=300)


diurnals_2021_leaf_temp_irt_vs_licor_plot<- ggscatter(diurnals_2021_leaf_temp_irt_vs_licor, x = "Tleaf", y = "leaf_temp", 
                                                      add = "reg.line", conf.int = TRUE, 
                                                      cor.coef = TRUE, cor.method = "pearson",
                                                      xlab = "LI-COR Temperature (ºC)", ylab = "IRT Temperature (ºC)") +
  theme_classic() +
  geom_point(alpha = 0.1, color = "pink" ) +
  stat_regline_equation(label.y = 45)  +
  theme(axis.title.y = element_text(size=18, family = "serif")) +
  theme(axis.title.x = element_text(size=18, family = "serif"))   +
  theme(axis.text.x = element_text(size =16))+
  theme(axis.text.y = element_text(size =16)) 

cor(diurnals_2021_leaf_temp_irt_vs_licor$Tleaf, diurnals_2021_leaf_temp_irt_vs_licor$leaf_temp, method = "pearson")


ggsave(diurnals_2021_leaf_temp_irt_vs_licor_plot , filename = "figures/BH_2021_leaf_temp_irt_vs_licor_plot.jpg",  width = 10, height = 8,dpi=300)


