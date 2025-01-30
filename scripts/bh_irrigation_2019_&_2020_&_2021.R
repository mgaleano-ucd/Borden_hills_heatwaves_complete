library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(xtable)
library(agricolae)
library(gtools)
library(cowplot)
library(Cairo)
library(ggpmisc)

####plotting in ggplot irrigation and T max graph season 2019####

tmax <- read.csv("data/tmax_complete_season_2019.csv")
irr <- read.csv("data/BH_irrigation_2019_hours_complete.csv")

str(irr)
str(tmax)

tmax$Date <- as.character(tmax$Date)
tmax$Date <- as.Date(tmax$Date,format = "%m/%d/%Y")

irr$Date <- as.character(irr$Date)
irr$Date <-as.Date(irr$Date,format = "%m/%d/%Y")

tmax<-tmax %>%
  filter(!is.na(Tmax))

irr<-irr%>%
  filter(!is.na(hours))


str(tmax$Date)
str(irr$Date)


tmax<-tmax %>%
  filter(!is.na(Tmax))

irr<-irr%>%
  mutate(gal_acre_day_irrigation =(hours*573.5885))

common_col_names <- intersect(names(irr), names(tmax))

irr_tmax_combined_2019 <- merge(irr, tmax, by=common_col_names, all =TRUE) 

irr_tmax_combined_2019$treatment<-format(irr_tmax_combined_2019$treatment)
as.character(irr_tmax_combined_2019$treatment)


str(irr_tmax_combined_2019)

irr_tmax_combined_2019_date_selection<- irr_tmax_combined_2019%>%
  filter(Date > "2019-05-15")%>%
  filter(Date<"2019-09-16")

str(irr_tmax_combined_2019_date_selection)

write.csv(irr_tmax_combined_2019_date_selection,"data_output/irr_tmax_combined_2019_date_selection.csv")



daily_irr_tmax_2019_HW_plot_date_selection<- ggplot(irr_tmax_combined_2019_date_selection,aes(Date,gal_acre_day_irrigation, group = treatment, color = treatment)) +
  geom_line(alpha =0.7, size =1, linetype = "solid") +
 geom_line(aes(y = Tmax * 320, color = "Tmax", group = "Tmax"), linetype = "dashed") +
# geom_line(aes(y = Tmax*320),linetype ="dashed", color = "black", group ="Tmax")+
  scale_y_continuous(sec.axis = sec_axis(~./320, name = "Maximum temperature (ºC)"),breaks=seq(0,15000,4000), limits = c (0,15000))+
  theme_classic()+
#  ggtitle("2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold", family = "serif")) + 
  scale_color_manual(name = "Treatment",
    values = c("#FDE725FF","#21908CFF","#440154FF" ,"black"),
    labels = c("Baseline (60% ET)", "90-120% ET", "120-180% ET", "Tmax")) +
  ylab(label = "Daily applied irrigation (gallons/acre)") +
  xlab("Date") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (1, "cm")) +
  theme(legend.key.width = unit(1,"cm"))+
  theme(legend.text = element_text(size = 20),
  legend.title = element_text(size = 22))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 14000, label = "HW1", size = 6) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 14000, label ="HW2", size = 6) +
  theme(legend.position = "none") +
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank())


ggsave(daily_irr_tmax_2019_HW_plot_date_selection, filename = "figures/daily_irr_tmax_2019_HW_plot_date_selection.pdf", device = cairo_pdf, width = 13, height = 8)


phenological_stages_2019 <- data.frame(
  stage = c("Flowering", "Fruit-set", "Veraison", "Harvest"),
  start = as.Date(c("2019-05-20", "2019-06-06", "2019-08-02", "2019-09-12")),
  end = as.Date(c("2019-06-06", "2019-08-02", "2019-09-12", "2019-09-19"))
)

phenological_bar_plot_2019 <- ggplot(phenological_stages_2019, aes(xmin = start, xmax = end, ymin = 0, ymax = 1, fill = stage)) +
  geom_rect() +
  geom_text(aes(x = (start), y = 0.5, label = stage), color = "black", size = 5) +
  scale_fill_manual(values = c("#FFB6C1", "#FF69B4", "#C71585", "#9400D3"), name = "Phenological Stages") +
  theme_void() +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16")), ylim = c(0, 1))+
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(size=138, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(axis.text.x = element_text(size =14))+
  theme(axis.text.y = element_text(size =23))+
  scale_y_continuous(sec.axis = sec_axis(~./1),breaks=seq(0,1), limits = c (0,1))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank()) 


panel_plot_daily_irr_tmax_2019_with_pheno_bar<-plot_grid(daily_irr_tmax_2019_HW_plot_date_selection, phenological_bar_plot_2019, ncol = 1,
rel_heights = c(4,0.4)
)

ggsave(panel_plot_daily_irr_tmax_2019_with_pheno_bar, filename = "figures/panel_plot_daily_irr_tmax_2019_with_pheno_bar.pdf", device = cairo_pdf, width = 12, height =14)


###Cumulative irrigation 2019 

str(irr_tmax_combined_2019_date_selection)

irr_tmax_combined_2019_date_selection<-irr_tmax_combined_2019%>%
  select(Date, hours,treatment,gal_acre_day_irrigation)

str(irr_tmax_combined_2019_date_selection)

irr_tmax_combined_2019_date_selection$treatment<-format(irr_tmax_combined_2019_date_selection$treatment)
as.character(irr_tmax_combined_2019_date_selection$treatment)

irr_tmax_combined_2019_date_selection<-irr_tmax_combined_2019_date_selection%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))


cumulative_irr_tmax_combined_2019 <-irr_tmax_combined_2019_date_selection %>%
  arrange(Date) %>%
  group_by(treatment) %>%
  mutate(Cumulative_Irrigation_mm = cumsum(mm_per_day_acre))%>%
  mutate(Cumulative_Irrigation_mm_gallo = cumsum(mm_per_day_acre_gallo))%>%
  filter(!is.na(mm_per_day_acre))%>%
  filter(!is.na(Date))%>%
  filter(!is.na(treatment))


cumulative_irrigation_bh_2019<-ggplot(cumulative_irr_tmax_combined_2019, aes(x = Date, y = Cumulative_Irrigation_mm, color = treatment)) +
  geom_line(size= 1, alpha =0.8) +
  geom_point( size =1.1, alpha =0.6) + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET", "Tmax")) +
  ylab(label = "Cumulative irrigation (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold", family = "serif")) +
  xlab("Date") +
  ggtitle("2019")+
  theme(plot.title = element_text(size = 18))+
  theme(axis.title.y = element_text(size=18, family = "serif")) +
  theme(axis.title.x = element_text(size=18, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,450,50), limits = c (0,450)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2)+
  theme(axis.text.x = element_text(size =15))+
  theme(axis.text.y = element_text(size =15))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 450, label = "HW1", size = 4) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 450, label ="HW2", size = 4) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) +
  theme(legend.position = "none") 


ggsave(cumulative_irrigation_bh_2019, filename = "figures/cumulative_irrigation_bh_2019.pdf", device = cairo_pdf, width = 9, height = 9)


##### Irrigation BH 2020 #####


irr <- read.csv("data/bh_irrigation_2020_complete_season.csv")
tair_bh_2020<-read.csv("data/Tmax_BH_2020.csv")

str(irr)
str(tair_bh_2020)

tmax <- tair_bh_2020 %>%
  group_by(DOY) %>%
  summarise(tair_max = max(Tair_C_hmp))

tmax$Date <- as.Date(paste("2020", tmax$DOY), format = "%Y %j")



irr$Date <- as.character(irr$Date)
irr$Date <-as.Date(irr$Date,format = "%m/%d/%Y")


irr<-irr%>%
  filter(!is.na(hours))
tmax<-tmax %>%
  filter(!is.na(tair_max))


str(tmax$Date)
str(irr$Date)


irr<-irr%>%
  mutate(gal_acre_day_irrigation =(hours*573.5885))

str(irr)

irr<-irr%>%
  select(Date, hours,treatment,gal_acre_day_irrigation)

str(irr)

irr$treatment<-format(irr$treatment)
as.character(irr$treatment)


common_col_names <- intersect(names(irr), names(tmax))

irr_tmax_combined_2020 <- merge(irr, tmax, by=common_col_names, all =TRUE)

irr_tmax_combined_2020$treatment<-format(irr_tmax_combined_2020$treatment)
as.character(irr_tmax_combined_2020$treatment)


str(irr_tmax_combined_2020)

tmax_combined_2020_ave_HW1<-irr_tmax_combined_2020%>%
  select(tair_max,Date,treatment)%>%
  filter(Date == "2020-05-26" | Date == "2020-05-27" | Date == "2020-05-28") %>%
  group_by(treatment)%>%
  summarise(avg_tmax = mean(tair_max))

tmax_combined_2020_ave_HW2<-irr_tmax_combined_2020%>%
  select(tair_max,Date,treatment)%>%
  filter(Date == "2020-07-10" | Date == "2020-07-11"| Date == "2020-07-12")%>%
  group_by(treatment)%>%
  summarise(avg_tmax = mean(tair_max))

tmax_combined_2020_ave_HW3<-irr_tmax_combined_2020%>%
  select(tair_max,Date,treatment)%>%
  filter( Date == "2020-08-14"| Date == "2020-08-15"|Date == "2020-08-16" | Date == "2020-08-17"| Date == "2020-08-18")%>%
  group_by(treatment)%>%
  summarise(avg_tmax = mean(tair_max))

tmax_combined_2020_ave_HW4<-irr_tmax_combined_2020%>%
  select(tair_max,Date,treatment)%>%
  filter( Date == "2020-09-05"| Date == "2020-09-06"|Date == "2020-09-07")%>%
  group_by(treatment)%>%
  summarise(avg_tmax = mean(tair_max))


irr_combined_2020_ave_HW1<-irr_tmax_combined_2020%>%
  select(gal_acre_day_irrigation,tair_max,Date,treatment)%>%
  filter(Date == "2020-05-25" |Date == "2020-05-26" | Date == "2020-05-27" | Date == "2020-05-28") %>%
  group_by(treatment)%>%
  summarise(avg_irr = mean(gal_acre_day_irrigation),avg_tmax = mean(tair_max))

irr_combined_2020_ave_HW2<-irr_tmax_combined_2020%>%
  select(gal_acre_day_irrigation,tair_max,Date,treatment)%>%
  filter(Date == "2020-07-11" | Date == "2020-07-12"| Date == "2020-07-13")%>%
  group_by(treatment)%>%
  summarise(avg_irr = mean(gal_acre_day_irrigation),avg_tmax = mean(tair_max))

irr_combined_2020_ave_HW3<-irr_tmax_combined_2020%>%
  select(gal_acre_day_irrigation,tair_max,Date,treatment)%>%
  filter( Date == "2020-08-13"|Date == "2020-08-14"| Date == "2020-08-15"|Date == "2020-08-16" | Date == "2020-08-17"| Date == "2020-08-18"| Date == "2020-08-19"| Date == "2020-08-20")%>%
  group_by(treatment)%>%
  summarise(avg_irr = mean(gal_acre_day_irrigation),avg_tmax = mean(tair_max))

irr_combined_2020_ave_HW4<-irr_tmax_combined_2020%>%
  select(gal_acre_day_irrigation,tair_max,Date,treatment)%>%
  filter(Date == "2020-09-04"| Date == "2020-09-05"| Date == "2020-09-06"|Date == "2020-09-07")%>%
  group_by(treatment)%>%
  summarise(avg_irr = mean(gal_acre_day_irrigation),avg_tmax = mean(tair_max))

irr_tmax_combined_2020_date_selection<-irr_tmax_combined_2020%>%
  filter(Date > "2020-05-15")%>%
  filter(Date<"2020-09-16")

phenological_stages_2020 <- data.frame(
  stage = c("Flowering", "Fruit-set", "Veraison", "Harvest"),
  start = as.Date(c("2020-05-20", "2020-06-05", "2020-07-26", "2020-09-15")),
  end = as.Date(c("2020-06-05", "2020-07-26", "2020-09-15", "2020-09-19"))
)

str(irr_tmax_combined_2020_date_selection$Date)
str(phenological_stages_2020$start)
str(phenological_stages_2020$end)

write.csv(irr_tmax_combined_2020_date_selection,"data_output/irr_tmax_combined_2020_date_selection.csv")


phenological_stages_2020$midpoint <- as.numeric(phenological_stages_2020$start)


daily_irr_tmax_2020_HW_plot_date_selection<-ggplot(irr_tmax_combined_2020_date_selection,aes(Date,gal_acre_day_irrigation, group = treatment, color = treatment)) +
  geom_line(alpha =0.7, size =1, linetype = "solid") +
  scale_colour_viridis_d(direction = -1 , begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET", "Tmax"))+
  geom_line(aes(y = tair_max * 320, linetype = "Tmax"), linetype = "dashed", color ="black") +
  scale_y_continuous(sec.axis = sec_axis(~./320, name = "Maximum temperature (ºC)"),breaks=seq(0,15000,4000), limits = c (0,15000))+
  theme_classic()+
#  ggtitle("2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold", family = "serif")) +
  ylab(label = "Daily applied irrigation (gallons/acre)") +
  xlab("Date") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 14000, label = "HW1", size = 6) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 14000, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 14000, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 14000, label ="HW4", size = 6) +
  theme(legend.position = "none") +
  theme(axis.title.y.left = element_text(color = "white"), axis.text.y.left = element_text(color = "white"), axis.ticks.y.left = element_blank()) +
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank())
  





phenological_bar_plot_2020 <- ggplot(phenological_stages_2020, aes(xmin = start, xmax = end, ymin = 0, ymax = 1, fill = stage)) +
  geom_rect() +
  geom_text(aes(x = (start), y = 0.5, label = stage), color = "black", size = 5) +
  scale_fill_manual(values = c("#FFB6C1", "#FF69B4", "#C71585", "#9400D3"), name = "Phenological Stages") +
  theme_void() +
  theme(legend.position = "none") +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16")), ylim = c(0, 1))+
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(size=138, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(axis.text.x = element_text(size =14))+
  theme(axis.text.y = element_text(size =23))+
  scale_y_continuous(sec.axis = sec_axis(~./1),breaks=seq(0,1), limits = c (0,1))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank()) 


ggsave(daily_irr_tmax_2020_HW_plot_date_selection, filename = "figures/daily_irr_tmax_2020_HW_plot_date_selection.pdf", device = cairo_pdf, width = 13, height = 8)

panel_plot_daily_irr_tmax_2020_with_pheno_bar<-plot_grid(daily_irr_tmax_2020_HW_plot_date_selection, phenological_bar_plot_2020, ncol = 1,
rel_heights = c(4,0.4)
)

ggsave(panel_plot_daily_irr_tmax_2020_with_pheno_bar, filename = "figures/panel_plot_daily_irr_tmax_2020_with_pheno_bar.pdf", device = cairo_pdf, width = 12, height =14)




panel_plot_daily_irr_tmax_2019_2020 <- plot_grid(
  daily_irr_tmax_2019_HW_plot_date_selection + ggtitle("2019"), 
  daily_irr_tmax_2020_HW_plot_date_selection + ggtitle("2020"), 
  labels = c("A", "B"), 
  ncol = 1, 
  vjust = 1.5, 
  hjust = -9.6, 
  label_size = 18
)


ggsave(panel_plot_daily_irr_tmax_2019_2020, filename = "figures/panel_plot_daily_irr_tmax_2019_2020.pdf", device = cairo_pdf, width = 11, height =12)

#panel_plot_daily_irr_tmax_2019_2020_with_pheno_bar<-plot_grid(panel_plot_daily_irr_tmax_2019_2020, phenological_bar_plot, ncol = 1,
#rel_heights = c(4,0.4)
#)

#ggsave(panel_plot_daily_irr_tmax_2019_2020_with_pheno_bar, filename = "figures/panel_plot_daily_irr_tmax_2019_2020_with_pheno_bar.pdf", device = cairo_pdf, width = 12, height =14)


#####add axis, legend, title etc

#theme(axis.text.y = element_blank(),axis.title.y = element_blank(),
#      axis.ticks.y = element_blank()) +
#  theme(legend.position = "none") +
#  theme(axis.text.x = element_blank(),axis.title.x = element_blank(),
#        axis.ticks.x = element_blank())


#Cumulative irrigation 2020

bH_irrigation_summary_2020 <- read.csv("data/BH_irrigation_summary_2020.csv", header  = FALSE) 
class(bH_irrigation_summary_2020)

transposed_data_irr_bh_2020<- t(bH_irrigation_summary_2020 )

transposed_data_irr_bh_2020<- as.data.frame(transposed_data_irr_bh_2020 )

transposed_data_irr_bh_2020<-transposed_data_irr_bh_2020[-1, ]

str(transposed_data_irr_bh_2020)


header<- c("Date","Pixel_1","Pixel_2","Pixel_3", "Pixel_4","Pixel_5", "Pixel_6","Pixel_7","Pixel_8","Pixel_9","Pixel_10","Pixel_11","Pixel_12","Pixel_13","Pixel_14","Pixel_15","Pixel_16","Pixel_17","Pixel_18","Pixel_19","Pixel_20","Pixel_21","Pixel_22","Pixel_23","Pixel_24","Pixel_25","Pixel_26","Pixel_27","Pixel_28","Pixel_29","Pixel_30","Pixel_31","Pixel_32","Pixel_33","Pixel_34","Pixel_35","Pixel_36","Pixel_37","Pixel_38","Pixel_39","Pixel_40","Pixel_41","Pixel_42","Pixel_43","Pixel_44","Pixel_45","Pixel_46","Pixel_47","Pixel_48","Pixel_49","Pixel_50","Pixel_51","Pixel_52","Pixel_53","Pixel_54","Pixel_55","Pixel_56","Pixel_57","Pixel_58","Pixel_59","Pixel_60","Pixel_61","Pixel_62","Pixel_63","Pixel_64","Pixel_65","Pixel_66","Pixel_67","Pixel_68","Pixel_69","Pixel_70","Pixel_71","Pixel_72","Pixel_73","Pixel_74","Pixel_75","Pixel_76","Pixel_77","Pixel_78","Pixel_79","Pixel_80","Pixel_81","Pixel_82","Pixel_83","Pixel_84","Pixel_85","Pixel_86","Pixel_88","Pixel_89","Pixel_90","Pixel_91","Pixel_92","Pixel_93","Pixel_95","Pixel_96","Pixel_97","Pixel_98","Pixel_99","Pixel_87A","Pixel_87B")
str(header)
colnames(transposed_data_irr_bh_2020 ) <- header
colnames(transposed_data_irr_bh_2020 )

transposed_data_irr_bh_2020$Date<-mdy(transposed_data_irr_bh_2020 $Date)
str(transposed_data_irr_bh_2020$Date)

data_irr_bh_2020<-transposed_data_irr_bh_2020%>%
  select(Date,Pixel_4,Pixel_6,Pixel_13,Pixel_15,Pixel_14,Pixel_16,Pixel_23,Pixel_25,Pixel_24,Pixel_26,Pixel_33,Pixel_35,Pixel_34,Pixel_36,Pixel_43,Pixel_45,Pixel_44,Pixel_46,Pixel_53,Pixel_55,Pixel_54,Pixel_56,Pixel_63,Pixel_65,Pixel_18,Pixel_20,Pixel_27,Pixel_29,Pixel_47,Pixel_49,Pixel_48,Pixel_50,Pixel_58,Pixel_60,Pixel_67,Pixel_69)


columns_to_select_treatment1 <- data_irr_bh_2020[, c("Pixel_4","Pixel_6","Pixel_13","Pixel_15","Pixel_44","Pixel_46","Pixel_53","Pixel_55","Pixel_58","Pixel_60","Pixel_67","Pixel_69")]

time_columns<- c("Pixel_4","Pixel_6","Pixel_13","Pixel_15","Pixel_44","Pixel_46","Pixel_53","Pixel_55","Pixel_58","Pixel_60","Pixel_67","Pixel_69")

for (col in time_columns) {
  time_split <- strsplit(columns_to_select_treatment1[[col]], ":")
  
  # Extract hours and minutes
  hours <- sapply(time_split, function(x) as.numeric(x[1]))
  minutes <- sapply(time_split, function(x) as.numeric(x[2]))
  
  # Calculate total time in minutes
  total_time_float <- hours + minutes / 60
  
  # Update the column in the data frame with the total time in hours
  columns_to_select_treatment1[[col]] <- total_time_float
}

str(columns_to_select_treatment1)


row_averages_treatment_1 <- rowMeans(columns_to_select_treatment1)

# Add the row averages to the data frame
columns_to_select_treatment1$Row_Average <- row_averages_treatment_1
columns_to_select_treatment1$Date<-data_irr_bh_2020$Date
columns_to_select_treatment1$treatment<- c(1) 

str(columns_to_select_treatment1)

columns_to_select_treatment2 <- data_irr_bh_2020[, c("Pixel_14","Pixel_16","Pixel_23","Pixel_25","Pixel_24","Pixel_26","Pixel_33","Pixel_35","Pixel_47","Pixel_48","Pixel_49","Pixel_50")]

time_columns<- c("Pixel_14","Pixel_16","Pixel_23","Pixel_25","Pixel_24","Pixel_26","Pixel_33","Pixel_35","Pixel_47","Pixel_48","Pixel_49","Pixel_50")

for (col in time_columns) {
  time_split <- strsplit(columns_to_select_treatment2[[col]], ":")
  
  # Extract hours and minutes
  hours <- sapply(time_split, function(x) as.numeric(x[1]))
  minutes <- sapply(time_split, function(x) as.numeric(x[2]))
  
  # Calculate total time in minutes
  total_time_float <- hours + minutes / 60
  
  # Update the column in the data frame with the total time in hours
  columns_to_select_treatment2[[col]] <- total_time_float
}

str(columns_to_select_treatment2)


row_averages_treatment_2 <- rowMeans(columns_to_select_treatment2)

# Add the row averages to the data frame
columns_to_select_treatment2$Row_Average <- row_averages_treatment_2
columns_to_select_treatment2$Date<-data_irr_bh_2020$Date
columns_to_select_treatment2$treatment<- c(2) 

str(columns_to_select_treatment2)

columns_to_select_treatment3 <- data_irr_bh_2020[, c("Pixel_34","Pixel_36","Pixel_43","Pixel_45","Pixel_54","Pixel_56","Pixel_63","Pixel_65","Pixel_18","Pixel_20","Pixel_27","Pixel_29")]

time_columns<- c("Pixel_34","Pixel_36","Pixel_43","Pixel_45","Pixel_54","Pixel_56","Pixel_63","Pixel_65","Pixel_18","Pixel_20","Pixel_27","Pixel_29")

for (col in time_columns) {
  time_split <- strsplit(columns_to_select_treatment3[[col]], ":")
  
  # Extract hours and minutes
  hours <- sapply(time_split, function(x) as.numeric(x[1]))
  minutes <- sapply(time_split, function(x) as.numeric(x[2]))
  
  # Calculate total time in minutes
  total_time_float <- hours + minutes / 60
  
  # Update the column in the data frame with the total time in hours
  columns_to_select_treatment3[[col]] <- total_time_float
}

str(columns_to_select_treatment3)


row_averages_treatment_3 <- rowMeans(columns_to_select_treatment3)

# Add the row averages to the data frame
columns_to_select_treatment3$Row_Average <- row_averages_treatment_3
columns_to_select_treatment3$Date<-data_irr_bh_2020$Date
columns_to_select_treatment3$treatment<- c(3) 

str(columns_to_select_treatment3)

all_dates1<-data.frame(Date = seq(min(columns_to_select_treatment1$Date), max(columns_to_select_treatment1$Date), by = "day"))
all_dates2<-data.frame(Date = seq(min(columns_to_select_treatment2$Date), max(columns_to_select_treatment2$Date), by = "day"))
all_dates3<-data.frame(Date = seq(min(columns_to_select_treatment3$Date), max(columns_to_select_treatment3$Date), by = "day"))

complete_df1 <- merge(all_dates1, columns_to_select_treatment1, by = "Date", all.x = TRUE)
complete_df2 <- merge(all_dates2, columns_to_select_treatment2, by = "Date", all.x = TRUE)
complete_df3 <- merge(all_dates3, columns_to_select_treatment3, by = "Date", all.x = TRUE)

complete_df1$treatment<- c(1) 
complete_df2$treatment<- c(2) 
complete_df3$treatment<- c(3) 


complete_df1[is.na(complete_df1)] <- 0
complete_df2[is.na(complete_df2)] <- 0
complete_df3[is.na(complete_df3)] <- 0


columns_to_select_treatment1<-complete_df1
columns_to_select_treatment2<-complete_df2
columns_to_select_treatment3<-complete_df3

columns_to_select_treatment1<-columns_to_select_treatment1%>%
  select(Date, treatment,Row_Average)
columns_to_select_treatment2<-columns_to_select_treatment2%>%
  select(Date, treatment,Row_Average)
columns_to_select_treatment3<-columns_to_select_treatment3%>%
  select(Date, treatment,Row_Average)

bh_irrigation_2020_complete_season<-rbind(columns_to_select_treatment1,columns_to_select_treatment2,columns_to_select_treatment3)



bh_irrigation_2020_complete_season<-bh_irrigation_2020_complete_season%>%
  mutate(hours = Row_Average)

str(bh_irrigation_2020_complete_season)


bh_irrigation_2020_complete_season<-bh_irrigation_2020_complete_season%>%
  filter(!is.na(hours))

str(bh_irrigation_2020_complete_season$Date)

bh_irrigation_2020_complete_season<-bh_irrigation_2020_complete_season%>%
  mutate(gal_acre_day_irrigation =(hours*573.5885))

str(bh_irrigation_2020_complete_season)

bh_irrigation_2020_complete_season<-bh_irrigation_2020_complete_season%>%
  select(Date, hours,treatment,gal_acre_day_irrigation)

str(bh_irrigation_2020_complete_season)

bh_irrigation_2020_complete_season$treatment<-format(bh_irrigation_2020_complete_season$treatment)
as.character(bh_irrigation_2020_complete_season$treatment)

irr_tmax_combined_2020<-bh_irrigation_2020_complete_season%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))



cumulative_irr_tmax_combined_2020 <-irr_tmax_combined_2020 %>%
  arrange(Date) %>%
  group_by(treatment) %>%
  mutate(Cumulative_Irrigation_mm = cumsum(mm_per_day_acre))%>%
  mutate(Cumulative_Irrigation_mm_gallo = cumsum(mm_per_day_acre_gallo))





cumulative_irrigation_bh_2020<-ggplot(cumulative_irr_tmax_combined_2020, aes(x = Date, y = Cumulative_Irrigation_mm, color = treatment)) +
  geom_line(size= 1, alpha =0.8) +
  geom_point( size =1.1, alpha =0.6) + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET", "Tmax")) +
  ylab(label = "Cumulative irrigation (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold", family = "serif")) +
  xlab("Date") +
  ggtitle("2020")+
  theme(plot.title = element_text(size = 18))+
  theme(axis.title.y = element_text(size=18, family = "serif")) +
  theme(axis.title.x = element_text(size=18, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  scale_y_continuous(breaks=seq(0,450,50), limits = c (0,450)) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2)+
  theme(axis.text.x = element_text(size =15))+
  theme(axis.text.y = element_text(size =15))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 450, label = "HW1", size = 4) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 450, label ="HW2", size = 4) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 450, label ="HW3", size = 4)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 450, label ="HW4", size = 4) +
  theme(legend.position = "none") 



ggsave(cumulative_irrigation_bh_2020, filename = "figures/cumulative_irrigation_bh_2020.pdf", device = cairo_pdf, width = 9, height = 9)


panel_plot_cumulative_irr_2019_2020 <- plot_grid(
  cumulative_irrigation_bh_2019, 
  cumulative_irrigation_bh_2020, 
  labels = c("A", "B"), 
  ncol = 1, 
  vjust = 1.5, 
  hjust = -9.6, 
  label_size = 18
)

ggsave(panel_plot_cumulative_irr_2019_2020, filename = "figures/panel_plot_cumulative_irr_2019_2020.pdf", device = cairo_pdf, width = 12, height =14)



##### Irrigation BH 2021 #####

bH_irrigation_summary_2021 <- read.csv("data/BH_irrigation_summary_2021.csv", header  = FALSE) 
class(bH_irrigation_summary_2021)

transposed_data_irr_bh_2021 <- t(bH_irrigation_summary_2021)

transposed_data_irr_bh_2021 <- as.data.frame(transposed_data_irr_bh_2021)

transposed_data_irr_bh_2021 <- transposed_data_irr_bh_2021[-1, ]

str(transposed_data_irr_bh_2021)



header<- c("Date","Pixel_1","Pixel_2","Pixel_3", "Pixel_4","Pixel_5", "Pixel_6","Pixel_7","Pixel_8","Pixel_9","Pixel_10","Pixel_11","Pixel_12","Pixel_13","Pixel_14","Pixel_15","Pixel_16","Pixel_17","Pixel_18","Pixel_19","Pixel_20","Pixel_21","Pixel_22","Pixel_23","Pixel_24","Pixel_25","Pixel_26","Pixel_27","Pixel_28","Pixel_29","Pixel_30","Pixel_31","Pixel_32","Pixel_33","Pixel_34","Pixel_35","Pixel_36","Pixel_37","Pixel_38","Pixel_39","Pixel_40","Pixel_41","Pixel_42","Pixel_43","Pixel_44","Pixel_45","Pixel_46","Pixel_47","Pixel_48","Pixel_49","Pixel_50","Pixel_51","Pixel_52","Pixel_53","Pixel_54","Pixel_55","Pixel_56","Pixel_57","Pixel_58","Pixel_59","Pixel_60","Pixel_61","Pixel_62","Pixel_63","Pixel_64","Pixel_65","Pixel_66","Pixel_67","Pixel_68","Pixel_69","Pixel_70","Pixel_71","Pixel_72","Pixel_73","Pixel_74","Pixel_75","Pixel_76","Pixel_77","Pixel_78","Pixel_79","Pixel_80","Pixel_81","Pixel_82","Pixel_83","Pixel_84","Pixel_85","Pixel_86","Pixel_88","Pixel_89","Pixel_90","Pixel_91","Pixel_92","Pixel_93","Pixel_95","Pixel_96","Pixel_97","Pixel_98","Pixel_99","Pixel_87A","Pixel_87B")
str(header)
colnames(transposed_data_irr_bh_2021) <- header
colnames(transposed_data_irr_bh_2021)

transposed_data_irr_bh_2021$Date<-mdy(transposed_data_irr_bh_2021$Date)
str(transposed_data_irr_bh_2021$Date)

data_irr_bh_2021<-transposed_data_irr_bh_2021%>%
  select(Date,Pixel_4,Pixel_6,Pixel_13,Pixel_15,Pixel_14,Pixel_16,Pixel_23,Pixel_25,Pixel_24,Pixel_26,Pixel_33,Pixel_35,Pixel_34,Pixel_36,Pixel_43,Pixel_45,Pixel_44,Pixel_46,Pixel_53,Pixel_55,Pixel_54,Pixel_56,Pixel_63,Pixel_65,Pixel_18,Pixel_20,Pixel_27,Pixel_29,Pixel_47,Pixel_49,Pixel_48,Pixel_50,Pixel_58,Pixel_60,Pixel_67,Pixel_69)


columns_to_select_treatment1 <- data_irr_bh_2021[, c("Pixel_4","Pixel_6","Pixel_13","Pixel_15","Pixel_44","Pixel_46","Pixel_53","Pixel_55","Pixel_58","Pixel_60","Pixel_67","Pixel_69")]

time_columns<- c("Pixel_4","Pixel_6","Pixel_13","Pixel_15","Pixel_44","Pixel_46","Pixel_53","Pixel_55","Pixel_58","Pixel_60","Pixel_67","Pixel_69")

for (col in time_columns) {
  time_split <- strsplit(columns_to_select_treatment1[[col]], ":")
  
  # Extract hours and minutes
  hours <- sapply(time_split, function(x) as.numeric(x[1]))
  minutes <- sapply(time_split, function(x) as.numeric(x[2]))
  
  # Calculate total time in minutes
  total_time_float <- hours + minutes / 60
  
  # Update the column in the data frame with the total time in hours
  columns_to_select_treatment1[[col]] <- total_time_float
}

str(columns_to_select_treatment1)


row_averages_treatment_1 <- rowMeans(columns_to_select_treatment1)

# Add the row averages to the data frame
columns_to_select_treatment1$Row_Average <- row_averages_treatment_1
columns_to_select_treatment1$Date<-data_irr_bh_2021$Date
columns_to_select_treatment1$treatment<- c(1) 


columns_to_select_treatment2 <- data_irr_bh_2021[, c("Pixel_14","Pixel_16","Pixel_23","Pixel_25","Pixel_24","Pixel_26","Pixel_33","Pixel_35","Pixel_47","Pixel_48","Pixel_49","Pixel_50")]

time_columns<- c("Pixel_14","Pixel_16","Pixel_23","Pixel_25","Pixel_24","Pixel_26","Pixel_33","Pixel_35","Pixel_47","Pixel_48","Pixel_49","Pixel_50")

for (col in time_columns) {
  time_split <- strsplit(columns_to_select_treatment2[[col]], ":")
  
  # Extract hours and minutes
  hours <- sapply(time_split, function(x) as.numeric(x[1]))
  minutes <- sapply(time_split, function(x) as.numeric(x[2]))
  
  # Calculate total time in minutes
  total_time_float <- hours + minutes / 60
  
  # Update the column in the data frame with the total time in hours
  columns_to_select_treatment2[[col]] <- total_time_float
}

str(columns_to_select_treatment2)


row_averages_treatment_2 <- rowMeans(columns_to_select_treatment2)

# Add the row averages to the data frame
columns_to_select_treatment2$Row_Average <- row_averages_treatment_2
columns_to_select_treatment2$Date<-data_irr_bh_2021$Date
columns_to_select_treatment2$treatment<- c(2) 



columns_to_select_treatment3 <- data_irr_bh_2021[, c("Pixel_34","Pixel_36","Pixel_43","Pixel_45","Pixel_54","Pixel_56","Pixel_63","Pixel_65","Pixel_18","Pixel_20","Pixel_27","Pixel_29")]

time_columns<- c("Pixel_34","Pixel_36","Pixel_43","Pixel_45","Pixel_54","Pixel_56","Pixel_63","Pixel_65","Pixel_18","Pixel_20","Pixel_27","Pixel_29")

for (col in time_columns) {
  time_split <- strsplit(columns_to_select_treatment3[[col]], ":")
  
  # Extract hours and minutes
  hours <- sapply(time_split, function(x) as.numeric(x[1]))
  minutes <- sapply(time_split, function(x) as.numeric(x[2]))
  
  # Calculate total time in minutes
  total_time_float <- hours + minutes / 60
  
  # Update the column in the data frame with the total time in hours
  columns_to_select_treatment3[[col]] <- total_time_float
}

str(columns_to_select_treatment3)


row_averages_treatment_3 <- rowMeans(columns_to_select_treatment3)

# Add the row averages to the data frame
columns_to_select_treatment3$Row_Average <- row_averages_treatment_3
columns_to_select_treatment3$Date<-data_irr_bh_2021$Date
columns_to_select_treatment3$treatment<- c(3) 

all_dates1 <- data.frame(Date = seq(min(columns_to_select_treatment1$Date), max(columns_to_select_treatment1$Date), by = "day"))
all_dates2 <- data.frame(Date = seq(min(columns_to_select_treatment2$Date), max(columns_to_select_treatment2$Date), by = "day"))
all_dates3 <- data.frame(Date = seq(min(columns_to_select_treatment3$Date), max(columns_to_select_treatment3$Date), by = "day"))

complete_df1 <- merge(all_dates1, columns_to_select_treatment1, by = "Date", all.x = TRUE)
complete_df2 <- merge(all_dates2, columns_to_select_treatment2, by = "Date", all.x = TRUE)
complete_df3 <- merge(all_dates3, columns_to_select_treatment3, by = "Date", all.x = TRUE)

complete_df1$treatment<- c(1) 
complete_df2$treatment<- c(2) 
complete_df3$treatment<- c(3) 


complete_df1[is.na(complete_df1)] <- 0
complete_df2[is.na(complete_df2)] <- 0
complete_df3[is.na(complete_df3)] <- 0


columns_to_select_treatment1<-complete_df1
columns_to_select_treatment2<-complete_df2
columns_to_select_treatment3<-complete_df3

columns_to_select_treatment1<-columns_to_select_treatment1%>%
  select(Date, treatment,Row_Average)
columns_to_select_treatment2<-columns_to_select_treatment2%>%
  select(Date, treatment,Row_Average)
columns_to_select_treatment3<-columns_to_select_treatment3%>%
  select(Date, treatment,Row_Average)

bh_irrigation_2021_complete_season<-rbind(columns_to_select_treatment1,columns_to_select_treatment2,columns_to_select_treatment3)

irr<-bh_irrigation_2021_complete_season

irr<-irr%>%
  mutate(hours = Row_Average)

tair_bh_2021<-read.csv("data/Tair_BH_2021.csv")

str(irr)
str(tair_bh_2021)

tmax <- tair_bh_2021 %>%
  group_by(DOY) %>%
  summarise(tair_max = max(Tair_C_hmp))

tmax$Date <- as.Date(paste("2021", tmax$DOY), format = "%Y %j")

str(tmax)

irr<-irr%>%
  filter(!is.na(hours))

tmax<-tmax %>%
  filter(!is.na(tair_max))


str(tmax$Date)
str(irr$Date)

irr<-irr%>%
  mutate(gal_acre_day_irrigation =(hours*573.5885))

str(irr)

irr<-irr%>%
  select(Date, hours,treatment,gal_acre_day_irrigation)

str(irr)

irr$treatment<-format(irr$treatment)
as.character(irr$treatment)

common_col_names <- intersect(names(irr), names(tmax))

irr_tmax_combined_2021 <- merge(irr, tmax, by=common_col_names, all =TRUE)

irr_tmax_combined_2021$treatment<-format(irr_tmax_combined_2021$treatment)

as.character(irr_tmax_combined_2021$treatment)

write.csv(irr_tmax_combined_2021,"data_output/bh_irr_tmax_combined_2021.csv")

irr_tmax_combined_2021_date_selection<-irr_tmax_combined_2021%>%
  filter(Date > "2021-05-15")%>%
  filter(Date<"2021-09-16")

daily_irr_tmax_2021_HW_plot_date_selection<-ggplot(irr_tmax_combined_2021_date_selection,aes(Date,gal_acre_day_irrigation, group = treatment, color = treatment)) +
  geom_line(alpha =0.7, size =1, linetype = "solid") +
  geom_line(aes(y = tair_max*320),linetype ="dashed", color = "black")+
  scale_y_continuous(sec.axis = sec_axis(~./320, name = "Maximum temperature (ºC)"),breaks=seq(0,15000,4000), limits = c (0,15000))+
  theme_classic()+
#  ggtitle("2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold", family = "serif")) +
  scale_color_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "90% ET", "120% ET", "Tmax")) +
  ylab(label = "Daily applied irrigation (gallons/acre)") +
  xlab("Date") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 15000, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 15000, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 15000, label = "HW1", size = 6) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 0, ymax = 15000,
           alpha = .2)  +
  theme(legend.position = "none") +
  theme(axis.title.y.left = element_text(color = "white"), axis.text.y.left = element_text(color = "white"), axis.ticks.y.left = element_blank()) 

ggsave(daily_irr_tmax_2021_HW_plot_date_selection, filename = "figures/daily_irr_tmax_2021_HW_plot_date_selection.pdf", device = cairo_pdf, width = 13, height = 8)

irr_tmax_combined_2021_date_selection<-irr_tmax_combined_2021_date_selection%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))


cumulative_irr_tmax_combined_2021 <-irr_tmax_combined_2021_date_selection %>%
  arrange(Date) %>%
  group_by(treatment) %>%
  mutate(Cumulative_Irrigation_mm = cumsum(mm_per_day_acre))%>%
  mutate(cumulative_gallons_acre = cumsum(mm_per_day_acre_gallo))


cumulative_irrigation_bh_2021_date_selection<-ggplot(cumulative_irr_tmax_combined_2021, aes(x = Date, y = Cumulative_Irrigation_mm, color = treatment)) +
  geom_line(size= 1, alpha =0.8) +
  geom_point( size =1.1, alpha =0.6) + 
  theme_classic()+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET", "Tmax")) +
  ylab(label = "Cumulative irrigation (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 21, face = "bold", family = "serif")) +
  xlab("Date") +
  #  ggtitle("BH season 2021")+
  #  theme(plot.title = element_text(size = 18))+
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 450, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 450, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 450, label = "HW1", size = 6) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  scale_y_continuous(breaks=seq(0,450,50), limits = c (0,470)) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 0, ymax = 450,
           alpha = .2)+
  theme(legend.position = "none")  

ggsave(cumulative_irrigation_bh_2021_date_selection, filename = "figures/cumulative_irrigation_bh_2021_date_selection.pdf", device = cairo_pdf, width = 13, height = 8)




panel_plot_daily_irr_tmax_2021_with_pheno_bar<-plot_grid( daily_irr_tmax_2021_HW_plot_date_selection, phenological_bar_plot_2020, ncol = 1, rel_heights = c(4,0.4))



ggsave(panel_plot_daily_irr_tmax_2021_with_pheno_bar, filename = "figures/panel_plot_daily_irr_tmax_2021_with_pheno_bar.pdf", device = cairo_pdf, width = 12, height =8)


#####Cumulative irrigation for 2019-2020-2021####


cumulative_irr_bh_2019 <-irr_tmax_combined_2019  %>%
  arrange(Date) %>%
  group_by(treatment) %>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))%>%
  mutate(Cumulative_Irrigation_mm = cumsum(mm_per_day_acre))%>%
  mutate(cumulative_gallons_acre = cumsum(mm_per_day_acre_gallo))


cumulative_irr_bh_2020 <-irr_tmax_combined_2020  %>%
  arrange(Date) %>%
  group_by(treatment) %>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))%>%
  mutate(Cumulative_Irrigation_mm = cumsum(mm_per_day_acre))%>%
  mutate(cumulative_gallons_acre = cumsum(mm_per_day_acre_gallo))


cumulative_irr_bh_2021 <-irr_tmax_combined_2021  %>%
  arrange(Date) %>%
  group_by(treatment) %>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))%>%
  mutate(Cumulative_Irrigation_mm = cumsum(mm_per_day_acre))%>%
  mutate(cumulative_gallons_acre = cumsum(mm_per_day_acre_gallo))


# Combine dataframes for 2019, 2020, and 2021
combined_data <- bind_rows(
  mutate(cumulative_irr_bh_2019, year = 2019),
  mutate(cumulative_irr_bh_2020, year = 2020),
  mutate(cumulative_irr_bh_2021, year = 2021)
)

# Arrange the combined data by date
combined_data <- combined_data %>% arrange(Date)

# Group by year and treatment, and calculate cumulative irrigation
cumulative_irrigation <- combined_data %>%
  group_by( treatment) %>%
  mutate(Cumulative_Irrigation_mm_total = cumsum(mm_per_day_acre))

# Plot cumulative irrigation for each year
cumulative_irrigation$Date <- as.Date(cumulative_irrigation$Date)

# Define the start and end dates for each year
year_start_dates <- as.Date(c("2019-01-01", "2020-01-01", "2021-01-01"))
year_end_dates <- as.Date(c("2019-12-31", "2020-12-31", "2021-12-31"))

# Create the plot
# Check the Date column
str(cumulative_irrigation$Date)

# Ensure Date column is of Date type and check for NAs
cumulative_irrigation <- cumulative_irrigation %>%
  mutate(Date = as.Date(Date)) %>%
  filter(!is.na(Date))

# Confirm there are no NAs
anyNA(cumulative_irrigation$Date)

# Define the start and end dates for each year
year_start_dates <- as.Date(c("2019-01-01", "2020-01-01", "2021-01-01"))
year_end_dates <- as.Date(c("2019-12-31", "2020-12-31", "2021-12-31"))




####Add daily precipations to the cumulative irrigation plot####

bh_pp_2019<-read.csv("data/bh_precipation_2019.csv")
bh_pp_2020<-read.csv("data/bh_precipitation_2020.csv")
bh_pp_2021<-read.csv("data/bh_precipitation_2021.csv")
str(bh_pp_2019)
str(bh_pp_2020)
str(bh_pp_2021)

bh_daily_precipitation_2020 <-  bh_pp_2020 %>%
  group_by(DOY) %>%
  summarize(daily_rain_mm = sum(rain_mm, na.rm = TRUE)) %>%
  mutate(Date = ymd(paste(2020, 1, 1, sep = "-")) + days(DOY - 1))%>%
  filter(!daily_rain_mm<0)


bh_daily_precipitation_2021 <-  bh_pp_2021 %>%
  group_by(DOY) %>%
  summarize(daily_rain_mm = sum(rain_mm, na.rm = TRUE))%>%
  mutate(Date = ymd(paste(2021, 1, 1, sep = "-")) + days(DOY - 1))%>%
  filter(!daily_rain_mm<0)

str(bh_daily_precipitation_2021)
# Create a complete sequence of dates for the year 2021
complete_dates <- data.frame(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"))

# Merge with the original data frame, filling missing values with zero
bh_daily_precipitation_2021<- complete_dates %>%
  left_join(bh_daily_precipitation_2021, by = "Date") %>%
  mutate(daily_rain_mm = ifelse(is.na(daily_rain_mm), 0, daily_rain_mm))

str(bh_daily_precipitation_2021)

bh_daily_precipitation_2019<-bh_pp_2019 %>%
  mutate(Date = DATE_)%>%
  mutate(daily_rain_mm =rain_mm)

bh_daily_precipitation_2019$Date<-mdy(bh_daily_precipitation_2019$Date)

str(bh_daily_precipitation_2019$Date)

combined_daily_precipitation_bh <- bind_rows(
  bh_daily_precipitation_2019, bh_daily_precipitation_2020, bh_daily_precipitation_2021
)%>%
  select(Date, daily_rain_mm)

str(combined_daily_precipitation_bh)
str(cumulative_irrigation)

combined_daily_precipitation_bh %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarize(Total_Precipitation_mm = sum(daily_rain_mm, na.rm = TRUE))




panel_plot_cumulative_irr_bh_2019_2020_2021_with_daily_irrigation <- ggplot(cumulative_irrigation, aes(x = Date, y = Cumulative_Irrigation_mm_total, color = treatment)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 1.1, alpha = 0.6) +
  theme_classic() +
  scale_y_continuous(
    breaks = seq(0, 1300, 200), limits = c(0, 1300),
    sec.axis = sec_axis(~ . / 10, name = "Daily Precipitation (mm)")
  ) +
  geom_line(data = combined_daily_precipitation_bh, aes(x = Date, y = daily_rain_mm * 10), color = "blue", size = 0.5, alpha = 0.7) +
  geom_point(data = combined_daily_precipitation_bh, aes(x = Date, y = daily_rain_mm * 10), color = "blue", size = 1, alpha = 0.7)+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET", "Precipation mm")) +
  ylab("Cumulative irrigation (mm)") +
  xlab("Date") +
  scale_x_date(date_labels = "%b %Y", breaks = seq.Date(min(combined_daily_precipitation_bh$Date, na.rm = TRUE), max(combined_daily_precipitation_bh$Date, na.rm = TRUE), by = "1.8 months")) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 24, family = "serif")) +
  theme(axis.title.x = element_text(size = 24, family = "serif")) +
  guides(color = guide_legend(override.aes = list(size = 5))) + # Increase legend size
  geom_vline(xintercept = year_start_dates, linetype = "dashed", color = "black") +
  geom_vline(xintercept = year_end_dates, linetype = "dashed", color = "black") +
  annotate("rect", xmin = as.Date("2019-07-25"), xmax = as.Date("2019-07-29"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2019-08-13"), xmax = as.Date("2019-08-17"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("text", x = as.Date("2019-07-24"), y = 1250, label = "HW1", size = 5) +
  annotate("text", x = as.Date("2019-08-19"), y = 1250, label = "HW2", size = 5) +
  annotate("rect", xmin = as.Date("2020-05-26"), xmax = as.Date("2020-05-28"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-07-09"), xmax = as.Date("2020-07-12"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-08-13"), xmax = as.Date("2020-08-19"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-09-05"), xmax = as.Date("2020-09-08"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("text", x = as.Date("2020-05-27"), y = 1250, label = "HW1", size = 5) +
  annotate("text", x = as.Date("2020-07-10"), y = 1250, label = "HW2", size = 5) +
  annotate("text", x = as.Date("2020-08-12"), y = 1250, label = "HW3", size = 5) +
  annotate("text", x = as.Date("2020-09-10"), y = 1250, label = "HW4", size = 5) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 1250, label = "HW3", size = 5) +
  annotate("text", x = as.Date("07-12-2021", format= "%m-%d-%Y"), y = 1250, label = "HW2", size = 5) +
  annotate("text", x = as.Date("06-15-2021", format= "%m-%d-%Y"), y = 1250, label = "HW1", size = 5) +
  annotate("rect", xmin = as.Date("2021-09-07"), xmax = as.Date("2021-09-09"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2021-07-09"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2021-06-18"), xmax = as.Date("2021-06-20"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("text", x = as.Date("08-01-2019", format= "%m-%d-%Y"), y = 1300, label = "2019", size = 6) +
  annotate("text", x = as.Date("06-30-2020", format= "%m-%d-%Y"), y = 1300, label = "2020", size = 6) +
  annotate("text", x = as.Date("05-30-2021", format= "%m-%d-%Y"), y = 1300, label = "2021", size = 6)+
  theme(legend.text = element_text(size = 14))+
  theme(legend.title = element_text(size = 14))# Increase legend text size

ggsave(panel_plot_cumulative_irr_bh_2019_2020_2021_with_daily_irrigation, filename = "figures/panel_plot_cumulative_irr_bh_2019_2020_2021_with_daily_irrigation.png", width = 23, height =12, dpi =300)


# Add treatment column to combined_daily_precipitation_bh with constant value
combined_daily_precipitation_bh$treatment <- "Daily Precipitation"

# Plot cumulative irrigation with viridis colors for treatments and add daily precipitation to the legend
combined_daily_precipitation_bh<- ggplot(cumulative_irrigation, aes(x = Date, y = Cumulative_Irrigation_mm_total, color = treatment)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 1.1, alpha = 0.6) +
  geom_line(data = combined_daily_precipitation_bh, aes(x = Date, y = daily_rain_mm * 10, color = treatment), size = 0.5, alpha = 0.7, show.legend = TRUE) +
  geom_point(data = combined_daily_precipitation_bh, aes(x = Date, y = daily_rain_mm * 10, color = treatment), size = 1, alpha = 0.7, show.legend = TRUE) +
  theme_classic() +
  scale_colour_manual(values = c(scales::viridis_pal(direction = -1, begin = 0.05, end = 0.93)(3), "blue"), 
                      name = "Legend", 
                      labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET", "Daily Precipitation")) +
  ylab("Cumulative irrigation (mm)") +
  xlab("Date") +
  scale_x_date(date_labels = "%b %Y", breaks = seq.Date(min(combined_daily_precipitation_bh$Date, na.rm = TRUE), max(combined_daily_precipitation_bh$Date, na.rm = TRUE), by = "1.8 months")) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1)) + # Rotate x-axis labels for better readability
  theme(axis.text.y = element_text(size = 20)) +
  theme(axis.title.y = element_text(size = 24, family = "serif")) +
  theme(axis.title.x = element_text(size = 24, family = "serif")) +
  guides(color = guide_legend(override.aes = list(size = 5))) + # Increase legend size
  geom_vline(xintercept = year_start_dates, linetype = "dashed", color = "black") +
  geom_vline(xintercept = year_end_dates, linetype = "dashed", color = "black") +
  annotate("rect", xmin = as.Date("2019-07-25"), xmax = as.Date("2019-07-29"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2019-08-13"), xmax = as.Date("2019-08-17"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("text", x = as.Date("2019-07-24"), y = 1250, label = "HW1", size = 5) +
  annotate("text", x = as.Date("2019-08-19"), y = 1250, label = "HW2", size = 5) +
  annotate("rect", xmin = as.Date("2020-05-26"), xmax = as.Date("2020-05-28"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-07-09"), xmax = as.Date("2020-07-12"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-08-13"), xmax = as.Date("2020-08-19"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-09-05"), xmax = as.Date("2020-09-08"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("text", x = as.Date("2020-05-27"), y = 1250, label = "HW1", size = 5) +
  annotate("text", x = as.Date("2020-07-10"), y = 1250, label = "HW2", size = 5) +
  annotate("text", x = as.Date("2020-08-12"), y = 1250, label = "HW3", size = 5) +
  annotate("text", x = as.Date("2020-09-10"), y = 1250, label = "HW4", size = 5) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 1250, label = "HW3", size = 5) +
  annotate("text", x = as.Date("07-12-2021", format= "%m-%d-%Y"), y = 1250, label = "HW2", size = 5) +
  annotate("text", x = as.Date("06-15-2021", format= "%m-%d-%Y"), y = 1250, label = "HW1", size = 5) +
  annotate("rect", xmin = as.Date("2021-09-07"), xmax = as.Date("2021-09-09"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2021-07-09"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 1250, alpha = .2) +
  annotate("rect", xmin = as.Date("2021-06-18"), xmax = as.Date("2021-06-20"), ymin = 0, ymax = 1250, alpha = .2) +
  scale_y_continuous(
    breaks = seq(0, 1300, 200), limits = c(0, 1300),
    sec.axis = sec_axis(~ . / 10, name = "Daily Precipitation (mm)")
  ) +
  annotate("text", x = as.Date("08-01-2019", format= "%m-%d-%Y"), y = 1300, label = "2019", size = 6) +
  annotate("text", x = as.Date("06-30-2020", format= "%m-%d-%Y"), y = 1300, label = "2020", size = 6) +
  annotate("text", x = as.Date("05-30-2021", format= "%m-%d-%Y"), y = 1300, label = "2021", size = 6) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 14)) # Increase legend text size

print(combined_daily_precipitation_bh)
ggsave(combined_daily_precipitation_bh, filename = "figures/combined_daily_precipitation_bh.png", width = 23, height =12, dpi =300)

#### Total irrigation 2019 -2020 -2021 - all treatments ####

str(irr_tmax_combined_2019)
total_rrigation_bh_2019<-irr_tmax_combined_2019  %>%
  arrange(Date) %>%
  group_by(treatment) %>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))%>%
  mutate(mm_per_day_hectare = (mm_per_day_acre*2.47105))

irr_tmax_combined_2019_total<- total_rrigation_bh_2019%>%
  group_by(treatment)%>%
  summarise(Total_mm_per_day = sum(mm_per_day_acre,na.rm =TRUE), Total_gal_acre_day_irrigation = sum(gal_acre_day_irrigation,na.rm = TRUE))%>%
  mutate(year ="2019")


bh_total_irrigation_2020<-bh_irrigation_2020_complete_season%>%
  mutate(mm_per_day_hectare = (gal_acre_day_irrigation/2642.00792602))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1069.10429702))


irr_tmax_combined_2020_total<- bh_total_irrigation_2020%>%
  group_by(treatment)%>%
  summarise(Total_mm_per_day = sum(mm_per_day_acre,na.rm =TRUE), Total_gal_acre_day_irrigation = sum(gal_acre_day_irrigation,na.rm = TRUE))%>%
  mutate(year = "2020")

bh_total_irrigation_2021<-irr_tmax_combined_2021%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1076.39))%>%
  mutate(mm_per_day_acre_gallo = (((hours*0.75)*((43560/60)/325851)*304.8)))


irr_tmax_combined_2021_total<- bh_total_irrigation_2021%>%
  group_by(treatment)%>%
  summarise(Total_mm_per_day = sum(mm_per_day_acre,na.rm =TRUE), Total_gal_acre_day_irrigation = sum(gal_acre_day_irrigation,na.rm = TRUE))%>%
  mutate(year = "2021")
  
####Add daily precipations to the cumulative irrigation plot####

bh_pp_2019<-read.csv("data/bh_precipation_2019.csv")
bh_pp_2020<-read.csv("data/bh_precipitation_2020.csv")
bh_pp_2021<-read.csv("data/bh_precipitation_2021.csv")
str(bh_pp_2019)
str(bh_pp_2020)
str(bh_pp_2021)

bh_daily_precipitation_2020 <-  bh_pp_2020 %>%
  group_by(DOY) %>%
  summarize(daily_rain_mm = sum(rain_mm, na.rm = TRUE)) %>%
  mutate(Date = ymd(paste(2020, 1, 1, sep = "-")) + days(DOY - 1))%>%
  filter(!daily_rain_mm<0)


bh_daily_precipitation_2021 <-  bh_pp_2021 %>%
  group_by(DOY) %>%
  summarize(daily_rain_mm = sum(rain_mm, na.rm = TRUE))%>%
  mutate(Date = ymd(paste(2021, 1, 1, sep = "-")) + days(DOY - 1))%>%
  filter(!daily_rain_mm<0)

str(bh_daily_precipitation_2021)
# Create a complete sequence of dates for the year 2021
complete_dates <- data.frame(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"))

# Merge with the original data frame, filling missing values with zero
bh_daily_precipitation_2021<- complete_dates %>%
  left_join(bh_daily_precipitation_2021, by = "Date") %>%
  mutate(daily_rain_mm = ifelse(is.na(daily_rain_mm), 0, daily_rain_mm))

str(bh_daily_precipitation_2021)

bh_daily_precipitation_2019<-bh_pp_2019 %>%
  mutate(Date = DATE_)%>%
  mutate(daily_rain_mm =rain_mm)

bh_daily_precipitation_2019$Date<-mdy(bh_daily_precipitation_2019$Date)

str(bh_daily_precipitation_2019$Date)

combined_daily_precipitation_bh <- bind_rows(
  bh_daily_precipitation_2019, bh_daily_precipitation_2020, bh_daily_precipitation_2021
)%>%
  select(Date, daily_rain_mm)

str(combined_daily_precipitation_bh)


combined_daily_precipitation_bh %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year) %>%
  summarize(Total_Precipitation_mm = sum(daily_rain_mm, na.rm = TRUE))

total_precipitation_per_year_bh<-combined_daily_precipitation_bh%>%
  mutate(Year = format(Date, "%Y")) %>%  # Extract year from Date
  group_by(Year) %>%                     # Group by year
  summarise(total_rain_mm = sum(daily_rain_mm, na.rm = TRUE))  # Sum rainfall for each year

Total_irrigation_all_years_bh<-rbind(irr_tmax_combined_2019_total,irr_tmax_combined_2020_total, irr_tmax_combined_2021_total)

str(total_precipitation_per_year_bh)

Total_irrigation_all_years_bh <- left_join(total_precipitation_per_year_bh, Total_irrigation_all_years_bh, by = c("Year" = "year"))


str(Total_irrigation_all_years_bh)
Total_irrigation_all_years_bh$Year<-as_factor(Total_irrigation_all_years_bh$Year)
Total_irrigation_all_years_bh$treatment<-as.character(Total_irrigation_all_years_bh$treatment)

str(Total_irrigation_all_years_bh)


write.csv(Total_irrigation_all_years_bh,"data_output/Total_irrigation_all_years_bh.csv")

library(scales)

total_irrigation_gal_acre_bh_all_years_plot<-ggplot(Total_irrigation_all_years_bh, aes(x = Year, y = Total_gal_acre_day_irrigation, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120-90% ET", "180-120% ET")) +
  labs(
    x = "Year",
    y = "Total irrigation (gallons/acre)"
  ) +
  theme_classic() +
theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14))+
  scale_y_continuous(labels = label_comma())

ggsave(total_irrigation_gal_acre_bh_all_years_plot, filename = "figures/total_irrigation_gal_acre_bh_all_years_plot.jpg", width = 14, height =10, dpi =600)


total_irrigation_mm_bh_all_years_plot<-ggplot(Total_irrigation_all_years_bh, aes(x = Year, y = Total_mm_per_day, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120-90% ET", "180-120% ET")) +
  labs(
    x = "Year",
    y = "Total irrigation (mm)"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14))

ggsave(total_irrigation_mm_bh_all_years_plot, filename = "figures/total_irrigation_mm_bh_all_years_plot.jpg", width = 14, height =10, dpi =600)

library(viridis)
fill_colors <- viridis(n = 3, direction = -1, begin = 0.05, end = 0.93)


# Print the hex colors
print(fill_colors)

colors <- viridis(n = 3, direction = -1, begin = 0.05, end = 0.93)

# Print the hex colors
print(colors)



library(tidyr)
library(dplyr)
library(ggplot2)

# Create a version of the dataframe that differentiates between rain and irrigation
combined_df <- Total_irrigation_all_years_bh %>%
  mutate(treatment = as.factor(treatment)) %>%
  group_by(Year) %>%
  summarise(
    total_rain_mm = first(total_rain_mm),  # Keep only one rain value per year
    treatment_1_irrigation = sum(Total_mm_per_day[treatment == "1"]),
    treatment_2_irrigation = sum(Total_mm_per_day[treatment == "2"]),
    treatment_3_irrigation = sum(Total_mm_per_day[treatment == "3"])
  ) %>%
  pivot_longer(
    cols = c(treatment_1_irrigation, treatment_2_irrigation, treatment_3_irrigation, total_rain_mm),
    names_to = "type",
    values_to = "mm_value"
  ) %>%
  # Reorder the 'type' column so that total_rain_mm appears last
  mutate(type = factor(type, levels = c("treatment_1_irrigation", "treatment_2_irrigation", "treatment_3_irrigation", "total_rain_mm")))

# Create the plot
total_irrigation_rain_mm_plot <- ggplot(combined_df, aes(x = Year, y = mm_value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(
    values = c("treatment_1_irrigation" = "#FDE725FF",  #FDE725FF# Viridis colors for treatments
               "treatment_2_irrigation" = "#21908CFF",
               "treatment_3_irrigation" = "#440154FF",
               "total_rain_mm" = "blue"),               # Blue for rainfall
    labels = c("Baseline (60% ET)", "120-90% ET", "180-120% ET", "Rainfall")
  ) +
  labs(
    x = "Year",
    y = "Total irrigation/rain (mm)",
    fill = "Type"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.justification = "center") +
  theme(legend.position = "right") +
  theme(legend.title.align = 0) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  scale_y_continuous(labels = scales::label_comma())

# Print the plot
print(total_irrigation_rain_mm_plot)

# Save the plot
ggsave("figures/total_irrigation_rain_mm_per_year_plot.jpg", total_irrigation_rain_mm_plot, width = 14, height = 10, dpi = 600)


library(dplyr)
library(lubridate)

# Assume your data frame is called 'rainfall_data' with columns 'Date' and 'daily_rain_mm'

# Create a custom year that runs from October of one year to October of the next year
rainfall_data_custom_year <- combined_daily_precipitation_bh %>%
  mutate(custom_year = if_else(month(Date) >= 10, year(Date), year(Date) - 1))  # If month is Oct-Dec, keep the year, else subtract 1

# Now, group by this custom year and sum the rainfall

bh_rainfal_2018_oct_dec<-read.csv("data/bh_oct_nov_dec_rainfall_2018.csv", header = TRUE)
str(bh_rainfal_2018_oct_dec)

bh_rainfal_2018_oct_dec$Date<-mdy(bh_rainfal_2018_oct_dec$Date)


str(rainfall_data_custom_year)


rainfall_data_custom_year_with_2018<-bind_rows(rainfall_data_custom_year,bh_rainfal_2018_oct_dec)

rainfall_by_custom_year <- rainfall_data_custom_year_with_2018 %>%
  group_by(custom_year) %>%
  summarise(total_rain_mm = sum(daily_rain_mm, na.rm = TRUE))




# View the result
print(rainfall_by_custom_year)


df_adjusted <- rainfall_by_custom_year %>%
  filter(custom_year != "2021")  %>%
  mutate(custom_year = case_when(
    custom_year == "2018" ~ "2019",
    custom_year == "2019" ~ "2020",
    custom_year == "2020" ~ "2021",
    TRUE ~ as.character(custom_year)
  ))


df_adjusted <- df_adjusted %>%
  rename(Year = custom_year)

Total_irrigation_all_years_bh<-rbind(irr_tmax_combined_2019_total,irr_tmax_combined_2020_total, irr_tmax_combined_2021_total)


Total_irrigation_all_years_bh_oct_oct <- left_join(df_adjusted, Total_irrigation_all_years_bh, by = c("Year" = "year"))

str(Total_irrigation_all_years_bh_oct_oct)

write.csv(Total_irrigation_all_years_bh_oct_oct,"data_output/Total_irrigation_all_years_bh_oct_oct.csv")

combined_df <- Total_irrigation_all_years_bh_oct_oct %>%
  mutate(treatment = as.factor(treatment)) %>%
  group_by(Year) %>%
  summarise(
    total_rain_mm = first(total_rain_mm),  # Keep only one rain value per year
    treatment_1_irrigation = sum(Total_mm_per_day[treatment == "1"]),
    treatment_2_irrigation = sum(Total_mm_per_day[treatment == "2"]),
    treatment_3_irrigation = sum(Total_mm_per_day[treatment == "3"])
  ) %>%
  pivot_longer(
    cols = c(treatment_1_irrigation, treatment_2_irrigation, treatment_3_irrigation, total_rain_mm),
    names_to = "type",
    values_to = "mm_value"
  ) %>%
  # Reorder the 'type' column so that total_rain_mm appears last
  mutate(type = factor(type, levels = c("treatment_1_irrigation", "treatment_2_irrigation", "treatment_3_irrigation", "total_rain_mm")))

# Create the plot
total_irrigation_rain_mm_plot_oct_to_oct_rain <- ggplot(combined_df, aes(x = Year, y = mm_value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_manual(
    values = c("treatment_1_irrigation" = "#FDE725FF",  #FDE725FF# Viridis colors for treatments
               "treatment_2_irrigation" = "#21908CFF",
               "treatment_3_irrigation" = "#440154FF",
               "total_rain_mm" = "blue"),               # Blue for rainfall
    labels = c("Baseline (60% ET)", "120-90% ET", "180-120% ET", "Rainfall")
  ) +
  labs(
    x = "Year",
    y = "Total irrigation/rain (mm)",
    fill = "Type"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.justification = "center") +
  theme(legend.position = "right") +
  theme(legend.title.align = 0) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 14)) +
  scale_y_continuous(labels = scales::label_comma())

# Print the plot
print(total_irrigation_rain_mm_plot_oct_to_oct_rain)

# Save the plot
ggsave("figures/total_irrigation_rain_mm_plot_oct_to_oct_rain.jpg", total_irrigation_rain_mm_plot_oct_to_oct_rain, width = 14, height = 10, dpi = 600)

######Supplemental figure new phytologist #####





legend <- get_legend(daily_irr_tmax_2019_HW_plot_date_selection)

ggsave(legend, filename = "figures/legend.jpg", width = 3, height =1.5, dpi =600)


panel_plot_irrigation_bh_2019_2020_2021 <- plot_grid(
plot_grid(panel_plot_daily_irr_tmax_2019_with_pheno_bar, panel_plot_daily_irr_tmax_2020_with_pheno_bar,panel_plot_daily_irr_tmax_2021_with_pheno_bar, legend, labels = c("a", "b", "c"),ncol = 1, 
          vjust = 1.5, 
          hjust = -9.6, 
          label_size = 28,
          align = "v", axis = "l")
)

panel_plot_irrigation_bh_2019_2020_2021 <- plot_grid(
  plot_grid(
    panel_plot_daily_irr_tmax_2019_with_pheno_bar, 
    panel_plot_daily_irr_tmax_2020_with_pheno_bar, 
    panel_plot_daily_irr_tmax_2021_with_pheno_bar, 
    labels = c("a", "b", "c"),
    ncol = 1, 
    vjust = 1.5, 
    hjust = -9.6, 
    label_size = 28,
    align = "v", 
    axis = "l",
    rel_heights = c(1, 1, 1)  # Adjust this to control relative heights of panels
  ),
  legend,  # Add the legend as a separate element
  ncol = 2,  # Layout: 2 columns (3 plots and 1 legend)
  rel_widths = c(4, 1)  # Increase the width for the legend
)


ggsave(panel_plot_irrigation_bh_2019_2020_2021, filename = "figures/panel_plot_irrigation_bh_2019_2020_2021.pdf", device = cairo_pdf, width = 24, height =22)


ggsave(panel_plot_irrigation_bh_2019_2020_2021, filename = "figures/panel_plot_irrigation_bh_2019_2020_2021.jpg", width = 20, height =30, dpi =600)

legend_figure_2 <- get_legend(daily_irr_tmax_2019_HW_plot_date_selection)

legend_plot <- ggdraw(legend_figure_2)

ggsave(legend_plot, filename = "figures/legend_figure_2.jpg", width = 4, height =4, dpi =600)
