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
bH_irrigation_summary_2019 <- read.csv("data/2019_BH455_Summary.csv", header = FALSE)
class(bH_irrigation_summary_2019)

transposed_data_irr_bh_2019 <- t(bH_irrigation_summary_2019)

transposed_data_irr_bh_2019 <- as.data.frame(transposed_data_irr_bh_2019)

transposed_data_irr_bh_2019 <- transposed_data_irr_bh_2019[-1, ]

str(transposed_data_irr_bh_2019)



header<- c("Date","Pixel_1","Pixel_2","Pixel_3", "Pixel_4","Pixel_5", "Pixel_6","Pixel_7","Pixel_8","Pixel_9","Pixel_10","Pixel_11","Pixel_12","Pixel_13","Pixel_14","Pixel_15","Pixel_16","Pixel_17","Pixel_18","Pixel_19","Pixel_20","Pixel_21","Pixel_22","Pixel_23","Pixel_24","Pixel_25","Pixel_26","Pixel_27","Pixel_28","Pixel_29","Pixel_30","Pixel_31","Pixel_32","Pixel_33","Pixel_34","Pixel_35","Pixel_36","Pixel_37","Pixel_38","Pixel_39","Pixel_40","Pixel_41","Pixel_42","Pixel_43","Pixel_44","Pixel_45","Pixel_46","Pixel_47","Pixel_48","Pixel_49","Pixel_50","Pixel_51","Pixel_52","Pixel_53","Pixel_54","Pixel_55","Pixel_56","Pixel_57","Pixel_58","Pixel_59","Pixel_60","Pixel_61","Pixel_62","Pixel_63","Pixel_64","Pixel_65","Pixel_66","Pixel_67","Pixel_68","Pixel_69","Pixel_70","Pixel_71","Pixel_72","Pixel_73","Pixel_74","Pixel_75","Pixel_76","Pixel_77","Pixel_78","Pixel_79","Pixel_80","Pixel_81","Pixel_82","Pixel_83","Pixel_84","Pixel_85","Pixel_86","Pixel_88","Pixel_89","Pixel_90","Pixel_91","Pixel_92","Pixel_93","Pixel_95","Pixel_96","Pixel_97","Pixel_98","Pixel_99","Pixel_87A","Pixel_87B")
str(header)
colnames(transposed_data_irr_bh_2019) <- header
colnames(transposed_data_irr_bh_2019)

transposed_data_irr_bh_2019$Date <- as.Date(paste(transposed_data_irr_bh_2019$Date, "2019"), format = "%d-%b %Y")

str(transposed_data_irr_bh_2019$Date)

data_irr_bh_2019<-transposed_data_irr_bh_2019%>%
  select(-Pixel_23,-Pixel_35,-Pixel_34,-Pixel_45,-Pixel_55,-Pixel_54,-Pixel_27,-Pixel_48,-Pixel_67)

data_irr_bh_2019$Date<-ymd(data_irr_bh_2019$Date)

str(data_irr_bh_2019)


data_irr_bh_2019<-data_irr_bh_2019%>%
  filter(Date<"2019-09-19")

# Sample data frame structure
data_irr_bh_2019_total_hours <- data_irr_bh_2019  # Replace with your actual data frame

# Loop through each "Pixel" column to convert time format to hours with decimals
for (pixel_col in grep("^Pixel_", names(data_irr_bh_2019_total_hours), value = TRUE)) {
  # Convert time format (e.g., "6:00") to numeric hours with decimals
  data_irr_bh_2019_total_hours[[pixel_col]] <- ifelse(
    data_irr_bh_2019_total_hours[[pixel_col]] == "", 0,  # Replace empty strings with 0
    as.numeric(sub(":(\\d+)", "", data_irr_bh_2019_total_hours[[pixel_col]])) + 
      as.numeric(sub("^(\\d+):", "", data_irr_bh_2019_total_hours[[pixel_col]])) / 60
  )
}


# Sum the minutes per pixel column
data_irr_bh_2019_total_hours_complete <- sapply(data_irr_bh_2019_total_hours[, grep("^Pixel_", names(data_irr_bh_2019_total_hours))], sum, na.rm = TRUE)

# Print results
data_irr_bh_2019_total_hours_complete_all_pixels <- data.frame(
  Pixel = names(data_irr_bh_2019_total_hours_complete),
  Total_hours = data_irr_bh_2019_total_hours_complete
)

str(data_irr_bh_2019_total_hours_complete_all_pixels)


average_total_hours_all_pixel_bh_2019 <- mean(data_irr_bh_2019_total_hours_complete_all_pixels$Total_hours, na.rm = TRUE)


average_total_hours_all_pixel_bh_2019 <- data.frame(
  pixels = "allpixels",
  total_hours = average_total_hours_all_pixel_bh_2019
)


bh_2019_total_rrigation_pixels_outside_treatment<-average_total_hours_all_pixel_bh_2019%>%
  mutate(gal_acre_day_irrigation =(total_hours*573.5885))%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1069.10429702))%>%
  mutate(mm_per_day_heactare = (mm_per_day_acre*2.47105))



##### Irrigation BH 2020 #####


bH_irrigation_summary_2020 <- read.csv("data/BH455_2020_Summary.csv", header = FALSE)
tair_bh_2020<-read.csv("data/Tmax_BH_2020.csv")

transposed_data_irr_bh_2020 <- t(bH_irrigation_summary_2020)

transposed_data_irr_bh_2020 <- as.data.frame(transposed_data_irr_bh_2020)

transposed_data_irr_bh_2020 <- transposed_data_irr_bh_2020[-1, ]

str(transposed_data_irr_bh_2020)



header<- c("Date","Pixel_1","Pixel_2","Pixel_3", "Pixel_4","Pixel_5", "Pixel_6","Pixel_7","Pixel_8","Pixel_9","Pixel_10","Pixel_11","Pixel_12","Pixel_13","Pixel_14","Pixel_15","Pixel_16","Pixel_17","Pixel_18","Pixel_19","Pixel_20","Pixel_21","Pixel_22","Pixel_23","Pixel_24","Pixel_25","Pixel_26","Pixel_27","Pixel_28","Pixel_29","Pixel_30","Pixel_31","Pixel_32","Pixel_33","Pixel_34","Pixel_35","Pixel_36","Pixel_37","Pixel_38","Pixel_39","Pixel_40","Pixel_41","Pixel_42","Pixel_43","Pixel_44","Pixel_45","Pixel_46","Pixel_47","Pixel_48","Pixel_49","Pixel_50","Pixel_51","Pixel_52","Pixel_53","Pixel_54","Pixel_55","Pixel_56","Pixel_57","Pixel_58","Pixel_59","Pixel_60","Pixel_61","Pixel_62","Pixel_63","Pixel_64","Pixel_65","Pixel_66","Pixel_67","Pixel_68","Pixel_69","Pixel_70","Pixel_71","Pixel_72","Pixel_73","Pixel_74","Pixel_75","Pixel_76","Pixel_77","Pixel_78","Pixel_79","Pixel_80","Pixel_81","Pixel_82","Pixel_83","Pixel_84","Pixel_85","Pixel_86","Pixel_88","Pixel_89","Pixel_90","Pixel_91","Pixel_92","Pixel_93","Pixel_95","Pixel_96","Pixel_97","Pixel_98","Pixel_99","Pixel_87A","Pixel_87B")
str(header)
colnames(transposed_data_irr_bh_2020) <- header
colnames(transposed_data_irr_bh_2020)

transposed_data_irr_bh_2020$Date <- as.Date(paste(transposed_data_irr_bh_2020$Date, "2020"), format = "%d-%b %Y")

str(transposed_data_irr_bh_2020$Date)

data_irr_bh_2020<-transposed_data_irr_bh_2020%>%
  select(-Pixel_4,-Pixel_6,-Pixel_13,-Pixel_15,-Pixel_14,-Pixel_16,-Pixel_23,-Pixel_25,-Pixel_24,-Pixel_26,-Pixel_33,-Pixel_35,-Pixel_34,-Pixel_36,-Pixel_43,-Pixel_45,-Pixel_44,-Pixel_46,-Pixel_53,-Pixel_55,-Pixel_54,-Pixel_56,-Pixel_63,-Pixel_65,-Pixel_18,-Pixel_20,-Pixel_27,-Pixel_29,-Pixel_47,-Pixel_49,-Pixel_48,-Pixel_50,-Pixel_58,-Pixel_60,-Pixel_67,-Pixel_69)

str(data_irr_bh_2020)

data_irr_bh_2020$Date<-ymd(data_irr_bh_2020$Date)
data_irr_bh_2020<-data_irr_bh_2020%>%
  filter(Date<"2020-09-17")


# Sample data frame structure
data_irr_bh_2020_total_hours <- data_irr_bh_2020  # Replace with your actual data frame

# Loop through each "Pixel" column to convert time format to hours with decimals
for (pixel_col in grep("^Pixel_", names(data_irr_bh_2020_total_hours), value = TRUE)) {
  # Convert time format (e.g., "6:00") to numeric hours with decimals
  data_irr_bh_2020_total_hours[[pixel_col]] <- ifelse(
    data_irr_bh_2020_total_hours[[pixel_col]] == "", 0,  # Replace empty strings with 0
    as.numeric(sub(":(\\d+)", "", data_irr_bh_2020_total_hours[[pixel_col]])) + 
      as.numeric(sub("^(\\d+):", "", data_irr_bh_2020_total_hours[[pixel_col]])) / 60
  )
}


# Sum the minutes per pixel column
data_irr_bh_2020_total_hours_complete <- sapply(data_irr_bh_2020_total_hours[, grep("^Pixel_", names(data_irr_bh_2020_total_hours))], sum, na.rm = TRUE)

# Print results
data_irr_bh_2020_total_hours_complete_all_pixels <- data.frame(
  Pixel = names(data_irr_bh_2020_total_hours_complete),
  Total_hours = data_irr_bh_2020_total_hours_complete
)

str(data_irr_bh_2020_total_hours_complete_all_pixels)


average_total_hours_all_pixel_bh_2020 <- mean(data_irr_bh_2020_total_hours_complete_all_pixels$Total_hours, na.rm = TRUE)


average_total_hours_all_pixel_bh_2020 <- data.frame(
  pixels = "allpixels",
  total_hours = average_total_hours_all_pixel_bh_2020
)


bh_2020_total_rrigation_pixels_outside_treatment<-average_total_hours_all_pixel_bh_2020%>%
  mutate(gal_acre_day_irrigation =(total_hours*573.5885))%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1069.10429702))%>%
  mutate(mm_per_day_heactare = (mm_per_day_acre*2.47105))




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
  select(-Pixel_4,-Pixel_6,-Pixel_13,-Pixel_15,-Pixel_14,-Pixel_16,-Pixel_23,-Pixel_25,-Pixel_24,-Pixel_26,-Pixel_33,-Pixel_35,-Pixel_34,-Pixel_36,-Pixel_43,-Pixel_45,-Pixel_44,-Pixel_46,-Pixel_53,-Pixel_55,-Pixel_54,-Pixel_56,-Pixel_63,-Pixel_65,-Pixel_18,-Pixel_20,-Pixel_27,-Pixel_29,-Pixel_47,-Pixel_49,-Pixel_48,-Pixel_50,-Pixel_58,-Pixel_60,-Pixel_67,-Pixel_69)



data_irr_bh_2021<-data_irr_bh_2021%>%
  filter(Date<"2021-09-16")


# Sample data frame structure
data_irr_bh_2021_total_hours <- data_irr_bh_2021  # Replace with your actual data frame

# Loop through each "Pixel" column to convert time format to hours with decimals
for (pixel_col in grep("^Pixel_", names(data_irr_bh_2021_total_hours), value = TRUE)) {
  # Convert time format (e.g., "6:00") to numeric hours with decimals
  data_irr_bh_2021_total_hours[[pixel_col]] <- ifelse(
    data_irr_bh_2021_total_hours[[pixel_col]] == "", 0,  # Replace empty strings with 0
    as.numeric(sub(":(\\d+)", "", data_irr_bh_2021_total_hours[[pixel_col]])) + 
      as.numeric(sub("^(\\d+):", "", data_irr_bh_2021_total_hours[[pixel_col]])) / 60
  )
}


# Sum the minutes per pixel column
data_irr_bh_2021_total_hours_complete <- sapply(data_irr_bh_2021_total_hours[, grep("^Pixel_", names(data_irr_bh_2021_total_hours))], sum, na.rm = TRUE)

# Print results
data_irr_bh_2021_total_hours_complete_all_pixels <- data.frame(
  Pixel = names(data_irr_bh_2021_total_hours_complete),
  Total_hours = data_irr_bh_2021_total_hours_complete
)

str(data_irr_bh_2021_total_hours_complete_all_pixels)


average_total_hours_all_pixel_bh_2021 <- mean(data_irr_bh_2021_total_hours_complete_all_pixels$Total_hours, na.rm = TRUE)


average_total_hours_all_pixel_bh_2021 <- data.frame(
  pixels = "allpixels",
  total_hours = average_total_hours_all_pixel_bh_2021
)


bh_2021_total_rrigation_pixels_outside_treatment<-average_total_hours_all_pixel_bh_2021%>%
  mutate(gal_acre_day_irrigation =(total_hours*573.5885))%>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/1069.10429702))%>%
  mutate(mm_per_day_heactare = (mm_per_day_acre*2.47105))

str(bh_2021_total_rrigation_pixels_outside_treatment)
str(bh_2020_total_rrigation_pixels_outside_treatment)
str(bh_2019_total_rrigation_pixels_outside_treatment)

bh_2021_total_rrigation_pixels_outside_treatment$year<-2021
bh_2020_total_rrigation_pixels_outside_treatment$year<-2020
bh_2019_total_rrigation_pixels_outside_treatment$year<-2019


bh_all_3_years_rest_of_pixels<-rbind(
  bh_2021_total_rrigation_pixels_outside_treatment,
  bh_2020_total_rrigation_pixels_outside_treatment,
  bh_2019_total_rrigation_pixels_outside_treatment
)

bh_all_3_years_rest_of_pixels$treatment<-4

str(total_irrigation_bh_all_years_at_harvest_sums)

bh_all_3_years_rest_of_pixels_mm_per_hectare<-bh_all_3_years_rest_of_pixels%>%
  mutate(Total_mm_per_day_per_hectare =mm_per_day_heactare)%>%
  select(year, treatment, Total_mm_per_day_per_hectare)

str(bh_all_3_years_rest_of_pixels_mm_per_hectare)

bh_all_years_tretaments_and_gallos_irrigation<-rbind(bh_all_3_years_rest_of_pixels_mm_per_hectare,total_irrigation_bh_all_years_at_harvest_sums )

str(bh_all_years_tretaments_and_gallos_irrigation)



# Create a version of the dataframe that differentiates between rain and irrigation
combined_df <- Total_irrigation_all_years_bh %>%
  mutate(treatment = as.factor(treatment)) %>%
  group_by(Year) %>%
  summarise(
    total_rain_mm = total_rain_mm,  # Keep only one rain value per year
    treatment_1_irrigation = (Total_mm_per_day_per_hectare[treatment == "1"]),
    treatment_2_irrigation =(Total_mm_per_day_per_hectare[treatment == "2"]),
    treatment_3_irrigation = (Total_mm_per_day_per_hectare[treatment == "3"])
  ) %>%
  pivot_longer(
    cols = c(treatment_1_irrigation, treatment_2_irrigation, treatment_3_irrigation, total_rain_mm),
    names_to = "type",
    values_to = "mm_value"
  ) %>%
  # Reorder the 'type' column so that total_rain_mm appears last
  mutate(type = factor(type, levels = c("treatment_1_irrigation", "treatment_2_irrigation", "treatment_3_irrigation", "total_rain_mm")))

bh_all_years_tretaments_and_gallos_irrigation$treatment<-as.factor(bh_all_years_tretaments_and_gallos_irrigation$treatment)

library(viridisLite)
first_four_colors <- viridis(4, direction = -1)

# Display the colors
first_four_colors

str(bh_all_years_tretaments_and_gallos_irrigation$treatment)
# Create the plot
total_irrigation_rain_mm_plot_with_gallos_irrigation <- ggplot(bh_all_years_tretaments_and_gallos_irrigation, aes(x = year, y = Total_mm_per_day_per_hectare, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
#  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET", "Commercial Irrigation"))+
  scale_fill_manual(
    values = c("1" = "#FDE725FF",  #FDE725FF# Viridis colors for treatments
               "2" = "#21908CFF",
               "3" = "#440154FF",
               "4" = "#31688EFF"),               # Blue for rainfall
    labels = c("Baseline (60% ET)", "120-90% ET", "180-120% ET", "Commercial Irrigation")
  ) +
  labs(
    x = "Year",
    y = "Total irrigation (mm/hectare)"
  ) +
  theme_classic() +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.justification = "center") +
  theme(legend.position = "right") +
  theme(legend.title.align = 0) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) 

ggsave(total_irrigation_rain_mm_plot_with_gallos_irrigation)

ggsave("figures/total_irrigation_rain_mm_plot_with_gallos_irrigation.jpg", total_irrigation_rain_mm_plot_with_gallos_irrigation, width = 12, height = 8)

write.csv(bh_all_years_tretaments_and_gallos_irrigation,"data_output/bh_all_years_tretaments_and_gallos_irrigation.csv")

# Print the plot