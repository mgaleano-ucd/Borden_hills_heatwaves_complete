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
  select(Date,Pixel_23,Pixel_35,Pixel_34,Pixel_45,Pixel_55,Pixel_54,Pixel_27,Pixel_48,Pixel_67)


columns_to_select_treatment1 <- data_irr_bh_2019[, c("Pixel_34","Pixel_55","Pixel_67")]

time_columns<- c("Pixel_34","Pixel_55","Pixel_67")

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


#row_averages_treatment_1 <- rowMeans(columns_to_select_treatment1)

# Add the row averages to the data frame
#columns_to_select_treatment1$Row_Average <- row_averages_treatment_1
columns_to_select_treatment1$Date<-data_irr_bh_2019$Date
columns_to_select_treatment1$treatment<- c(1) 


columns_to_select_treatment2 <- data_irr_bh_2019[, c("Pixel_23","Pixel_35","Pixel_48")]

time_columns<- c("Pixel_23","Pixel_35","Pixel_48")

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


#row_averages_treatment_2 <- rowMeans(columns_to_select_treatment2)

# Add the row averages to the data frame
#columns_to_select_treatment2$Row_Average <- row_averages_treatment_2
columns_to_select_treatment2$Date<-data_irr_bh_2019$Date
columns_to_select_treatment2$treatment<- c(2) 



columns_to_select_treatment3 <- data_irr_bh_2019[, c("Pixel_45","Pixel_54","Pixel_27")]

time_columns<- c("Pixel_45","Pixel_54","Pixel_27")

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


#row_averages_treatment_3 <- rowMeans(columns_to_select_treatment3)

# Add the row averages to the data frame
#columns_to_select_treatment3$Row_Average <- row_averages_treatment_3
columns_to_select_treatment3$Date<-data_irr_bh_2019$Date
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

bh_irrigation_2019_complete_season <- bind_rows(
  columns_to_select_treatment1,
  columns_to_select_treatment2,
  columns_to_select_treatment3
)

write.csv(bh_irrigation_2019_complete_season,"data_output/bh_irrigation_2019_complete_season.csv")

irr<-bh_irrigation_2019_complete_season




tmax$Date <- mdy(tmax$Date)

str(tmax)


tmax<-tmax %>%
  filter(!is.na(Tmax))


str(tmax$Date)
str(irr$Date)

str(irr)
irr<-irr%>%
  mutate(gal_acre_day_irrigation_pixel_34 =(Pixel_34*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_55 =(Pixel_55*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_67 =(Pixel_67*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_23 =(Pixel_23*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_35 =(Pixel_35*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_48 =(Pixel_48*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_27 =(Pixel_27*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_45 =(Pixel_45*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_54 =(Pixel_54*573.5885))

bh_irrigation_2019_per_pixel<-irr%>%
  mutate(mm_per_day_acre_pixel_34 = (gal_acre_day_irrigation_pixel_34/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_55 = (gal_acre_day_irrigation_pixel_55/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_67 = (gal_acre_day_irrigation_pixel_67/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_23 = (gal_acre_day_irrigation_pixel_23/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_35 = (gal_acre_day_irrigation_pixel_35/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_48 = (gal_acre_day_irrigation_pixel_48/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_27 = (gal_acre_day_irrigation_pixel_27/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_45 = (gal_acre_day_irrigation_pixel_45/1069.10429702))%>%
  mutate(mm_per_day_acre_pixel_54 = (gal_acre_day_irrigation_pixel_54/1069.10429702))

bh_irrigation_2019_per_pixel<-bh_irrigation_2019_per_pixel%>%
  mutate(mm_per_day_heactare_pixel_34 = (mm_per_day_acre_pixel_34*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_55 = (mm_per_day_acre_pixel_55*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_67 = (mm_per_day_acre_pixel_67*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_23 = (mm_per_day_acre_pixel_23*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_35 = (mm_per_day_acre_pixel_35*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_48 = (mm_per_day_acre_pixel_48*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_27 = (mm_per_day_acre_pixel_27*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_45 = (mm_per_day_acre_pixel_45*2.47105))%>%
  mutate(mm_per_day_heactare_pixel_54 = (mm_per_day_acre_pixel_54*2.47105))

bh_irrigation_2019_per_day_pixel_area<-bh_irrigation_2019_per_pixel%>%
  mutate(mm_per_day_pixel_area_34 = (mm_per_day_acre_pixel_34/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_55 = (mm_per_day_acre_pixel_55/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_67 = (mm_per_day_acre_pixel_67/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_23 = (mm_per_day_acre_pixel_23/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_35 = (mm_per_day_acre_pixel_35/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_48 = (mm_per_day_acre_pixel_48/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_27 = (mm_per_day_acre_pixel_27/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_45 = (mm_per_day_acre_pixel_45/4046.86*900))%>%
  mutate(mm_per_day_pixel_area_54 = (mm_per_day_acre_pixel_54/4046.86*900))


bh_irrigation_2019_per_pixel_mm_per_day<-bh_irrigation_2019_per_pixel%>%
  select(Date,mm_per_day_acre_pixel_34, mm_per_day_acre_pixel_55, mm_per_day_acre_pixel_67, mm_per_day_acre_pixel_23, mm_per_day_acre_pixel_35, mm_per_day_acre_pixel_48, mm_per_day_acre_pixel_27, mm_per_day_acre_pixel_45, mm_per_day_acre_pixel_54)

bh_irrigation_2019_per_pixel_mm_per_day_date_selection<-bh_irrigation_2019_per_pixel_mm_per_day%>%
  filter(Date<"2019-09-19")



str(bh_irrigation_2019_per_pixel_mm_per_day_date_selection)

bh_irrigation_2019_sums_mm_per_day_acre <- bh_irrigation_2019_per_pixel_mm_per_day_date_selection%>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2019_sums_mm_per_day_acre$year<-2019


bh_irrigation_2019_per_pixel_mm_per_day_hectare<-bh_irrigation_2019_per_pixel%>%
  select(Date,mm_per_day_heactare_pixel_34, mm_per_day_heactare_pixel_55, mm_per_day_heactare_pixel_67, mm_per_day_heactare_pixel_23, mm_per_day_heactare_pixel_35, mm_per_day_heactare_pixel_48, mm_per_day_heactare_pixel_27, mm_per_day_heactare_pixel_45, mm_per_day_heactare_pixel_54)

bh_irrigation_2019_per_pixel_mm_per_day_hectare_date_selection<-bh_irrigation_2019_per_pixel_mm_per_day_hectare%>%
  filter(Date<"2019-09-19")

str(bh_irrigation_2019_per_pixel_mm_per_day_hectare_date_selection)

bh_irrigation_2019_sums_mm_per_day_hectare <- bh_irrigation_2019_per_pixel_mm_per_day_hectare_date_selection%>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)


bh_irrigation_2019_sums_mm_per_day_hectare$year<-2019


bh_irrigation_2019_per_pixel_gallons_acre<-bh_irrigation_2019_per_pixel%>%
  select(Date,gal_acre_day_irrigation_pixel_34, gal_acre_day_irrigation_pixel_55, gal_acre_day_irrigation_pixel_67, gal_acre_day_irrigation_pixel_23, gal_acre_day_irrigation_pixel_35, gal_acre_day_irrigation_pixel_48, gal_acre_day_irrigation_pixel_27, gal_acre_day_irrigation_pixel_45, gal_acre_day_irrigation_pixel_54)

bh_irrigation_2019_per_pixel_gallons_acre_date_selection<-bh_irrigation_2019_per_pixel_gallons_acre%>%
  filter(Date<"2019-09-19")



str(bh_irrigation_2019_per_pixel_gallons_acre_date_selection)


bh_irrigation_2019_sums_gallon_per_acre <- bh_irrigation_2019_per_pixel_gallons_acre_date_selection%>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2019_sums_gallon_per_acre$year<-2019


bh_irrigation_2019_per_day_pixel_area<-bh_irrigation_2019_per_day_pixel_area%>%
  select(Date,mm_per_day_pixel_area_34, mm_per_day_pixel_area_55, mm_per_day_pixel_area_67, mm_per_day_pixel_area_23, mm_per_day_pixel_area_35, mm_per_day_pixel_area_48, mm_per_day_pixel_area_27, mm_per_day_pixel_area_45, mm_per_day_pixel_area_54)

bh_irrigation_2019_per_day_pixel_area_date_selection<-bh_irrigation_2019_per_day_pixel_area%>%
  filter(Date<"2019-09-19")



str(bh_irrigation_2019_per_day_pixel_area_date_selection)

bh_irrigation_2019_sums_mm_per_day_pixel_area <- bh_irrigation_2019_per_day_pixel_area_date_selection%>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2019_sums_mm_per_day_pixel_area$year<-2019


str(bh_irrigation_2019_sums_gallon_per_acre)
str(bh_irrigation_2019_sums_mm_per_day_hectare)
str(bh_irrigation_2019_sums_mm_per_day_acre)
str(bh_irrigation_2019_sums_mm_per_day_pixel_area)
# 1. Convert the gallon per acre data to long format
bh_gallon_long <- bh_irrigation_2019_sums_gallon_per_acre %>%
  pivot_longer(
    cols = starts_with("gal_acre_day_irrigation_pixel_"),
    names_to = "pixel",
    names_prefix = "gal_acre_day_irrigation_pixel_",
    values_to = "gallon_per_acre"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))

# 2. Convert the mm per hectare data to long format
bh_hectare_long <- bh_irrigation_2019_sums_mm_per_day_hectare %>%
  pivot_longer(
    cols = starts_with("mm_per_day_heactare_pixel_"),
    names_to = "pixel",
    names_prefix = "mm_per_day_heactare_pixel_",
    values_to = "mm_per_day_hectare"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))

str(bh_hectare_long)

# 3. Convert the mm per acre data to long format
bh_acre_long <- bh_irrigation_2019_sums_mm_per_day_acre %>%
  pivot_longer(
    cols = starts_with("mm_per_day_acre_pixel_"),
    names_to = "pixel",
    names_prefix = "mm_per_day_acre_pixel_",
    values_to = "mm_per_day_acre"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))

bh_mm__per_pixel_area_long <- bh_irrigation_2019_sums_mm_per_day_pixel_area %>%
  pivot_longer(
    cols = starts_with("mm_per_day_pixel_area_"),  # Pivot all columns that start with "mm_per_day_pixel_area_"
    names_to = "pixel",                            # Name for the pixel column
    names_prefix = "mm_per_day_pixel_area_",       # Remove the prefix when pivoting
    values_to = "mm_per_day_per_pixel_area"                       # Name for the values column
  ) %>%
  mutate(pixel = gsub("_.*", "", pixel))           # Clean up pixel names if needed

# 4. Merge all three long-format data frames by pixel and year
combined_data_bh_2019 <- bh_gallon_long %>%
  left_join(bh_hectare_long, by = c("pixel", "year")) %>%
  left_join(bh_acre_long, by = c("pixel", "year"))%>%
  left_join(bh_mm__per_pixel_area_long, by = c("pixel", "year"))

combined_data_bh_2019_pixel<-combined_data_bh_2019%>%
  mutate(BH_Block = case_when(
    pixel == 34 ~ "B1R2 LEAKY_PIXEL",
    pixel == 55 ~ "B1R3 SE",
    pixel == 67 ~ "B1R4 NE", 
    pixel == 23 ~ "B2R1 NE",
    pixel == 35 ~ "B2R2 SE",
    pixel == 48 ~ "B2R3 NE",
    pixel == 27 ~ "B3R1 NE",
    pixel == 45 ~ "B3R2 SE",
    pixel == 54 ~ "B3R3 NW"
  ))%>%
  mutate( treatment= case_when(
    BH_Block == "B1R2 LEAKY_PIXEL" ~ 1,
    BH_Block == "B1R3 SE" ~ 1,
    BH_Block == "B1R4 NE" ~ 1, 
    BH_Block == "B2R1 NE" ~ 2,
    BH_Block == "B2R2 SE"~ 2,
    BH_Block == "B2R3 NE" ~ 2,
    BH_Block == "B3R1 NE" ~ 3,
    BH_Block == "B3R2 SE" ~ 3,
    BH_Block == "B3R3 NW" ~ 3))



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


bh_irrigation_2020_complete_season <- bind_rows(
  columns_to_select_treatment1,
  columns_to_select_treatment2,
  columns_to_select_treatment3
)


bh_irrigation_2020_complete_season<-bh_irrigation_2020_complete_season

bh_irrigation_2020_complete_season<-bh_irrigation_2020_complete_season%>%
  mutate(hours = Row_Average)


bh_irrigation_2020_complete_season<-bh_irrigation_2020_complete_season%>%
  filter(!is.na(hours))


str(bh_irrigation_2020_complete_season$Date)

str(bh_irrigation_2020_complete_season)

bh_irrigation_2020_per_pixel<-bh_irrigation_2020_complete_season%>%
  mutate(gal_acre_day_irrigation =(hours*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_4 =(Pixel_4*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_6 =(Pixel_6*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_13 =(Pixel_13*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_15 =(Pixel_15*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_44 =(Pixel_44*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_46 =(Pixel_46*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_53 =(Pixel_53*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_55 =(Pixel_55*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_58 =(Pixel_58*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_60 =(Pixel_60*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_67 =(Pixel_67*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_69 =(Pixel_69*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_14 =(Pixel_14*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_16 =(Pixel_16*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_23 =(Pixel_23*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_25 =(Pixel_25*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_24 =(Pixel_24*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_26 =(Pixel_26*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_33 =(Pixel_33*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_35 =(Pixel_35*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_47 =(Pixel_47*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_48 =(Pixel_48*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_49 =(Pixel_49*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_50 =(Pixel_50*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_34 =(Pixel_34*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_36 =(Pixel_36*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_43 =(Pixel_43*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_45 =(Pixel_45*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_54 =(Pixel_54*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_56 =(Pixel_56*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_63 =(Pixel_63*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_65 =(Pixel_65*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_18 =(Pixel_18*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_20 =(Pixel_20*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_27 =(Pixel_27*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_29 =(Pixel_29*573.5885))



bh_irrigation_2020_per_pixel <- bh_irrigation_2020_per_pixel %>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation/ 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_34 = (gal_acre_day_irrigation_pixel_34 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_55 = (gal_acre_day_irrigation_pixel_55 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_67 = (gal_acre_day_irrigation_pixel_67 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_23 = (gal_acre_day_irrigation_pixel_23 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_35 = (gal_acre_day_irrigation_pixel_35 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_48 = (gal_acre_day_irrigation_pixel_48 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_27 = (gal_acre_day_irrigation_pixel_27 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_45 = (gal_acre_day_irrigation_pixel_45 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_54 = (gal_acre_day_irrigation_pixel_54 / 1069.10429702)) %>%
  # Add the missing pixel columns below
  mutate(mm_per_day_acre_pixel_4  = (gal_acre_day_irrigation_pixel_4  / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_6  = (gal_acre_day_irrigation_pixel_6  / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_13 = (gal_acre_day_irrigation_pixel_13 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_15 = (gal_acre_day_irrigation_pixel_15 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_44 = (gal_acre_day_irrigation_pixel_44 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_46 = (gal_acre_day_irrigation_pixel_46 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_53 = (gal_acre_day_irrigation_pixel_53 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_58 = (gal_acre_day_irrigation_pixel_58 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_60 = (gal_acre_day_irrigation_pixel_60 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_69 = (gal_acre_day_irrigation_pixel_69 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_14 = (gal_acre_day_irrigation_pixel_14 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_16 = (gal_acre_day_irrigation_pixel_16 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_25 = (gal_acre_day_irrigation_pixel_25 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_24 = (gal_acre_day_irrigation_pixel_24 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_26 = (gal_acre_day_irrigation_pixel_26 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_33 = (gal_acre_day_irrigation_pixel_33 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_36 = (gal_acre_day_irrigation_pixel_36 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_43 = (gal_acre_day_irrigation_pixel_43 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_56 = (gal_acre_day_irrigation_pixel_56 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_63 = (gal_acre_day_irrigation_pixel_63 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_65 = (gal_acre_day_irrigation_pixel_65 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_18 = (gal_acre_day_irrigation_pixel_18 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_20 = (gal_acre_day_irrigation_pixel_20 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_29 = (gal_acre_day_irrigation_pixel_29 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_47 = (gal_acre_day_irrigation_pixel_47 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_49 = (gal_acre_day_irrigation_pixel_49 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_50 = (gal_acre_day_irrigation_pixel_50 / 1069.10429702))


bh_irrigation_2020_per_pixel <- bh_irrigation_2020_per_pixel%>%
  mutate(mm_per_day_hectare = (mm_per_day_acre *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_34 = (mm_per_day_acre_pixel_34 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_55 = (mm_per_day_acre_pixel_55 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_67 = (mm_per_day_acre_pixel_67 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_23 = (mm_per_day_acre_pixel_23 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_35 = (mm_per_day_acre_pixel_35 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_48 = (mm_per_day_acre_pixel_48 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_27 = (mm_per_day_acre_pixel_27 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_45 = (mm_per_day_acre_pixel_45 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_54 = (mm_per_day_acre_pixel_54 *2.47105)) %>%
  # Add the missing pixel columns below
  mutate(mm_per_day_hectare_pixel_4  = (mm_per_day_acre_pixel_4  *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_6  = (mm_per_day_acre_pixel_6  *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_13 = (mm_per_day_acre_pixel_13 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_15 = (mm_per_day_acre_pixel_15 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_44 = (mm_per_day_acre_pixel_44 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_46 = (mm_per_day_acre_pixel_46 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_53 = (mm_per_day_acre_pixel_53 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_58 = (mm_per_day_acre_pixel_58 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_60 = (mm_per_day_acre_pixel_60 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_69 = (mm_per_day_acre_pixel_69 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_14 = (mm_per_day_acre_pixel_14 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_16 = (mm_per_day_acre_pixel_16 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_25 = (mm_per_day_acre_pixel_25 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_24 = (mm_per_day_acre_pixel_24 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_26 = (mm_per_day_acre_pixel_26 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_33 = (mm_per_day_acre_pixel_33 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_36 = (mm_per_day_acre_pixel_36 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_43 = (mm_per_day_acre_pixel_43 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_56 = (mm_per_day_acre_pixel_56 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_63 = (mm_per_day_acre_pixel_63 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_65 = (mm_per_day_acre_pixel_65 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_18 = (mm_per_day_acre_pixel_18 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_20 = (mm_per_day_acre_pixel_20 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_29 = (mm_per_day_acre_pixel_29 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_47 = (mm_per_day_acre_pixel_47 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_49 = (mm_per_day_acre_pixel_49 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_50 = (mm_per_day_acre_pixel_50 *2.47105))



bh_irrigation_2020_per_pixel <- bh_irrigation_2020_per_pixel%>%
  mutate(mm_per_day_pixel_area = (mm_per_day_acre /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_34 = (mm_per_day_acre_pixel_34 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_55 = (mm_per_day_acre_pixel_55 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_67 = (mm_per_day_acre_pixel_67 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_23 = (mm_per_day_acre_pixel_23 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_35 = (mm_per_day_acre_pixel_35 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_48 = (mm_per_day_acre_pixel_48 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_27 = (mm_per_day_acre_pixel_27 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_45 = (mm_per_day_acre_pixel_45 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_54 = (mm_per_day_acre_pixel_54 /4046.86*900)) %>%
  # Add the missing pixel columns below
  mutate(mm_per_day_pixel_area_4  = (mm_per_day_acre_pixel_4  /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_6  = (mm_per_day_acre_pixel_6  /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_13 = (mm_per_day_acre_pixel_13 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_15 = (mm_per_day_acre_pixel_15 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_44 = (mm_per_day_acre_pixel_44 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_46 = (mm_per_day_acre_pixel_46 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_53 = (mm_per_day_acre_pixel_53 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_58 = (mm_per_day_acre_pixel_58 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_60 = (mm_per_day_acre_pixel_60 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_69 = (mm_per_day_acre_pixel_69 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_14 = (mm_per_day_acre_pixel_14 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_16 = (mm_per_day_acre_pixel_16 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_25 = (mm_per_day_acre_pixel_25 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_24 = (mm_per_day_acre_pixel_24 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_26 = (mm_per_day_acre_pixel_26 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_33 = (mm_per_day_acre_pixel_33 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_36 = (mm_per_day_acre_pixel_36 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_43 = (mm_per_day_acre_pixel_43 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_56 = (mm_per_day_acre_pixel_56 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_63 = (mm_per_day_acre_pixel_63 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_65 = (mm_per_day_acre_pixel_65 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_18 = (mm_per_day_acre_pixel_18 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_20 = (mm_per_day_acre_pixel_20 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_29 = (mm_per_day_acre_pixel_29 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_47 = (mm_per_day_acre_pixel_47 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_49 = (mm_per_day_acre_pixel_49 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_50 = (mm_per_day_acre_pixel_50 /4046.86*900))

bh_irrigation_2020_per_pixel<-bh_irrigation_2020_per_pixel%>%
  filter(Date<"2020-09-17")

write.csv(bh_irrigation_2020_per_pixel,"data_output/bh_irrigation_2020_per_pixel.csv")

bh_irrigation_2020_per_pixel_mm_per_acre <- bh_irrigation_2020_per_pixel %>%
  select(Date, starts_with("mm_per_day_acre_pixel"))


str(bh_irrigation_2020_per_pixel)

bh_irrigation_2020_sums_mm_per_acre <- bh_irrigation_2020_per_pixel_mm_per_acre %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2020_sums_mm_per_acre$year<-2020

str(bh_irrigation_2020_per_pixel)
bh_irrigation_2020_per_pixel_gallons_per_acre <- bh_irrigation_2020_per_pixel %>%
  select(Date, starts_with("gal_acre_day_irrigation_pixel"))


str(bh_irrigation_2020_per_pixel_gallons_per_acre)

bh_irrigation_2020_sums_gallons_per_acre <- bh_irrigation_2020_per_pixel_gallons_per_acre %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2020_sums_gallons_per_acre$year<-2020

bh_irrigation_2020_per_pixel_mm_per_hectare<- bh_irrigation_2020_per_pixel %>%
  select(Date, starts_with("mm_per_day_hectare_"))


str(bh_irrigation_2020_per_pixel_mm_per_hectare)

bh_irrigation_2020_sums_mm_per_hectare <- bh_irrigation_2020_per_pixel_mm_per_hectare %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2020_sums_mm_per_hectare$year<-2020

str(bh_irrigation_2020_per_pixel)
bh_irrigation_2020_mm_per_day_per_pixel_area<- bh_irrigation_2020_per_pixel %>%
  select(Date, starts_with("mm_per_day_pixel_area_"))


str(bh_irrigation_2020_mm_per_day_per_pixel_area)

bh_irrigation_2020_sums_mm_per_day_per_pixel_area <- bh_irrigation_2020_mm_per_day_per_pixel_area %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2020_sums_mm_per_day_per_pixel_area$year<-2020

str(bh_irrigation_2020_sums_gallons_per_acre)
str(bh_irrigation_2020_sums_mm_per_acre)
str(bh_irrigation_2020_sums_mm_per_hectare)
str(bh_irrigation_2020_sums_mm_per_day_per_pixel_area)


# 1. Convert the gallon per acre data to long format
bh_gallon_long_2020 <- bh_irrigation_2020_sums_gallons_per_acre %>%
  pivot_longer(
    cols = starts_with("gal_acre_day_irrigation_pixel_"),
    names_to = "pixel",
    names_prefix = "gal_acre_day_irrigation_pixel_",
    values_to = "gallon_per_acre"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))

# 2. Convert the mm per hectare data to long format
bh_hectare_long_2020 <- bh_irrigation_2020_sums_mm_per_hectare %>%
  pivot_longer(
    cols = starts_with("mm_per_day_hectare_pixel_"),
    names_to = "pixel",
    names_prefix = "mm_per_day_hectare_pixel_",
    values_to = "mm_per_day_hectare"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))



# 3. Convert the mm per acre data to long format
bh_acre_long_2020 <-bh_irrigation_2020_sums_mm_per_acre %>%
  pivot_longer(
    cols = starts_with("mm_per_day_acre_pixel_"),
    names_to = "pixel",
    names_prefix = "mm_per_day_acre_pixel_",
    values_to = "mm_per_day_acre"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))


bh_mm_2020_per_pixel_area_long <-bh_irrigation_2020_sums_mm_per_day_per_pixel_area%>%
  pivot_longer(
    cols = starts_with("mm_per_day_pixel_area_"),  # Pivot all columns that start with "mm_per_day_pixel_area_"
    names_to = "pixel",                            # Name for the pixel column
    names_prefix = "mm_per_day_pixel_area_",       # Remove the prefix when pivoting
    values_to = "mm_per_day_per_pixel_area"                       # Name for the values column
  ) %>%
  mutate(pixel = gsub("_.*", "", pixel))           # Clean up pixel names if needed


# 4. Merge all three long-format data frames by pixel and year
combined_data_bh_2020 <- bh_gallon_long_2020 %>%
  left_join(bh_hectare_long_2020, by = c("pixel", "year")) %>%
  left_join(bh_acre_long_2020, by = c("pixel", "year"))%>%
  left_join(bh_mm_2020_per_pixel_area_long, by = c("pixel", "year"))

combined_data_bh_2020_pixel<-combined_data_bh_2020%>%
  mutate(
    BH_Block = case_when(
      pixel == 13 ~ "B1R1 NE",
      pixel == 4  ~ "B1R1 NW",
      pixel == 15 ~ "B1R1 SE",
      pixel == 6  ~ "B1R1 SW",
      pixel == 53 ~ "B1R3 NE",
      pixel == 44 ~ "B1R3 NW",
      pixel == 55 ~ "B1R3 SE",
      pixel == 46 ~ "B1R3 SW",
      pixel == 67 ~ "B1R4 NE",
      pixel == 58 ~ "B1R4 NW",
      pixel == 69 ~ "B1R4 SE",
      pixel == 60 ~ "B1R4 SW",
      pixel == 23 ~ "B2R1 NE",
      pixel == 14 ~ "B2R1 NW",
      pixel == 25 ~ "B2R1 SE",
      pixel == 16 ~ "B2R1 SW",
      pixel == 33 ~ "B2R2 NE",
      pixel == 24 ~ "B2R2 NW",
      pixel == 35 ~ "B2R2 SE",
      pixel == 26 ~ "B2R2 SW",
      pixel == 48 ~ "B2R3 NE",
      pixel == 47 ~ "B2R3 NW",
      pixel == 50 ~ "B2R3 SE",
      pixel == 49 ~ "B2R3 SW",
      pixel == 27 ~ "B3R1 NE",
      pixel == 18 ~ "B3R1 NW",
      pixel == 29 ~ "B3R1 SE",
      pixel == 20 ~ "B3R1 SW",
      pixel == 43 ~ "B3R2 NE",
      pixel == 34 ~ "B3R2 NW",
      pixel == 45 ~ "B3R2 SE",
      pixel == 36 ~ "B3R2 SW",
      pixel == 63 ~ "B3R3 NE",
      pixel == 54 ~ "B3R3 NW",
      pixel == 65 ~ "B3R3 SE",
      pixel == 56 ~ "B3R3 SW"
    ),
    treatment = case_when(
      pixel %in% c(13, 4, 15, 6, 53, 44, 55, 46, 67, 58, 69, 60) ~ 1,
      pixel %in% c(23, 14, 25, 16, 33, 24, 35, 26, 48, 47, 50, 49) ~ 2,
      pixel %in% c(27, 18, 29, 20, 43, 34, 45, 36, 63, 54, 65, 56) ~ 3
    )
  )



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

bh_irrigation_2021_complete_season <- bind_rows(
  columns_to_select_treatment1,
  columns_to_select_treatment2,
  columns_to_select_treatment3
)


irr<-bh_irrigation_2021_complete_season

irr<-irr%>%
  mutate(hours = Row_Average)


irr<-irr%>%
  filter(!is.na(hours))

str(irr$Date)

str(irr)
irr<-irr%>%
  mutate(gal_acre_day_irrigation =(hours*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_4 =(Pixel_4*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_6 =(Pixel_6*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_13 =(Pixel_13*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_15 =(Pixel_15*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_44 =(Pixel_44*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_46 =(Pixel_46*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_53 =(Pixel_53*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_55 =(Pixel_55*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_58 =(Pixel_58*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_60 =(Pixel_60*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_67 =(Pixel_67*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_69 =(Pixel_69*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_14 =(Pixel_14*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_16 =(Pixel_16*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_23 =(Pixel_23*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_25 =(Pixel_25*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_24 =(Pixel_24*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_26 =(Pixel_26*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_33 =(Pixel_33*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_35 =(Pixel_35*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_47 =(Pixel_47*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_48 =(Pixel_48*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_49 =(Pixel_49*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_50 =(Pixel_50*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_34 =(Pixel_34*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_36 =(Pixel_36*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_43 =(Pixel_43*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_45 =(Pixel_45*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_54 =(Pixel_54*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_56 =(Pixel_56*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_63 =(Pixel_63*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_65 =(Pixel_65*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_18 =(Pixel_18*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_20 =(Pixel_20*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_27 =(Pixel_27*573.5885))%>%
  mutate(gal_acre_day_irrigation_pixel_29 =(Pixel_29*573.5885))



bh_irrigation_2021_per_pixel <- irr %>%
  mutate(mm_per_day_acre = (gal_acre_day_irrigation / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_34 = (gal_acre_day_irrigation_pixel_34 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_55 = (gal_acre_day_irrigation_pixel_55 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_67 = (gal_acre_day_irrigation_pixel_67 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_23 = (gal_acre_day_irrigation_pixel_23 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_35 = (gal_acre_day_irrigation_pixel_35 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_48 = (gal_acre_day_irrigation_pixel_48 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_27 = (gal_acre_day_irrigation_pixel_27 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_45 = (gal_acre_day_irrigation_pixel_45 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_54 = (gal_acre_day_irrigation_pixel_54 / 1069.10429702)) %>%
  # Add the missing pixel columns below
  mutate(mm_per_day_acre_pixel_4  = (gal_acre_day_irrigation_pixel_4  / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_6  = (gal_acre_day_irrigation_pixel_6  / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_13 = (gal_acre_day_irrigation_pixel_13 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_15 = (gal_acre_day_irrigation_pixel_15 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_44 = (gal_acre_day_irrigation_pixel_44 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_46 = (gal_acre_day_irrigation_pixel_46 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_53 = (gal_acre_day_irrigation_pixel_53 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_58 = (gal_acre_day_irrigation_pixel_58 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_60 = (gal_acre_day_irrigation_pixel_60 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_69 = (gal_acre_day_irrigation_pixel_69 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_14 = (gal_acre_day_irrigation_pixel_14 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_16 = (gal_acre_day_irrigation_pixel_16 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_25 = (gal_acre_day_irrigation_pixel_25 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_24 = (gal_acre_day_irrigation_pixel_24 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_26 = (gal_acre_day_irrigation_pixel_26 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_33 = (gal_acre_day_irrigation_pixel_33 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_36 = (gal_acre_day_irrigation_pixel_36 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_43 = (gal_acre_day_irrigation_pixel_43 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_56 = (gal_acre_day_irrigation_pixel_56 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_63 = (gal_acre_day_irrigation_pixel_63 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_65 = (gal_acre_day_irrigation_pixel_65 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_18 = (gal_acre_day_irrigation_pixel_18 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_20 = (gal_acre_day_irrigation_pixel_20 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_29 = (gal_acre_day_irrigation_pixel_29 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_47 = (gal_acre_day_irrigation_pixel_47 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_49 = (gal_acre_day_irrigation_pixel_49 / 1069.10429702)) %>%
  mutate(mm_per_day_acre_pixel_50 = (gal_acre_day_irrigation_pixel_50 / 1069.10429702))



bh_irrigation_2021_per_pixel <- bh_irrigation_2021_per_pixel%>%
  mutate(mm_per_day_hectare = (mm_per_day_acre *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_34 = (mm_per_day_acre_pixel_34 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_55 = (mm_per_day_acre_pixel_55 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_67 = (mm_per_day_acre_pixel_67 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_23 = (mm_per_day_acre_pixel_23 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_35 = (mm_per_day_acre_pixel_35 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_48 = (mm_per_day_acre_pixel_48 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_27 = (mm_per_day_acre_pixel_27 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_45 = (mm_per_day_acre_pixel_45 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_54 = (mm_per_day_acre_pixel_54 *2.47105)) %>%
  # Add the missing pixel columns below
  mutate(mm_per_day_hectare_pixel_4  = (mm_per_day_acre_pixel_4  *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_6  = (mm_per_day_acre_pixel_6  *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_13 = (mm_per_day_acre_pixel_13 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_15 = (mm_per_day_acre_pixel_15 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_44 = (mm_per_day_acre_pixel_44 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_46 = (mm_per_day_acre_pixel_46 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_53 = (mm_per_day_acre_pixel_53 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_58 = (mm_per_day_acre_pixel_58 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_60 = (mm_per_day_acre_pixel_60 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_69 = (mm_per_day_acre_pixel_69 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_14 = (mm_per_day_acre_pixel_14 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_16 = (mm_per_day_acre_pixel_16 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_25 = (mm_per_day_acre_pixel_25 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_24 = (mm_per_day_acre_pixel_24 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_26 = (mm_per_day_acre_pixel_26 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_33 = (mm_per_day_acre_pixel_33 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_36 = (mm_per_day_acre_pixel_36 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_43 = (mm_per_day_acre_pixel_43 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_56 = (mm_per_day_acre_pixel_56 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_63 = (mm_per_day_acre_pixel_63 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_65 = (mm_per_day_acre_pixel_65 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_18 = (mm_per_day_acre_pixel_18 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_20 = (mm_per_day_acre_pixel_20 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_29 = (mm_per_day_acre_pixel_29 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_47 = (mm_per_day_acre_pixel_47 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_49 = (mm_per_day_acre_pixel_49 *2.47105)) %>%
  mutate(mm_per_day_hectare_pixel_50 = (mm_per_day_acre_pixel_50 *2.47105))

bh_irrigation_2021_per_pixel <- bh_irrigation_2021_per_pixel%>%
  mutate(mm_per_day_pixel_area = (mm_per_day_acre /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_34 = (mm_per_day_acre_pixel_34 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_55 = (mm_per_day_acre_pixel_55 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_67 = (mm_per_day_acre_pixel_67 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_23 = (mm_per_day_acre_pixel_23 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_35 = (mm_per_day_acre_pixel_35 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_48 = (mm_per_day_acre_pixel_48 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_27 = (mm_per_day_acre_pixel_27 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_45 = (mm_per_day_acre_pixel_45 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_54 = (mm_per_day_acre_pixel_54 /4046.86*900)) %>%
  # Add the missing pixel columns below
  mutate(mm_per_day_pixel_area_4  = (mm_per_day_acre_pixel_4  /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_6  = (mm_per_day_acre_pixel_6  /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_13 = (mm_per_day_acre_pixel_13 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_15 = (mm_per_day_acre_pixel_15 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_44 = (mm_per_day_acre_pixel_44 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_46 = (mm_per_day_acre_pixel_46 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_53 = (mm_per_day_acre_pixel_53 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_58 = (mm_per_day_acre_pixel_58 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_60 = (mm_per_day_acre_pixel_60 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_69 = (mm_per_day_acre_pixel_69 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_14 = (mm_per_day_acre_pixel_14 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_16 = (mm_per_day_acre_pixel_16 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_25 = (mm_per_day_acre_pixel_25 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_24 = (mm_per_day_acre_pixel_24 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_26 = (mm_per_day_acre_pixel_26 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_33 = (mm_per_day_acre_pixel_33 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_36 = (mm_per_day_acre_pixel_36 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_43 = (mm_per_day_acre_pixel_43 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_56 = (mm_per_day_acre_pixel_56 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_63 = (mm_per_day_acre_pixel_63 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_65 = (mm_per_day_acre_pixel_65 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_18 = (mm_per_day_acre_pixel_18 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_20 = (mm_per_day_acre_pixel_20 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_29 = (mm_per_day_acre_pixel_29 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_47 = (mm_per_day_acre_pixel_47 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_49 = (mm_per_day_acre_pixel_49 /4046.86*900)) %>%
  mutate(mm_per_day_pixel_area_50 = (mm_per_day_acre_pixel_50 /4046.86*900))

bh_irrigation_2021_per_pixel<-bh_irrigation_2021_per_pixel%>%
  filter(Date<"2021-09-16")

bh_irrigation_2021_per_pixel_mm_per_acre <- bh_irrigation_2021_per_pixel %>%
  select(Date, starts_with("mm_per_day_acre_pixel"))


str(bh_irrigation_2021_per_pixel_mm_per_acre)

bh_irrigation_2021_sums_mm_per_acre <- bh_irrigation_2021_per_pixel_mm_per_acre %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2021_sums_mm_per_acre$year<-2021

str(bh_irrigation_2021_per_pixel)
bh_irrigation_2021_per_pixel_gallons_per_acre <- bh_irrigation_2021_per_pixel %>%
  select(Date, starts_with("gal_acre_day_irrigation_pixel"))


str(bh_irrigation_2021_per_pixel_gallons_per_acre)

bh_irrigation_2021_sums_gallons_per_acre <- bh_irrigation_2021_per_pixel_gallons_per_acre %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2021_sums_gallons_per_acre$year<-2021

bh_irrigation_2021_per_pixel_mm_per_hectare<- bh_irrigation_2021_per_pixel %>%
  select(Date, starts_with("mm_per_day_hectare_"))


str(bh_irrigation_2021_per_pixel_mm_per_hectare)

bh_irrigation_2021_sums_mm_per_hectare <- bh_irrigation_2021_per_pixel_mm_per_hectare %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2021_sums_mm_per_hectare$year<-2021

str(bh_irrigation_2021_per_pixel)
bh_irrigation_2021_mm_per_day_per_pixel_area<- bh_irrigation_2021_per_pixel %>%
  select(Date, starts_with("mm_per_day_pixel_area_"))


str(bh_irrigation_2021_mm_per_day_per_pixel_area)

bh_irrigation_2021_sums_mm_per_day_per_pixel_area <- bh_irrigation_2021_mm_per_day_per_pixel_area %>%
  summarise_at(vars(-Date), sum, na.rm = TRUE)

# View the result
bh_irrigation_2021_sums_mm_per_day_per_pixel_area$year<-2021

# 1. Convert the gallon per acre data to long format
bh_gallon_long_2021 <- bh_irrigation_2021_sums_gallons_per_acre %>%
  pivot_longer(
    cols = starts_with("gal_acre_day_irrigation_pixel_"),
    names_to = "pixel",
    names_prefix = "gal_acre_day_irrigation_pixel_",
    values_to = "gallon_per_acre"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))

# 2. Convert the mm per hectare data to long format
bh_hectare_long_2021 <- bh_irrigation_2021_sums_mm_per_hectare %>%
  pivot_longer(
    cols = starts_with("mm_per_day_hectare_pixel_"),
    names_to = "pixel",
    names_prefix = "mm_per_day_hectare_pixel_",
    values_to = "mm_per_day_hectare"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))



# 3. Convert the mm per acre data to long format
bh_acre_long_2021 <-bh_irrigation_2021_sums_mm_per_acre %>%
  pivot_longer(
    cols = starts_with("mm_per_day_acre_pixel_"),
    names_to = "pixel",
    names_prefix = "mm_per_day_acre_pixel_",
    values_to = "mm_per_day_acre"
  )%>%
  mutate(pixel = gsub("_.*", "", pixel))


bh_mm_2021_per_pixel_area_long <-bh_irrigation_2021_sums_mm_per_day_per_pixel_area%>%
  pivot_longer(
    cols = starts_with("mm_per_day_pixel_area_"),  # Pivot all columns that start with "mm_per_day_pixel_area_"
    names_to = "pixel",                            # Name for the pixel column
    names_prefix = "mm_per_day_pixel_area_",       # Remove the prefix when pivoting
    values_to = "mm_per_day_per_pixel_area"                       # Name for the values column
  ) %>%
  mutate(pixel = gsub("_.*", "", pixel))           # Clean up pixel names if needed



# 4. Merge all three long-format data frames by pixel and year
combined_data_bh_2021 <- bh_gallon_long_2021 %>%
  left_join(bh_hectare_long_2021, by = c("pixel", "year")) %>%
  left_join(bh_acre_long_2021, by = c("pixel", "year"))%>%
  left_join(bh_mm_2021_per_pixel_area_long, by = c("pixel", "year"))

combined_data_bh_2021_pixel<-combined_data_bh_2021%>%
  mutate(
    BH_Block = case_when(
      pixel == 13 ~ "B1R1 NE",
      pixel == 4  ~ "B1R1 NW",
      pixel == 15 ~ "B1R1 SE",
      pixel == 6  ~ "B1R1 SW",
      pixel == 53 ~ "B1R3 NE",
      pixel == 44 ~ "B1R3 NW",
      pixel == 55 ~ "B1R3 SE",
      pixel == 46 ~ "B1R3 SW",
      pixel == 67 ~ "B1R4 NE",
      pixel == 58 ~ "B1R4 NW",
      pixel == 69 ~ "B1R4 SE",
      pixel == 60 ~ "B1R4 SW",
      pixel == 23 ~ "B2R1 NE",
      pixel == 14 ~ "B2R1 NW",
      pixel == 25 ~ "B2R1 SE",
      pixel == 16 ~ "B2R1 SW",
      pixel == 33 ~ "B2R2 NE",
      pixel == 24 ~ "B2R2 NW",
      pixel == 35 ~ "B2R2 SE",
      pixel == 26 ~ "B2R2 SW",
      pixel == 48 ~ "B2R3 NE",
      pixel == 47 ~ "B2R3 NW",
      pixel == 50 ~ "B2R3 SE",
      pixel == 49 ~ "B2R3 SW",
      pixel == 27 ~ "B3R1 NE",
      pixel == 18 ~ "B3R1 NW",
      pixel == 29 ~ "B3R1 SE",
      pixel == 20 ~ "B3R1 SW",
      pixel == 43 ~ "B3R2 NE",
      pixel == 34 ~ "B3R2 NW",
      pixel == 45 ~ "B3R2 SE",
      pixel == 36 ~ "B3R2 SW",
      pixel == 63 ~ "B3R3 NE",
      pixel == 54 ~ "B3R3 NW",
      pixel == 65 ~ "B3R3 SE",
      pixel == 56 ~ "B3R3 SW"
    ),
    treatment = case_when(
      pixel %in% c(13, 4, 15, 6, 53, 44, 55, 46, 67, 58, 69, 60) ~ 1,
      pixel %in% c(23, 14, 25, 16, 33, 24, 35, 26, 48, 47, 50, 49) ~ 2,
      pixel %in% c(27, 18, 29, 20, 43, 34, 45, 36, 63, 54, 65, 56) ~ 3
    )
  )



str(combined_data_bh_2021_pixel)
str(combined_data_bh_2020_pixel)
str(combined_data_bh_2019_pixel)

Total_irrigation_bh_2021 <- combined_data_bh_2021_pixel %>%
  group_by(treatment) %>%
  summarise(
    avg_gallon_per_acre = mean(gallon_per_acre, na.rm = TRUE),
    avg_mm_per_day_acre = mean(mm_per_day_acre, na.rm = TRUE),
    avg_mm_per_day_hectare = mean(mm_per_day_hectare, na.rm = TRUE),
    avg_mm_per_day_pixel_area = mean(mm_per_day_per_pixel_area, na.rm = TRUE)
  ) %>%
  ungroup()

Total_irrigation_bh_2021$year<-2021


Total_irrigation_bh_2020 <- combined_data_bh_2020_pixel %>%
  group_by(treatment) %>%
  summarise(
    avg_gallon_per_acre = mean(gallon_per_acre, na.rm = TRUE),
    avg_mm_per_day_acre = mean(mm_per_day_acre, na.rm = TRUE),
    avg_mm_per_day_hectare = mean(mm_per_day_hectare, na.rm = TRUE),
    avg_mm_per_day_pixel_area = mean(mm_per_day_per_pixel_area, na.rm = TRUE)
  ) %>%
  ungroup()

Total_irrigation_bh_2020$year<-2020


Total_irrigation_bh_2019 <- combined_data_bh_2019_pixel %>%
  group_by(treatment) %>%
  summarise(
    avg_gallon_per_acre = mean(gallon_per_acre, na.rm = TRUE),
    avg_mm_per_day_acre = mean(mm_per_day_acre, na.rm = TRUE),
    avg_mm_per_day_hectare = mean(mm_per_day_hectare, na.rm = TRUE),
    avg_mm_per_day_pixel_area = mean(mm_per_day_per_pixel_area, na.rm = TRUE)
  ) %>%
  ungroup()

Total_irrigation_bh_2019$year<-2019

Total_irrigation_bh_all_years<-rbind(Total_irrigation_bh_2019,Total_irrigation_bh_2020,Total_irrigation_bh_2021)

write.csv(combined_data_bh_2021_pixel,"data_output/combined_data_bh_2021_pixel.csv")
write.csv(combined_data_bh_2020_pixel,"data_output/combined_data_bh_2020_pixel.csv")
write.csv(combined_data_bh_2019_pixel,"data_output/combined_data_bh_2019_pixel.csv")
write.csv(Total_irrigation_bh_all_years,"data_output/Total_irrigation_bh_all_years_at_harvest.csv")


result_sublocks_one_way_anova_year_factor_weight_kg<-result_sublocks_one_way_anova_year_factor_weight_kg%>%
  mutate(Yiekd_kg_per_vine = Mean_sem)%>%
  mutate(BH_Block =block_sublock)%>%
  select(year, BH_Block,Yiekd_kg_per_vine)


result_sublocks_one_way_anova_year_factor_gr_cluster<-result_sublocks_one_way_anova_year_factor_gr_cluster%>%
  mutate(gr_clusters = Mean_sem)%>%
  mutate(BH_Block =block_sublock)%>%
  select(year, BH_Block,gr_clusters)


result_sublocks_one_way_anova_year_factor_total.clusters<-result_sublocks_one_way_anova_year_factor_total.clusters%>%
  mutate(total_clusters = Mean_sem)%>%
  mutate(BH_Block =block_sublock)%>%
  select(year, BH_Block,total_clusters)

str(result_sublocks_one_way_anova_year_factor_weight_kg)
str(result_sublocks_one_way_anova_year_factor_gr_cluster)
str(result_sublocks_one_way_anova_year_factor_total.clusters)
str(combined_data_bh_2021_pixel)
str(combined_data_bh_2020_pixel)
str(combined_data_bh_2019_pixel)


# Step 1: Ensure the 'year' columns are characters
result_sublocks_one_way_anova_year_factor_weight_kg$year <- as.character(result_sublocks_one_way_anova_year_factor_weight_kg$year)
result_sublocks_one_way_anova_year_factor_gr_cluster$year <- as.character(result_sublocks_one_way_anova_year_factor_gr_cluster$year)
result_sublocks_one_way_anova_year_factor_total.clusters$year <- as.character(result_sublocks_one_way_anova_year_factor_total.clusters$year)
combined_data_bh_2021_pixel$year <- as.character(combined_data_bh_2021_pixel$year)
combined_data_bh_2020_pixel$year <- as.character(combined_data_bh_2020_pixel$year)
combined_data_bh_2019_pixel$year <- as.character(combined_data_bh_2019_pixel$year)

# Step 2: Rename columns for clarity (same as before)

str(result_sublocks_one_way_anova_year_factor_weight_kg)
result_sublocks_one_way_anova_year_factor_weight_kg <- result_sublocks_one_way_anova_year_factor_weight_kg %>%
  rename(Yield_kg_per_vine = Yiekd_kg_per_vine)

str(result_sublocks_one_way_anova_year_factor_gr_cluster)
result_sublocks_one_way_anova_year_factor_gr_cluster <- result_sublocks_one_way_anova_year_factor_gr_cluster %>%
  rename(Cluster_gr = gr_clusters)

result_sublocks_one_way_anova_year_factor_total.clusters <- result_sublocks_one_way_anova_year_factor_total.clusters %>%
  rename(Total_clusters = total_clusters)

# Step 3: Merge data and handle .x and .y columns using `coalesce`
combined_all_data <- result_sublocks_one_way_anova_year_factor_weight_kg %>%
  full_join(result_sublocks_one_way_anova_year_factor_gr_cluster, by = c("year", "BH_Block")) %>%
  full_join(result_sublocks_one_way_anova_year_factor_total.clusters, by = c("year", "BH_Block")) %>%
  full_join(combined_data_bh_2021_pixel, by = c("year", "BH_Block")) %>%
  full_join(combined_data_bh_2020_pixel, by = c("year", "BH_Block"), suffix = c("", "_2020")) %>%
  full_join(combined_data_bh_2019_pixel, by = c("year", "BH_Block"), suffix = c("", "_2019")) %>%
  mutate(
    # Use `coalesce()` to handle conflicts and ensure single columns
    pixel = coalesce(pixel, pixel_2020, pixel_2019),
    gallon_per_acre = coalesce(gallon_per_acre, gallon_per_acre_2020, gallon_per_acre_2019),
    mm_per_day_hectare = coalesce(mm_per_day_hectare, mm_per_day_hectare_2020, mm_per_day_hectare_2019),
    mm_per_day_acre = coalesce(mm_per_day_acre, mm_per_day_acre_2020, mm_per_day_acre_2019),
    mm_per_day_per_pixel_area = coalesce(mm_per_day_per_pixel_area, mm_per_day_per_pixel_area_2020, mm_per_day_per_pixel_area_2019),
    treatment = coalesce(treatment, treatment_2020, treatment_2019)
  ) %>%
  # Step 4: Remove redundant columns
  select(-starts_with("pixel_2020"), -starts_with("gallon_per_acre_2020"), -starts_with("mm_per_day_hectare_2020"), 
         -starts_with("mm_per_day_acre_2020"), -starts_with("treatment_2020"),-starts_with("mm_per_day_per_pixel_area_2020"),
         -starts_with("pixel_2019"), -starts_with("gallon_per_acre_2019"), -starts_with("mm_per_day_hectare_2019"), 
         -starts_with("mm_per_day_acre_2019"), -starts_with("treatment_2019"),-starts_with("mm_per_day_per_pixel_area_2019"))

# Step 5: View the merged data without .x or .y columns
View(combined_all_data)



write.csv(combined_all_data,"data_output/total_irrigation_and_yield_components_pixels.csv")

###### Average T air max BH 2019-2020-2021#####

Tair_max_bh_2019<-read.csv("data/bh_tair_2019_all_year.csv")
Tair_max_bh_2020<-read.csv("data/bh_tair_2020_all_year.csv")
Tair_max_bh_2021<-read.csv("data/bh_tair_2021_all_year.csv")

str(Tair_max_bh_2019)
Tair_max_bh_2019$date<-mdy(Tair_max_bh_2019$date)


Tair_max_bh_2020_average <- Tair_max_bh_2020 %>%
  group_by(DOY) %>%
  summarise(tair_max = max(Tair_C_hmp))
Tair_max_bh_2020_average$date <- as.Date(paste("2020", Tair_max_bh_2020_average$DOY), format = "%Y %j")

str(Tair_max_bh_2020_average)


Tair_max_bh_2021_average <- Tair_max_bh_2021 %>%
  group_by(DOY) %>%
  summarise(tair_max = max(Tair_C_hmp))

Tair_max_bh_2021_average$date <- as.Date(paste("2021", Tair_max_bh_2021_average$DOY), format = "%Y %j")

str(Tair_max_bh_2021_average)

str(Tair_max_bh_2019)

Tair_max_bh_2020_average_april_to_oct<-Tair_max_bh_2020_average %>%
  filter(date >= "2020-04-01") %>%
  filter(date <= "2020-10-31")

Tair_max_bh_2021_average_april_to_oct<-Tair_max_bh_2021_average %>%
  filter(date >= "2021-04-01") %>%
  filter(date <= "2021-10-31")

Tair_max_bh_2019_april_to_oct<-Tair_max_bh_2019 %>%
  filter(date >= "2019-04-01") %>%
  filter(date <= "2019-10-31")

str(Tair_max_bh_2019_april_to_oct)

library(dplyr)

# Calculate the average Tair_max
average_Tair_max_2020 <- Tair_max_bh_2020_average_april_to_oct %>%
  summarise(average_Tair_max = mean(tair_max, na.rm = TRUE))

average_Tair_max_2020$year<-2020

average_Tair_max_2021 <- Tair_max_bh_2021_average_april_to_oct %>%
  summarise(average_Tair_max = mean(tair_max, na.rm = TRUE))

average_Tair_max_2021$year<-2021

str(Tair_max_bh_2019_april_to_oct)
average_Tair_max_2019 <- Tair_max_bh_2019_april_to_oct %>%
  summarise(average_Tair_max = mean(Tair_max, na.rm = TRUE))

average_Tair_max_2019$year<-2019

str(average_Tair_max_2019)
str(average_Tair_max_2020)
str(average_Tair_max_2021)

average_Tair_max_all_years<-rbind(average_Tair_max_2019, average_Tair_max_2020, average_Tair_max_2021)


write.csv(average_Tair_max_all_years,"data_output/average_Tair_max_all_years.csv")



###### GDD FOR THE THREE YEARS ######

Tair_bh_2019<-read.csv("data/BH_air_temp_2019.csv")
Tair_bh_2020<-read.csv("data/bh_tair_2020_all_year.csv")
Tair_bh_2021<-read.csv("data/bh_tair_2021_all_year.csv")

Tair_bh_2020_daily_max_min <- Tair_bh_2020 %>%
  group_by(DOY) %>%
  summarise(tair_max = max(Tair_C_hmp), tair_min = min(Tair_C_hmp))
 
Tair_bh_2020_daily_max_min$date <- as.Date(paste("2020", Tair_bh_2020_daily_max_min$DOY), format = "%Y %j")

str(Tair_bh_2020_daily_max_min)


Tair_bh_2021_daily_max_min <- Tair_bh_2021 %>%
  group_by(DOY) %>%
  summarise(tair_max = max(Tair_C_hmp), tair_min = min(Tair_C_hmp))

Tair_bh_2021_daily_max_min$date <- as.Date(paste("2021", Tair_bh_2021_daily_max_min$DOY), format = "%Y %j")

str(Tair_bh_2021_daily_max_min)

str(Tair_bh_2019)

Tair_bh_2019_daily_max_min<-Tair_bh_2019

Tair_bh_2020_daily_max_min_april_to_oct<-Tair_bh_2020_daily_max_min %>%
  filter(date >= "2020-04-01") %>%
  filter(date <= "2020-10-31")

Tair_bh_2021_daily_max_min_april_to_oct<-Tair_bh_2021_daily_max_min %>%
  filter(date >= "2021-04-01") %>%
  filter(date <= "2021-10-31")

Tair_bh_2019_daily_max_min$date<-mdy(Tair_bh_2019_daily_max_min$date)

str(Tair_bh_2019_daily_max_min)
Tair_bh_2019_daily_max_min_april_to_oct<-Tair_bh_2019_daily_max_min %>%
  filter(date >= "2019-04-01") %>%
  filter(date <= "2019-10-31")

str(Tair_bh_2019_daily_max_min_april_to_oct)
str(Tair_bh_2020_daily_max_min_april_to_oct)
str(Tair_bh_2021_daily_max_min_april_to_oct)

sum(is.na(Tair_bh_2019_daily_max_min_april_to_oct$Tair_max))
sum(is.na(Tair_bh_2020_daily_max_min_april_to_oct$tair_max))
sum(is.na(Tair_bh_2021_daily_max_min_april_to_oct$tair_max))


Tair_bh_2020_daily_max_min_april_to_oct <- Tair_bh_2020_daily_max_min_april_to_oct %>% mutate(Tair_mean = ((Tair_bh_2020_daily_max_min_april_to_oct$tair_max + Tair_bh_2020_daily_max_min_april_to_oct$tair_min)/2))

Tair_bh_2021_daily_max_min_april_to_oct <- Tair_bh_2021_daily_max_min_april_to_oct %>% mutate(Tair_mean = ((Tair_bh_2021_daily_max_min_april_to_oct$tair_max + Tair_bh_2021_daily_max_min_april_to_oct$tair_min)/2))


str(Tair_bh_2019_daily_max_min_april_to_oct)
str(Tair_bh_2020_daily_max_min_april_to_oct)
str(Tair_bh_2021_daily_max_min_april_to_oct)

Tair_bh_2019_daily_max_min_april_to_oct<-Tair_bh_2019_daily_max_min_april_to_oct%>%
  mutate(Tair_mean_f = ((Tair_mean*9/5)+32))

Tair_bh_2020_daily_max_min_april_to_oct<-Tair_bh_2020_daily_max_min_april_to_oct%>%
  mutate(Tair_mean_f = ((Tair_mean*9/5)+32))

Tair_bh_2021_daily_max_min_april_to_oct<-Tair_bh_2021_daily_max_min_april_to_oct%>%
  mutate(Tair_mean_f = ((Tair_mean*9/5)+32))

# Take the full season version of this df and calculate growing degree days
BH_2019_Daily <- Tair_bh_2019_daily_max_min_april_to_oct %>% mutate(GDD = Tair_mean_f-50)
BH_2019_Daily$GDD <- ifelse(BH_2019_Daily$GDD<0,0,BH_2019_Daily$GDD)
BH_2019_Daily <- BH_2019_Daily %>% mutate(GDD_Sum = cumsum(GDD))
BH_2019_Daily <- BH_2019_Daily %>% mutate(GDDc = ((5/9)*(GDD_Sum-32)))
BH_2019_Daily$GDDc <- ifelse(BH_2019_Daily$GDDc<0,0,BH_2019_Daily$GDDc)



BH_2020_Daily <- Tair_bh_2020_daily_max_min_april_to_oct %>% mutate(GDD = Tair_mean_f-50)
BH_2020_Daily$GDD <- ifelse(BH_2020_Daily$GDD<0,0,BH_2020_Daily$GDD)
BH_2020_Daily <- BH_2020_Daily %>% mutate(GDD_Sum = cumsum(GDD))
BH_2020_Daily <- BH_2020_Daily %>% mutate(GDDc = ((5/9)*(GDD_Sum-32)))
BH_2020_Daily$GDDc <- ifelse(BH_2020_Daily$GDDc<0,0,BH_2020_Daily$GDDc)


BH_2021_Daily <- Tair_bh_2021_daily_max_min_april_to_oct %>% mutate(GDD = Tair_mean_f-50)
BH_2021_Daily$GDD <- ifelse(BH_2021_Daily$GDD<0,0,BH_2021_Daily$GDD)
BH_2021_Daily <- BH_2021_Daily %>% mutate(GDD_Sum = cumsum(GDD))
BH_2021_Daily <- BH_2021_Daily %>% mutate(GDDc = ((5/9)*(GDD_Sum-32)))
BH_2021_Daily$GDDc <- ifelse(BH_2021_Daily$GDDc<0,0,BH_2021_Daily$GDDc)

# Capture the last row of GDDc for each year
BH_GDD <- tibble(
  year = c(2019, 2020, 2021),
  GDDc = c(tail(BH_2019_Daily$GDDc, 1), 
           tail(BH_2020_Daily$GDDc, 1), 
           tail(BH_2021_Daily$GDDc, 1))
)

write.csv(BH_GDD,"data_output/BH_GDD.csv")

####Total irrigation figure with precipitation######
total_irrigation_bh_all_years_at_harvest<-combined_all_data%>%
  filter(!is.na(treatment))%>%
  select(year, BH_Block, pixel,mm_per_day_hectare, treatment)


total_irrigation_bh_all_years_at_harvest_sums <- total_irrigation_bh_all_years_at_harvest %>%
  group_by(year, treatment) %>%
  summarise(
    Total_mm_per_day_per_hectare = sum(mm_per_day_hectare, na.rm = TRUE) / n_distinct(BH_Block)
  )

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

Total_irrigation_all_years_bh <- left_join(total_precipitation_per_year_bh, total_irrigation_bh_all_years_at_harvest_sums, by = c("Year" = "year"))

str(Total_irrigation_all_years_bh)



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
colnames(df_adjusted)
colnames(Total_irrigation_all_years_bh)


Total_irrigation_all_years_bh_oct_oct <- left_join(df_adjusted, Total_irrigation_all_years_bh, by = c("Year"))

str(Total_irrigation_all_years_bh_oct_oct)

write.csv(Total_irrigation_all_years_bh_oct_oct,"data_output/Total_irrigation_all_years_bh_oct_oct.csv")

combined_df <- Total_irrigation_all_years_bh_oct_oct %>%
  mutate(treatment = as.factor(treatment)) %>%
  group_by(Year) %>%
  summarise(
    total_rain_mm = total_rain_mm.x,  # Keep only one rain value per year
    treatment_1_irrigation = (Total_mm_per_day_per_hectare[treatment == "1"]),
    treatment_2_irrigation = (Total_mm_per_day_per_hectare[treatment == "2"]),
    treatment_3_irrigation = (Total_mm_per_day_per_hectare[treatment == "3"])
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