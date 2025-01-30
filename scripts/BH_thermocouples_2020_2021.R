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
library(suncalc)


#####Berry thermocouple analysis ####

read_and_fill_na <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)  # Read CSV without printing column types
  return(df)
}

# Define the file paths
file_paths <- c("data/19170 (B1R1)_Thermocouples_out.csv", 
                "data/19120 (B2R1)_Thermocouples_out.csv",
                "data/18330 (B3R1)_Thermocouples_out.csv",
                "data/18331 (B1R4)_Thermocouples_out.csv",
                "data/18333 (B2R2)_Thermocouples_out.csv",
                "data/18335 (B3R2)_Thermocouples_out.csv",
                "data/19171 (B3R3)_Thermocouples_out.csv",
                "data/3859 (B2R3)_Thermocouples_out.csv",
                "data/17702 (B1R3)_Thermocouples_out.csv")


read_and_fix_types <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  
  if ("Temp_C_TMx(19)" %in% names(df)) {
    df$`Temp_C_TMx(19)` <- as.character(df$`Temp_C_TMx(19)`)
  }
  
  if ("Temp_C_TMx(20)" %in% names(df)) {
    df$`Temp_C_TMx(20)` <- as.character(df$`Temp_C_TMx(20)`)
  }
  
  return(df)
}

berry_thermocouples_BH_stations_2020 <- file_paths %>%
  lapply(read_and_fix_types) %>%
  bind_rows()

str(berry_thermocouples_BH_stations_2020)
# Check the combined data
head(berry_thermocouples_BH_stations_2020)

berry_thermocouple_HWS_2020<-berry_thermocouples_BH_stations_2020

str(berry_thermocouple_HWS_2020)


berry_thermocouple_HWS_2020<-berry_thermocouple_HWS_2020%>%
select(TIMESTAMP, Station, 'Temp_C_Max(1)', 'Temp_C_Max(2)', 'Temp_C_Max(3)', 'Temp_C_Max(4)', 'Temp_C_Max(5)', 'Temp_C_Max(6)', 'Temp_C_Max(7)', 'Temp_C_Max(8)', 'Temp_C_Max(9)', 'Temp_C_Max(10)', 'Temp_C_Max(11)', 'Temp_C_Max(12)', 'Temp_C_Max(13)', 'Temp_C_Max(14)', 'Temp_C_Max(15)', 'Temp_C_Max(16)', 'Temp_C_Max(17)', 'Temp_C_Max(18)', 'Temp_C_Max(19)', 'Temp_C_Max(20)')

str(berry_thermocouple_HWS_2020)

sum(is.na(berry_thermocouple_HWS_2020$`Temp_C_Max(17)`))

berry_thermocouple_HWS_2020$Date <- as.Date(berry_thermocouple_HWS_2020$TIMESTAMP)

# Extract Time
berry_thermocouple_HWS_2020$Time <- format(berry_thermocouple_HWS_2020$TIMESTAMP, format = "%H:%M:%S")

# View the modified dataframe
head(berry_thermocouple_HWS_2020)

# Get unique station values
unique_stations <- unique(berry_thermocouples_BH_stations_2020$Station)

# View the unique station values
print(unique_stations)

berry_thermocouple_HWS_2020_timeseries<-berry_thermocouple_HWS_2020%>%
  mutate(treatment = case_when(
    Station == "17702 (B1R3)" ~ 1,
    Station == "19170 (B1R1)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))


berry_thermocouple_HWS_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-08-13"| Date == "2020-08-16"| Date == "2020-08-22"| Date == "2020-09-04"| Date == "2020-09-07"| Date == "2020-09-08"|Date == "2020-09-12")%>%
  mutate(treatment = case_when(
    Station == "17702 (B1R3)" ~ 1,
    Station == "19170 (B1R1)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))



berry_temp_aug_13_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-08-13")

str(berry_temp_aug_13_2020)

berry_long <- berry_temp_aug_13_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)


berry_long <- berry_long %>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)")) ###Remove thermocouple 13 in B1R4 weird reading 


library(ggplot2)

# Plot Temperature vs. Time for all sensors, separate by Station

  BERRY_TEMP_AUG_13_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
    geom_line() +
    labs(title = "Temperature Over Time for Each Sensor Aug 13",
         x = "Time",
         y = "Temperature (°C)",
         color = "Sensor") +
    scale_color_viridis_d() +   # Using viridis color scale
    facet_wrap(~ Station, scales = "fixed") +  # One plot per station
    theme_minimal() +
    theme(legend.position = "bottom")  # Position legend below the plots
  
  
  
  ggsave("figures/berry_temp_aug_13_2020_BH_removed_weird_thermo.jpg", plot = BERRY_TEMP_AUG_13_2020_BH, width = 20, height = 15)


berry_temp_aug_16_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-08-16")

str(berry_temp_aug_16_2020)

berry_long <- berry_temp_aug_16_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)
berry_long <- berry_long %>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)")) ###Remove thermocouple 13 in B1R4 weird reading 

library(ggplot2)

# Plot Temperature vs. Time for all sensors, separate by Station
BERRY_TEMP_AUG_16_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor)) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots


BERRY_TEMP_AUG_16_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor Aug 16",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots




ggsave("figures/berry_temp_aug_16_2020_BH.jpg", plot = BERRY_TEMP_AUG_16_2020_BH, width = 20, height = 15, dpi = 600)


berry_temp_aug_22_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-08-22")

str(berry_temp_aug_22_2020)

berry_long <- berry_temp_aug_22_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)
berry_long <- berry_long %>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)")) ###Remove thermocouple 13 in B1R4 weird reading 

BERRY_TEMP_AUG_22_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor Aug 22",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots




ggsave("figures/berry_temp_aug_22_2020_BH.jpg", plot = BERRY_TEMP_AUG_22_2020_BH, width = 20, height = 15, dpi = 600)

berry_temp_sep_4_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-09-04")

str(berry_temp_sep_4_2020)

berry_long <- berry_temp_sep_4_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)
berry_long <- berry_long %>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Sensor == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))
  ###Remove thermocouples  weird readings


BERRY_TEMP_sep_4_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor Sep 4",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots




ggsave("figures/berry_temp_sep_4_2020_BH.jpg", plot = BERRY_TEMP_sep_4_2020_BH, width = 20, height = 15, dpi = 600)




berry_temp_sep_7_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-09-07")

str(berry_temp_sep_7_2020)

berry_long <- berry_temp_sep_7_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

berry_long <- berry_long %>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Sensor == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))%>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(7)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(10)" & Station == "17702 (B1R3)"))%>%
  filter(!(Sensor == "Temp_C_Max(18)" & Station == "19171 (B3R3)"))  
  
  ###Remove thermocouples weird readings

BERRY_TEMP_sep_7_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor Sep 7",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots




ggsave("figures/berry_temp_sep_7_2020_BH.jpg", plot = BERRY_TEMP_sep_7_2020_BH, width = 20, height = 15, dpi = 600)


berry_temp_sep_8_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-09-08")

str(berry_temp_sep_8_2020)

berry_long <- berry_temp_sep_8_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

berry_long <- berry_long %>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Sensor == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))%>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(7)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(10)" & Station == "17702 (B1R3)"))%>%
  filter(!(Sensor == "Temp_C_Max(18)" & Station == "19171 (B3R3)"))  

###Remove thermocouples weird readings

BERRY_TEMP_sep_8_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor Sep 7",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots




ggsave("figures/berry_temp_sep_8_2020_BH.jpg", plot = BERRY_TEMP_sep_8_2020_BH, width = 20, height = 15, dpi = 600)


berry_temp_sep_12_2020<-berry_thermocouple_HWS_2020%>%
  filter(Date == "2020-09-12")

str(berry_temp_sep_12_2020)

berry_long <- berry_temp_sep_12_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

berry_long <- berry_long %>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Sensor == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))%>%
  filter(!(Sensor == "Temp_C_Max(13)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(7)" & Station == "19170 (B1R1)"))%>%
  filter(!(Sensor == "Temp_C_Max(10)" & Station == "17702 (B1R3)"))%>%
  filter(!(Sensor == "Temp_C_Max(18)" & Station == "19171 (B3R3)"))%>%
  filter(!(Sensor == "Temp_C_Max(7)" & Station == "18335 (B3R2)"))    

###Remove thermocouples weird readings
BERRY_TEMP_sep_12_2020_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor Sep 12",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots




ggsave("figures/berry_temp_sep_12_2020_BH.jpg", plot = BERRY_TEMP_sep_12_2020_BH, width = 20, height = 15, dpi = 600)

#####Berry thermocouples 2021####
berry_thermocouples_BH_stations_2021<- read.csv("data/Met_TC_BH2021.csv")

str(berry_thermocouples_BH_stations_2021)

berry_thermocouple_HWS_2021<-berry_thermocouples_BH_stations_2021%>%
  select(TIMESTAMP, Station, 'Therm_Couple_1', 'Therm_Couple_2', 'Therm_Couple_3', 'Therm_Couple_4', 'Therm_Couple_5', 'Therm_Couple_6', 'Therm_Couple_7', 'Therm_Couple_8', 'Therm_Couple_9', 'Therm_Couple_10', 'Therm_Couple_11', 'Therm_Couple_12', 'Therm_Couple_13', 'Therm_Couple_14', 'Therm_Couple_15', 'Therm_Couple_16', 'Therm_Couple_17', 'Therm_Couple_18' ,'Therm_Couple_19', 'Therm_Couple_20')

str(berry_thermocouple_HWS_2021)
berry_thermocouple_HWS_2021$TIMESTAMP<-ymd_hms(berry_thermocouple_HWS_2021$TIMESTAMP)

str(berry_thermocouple_HWS_2021$TIMESTAMP)

sum(is.na(berry_thermocouple_HWS_2021$`Therm_Couple_17`))

berry_thermocouple_HWS_2021$Date <- as.Date(berry_thermocouple_HWS_2021$TIMESTAMP)

# Extract Time
berry_thermocouple_HWS_2021$Time <- format(berry_thermocouple_HWS_2021$TIMESTAMP, format = "%H:%M:%S")

# View the modified dataframe
head(berry_thermocouple_HWS_2021)

# Get unique station values
unique_stations <- unique(berry_thermocouples_BH_stations_2021$Station)

# View the unique station values
print(unique_stations)


berry_thermocouple_HWS_2021_timeseries<-berry_thermocouple_HWS_2021%>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station== "B3R3" ~ 3
  ))

berry_thermocouple_HWS_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-06-15"| Date == "2021-06-17"| Date == "2021-06-22"| Date == "2021-07-07"| Date == "2021-07-10"| Date == "2021-07-14"| Date == "2021-09-08"| Date == "2021-09-15")%>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station== "B3R3" ~ 3
  ))



berry_temp_june_15_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-06-15")

str(berry_temp_june_15_2021)

berry_long <- berry_temp_june_15_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple_"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

#berry_long <- berry_long %>%
#  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))

# Plot Temperature vs. Time for all sensors, separate by Station

BERRY_TEMP_JUNE_15_2021_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor June 15 2021",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots



ggsave("figures/berry_temp_june_15_2021_BH.jpg", plot = BERRY_TEMP_JUNE_15_2021_BH, width = 20, height = 15)

berry_temp_june_17_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-06-17")

str(berry_temp_june_17_2021)

berry_long <- berry_temp_june_17_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple_"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

#berry_long <- berry_long %>%
#  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))

# Plot Temperature vs. Time for all sensors, separate by Station

BERRY_TEMP_JUNE_17_2021_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor June 17 2021",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots



ggsave("figures/berry_temp_june_17_2021_BH.jpg", plot = BERRY_TEMP_JUNE_17_2021_BH, width = 20, height = 15)



berry_temp_june_22_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-06-22")

str(berry_temp_june_22_2021)

berry_long <- berry_temp_june_22_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple_"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

#berry_long <- berry_long %>%
#  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))

# Plot Temperature vs. Time for all sensors, separate by Station

BERRY_TEMP_JUNE_22_2021_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor June 22 2021",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots



ggsave("figures/berry_temp_june_22_2021_BH.jpg", plot = BERRY_TEMP_JUNE_22_2021_BH, width = 20, height = 15)




berry_temp_july_07_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-07-07")

str(berry_temp_july_07_2021)

berry_long <- berry_temp_july_07_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple_"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

#berry_long <- berry_long %>%
#  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))

# Plot Temperature vs. Time for all sensors, separate by Station

BERRY_TEMP_july_07_2021_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor July 07 2021",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots



ggsave("figures/berry_temp_july_07_2021_BH.jpg", plot = BERRY_TEMP_july_07_2021_BH, width = 20, height = 15)



berry_temp_july_10_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-07-10")

str(berry_temp_july_10_2021)

berry_long <- berry_temp_july_10_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple_"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

#berry_long <- berry_long %>%
#  filter(!(Sensor == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))

# Plot Temperature vs. Time for all sensors, separate by Station

BERRY_TEMP_july_10_2021_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor July 10 2021",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots



ggsave("figures/berry_temp_july_10_2021_BH.jpg", plot = BERRY_TEMP_july_10_2021_BH, width = 20, height = 15)




berry_temp_july_14_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-07-14")

str(berry_temp_july_14_2021)

berry_long <- berry_temp_july_14_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple_"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)

berry_long <- berry_long %>%
  filter(!(Station == "B3R3"))

# Plot Temperature vs. Time for all sensors, separate by Station

BERRY_TEMP_july_14_2021_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor July 14 2021",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots



ggsave("figures/berry_temp_july_14_2021_BH.jpg", plot = BERRY_TEMP_july_14_2021_BH, width = 20, height = 15)



berry_temp_sep_08_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-09-08")

str(berry_temp_sep_08_2021)

berry_long <- berry_temp_sep_08_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple_"),  # All Temp_C_Max columns
               names_to = "Sensor",              # Name for the new sensor column
               values_to = "Temperature") 

str(berry_long)


#berry_long <- berry_long %>%
#  filter(!(Station == "B3R3"))

# Plot Temperature vs. Time for all sensors, separate by Station

BERRY_TEMP_sep_08_2021_BH<-ggplot(berry_long, aes(x = TIMESTAMP, y = Temperature, color = Sensor, group = interaction(Station, Sensor))) +
  geom_line() +
  labs(title = "Temperature Over Time for Each Sensor July 14 2021",
       x = "Time",
       y = "Temperature (°C)",
       color = "Sensor") +
  scale_color_viridis_d() +   # Using viridis color scale
  facet_wrap(~ Station, scales = "fixed") +  # One plot per station
  theme_minimal() +
  theme(legend.position = "bottom")  # Position legend below the plots



ggsave("figures/berry_temp_sep_08_2021_BH.jpg", plot = BERRY_TEMP_sep_08_2021_BH, width = 20, height = 15)

######Group thermocoupleS by treatment berry temp 2020 ######
######Time series from pre to post HW3 2020 ######

str(berry_thermocouple_HWS_2020_timeseries)

berry_thermocouple_HW3_timeseries<- berry_thermocouple_HWS_2020_timeseries %>%
  filter(Date >= as.Date("2020-08-14") & Date <= as.Date("2020-08-17"))

str(berry_thermocouple_HW3_timeseries)


# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HW3_timeseries %>%
  select(Station, starts_with("Temp_C_Max")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_HW3_timeseries_filtered <- berry_thermocouple_HW3_timeseries %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")


# Check the structure of the filtered dataset
str(berry_thermocouple_HW3_timeseries_filtered)

berry_long <- berry_thermocouple_HW3_timeseries_filtered %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


#berry_long <- berry_long %>%
#  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))


berry_long <- berry_long %>%
  unnest(Temperature)
str(berry_long)


berry_long<-berry_long%>%
  mutate(across(where(is.numeric), ~ replace(., is.nan(.), NA)))%>%
  filter(!is.na(Temperature))
str(berry_long)

# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette


str(result)
# Plotting
berry_temperature_HW3_2020_plot_avg_CI_timeseries<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature  HW3 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1))+ # Rotates x-axis labels by 45 degrees
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(
    date_labels = "%b %d %H:%M",  # Format as "Month Day Hour:Minute" (e.g., "Aug 11 00:00")
    date_breaks = "8 hours"       # Breaks every 8 hours
  )

ggsave("figures/berry_temperature_HW3_2020_plot_avg_CI_timeseries.jpg", plot = berry_temperature_HW3_2020_plot_avg_CI_timeseries, width = 15, height = 7)


str(berry_temp_aug_16_2020)

berry_long <- berry_temp_aug_16_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


berry_long <- berry_long %>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)")) ###Remove thermocouple 13 in B1R4 weird reading 


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_aug_16_HW3_2020_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature August 16 HW3 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_aug_16_HW3_2020_plot_avg_CI.jpg", plot = berry_temperature_aug_16_HW3_2020_plot_avg_CI, width = 8, height = 6)

#####Aug 13 pre HW####
str(berry_temp_aug_13_2020)

berry_long <- berry_temp_aug_13_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


berry_long <- berry_long %>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)")) ###Remove thermocouple 13 in B1R4 weird reading 


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_aug_13_HW3_2020_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature August 13 PRE HW3 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_aug_13_HW3_2020_plot_avg_CI.jpg", plot = berry_temperature_aug_13_HW3_2020_plot_avg_CI, width = 8, height = 6)

######Aug 22 post HW##########


str(berry_temp_aug_22_2020)

berry_long <- berry_temp_aug_22_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


berry_long <- berry_long %>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)")) ###Remove thermocouple 13 in B1R4 weird reading 


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_aug_22_HW3_2020_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature August 22 POST HW3 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_aug_22_HW3_2020_plot_avg_CI.jpg", plot = berry_temperature_aug_22_HW3_2020_plot_avg_CI, width = 8, height = 6)




#####Sep 8 2020 HW4#####

str(berry_thermocouple_HWS_2020)
  str(berry_temp_sep_8_2020)



berry_long <- berry_temp_sep_8_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


berry_long <- berry_long %>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(7)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(10)" & Station == "17702 (B1R3)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(18)" & Station == "19171 (B3R3)"))  

# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_sep_8_HW4_2020_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature Sept 8 HW4 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_sep_8_HW4_2020_plot_avg_CI.jpg", plot = berry_temperature_sep_8_HW4_2020_plot_avg_CI, width = 8, height = 6)


#####Sep 7 2020 HW4#####

str(berry_thermocouple_HWS_2020)
str(berry_temp_sep_7_2020)



berry_long <- berry_temp_sep_7_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


berry_long <- berry_long %>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(7)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(10)" & Station == "17702 (B1R3)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(18)" & Station == "19171 (B3R3)"))  

# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_sep_7_HW4_2020_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature Sept 7 HW4 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_sep_7_HW4_2020_plot_avg_CI.jpg", plot = berry_temperature_sep_7_HW4_2020_plot_avg_CI, width = 8, height = 6)

###Sep 4 PRE HW4 2020######

str(berry_thermocouple_HWS_2020)
str(berry_temp_sep_4_2020)



berry_long <- berry_temp_sep_4_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


berry_long <- berry_long %>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))
###Remove thermocouples  weird readings

# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_sep_4_HW4_2020_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature Sept 4 PRE HW4 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_sep_4_HW4_2020_plot_avg_CI.jpg", plot = berry_temperature_sep_4_HW4_2020_plot_avg_CI, width = 8, height = 6)

###Sep 12 PRE HW4 2020######

str(berry_thermocouple_HWS_2020)
str(berry_temp_sep_12_2020)



berry_long <- berry_temp_sep_12_2020 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


berry_long <- berry_long %>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "18331 (B1R4)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(17)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(6)" & Station == "3859 (B2R3)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(13)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(7)" & Station == "19170 (B1R1)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(10)" & Station == "17702 (B1R3)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(18)" & Station == "19171 (B3R3)"))%>%
  filter(!(Thermocouple == "Temp_C_Max(7)" & Station == "18335 (B3R2)"))  

# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_sep_12_HW4_2020_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature Sept 12 POST HW4 2020",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_sep_12_HW4_2020_plot_avg_CI.jpg", plot = berry_temperature_sep_12_HW4_2020_plot_avg_CI, width = 8, height = 6)

###### Berry thermocopules figures 2021 group by treatment #####

####June 17 HW1####
str(berry_thermocouple_HWS_2021)
str(berry_temp_june_17_2021)


berry_long <- berry_temp_june_17_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_june_17_HW1_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature June 17 HW1 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_june_17_HW1_2021_plot_avg_CI.jpg", plot = berry_temperature_june_17_HW1_2021_plot_avg_CI, width = 8, height = 6)

####June 15 PRE HW1####
str(berry_temp_june_15_2021)


berry_long <- berry_temp_june_15_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_june_15_HW1_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temperature June 15 PRE HW1 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_june_15_HW1_2021_plot_avg_CI.jpg", plot = berry_temperature_june_15_HW1_2021_plot_avg_CI, width = 8, height = 6)


####jUNE 22 POST HW1######


str(berry_temp_june_22_2021)


berry_long <- berry_temp_june_22_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_june_22_HW1_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temperature June 22 POST HW1 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_june_22_HW1_2021_plot_avg_CI.jpg", plot = berry_temperature_june_22_HW1_2021_plot_avg_CI, width = 8, height = 6)


######  July 10 HW2 2021#####
str(berry_thermocouple_HWS_2021)
str(berry_temp_july_10_2021)



berry_long <- berry_temp_july_10_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_july_10_HW2_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature July 10 HW2 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_july_10_HW2_2021_plot_avg_CI.jpg", plot = berry_temperature_july_10_HW2_2021_plot_avg_CI, width = 8, height = 6)


####jULY 7 PRE HW2 2021####

str(berry_temp_july_07_2021)



berry_long <- berry_temp_july_07_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_july_07_HW2_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature July 07 PRE HW2 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_july_07_HW2_2021_plot_avg_CI.jpg", plot = berry_temperature_july_07_HW2_2021_plot_avg_CI, width = 8, height = 6)

#####July 14 POST HW2 2021####

str(berry_temp_july_14_2021)



berry_long <- berry_temp_july_14_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values

berry_long <- berry_long %>%
  filter(!(Station == "B3R3"))

# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_july_14_HW2_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature July 14 POST HW2 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_july_14_HW2_2021_plot_avg_CI.jpg", plot = berry_temperature_july_14_HW2_2021_plot_avg_CI, width = 8, height = 6)


#####Sep 8 HW3 2021#####
str(berry_thermocouple_HWS_2021)
  str(berry_temp_sep_08_2021)



berry_long <- berry_temp_sep_08_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_sep_08_HW3_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temprature Sep 8 HW3 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_sep_08_HW3_2021_plot_avg_CI.jpg", plot = berry_temperature_sep_08_HW3_2021_plot_avg_CI, width = 8, height = 6)



#####Sep 15 2021 post hw3####


berry_temp_sep_15_2021<-berry_thermocouple_HWS_2021%>%
  filter(Date == "2021-09-15")
str(berry_temp_sep_15_2021)
berry_long <- berry_temp_sep_15_2021 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


# Calculate mean temperature and confidence intervals
result <- berry_long %>%
  group_by(treatment, TIMESTAMP) %>%
  summarise(
    mean_temp = mean(Temperature, na.rm = TRUE),  # Mean temperature
    sd_temp = sd(Temperature, na.rm = TRUE),     # Standard deviation
    n = n(),                                     # Number of observations
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_temp / sqrt(n),                      # Standard error
    ci_lower = mean_temp - qt(0.975, n - 1) * se, # Lower CI (95%)
    ci_upper = mean_temp + qt(0.975, n - 1) * se  # Upper CI (95%)
  )

# View the result
print(result)
# Update treatment names and reverse color order
result <- result %>%
  mutate(
    treatment_label = factor(
      treatment,
      levels = c(1, 2, 3),  # Specify the levels explicitly to control the order
      labels = c("Baseline (60% ET)", "120% ET", "180% ET")
    )
  )

# Define custom colors in reverse order
custom_colors <- c("#FDE725FF", "#21908CFF", "#440154FF")  # Reverse viridis palette

# Plotting
berry_temperature_sep_15_HW3_2021_plot_avg_CI<-ggplot(result, aes(x = TIMESTAMP, y = mean_temp, color = treatment_label, group = treatment_label)) +
  geom_line(size = 1) +  # Line for the mean temperature
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = treatment_label), alpha = 0.6, color = NA) +  # Confidence interval
  labs(
    title = "Berry Temperature Sep 15 POST HW3 2021",
    x = "Time",
    y = "Mean Max Temperature (°C)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_manual(values = custom_colors) +  # Custom colors for lines
  scale_fill_manual(values = custom_colors) +    # Custom colors for ribbons
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

ggsave("figures/berry_temperature_sep_15_HW3_2021_plot_avg_CI.jpg", plot = berry_temperature_sep_15_HW3_2021_plot_avg_CI, width = 8, height = 6)


#####Boxplots for duration of HWs 2020 each day at 2:am, 2 pm, 3 pm and 4 pm  #####
#####HW3 2020#####
berry_thermocouple_HWS_2020_timeseries_HW3<-berry_thermocouple_HWS_2020_timeseries%>%
  filter(Date >= as.Date("2020-08-14") & Date <= as.Date("2020-08-18"))

str(berry_thermocouple_HWS_2020_timeseries_HW3)

unique(berry_thermocouple_HWS_2020_timeseries_HW3$Station)

# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HWS_2020_timeseries_HW3 %>%
  select(Station, starts_with("Temp_C_Max")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_HW3_timeseries_filtered <- berry_thermocouple_HWS_2020_timeseries_HW3 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")


unique(berry_thermocouple_HW3_timeseries_filtered$Station)

# Check the structure of the filtered dataset
str(berry_thermocouple_HW3_timeseries_filtered)

berry_long <- berry_thermocouple_HW3_timeseries_filtered %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


str(berry_thermocouple_HW3_timeseries_filtered)

##### GRAPHS for differnet hours HW3####
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Step 1: Filter rows for 2:00 AM and 14:00 using TIMESTAMP
filtered_data <- berry_long %>%
  filter(format(TIMESTAMP, "%H:%M:%S") %in% c("02:00:00", "14:00:00", "15:00:00", "16:00:00")) %>%
  filter(!Date == "2020-08-18")


str(filtered_data)

filtered_data$Temperature <- as.numeric(unlist(filtered_data$Temperature))

filtered_data<-filtered_data%>%
  mutate(across(where(is.numeric), ~ replace(., is.nan(.), NA)))%>%
  filter(!is.na(Temperature))

sum(is.na(filtered_data$Temperature))
sum(is.nan(filtered_data$Temperature))

# Step 3: Split data into 2:00 AM and 14:00 groups
data_2am <- filtered_data %>% filter(hour(TIMESTAMP) == 2)
data_15pm <- filtered_data %>% filter(hour(TIMESTAMP) == 15)
data_14pm <- filtered_data %>% filter(hour(TIMESTAMP) == 14)
data_16pm <- filtered_data %>% filter(hour(TIMESTAMP) == 16)


  tally_data_2am<-data_2am%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()
  
  
  tally_data_2pm<-data_14pm%>%
    group_by(TIMESTAMP, treatment)%>%
    tally()
  
  
  tally_data_3pm<-data_15pm%>%
    group_by(TIMESTAMP, treatment)%>%
    tally()
  
  
  tally_data_4pm<-data_16pm%>%
    group_by(TIMESTAMP, treatment)%>%
    tally()

  str(data_2am)
# Step 4: Create boxplots for 2:00 AM, faceted by Date
plot_2am <- ggplot(data_2am, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 AM HW3 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2020_2AM.jpg", plot = plot_2am, width = 12, height = 6)

# Step 5: Create boxplots for 14:00, faceted by Date
plot_15pm <- ggplot(data_15pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 3:00 PM HW3 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2020_3PM.jpg", plot = plot_15pm, width = 12, height = 6)



plot_14pm <- ggplot(data_14pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 PM HW3 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2020_2PM.jpg", plot = plot_14pm, width = 12, height = 6)


plot_16pm <- ggplot(data_16pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 4:00 PM HW3 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2020_4PM.jpg", plot = plot_16pm, width = 12, height = 6)


###HW4 2020 #####

berry_thermocouple_HWS_2020_timeseries_HW4<-berry_thermocouple_HWS_2020_timeseries%>%
  filter(Date >= as.Date("2020-09-05") & Date <= as.Date("2020-09-07"))

str(berry_thermocouple_HWS_2020_timeseries_HW4)


# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HWS_2020_timeseries_HW4 %>%
  select(Station, starts_with("Temp_C_Max")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_HW4_timeseries_filtered <- berry_thermocouple_HWS_2020_timeseries_HW4 %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")

berry_thermocouple_HW4_timeseries_filtered <- berry_thermocouple_HW4_timeseries_filtered %>%
  mutate(across(everything(), ~ replace(., is.null(.), NA)))

unique(berry_thermocouple_HW4_timeseries_filtered$Station)
# Check the structure of the filtered dataset
str(berry_thermocouple_HW4_timeseries_filtered)

unique(berry_thermocouple_HW4_timeseries_filtered$Station)

berry_long <-berry_thermocouple_HW4_timeseries_filtered%>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values

sum(is.null(berry_long$Temperature))

str(berry_thermocouple_HW4_timeseries_filtered)

##### GRAPHS for different hours HW4####
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Step 1: Filter rows for 2:00 AM and 14:00 using TIMESTAMP
filtered_data <- berry_long %>%
  filter(format(TIMESTAMP, "%H:%M:%S") %in% c("02:00:00", "14:00:00", "15:00:00", "16:00:00"))

unique(filtered_data$treatment)
str(filtered_data)

filtered_data$Temperature <- sapply(filtered_data$Temperature, function(x) x[1])

filtered_data$Temperature <- as.numeric(unlist(filtered_data$Temperature))

filtered_data<-filtered_data%>%
  mutate(across(where(is.numeric), ~ replace(., is.nan(.), NA)))%>%
  filter(!is.na(Temperature))

sum(is.na(filtered_data$Temperature))
sum(is.nan(filtered_data$Temperature))

# Step 3: Split data into 2:00 AM and 14:00 groups
data_2am <- filtered_data %>% filter(hour(TIMESTAMP) == 2)
data_15pm <- filtered_data %>% filter(hour(TIMESTAMP) == 15)
data_14pm <- filtered_data %>% filter(hour(TIMESTAMP) == 14)
data_16pm <- filtered_data %>% filter(hour(TIMESTAMP) == 16)


tally_data_2am<-data_2am%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_2pm<-data_14pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_3pm<-data_15pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_4pm<-data_16pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


# Step 4: Create boxplots for 2:00 AM, faceted by Date
plot_2am <- ggplot(data_2am, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 AM HW4 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW4_2020_2AM.jpg", plot = plot_2am, width = 12, height = 6)

# Step 5: Create boxplots for 14:00, faceted by Date
plot_15pm <- ggplot(data_15pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 3:00 PM HW4 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW4_2020_3PM.jpg", plot = plot_15pm, width = 12, height = 6)



plot_14pm <- ggplot(data_14pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 PM HW4 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW4_2020_2PM.jpg", plot = plot_14pm, width = 12, height = 6)


plot_16pm <- ggplot(data_16pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 4:00 PM HW4 2020"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW4_2020_4PM.jpg", plot = plot_16pm, width = 12, height = 6)


#####boxplots HWs 2021 each day at 2:am, 2 pm, 3 pm and 4 pm #####

#####HW1 2021#####
berry_thermocouple_HWS_2021_timeseries_HW1<-berry_thermocouple_HWS_2021_timeseries%>%
  filter(Date >= as.Date("2021-06-17") & Date <= as.Date("2021-06-19"))

str(berry_thermocouple_HWS_2021_timeseries_HW1)

unique(berry_thermocouple_HWS_2021_timeseries_HW1$Station)

# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HWS_2021_timeseries_HW1 %>%
  select(Station, starts_with("Therm_Couple")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_HW1_timeseries_filtered <- berry_thermocouple_HWS_2021_timeseries_HW1 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")


unique(berry_thermocouple_HW1_timeseries_filtered$Station)

# Check the structure of the filtered dataset
str(berry_thermocouple_HW1_timeseries_filtered)

berry_long <- berry_thermocouple_HW1_timeseries_filtered %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


str(berry_thermocouple_HW1_timeseries_filtered)

##### GRAPHS for differnet hours HW1 2021 ####
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Step 1: Filter rows for 2:00 AM and 14:00 using TIMESTAMP
filtered_data <- berry_long %>%
  filter(format(TIMESTAMP, "%H:%M:%S") %in% c("02:00:00", "14:00:00", "15:00:00", "16:00:00")) 


str(filtered_data)

filtered_data$Temperature <- as.numeric(unlist(filtered_data$Temperature))

filtered_data<-filtered_data%>%
  mutate(across(where(is.numeric), ~ replace(., is.nan(.), NA)))%>%
  filter(!is.na(Temperature))

sum(is.na(filtered_data$Temperature))
sum(is.nan(filtered_data$Temperature))

# Step 3: Split data into 2:00 AM and 14:00 groups
data_2am <- filtered_data %>% filter(hour(TIMESTAMP) == 2)
data_15pm <- filtered_data %>% filter(hour(TIMESTAMP) == 15)
data_14pm <- filtered_data %>% filter(hour(TIMESTAMP) == 14)
data_16pm <- filtered_data %>% filter(hour(TIMESTAMP) == 16)


tally_data_2am<-data_2am%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_2pm<-data_14pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_3pm<-data_15pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_4pm<-data_16pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()

str(data_2am)
# Step 4: Create boxplots for 2:00 AM, faceted by Date
plot_2am <- ggplot(data_2am, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 AM HW1 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW1_2021_2AM.jpg", plot = plot_2am, width = 12, height = 6)

# Step 5: Create boxplots for 14:00, faceted by Date
plot_15pm <- ggplot(data_15pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 3:00 PM HW1 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW1_2021_3PM.jpg", plot = plot_15pm, width = 12, height = 6)



plot_14pm <- ggplot(data_14pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 PM HW1 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW1_2021_2PM.jpg", plot = plot_14pm, width = 12, height = 6)


plot_16pm <- ggplot(data_16pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 4:00 PM HW1 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW1_2021_4PM.jpg", plot = plot_16pm, width = 12, height = 6)


#####HW2 2021#####

berry_thermocouple_HWS_2021_timeseries_HW2<-berry_thermocouple_HWS_2021_timeseries%>%
  filter(Date >= as.Date("2021-07-09") & Date <= as.Date("2021-07-11"))

str(berry_thermocouple_HWS_2021_timeseries_HW2)

unique(berry_thermocouple_HWS_2021_timeseries_HW2$Station)

# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HWS_2021_timeseries_HW2 %>%
  select(Station, starts_with("Therm_Couple")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_HW2_timeseries_filtered <- berry_thermocouple_HWS_2021_timeseries_HW2 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")


unique(berry_thermocouple_HW2_timeseries_filtered$Station)

# Check the structure of the filtered dataset
str(berry_thermocouple_HW2_timeseries_filtered)

berry_long <- berry_thermocouple_HW2_timeseries_filtered %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


str(berry_thermocouple_HW2_timeseries_filtered)

##### GRAPHS for differnet hours HW2 2021 ####
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Step 1: Filter rows for 2:00 AM and 14:00 using TIMESTAMP
filtered_data <- berry_long %>%
  filter(format(TIMESTAMP, "%H:%M:%S") %in% c("02:00:00", "14:00:00", "15:00:00", "16:00:00")) 


str(filtered_data)

filtered_data$Temperature <- as.numeric(unlist(filtered_data$Temperature))

filtered_data<-filtered_data%>%
  mutate(across(where(is.numeric), ~ replace(., is.nan(.), NA)))%>%
  filter(!is.na(Temperature))

sum(is.na(filtered_data$Temperature))
sum(is.nan(filtered_data$Temperature))

# Step 3: Split data into 2:00 AM and 14:00 groups
data_2am <- filtered_data %>% filter(hour(TIMESTAMP) == 2)
data_15pm <- filtered_data %>% filter(hour(TIMESTAMP) == 15)
data_14pm <- filtered_data %>% filter(hour(TIMESTAMP) == 14)
data_16pm <- filtered_data %>% filter(hour(TIMESTAMP) == 16)


tally_data_2am<-data_2am%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_2pm<-data_14pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_3pm<-data_15pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_4pm<-data_16pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()

str(data_2am)
# Step 4: Create boxplots for 2:00 AM, faceted by Date
plot_2am <- ggplot(data_2am, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 AM HW2 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW2_2021_2AM.jpg", plot = plot_2am, width = 12, height = 6)

# Step 5: Create boxplots for 14:00, faceted by Date
plot_15pm <- ggplot(data_15pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 3:00 PM HW2 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW2_2021_3PM.jpg", plot = plot_15pm, width = 12, height = 6)



plot_14pm <- ggplot(data_14pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 PM HW2 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW2_2021_2PM.jpg", plot = plot_14pm, width = 12, height = 6)


plot_16pm <- ggplot(data_16pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 4:00 PM HW2 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW2_2021_4PM.jpg", plot = plot_16pm, width = 12, height = 6)

#####HW3 2021#####

berry_thermocouple_HWS_2021_timeseries_HW3<-berry_thermocouple_HWS_2021_timeseries%>%
  filter(Date >= as.Date("2021-09-06") & Date <= as.Date("2021-09-08"))

str(berry_thermocouple_HWS_2021_timeseries_HW3)

unique(berry_thermocouple_HWS_2021_timeseries_HW3$Station)

# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HWS_2021_timeseries_HW3 %>%
  select(Station, starts_with("Therm_Couple")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_HW3_timeseries_filtered <- berry_thermocouple_HWS_2021_timeseries_HW3 %>%
  pivot_longer(cols = starts_with("Therm_Couple"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")


unique(berry_thermocouple_HW3_timeseries_filtered$Station)

# Check the structure of the filtered dataset
str(berry_thermocouple_HW3_timeseries_filtered)

berry_long <- berry_thermocouple_HW3_timeseries_filtered %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature") %>%
  drop_na(Temperature)  # Remove NA values


str(berry_thermocouple_HW3_timeseries_filtered)

##### GRAPHS for differnet hours HW3 2021 ####
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Step 1: Filter rows for 2:00 AM and 14:00 using TIMESTAMP
filtered_data <- berry_long %>%
  filter(format(TIMESTAMP, "%H:%M:%S") %in% c("02:00:00", "14:00:00", "15:00:00", "16:00:00")) 


str(filtered_data)

filtered_data$Temperature <- as.numeric(unlist(filtered_data$Temperature))

filtered_data<-filtered_data%>%
  mutate(across(where(is.numeric), ~ replace(., is.nan(.), NA)))%>%
  filter(!is.na(Temperature))

sum(is.na(filtered_data$Temperature))
sum(is.nan(filtered_data$Temperature))

# Step 3: Split data into 2:00 AM and 14:00 groups
data_2am <- filtered_data %>% filter(hour(TIMESTAMP) == 2)
data_15pm <- filtered_data %>% filter(hour(TIMESTAMP) == 15)
data_14pm <- filtered_data %>% filter(hour(TIMESTAMP) == 14)
data_16pm <- filtered_data %>% filter(hour(TIMESTAMP) == 16)


tally_data_2am<-data_2am%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_2pm<-data_14pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_3pm<-data_15pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()


tally_data_4pm<-data_16pm%>%
  group_by(TIMESTAMP, treatment)%>%
  tally()

str(data_2am)
# Step 4: Create boxplots for 2:00 AM, faceted by Date
plot_2am <- ggplot(data_2am, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 AM HW3 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2021_2AM.jpg", plot = plot_2am, width = 12, height = 6)

# Step 5: Create boxplots for 14:00, faceted by Date
plot_15pm <- ggplot(data_15pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 3:00 PM HW3 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2021_3PM.jpg", plot = plot_15pm, width = 12, height = 6)



plot_14pm <- ggplot(data_14pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 2:00 PM HW3 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2021_2PM.jpg", plot = plot_14pm, width = 12, height = 6)


plot_16pm <- ggplot(data_16pm, aes(x = factor(treatment), y = Temperature, fill = factor(treatment))) +
  geom_boxplot() +
  facet_wrap(~ Date, scales = "free_x") +  # Facet by Date
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "120% ET", "180% ET"))+
  labs(
    x = "Treatment",
    y = "Temperature (°C)",
    title = "Boxplots of Temperatures at 4:00 PM HW3 2021"
  ) +
  theme_minimal(base_size = 14)+
  scale_x_discrete(
    labels = c("1" = "Baseline (60% ET)", "2" = "120% ET", "3" = "180% ET")  # Map treatment values to labels
  )

ggsave("figures/boxplot_HW3_2021_4PM.jpg", plot = plot_16pm, width = 12, height = 6)



#####HEAT SUMMATION FOR THERMOCOUPLES 2021#####

berry_thermocouple_HWS_2021_timeseries

str(berry_thermocouple_HWS_2021_timeseries)

unique(berry_thermocouple_HWS_2021_timeseries$Station)

max(berry_thermocouple_HWS_2021_timeseries$TIMESTAMP, na.rm = TRUE)

min(berry_thermocouple_HWS_2021_timeseries$TIMESTAMP, na.rm = TRUE)

# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HWS_2021_timeseries %>%
  select(Station, starts_with("Therm_Couple")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_timeseries_2021_filtered <- berry_thermocouple_HWS_2021_timeseries %>%
  pivot_longer(cols = starts_with("Therm_Couple"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")


unique(berry_thermocouple_timeseries_2021_filtered$Station)


# Check the structure of the filtered dataset
str(berry_thermocouple_timeseries_2021_filtered)

berry_long <- berry_thermocouple_timeseries_2021_filtered %>%
  pivot_longer(cols = starts_with("Therm_Couple"), 
               names_to = "Thermocouple", 
               values_to = "Temperature")

str(berry_long)


no_nas_check<-berry_long %>%
  group_by(Station, Thermocouple) %>%
  summarise(na_count = sum(is.na(Temperature)), .groups = "drop")

berry_hourly <- berry_long %>%
  mutate(Hourly_Timestamp = floor_date(TIMESTAMP, "hour")) %>%  # Round down to the hour
  group_by(Station, Thermocouple, Hourly_Timestamp) %>%
  summarise(Temperature = mean(Temperature, na.rm = TRUE), .groups = "drop") %>%  # Compute mean temp
  filter(!Hourly_Timestamp < as.POSIXct("2021-03-31 23:45:00", tz = "UTC"))  # Correct filtering

no_nas_check<-berry_hourly %>%
  group_by(Station, Thermocouple) %>%
  summarise(na_count = sum(is.na(Temperature)), .groups = "drop")

berry_hourly_no_nas<-berry_hourly%>%
  drop_na(Temperature)  # Remove NA values


str(berry_hourly_no_nas)

sum(is.na(berry_hourly_no_nas))
str(berry_hourly_no_nas)


heat_summation_2021 <- berry_hourly_no_nas %>%
  group_by(Station, Thermocouple) %>%
  summarise(
    Total_Hours = n(),  # Count number of recorded hours
    Heat_Summation_C = sum(Temperature, na.rm = TRUE),  # Sum temperatures
    Earliest_Timestamp = min(Hourly_Timestamp, na.rm = TRUE),  # Earliest timestamp
    Latest_Timestamp = max(Hourly_Timestamp, na.rm = TRUE),  # Latest timestamp
    .groups = "drop"
  )

write.csv(heat_summation_2021,"data_output/heat_summation_2021_BH_thermocouples.csv")


#####HEAT SUMMATION FOR THERMOCOUPLES 2020#####

berry_thermocouple_HWS_2020_timeseries

str(berry_thermocouple_HWS_2020_timeseries)

unique(berry_thermocouple_HWS_2020_timeseries$Station)

max(berry_thermocouple_HWS_2020_timeseries$TIMESTAMP, na.rm = TRUE)

min(berry_thermocouple_HWS_2020_timeseries$TIMESTAMP, na.rm = TRUE)


# Convert to hourly data
berry_thermocouple_HWS_2020_timeseries_hourly <- berry_thermocouple_HWS_2020_timeseries %>%
  mutate(Hourly_Timestamp = floor_date(TIMESTAMP, "hour")) %>%  # Round timestamp to the hour
  group_by(Station, Hourly_Timestamp) %>%
  summarise(across(starts_with("Temp_C_Max"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")  # Compute mean temp

# Step 1: Identify stations and Temp_C_Max columns with invalid values
flagged_stations_and_columns <- berry_thermocouple_HWS_2020_timeseries %>%
  select(Station, starts_with("Temp_C_Max")) %>%
  pivot_longer(cols = -Station, names_to = "Column", values_to = "Value") %>%
  filter(Value < 0 | Value > 100) %>%
  distinct(Station, Column)

# Step 2: Remove values for flagged Temp_C_Max columns in flagged stations
berry_thermocouple_timeseries_2020_filtered <- berry_thermocouple_HWS_2020_timeseries_hourly %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), names_to = "Column", values_to = "Value") %>%
  anti_join(flagged_stations_and_columns, by = c("Station", "Column")) %>%
  pivot_wider(names_from = "Column", values_from = "Value")


unique(berry_thermocouple_timeseries_2020_filtered$Station)


# Check the structure of the filtered dataset
str(berry_thermocouple_timeseries_2020_filtered)

berry_long <- berry_thermocouple_timeseries_2020_filtered %>%
  pivot_longer(cols = starts_with("Temp_C_Max"), 
               names_to = "Thermocouple", 
               values_to = "Temperature")

str(berry_long)


no_nas_check<-berry_long %>%
  group_by(Station, Thermocouple) %>%
  summarise(na_count = sum(is.na(Temperature)), .groups = "drop")


berry_hourly_no_nas<-berry_long%>%
  drop_na(Temperature)  # Remove NA values


str(berry_hourly_no_nas)

sum(is.na(berry_hourly_no_nas))


heat_summation_2020 <- berry_hourly_no_nas %>%
  group_by(Station, Thermocouple) %>%
  summarise(
    Total_Hours = n(),  # Count number of recorded hours
    Heat_Summation_C = sum(Temperature, na.rm = TRUE),  # Sum temperatures
    Earliest_Timestamp = min(Hourly_Timestamp, na.rm = TRUE),  # Earliest timestamp
    Latest_Timestamp = max(Hourly_Timestamp, na.rm = TRUE),  # Latest timestamp
    .groups = "drop"
  )


write.csv(heat_summation_2020,"data_output/heat_summation_2020_BH_thermocouples.csv")
