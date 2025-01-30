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

se <- function(x) {
  x <- na.omit(x)  # Remove NA values
  sd(x) / sqrt(length(x))  # Standard error formula
}

##### BH Stations data ####

BH_met_2020<-read.csv("data/All_stations_out.csv")
BH_met_2021<-read.csv("data/Met_All_15min_2021.csv")
BH_met_2021<-read.csv("data/Met_All_15min_2021 _AG.csv")


BH_met_2020_temps<-BH_met_2020%>%
  select(TIMESTAMP,Station, Soil_Temp_TCAV_Avg,VWC_Avg, T_IRT_Avg,T_IRT_Max, AirTC_Avg, AirTC_Max, AirTC_AC_Avg, AirTC_AC_Max, RH, RH_Max, RH_Min, RH_AC, RH_AC_Max, RH_AC_Min)

BH_met_2021_temps<-BH_met_2021%>%
  select(TIMESTAMP, Station, Soil_Temp_TCAV_Avg,VWC_Avg,VWC_2_Avg, VWC_3_Avg, T_IRT_Avg,T_IRT_Max, AirTC_Avg, AirTC_Max, AirTC_AC_Avg, AirTC_AC_Max, RH, RH_Max, RH_Min, RH_AC, RH_AC_Max, RH_AC_Min)

BH_met_2021_temps$TIMESTAMP<-mdy_hm(BH_met_2021_temps$TIMESTAMP)

str(BH_met_2020_temps)

str(BH_met_2021_temps)

#BH_met_2021_temps$TIMESTAMP<- mdy_hm(BH_met_2021_temps$TIMESTAMP)


### Soil temp 5 cm Daily average, MIN, AND MAX 2020 and 2021 ######

get_temp_at_14 <- function(timestamp, air_temp) {
  # Convert timestamp to hour
  hour_val <- hour(timestamp)
  
  # Check if temperature at 14:00 exists, if not try 13:00 or 15:00
  if (14 %in% hour_val) {
    return(air_temp[hour_val == 14][1])
  } else if (13 %in% hour_val) {
    return(air_temp[hour_val == 13][1])
  } else if (15 %in% hour_val) {
    return(air_temp[hour_val == 15][1])
  } else {
    return(NA)  # If none of these are available, return NA
  }
}



# Explicitly handle problematic TIMESTAMP values
BH_2020_soil_temp_5cm <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         Soil_Temp_TCAV_Avg = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Soil_Temp_TCAV_Avg = mean(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Max_Soil_Temp_TCAV = max(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Min_Soil_Temp_TCAV = min(Soil_Temp_TCAV_Avg, na.rm = TRUE),
#    Soil_temp_at_14 = Soil_Temp_TCAV_Avg[hour(TIMESTAMP) == 14][1],  # Air temp at 14:00 (if available)   
    Soil_temp_at_14 = get_temp_at_14(TIMESTAMP_parsed, Soil_Temp_TCAV_Avg),  # Use helper function   
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))%>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station== "19171 (B3R3)" ~ "B3R3"
  ))


str(BH_2020_soil_temp_5cm)

BH_2020_soil_temp_5cm_anova<-BH_2020_soil_temp_5cm

str(BH_2020_soil_temp_5cm_anova)
BH_2020_soil_temp_5cm_anova<-BH_2020_soil_temp_5cm_anova%>%
  select(Date, treatment, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV)

BH_2020_soil_temp_5cm_anova$treatment<-as.factor(BH_2020_soil_temp_5cm_anova$treatment)

str(BH_2020_soil_temp_5cm_anova)

sum(is.na(BH_2020_soil_temp_5cm_anova$Daily_Max_Soil_Temp_TCAV))
sum(is.na(BH_2020_soil_temp_5cm_anova$Date))
sum(is.na(BH_2020_soil_temp_5cm_anova$treatment))

BH_2020_soil_temp_5cm_anova_tally<-BH_2020_soil_temp_5cm_anova%>%
  group_by(Date, treatment)%>%
  tally()

BH_2020_soil_temp_5cm_anova <- BH_2020_soil_temp_5cm_anova %>%
  group_by(Date, treatment) %>%
  filter(n() > 1) %>%
  ungroup()

# Initialize an empty data frame to store results
dates <- unique(BH_2020_soil_temp_5cm_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_soil_temp_2020_MAX<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_2020_soil_temp_5cm_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Daily_Max_Soil_Temp_TCAV,
    standard_error = standard_errors$Daily_Max_Soil_Temp_TCAV,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_soil_temp_2020_MAX <- rbind(date_results,results_df_soil_temp_2020_MAX)
}

results_df_soil_temp_2020_MAX$date <- as.Date(results_df_soil_temp_2020_MAX$date)

# Check the structure to confirm the conversion
str(results_df_soil_temp_2020_MAX$date)


str(results_df_soil_temp_2020_MAX)
results_df_soil_temp_2020_MAX$Mean_sem <- paste(round(results_df_soil_temp_2020_MAX$mean, 2), "±", round(results_df_soil_temp_2020_MAX$standard_error, 2),results_df_soil_temp_2020_MAX$letters_ordered.groups)


str(results_df_soil_temp_2020_MAX)

write.csv(results_df_soil_temp_2020_MAX,"data_output/results_df_soil_temp_2020_MAX.csv")



# Reshape the data to long format
BH_2020_soil_temp_long <- BH_2020_soil_temp_5cm %>%
  pivot_longer(cols = c(Daily_mean_Soil_Temp_TCAV_Avg, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV, Soil_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")


# Plot with ggplot

BH_soil_temp_2020_5cm_per_station<-ggplot(BH_2020_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends 2020",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_soil_temp_2020_5cm_per_station_panel_plot.jpg", plot = BH_soil_temp_2020_5cm_per_station, width = 18, height = 13, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_soil_temp_2020_5cm_per_station_one_plot<-ggplot(BH_2020_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Stations, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends by Station and Temperature Type 2020",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = "solid", 
                                   "Daily_Max_Soil_Temp_TCAV" = "dashed", 
                                   "Daily_Min_Soil_Temp_TCAV" = "dotdash", 
                                   "Soil_temp_at_14" ="dotted")) 
# Line types for temp types

ggsave("figures/BH_soil_temp_2020_5cm_per_station_one_plot.jpg", plot = BH_soil_temp_2020_5cm_per_station_one_plot, width = 10, height = 6, dpi = 600)


BH_2020_soil_temp_by_treatment<-BH_met_2020_temps %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))%>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station== "19171 (B3R3)" ~ "B3R3"
  ))

BH_2020_soil_temp_by_treatment<-BH_2020_soil_temp_by_treatment%>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         Soil_Temp_TCAV_Avg = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by Station and Date
  group_by(treatment, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Soil_Temp_TCAV_Avg = mean(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Max_Soil_Temp_TCAV = max(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Min_Soil_Temp_TCAV = min(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    .groups = 'drop'  # Prevent grouped output warning
  )

# Reshape the data to long format
BH_2020_soil_temp_by_treatment_long <- BH_2020_soil_temp_by_treatment %>%
  pivot_longer(cols = c(Daily_mean_Soil_Temp_TCAV_Avg, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")

str(BH_2020_soil_temp_by_treatment_long)

BH_2020_soil_temp_by_treatment_long$treatment<-as.factor(BH_2020_soil_temp_by_treatment_long$treatment)

BH_soil_temp_2020_5cm_per_treatment<-ggplot(BH_2020_soil_temp_by_treatment_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Soil Temperature Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = 1,    # Full opacity
                                "Daily_Max_Soil_Temp_TCAV" = 0.6,   # Slight transparency
                                "Daily_Min_Soil_Temp_TCAV" = 0.3 )) + # More transparency
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 10, ymax =  50,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 10, ymax =  50,
           alpha = .2)+
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y =  50, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y =  50, label ="HW4", size = 7) 

ggsave("figures/BH_soil_temp_2020_5cm_per_treatment.jpg", plot = BH_soil_temp_2020_5cm_per_treatment, width = 10, height = 6, dpi = 600)

#### soil temperature WITH 95% CONFIDENCE INTERVALS AND SUMMARIZING STATIONS PER TREATMENT

# Continue with the parsing and processing

BH_2020_Soil_temp_5cm_temp_grouped<- BH_met_2020_temps %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station == "19171 (B3R3)" ~ 3
  ))%>%
  mutate(treatment = as.factor(treatment)) 

BH_2020_Soil_temp_5cm_temp_per_station <- BH_2020_Soil_temp_5cm_temp_grouped %>%
  mutate(Date = as.Date(TIMESTAMP),
         soil_temp_5_cm = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by treatment, station, and date to get max/min/mean for each station per day
  group_by(treatment, Station, Date) %>%
  summarise(
    Max_Soil_Temp_TCAV_Avg_station = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, max(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    Min_Soil_Temp_TCAV_Avg_station = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, min(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    .groups = 'drop'
  )

# Now group by treatment and date, and calculate the mean and 95% CI across stations
BH_2020_Soil_temp_5cm_temp_per_treatment <- BH_2020_Soil_temp_5cm_temp_per_station %>%
  group_by(treatment, Date) %>%
  summarise(
    Mean_Max_Soil_Temp_TCAV_Avg = mean(Max_Soil_Temp_TCAV_Avg_station, na.rm = TRUE),
    Mean_Min_Soil_Temp_TCAV_Avg = mean(Min_Soil_Temp_TCAV_Avg_station, na.rm = TRUE),
    CI_Max_Soil_Temp_TCAV_Avg = 1.96 * sd(Max_Soil_Temp_TCAV_Avg_station, na.rm = TRUE) / sqrt(n()),
    CI_Min_Soil_Temp_TCAV_Avg = 1.96 * sd(Min_Soil_Temp_TCAV_Avg_station, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  filter(!Mean_Max_Soil_Temp_TCAV_Avg < 0, !Mean_Min_Soil_Temp_TCAV_Avg < 0)

write.csv(BH_2020_Soil_temp_5cm_temp_per_treatment,"data_output/BH_2020_Soil_temp_5cm_temp_per_treatment.csv")

#  for plotting
BH_2020_soil_temp_avg_per_treatment_long_with_CI <-ggplot(BH_2020_Soil_temp_5cm_temp_per_treatment, aes(x = Date)) +
  # Map linetype inside aes based on whether it's max or min temperature
  geom_line(aes(y = Mean_Max_Soil_Temp_TCAV_Avg, color = treatment, linetype = "Max Temp"), size =1.2) +
  geom_line(aes(y = Mean_Min_Soil_Temp_TCAV_Avg, color = treatment, linetype = "Min Temp"), size =1.2) +
  labs(title = "2020",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_linetype_manual(values = c("Max Temp" = "solid", "Min Temp" = "dashed"), 
                        name = "Temperature Type", 
                        labels = c("Daily mean Max temperature", "Daily mean Min temperature")) +
  
  # Add ribbons for 95% CI with same colors and treatment names
  geom_ribbon(aes(x = Date, ymin = Mean_Max_Soil_Temp_TCAV_Avg - CI_Max_Soil_Temp_TCAV_Avg, 
                  ymax = Mean_Max_Soil_Temp_TCAV_Avg + CI_Max_Soil_Temp_TCAV_Avg, fill = treatment), 
              alpha = 0.3) +
  geom_ribbon(aes(x = Date, ymin = Mean_Min_Soil_Temp_TCAV_Avg - CI_Min_Soil_Temp_TCAV_Avg, 
                  ymax = Mean_Min_Soil_Temp_TCAV_Avg + CI_Min_Soil_Temp_TCAV_Avg, fill = treatment), 
              alpha = 0.3) +
  
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                       labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  
  # Annotate heatwave events
  annotate("rect", xmin = as.Date("2020-08-13"), xmax = as.Date("2020-08-19"), ymin = 10, ymax = 45, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-09-05"), xmax = as.Date("2020-09-08"), ymin = 10, ymax = 45, alpha = .2) +
  annotate("text", x = as.Date("2020-08-15"), y = 45, label = "HW3", size = 7) +
  annotate("text", x = as.Date("2020-09-07"), y = 45, label = "HW4", size = 7)+
  scale_y_continuous(breaks=seq(10,45,10), limits = c (10,45)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-07-23"), as.Date("2020-09-14"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-07-23", "2020-09-14"))) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  theme(legend.position = "none") 


ggsave("figures/BH_2020_soil_temp_avg_per_treatment_long_with_CI.jpg", plot = BH_2020_soil_temp_avg_per_treatment_long_with_CI, width = 10, height = 6, dpi = 600) 


#### soil 2021
str(BH_met_2021_temps)
# Explicitly handle problematic TIMESTAMP values
BH_2021_soil_temp_5cm <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         Soil_Temp_TCAV_Avg = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Soil_Temp_TCAV_Avg = mean(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Max_Soil_Temp_TCAV = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, max(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    Daily_Min_Soil_Temp_TCAV = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, min(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
#    Soil_temp_at_14 = Soil_Temp_TCAV_Avg[hour(TIMESTAMP) == 14][1],  # Air temp at 14:00 (if available)   
    Soil_temp_at_14 = get_temp_at_14(TIMESTAMP, Soil_Temp_TCAV_Avg),  # Use helper function   
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))%>%
  filter(!Daily_mean_Soil_Temp_TCAV_Avg<0)


# Reshape the data to long format
BH_2021_soil_temp_long <- BH_2021_soil_temp_5cm %>%
  pivot_longer(cols = c(Daily_mean_Soil_Temp_TCAV_Avg, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV,Soil_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")





str(BH_2021_soil_temp_5cm)

BH_2021_soil_temp_5cm_anova<-BH_2021_soil_temp_5cm

str(BH_2021_soil_temp_5cm_anova)
BH_2021_soil_temp_5cm_anova<-BH_2021_soil_temp_5cm_anova%>%
  select(Date, treatment, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV)

BH_2021_soil_temp_5cm_anova$treatment<-as.factor(BH_2021_soil_temp_5cm_anova$treatment)

str(BH_2021_soil_temp_5cm_anova)

sum(is.na(BH_2021_soil_temp_5cm_anova$Daily_Max_Soil_Temp_TCAV))
sum(is.na(BH_2021_soil_temp_5cm_anova$Date))
sum(is.na(BH_2021_soil_temp_5cm_anova$treatment))

BH_2021_soil_temp_5cm_anova_tally<-BH_2021_soil_temp_5cm_anova%>%
  group_by(Date, treatment)%>%
  tally()

BH_2021_soil_temp_5cm_anova <- BH_2021_soil_temp_5cm_anova %>%
  group_by(Date, treatment) %>%
  filter(n() > 1) %>%
  ungroup()

# Initialize an empty data frame to store results
dates <- unique(BH_2021_soil_temp_5cm_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_soil_temp_2021_MAX<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_2021_soil_temp_5cm_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Daily_Max_Soil_Temp_TCAV,
    standard_error = standard_errors$Daily_Max_Soil_Temp_TCAV,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_soil_temp_2021_MAX <- rbind(date_results,results_df_soil_temp_2021_MAX)
}

results_df_soil_temp_2021_MAX$date <- as.Date(results_df_soil_temp_2021_MAX$date)

# Check the structure to confirm the conversion
str(results_df_soil_temp_2021_MAX$date)


str(results_df_soil_temp_2021_MAX)
results_df_soil_temp_2021_MAX$Mean_sem <- paste(round(results_df_soil_temp_2021_MAX$mean, 2), "±", round(results_df_soil_temp_2021_MAX$standard_error, 2),results_df_soil_temp_2021_MAX$letters_ordered.groups)


str(results_df_soil_temp_2021_MAX)

write.csv(results_df_soil_temp_2021_MAX,"data_output/results_df_soil_temp_2021_MAX.csv")

# Plot with ggplot

BH_soil_temp_2021_5cm_per_station<-ggplot(BH_2021_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends 2021",
       x = "Date",
       y = "Soil Temperature at 5 cm (°C)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_soil_temp_2021_5cm_per_station_panel_plot.jpg", plot = BH_soil_temp_2021_5cm_per_station, width = 18, height = 13, dpi = 600)



#ggplot(BH_2021_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Station, linetype = Temperature_Type)) +
#  geom_line() +  # Line plot
#  labs(title = "Soil Temperature Trends Across Stations",
#       x = "Date",
#       y = "Temperature (°C)") +
#  theme_minimal() +  # Clean theme
#  scale_color_viridis_d() +  # Use viridis color scale for stations
#  scale_linetype_manual(values = c("solid", "dashed", "dotted"))  # Line types for temperature types



# Define custom colors: shades of similar colors for B1, B2, and B3 groups

#custom_station_colors <- c(
#  "B1R1" = "#FDE725FF",     # Light Yellow for B1R1
#  "B1R3" = "#E6D900FF",     # Medium Yellow for B1R3
#  "B1R4" = "#D4C700FF",     # Dark Yellow for B1R4
#  "B2R1" = "#21908CFF",     # Light Teal for B2R1
#  "B2R2" = "#1E7F7FFF",     # Medium Teal for B2R2
#  "B2R3" = "#1B6C6CFF",     # Dark Teal for B2R3
#  "B3R1" = "#440154FF",     # Light Purple for B3R1
#  "B3R2" = "#3D0F40FF",     # Medium Purple for B3R2
#  "B3R3" = "#360B3CFF"      # Dark Purple for B3R3
#)
custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_soil_temp_2021_5cm_per_station_one_plot<-ggplot(BH_2021_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Station, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends by Station and Temperature Type 2021",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = "solid", 
                                   "Daily_Max_Soil_Temp_TCAV" = "dashed", 
                                   "Daily_Min_Soil_Temp_TCAV" = "dotted"))  # Line types for temp types

ggsave("figures/BH_soil_temp_2021_5cm_per_station_one_plot.jpg", plot = BH_soil_temp_2021_5cm_per_station_one_plot, width = 10, height = 6, dpi = 600)



BH_2021_soil_temp_per_treatment <- BH_met_2021_temps %>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))%>%
  mutate(treatment = as.factor(treatment)) 

BH_2021_soil_temp_per_treatment <- BH_2021_soil_temp_per_treatment %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         Soil_Temp_TCAV_Avg = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by Station and Date
  group_by(treatment, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Soil_Temp_TCAV_Avg = mean(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Max_Soil_Temp_TCAV = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, max(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    Daily_Min_Soil_Temp_TCAV = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, min(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  filter(!Daily_mean_Soil_Temp_TCAV_Avg<0)


# Reshape the data to long format
BH_2021_soil_tempper_treatment_long <- BH_2021_soil_temp_per_treatment %>%
  pivot_longer(cols = c(Daily_mean_Soil_Temp_TCAV_Avg, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



BH_soil_temp_2021_5cm_per_treatment<-ggplot(BH_2021_soil_tempper_treatment_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = " 2021",
       x = "Date",
       y = "Soil Temperature at 5 cm (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = 1,    # Full opacity
                                "Daily_Max_Soil_Temp_TCAV" = 0.6,   # Slight transparency
                                "Daily_Min_Soil_Temp_TCAV" = 0.3)) + # More transparency
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 45, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 45, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 45, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 10, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin =  10, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 10, ymax = 45,
           alpha = .2)+
  scale_y_continuous(breaks=seq(10,45,10), limits = c (10,45)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-03-28"), as.Date("2021-09-13"), by = "30 days")) +
  coord_cartesian(xlim = as.Date(c("2021-03-28", "2021-09-13"))) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())
  



ggsave("figures/BH_soil_temp_2021_5cm_per_treatment.jpg", plot = BH_soil_temp_2021_5cm_per_treatment, width = 10, height = 6, dpi = 600)  

#### soil temperature WITH 95% CONFIDENCE INTERVALS AND SUMMARIZING STATIONS PER TREATMENT

# Continue with the parsing and processing

BH_2021_Soil_temp_5cm_temp_grouped<- BH_met_2021_temps %>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))%>%
  mutate(treatment = as.factor(treatment)) 

BH_2021_Soil_temp_5cm_temp_per_station <- BH_2021_Soil_temp_5cm_temp_grouped %>%
  mutate(Date = as.Date(TIMESTAMP),
         soil_temp_5_cm = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by treatment, station, and date to get max/min/mean for each station per day
  group_by(treatment, Station, Date) %>%
  summarise(
    Max_Soil_Temp_TCAV_Avg_station = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, max(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    Min_Soil_Temp_TCAV_Avg_station = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, min(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    .groups = 'drop'
  )

# Now group by treatment and date, and calculate the mean and 95% CI across stations
BH_2021_Soil_temp_5cm_temp_per_treatment <- BH_2021_Soil_temp_5cm_temp_per_station %>%
  group_by(treatment, Date) %>%
  summarise(
    Mean_Max_Soil_Temp_TCAV_Avg = mean(Max_Soil_Temp_TCAV_Avg_station, na.rm = TRUE),
    Mean_Min_Soil_Temp_TCAV_Avg = mean(Min_Soil_Temp_TCAV_Avg_station, na.rm = TRUE),
    CI_Max_Soil_Temp_TCAV_Avg = 1.96 * sd(Max_Soil_Temp_TCAV_Avg_station, na.rm = TRUE) / sqrt(n()),
    CI_Min_Soil_Temp_TCAV_Avg = 1.96 * sd(Min_Soil_Temp_TCAV_Avg_station, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  filter(!Mean_Max_Soil_Temp_TCAV_Avg < 0, !Mean_Min_Soil_Temp_TCAV_Avg < 0)


write.csv(BH_2021_Soil_temp_5cm_temp_per_treatment,"data_output/BH_2021_Soil_temp_5cm_temp_per_treatment.csv")
#  for plotting
BH_2021_soil_temp_avg_per_treatment_long_with_CI <-ggplot(BH_2021_Soil_temp_5cm_temp_per_treatment, aes(x = Date)) +
  # Map linetype inside aes based on whether it's max or min temperature
  geom_line(aes(y = Mean_Max_Soil_Temp_TCAV_Avg, color = treatment, linetype = "Max Temp"), size =1.2) +
  geom_line(aes(y = Mean_Min_Soil_Temp_TCAV_Avg, color = treatment, linetype = "Min Temp"), size =1.2) +
  labs(title = "2021",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_linetype_manual(values = c("Max Temp" = "solid", "Min Temp" = "dashed"), 
                        name = "Temperature Type", 
                        labels = c("Mean_Max_Soil_Temp_TCAV_Avg", "Mean_Min_Soil_Temp_TCAV_Avg")) +
  
  # Add ribbons for 95% CI with same colors and treatment names
  geom_ribbon(aes(x = Date, ymin = Mean_Max_Soil_Temp_TCAV_Avg - CI_Max_Soil_Temp_TCAV_Avg, 
                  ymax = Mean_Max_Soil_Temp_TCAV_Avg + CI_Max_Soil_Temp_TCAV_Avg, fill = treatment), 
              alpha = 0.3) +
  geom_ribbon(aes(x = Date, ymin = Mean_Min_Soil_Temp_TCAV_Avg - CI_Min_Soil_Temp_TCAV_Avg, 
                  ymax = Mean_Min_Soil_Temp_TCAV_Avg + CI_Min_Soil_Temp_TCAV_Avg, fill = treatment), 
              alpha = 0.3) +
  
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                       labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  
  # Annotate heatwave events
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 45, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 45, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 45, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 10, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin =  10, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 10, ymax = 45,
           alpha = .2)+
  scale_y_continuous(breaks=seq(10,45,10), limits = c (10,45)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-03-28"), as.Date("2021-09-13"), by = "30 days")) +
  coord_cartesian(xlim = as.Date(c("2021-03-28", "2021-09-13"))) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())



BH_2021_soil_temp_avg_per_treatment_long_with_CI_legend <-ggplot(BH_2021_Soil_temp_5cm_temp_per_treatment, aes(x = Date)) +
  # Map linetype inside aes based on whether it's max or min temperature
  geom_line(aes(y = Mean_Max_Soil_Temp_TCAV_Avg, color = treatment, linetype = "Max Temp"), size =1.2) +
  geom_line(aes(y = Mean_Min_Soil_Temp_TCAV_Avg, color = treatment, linetype = "Min Temp"), size =1.2) +
  labs(title = "2021",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_linetype_manual(values = c("Max Temp" = "solid", "Min Temp" = "dashed"), 
                        name = "Temperature Type", 
                        labels = c("Daily mean Max temperature", "Daily mean Min temperature")) +
  
  # Add ribbons for 95% CI with same colors and treatment names
  geom_ribbon(aes(x = Date, ymin = Mean_Max_Soil_Temp_TCAV_Avg - CI_Max_Soil_Temp_TCAV_Avg, 
                  ymax = Mean_Max_Soil_Temp_TCAV_Avg + CI_Max_Soil_Temp_TCAV_Avg, fill = treatment), 
              alpha = 0.3) +
  geom_ribbon(aes(x = Date, ymin = Mean_Min_Soil_Temp_TCAV_Avg - CI_Min_Soil_Temp_TCAV_Avg, 
                  ymax = Mean_Min_Soil_Temp_TCAV_Avg + CI_Min_Soil_Temp_TCAV_Avg, fill = treatment), 
              alpha = 0.3) +
  
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                       labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  
  # Annotate heatwave events
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 45, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 45, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 45, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 10, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin =  10, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 10, ymax = 45,
           alpha = .2)+
  scale_y_continuous(breaks=seq(10,45,10), limits = c (10,45)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-03-28"), as.Date("2021-09-13"), by = "30 days")) +
  coord_cartesian(xlim = as.Date(c("2021-03-28", "2021-09-13"))) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)


ggsave("figures/BH_2021_soil_temp_avg_per_treatment_long_with_CI.jpg", plot = BH_2021_soil_temp_avg_per_treatment_long_with_CI, width = 10, height = 6, dpi = 600) 


#####IRT AVERAGE CANOPY TEMPERATURE 2020 & 2021 #####

# Explicitly handle problematic TIMESTAMP values
BH_2020_IRT_Avg_Canopy_temp <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         T_IRT_Avg  = as.numeric(T_IRT_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_IRT_Avg = mean(T_IRT_Avg, na.rm = TRUE),
    Daily_Max_Temp_IRT_Avg = max(T_IRT_Avg, na.rm = TRUE),
    Daily_Min_Temp_IRT_Avg = min(T_IRT_Avg, na.rm = TRUE),
#    IRT_Avg_temp_at_14 = T_IRT_Avg[hour(TIMESTAMP) == 14][1],  # Air temp at 14:00 (if available)   
    IRT_Avg_temp_at_14 = get_temp_at_14(TIMESTAMP_parsed, T_IRT_Avg),  # Use helper function       
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))%>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station== "19171 (B3R3)" ~ "B3R3"
  ))

BH_2020_IRT_Avg_Canopy_temp%>%
  group_by(Stations)%>%
  tally()


str(BH_2020_IRT_Avg_Canopy_temp)
# Reshape the data to long format
BH_2020_IRT_temp_avg_long <- BH_2020_IRT_Avg_Canopy_temp %>%
  pivot_longer(cols = c(Daily_mean_Temp_IRT_Avg, Daily_Max_Temp_IRT_Avg, Daily_Min_Temp_IRT_Avg, IRT_Avg_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_IRT_temp_Avg_canopy_2020_per_station<-ggplot(BH_2020_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Avg Temperature Trends 2020",
       x = "Date",
       y = "IRT Canopy Temperature (°C)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_IRT_temp_Avg_canopy_2020_per_station_panel_plot.jpg", plot = BH_IRT_temp_Avg_canopy_2020_per_station, width = 18, height = 13, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_IRT_temp_Avg_canopy_2020_per_station_one_plot<-ggplot(BH_2020_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Stations, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Avg Temperature Trends by Station and Temperature Type 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Temp_IRT_Avg" = "solid", 
                                   "Daily_Max_Temp_IRT_Avg" = "dashed", 
                                   "Daily_Min_Temp_IRT_Avg" = "dotted"))  # Line types for temp types

ggsave("figures/BH_IRT_temp_Avg_canopy_2020_per_station_one_plot.jpg", plot =BH_IRT_temp_Avg_canopy_2020_per_station_one_plot, width = 10, height = 6, dpi = 600)


BH_2020_IRT_Avg_Canopy_temp_per_treatment <- BH_met_2020_temps %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))%>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station== "19171 (B3R3)" ~ "B3R3"
  )) %>%
  mutate(treatment = as.factor(treatment)) 


BH_2020_IRT_Avg_Canopy_temp_per_treatment <- BH_2020_IRT_Avg_Canopy_temp_per_treatment %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         T_IRT_Avg  = as.numeric(T_IRT_Avg)) %>%
  # Group by Station and Date
  group_by(treatment, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_IRT_Avg = mean(T_IRT_Avg, na.rm = TRUE),
    Daily_Max_Temp_IRT_Avg = max(T_IRT_Avg, na.rm = TRUE),
    Daily_Min_Temp_IRT_Avg = min(T_IRT_Avg, na.rm = TRUE),
    .groups = 'drop'  # Prevent grouped output warning
)


# Reshape the data to long format
BH_2020_IRT_temp_avg_per_treatment_long <- BH_2020_IRT_Avg_Canopy_temp_per_treatment %>%
  pivot_longer(cols = c(Daily_mean_Temp_IRT_Avg, Daily_Max_Temp_IRT_Avg, Daily_Min_Temp_IRT_Avg),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")

BH_IRT_temp_Avg_canopy_2020_per_treatment<-ggplot(BH_2020_IRT_temp_avg_per_treatment_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title =  "Canopy IRT Avg Temperature Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "IRT Canopy Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_IRT_Avg" = 1,    # Full opacity
                                "Daily_Max_Temp_IRT_Avg" = 0.6,   # Slight transparency
                                "Daily_Min_Temp_IRT_Avg" = 0.3)) + # More transparency
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 0, ymax =  45,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 0, ymax =  45,
           alpha = .2)+
  annotate("text",  x = as.Date("2020-08-15", "%Y-%m-%d"), y =  45, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y =  45, label ="HW4", size = 7) 
ggsave("figures/BH_IRT_temp_Avg_canopy_2020_per_treatment.jpg", plot = BH_IRT_temp_Avg_canopy_2020_per_treatment, width = 10, height = 6, dpi = 600)

#### IRT CANOPY WITH 95% CONFIDENCE INTERVALS AND SUMMARIZING STATIONS PER TREATMENT

# Continue with the parsing and processing

BH_2020_IRT_Avg_Canopy_temp_grouped<- BH_met_2020_temps %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))%>%
  mutate(treatment = as.factor(treatment)) 

BH_2020_IRT_Avg_Canopy_temp_per_station <- BH_2020_IRT_Avg_Canopy_temp_grouped %>%
  mutate(Date = as.Date(TIMESTAMP),
         T_IRT_Avg = as.numeric(T_IRT_Avg)) %>%
  # Group by treatment, station, and date to get max/min/mean for each station per day
  group_by(treatment, Station, Date) %>%
  summarise(
    Max_Temp_IRT_Avg_station = ifelse(all(is.na(T_IRT_Avg)), NA, max(T_IRT_Avg, na.rm = TRUE)),
    Min_Temp_IRT_Avg_station = ifelse(all(is.na(T_IRT_Avg)), NA, min(T_IRT_Avg, na.rm = TRUE)),
    Mean_Temp_IRT_Avg_station = mean(T_IRT_Avg, na.rm = TRUE),
    .groups = 'drop'
  )

# Now group by treatment and date, and calculate the mean and 95% CI across stations
BH_2020_IRT_Avg_Canopy_temp_per_treatment <- BH_2020_IRT_Avg_Canopy_temp_per_station %>%
  group_by(treatment, Date) %>%
  summarise(
    Mean_Max_Temp_IRT_Avg = mean(Max_Temp_IRT_Avg_station, na.rm = TRUE),
    Mean_Min_Temp_IRT_Avg = mean(Min_Temp_IRT_Avg_station, na.rm = TRUE),
    Mean_Temp_IRT_Avg = mean(Mean_Temp_IRT_Avg_station, na.rm = TRUE),
    CI_Max_Temp_IRT_Avg = 1.96 * sd(Max_Temp_IRT_Avg_station, na.rm = TRUE) / sqrt(n()),
    CI_Min_Temp_IRT_Avg = 1.96 * sd(Min_Temp_IRT_Avg_station, na.rm = TRUE) / sqrt(n()),
    CI_Mean_Temp_IRT_Avg = 1.96 * sd(Mean_Temp_IRT_Avg_station, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  filter(!Mean_Max_Temp_IRT_Avg < 0, !Mean_Min_Temp_IRT_Avg < 0)



#  for plotting
BH_2020_IRT_temp_avg_per_treatment_long_with_CI <- ggplot(BH_2020_IRT_Avg_Canopy_temp_per_treatment, aes(x = Date)) +
  geom_line(aes(y = Mean_Max_Temp_IRT_Avg, color = treatment, alpha = "Mean_Max_Temp_IRT_Avg"), linetype = "solid") +
  geom_line(aes(y = Mean_Temp_IRT_Avg, color = treatment, alpha = "Mean_Temp_IRT_Avg"), linetype = "solid") +
  geom_line(aes(y = Mean_Min_Temp_IRT_Avg, color = treatment, alpha = "Mean_Min_Temp_IRT_Avg"), linetype = "solid") +
  labs(title = "Canopy IRT Avg Temperature Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "IRT Canopy Temperature (°C)") +
  theme_minimal() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_alpha_manual(values = c("Mean_Max_Temp_IRT_Avg" = 1, "Mean_Temp_IRT_Avg" = 0.7, "Mean_Min_Temp_IRT_Avg" = 0.3), name = "Temperature Type") + 
  # Add ribbons for 95% CI with same colors and treatment names
  geom_ribbon(aes(x = Date, ymin = Mean_Max_Temp_IRT_Avg - CI_Max_Temp_IRT_Avg, ymax = Mean_Max_Temp_IRT_Avg + CI_Max_Temp_IRT_Avg, fill = treatment), 
              inherit.aes = FALSE, alpha = 0.2) +
  geom_ribbon(aes(x = Date, ymin = Mean_Min_Temp_IRT_Avg - CI_Min_Temp_IRT_Avg, ymax = Mean_Min_Temp_IRT_Avg + CI_Min_Temp_IRT_Avg, fill = treatment), 
              inherit.aes = FALSE, alpha = 0.2) +
  geom_ribbon(aes(x = Date, ymin = Mean_Temp_IRT_Avg - CI_Mean_Temp_IRT_Avg, ymax = Mean_Temp_IRT_Avg + CI_Mean_Temp_IRT_Avg, fill = treatment), 
              inherit.aes = FALSE, alpha = 0.2) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                       labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +  # Same colors for fill
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 0, ymax =  45,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 0, ymax =  45,
           alpha = .2)+
  annotate("text",  x = as.Date("2020-08-15", "%Y-%m-%d"), y =  45, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y =  45, label ="HW4", size = 7) 

 
ggsave("figures/BH_2020_IRT_temp_avg_per_treatment_long_with_CI.jpg", plot = BH_2020_IRT_temp_avg_per_treatment_long_with_CI, width = 10, height = 6, dpi = 600) 




#####IRT 2021

BH_2021_IRT_Avg_Canopy_temp <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         T_IRT_Avg  = as.numeric(T_IRT_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_IRT_Avg = mean(T_IRT_Avg, na.rm = TRUE),
    Daily_Max_Temp_IRT_Avg = ifelse(all(is.na(T_IRT_Avg)), NA, max(T_IRT_Avg, na.rm = TRUE)),
    Daily_Min_Temp_IRT_Avg = ifelse(all(is.na(T_IRT_Avg)), NA, min(T_IRT_Avg, na.rm = TRUE)),
 #   IRT_Avg_temp_at_14 = T_IRT_Avg[hour(TIMESTAMP) == 14][1],  # Air temp at 14:00 (if available)  
    IRT_Avg_temp_at_14 = get_temp_at_14(TIMESTAMP, T_IRT_Avg),  # Use helper function     
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
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
  ))%>%
  filter(!Daily_mean_Temp_IRT_Avg<0)

# Reshape the data to long format
BH_2021_IRT_temp_avg_long <- BH_2021_IRT_Avg_Canopy_temp %>%
  pivot_longer(cols = c(Daily_mean_Temp_IRT_Avg, Daily_Max_Temp_IRT_Avg, Daily_Min_Temp_IRT_Avg,IRT_Avg_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_IRT_temp_Avg_canopy_2021_per_station<-ggplot(BH_2021_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Avg Temperature Trends 2021",
       x = "Date",
       y = "Temperature (°C)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_IRT_temp_Avg_canopy_2021_per_station_panel_plot.jpg", plot = BH_IRT_temp_Avg_canopy_2021_per_station, width = 18, height = 13, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_IRT_temp_Avg_canopy_2021_per_station_one_plot<-ggplot(BH_2021_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Station, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Avg Temperature Trends by Station and Temperature Type 2021",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Temp_IRT_Avg" = "solid", 
                                   "Daily_Max_Temp_IRT_Avg" = "dashed", 
                                   "Daily_Min_Temp_IRT_Avg" = "dotted"))  # Line types for temp types

ggsave("figures/BH_IRT_temp_Avg_canopy_2021_per_station_one_plot.jpg", plot = BH_IRT_temp_Avg_canopy_2021_per_station_one_plot, width = 10, height = 6, dpi = 600)


str(BH_2021_IRT_temp_avg_long)


BH_2021_IRT_Avg_Canopy_temp_per_treatment<- BH_met_2021_temps %>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))%>%
  mutate(treatment = as.factor(treatment)) 

str(BH_2021_IRT_Avg_Canopy_temp_per_treatment)

BH_2021_IRT_Avg_Canopy_temp_per_treatment <- BH_2021_IRT_Avg_Canopy_temp_per_treatment  %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         T_IRT_Avg  = as.numeric(T_IRT_Avg)) %>%
  # Group by Station and Date
  group_by(treatment, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_IRT_Avg = mean(T_IRT_Avg, na.rm = TRUE),
    Daily_Max_Temp_IRT_Avg = ifelse(all(is.na(T_IRT_Avg)), NA, max(T_IRT_Avg, na.rm = TRUE)),
    Daily_Min_Temp_IRT_Avg = ifelse(all(is.na(T_IRT_Avg)), NA, min(T_IRT_Avg, na.rm = TRUE)),
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  filter(!Daily_mean_Temp_IRT_Avg<0)

# Reshape the data to long format
BH_2021_IRT_temp_avg_per_treatment_long <- BH_2021_IRT_Avg_Canopy_temp_per_treatment %>%
  pivot_longer(cols = c(Daily_mean_Temp_IRT_Avg, Daily_Max_Temp_IRT_Avg, Daily_Min_Temp_IRT_Avg),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



BH_IRT_temp_Avg_canopy_2021_per_treatment<-ggplot(BH_2021_IRT_temp_avg_per_treatment_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Canopy IRT Avg Temperature Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "IRT Canopy Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_IRT_Avg" = 1,    # Full opacity
                                "Daily_Max_Temp_IRT_Avg" = 0.6,   # Slight transparency
                                "Daily_Min_Temp_IRT_Avg" = 0.3)) + # More transparency
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 50, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 50, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 50, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0, ymax = 50,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin =  0, ymax = 50,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 0, ymax = 50,
           alpha = .2)

ggsave("figures/BH_IRT_temp_Avg_canopy_2021_per_treatment.jpg", plot = BH_IRT_temp_Avg_canopy_2021_per_treatment, width = 10, height = 6, dpi = 600)  

#### IRT CANOPY WITH 95% CONFIDENCE INTERVALS AND SUMMARIZING STATIONS PER TREATMENT

# Continue with the parsing and processing

BH_2021_IRT_Avg_Canopy_temp_grouped<- BH_met_2021_temps %>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))%>%
  mutate(treatment = as.factor(treatment)) 

BH_2021_IRT_Avg_Canopy_temp_per_station <- BH_2021_IRT_Avg_Canopy_temp_grouped %>%
  mutate(Date = as.Date(TIMESTAMP),
         T_IRT_Avg = as.numeric(T_IRT_Avg)) %>%
  # Group by treatment, station, and date to get max/min/mean for each station per day
  group_by(treatment, Station, Date) %>%
  summarise(
    Max_Temp_IRT_Avg_station = ifelse(all(is.na(T_IRT_Avg)), NA, max(T_IRT_Avg, na.rm = TRUE)),
    Min_Temp_IRT_Avg_station = ifelse(all(is.na(T_IRT_Avg)), NA, min(T_IRT_Avg, na.rm = TRUE)),
    Mean_Temp_IRT_Avg_station = mean(T_IRT_Avg, na.rm = TRUE),
    .groups = 'drop'
  )


# Now group by treatment and date, and calculate the mean and 95% CI across stations
BH_2021_IRT_Avg_Canopy_temp_per_treatment <- BH_2021_IRT_Avg_Canopy_temp_per_station %>%
  group_by(treatment, Date) %>%
  summarise(
    Mean_Max_Temp_IRT_Avg = mean(Max_Temp_IRT_Avg_station, na.rm = TRUE),
    Mean_Min_Temp_IRT_Avg = mean(Min_Temp_IRT_Avg_station, na.rm = TRUE),
    Mean_Temp_IRT_Avg = mean(Mean_Temp_IRT_Avg_station, na.rm = TRUE),
    CI_Max_Temp_IRT_Avg = 1.96 * sd(Max_Temp_IRT_Avg_station, na.rm = TRUE) / sqrt(n()),
    CI_Min_Temp_IRT_Avg = 1.96 * sd(Min_Temp_IRT_Avg_station, na.rm = TRUE) / sqrt(n()),
    CI_Mean_Temp_IRT_Avg = 1.96 * sd(Mean_Temp_IRT_Avg_station, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  filter(!Mean_Max_Temp_IRT_Avg < 0, !Mean_Min_Temp_IRT_Avg < 0)

# plotting
###
BH_2021_IRT_temp_avg_per_treatment_long_with_CI <- ggplot(BH_2021_IRT_Avg_Canopy_temp_per_treatment, aes(x = Date)) +
  geom_line(aes(y = Mean_Max_Temp_IRT_Avg, color = treatment, alpha = "Mean_Max_Temp_IRT_Avg"), linetype = "solid") +
  geom_line(aes(y = Mean_Temp_IRT_Avg, color = treatment, alpha = "Mean_Temp_IRT_Avg"), linetype = "solid") +
  geom_line(aes(y = Mean_Min_Temp_IRT_Avg, color = treatment, alpha = "Mean_Min_Temp_IRT_Avg"), linetype = "solid") +
  labs(title = "Canopy IRT Avg Temperature Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "IRT Canopy Temperature (°C)") +
  theme_minimal() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET")) +
  scale_alpha_manual(values = c("Mean_Max_Temp_IRT_Avg" = 1, "Mean_Temp_IRT_Avg" = 0.7, "Mean_Min_Temp_IRT_Avg" = 0.3), name = "Temperature Type") + 
  # Add ribbons for 95% CI with same colors and treatment names
  geom_ribbon(aes(x = Date, ymin = Mean_Max_Temp_IRT_Avg - CI_Max_Temp_IRT_Avg, ymax = Mean_Max_Temp_IRT_Avg + CI_Max_Temp_IRT_Avg, fill = treatment), 
              inherit.aes = FALSE, alpha = 0.2) +
  geom_ribbon(aes(x = Date, ymin = Mean_Min_Temp_IRT_Avg - CI_Min_Temp_IRT_Avg, ymax = Mean_Min_Temp_IRT_Avg + CI_Min_Temp_IRT_Avg, fill = treatment), 
              inherit.aes = FALSE, alpha = 0.2) +
  geom_ribbon(aes(x = Date, ymin = Mean_Temp_IRT_Avg - CI_Mean_Temp_IRT_Avg, ymax = Mean_Temp_IRT_Avg + CI_Mean_Temp_IRT_Avg, fill = treatment), 
              inherit.aes = FALSE, alpha = 0.2) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                       labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET")) +  # Same colors for fill
  annotate("text", x = as.Date("2021-09-08"), y = 50, label = "HW3", size = 6) +
  annotate("text", x = as.Date("2021-07-10"), y = 50, label = "HW2", size = 6) +
  annotate("text", x = as.Date("2021-06-18"), y = 50, label = "HW1", size = 6) +
  annotate("rect", xmin = as.Date("2021-09-07"), xmax = as.Date("2021-09-09"), ymin = 0, ymax = 50, alpha = .2) +
  annotate("rect", xmin = as.Date("2021-07-08"), xmax = as.Date("2021-07-11"), ymin = 0, ymax = 50, alpha = .2) +
  annotate("rect", xmin = as.Date("2021-06-17"), xmax = as.Date("2021-06-20"), ymin = 0, ymax = 50, alpha = .2)

ggsave("figures/BH_2021_IRT_temp_avg_per_treatment_long_with_CI.jpg", plot = BH_2021_IRT_temp_avg_per_treatment_long_with_CI, width = 10, height = 6, dpi = 600)


####
  

#####IRT MAXIMUM CANOPY TEMPERATURE 2020 & 2021 #####

# Explicitly handle problematic TIMESTAMP values
BH_2020_IRT_Max_Canopy_temp <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         T_IRT_Max  = as.numeric(T_IRT_Max)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_IRT_Max = mean(T_IRT_Max, na.rm = TRUE),
    Daily_Max_Temp_IRT_Max = max(T_IRT_Max, na.rm = TRUE),
    Daily_Min_Temp_IRT_Max = min(T_IRT_Max, na.rm = TRUE),
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))%>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station== "19171 (B3R3)" ~ "B3R3"
  ))

BH_2020_IRT_Max_Canopy_temp%>%
  group_by(Stations)%>%
  tally()


str(BH_2020_IRT_Max_Canopy_temp)
# Reshape the data to long format
BH_2020_IRT_temp_avg_long <- BH_2020_IRT_Max_Canopy_temp %>%
  pivot_longer(cols = c(Daily_mean_Temp_IRT_Max, Daily_Max_Temp_IRT_Max, Daily_Min_Temp_IRT_Max),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_IRT_temp_Max_canopy_2020_per_station<-ggplot(BH_2020_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Max Temperature Trends 2020",
       x = "Date",
       y = "Temperature (°C)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_IRT_temp_Max_canopy_2020_per_station_panel_plot.jpg", plot = BH_IRT_temp_Max_canopy_2020_per_station, width = 18, height = 13, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_IRT_temp_Max_canopy_2020_per_station_one_plot<-ggplot(BH_2020_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Stations, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Max Temperature Trends by Station and Temperature Type 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Temp_IRT_Max" = "solid", 
                                   "Daily_Max_Temp_IRT_Max" = "dashed", 
                                   "Daily_Min_Temp_IRT_Max" = "dotted"))  # Line types for temp types

ggsave("figures/BH_IRT_temp_Max_canopy_2020_per_station_one_plot.jpg", plot =BH_IRT_temp_Max_canopy_2020_per_station_one_plot, width = 10, height = 6, dpi = 600)


str(BH_2020_IRT_temp_avg_long)
BH_2020_IRT_temp_avg_long <- BH_2020_IRT_temp_avg_long %>%
  mutate(treatment = as.factor(treatment)) 



BH_IRT_temp_Max_canopy_2020_per_treatment<-ggplot(BH_2020_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title =  "Canopy IRT Max Temperature Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_IRT_Max" = 1,    # Full opacity
                                "Daily_Max_Temp_IRT_Max" = 0.6,   # Slight transparency
                                "Daily_Min_Temp_IRT_Max" = 0.3))  # More transparency

ggsave("figures/BH_IRT_temp_Max_canopy_2020_per_treatment.jpg", plot = BH_IRT_temp_Max_canopy_2020_per_treatment, width = 10, height = 6, dpi = 600)

#####IRT 2021

BH_2021_IRT_Max_Canopy_temp <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         T_IRT_Max  = as.numeric(T_IRT_Max)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_IRT_Max = mean(T_IRT_Max, na.rm = TRUE),
    Daily_Max_Temp_IRT_Max = ifelse(all(is.na(T_IRT_Max)), NA, max(T_IRT_Max, na.rm = TRUE)),
    Daily_Min_Temp_IRT_Max = ifelse(all(is.na(T_IRT_Max)), NA, min(T_IRT_Max, na.rm = TRUE)),
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
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
  ))%>%
  filter(!Daily_mean_Temp_IRT_Max<0)

# Reshape the data to long format
BH_2021_IRT_temp_avg_long <- BH_2021_IRT_Max_Canopy_temp %>%
  pivot_longer(cols = c(Daily_mean_Temp_IRT_Max, Daily_Max_Temp_IRT_Max, Daily_Min_Temp_IRT_Max),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_IRT_temp_Max_canopy_2021_per_station<-ggplot(BH_2021_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Max Temperature Trends 2021",
       x = "Date",
       y = "Temperature (°C)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_IRT_temp_Max_canopy_2021_per_station_panel_plot.jpg", plot = BH_IRT_temp_Max_canopy_2021_per_station, width = 18, height = 13, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_IRT_temp_Max_canopy_2021_per_station_one_plot<-ggplot(BH_2021_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Station, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Canopy IRT Max Temperature Trends by Station and Temperature Type 2021",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Temp_IRT_Max" = "solid", 
                                   "Daily_Max_Temp_IRT_Max" = "dashed", 
                                   "Daily_Min_Temp_IRT_Max" = "dotted"))  # Line types for temp types

ggsave("figures/BH_IRT_temp_Max_canopy_2021_per_station_one_plot.jpg", plot = BH_IRT_temp_Max_canopy_2021_per_station_one_plot, width = 10, height = 6, dpi = 600)


str(BH_2021_IRT_temp_avg_long)
BH_2021_IRT_temp_avg_long <- BH_2021_IRT_temp_avg_long %>%
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
  ))%>%
  mutate(treatment = as.factor(treatment)) 

str(BH_2021_IRT_temp_avg_long)


BH_IRT_temp_Max_canopy_2021_per_treatment<-ggplot(BH_2021_IRT_temp_avg_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Canopy IRT Max Temperature Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_IRT_Max" = 1,    # Full opacity
                                "Daily_Max_Temp_IRT_Max" = 0.6,   # Slight transparency
                                "Daily_Min_Temp_IRT_Max" = 0.3))  # More transparency

ggsave("figures/BH_IRT_temp_Max_canopy_2021_per_treatment.jpg", plot = BH_IRT_temp_Max_canopy_2021_per_treatment, width = 10, height = 6, dpi = 600)  


#####AIR Temperature Average 2020 & 2021 #####


BH_2020_Air_temp_Avg <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         AirTC_Avg  = as.numeric(AirTC_Avg)) %>% 
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values, ensuring no all-NA cases
  summarise(
    Daily_mean_Temp_Air_Avg = mean(AirTC_Avg, na.rm = TRUE),
    Daily_Max_Temp_Air_Avg = ifelse(all(is.na(AirTC_Avg)), NA, max(AirTC_Avg, na.rm = TRUE)),
    Daily_Min_Temp_Air_Avg = ifelse(all(is.na(AirTC_Avg)), NA, min(AirTC_Avg, na.rm = TRUE)),
    Air_Avg_temp_at_14 = get_temp_at_14(TIMESTAMP_parsed, AirTC_Avg),  # Use helper function   
    .groups = 'drop'  # Prevent grouped output warning
  ) %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station == "19171 (B3R3)" ~ 3
  )) %>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station == "19171 (B3R3)" ~ "B3R3"
  ))%>%
  filter(!Daily_Min_Temp_Air_Avg<0)

BH_2020_Air_temp_Avg%>%
  group_by(Stations)%>%
  tally()


str(BH_2020_Air_temp_Avg)
# Reshape the data to long format
BH_2020_Air_temp_avg_long <- BH_2020_Air_temp_Avg %>%
  pivot_longer(cols = c(Daily_mean_Temp_Air_Avg, Daily_Max_Temp_Air_Avg, Daily_Min_Temp_Air_Avg,Air_Avg_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_Air_temp_Avg_2020_per_station<-ggplot(BH_2020_Air_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Air Temperature Avg Trends 2020",
       x = "Date",
       y = "Air Temperature (°C)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_Air_temp_Avg_2020_per_station_panel_plot.jpg", plot = BH_Air_temp_Avg_2020_per_station, width = 16, height = 6, dpi = 600)



str(BH_2020_Air_temp_avg_long)
BH_2020_Air_temp_avg_long <- BH_2020_Air_temp_avg_long %>%
  mutate(treatment = as.factor(treatment)) 

BH_2020_Air_temp_avg_long_filtered <- BH_2020_Air_temp_avg_long %>%
filter(Temperature_Type!= "Air_Avg_temp_at_14")

BH_Air_temp_Avg_2020_per_treatment<-ggplot(BH_2020_Air_temp_avg_long_filtered, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title =  "Air Temperature Avg Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "Air Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", "B2R2 2x baseline ET", "B3R2 3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_Air_Avg" = 1,    # Full opacity
                                "Daily_Max_Temp_Air_Avg" = 0.6,   # Slight transparency
                                "Daily_Min_Temp_Air_Avg" = 0.3))+  # More transparency
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 5, ymax =  45,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 5, ymax =  45,
           alpha = .2)+
  annotate("text",  x = as.Date("2020-08-15", "%Y-%m-%d"), y =  45, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y =  45, label ="HW4", size = 7) 
ggsave("figures/BH_Air_temp_Avg_2020_per_treatment.jpg", plot = BH_Air_temp_Avg_2020_per_treatment, width = 10, height = 6, dpi = 600)

BH_2020_Air_temp_avg_long_filtered_2 <- BH_2020_Air_temp_avg_long %>%
  filter(Temperature_Type== "Air_Avg_temp_at_14")

BH_Air_temp_Avg_2020_per_treatment_at_midday<-ggplot(BH_2020_Air_temp_avg_long_filtered_2, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title =  "Air Temperature Avg at Midday by Treatment and Temperature Type 2020",
       x = "Date",
       y = "Air Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", "B2R2 2x baseline ET", "B3R2 3x baseline ET")) +
  scale_alpha_manual(values = c("Air_Avg_temp_at_14" = 1))

ggsave("figures/BH_Air_temp_Avg_2020_per_treatment_at_midday.jpg", plot = BH_Air_temp_Avg_2020_per_treatment_at_midday, width = 10, height = 6, dpi = 600)



#####AIR TEMPERATURE  2021

BH_2021_Air_temp_Avg <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         AirTC_Avg  = as.numeric(AirTC_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_Air_Avg = mean(AirTC_Avg, na.rm = TRUE),
    Daily_Max_Temp_Air_Avg = ifelse(all(is.na(AirTC_Avg)), NA, max(AirTC_Avg, na.rm = TRUE)),
    Daily_Min_Temp_Air_Avg = ifelse(all(is.na(AirTC_Avg)), NA, min(AirTC_Avg, na.rm = TRUE)),
 #   Air_Avg_temp_at_14 = AirTC_Avg[hour(TIMESTAMP) == 14][1],  # Air temp at 14:00 (if available)   
    Air_Avg_temp_at_14 = get_temp_at_14(TIMESTAMP, AirTC_Avg),  # Use helper function     
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
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
  ))%>%
  filter(!Daily_Min_Temp_Air_Avg<0)

str(BH_2021_Air_temp_Avg)

# Reshape the data to long format
BH_2021_Air_temp_avg_long <- BH_2021_Air_temp_Avg %>%
  pivot_longer(cols = c(Daily_mean_Temp_Air_Avg, Daily_Max_Temp_Air_Avg, Daily_Min_Temp_Air_Avg,Air_Avg_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_Air_temp_Avg_2021_per_station<-ggplot(BH_2021_Air_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Air Temperature Avg Trends 2021",
       x = "Date",
       y = "Air Temperature (°C)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_Air_temp_Avg_2021_per_station_panel_plot.jpg", plot = BH_Air_temp_Avg_2021_per_station, width = 16, height = 6, dpi = 600)


str(BH_2021_Air_temp_avg_long)
BH_2021_Air_temp_avg_long <- BH_2021_Air_temp_avg_long %>%
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
  ))%>%
  mutate(treatment = as.factor(treatment)) 

str(BH_2021_Air_temp_avg_long)


BH_2021_Air_temp_avg_long_filtered<-BH_2021_Air_temp_avg_long%>%
  filter(Temperature_Type!= "Air_Avg_temp_at_14")

BH_Air_temp_Avg_2021_per_treatment<-ggplot(BH_2021_Air_temp_avg_long_filtered, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Air Temperature Avg Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "Air Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", "B2R2 1.5x baseline ET", "B3R2 2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_Air_Avg" = 1,    # Full opacity
                                "Daily_Max_Temp_Air_Avg" = 0.6,   # Slight transparency
                                "Daily_Min_Temp_Air_Avg" = 0.3)) +# More transparency
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 45, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 45, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 45, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin =  0, ymax = 45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 0, ymax = 45,
           alpha = .2)

ggsave("figures/BH_Air_temp_Avg_2021_per_treatment.jpg", plot = BH_Air_temp_Avg_2021_per_treatment, width = 10, height = 6, dpi = 600)  

BH_2021_Air_temp_avg_long_filtered_2<-BH_2021_Air_temp_avg_long%>%
  filter(Temperature_Type== "Air_Avg_temp_at_14")

BH_Air_temp_Avg_2021_per_treatment_at_midday<-ggplot(BH_2021_Air_temp_avg_long_filtered_2, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Air Temperature Avg Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "Air Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", "B2R2 1.5x baseline ET", "B3R2 2x baseline ET")) +
  scale_alpha_manual(values = c("Air_Avg_temp_at_14" = 1))

ggsave("figures/BH_Air_temp_Avg_2021_per_treatment_at_midday.jpg", plot = BH_Air_temp_Avg_2021_per_treatment_at_midday, width = 10, height = 6, dpi = 600)  


#####RELATIVE HUMIDITY Average 2020 & 2021 #####

BH_2020_rel_hum_Avg <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         RH  = as.numeric(RH)) %>% 
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values, ensuring no all-NA cases
  summarise(
    Daily_mean_rel_hum_Avg = mean(RH, na.rm = TRUE),
    Daily_Max_rel_hum_Avg = ifelse(all(is.na(RH)), NA, max(RH, na.rm = TRUE)),
    Daily_Min_rel_hum_Avg = ifelse(all(is.na(RH)), NA, min(RH, na.rm = TRUE)),
    RH_at_14 = get_temp_at_14(TIMESTAMP_parsed, RH),  # Use helper function     
    .groups = 'drop'  # Prevent grouped output warning
  ) %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station == "19171 (B3R3)" ~ 3
  )) %>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station == "19171 (B3R3)" ~ "B3R3"
  ))%>%
  filter(!Daily_Min_rel_hum_Avg<0)

BH_2020_rel_hum_Avg%>%
  group_by(Stations)%>%
  tally()


str(BH_2020_rel_hum_Avg)
# Reshape the data to long format
BH_2020_rel_hum_Avg_long <- BH_2020_rel_hum_Avg %>%
  pivot_longer(cols = c(Daily_mean_rel_hum_Avg, Daily_Max_rel_hum_Avg, Daily_Min_rel_hum_Avg),
               names_to = "RH_Type",
               values_to = "RH_Value")



# Plot with ggplot

BH_rel_hum_Avg_2020_per_station<-ggplot(BH_2020_rel_hum_Avg_long, aes(x = Date, y = RH_Value, color = RH_Type)) +
  geom_line() +  # Line plot
  labs(title = "Relative Humidity Average Trends 2020",
       x = "Date",
       y = "Relative Humidity (%)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_rel_hum_Avg_2020_per_station_panel_plot.jpg", plot = BH_rel_hum_Avg_2020_per_station, width = 16, height = 6, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_rel_hum_Avg_2020_per_station_one_plot<-ggplot(BH_2020_rel_hum_Avg_long, aes(x = Date, y = RH_Value, color = Stations, linetype = RH_Type)) +
  geom_line() +  # Line plot
  labs(title = "Relative Humidity Average Trends by Station and Temperature Type 2020",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_rel_hum_Avg" = "solid", 
                                   "Daily_Max_rel_hum_Avg" = "dashed", 
                                   "Daily_Min_rel_hum_Avg" = "dotted"))  # Line types for temp types

ggsave("figures/BH_rel_hum_Avg_2020_per_station_one_plot.jpg", plot =BH_rel_hum_Avg_2020_per_station_one_plot, width = 10, height = 6, dpi = 600)


str(BH_2020_rel_hum_Avg_long)
BH_2020_rel_hum_Avg_long <- BH_2020_rel_hum_Avg_long %>%
  mutate(treatment = as.factor(treatment)) 



BH_rel_hum_Avg_2020_per_treatment<-ggplot(BH_2020_rel_hum_Avg_long, aes(x = Date, y = RH_Value, color = treatment, alpha = RH_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title =  "Relative Humidity Average Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_rel_hum_Avg" = 1,    # Full opacity
                                "Daily_Max_rel_hum_Avg" = 0.6,   # Slight transparency
                                "Daily_Min_rel_hum_Avg" = 0.3))  # More transparency

ggsave("figures/BH_rel_hum_Avg_2020_per_treatment.jpg", plot = BH_rel_hum_Avg_2020_per_treatment, width = 10, height = 6, dpi = 600)

#####RELATIVE HUMIDITY  2021

BH_2021_rel_hum_Avg <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         RH  = as.numeric(RH)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_rel_hum_Avg = mean(RH, na.rm = TRUE),
    Daily_Max_rel_hum_Avg = ifelse(all(is.na(RH)), NA, max(RH, na.rm = TRUE)),
    Daily_Min_rel_hum_Avg = ifelse(all(is.na(RH)), NA, min(RH, na.rm = TRUE)),
    RH_at_14 = get_temp_at_14(TIMESTAMP, RH),  # Use helper function     
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))%>%
  filter(!Daily_Min_rel_hum_Avg<0)

# Reshape the data to long format
BH_2021_rel_hum_Avg_long <- BH_2021_rel_hum_Avg %>%
  pivot_longer(cols = c(Daily_mean_rel_hum_Avg, Daily_Max_rel_hum_Avg, Daily_Min_rel_hum_Avg),
               names_to = "RH_Type",
               values_to = "RH_Value")



# Plot with ggplot

BH_rel_hum_Avg_2021_per_station<-ggplot(BH_2021_rel_hum_Avg_long, aes(x = Date, y = RH_Value, color = RH_Type)) +
  geom_line() +  # Line plot
  labs(title = "Relative Humidity Average Trends 2021",
       x = "Date",
       y = "Relative Humidity (%)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_rel_hum_Avg_2021_per_station_panel_plot.jpg", plot = BH_rel_hum_Avg_2021_per_station, width = 16, height = 6, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_rel_hum_Avg_2021_per_station_one_plot<-ggplot(BH_2021_rel_hum_Avg_long, aes(x = Date, y = RH_Value, color = Station, linetype = RH_Type)) +
  geom_line() +  # Line plot
  labs(title = "Relative Humidity Average Trends by Station and Temperature Type 2021",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_rel_hum_Avg" = "solid", 
                                   "Daily_Max_rel_hum_Avg" = "dashed", 
                                   "Daily_Min_rel_hum_Avg" = "dotted"))  # Line types for temp types

ggsave("figures/BH_rel_hum_Avg_2021_per_station_one_plot.jpg", plot = BH_rel_hum_Avg_2021_per_station_one_plot, width = 10, height = 6, dpi = 600)


str(BH_2021_rel_hum_Avg_long)
BH_2021_rel_hum_Avg_long <- BH_2021_rel_hum_Avg_long %>%
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
  ))%>%
  mutate(treatment = as.factor(treatment)) 

str(BH_2021_rel_hum_Avg_long)


BH_rel_hum_Avg_2021_per_treatment<-ggplot(BH_2021_rel_hum_Avg_long, aes(x = Date, y = RH_Value, color = treatment, alpha = RH_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Relative Humidity Average Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", "B2R2 1.5x baseline ET", "B3R2 2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_rel_hum_Avg" = 1,    # Full opacity
                                "Daily_Max_rel_hum_Avg" = 0.6,   # Slight transparency
                                "Daily_Min_rel_hum_Avg" = 0.3))  # More transparency

ggsave("figures/BH_rel_hum_Avg_2021_per_treatment.jpg", plot = BH_rel_hum_Avg_2021_per_treatment, width = 10, height = 6, dpi = 600)  

##### Calculate VPD based on AirTC_avg and RH Avg 2020 and 2021####


# Example dataset: Assuming you have DateTime, Temperature_C, and RH columns
BH_2020_daily_air_VPD<- BH_met_2020_temps%>%
  select(TIMESTAMP, Station, AirTC_Avg, RH)

str(BH_2020_daily_air_VPD)


BH_2020_daily_air_VPD <- BH_2020_daily_air_VPD %>%
  filter(TIMESTAMP != "NAN")

BH_2020_daily_air_VPD <- BH_2020_daily_air_VPD %>%
  filter(AirTC_Avg != "NAN", RH != "NAN")


BH_2020_daily_air_VPD <- BH_2020_daily_air_VPD %>%
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP),   # Convert TIMESTAMP to POSIXct
         AirTC_Avg = as.numeric(AirTC_Avg), # Convert AirTC_Avg to numeric
         RH = as.numeric(RH))                 # Convert RH to numeric

BH_2020_daily_air_VPD%>%
  group_by(Station)%>%
  tally()

# Calculate Vapor Pressure
BH_2020_daily_air_VPD <- BH_2020_daily_air_VPD %>%
  mutate(es = 6.112 * exp((17.67 * AirTC_Avg) / (AirTC_Avg + 243.5)),  # Saturation vapor pressure in hPa
         ea = (RH / 100) * es)  # Actual vapor pressure in hPa

# Function to calculate VPD
calculate_vpd <- function(air_temp, rh) {
  svp <- 6.112 * exp((17.67 * air_temp) / (air_temp + 243.5))  # Saturation Vapor Pressure
  avp <- svp * (rh / 100)  # Actual Vapor Pressure
  vpd <- svp - avp  # Vapor Pressure Deficit
  return(vpd)
}

# Calculate VPD and filter for daylight hours (6:00 AM to 6:00 PM)
BH_2020_daily_air_VPD <- BH_2020_daily_air_VPD %>%
  mutate(VPD = calculate_vpd(AirTC_Avg, RH)) 

# Calculate daily mean VPD per station
daily_vpd_bh_2020 <- BH_2020_daily_air_VPD %>%
  group_by(Station, date = as.Date(TIMESTAMP)) %>%
  summarise(
    daily_mean_vpd = mean(VPD, na.rm = TRUE),
    daily_max_vpd = max(VPD, na.rm = TRUE),  # Daily maximum VPD
    vpd_at_14 = VPD[hour(TIMESTAMP) == 14][1]  # VPD at 14:00 (if available)
  )

str(daily_vpd_bh_2020)

# Reshape the data into long format for easier plotting
df_long <- daily_vpd_bh_2020 %>%
  pivot_longer(cols = c(daily_mean_vpd, daily_max_vpd),
               names_to = "VPD_Type",
               values_to = "VPD_Value")%>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station == "19171 (B3R3)" ~ "B3R3"
  ))%>%
  mutate(treatment = case_when(
    Stations == "B1R1" ~ 1,
    Stations == "B1R3" ~ 1,
    Stations == "B1R4" ~ 1, 
    Stations == "B2R1" ~ 2,
    Stations == "B2R2" ~ 2,
    Stations == "B2R3" ~ 2,
    Stations == "B3R1" ~ 3,
    Stations == "B3R2" ~ 3,
    Stations == "B3R3" ~ 3
  ))

BH_VPD_2020_per_station<-ggplot(df_long, aes(x = date, y = VPD_Value, color = VPD_Type)) +
  geom_line(size =1.8) +  # Line plot
  labs(title = "VPD Trends 2020",
       x = "Date",
       y = "VPD (kPa)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_VPD_2020_per_station_panel_plot.jpg", plot = BH_VPD_2020_per_station, width = 16, height = 6, dpi = 600)


str(df_long)
df_long <- df_long %>%
  mutate(treatment = as.factor(treatment)) 

write.csv(df_long,"data_output/bh_2020_VPD_per_treatment.csv")
BH_VPD_2020_per_treatment<-ggplot(df_long, aes(x = date, y = VPD_Value, color = treatment, linetype = VPD_Type)) +
  geom_line(size = 1.2) +  # Line plot with solid/dashed lines based on VPD_Type
  labs(title =  "2020",
       x = "Date",
       y = "VPD (kPa)") +
  theme_classic() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_linetype_manual(values = c("daily_mean_vpd" = "dashed",    # Solid line for mean VPD
                                   "daily_max_vpd" = "solid")) +  # Dashed line for max VPD
  
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 0, ymax = 80,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 0, ymax = 80,
           alpha = .2) +
  annotate("text", x = as.Date("2020-08-15", "%Y-%m-%d"), y = 80, label = "HW3", size = 7)  +
  annotate("text", x = as.Date("2020-09-07", "%Y-%m-%d"), y = 80, label = "HW4", size = 7)+
  scale_y_continuous(breaks=seq(0,80,20), limits = c (0,80)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-07-23"), as.Date("2020-09-14"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-07-23", "2020-09-14"))) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  theme(legend.position = "none") 

BH_VPD_2020_per_treatment_LEGEND<-ggplot(df_long, aes(x = date, y = VPD_Value, color = treatment, linetype = VPD_Type)) +
  geom_line(size =1.2) +  # Line plot with solid/dashed lines based on VPD_Type
  labs(title =  "2020",
       x = "Date",
       y = "VPD (kPa)") +
  theme_classic() +  # Clean theme
  
  # Color scale for 'treatment', with custom labels
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "90-120% ET", "120-180% ET")) +
  
  # Linetype scale for 'VPD_Type', with custom labels for solid/dashed lines
  scale_linetype_manual(
    values = c("daily_mean_vpd" = "dashed", "daily_max_vpd" = "solid"), 
    name = "VPD Type",  # Title for the linetype legend
    labels = c("Daily Mean VPD", "Daily Max VPD")  # Labels for dashed and solid lines
  ) +
  
  annotate("rect", xmin = as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d"), ymin = 0, ymax = 65, alpha = .2) +
  annotate("rect", xmin = as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d"), ymin = 0, ymax = 65, alpha = .2) +
  annotate("text", x = as.Date("2020-08-15", "%Y-%m-%d"), y = 65, label = "HW3", size = 7) +
  annotate("text", x = as.Date("2020-09-07", "%Y-%m-%d"), y = 65, label = "HW4", size = 7) +
  
  scale_y_continuous(breaks = seq(0, 65, 15), limits = c(0, 65)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-07-23"), as.Date("2020-09-14"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-07-23", "2020-09-14"))) +
  
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif"),
        axis.title.y = element_text(size = 23, family = "serif"),
        axis.title.x = element_text(size = 23, family = "serif"),
        
        # Legend styling
        legend.key.size = unit(0.5, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.justification = "center",
        legend.position = "right",
        legend.title.align = 0)


ggsave("figures/BH_VPD_2020_per_treatment.jpg", plot = BH_VPD_2020_per_treatment, width = 10, height = 6, dpi = 600)

####2021 VPD

BH_2021_daily_air_VPD<- BH_met_2021_temps%>%
  select(TIMESTAMP, Station, AirTC_Avg, RH)

str(BH_2021_daily_air_VPD)


#BH_2021_daily_air_VPD <- BH_2021_daily_air_VPD %>%
#  filter(TIMESTAMP != "NAN")

BH_2021_daily_air_VPD <- BH_2021_daily_air_VPD %>%
  filter(AirTC_Avg != "NAN", RH != "NAN")

sum(is.na(BH_2021_daily_air_VPD$TIMESTAMP))
str(BH_2021_daily_air_VPD)

# Use mdy_hm() to parse "m/d/yy h:m" format
BH_2021_daily_air_VPD <- BH_2021_daily_air_VPD %>%
  mutate(   # Correct function for the given format
         AirTC_Avg = as.numeric(AirTC_Avg),  # Convert AirTC_Avg to numeric
         RH = as.numeric(RH))

BH_2021_daily_air_VPD%>%
  group_by(Station)%>%
  tally()

# Calculate Vapor Pressure
BH_2021_daily_air_VPD <- BH_2021_daily_air_VPD %>%
  mutate(es = 6.112 * exp((17.67 * AirTC_Avg) / (AirTC_Avg + 243.5)),  # Saturation vapor pressure in hPa
         ea = (RH / 100) * es)  # Actual vapor pressure in hPa

# Calculate VPD and filter for daylight hours (6:00 AM to 6:00 PM)
BH_2021_daily_air_VPD <- BH_2021_daily_air_VPD %>%
  mutate(VPD = calculate_vpd(AirTC_Avg, RH)) 

# Calculate daily mean VPD per station
daily_vpd_bh_2021 <- BH_2021_daily_air_VPD %>%
  group_by(Station, date = as.Date(TIMESTAMP)) %>%
  summarise(
    daily_mean_vpd = mean(VPD, na.rm = TRUE),
    daily_max_vpd = max(VPD, na.rm = TRUE),  # Daily maximum VPD
    vpd_at_14 = VPD[hour(TIMESTAMP) == 14][1]  # VPD at 14:00 (if available)
  )

str(daily_vpd_bh_2021)

# Reshape the data into long format for easier plotting
df_long <- daily_vpd_bh_2021 %>%
  pivot_longer(cols = c(daily_mean_vpd, daily_max_vpd),
               names_to = "VPD_Type",
               values_to = "VPD_Value")%>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))

BH_VPD_2021_per_station<-ggplot(df_long, aes(x = date, y = VPD_Value, color = VPD_Type)) +
  geom_line() +  # Line plot
  labs(title = "VPD Trends 2021",
       x = "Date",
       y = "VPD (kPa)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_VPD_2021_per_station_panel_plot.jpg", plot = BH_VPD_2021_per_station, width = 18, height = 6, dpi = 600)


str(df_long)
df_long <- df_long %>%
  mutate(treatment = as.factor(treatment)) 

write.csv(df_long,"data_output/bh_2021_VPD_per_treatment.csv")

BH_VPD_2021_per_treatment<-ggplot(df_long, aes(x = date, y = VPD_Value, color = treatment, linetype = VPD_Type)) +
  geom_line(size = 1.2) +   # Line plot with solid/dashed lines based on VPD_Type
  labs(title =  "2021",
       x = "Date",
       y = "VPD (kPa)") +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "1.5x -2x baseline ET", "2x -3x baseline ET")) +
  scale_linetype_manual(values = c("daily_mean_vpd" = "dashed",    # Solid line for mean VPD
                                   "daily_max_vpd" = "solid")) +  # Dashed line for max VPD
  annotate("text", x = as.Date("2021-09-08", "%Y-%m-%d"), y = 80, label = "HW3", size = 6) +
  annotate("text", x = as.Date("2021-07-10", "%Y-%m-%d"), y = 80, label = "HW2", size = 6) +
  annotate("text", x = as.Date("2021-06-18", "%Y-%m-%d"), y = 80, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0, ymax = 80,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 0, ymax = 80,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 0, ymax = 80,
           alpha = .2)+
scale_y_continuous(breaks=seq(0,80,20), limits = c (0,80)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-03-28"), as.Date("2021-09-13"), by = "30 days")) +
  coord_cartesian(xlim = as.Date(c("2021-03-28", "2021-09-13"))) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())



ggsave("figures/BH_VPD_2021_per_treatment.jpg", plot = BH_VPD_2021_per_treatment, width = 10, height = 6, dpi = 600)

#####AIR Temperature ABOVE CANOPY 2020 & 2021 #####

BH_2020_Air_above_canopy_temp_Avg <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         AirTC_AC_Avg  = as.numeric(AirTC_AC_Avg)) %>% 
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values, ensuring no all-NA cases
  summarise(
    Daily_mean_Temp_Air_above_canopy_Avg = mean(AirTC_AC_Avg, na.rm = TRUE),
    Daily_Max_Temp_Air_above_canopy_Avg = ifelse(all(is.na(AirTC_AC_Avg)), NA, max(AirTC_AC_Avg, na.rm = TRUE)),
    Daily_Min_Temp_Air_above_canopy_Avg = ifelse(all(is.na(AirTC_AC_Avg)), NA, min(AirTC_AC_Avg, na.rm = TRUE)),
    Air_temp_above_canopy_at_14 = AirTC_AC_Avg[hour(TIMESTAMP) == 14][1],  # Air temop at 14:00 (if available)
    .groups = 'drop'  # Prevent grouped output warning
  ) %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station == "19171 (B3R3)" ~ 3
  )) %>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station == "19171 (B3R3)" ~ "B3R3"
  ))%>%
  filter(!Daily_Min_Temp_Air_above_canopy_Avg<0)

BH_2020_Air_above_canopy_temp_Avg%>%
  group_by(Stations)%>%
  tally()


str(BH_2020_Air_above_canopy_temp_Avg)
# Reshape the data to long format
BH_2020_Air_above_canopy_temp_avg_long <- BH_2020_Air_above_canopy_temp_Avg %>%
  pivot_longer(cols = c(Daily_mean_Temp_Air_above_canopy_Avg, Daily_Max_Temp_Air_above_canopy_Avg, Daily_Min_Temp_Air_above_canopy_Avg, Air_temp_above_canopy_at_14),,
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_Air_above_canopy_temp_Avg_2020_per_station<-ggplot(BH_2020_Air_above_canopy_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Air Temperature above canopy Avg Trends 2020",
       x = "Date",
       y = "Temperature (°C)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_Air_above_canopy_temp_Avg_2020_per_station_panel_plot.jpg", plot = BH_Air_above_canopy_temp_Avg_2020_per_station, width = 16, height = 6, dpi = 600)

str(BH_2020_Air_above_canopy_temp_avg_long)
BH_2020_Air_above_canopy_temp_avg_long <- BH_2020_Air_above_canopy_temp_avg_long %>%
  mutate(treatment = as.factor(treatment)) 


BH_Air_above_canopy_temp_Avg_2020_per_treatment<-ggplot(BH_2020_Air_above_canopy_temp_avg_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title =  "Air Temperature Avg above canopy Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c(" B1R3 Baseline (60% ET)", " B2R2 2x baseline ET", " B3R2 3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_Air_above_canopy_Avg" = 1,    # Full opacity
                                "Daily_Max_Temp_Air_above_canopy_Avg" = 0.8,   # Slight transparency
                                "Daily_Min_Temp_Air_above_canopy_Avg" = 0.6, 
                                "Air_temp_above_canopy_at_14" = 0.4))  # More transparency

ggsave("figures/BH_Air_above_canopy_temp_Avg_2020_per_treatment.jpg", plot = BH_Air_above_canopy_temp_Avg_2020_per_treatment, width = 10, height = 6, dpi = 600)

#####AIR TEMPERATURE  2021

BH_2021_Air_above_canopy_temp_Avg <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         AirTC_AC_Avg  = as.numeric(AirTC_AC_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Temp_Air_above_canopy_Avg = mean(AirTC_AC_Avg, na.rm = TRUE),
    Daily_Max_Temp_Air_above_canopy_Avg = ifelse(all(is.na(AirTC_AC_Avg)), NA, max(AirTC_AC_Avg, na.rm = TRUE)),
    Daily_Min_Temp_Air_above_canopy_Avg = ifelse(all(is.na(AirTC_AC_Avg)), NA, min(AirTC_AC_Avg, na.rm = TRUE)),
    Air_temp_above_canopy_at_14 = AirTC_AC_Avg[hour(TIMESTAMP) == 14][1],  # Air temop at 14:00 (if available)
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
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
  ))%>%
  filter(!Daily_Min_Temp_Air_above_canopy_Avg<0)

# Reshape the data to long format
BH_2021_Air_above_canopy_temp_avg_long <- BH_2021_Air_above_canopy_temp_Avg %>%
  pivot_longer(cols = c(Daily_mean_Temp_Air_above_canopy_Avg, Daily_Max_Temp_Air_above_canopy_Avg, Daily_Min_Temp_Air_above_canopy_Avg, Air_temp_above_canopy_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_Air_above_canopy_temp_Avg_2021_per_station<-ggplot(BH_2021_Air_above_canopy_temp_avg_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Air Temperature Above canopy Avg Trends 2021",
       x = "Date",
       y = "Temperature (°C)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_Air_above_canopy_temp_Avg_2021_per_station_panel_plot.jpg", plot = BH_Air_above_canopy_temp_Avg_2021_per_station, width = 16, height = 6, dpi = 600)


str(BH_2021_Air_above_canopy_temp_avg_long)
BH_2021_Air_above_canopy_temp_avg_long <- BH_2021_Air_above_canopy_temp_avg_long %>%
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
  ))%>%
  mutate(treatment = as.factor(treatment)) 

str(BH_2021_Air_above_canopy_temp_avg_long)


BH_Air_above_canopy_temp_Avg_2021_per_treatment<-ggplot(BH_2021_Air_above_canopy_temp_avg_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Air Temperature Avg Above canopy Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", "B2R2 1.5x baseline ET", "B3R2 2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Temp_Air_above_canopy_Avg" = 1,    # Full opacity
                                "Daily_Max_Temp_Air_above_canopy_Avg" = 0.8,   # Slight transparency
                                "Daily_Min_Temp_Air_above_canopy_Avg" = 0.6, 
                                "Air_temp_above_canopy_at_14" = 0.4))  # More transparency
ggsave("figures/BH_Air_above_canopy_temp_Avg_2021_per_treatment.jpg", plot = BH_Air_above_canopy_temp_Avg_2021_per_treatment, width = 10, height = 6, dpi = 600)  

#####RELATIVE HUMIDITY ABOVE CANOPY Average 2020 & 2021 #####

BH_2020_rel_hum_above_canopy_Avg <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         RH_AC  = as.numeric(RH_AC)) %>% 
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values, ensuring no all-NA cases
  summarise(
    Daily_mean_rel_hum_above_canopy_Avg = mean(RH_AC, na.rm = TRUE),
    Daily_Max_rel_hum_above_canopy_Avg = ifelse(all(is.na(RH_AC)), NA, max(RH_AC, na.rm = TRUE)),
    Daily_Min_rel_hum_above_canopy_Avg = ifelse(all(is.na(RH_AC)), NA, min(RH_AC, na.rm = TRUE)),
    RH_above_canopy_at_14 = RH_AC[hour(TIMESTAMP) == 14][1],  # Air temop at 14:00 (if available)   
    .groups = 'drop'  # Prevent grouped output warning
  ) %>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station == "19171 (B3R3)" ~ 3
  )) %>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station == "19171 (B3R3)" ~ "B3R3"
  ))%>%
  filter(!Daily_Min_rel_hum_above_canopy_Avg<0)

BH_2020_rel_hum_above_canopy_Avg%>%
  group_by(Stations)%>%
  tally()


str(BH_2020_rel_hum_above_canopy_Avg)
# Reshape the data to long format
BH_2020_rel_hum_above_canopy_Avg_long <- BH_2020_rel_hum_above_canopy_Avg %>%
  pivot_longer(cols = c(Daily_mean_rel_hum_above_canopy_Avg, Daily_Max_rel_hum_above_canopy_Avg, Daily_Min_rel_hum_above_canopy_Avg, RH_above_canopy_at_14),,
               names_to = "RH_Type",
               values_to = "RH_Value")



# Plot with ggplot

BH_rel_hum_above_canopy_Avg_2020_per_station<-ggplot(BH_2020_rel_hum_above_canopy_Avg_long, aes(x = Date, y = RH_Value, color = RH_Type)) +
  geom_line() +  # Line plot
  labs(title = "Relative Humidity above canopy Average Trends 2020",
       x = "Date",
       y = "Temperature (°C)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_rel_hum_above_canopy_Avg_2020_per_station_panel_plot.jpg", plot = BH_rel_hum_above_canopy_Avg_2020_per_station, width = 16, height = 6, dpi = 600)


str(BH_2020_rel_hum_above_canopy_Avg_long)
BH_2020_rel_hum_above_canopy_Avg_long <- BH_2020_rel_hum_above_canopy_Avg_long %>%
  mutate(treatment = as.factor(treatment)) 



BH_rel_hum_above_canopy_Avg_2020_per_treatment<-ggplot(BH_2020_rel_hum_above_canopy_Avg_long, aes(x = Date, y = RH_Value, color = treatment, alpha = RH_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title =  "Relative Humidity Above canopy Average Trends by Treatment and RH Type 2020",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", " B2R2 2x baseline ET", " B3R2 3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_rel_hum_above_canopy_Avg" = 1,    # Full opacity
                                "Daily_Max_rel_hum_above_canopy_Avg" = 0.6,   # Slight transparency
                                "Daily_Min_rel_hum_above_canopy_Avg" = 0.3, 
                                "RH_above_canopy_at_14" = 0.8))  # More transparency

ggsave("figures/BH_rel_hum_above_canopy_Avg_2020_per_treatment.jpg", plot = BH_rel_hum_above_canopy_Avg_2020_per_treatment, width = 10, height = 6, dpi = 600)

#####RELATIVE HUMIDITY  2021

BH_2021_rel_hum_above_canopy_Avg <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         RH_AC  = as.numeric(RH_AC)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_rel_hum_above_canopy_Avg = mean(RH_AC, na.rm = TRUE),
    Daily_Max_rel_hum_above_canopy_Avg = ifelse(all(is.na(RH_AC)), NA, max(RH_AC, na.rm = TRUE)),
    Daily_Min_rel_hum_above_canopy_Avg = ifelse(all(is.na(RH_AC)), NA, min(RH_AC, na.rm = TRUE)),
    RH_above_canopy_at_14 = RH_AC[hour(TIMESTAMP) == 14][1],  # Air temop at 14:00 (if available)   
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  mutate(treatment = case_when(
    Station == "B1R1" ~ 1,
    Station == "B1R3" ~ 1,
    Station == "B1R4" ~ 1, 
    Station == "B2R1" ~ 2,
    Station == "B2R2" ~ 2,
    Station == "B2R3" ~ 2,
    Station == "B3R1" ~ 3,
    Station == "B3R2" ~ 3,
    Station == "B3R3" ~ 3
  ))%>%
  filter(!Daily_Min_rel_hum_above_canopy_Avg<0)

# Reshape the data to long format
BH_2021_rel_hum_above_canopy_Avg_long <- BH_2021_rel_hum_above_canopy_Avg %>%
  pivot_longer(cols = c(Daily_mean_rel_hum_above_canopy_Avg, Daily_Max_rel_hum_above_canopy_Avg, Daily_Min_rel_hum_above_canopy_Avg, RH_above_canopy_at_14),
               names_to = "RH_Type",
               values_to = "RH_Value")



# Plot with ggplot

BH_rel_hum_above_canopy_Avg_2021_per_station<-ggplot(BH_2021_rel_hum_above_canopy_Avg_long, aes(x = Date, y = RH_Value, color = RH_Type)) +
  geom_line() +  # Line plot
  labs(title = "Relative Humidity Average Trends 2021",
       x = "Date",
       y = "Relative Humidity (%)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_rel_hum_above_canopy_Avg_2021_per_station_panel_plot.jpg", plot = BH_rel_hum_above_canopy_Avg_2021_per_station, width = 16, height = 6, dpi = 600)


str(BH_2021_rel_hum_above_canopy_Avg_long)
BH_2021_rel_hum_above_canopy_Avg_long <- BH_2021_rel_hum_above_canopy_Avg_long %>%
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
  ))%>%
  mutate(treatment = as.factor(treatment)) 

str(BH_2021_rel_hum_above_canopy_Avg_long)


BH_rel_hum_above_canopy_Avg_2021_per_treatment<-ggplot(BH_2021_rel_hum_above_canopy_Avg_long, aes(x = Date, y = RH_Value, color = treatment, alpha = RH_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Relative Humidity Average Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "Relative Humidity (%)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("B1R3 Baseline (60% ET)", "B2R2 1.5x baseline ET", "B3R2 2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_rel_hum_above_canopy_Avg" = 1,    # Full opacity
                                "Daily_Max_rel_hum_above_canopy_Avg" = 0.5,   # Slight transparency
                                "Daily_Min_rel_hum_above_canopy_Avg" = 0.3, 
                                "RH_above_canopy_at_14" = 0.7))  # More transparency

ggsave("figures/BH_rel_hum_above_canopy_Avg_2021_per_treatment.jpg", plot = BH_rel_hum_above_canopy_Avg_2021_per_treatment, width = 10, height = 6, dpi = 600)  


####### LEAF TEMPERATURES DAILY MAX, AVERAGE, AT 14 2019, 2020, 2021 #####



# Explicitly handle problematic TIMESTAMP values
BH_2020_soil_temp_5cm <- BH_met_2020_temps %>%
  # Attempt to parse TIMESTAMP and flag parsing errors
  mutate(TIMESTAMP_parsed = suppressWarnings(ymd_hms(TIMESTAMP))) %>%
  # Filter out rows where TIMESTAMP could not be parsed
  filter(!is.na(TIMESTAMP_parsed)) %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP_parsed),
         Soil_Temp_TCAV_Avg = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Soil_Temp_TCAV_Avg = mean(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Max_Soil_Temp_TCAV = max(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Min_Soil_Temp_TCAV = min(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    #    Soil_temp_at_14 = Soil_Temp_TCAV_Avg[hour(TIMESTAMP) == 14][1],  # Air temp at 14:00 (if available)   
    Soil_temp_at_14 = get_temp_at_14(TIMESTAMP_parsed, Soil_Temp_TCAV_Avg),  # Use helper function   
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
  mutate(treatment = case_when(
    Station == "19170 (B1R1)" ~ 1,
    Station == "17702 (B1R3)" ~ 1,
    Station == "18331 (B1R4)" ~ 1, 
    Station == "19120 (B2R1)" ~ 2,
    Station == "18333 (B2R2)" ~ 2,
    Station == "3859 (B2R3)" ~ 2,
    Station == "18330 (B3R1)" ~ 3,
    Station == "18335 (B3R2)" ~ 3,
    Station== "19171 (B3R3)" ~ 3
  ))%>%
  mutate(Stations = case_when(
    Station == "19170 (B1R1)" ~ "B1R1",
    Station == "17702 (B1R3)" ~ "B1R3",
    Station == "18331 (B1R4)" ~ "B1R4", 
    Station == "19120 (B2R1)" ~ "B2R1",
    Station == "18333 (B2R2)" ~ "B2R2",
    Station == "3859 (B2R3)" ~ "B2R3",
    Station == "18330 (B3R1)" ~ "B3R1",
    Station == "18335 (B3R2)" ~ "B3R2",
    Station== "19171 (B3R3)" ~ "B3R3"
  ))


str(BH_2020_soil_temp_5cm)
# Reshape the data to long format
BH_2020_soil_temp_long <- BH_2020_soil_temp_5cm %>%
  pivot_longer(cols = c(Daily_mean_Soil_Temp_TCAV_Avg, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV, Soil_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")


# Plot with ggplot

BH_soil_temp_2020_5cm_per_station<-ggplot(BH_2020_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends 2020",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  facet_wrap(~ factor(Stations, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_soil_temp_2020_5cm_per_station_panel_plot.jpg", plot = BH_soil_temp_2020_5cm_per_station, width = 18, height = 13, dpi = 600)


custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_soil_temp_2020_5cm_per_station_one_plot<-ggplot(BH_2020_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Stations, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends by Station and Temperature Type 2020",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = "solid", 
                                   "Daily_Max_Soil_Temp_TCAV" = "dashed", 
                                   "Daily_Min_Soil_Temp_TCAV" = "dotdash", 
                                   "Soil_temp_at_14" ="dotted")) 
# Line types for temp types

ggsave("figures/BH_soil_temp_2020_5cm_per_station_one_plot.jpg", plot = BH_soil_temp_2020_5cm_per_station_one_plot, width = 10, height = 6, dpi = 600)


str(BH_2020_soil_temp_long)
BH_2020_soil_temp_long <- BH_2020_soil_temp_long %>%
  mutate(treatment = as.factor(treatment)) 

str(BH_2021_soil_temp_long)

BH_2020_soil_temp_long_filtered <- BH_2020_soil_temp_long %>%
  filter(Temperature_Type != "Soil_temp_at_14")


BH_soil_temp_2020_5cm_per_treatment<-ggplot(BH_2020_soil_temp_long_filtered, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Soil Temperature Trends by Treatment and Temperature Type 2020",
       x = "Date",
       y = "Soil Temperature 5 cm (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = 1,    # Full opacity
                                "Daily_Max_Soil_Temp_TCAV" = 0.6,   # Slight transparency
                                "Daily_Min_Soil_Temp_TCAV" = 0.3 ))  # More transparency

ggsave("figures/BH_soil_temp_2020_5cm_per_treatment.jpg", plot = BH_soil_temp_2020_5cm_per_treatment, width = 10, height = 6, dpi = 600)


# Explicitly handle problematic TIMESTAMP values
BH_2021_soil_temp_5cm <- BH_met_2021_temps %>%
  # Continue with the parsing and processing
  mutate(Date = as.Date(TIMESTAMP),
         Soil_Temp_TCAV_Avg = as.numeric(Soil_Temp_TCAV_Avg)) %>%
  # Group by Station and Date
  group_by(Station, Date) %>%
  # Summarize the daily average, max, and min values
  summarise(
    Daily_mean_Soil_Temp_TCAV_Avg = mean(Soil_Temp_TCAV_Avg, na.rm = TRUE),
    Daily_Max_Soil_Temp_TCAV = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, max(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    Daily_Min_Soil_Temp_TCAV = ifelse(all(is.na(Soil_Temp_TCAV_Avg)), NA, min(Soil_Temp_TCAV_Avg, na.rm = TRUE)),
    #    Soil_temp_at_14 = Soil_Temp_TCAV_Avg[hour(TIMESTAMP) == 14][1],  # Air temp at 14:00 (if available)   
    Soil_temp_at_14 = get_temp_at_14(TIMESTAMP, Soil_Temp_TCAV_Avg),  # Use helper function   
    .groups = 'drop'  # Prevent grouped output warning
  )%>%
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
  ))%>%
  filter(!Daily_mean_Soil_Temp_TCAV_Avg<0)


# Reshape the data to long format
BH_2021_soil_temp_long <- BH_2021_soil_temp_5cm %>%
  pivot_longer(cols = c(Daily_mean_Soil_Temp_TCAV_Avg, Daily_Max_Soil_Temp_TCAV, Daily_Min_Soil_Temp_TCAV,Soil_temp_at_14),
               names_to = "Temperature_Type",
               values_to = "Temperature_Value")



# Plot with ggplot

BH_soil_temp_2021_5cm_per_station<-ggplot(BH_2021_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends 2021",
       x = "Date",
       y = "Soil Temperature at 5 cm (°C)") +
  facet_wrap(~ factor(Station, levels = c("B1R1", "B1R3", "B1R4", "B2R1", "B2R2", "B2R3", "B3R1", "B3R2", "B3R3")), 
             scales = "fixed") +  # Same y-axis across stations
  theme_minimal() +  # Clean theme
  scale_color_viridis_d()  # Use viridis color scale

ggsave("figures/BH_soil_temp_2021_5cm_per_station_panel_plot.jpg", plot = BH_soil_temp_2021_5cm_per_station, width = 18, height = 13, dpi = 600)



#ggplot(BH_2021_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Station, linetype = Temperature_Type)) +
#  geom_line() +  # Line plot
#  labs(title = "Soil Temperature Trends Across Stations",
#       x = "Date",
#       y = "Temperature (°C)") +
#  theme_minimal() +  # Clean theme
#  scale_color_viridis_d() +  # Use viridis color scale for stations
#  scale_linetype_manual(values = c("solid", "dashed", "dotted"))  # Line types for temperature types



# Define custom colors: shades of similar colors for B1, B2, and B3 groups

#custom_station_colors <- c(
#  "B1R1" = "#FDE725FF",     # Light Yellow for B1R1
#  "B1R3" = "#E6D900FF",     # Medium Yellow for B1R3
#  "B1R4" = "#D4C700FF",     # Dark Yellow for B1R4
#  "B2R1" = "#21908CFF",     # Light Teal for B2R1
#  "B2R2" = "#1E7F7FFF",     # Medium Teal for B2R2
#  "B2R3" = "#1B6C6CFF",     # Dark Teal for B2R3
#  "B3R1" = "#440154FF",     # Light Purple for B3R1
#  "B3R2" = "#3D0F40FF",     # Medium Purple for B3R2
#  "B3R3" = "#360B3CFF"      # Dark Purple for B3R3
#)
custom_station_colors <- c(
  "B1R1" = "#FDE725FF",  # Bright Yellow for B1R1
  "B1R3" = "#F9A825FF",  # Golden Yellow for B1R3
  "B1R4" = "#F57F20FF",  # Orange for B1R4
  "B2R1" = "#21908CFF",  # Teal for B2R1
  "B2R2" = "#1E7F7FFF",  # Medium Teal for B2R2
  "B2R3" = "#0E4C92FF",  # Darker Teal for B2R3
  "B3R1" = "#440154FF",  # Light Purple for B3R1
  "B3R2" = "#4B0B72FF",  # Medium Purple for B3R2
  "B3R3" = "#5C3F8DFF"   # Dark Purple for B3R3
)
# Plot with custom colors for similar station groups
BH_soil_temp_2021_5cm_per_station_one_plot<-ggplot(BH_2021_soil_temp_long, aes(x = Date, y = Temperature_Value, color = Station, linetype = Temperature_Type)) +
  geom_line() +  # Line plot
  labs(title = "Soil Temperature Trends by Station and Temperature Type 2021",
       x = "Date",
       y = "Temperature (°C)") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = custom_station_colors) +  # Use custom colors for stations
  scale_linetype_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = "solid", 
                                   "Daily_Max_Soil_Temp_TCAV" = "dashed", 
                                   "Daily_Min_Soil_Temp_TCAV" = "dotted"))  # Line types for temp types

ggsave("figures/BH_soil_temp_2021_5cm_per_station_one_plot.jpg", plot = BH_soil_temp_2021_5cm_per_station_one_plot, width = 10, height = 6, dpi = 600)


str(BH_2021_soil_temp_long)
BH_2021_soil_temp_long <- BH_2021_soil_temp_long %>%
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
  ))%>%
  mutate(treatment = as.factor(treatment)) 


BH_2020_soil_temp_long_filtered <- BH_2020_soil_temp_long %>%
  filter(Temperature_Type != "Soil_temp_at_14")

str(BH_2021_soil_temp_long)


BH_soil_temp_2021_5cm_per_treatment<-ggplot(BH_2021_soil_temp_long, aes(x = Date, y = Temperature_Value, color = treatment, alpha = Temperature_Type)) +
  geom_line(linetype = "solid") +  # Line plot with solid lines
  labs(title = "Soil Temperature Trends by Treatment and Temperature Type 2021",
       x = "Date",
       y = "Soil Temperature at 5 cm (°C)") +
  theme_minimal() +  # Clean theme
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", 
                         labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET")) +
  scale_alpha_manual(values = c("Daily_mean_Soil_Temp_TCAV_Avg" = 1,    # Full opacity
                                "Daily_Max_Soil_Temp_TCAV" = 0.6,   # Slight transparency
                                "Daily_Min_Soil_Temp_TCAV" = 0.3))  # More transparency

ggsave("figures/BH_soil_temp_2021_5cm_per_treatment.jpg", plot = BH_soil_temp_2021_5cm_per_treatment, width = 10, height = 6, dpi = 600)  


#######TABLE WITH MEAN VALUES AND SE FOR SIL, AIR BELOW CANOPY, CANOPY AND LEAF TEMPERATURE #####

#####LICOR LEAF TEMPERATURE MIDDAY VALUES (14:00) 2020 and 2021#####


diurnals_borden_hills_2020 <-read.csv("data_output/data_physiology_all_complete_BH_2020.csv", header = TRUE)

str(diurnals_borden_hills_2020)

diurnals_2020_A_vs_time <- diurnals_borden_hills_2020 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time))

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

str(diurnals_2020_A_vs_time$leaf_temp)


BH_2020_LICOR_leaf_temp_mean_midday <- diurnals_2020_A_vs_time %>%
  mutate(Date = as.Date(datetime),
         Tleaf  = as.numeric(Tleaf))%>%
  filter(round == 4 | Date == "2020-09-07" & round ==3)%>%
  mutate(treatment = case_when(
    BLOCK == "B1R1" ~ 1,
    BLOCK == "B1R3" ~ 1,
    BLOCK == "B1R4" ~ 1, 
    BLOCK == "B2R1" ~ 2,
    BLOCK == "B2R2" ~ 2,
    BLOCK == "B2R3" ~ 2,
    BLOCK == "B3R1" ~ 3,
    BLOCK == "B3R2" ~ 3,
    BLOCK == "B3R3" ~ 3
  ))%>%
  # Group by Station and Date
  group_by(treatment, Date,) %>%
  summarise(
    Midday_mean_Tleaf = mean(Tleaf), std_error = se(Tleaf),
    .groups = 'drop'  # Prevent grouped output warning
  ) 
BH_2020_LICOR_leaf_temp_mean_midday$Mean_sem_Tleaf <- paste(round(BH_2020_LICOR_leaf_temp_mean_midday$Midday_mean_Tleaf, 2), "±", round(BH_2020_LICOR_leaf_temp_mean_midday$std_error, 2))

BH_2020_LICOR_leaf_temp_midday<- diurnals_2020_A_vs_time %>%
  mutate(Date = as.Date(datetime),
         Tleaf  = as.numeric(Tleaf))%>%
  filter(round == 4)%>%
  select(datetime, Date,BLOCK, LEAF, treatment, round, Tleaf)

####2021 data 


data_physiology_complete_BH_2021_middays<-read.csv("data_output/data_physiology_complete_BH_2021_middays.csv", header = TRUE)


data_physiology_complete_BH_2021_middays <- data_physiology_complete_BH_2021_middays

data_physiology_complete_BH_2021_middays$date<- ymd(data_physiology_complete_BH_2021_middays$date)
str(data_physiology_complete_BH_2021_middays$date)

data_physiology_complete_BH_2021_middays<-data_physiology_complete_BH_2021_middays%>%
  filter(!is.na(Tleaf))

data_physiology_complete_BH_2021_midday_tally<-data_physiology_complete_BH_2021_middays %>%
  group_by(treatment, date,) %>%
  tally()

BH_2021_LICOR_leaf_temp_mean_midday <- data_physiology_complete_BH_2021_middays %>%
  mutate(Date = as.Date(date),
         Tleaf  = as.numeric(Tleaf))%>%
  # Group by Station and Date
  group_by(treatment, Date) %>%
  summarise(
    Midday_mean_Tleaf = mean(Tleaf), std_error = se(Tleaf),
    .groups = 'drop'  # Prevent grouped output warning
  ) 
BH_2021_LICOR_leaf_temp_mean_midday$Mean_sem_Tleaf <- paste(round(BH_2021_LICOR_leaf_temp_mean_midday$Midday_mean_Tleaf, 2), "±", round(BH_2021_LICOR_leaf_temp_mean_midday$std_error, 2))



#####SOIL TEMPERATURE 5 CM  MIDDAY VALUES (14:00) 2020 and 2021#####
####2020

BH_2020_soil_temp_5cm_midday_tally<- BH_2020_soil_temp_5cm %>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% ####2020-09-15 is not recorded last date recorde 2020-09-14%>%
  filter(!is.na(Soil_temp_at_14))%>%
  group_by (Date,treatment)%>%
  tally()


str(BH_2020_soil_temp_5cm)
BH_2020_soil_temp_5cm_midday<- BH_2020_soil_temp_5cm %>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% ####2020-09-15 is not recorded last date recorded 2020-09-14
  group_by (Date,treatment) %>%
  summarise(
    Midday_soil_temp = mean(Soil_temp_at_14,na.rm = TRUE), std_error = se(Soil_temp_at_14),
    .groups = 'drop'  # Prevent grouped output warning
  ) 
  
  BH_2020_soil_temp_5cm_midday$Mean_sem_soil_temp <- paste(round(BH_2020_soil_temp_5cm_midday$Midday_soil_temp, 2), "±", round(BH_2020_soil_temp_5cm_midday$std_error, 2))

  ####2021

  BH_2021_soil_temp_5cm_midday<- BH_2021_soil_temp_5cm %>%
    filter(Date =="2021-06-04" |Date =="2021-06-15"|Date =="2021-06-17"|Date =="2021-06-22"|Date =="2021-06-30" |Date =="2021-07-07"|Date =="2021-07-10"|Date =="2021-07-14"|Date =="2021-07-28"|Date =="2021-08-11"|Date =="2021-08-30"|Date =="2021-09-08")%>%
    group_by (Date,treatment) %>%
    summarise(
      Midday_soil_temp = mean(Soil_temp_at_14,na.rm = TRUE), std_error = se(Soil_temp_at_14),
      .groups = 'drop'  # Prevent grouped output warning
    ) 
  
BH_2021_soil_temp_5cm_midday$Mean_sem_soil_temp <- paste(round(BH_2021_soil_temp_5cm_midday$Midday_soil_temp, 2), "±", round(BH_2021_soil_temp_5cm_midday$std_error, 2))
  
#####IRT CANOPY AVG  MIDDAY VALUES (14:00) 2020 and 2021#####
####2020

BH_2020_IRT_canopy_AVG_midday_tally<- BH_2020_IRT_Avg_Canopy_temp%>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% ####2020-09-15 is not recorded last date recorde 2020-09-14%>%
  filter(!is.na(IRT_Avg_temp_at_14))%>%
  group_by (Date,treatment)%>%
  tally()


str(BH_2020_IRT_Avg_Canopy_temp)
BH_2020_IRT_canopy_AVG_midday<- BH_2020_IRT_Avg_Canopy_temp %>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% ####2020-09-15 is not recorded last date recorded 2020-09-14
  group_by (Date,treatment) %>%
  summarise(
    Midday_IRT_temp = mean(IRT_Avg_temp_at_14,na.rm = TRUE), std_error = se(IRT_Avg_temp_at_14),
    .groups = 'drop'  # Prevent grouped output warning
  ) 

BH_2020_IRT_canopy_AVG_midday$Mean_sem_IRT_canopy_temp <- paste(round(BH_2020_IRT_canopy_AVG_midday$Midday_IRT_temp, 2), "±", round(BH_2020_IRT_canopy_AVG_midday$std_error, 2))

####2021

BH_2021_IRT_canopy_AVG_midday<- BH_2021_IRT_Avg_Canopy_temp %>%
  filter(Date =="2021-06-04" |Date =="2021-06-15"|Date =="2021-06-17"|Date =="2021-06-22"|Date =="2021-06-30" |Date =="2021-07-07"|Date =="2021-07-10"|Date =="2021-07-14"|Date =="2021-07-28"|Date =="2021-08-11"|Date =="2021-08-30"|Date =="2021-09-08")%>%
  group_by (Date,treatment) %>%
  summarise(
    Midday_IRT_temp = mean(IRT_Avg_temp_at_14,na.rm = TRUE), std_error = se(IRT_Avg_temp_at_14),
    .groups = 'drop'  # Prevent grouped output warning
  ) 

BH_2021_IRT_canopy_AVG_midday$Mean_sem_IRT_canopy_temp <- paste(round(BH_2021_IRT_canopy_AVG_midday$Midday_IRT_temp, 2), "±", round(BH_2021_IRT_canopy_AVG_midday$std_error, 2))



#####AIR TEMPERATURE BELOW CANOPY AT MIDDAY (14:00) 2020 and 2021#####
####2020

BH_2020_Air_temp_below_canopy_midday_tally<- BH_2020_Air_temp_Avg%>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% ####2020-09-15 is not recorded last date recorded 2020-09-14%>%
  filter(!is.na(Air_Avg_temp_at_14))%>%
  group_by (Date,treatment)%>%
  tally()

str(BH_2020_Air_temp_Avg)

str(BH_2020_Air_temp_Avg)
BH_2020_Air_temp_below_canopy_midday<- BH_2020_Air_temp_Avg %>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% #### 2020-09-15 is not recorded last date recorded 2020-09-14
  group_by (Date,treatment) %>%
  summarise(
    Midday_Air_temp = mean(Air_Avg_temp_at_14,na.rm = TRUE),
    .groups = 'drop'  # Prevent grouped output warning
  ) 


####2021

str(BH_2021_Air_temp_Avg)

BH_2021_Air_temp_below_canopy_midday<- BH_2021_Air_temp_Avg %>%
  filter(Date =="2021-06-04" |Date =="2021-06-15"|Date =="2021-06-17"|Date =="2021-06-22"|Date =="2021-06-30" |Date =="2021-07-07"|Date =="2021-07-10"|Date =="2021-07-14"|Date =="2021-07-28"|Date =="2021-08-11"|Date =="2021-08-30"|Date =="2021-09-08")%>%
  group_by (Date,treatment) %>%
  summarise(
    Midday_IRT_temp = mean(Air_Avg_temp_at_14,na.rm = TRUE),
    .groups = 'drop'  # Prevent grouped output warning
  ) 

##### VPD ######

str(BH_2020_Air_temp_Avg)
str(BH_2020_rel_hum_Avg)

####2020

BH_2020_VPD_at_midday <- data.frame(
  Date = BH_2020_Air_temp_Avg$Date,
  treatment = BH_2020_Air_temp_Avg$treatment,
  Air_Avg_temp_at_14 = BH_2020_Air_temp_Avg$Air_Avg_temp_at_14,
  RH_at_14 = BH_2020_rel_hum_Avg$RH_at_14
)

BH_2020_VPD_at_midday <- BH_2020_VPD_at_midday %>%
  mutate(VPD_at_14 = calculate_vpd(Air_Avg_temp_at_14,RH_at_14)) 

  
BH_2020_VPD_at_midday_tally<- BH_2020_VPD_at_midday%>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% ####2020-09-15 is not recorded last date recorded 2020-09-14%>%
  filter(!is.na(Air_Avg_temp_at_14))%>%
  group_by (Date,treatment)%>%
  tally()

str(BH_2020_VPD_at_midday)


BH_2020_VPD_at_midday_14<- BH_2020_VPD_at_midday %>%
  filter(Date =="2020-07-20" |Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-14")%>% #### 2020-09-15 is not recorded last date recorded 2020-09-14
  group_by (Date,treatment) %>%
  summarise(
    Midday_VPD = mean(VPD_at_14,na.rm = TRUE),
    .groups = 'drop'  # Prevent grouped output warning
  ) 


####2021


str(BH_2021_rel_hum_Avg)
str(BH_2021_Air_temp_Avg)
# Rename 'treatment' in one of the data frames before joining to avoid conflict
BH_2021_Air_temp_Avg <- BH_2021_Air_temp_Avg %>%
  rename(treatment_air = treatment)

BH_2021_rel_hum_Avg <- BH_2021_rel_hum_Avg %>%
  rename(treatment_rh = treatment)

# Perform the join based on the 'Date' and 'Station' columns
BH_2021_VPD_at_midday <- BH_2021_Air_temp_Avg %>%
  inner_join(BH_2021_rel_hum_Avg, by = c("Date", "Station")) %>%
  select(Date, Station, Air_Avg_temp_at_14, RH_at_14, treatment_air)


BH_2021_VPD_at_midday <- BH_2021_VPD_at_midday %>%
  mutate(VPD_at_14 = calculate_vpd(Air_Avg_temp_at_14,RH_at_14)) 

BH_2021_VPD_at_midday$treatment<-BH_2021_VPD_at_midday$treatment_air

BH_2021_VPD_at_midday_14<- BH_2021_VPD_at_midday %>%
  filter(Date =="2021-06-04" |Date =="2021-06-15"|Date =="2021-06-17"|Date =="2021-06-22"|Date =="2021-06-30" |Date =="2021-07-07"|Date =="2021-07-10"|Date =="2021-07-14"|Date =="2021-07-28"|Date =="2021-08-11"|Date =="2021-08-30"|Date =="2021-09-08")%>%
  group_by (Date,treatment) %>%
  summarise(
    Midday_VPD = mean(VPD_at_14,na.rm = TRUE),
    .groups = 'drop'  # Prevent grouped output warning
  ) 

BH_2020_LICOR_leaf_temp_mean_midday_dates_with_stations<-BH_2020_LICOR_leaf_temp_mean_midday%>%
 filter(Date =="2020-07-31"|Date =="2020-08-10"|Date =="2020-08-13" |Date =="2020-08-19"|Date =="2020-08-26"|Date =="2020-09-07"|Date =="2020-09-15")

BH_2020_LICOR_leaf_temp_mean_midday_dates_with_stations<-BH_2020_LICOR_leaf_temp_mean_midday_dates_with_stations%>%
select(-Midday_mean_Tleaf,-std_error)

BH_2020_soil_temp_5cm_midday<-BH_2020_soil_temp_5cm_midday%>%
  select(-Midday_soil_temp,-std_error)


BH_2020_IRT_canopy_AVG_midday<-BH_2020_IRT_canopy_AVG_midday%>%
  select(-Midday_IRT_temp,-std_error)

str(BH_2020_LICOR_leaf_temp_mean_midday_dates_with_stations)
str(BH_2020_soil_temp_5cm_midday)
str(BH_2020_IRT_canopy_AVG_midday)
str(BH_2020_Air_temp_below_canopy_midday)
str(BH_2020_VPD_at_midday_14)


# List of data frames to merge
BH_2020_temps_and_VPD <- list(
  BH_2020_LICOR_leaf_temp_mean_midday_dates_with_stations,
  BH_2020_soil_temp_5cm_midday,
  BH_2020_IRT_canopy_AVG_midday,
  BH_2020_Air_temp_below_canopy_midday,
  BH_2020_VPD_at_midday_14
)

# Merge all data frames by 'Date' and 'treatment'
BH_2020_temps_and_VPD_merged <- reduce(BH_2020_temps_and_VPD, full_join, by = c("Date", "treatment"))

# View the merged data
View(BH_2020_temps_and_VPD_merged)

write.csv(BH_2020_temps_and_VPD_merged,"data_output/BH_2020_temps_and_VPD_merged.csv")


BH_2021_LICOR_leaf_temp_mean_midday<-BH_2021_LICOR_leaf_temp_mean_midday%>%
  select(-Midday_mean_Tleaf,-std_error)

BH_2021_soil_temp_5cm_midday<-BH_2021_soil_temp_5cm_midday%>%
  select(-Midday_soil_temp,-std_error)


BH_2021_IRT_canopy_AVG_midday<-BH_2021_IRT_canopy_AVG_midday%>%
  select(-Midday_IRT_temp,-std_error)

str(BH_2021_LICOR_leaf_temp_mean_midday)
str(BH_2021_soil_temp_5cm_midday)
str(BH_2021_IRT_canopy_AVG_midday)
str(BH_2021_Air_temp_below_canopy_midday)
str(BH_2021_VPD_at_midday_14)

BH_2021_temps_and_VPD <- list(
  BH_2021_LICOR_leaf_temp_mean_midday,
  BH_2021_soil_temp_5cm_midday,
  BH_2021_IRT_canopy_AVG_midday,
  BH_2021_Air_temp_below_canopy_midday,
  BH_2021_VPD_at_midday_14
)

# Merge all data frames by 'Date' and 'treatment'
BH_2021_temps_and_VPD_merged <- reduce(BH_2021_temps_and_VPD, full_join, by = c("Date", "treatment"))

# View the merged data
View(BH_2021_temps_and_VPD_merged)

write.csv(BH_2021_temps_and_VPD_merged,"data_output/BH_2021_temps_and_VPD_merged.csv")


#######Figure 7 VPD BH 2020 AND 2021#####

# Adjust legend size in the original plot that generates the legend
legend <- get_legend(
  BH_VPD_2020_per_treatment_LEGEND +
#    geom_line(size = 2) +  # Increase the line size for better visibility
    theme(legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20),  # Adjust legend title size
          legend.key.size = unit(0.8, "cm"), # Adjust legend key size
          legend.key.width = unit(2, "cm"),  # Increase width of legend keys for better line visibility
          legend.position = "right")  # Ensure the legend is to the right
)

# Now, use plot_grid to arrange your plots and legend
panel_plot_VPD_bh_2020_2021<-plot_grid(
  plot_grid(
    BH_VPD_2020_per_treatment, BH_VPD_2021_per_treatment,
    labels = c("A", "B"),
    ncol = 2, 
    vjust = 1.5, 
    hjust = -9.6, 
    label_size = 20,
    align = "v", 
    axis = "l",
    rel_heights = c(1, 1)  # Adjust this to control relative heights of panels
  ),
  legend,  # Add the adjusted legend
  ncol = 2,  # Layout: 2 columns (2 plots and 1 legend)
  rel_widths = c(4, 1)  # Increase the width for the legend
)

ggsave(panel_plot_VPD_bh_2020_2021, filename = "figures/panel_plot_VPD_bh_2020_2021.jpg", width = 30, height =10, dpi =600)


#######Figure 8 SOIL TEMPERATURE BH 2020 AND 2021#####

# Adjust legend size in the original plot that generates the legend
legend_soil <- get_legend(
  BH_2021_soil_temp_avg_per_treatment_long_with_CI_legend +
#    geom_line(size = 2) +  # Increase the line size for better visibility
    theme(legend.text = element_text(size = 20),  # Adjust legend text size
          legend.title = element_text(size = 20),  # Adjust legend title size
          legend.key.size = unit(0.8, "cm"), # Adjust legend key size
          legend.key.width = unit(2, "cm"),  # Increase width of legend keys for better line visibility
          legend.position = "right")  # Ensure the legend is to the right
)

# Now, use plot_grid to arrange your plots and legend
panel_plot_soil_temperature_bh_2020_2021<-plot_grid(
  plot_grid(
    BH_2020_soil_temp_avg_per_treatment_long_with_CI, BH_2021_soil_temp_avg_per_treatment_long_with_CI,
    labels = c("A", "B"),
    ncol = 2, 
    vjust = 1.5, 
    hjust = -9.6, 
    label_size = 20,
    align = "v", 
    axis = "l",
    rel_heights = c(1, 1)  # Adjust this to control relative heights of panels
  ),
  legend_soil,  # Add the adjusted legend
  ncol = 2,  # Layout: 2 columns (2 plots and 1 legend)
  rel_widths = c(4, 1)  # Increase the width for the legend
)

ggsave(panel_plot_soil_temperature_bh_2020_2021, filename = "figures/panel_plot_soil_temperature_bh_2020_2021.jpg", width = 30, height =10, dpi =600)


