
####A FOR EACH DIURNAL BH 2019####
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(agricolae)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019 <-diurnals_borden_hills_2019%>%
  filter(!pixel_number == 34 )%>%
  select(-date)

str(diurnals_borden_hills_2019)

diurnals_borden_hills_2019$date <- as.Date(paste(diurnals_borden_hills_2019$day, "2019"), format = " %j %Y")

str(diurnals_borden_hills_2019$date)

diurnals_2019_F_vs_round <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) 
#%>%
#  filter(!is.na(time))


sum(is.na(diurnals_2019_F_vs_round$time))

se <- function(x) sqrt(var(x)/length(x))

se1<- function (x) sd (x)/sqrt(length(x))

diurnals_2019_F_vs_round$time<- format(strptime(diurnals_2019_F_vs_round$time,"%H:%M:%S"), format = "%H:%M")


str(diurnals_2019_F_vs_round$time)



diurnals_2019_F_vs_round$datetime <- paste(diurnals_2019_F_vs_round$date, " ", diurnals_2019_F_vs_round$time, sep = "")

glimpse(diurnals_2019_F_vs_round) 

diurnals_2019_F_vs_round$datetime <- ymd_hm(diurnals_2019_F_vs_round$datetime, tz = "UTC")


diurnals_2019_F_vs_round$round<-format(diurnals_2019_F_vs_round$round)
diurnals_2019_F_vs_round$round<-as.numeric(as.factor(diurnals_2019_F_vs_round$round))

str(diurnals_2019_F_vs_round$round)
str(diurnals_2019_F_vs_round)

tz(diurnals_2019_F_vs_round$datetime)

tz(diurnals_2019_F_vs_round$time)

str(diurnals_2019_F_vs_round$datetime)

####ANOVA AND TUKEYS HSD IRT TEMP DIURNAL AUG 15 2019 #####

diurnals_2019_IRT_leaf_temp_vs_time_aug15<- diurnals_2019_F_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep, BH_Vine, BH_Block, BH_Leaf, leaf_temp_C) %>%
  filter(day == "227") %>%
mutate(interval = case_when(
  round == 1 ~ "8/15/2019 5:30",
  round == 2 ~ "8/15/2019 9:00",
  round == 3 ~ "8/15/2019 11:30", 
  round == 4 ~ "8/15/2019 13:30",
  round == 5 ~ "8/15/2019 17:00"
)) 

str(diurnals_2019_IRT_leaf_temp_vs_time_aug15$interval)


diurnals_2019_IRT_leaf_temp_vs_time_aug15 %>%
  group_by(interval,round, treatment) %>%
  tally()

diurnals_2019_IRT_leaf_temp_vs_time_aug15$treatment<- reorder(diurnals_2019_IRT_leaf_temp_vs_time_aug15$treatment, diurnals_2019_IRT_leaf_temp_vs_time_aug15$datetime) 

tz(diurnals_2019_IRT_leaf_temp_vs_time_aug15$datetime)
diurnals_2019_IRT_leaf_temp_vs_time_aug15$Rep<- format(diurnals_2019_IRT_leaf_temp_vs_time_aug15$Rep)
as.character(diurnals_2019_IRT_leaf_temp_vs_time_aug15$Rep)
str(diurnals_2019_IRT_leaf_temp_vs_time_aug15$Rep)


str(diurnals_2019_IRT_leaf_temp_vs_time_aug15$interval)


diurnals_2019_IRT_leaf_temp_vs_time_aug15$interval<-mdy_hm(diurnals_2019_IRT_leaf_temp_vs_time_aug15$interval)

str(diurnals_2019_IRT_leaf_temp_vs_time_aug15$interval)


diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova<-diurnals_2019_IRT_leaf_temp_vs_time_aug15

str(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova)
diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$treatment <- as.character(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$treatment)
str(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$treatment)

diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova_tally<-diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 

write.csv(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova_tally,"data_output/diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova_tally.csv")
str(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova)

diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$leaf_temp_C<-as.numeric(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$leaf_temp_C)

remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment
    filter({
      Q1 <- quantile(leaf_temp_C, 0.25, na.rm = TRUE)
      Q3 <- quantile(leaf_temp_C, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      leaf_temp_C >= lower_bound & leaf_temp_C <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova <- diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova %>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova_tally_no_outliers<-diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 

write.csv(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova_tally_no_outliers,"data_output/diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova_tally_no_outliers.csv")

diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$treatment <- as.factor(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$treatment)

str(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$treatment)
str(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$interval)

# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_IRT_leaf_temp_vs_time_aug15_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_temp_C ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_temp_C ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_temp_C ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_temp_C,
    standard_error = standard_errors$leaf_temp_C,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019 <- rbind(interval_results,results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019)
}

str(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$interval)



results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$interval <- as.POSIXct(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$interval, format = "%Y-%m-%d %H:%M:%S",tz = "UTC" )


# Check the structure to confirm the conversion
str(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$interval)


str(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019)
results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$Mean_sem <- paste(round(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$mean, 2), "±", round(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$standard_error, 2),results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$letters_ordered.groups)


str(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019)

write.csv(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019,"data_output/results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019.csv")
# Add a dummy row at 5:30 with NA values for mean and standard_error
colnames(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019)

dummy_row <- data.frame(
  interval = as.POSIXct("2019-08-15 05:30:00", tz = "UTC"),
  mean = NA,
  standard_error = NA,
  treatment = factor(NA, levels = levels(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019$treatment)),
  letters_ordered.groups = NA,  # Add any additional columns here as needed,
  Mean_sem =NA, 
  letters_ordered.leaf_temp_C = NA, 
  p_value =NA
)

str(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019)

# Combine dummy row with the original data
results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019_with_dummy <- rbind(dummy_row, results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019)

# Plot with dummy data
pd<- position_dodge(1400)

irt_temp_vs_time_aug_15_BH_2019_diurnal<-
ggplot(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019_with_dummy, aes(x = interval, y = mean, group = treatment, color = treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha = 0.6) + 
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size = 1.1, width = 3800, alpha = 0.6) +
  scale_shape_manual(values = c(16, 15, 17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab("IRT leaf temperature (°C)") +
  xlab("Time") +
  theme(axis.title.y = element_text(size = 23, family = "serif")) +
  theme(axis.title.x = element_text(size = 23, family = "serif")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.key.width = unit(0.2, "cm")) +
  theme(legend.justification = "center") +
  theme(legend.position = "right") +
  theme(legend.title.align = 0) +
  scale_y_continuous(breaks = seq(22, 42, 4), limits = c(22, 42)) +
  scale_x_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M", # No padding on the x-axis
  ) +
  theme(axis.text.x = element_text(size = 20)) +
  theme(axis.text.y = element_text(size = 20)) +
  theme(legend.position = "none")



#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
#        axis.ticks.y = element_blank())

ggsave(irt_temp_vs_time_aug_15_BH_2019_diurnal, filename = "figures/irt_temp_vs_time_aug_15_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_diurnal_IRT_leaf_temp_aug_15_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.1  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
irt_temp_vs_time_aug_15_BH_2019_diurnal_with_letters <- irt_temp_vs_time_aug_15_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(22,42,4), limits = c (22,44)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(irt_temp_vs_time_aug_15_BH_2019_diurnal_with_letters, filename = "figures/irt_temp_vs_time_aug_15_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)



#### DIURNALS BH 2020 ####

diurnals_borden_hills_2020 <-read.csv("data_output/data_physiology_all_complete_BH_2020.csv", header = TRUE)

str(diurnals_borden_hills_2020)

diurnals_2020_A_vs_time <- diurnals_borden_hills_2020 %>%
  mutate(time = hhmmss)%>%
  select(-hhmmss) %>%
  filter(!is.na(time))

se <- function(x) sqrt(var(x)/length(x))

se1<- function (x) sd (x)/sqrt(length(x))

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


#####ANOVA AND TUKEYS HSD IRT TEMP  DIURNAL July 12 2020 #####

diurnals_2020_irt_temp_vs_time_july_12<- diurnals_2020_A_vs_time %>%
  filter(!is.na(leaf_temp)) %>%
  select(datetime, day, leaf_temp, pixel_number, round, treatment, VINE, BLOCK, LEAF) %>%
  filter(day == "194")  %>% #232
  mutate(interval = case_when(
    round == 1 ~ "7/12/2020 6:00",
    round == 2 ~ "7/12/2020 9:00",
    round == 3 ~ "7/12/2020 11:30", 
    round == 4 ~ "7/12/2020 13:30",
    round == 5 ~ "7/12/2020 17:00"
  ))


diurnals_2020_irt_temp_vs_time_july_12$interval <- format((diurnals_2020_irt_temp_vs_time_july_12$interval))
diurnals_2020_irt_temp_vs_time_july_12$interval<- as.factor(diurnals_2020_irt_temp_vs_time_july_12$interval)

str(diurnals_2020_irt_temp_vs_time_july_12$interval)


diurnals_2020_irt_temp_vs_time_july_12$interval<-mdy_hm(diurnals_2020_irt_temp_vs_time_july_12$interval)

str(diurnals_2020_irt_temp_vs_time_july_12$interval)


diurnals_2020_irt_temp_vs_time_july_12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_irt_temp_vs_time_july_12$treatment<- reorder(diurnals_2020_irt_temp_vs_time_july_12$treatment, diurnals_2020_irt_temp_vs_time_july_12$datetime) 


diurnals_2020_irt_temp_vs_time_july_12_anova<-diurnals_2020_irt_temp_vs_time_july_12


diurnals_2020_irt_temp_vs_time_july_12_anova$treatment <- as.character(diurnals_2020_irt_temp_vs_time_july_12_anova$treatment)
str(diurnals_2020_irt_temp_vs_time_july_12_anova$treatment)

diurnals_2020_irt_temp_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 


write.csv(diurnals_2020_irt_temp_vs_time_july_12_anova,"data_output/diurnals_2020_irt_temp_vs_time_july_12_anova.csv")

remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment
    filter({
      Q1 <- quantile(leaf_temp, 0.25, na.rm = TRUE)
      Q3 <- quantile(leaf_temp, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      leaf_temp >= lower_bound & leaf_temp <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2020_irt_temp_vs_time_july_12_anova <- diurnals_2020_irt_temp_vs_time_july_12_anova%>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2020_irt_temp_vs_time_july_12_anova_tally_no_outliers<-diurnals_2020_irt_temp_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 


write.csv(diurnals_2020_irt_temp_vs_time_july_12_anova_tally_no_outliers,"data_output/diurnals_2020_irt_temp_vs_time_july_12_anova_tally_no_outliers.csv")
diurnals_2020_irt_temp_vs_time_july_12_anova$treatment <- as.factor(diurnals_2020_irt_temp_vs_time_july_12_anova$treatment)

str(diurnals_2020_irt_temp_vs_time_july_12_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_irt_temp_vs_time_july_12_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_IRT_leaf_temp_diurnal_july_12_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_irt_temp_vs_time_july_12_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_temp ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_temp ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_temp ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_temp,
    standard_error = standard_errors$leaf_temp,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_IRT_leaf_temp_diurnal_july_12_bh_2020 <- rbind(interval_results,results_df_IRT_leaf_temp_diurnal_july_12_bh_2020)
}

results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$interval <- as.POSIXct(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$interval, format = "%Y-%m-%d %H:%M:%S",tz = "UTC" )

# Check the structure to confirm the conversion
str(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$interval)


str(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020)
results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$Mean_sem <- paste(round(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$mean, 2), "±", round(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$standard_error, 2),results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$letters_ordered.groups)


str(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020)

write.csv(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020,"data_output/results_df_IRT_leaf_temp_diurnal_july_12_bh_2020.csv")


colnames(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020)

dummy_row <- data.frame(
  interval = as.POSIXct("2020-07-12 05:30:00", tz = "UTC"),
  mean = NA,
  standard_error = NA,
  treatment = factor(NA, levels = levels(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020$treatment)),
  letters_ordered.groups = NA,  # Add any additional columns here as needed,
  Mean_sem =NA, 
  letters_ordered.leaf_temp = NA, 
  p_value =NA
)

str(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020)

# Combine dummy row with the original data
results_df_IRT_leaf_temp_diurnal_july_12_bh_2020_with_dummy <- rbind(dummy_row, results_df_IRT_leaf_temp_diurnal_july_12_bh_2020)


pd<- position_dodge(1400)

irt_temp_vs_time_july_12_BH_2020_diurnal<-
  ggplot(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020_with_dummy, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("IRT leaf temperature (°C)") +
#  ggtitle("HW3 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(22,42,4), limits = c (22,44)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(irt_temp_vs_time_july_12_BH_2020_diurnal, filename = "figures/irt_temp_vs_time_july_12_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_IRT_leaf_temp_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_IRT_leaf_temp_diurnal_july_12_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by date and descending order of significance letters
  group_by(interval) %>%
  mutate(
    max_mean = ifelse(all(is.na(mean)), NA, max(mean, na.rm = TRUE)),  # Handle cases where all values are NA
    y_position = max_mean + vertical_offset * (row_number() - 1.9)
  )

# Create the plot with letters of significance above the max mean points
irt_temp_vs_time_july_12_BH_2020_diurnal_with_letters <- irt_temp_vs_time_july_12_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(22,42,4), limits = c (22,44)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(irt_temp_vs_time_july_12_BH_2020_diurnal_with_letters, filename = "figures/irt_temp_vs_time_july_12_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####Figure 5 A, GSW AND IRT TEMP HW2 2019 AND HW3 2020 DIURNAL #####


library(cowplot)
panel_plot_gas_exchange_and_leaf_temp_bh_HW2_2019_HW3_2020_diurnal <- plot_grid(
  plot_grid(A_vs_time_aug_15_BH_2019_diurnal_with_letters,A_vs_time_july_12_BH_2020_diurnal_with_letters, gsw_vs_time_aug_15_BH_2019_diurnal_with_letters,gsw_vs_time_july_12_BH_2020_diurnal_with_letters, irt_temp_vs_time_aug_15_BH_2019_diurnal_with_letters, irt_temp_vs_time_july_12_BH_2020_diurnal_with_letters,  labels = c("A", "B", "C", "D", "E", "F"),ncol = 2, 
            vjust = 1.5, 
            hjust = -9.6, 
            label_size = 18,
            align = "v", axis = "l") # Adjust the height of the last row separately
)


ggsave(panel_plot_gas_exchange_and_leaf_temp_bh_HW2_2019_HW3_2020_diurnal, filename = "figures/panel_plot_gas_exchange_and_leaf_temp_bh_HW2_2019_HW3_2020_diurnal.jpg", width = 16, height =18, dpi = 600)


####ANOVA AND TUKEYS HSD TRANSPIRATION DIURNAL AUG 15 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_E_vs_time_aug15<- diurnals_2019_F_vs_round %>%
  filter(!is.na(E))%>%
  select(datetime, day, E, pixel_number, round, treatment, Rep, BH_Vine, BH_Block, BH_Leaf) %>%
  filter(day == "227") %>%
  mutate(interval = case_when(
    round == 1 ~ "8/15/2019 5:30",
    round == 2 ~ "8/15/2019 9:00",
    round == 3 ~ "8/15/2019 11:30", 
    round == 4 ~ "8/15/2019 13:30",
    round == 5 ~ "8/15/2019 17:00"
  )) 


str(diurnals_2019_E_vs_time_aug15$interval)

diurnals_2019_E_vs_time_aug15$interval<-mdy_hm(diurnals_2019_E_vs_time_aug15$interval)
diurnals_2019_E_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_E_vs_time_aug15$treatment<- reorder(diurnals_2019_E_vs_time_aug15$treatment, diurnals_2019_E_vs_time_aug15$datetime) 

tz(diurnals_2019_E_vs_time_aug15$datetime)
diurnals_2019_E_vs_time_aug15$Rep<- format(diurnals_2019_E_vs_time_aug15$Rep)
as.character(diurnals_2019_E_vs_time_aug15$Rep)
str(diurnals_2019_E_vs_time_aug15$Rep)


str(diurnals_2019_E_vs_time_aug15$interval)




str(diurnals_2019_E_vs_time_aug15$interval)


diurnals_2019_E_vs_time_aug15_anova<-diurnals_2019_E_vs_time_aug15


diurnals_2019_E_vs_time_aug15_anova$treatment <- as.character(diurnals_2019_E_vs_time_aug15_anova$treatment)
str(diurnals_2019_E_vs_time_aug15_anova$treatment)

diurnals_2019_E_vs_time_aug15_anova_tally<-diurnals_2019_E_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 


write.csv(diurnals_2019_E_vs_time_aug15_anova_tally,"data_output/diurnals_2019_E_vs_time_aug15_anova_tally.csv")


remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment
    filter({
      Q1 <- quantile(E, 0.25, na.rm = TRUE)
      Q3 <- quantile(E, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      E >= lower_bound & E <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2019_E_vs_time_aug15_anova <- diurnals_2019_E_vs_time_aug15_anova %>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2019_E_vs_time_aug15_anova_tally_no_outliers<-diurnals_2019_E_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 

check_vine_nested_2019<-diurnals_2019_E_vs_time_aug15_anova%>%
  group_by(interval, treatment, BH_Block, BH_Vine)%>%
  tally() 


str(diurnals_2019_E_vs_time_aug15_anova)
write.csv(diurnals_2019_E_vs_time_aug15_anova_tally_no_outliers,"data_output/diurnals_2019_E_vs_time_aug15_anova_tally_no_outliers.csv")
diurnals_2019_E_vs_time_aug15_anova$treatment <- as.factor(diurnals_2019_E_vs_time_aug15_anova$treatment)

str(diurnals_2019_E_vs_time_aug15_anova$treatment)
str(diurnals_2019_E_vs_time_aug15_anova)

# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_E_vs_time_aug15_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_E_diurnal_aug_15_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_E_vs_time_aug15_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(E ~ treatment/BH_Vine, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(E ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(E ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$E,
    standard_error = standard_errors$E,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_E_diurnal_aug_15_bh_2019 <- rbind(interval_results,results_df_E_diurnal_aug_15_bh_2019)
}

results_df_E_diurnal_aug_15_bh_2019$interval <- as.POSIXct(results_df_E_diurnal_aug_15_bh_2019$interval, format = "%m-%d-%Y %H:%M",tz = "UTC")

# Check the structure to confirm the conversion
str(results_df_E_diurnal_aug_15_bh_2019$interval)


str(results_df_E_diurnal_aug_15_bh_2019)
results_df_E_diurnal_aug_15_bh_2019$Mean_sem <- paste(round(results_df_E_diurnal_aug_15_bh_2019$mean, 2), "±", round(results_df_E_diurnal_aug_15_bh_2019$standard_error, 2),results_df_E_diurnal_aug_15_bh_2019$letters_ordered.groups)


str(results_df_E_diurnal_aug_15_bh_2019)

write.csv(results_df_E_diurnal_aug_15_bh_2019,"data_output/results_df_E_diurnal_aug_15_bh_2019.csv")

pd<- position_dodge(1400)

E_vs_time_aug_15_BH_2019_diurnal<-
  ggplot(results_df_E_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Transpiration~(mol~m^{-2}~s^{-1}))) +
  ggtitle("HW2 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.012,0.003), limits = c (0,0.012)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave(E_vs_time_aug_15_BH_2019_diurnal, filename = "figures/E_vs_time_aug_15_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_E_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_E_diurnal_aug_15_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.0008  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
E_vs_time_aug_15_BH_2019_diurnal_with_letters <- E_vs_time_aug_15_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.012,0.003), limits = c (0,0.014)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(E_vs_time_aug_15_BH_2019_diurnal_with_letters, filename = "figures/E_vs_time_aug_15_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####ANOVA AND TUKEYS HSD WUE INSTANTENOUS DIURNAL AUG 15 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_WUE_vs_time_aug15<- diurnals_2019_F_vs_round %>%
  mutate( WUE = A/E)%>%
  filter(!is.na(WUE))%>%
  select(datetime, day, WUE, pixel_number, round, treatment, Rep, BH_Vine, BH_Block, BH_Leaf) %>%
  filter(day == "227") %>%
  mutate(interval = case_when(
    round == 1 ~ "8/15/2019 5:30",
    round == 2 ~ "8/15/2019 9:00",
    round == 3 ~ "8/15/2019 11:30", 
    round == 4 ~ "8/15/2019 13:30",
    round == 5 ~ "8/15/2019 17:00"
  )) 


str(diurnals_2019_WUE_vs_time_aug15$interval)

diurnals_2019_WUE_vs_time_aug15$interval<-mdy_hm(diurnals_2019_WUE_vs_time_aug15$interval)
diurnals_2019_WUE_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_WUE_vs_time_aug15$treatment<- reorder(diurnals_2019_WUE_vs_time_aug15$treatment, diurnals_2019_WUE_vs_time_aug15$datetime) 

tz(diurnals_2019_WUE_vs_time_aug15$datetime)
diurnals_2019_WUE_vs_time_aug15$Rep<- format(diurnals_2019_WUE_vs_time_aug15$Rep)
as.character(diurnals_2019_WUE_vs_time_aug15$Rep)
str(diurnals_2019_WUE_vs_time_aug15$Rep)


str(diurnals_2019_WUE_vs_time_aug15$interval)




str(diurnals_2019_WUE_vs_time_aug15$interval)


diurnals_2019_WUE_vs_time_aug15_anova<-diurnals_2019_WUE_vs_time_aug15


diurnals_2019_WUE_vs_time_aug15_anova$treatment <- as.character(diurnals_2019_WUE_vs_time_aug15_anova$treatment)
str(diurnals_2019_WUE_vs_time_aug15_anova$treatment)

diurnals_2019_WUE_vs_time_aug15_anova_tally<-diurnals_2019_WUE_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 


write.csv(diurnals_2019_WUE_vs_time_aug15_anova_tally,"data_output/diurnals_2019_WUE_vs_time_aug15_anova_tally.csv")


remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment
    filter({
      Q1 <- quantile(WUE, 0.25, na.rm = TRUE)
      Q3 <- quantile(WUE, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      WUE >= lower_bound & WUE <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2019_WUE_vs_time_aug15_anova <- diurnals_2019_WUE_vs_time_aug15_anova %>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2019_WUE_vs_time_aug15_anova_tally_no_outliers<-diurnals_2019_WUE_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 


write.csv(diurnals_2019_WUE_vs_time_aug15_anova_tally_no_outliers,"data_output/diurnals_2019_WUE_vs_time_aug15_anova_tally_no_outliers.csv")
diurnals_2019_WUE_vs_time_aug15_anova<-diurnals_2019_WUE_vs_time_aug15_anova%>%
  filter(!round ==1 )


diurnals_2019_WUE_vs_time_aug15_anova$treatment <- as.factor(diurnals_2019_WUE_vs_time_aug15_anova$treatment)

str(diurnals_2019_WUE_vs_time_aug15_anova$treatment)
str(diurnals_2019_WUE_vs_time_aug15_anova)

# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_WUE_vs_time_aug15_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_WUE_diurnal_aug_15_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_WUE_vs_time_aug15_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE ~ treatment/BH_Vine, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(WUE ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(WUE ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE,
    standard_error = standard_errors$WUE,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_WUE_diurnal_aug_15_bh_2019 <- rbind(interval_results,results_df_WUE_diurnal_aug_15_bh_2019)
}

results_df_WUE_diurnal_aug_15_bh_2019$interval <- as.POSIXct(results_df_WUE_diurnal_aug_15_bh_2019$interval, format = "%m-%d-%Y %H:%M",tz = "UTC")

# Check the structure to confirm the conversion
str(results_df_WUE_diurnal_aug_15_bh_2019$interval)


str(results_df_WUE_diurnal_aug_15_bh_2019)
results_df_WUE_diurnal_aug_15_bh_2019$Mean_sem <- paste(round(results_df_WUE_diurnal_aug_15_bh_2019$mean, 2), "±", round(results_df_WUE_diurnal_aug_15_bh_2019$standard_error, 2),results_df_WUE_diurnal_aug_15_bh_2019$letters_ordered.groups)


str(results_df_WUE_diurnal_aug_15_bh_2019)

write.csv(results_df_WUE_diurnal_aug_15_bh_2019,"data_output/results_df_WUE_diurnal_aug_15_bh_2019.csv")

# Add a dummy row at 5:30 with NA values for mean and standard_error
colnames(results_df_WUE_diurnal_aug_15_bh_2019)

dummy_row <- data.frame(
  interval = as.POSIXct("2019-08-15 05:30:00", tz = "UTC"),
  mean = NA,
  standard_error = NA,
  treatment = factor(NA, levels = levels(results_df_WUE_diurnal_aug_15_bh_2019$treatment)),
  letters_ordered.groups = NA,  # Add any additional columns here as needed,
  Mean_sem =NA, 
  letters_ordered.WUE = NA, 
  p_value =NA
)

str(results_df_WUE_diurnal_aug_15_bh_2019)

# Combine dummy row with the original data
results_df_WUE_diurnal_aug_15_bh_2019_with_dummy <- rbind(dummy_row, results_df_WUE_diurnal_aug_15_bh_2019)

# Plot with dummy data
pd<- position_dodge(1400)

WUE_vs_time_aug_15_BH_2019_diurnal<-
  ggplot(results_df_WUE_diurnal_aug_15_bh_2019_with_dummy, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/E ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
 # ggtitle("HW2 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(1000,3700,700), limits = c (1000,3700)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave(WUE_vs_time_aug_15_BH_2019_diurnal, filename = "figures/WUE_vs_time_aug_15_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_WUE_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_WUE_diurnal_aug_15_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 120  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.5)) 

# Create the plot with letters of significance above the max mean points
WUE_vs_time_aug_15_BH_2019_diurnal_with_letters <- WUE_vs_time_aug_15_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(1000,3700,700), limits = c (1000,3700)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(WUE_vs_time_aug_15_BH_2019_diurnal_with_letters, filename = "figures/WUE_vs_time_aug_15_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####ANOVA AND TUKEYS HSD WUE INTRINSIC DIURNAL AUG 15 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_WUE_int_vs_time_aug15<- diurnals_2019_F_vs_round %>%
  mutate( WUE_int = A/gsw)%>%
  filter(!is.na(WUE_int))%>%
  select(datetime, day, WUE_int, pixel_number, round, treatment, Rep, BH_Vine, BH_Block, BH_Leaf) %>%
  filter(day == "227") %>%
  mutate(interval = case_when(
    round == 1 ~ "8/15/2019 5:30",
    round == 2 ~ "8/15/2019 9:00",
    round == 3 ~ "8/15/2019 11:30", 
    round == 4 ~ "8/15/2019 13:30",
    round == 5 ~ "8/15/2019 17:00"
  )) 


str(diurnals_2019_WUE_int_vs_time_aug15$interval)

diurnals_2019_WUE_int_vs_time_aug15$interval<-mdy_hm(diurnals_2019_WUE_int_vs_time_aug15$interval)
diurnals_2019_WUE_int_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_WUE_int_vs_time_aug15$treatment<- reorder(diurnals_2019_WUE_int_vs_time_aug15$treatment, diurnals_2019_WUE_int_vs_time_aug15$datetime) 

tz(diurnals_2019_WUE_int_vs_time_aug15$datetime)
diurnals_2019_WUE_int_vs_time_aug15$Rep<- format(diurnals_2019_WUE_int_vs_time_aug15$Rep)
as.character(diurnals_2019_WUE_int_vs_time_aug15$Rep)
str(diurnals_2019_WUE_int_vs_time_aug15$Rep)


str(diurnals_2019_WUE_int_vs_time_aug15$interval)




str(diurnals_2019_WUE_int_vs_time_aug15$interval)


diurnals_2019_WUE_int_vs_time_aug15_anova<-diurnals_2019_WUE_int_vs_time_aug15


diurnals_2019_WUE_int_vs_time_aug15_anova$treatment <- as.character(diurnals_2019_WUE_int_vs_time_aug15_anova$treatment)
str(diurnals_2019_WUE_int_vs_time_aug15_anova$treatment)

diurnals_2019_WUE_int_vs_time_aug15_anova_tally<-diurnals_2019_WUE_int_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 


write.csv(diurnals_2019_WUE_int_vs_time_aug15_anova_tally,"data_output/diurnals_2019_WUE_int_vs_time_aug15_anova_tally.csv")


remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment
    filter({
      Q1 <- quantile(WUE_int, 0.25, na.rm = TRUE)
      Q3 <- quantile(WUE_int, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      WUE_int >= lower_bound & WUE_int <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2019_WUE_int_vs_time_aug15_anova <- diurnals_2019_WUE_int_vs_time_aug15_anova %>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2019_WUE_int_vs_time_aug15_anova_tally_no_outliers<-diurnals_2019_WUE_int_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 


write.csv(diurnals_2019_WUE_int_vs_time_aug15_anova_tally_no_outliers,"data_output/diurnals_2019_WUE_int_vs_time_aug15_anova_tally_no_outliers.csv")
diurnals_2019_WUE_int_vs_time_aug15_anova<-diurnals_2019_WUE_int_vs_time_aug15_anova%>%
  filter(!round ==1 )


diurnals_2019_WUE_int_vs_time_aug15_anova$treatment <- as.factor(diurnals_2019_WUE_int_vs_time_aug15_anova$treatment)

str(diurnals_2019_WUE_int_vs_time_aug15_anova$treatment)
str(diurnals_2019_WUE_int_vs_time_aug15_anova)

# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_WUE_int_vs_time_aug15_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_WUE_int_diurnal_aug_15_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_WUE_int_vs_time_aug15_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE_int ~ treatment/BH_Vine, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(WUE_int ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(WUE_int ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE_int,
    standard_error = standard_errors$WUE_int,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_WUE_int_diurnal_aug_15_bh_2019 <- rbind(interval_results,results_df_WUE_int_diurnal_aug_15_bh_2019)
}

results_df_WUE_int_diurnal_aug_15_bh_2019$interval <- as.POSIXct(results_df_WUE_int_diurnal_aug_15_bh_2019$interval, format = "%m-%d-%Y %H:%M",tz = "UTC")

# Check the structure to confirm the conversion
str(results_df_WUE_int_diurnal_aug_15_bh_2019$interval)


str(results_df_WUE_int_diurnal_aug_15_bh_2019)
results_df_WUE_int_diurnal_aug_15_bh_2019$Mean_sem <- paste(round(results_df_WUE_int_diurnal_aug_15_bh_2019$mean, 2), "±", round(results_df_WUE_int_diurnal_aug_15_bh_2019$standard_error, 2),results_df_WUE_int_diurnal_aug_15_bh_2019$letters_ordered.groups)


str(results_df_WUE_int_diurnal_aug_15_bh_2019)

write.csv(results_df_WUE_int_diurnal_aug_15_bh_2019,"data_output/results_df_WUE_int_diurnal_aug_15_bh_2019.csv")

# Add a dummy row at 5:30 with NA values for mean and standard_error
colnames(results_df_WUE_int_diurnal_aug_15_bh_2019)

dummy_row <- data.frame(
  interval = as.POSIXct("2019-08-15 05:30:00", tz = "UTC"),
  mean = NA,
  standard_error = NA,
  treatment = factor(NA, levels = levels(results_df_WUE_int_diurnal_aug_15_bh_2019$treatment)),
  letters_ordered.groups = NA,  # Add any additional columns here as needed,
  Mean_sem =NA, 
  letters_ordered.WUE_int = NA, 
  p_value =NA
)

str(results_df_WUE_int_diurnal_aug_15_bh_2019)

# Combine dummy row with the original data
results_df_WUE_int_diurnal_aug_15_bh_2019_with_dummy <- rbind(dummy_row, results_df_WUE_int_diurnal_aug_15_bh_2019)

# Plot with dummy data
pd<- position_dodge(1400)

WUE_int_vs_time_aug_15_BH_2019_diurnal<-
  ggplot(results_df_WUE_int_diurnal_aug_15_bh_2019_with_dummy, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/g[s] ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
  # ggtitle("HW2 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(40,160,40), limits = c (40,160)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")

ggsave(WUE_int_vs_time_aug_15_BH_2019_diurnal, filename = "figures/WUE_int_vs_time_aug_15_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_WUE_int_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_WUE_int_diurnal_aug_15_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 10  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.5)) 

# Create the plot with letters of significance above the max mean points
WUE_int_vs_time_aug_15_BH_2019_diurnal_with_letters <- WUE_int_vs_time_aug_15_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(20,160,40), limits = c (20,160)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(WUE_int_vs_time_aug_15_BH_2019_diurnal_with_letters, filename = "figures/WUE_int_vs_time_aug_15_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD TRANSPIRATION DIURNAL july 12 2020 #####

diurnals_2020_E_vs_time_july_12<- diurnals_2020_A_vs_time %>%
  filter(!is.na(E)) %>%
  select(datetime, day, E, pixel_number, round, treatment, VINE, BLOCK, LEAF) %>%
  filter(day == "194")  %>%
  mutate(interval = case_when(
    round == 1 ~ "7/12/2020 5:30",
    round == 2 ~ "7/12/2020 9:00",
    round == 3 ~ "7/12/2020 11:30", 
    round == 4 ~ "7/12/2020 13:30",
    round == 5 ~ "7/12/2020 17:00"
  ))

diurnals_2020_E_vs_time_july_12$interval<-as.POSIXct(diurnals_2020_E_vs_time_july_12$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_E_vs_time_july_12$interval)


diurnals_2020_E_vs_time_july_12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_E_vs_time_july_12$treatment<- reorder(diurnals_2020_E_vs_time_july_12$treatment, diurnals_2020_E_vs_time_july_12$datetime) 


diurnals_2020_E_vs_time_july_12_anova<-diurnals_2020_E_vs_time_july_12


diurnals_2020_E_vs_time_july_12_anova_tally<-diurnals_2020_E_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

write.csv(diurnals_2020_E_vs_time_july_12_anova_tally,"data_output/diurnals_2020_E_vs_time_july_12_anova_tally.csv")

remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment and interval
    filter({
      if (unique(interval) == "2020-07-12 05:30:00") {  # Check for the specific interval only once per group
        TRUE  # Retain all values for the first interval
      } else {
        Q1 <- quantile(E, 0.25, na.rm = TRUE)
        Q3 <- quantile(E, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        E >= lower_bound & E <= upper_bound  # Apply filtering for other intervals
      }
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2020_E_vs_time_july_12_anova <- diurnals_2020_E_vs_time_july_12_anova %>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2020_E_vs_time_july_12_anova_tally_no_outliers<-diurnals_2020_E_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

str(diurnals_2020_E_vs_time_july_12_anova)
vine_check_nested<-diurnals_2020_E_vs_time_july_12_anova%>%
  group_by(interval, treatment, VINE)%>%
  tally() 


write.csv(diurnals_2020_E_vs_time_july_12_anova_tally_no_outliers,"data_output/diurnals_2020_E_vs_time_july_12_anova_tally_no_outliers.csv")
diurnals_2020_E_vs_time_july_12_anova$treatment <- as.character(diurnals_2020_E_vs_time_july_12_anova$treatment)

diurnals_2020_E_vs_time_july_12_anova$treatment <- as.factor(diurnals_2020_E_vs_time_july_12_anova$treatment)


str(diurnals_2020_E_vs_time_july_12_anova$treatment)


str(diurnals_2020_E_vs_time_july_12_anova)
sum(is.na(str(diurnals_2020_E_vs_time_july_12_anova$E)
))

sum(is.na(str(diurnals_2020_E_vs_time_july_12_anova$VINE)
))
sum(is.na(str(diurnals_2020_E_vs_time_july_12_anova$interval)
))
# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_E_vs_time_july_12_anova$interval) 
str(diurnals_2020_E_vs_time_july_12_anova)
# Create an empty dataframe to store results

results_df_E_diurnal_july_12_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_E_vs_time_july_12_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(E ~ treatment  /VINE, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(E ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(E ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$E,
    standard_error = standard_errors$E,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_E_diurnal_july_12_bh_2020 <- rbind(interval_results,results_df_E_diurnal_july_12_bh_2020)
}

results_df_E_diurnal_july_12_bh_2020$interval <- as.POSIXct(results_df_E_diurnal_july_12_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_E_diurnal_july_12_bh_2020$interval)


str(results_df_E_diurnal_july_12_bh_2020)
results_df_E_diurnal_july_12_bh_2020$Mean_sem <- paste(round(results_df_E_diurnal_july_12_bh_2020$mean, 2), "±", round(results_df_E_diurnal_july_12_bh_2020$standard_error, 2),results_df_E_diurnal_july_12_bh_2020$letters_ordered.groups)


str(results_df_E_diurnal_july_12_bh_2020)

write.csv(results_df_E_diurnal_july_12_bh_2020,"data_output/results_df_E_diurnal_july_12_bh_2020.csv")

pd<- position_dodge(1400)

E_vs_time_july_12_BH_2020_diurnal<-
  ggplot(results_df_E_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Transpiration~(mol~m^{-2}~s^{-1}))) +
  ggtitle("HW2 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.012,0.003), limits = c (0,0.014)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(E_vs_time_july_12_BH_2020_diurnal, filename = "figures/E_vs_time_july_12_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_E_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_E_diurnal_july_12_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.0009  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.4)) 

# Create the plot with letters of significance above the max mean points
E_vs_time_july_12_BH_2020_diurnal_with_letters <- E_vs_time_july_12_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.012,0.003), limits = c (0,0.014)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(E_vs_time_july_12_BH_2020_diurnal_with_letters, filename = "figures/E_vs_time_july_12_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD WUE INSTANTANEOUS DIURNAL july 12 2020 #####

diurnals_2020_WUE_vs_time_july_12<- diurnals_2020_A_vs_time %>%
  mutate(WUE = A/E)%>%
  filter(!is.na(WUE)) %>%
  select(datetime, day, WUE, pixel_number, round, treatment, VINE, BLOCK, LEAF) %>%
  filter(day == "194")  %>%
  mutate(interval = case_when(
    round == 1 ~ "8/19/2020 5:30",
    round == 2 ~ "8/19/2020 9:00",
    round == 3 ~ "8/19/2020 11:30", 
    round == 4 ~ "8/19/2020 13:30",
    round == 5 ~ "8/19/2020 17:00"
  ))


diurnals_2020_WUE_vs_time_july_12$interval<-mdy_hm(diurnals_2020_WUE_vs_time_july_12$interval)

str(diurnals_2020_WUE_vs_time_july_12$interval)


diurnals_2020_WUE_vs_time_july_12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_WUE_vs_time_july_12$treatment<- reorder(diurnals_2020_WUE_vs_time_july_12$treatment, diurnals_2020_WUE_vs_time_july_12$datetime) 


diurnals_2020_WUE_vs_time_july_12_anova<-diurnals_2020_WUE_vs_time_july_12


diurnals_2020_WUE_vs_time_july_12_anova$treatment <- as.character(diurnals_2020_WUE_vs_time_july_12_anova$treatment)
str(diurnals_2020_WUE_vs_time_july_12_anova$treatment)

diurnals_2020_WUE_vs_time_july_12_anova_tally<-diurnals_2020_WUE_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

write.csv(diurnals_2020_WUE_vs_time_july_12_anova_tally,"data_output/diurnals_2020_WUE_vs_time_july_12_anova_tally.csv")

remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment
    filter({
      Q1 <- quantile(WUE, 0.25, na.rm = TRUE)
      Q3 <- quantile(WUE, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      WUE >= lower_bound & WUE <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2020_WUE_vs_time_july_12_anova <- diurnals_2020_WUE_vs_time_july_12_anova %>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2020_WUE_vs_time_july_12_anova_tally_no_outliers<-diurnals_2020_WUE_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

write.csv(diurnals_2020_WUE_vs_time_july_12_anova_tally_no_outliers,"data_output/diurnals_2020_WUE_vs_time_july_12_anova_tally_no_outliers.csv")

diurnals_2020_WUE_vs_time_july_12_anova<-diurnals_2020_WUE_vs_time_july_12_anova%>%
  filter(!round ==1)

diurnals_2020_WUE_vs_time_july_12_anova$treatment <- as.factor(diurnals_2020_WUE_vs_time_july_12_anova$treatment)

str(diurnals_2020_WUE_vs_time_july_12_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_WUE_vs_time_july_12_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_WUE_diurnal_july_12_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_WUE_vs_time_july_12_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE ~ treatment /VINE, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(WUE ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(WUE ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE,
    standard_error = standard_errors$WUE,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_WUE_diurnal_july_12_bh_2020 <- rbind(interval_results,results_df_WUE_diurnal_july_12_bh_2020)
}

results_df_WUE_diurnal_july_12_bh_2020$interval <- as.POSIXct(results_df_WUE_diurnal_july_12_bh_2020$interval, format = "%m/%d/%Y %H:%M", tz ="UTC")

# Check the structure to confirm the conversion
str(results_df_WUE_diurnal_july_12_bh_2020$interval)


str(results_df_WUE_diurnal_july_12_bh_2020)
results_df_WUE_diurnal_july_12_bh_2020$Mean_sem <- paste(round(results_df_WUE_diurnal_july_12_bh_2020$mean, 2), "±", round(results_df_WUE_diurnal_july_12_bh_2020$standard_error, 2),results_df_WUE_diurnal_july_12_bh_2020$letters_ordered.groups)


str(results_df_WUE_diurnal_july_12_bh_2020)

write.csv(results_df_WUE_diurnal_july_12_bh_2020,"data_output/results_df_WUE_diurnal_july_12_bh_2020.csv")

# Add a dummy row at 5:30 with NA values for mean and standard_error
colnames(results_df_WUE_diurnal_july_12_bh_2020)

dummy_row <- data.frame(
  interval = as.POSIXct("2020-08-19 05:30:00", tz = "UTC"),
  mean = NA,
  standard_error = NA,
  treatment = factor(NA, levels = levels(results_df_WUE_diurnal_july_12_bh_2020$treatment)),
  letters_ordered.groups = NA,  # Add any additional columns here as needed,
  Mean_sem =NA, 
  letters_ordered.WUE = NA, 
  p_value =NA
)

str(results_df_WUE_diurnal_july_12_bh_2020)

# Combine dummy row with the original data
results_df_WUE_diurnal_july_12_bh_2020_with_dummy <- rbind(dummy_row, results_df_WUE_diurnal_july_12_bh_2020)



pd<- position_dodge(1400)

WUE_vs_time_july_12_BH_2020_diurnal<-
  ggplot(results_df_WUE_diurnal_july_12_bh_2020_with_dummy , aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/E ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
#  ggtitle("HW3 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(1000,3700,700), limits = c (1000,3700)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(WUE_vs_time_july_12_BH_2020_diurnal, filename = "figures/WUE_vs_time_july_12_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_WUE_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_WUE_diurnal_july_12_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 140  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.0)) 

# Create the plot with letters of significance above the max mean points
WUE_vs_time_july_12_BH_2020_diurnal_with_letters <- WUE_vs_time_july_12_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(1000,3700,700), limits = c (1000,3700)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(WUE_vs_time_july_12_BH_2020_diurnal_with_letters, filename = "figures/WUE_vs_time_july_12_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


#####ANOVA AND TUKEYS HSD WUE INTRINSIC DIURNAL july 12 2020 #####

diurnals_2020_WUE_int_vs_time_july_12<- diurnals_2020_A_vs_time %>%
  mutate(WUE_int = A/gsw)%>%
  filter(!is.na(WUE_int)) %>%
  select(datetime, day, WUE_int, pixel_number, round, treatment, VINE, BLOCK, LEAF) %>%
  filter(day == "194")  %>%
  mutate(interval = case_when(
    round == 1 ~ "8/19/2020 5:30",
    round == 2 ~ "8/19/2020 9:00",
    round == 3 ~ "8/19/2020 11:30", 
    round == 4 ~ "8/19/2020 13:30",
    round == 5 ~ "8/19/2020 17:00"
  ))


diurnals_2020_WUE_int_vs_time_july_12$interval<-mdy_hm(diurnals_2020_WUE_int_vs_time_july_12$interval)

str(diurnals_2020_WUE_int_vs_time_july_12$interval)


diurnals_2020_WUE_int_vs_time_july_12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_WUE_int_vs_time_july_12$treatment<- reorder(diurnals_2020_WUE_int_vs_time_july_12$treatment, diurnals_2020_WUE_int_vs_time_july_12$datetime) 


diurnals_2020_WUE_int_vs_time_july_12_anova<-diurnals_2020_WUE_int_vs_time_july_12


diurnals_2020_WUE_int_vs_time_july_12_anova$treatment <- as.character(diurnals_2020_WUE_int_vs_time_july_12_anova$treatment)
str(diurnals_2020_WUE_int_vs_time_july_12_anova$treatment)

diurnals_2020_WUE_int_vs_time_july_12_anova_tally<-diurnals_2020_WUE_int_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

write.csv(diurnals_2020_WUE_int_vs_time_july_12_anova_tally,"data_output/diurnals_2020_WUE_int_vs_time_july_12_anova_tally.csv")

remove_outliers_iqr_by_treatment_interval <- function(data) {
  data %>%
    group_by(treatment, interval) %>%  # Group by treatment
    filter({
      Q1 <- quantile(WUE_int, 0.25, na.rm = TRUE)
      Q3 <- quantile(WUE_int, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      WUE_int >= lower_bound & WUE_int <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

diurnals_2020_WUE_int_vs_time_july_12_anova <- diurnals_2020_WUE_int_vs_time_july_12_anova %>%
  do(remove_outliers_iqr_by_treatment_interval(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2020_WUE_int_vs_time_july_12_anova_tally_no_outliers<-diurnals_2020_WUE_int_vs_time_july_12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

write.csv(diurnals_2020_WUE_int_vs_time_july_12_anova_tally_no_outliers,"data_output/diurnals_2020_WUE_int_vs_time_july_12_anova_tally_no_outliers.csv")


diurnals_2020_WUE_int_vs_time_july_12_anova<-diurnals_2020_WUE_int_vs_time_july_12_anova%>%
  filter(!round ==1)

diurnals_2020_WUE_int_vs_time_july_12_anova$treatment <- as.factor(diurnals_2020_WUE_int_vs_time_july_12_anova$treatment)

str(diurnals_2020_WUE_int_vs_time_july_12_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_WUE_int_vs_time_july_12_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_WUE_int_diurnal_july_12_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_WUE_int_vs_time_july_12_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE_int ~ treatment /VINE, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(WUE_int ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(WUE_int ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current interval
  interval_results <- data.frame(
    interval = current_interval,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE_int,
    standard_error = standard_errors$WUE_int,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_WUE_int_diurnal_july_12_bh_2020 <- rbind(interval_results,results_df_WUE_int_diurnal_july_12_bh_2020)
}

results_df_WUE_int_diurnal_july_12_bh_2020$interval <- as.POSIXct(results_df_WUE_int_diurnal_july_12_bh_2020$interval, format = "%m/%d/%Y %H:%M", tz ="UTC")

# Check the structure to confirm the conversion
str(results_df_WUE_int_diurnal_july_12_bh_2020$interval)


str(results_df_WUE_int_diurnal_july_12_bh_2020)
results_df_WUE_int_diurnal_july_12_bh_2020$Mean_sem <- paste(round(results_df_WUE_int_diurnal_july_12_bh_2020$mean, 2), "±", round(results_df_WUE_int_diurnal_july_12_bh_2020$standard_error, 2),results_df_WUE_int_diurnal_july_12_bh_2020$letters_ordered.groups)


str(results_df_WUE_int_diurnal_july_12_bh_2020)

write.csv(results_df_WUE_int_diurnal_july_12_bh_2020,"data_output/results_df_WUE_int_diurnal_july_12_bh_2020.csv")

# Add a dummy row at 5:30 with NA values for mean and standard_error
colnames(results_df_WUE_int_diurnal_july_12_bh_2020)

dummy_row <- data.frame(
  interval = as.POSIXct("2020-08-19 05:30:00", tz = "UTC"),
  mean = NA,
  standard_error = NA,
  treatment = factor(NA, levels = levels(results_df_WUE_int_diurnal_july_12_bh_2020$treatment)),
  letters_ordered.groups = NA,  # Add any additional columns here as needed,
  Mean_sem =NA, 
  letters_ordered.WUE_int = NA, 
  p_value =NA
)

str(results_df_WUE_int_diurnal_july_12_bh_2020)

# Combine dummy row with the original data
results_df_WUE_int_diurnal_july_12_bh_2020_with_dummy <- rbind(dummy_row, results_df_WUE_int_diurnal_july_12_bh_2020)



pd<- position_dodge(1400)

WUE_int_vs_time_july_12_BH_2020_diurnal<-
  ggplot(results_df_WUE_int_diurnal_july_12_bh_2020_with_dummy , aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
    geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/g[s] ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
  #  ggtitle("HW3 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(40,160,40), limits = c (40,160)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(WUE_int_vs_time_july_12_BH_2020_diurnal, filename = "figures/WUE_int_vs_time_july_12_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_WUE_int_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_WUE_int_diurnal_july_12_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 10  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.8)) 

# Create the plot with letters of significance above the max mean points
WUE_int_vs_time_july_12_BH_2020_diurnal_with_letters <- WUE_int_vs_time_july_12_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(40,160,40), limits = c (40,160))
# + theme(legend.position = c(0.2, 0.2))


ggsave(WUE_int_vs_time_july_12_BH_2020_diurnal_with_letters, filename = "figures/WUE_int_vs_time_july_12_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


#####figure 6 E, A/E AND A/gsw TEMP HW2 2019 AND HW3 2020 DIURNAL #####


library(cowplot)
panel_plot_E_and_WUE_bh_HW2_2019_HW3_2020_diurnal <- plot_grid(
  plot_grid(E_vs_time_aug_15_BH_2019_diurnal_with_letters, E_vs_time_july_12_BH_2020_diurnal_with_letters,WUE_vs_time_aug_15_BH_2019_diurnal_with_letters, WUE_vs_time_july_12_BH_2020_diurnal_with_letters, WUE_int_vs_time_aug_15_BH_2019_diurnal_with_letters, WUE_int_vs_time_july_12_BH_2020_diurnal_with_letters,  labels = c("A", "B", "C", "D", "E", "F"),ncol = 2, 
            vjust = 1.5, 
            hjust = -9.6, 
            label_size = 18,
            align = "v", axis = "l") # Adjust the height of the last row separately
)


ggsave(panel_plot_E_and_WUE_bh_HW2_2019_HW3_2020_diurnal, filename = "figures/panel_plot_E_and_WUE_bh_HW2_2019_HW3_2020_diurnal.jpg", width = 16, height =19, dpi = 600)
