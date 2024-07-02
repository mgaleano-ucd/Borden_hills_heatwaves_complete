
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
  select(-hhmmss) %>%
  filter(!is.na(time))

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
####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL July 1 2019 #####

diurnals_2019_A_vs_time_jul1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "182") %>%
  mutate(interval = case_when(
    round == 1 ~ "7/1/2019  7:15",
    round == 2 ~ "7/1/2019  11:00",
    round == 3 ~ "7/1/2019  14:00", 
    round == 4 ~ "7/1/2019  17:15",
    round == 5 ~ "7/1/2019  19:10"
  ))

str(diurnals_2019_A_vs_time_jul1$interval)


diurnals_2019_A_vs_time_jul1 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_jul1$interval<-format(diurnals_2019_A_vs_time_jul1$interval)

diurnals_2019_A_vs_time_jul1$interval<-as.POSIXct(diurnals_2019_A_vs_time_jul1$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2019_A_vs_time_jul1$interval)


diurnals_2019_A_vs_time_jul1_anova<-diurnals_2019_A_vs_time_jul1


diurnals_2019_A_vs_time_jul1_anova$treatment <- as.character(diurnals_2019_A_vs_time_jul1_anova$treatment)
str(diurnals_2019_A_vs_time_jul1_anova$treatment)

diurnals_2019_A_vs_time_jul1_anova_tally<-diurnals_2019_A_vs_time_jul1_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_A_vs_time_jul1_anova$treatment <- as.factor(diurnals_2019_A_vs_time_jul1_anova$treatment)

str(diurnals_2019_A_vs_time_jul1_anova$treatment)
str(diurnals_2019_A_vs_time_jul1_anova$interval)

# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_A_vs_time_jul1_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_july_1_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_A_vs_time_jul1_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_july_1_bh_2019 <- rbind(interval_results,results_df_A_diurnal_july_1_bh_2019)
}

results_df_A_diurnal_july_1_bh_2019$interval <- as.POSIXct(results_df_A_diurnal_july_1_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_july_1_bh_2019$interval)


str(results_df_A_diurnal_july_1_bh_2019)
results_df_A_diurnal_july_1_bh_2019$Mean_sem <- paste(round(results_df_A_diurnal_july_1_bh_2019$mean, 2), "±", round(results_df_A_diurnal_july_1_bh_2019$standard_error, 2),results_df_A_diurnal_july_1_bh_2019$letters_ordered.groups)


str(results_df_A_diurnal_july_1_bh_2019)

write.csv(results_df_A_diurnal_july_1_bh_2019,"data_output/results_df_A_diurnal_july_1_bh_2019.csv")

pd<- position_dodge(1400)

A_vs_time_july_1_BH_2019_diurnal<-
  ggplot(results_df_A_diurnal_july_1_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("July 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave(A_vs_time_july_1_BH_2019_diurnal, filename = "figures/A_vs_time_july_1_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_july_1_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_july_1_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_july_1_BH_2019_diurnal_with_letters <- A_vs_time_july_1_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_july_1_BH_2019_diurnal_with_letters, filename = "figures/A_vs_time_july_1_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

diurnals_2019_A_vs_time_jul1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "206") 


####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL July 25 2019 #####

diurnals_2019_A_vs_time_Jul25<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "206") 

diurnals_2019_A_vs_time_Jul25$interval <-cut(diurnals_2019_A_vs_time_Jul25$datetime, breaks= "135 min", labels = c ("07-25-2019 6:00", "07-25-2019 8:30","07-25-2019 11:00", "07-25-2019 13:00", "07-25-2019 17:00", "07-25-2019 17:00"))

str(diurnals_2019_A_vs_time_Jul25$interval)


diurnals_2019_A_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_Jul25$treatment<- reorder(diurnals_2019_A_vs_time_Jul25$treatment, diurnals_2019_A_vs_time_Jul25$datetime) 

tz(diurnals_2019_A_vs_time_Jul25$datetime)
diurnals_2019_A_vs_time_Jul25$Rep<- format(diurnals_2019_A_vs_time_Jul25$Rep)
as.character(diurnals_2019_A_vs_time_Jul25$Rep)
str(diurnals_2019_A_vs_time_Jul25$Rep)


str(diurnals_2019_A_vs_time_Jul25$interval)


diurnals_2019_A_vs_time_Jul25$interval<-as.character(diurnals_2019_A_vs_time_Jul25$interval)

diurnals_2019_A_vs_time_Jul25$interval<-format(diurnals_2019_A_vs_time_Jul25$interval)

diurnals_2019_A_vs_time_Jul25$interval<-as.POSIXct(diurnals_2019_A_vs_time_Jul25$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_A_vs_time_Jul25$interval)


diurnals_2019_A_vs_time_Jul25_anova<-diurnals_2019_A_vs_time_Jul25


diurnals_2019_A_vs_time_Jul25_anova$treatment <- as.character(diurnals_2019_A_vs_time_Jul25_anova$treatment)
str(diurnals_2019_A_vs_time_Jul25_anova$treatment)

diurnals_2019_A_vs_time_Jul25_anova_tally<-diurnals_2019_A_vs_time_Jul25_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_A_vs_time_Jul25_anova$treatment <- as.factor(diurnals_2019_A_vs_time_Jul25_anova$treatment)

str(diurnals_2019_A_vs_time_Jul25_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_A_vs_time_Jul25_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_july_25_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_A_vs_time_Jul25_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_july_25_bh_2019 <- rbind(interval_results,results_df_A_diurnal_july_25_bh_2019)
}

results_df_A_diurnal_july_25_bh_2019$interval <- as.POSIXct(results_df_A_diurnal_july_25_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_july_25_bh_2019$interval)


str(results_df_A_diurnal_july_25_bh_2019)
results_df_A_diurnal_july_25_bh_2019$Mean_sem <- paste(round(results_df_A_diurnal_july_25_bh_2019$mean, 2), "±", round(results_df_A_diurnal_july_25_bh_2019$standard_error, 2),results_df_A_diurnal_july_25_bh_2019$letters_ordered.groups)


str(results_df_A_diurnal_july_25_bh_2019)

write.csv(results_df_A_diurnal_july_25_bh_2019,"data_output/results_df_A_diurnal_july_25_bh_2019.csv")

pd<- position_dodge(1400)

A_vs_time_july_25_BH_2019_diurnal<-
  ggplot(results_df_A_diurnal_july_25_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("July 25 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave(A_vs_time_july_25_BH_2019_diurnal, filename = "figures/A_vs_time_july_25_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_july_25_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_july_25_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_july_25_BH_2019_diurnal_with_letters <- A_vs_time_july_25_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_july_25_BH_2019_diurnal_with_letters, filename = "figures/A_vs_time_july_25_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

diurnals_2019_A_vs_time_Jul25<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "206") 

diurnals_2019_A_vs_time_Jul25$interval <-cut(diurnals_2019_A_vs_time_Jul25$datetime, breaks= "135 min", labels = c ("07-25-2019 6:00", "07-25-2019 8:30","07-25-2019 11:00", "07-25-2019 13:00", "07-25-2019 17:00", "07-25-2019 17:00"))

str(diurnals_2019_A_vs_time_Jul25$interval)


diurnals_2019_A_vs_time_Jul25$interval <- format((diurnals_2019_A_vs_time_Jul25$interval))

diurnals_2019_A_vs_time_Jul25$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_Jul25$interval))

diurnals_2019_A_vs_time_Jul25%>%
  group_by(treatment, interval)%>%
  tally()


str(diurnals_2019_A_vs_time_Jul25$interval)
diurnals_2019_A_vs_time_Jul25_avg_se <-diurnals_2019_A_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))

write.csv(diurnals_2019_A_vs_time_Jul25_avg_se,"data_output/diurnals_2019_A_vs_time_Jul25_avg_se_without_leaky_pixel.csv")

diurnals_2019_A_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_Jul25$Rep<- format(diurnals_2019_A_vs_time_Jul25$Rep)
as.character(diurnals_2019_A_vs_time_Jul25$Rep)
str(diurnals_2019_A_vs_time_Jul25$Rep)

diurnals_2019_A_vs_time_Jul25$treatment<- reorder(diurnals_2019_A_vs_time_Jul25$treatment, diurnals_2019_A_vs_time_Jul25$datetime) 

diurnals_2019_A_vs_time_Jul25_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_Jul25_avg_se$treatment, diurnals_2019_A_vs_time_Jul25_avg_se$interval)

str(diurnals_2019_A_vs_time_Jul25_avg_se$interval)
tz(diurnals_2019_A_vs_time_Jul25_avg_se$interval)
tz(diurnals_2019_A_vs_time_Jul25$datetime)

# Calculate max A for each treatment across all rounds
max_A_all_rounds <- diurnals_2019_A_vs_time_Jul25_avg_se %>%
  group_by(treatment) %>%
  summarise(max_A_all = max(avg_A, na.rm = TRUE))

# Calculate max A for each treatment in round 5
max_A_round_5 <- diurnals_2019_A_vs_time_Jul25_avg_se %>%
  filter(round == 5) %>%
  group_by(treatment) %>%
  summarise(max_A_5 = max(avg_A, na.rm = TRUE))

# Combine the max_A_all_rounds and max_A_round_5 dataframes
max_A_combined <- max_A_all_rounds %>%
  left_join(max_A_round_5, by = "treatment")

# Calculate the percentage decrease from the maximum A to round 5
max_A_combined <- max_A_combined %>%
  mutate(percentage_decrease = ((max_A_all - max_A_5) / max_A_all) * 100)

# View the results
print(max_A_combined)

####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL JULY 28 2019 #####

diurnals_2019_A_vs_time_Jul28<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "209") 

diurnals_2019_A_vs_time_Jul28$interval <-cut(diurnals_2019_A_vs_time_Jul28$datetime, breaks= "150 min", labels = c ("07-28-2019 6:00", "07-28-2019 9:00","07-28-2019 11:30", "07-28-2019 14:00", "07-28-2019 17:00", "07-28-2019 17:00"))

str(diurnals_2019_A_vs_time_Jul28$interval)


diurnals_2019_A_vs_time_Jul28 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_Jul28$treatment<- reorder(diurnals_2019_A_vs_time_Jul28$treatment, diurnals_2019_A_vs_time_Jul28$datetime) 

tz(diurnals_2019_A_vs_time_Jul28$datetime)
diurnals_2019_A_vs_time_Jul28$Rep<- format(diurnals_2019_A_vs_time_Jul28$Rep)
as.character(diurnals_2019_A_vs_time_Jul28$Rep)
str(diurnals_2019_A_vs_time_Jul28$Rep)


str(diurnals_2019_A_vs_time_Jul28$interval)


diurnals_2019_A_vs_time_Jul28$interval<-as.character(diurnals_2019_A_vs_time_Jul28$interval)

diurnals_2019_A_vs_time_Jul28$interval<-format(diurnals_2019_A_vs_time_Jul28$interval)

diurnals_2019_A_vs_time_Jul28$interval<-as.POSIXct(diurnals_2019_A_vs_time_Jul28$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_A_vs_time_Jul28$interval)


diurnals_2019_A_vs_time_Jul28_anova<-diurnals_2019_A_vs_time_Jul28


diurnals_2019_A_vs_time_Jul28_anova$treatment <- as.character(diurnals_2019_A_vs_time_Jul28_anova$treatment)
str(diurnals_2019_A_vs_time_Jul28_anova$treatment)

diurnals_2019_A_vs_time_Jul28_anova_tally<-diurnals_2019_A_vs_time_Jul28_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_A_vs_time_Jul28_anova$treatment <- as.factor(diurnals_2019_A_vs_time_Jul28_anova$treatment)

str(diurnals_2019_A_vs_time_Jul28_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_A_vs_time_Jul28_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_july_28_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_A_vs_time_Jul28_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_july_28_bh_2019 <- rbind(interval_results,results_df_A_diurnal_july_28_bh_2019)
}

results_df_A_diurnal_july_28_bh_2019$interval <- as.POSIXct(results_df_A_diurnal_july_28_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_july_28_bh_2019$interval)


str(results_df_A_diurnal_july_28_bh_2019)
results_df_A_diurnal_july_28_bh_2019$Mean_sem <- paste(round(results_df_A_diurnal_july_28_bh_2019$mean, 2), "±", round(results_df_A_diurnal_july_28_bh_2019$standard_error, 2),results_df_A_diurnal_july_28_bh_2019$letters_ordered.groups)


str(results_df_A_diurnal_july_28_bh_2019)

write.csv(results_df_A_diurnal_july_28_bh_2019,"data_output/results_df_A_diurnal_july_28_bh_2019.csv")

pd<- position_dodge(1400)

A_vs_time_july_28_BH_2019_diurnal<-
  ggplot(results_df_A_diurnal_july_28_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("July 28 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave(A_vs_time_july_28_BH_2019_diurnal, filename = "figures/A_vs_time_july_28_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_july_28_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_july_28_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_july_28_BH_2019_diurnal_with_letters <- A_vs_time_july_28_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_july_28_BH_2019_diurnal_with_letters, filename = "figures/A_vs_time_july_28_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL AUG 1 2019 #####

diurnals_2019_A_vs_time_aug1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "213") 

diurnals_2019_A_vs_time_aug1$interval <-cut(diurnals_2019_A_vs_time_aug1$datetime, breaks= "143 min", labels = c ("08-01-2019 6:00", "08-01-2019 8:30","08-01-2019 10:40", "08-01-2019 13:30", "08-01-2019 17:00", "08-01-2019 17:00"))

str(diurnals_2019_A_vs_time_aug1$interval)


diurnals_2019_A_vs_time_aug1 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_aug1$treatment<- reorder(diurnals_2019_A_vs_time_aug1$treatment, diurnals_2019_A_vs_time_aug1$datetime) 

tz(diurnals_2019_A_vs_time_aug1$datetime)
diurnals_2019_A_vs_time_aug1$Rep<- format(diurnals_2019_A_vs_time_aug1$Rep)
as.character(diurnals_2019_A_vs_time_aug1$Rep)
str(diurnals_2019_A_vs_time_aug1$Rep)


str(diurnals_2019_A_vs_time_aug1$interval)


diurnals_2019_A_vs_time_aug1$interval<-as.character(diurnals_2019_A_vs_time_aug1$interval)

diurnals_2019_A_vs_time_aug1$interval<-format(diurnals_2019_A_vs_time_aug1$interval)

diurnals_2019_A_vs_time_aug1$interval<-as.POSIXct(diurnals_2019_A_vs_time_aug1$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_A_vs_time_aug1$interval)


diurnals_2019_A_vs_time_aug1_anova<-diurnals_2019_A_vs_time_aug1


diurnals_2019_A_vs_time_aug1_anova$treatment <- as.character(diurnals_2019_A_vs_time_aug1_anova$treatment)
str(diurnals_2019_A_vs_time_aug1_anova$treatment)

diurnals_2019_A_vs_time_aug1_anova_tally<-diurnals_2019_A_vs_time_aug1_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_A_vs_time_aug1_anova$treatment <- as.factor(diurnals_2019_A_vs_time_aug1_anova$treatment)

str(diurnals_2019_A_vs_time_aug1_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_A_vs_time_aug1_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_aug_1_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_A_vs_time_aug1_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_aug_1_bh_2019 <- rbind(interval_results,results_df_A_diurnal_aug_1_bh_2019)
}

results_df_A_diurnal_aug_1_bh_2019$interval <- as.POSIXct(results_df_A_diurnal_aug_1_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_aug_1_bh_2019$interval)


str(results_df_A_diurnal_aug_1_bh_2019)
results_df_A_diurnal_aug_1_bh_2019$Mean_sem <- paste(round(results_df_A_diurnal_aug_1_bh_2019$mean, 2), "±", round(results_df_A_diurnal_aug_1_bh_2019$standard_error, 2),results_df_A_diurnal_aug_1_bh_2019$letters_ordered.groups)


str(results_df_A_diurnal_aug_1_bh_2019)

write.csv(results_df_A_diurnal_aug_1_bh_2019,"data_output/results_df_A_diurnal_aug_1_bh_2019.csv")

pd<- position_dodge(1400)

A_vs_time_aug_01_BH_2019_diurnal<-
  ggplot(results_df_A_diurnal_aug_1_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("August 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave(A_vs_time_aug_01_BH_2019_diurnal, filename = "figures/A_vs_time_aug_01_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_aug_1_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_aug_1_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.1 # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.6))  # Ensure spacing starts from max_mean

# Create the plot with letters of significance above the max mean points
A_vs_time_aug_01_BH_2019_diurnal_with_letters <- A_vs_time_aug_01_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_aug_01_BH_2019_diurnal_with_letters, filename = "figures/A_vs_time_aug_01_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

diurnals_2019_A_vs_time_aug1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "213") 

diurnals_2019_A_vs_time_aug1$interval <-cut(diurnals_2019_A_vs_time_aug1$datetime, breaks= "143 min", labels = c ("08-01-2019 6:00", "08-01-2019 8:30","08-01-2019 10:40", "08-01-2019 13:30", "08-01-2019 17:00", "08-01-2019 17:00"))

diurnals_2019_A_vs_time_aug1$interval <- format((diurnals_2019_A_vs_time_aug1$interval))

diurnals_2019_A_vs_time_aug1$interval<- mdy_hm(as.character(diurnals_2019_A_vs_time_aug1$interval))


str(diurnals_2019_A_vs_time_aug1$interval)
diurnals_2019_A_vs_time_aug1_avg_se <-diurnals_2019_A_vs_time_aug1 %>%
  group_by(interval, treatment, round) %>%
  summarise(avg_A = mean(A), sev = se(A))


write.csv(diurnals_2019_A_vs_time_aug1_avg_se,"data_output/diurnals_2019_A_vs_time_aug1_avg_se_without_leaky_pixel.csv")

diurnals_2019_A_vs_time_aug1 %>%
  group_by(treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_aug1$Rep<- format(diurnals_2019_A_vs_time_aug1$Rep)
as.character(diurnals_2019_A_vs_time_aug1$Rep)
str(diurnals_2019_A_vs_time_aug1$Rep)


diurnals_2019_A_vs_time_aug1$treatment<- reorder(diurnals_2019_A_vs_time_aug1$treatment, diurnals_2019_A_vs_time_aug1$datetime) 

diurnals_2019_A_vs_time_aug1_avg_se$treatment<- reorder(diurnals_2019_A_vs_time_aug1_avg_se$treatment, diurnals_2019_A_vs_time_aug1_avg_se$interval)

str(diurnals_2019_A_vs_time_aug1_avg_se$interval)
tz(diurnals_2019_A_vs_time_aug1_avg_se$interval)
tz(diurnals_2019_A_vs_time_aug1$datetime)

# Calculate max A for each treatment on July 28
max_A_Jul28 <- diurnals_2019_A_vs_time_Jul28_avg_se %>%
  group_by(treatment) %>%
  summarise(max_A_Jul28 = max(avg_A, na.rm = TRUE))

# Calculate max A for each treatment on August 1
max_A_Aug1 <- diurnals_2019_A_vs_time_aug1_avg_se %>%
  group_by(treatment) %>%
  summarise(max_A_Aug1 = max(avg_A, na.rm = TRUE))

# Combine the max_A_Jul28 and max_A_Aug1 dataframes
max_A_combined <- max_A_Jul28 %>%
  left_join(max_A_Aug1, by = "treatment")

# Calculate the percentage increase from July 28 to August 1
max_A_combined <- max_A_combined %>%
  mutate(percentage_increase = ((max_A_Aug1 - max_A_Jul28) / max_A_Jul28) * 100)

# View the results
print(max_A_combined)

####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL AUG 15 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_A_vs_time_aug15<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A))%>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "227") 

diurnals_2019_A_vs_time_aug15$interval <-cut(diurnals_2019_A_vs_time_aug15$datetime, breaks= "139 min", labels = c ("08-15-2019 6:00", "08-15-2019 9:00","08-15-2019 11:15", "08-15-2019 13:30", "08-15-2019 17:00", "08-15-2019 17:00"))

str(diurnals_2019_A_vs_time_aug15$interval)


diurnals_2019_A_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_aug15$treatment<- reorder(diurnals_2019_A_vs_time_aug15$treatment, diurnals_2019_A_vs_time_aug15$datetime) 

tz(diurnals_2019_A_vs_time_aug15$datetime)
diurnals_2019_A_vs_time_aug15$Rep<- format(diurnals_2019_A_vs_time_aug15$Rep)
as.character(diurnals_2019_A_vs_time_aug15$Rep)
str(diurnals_2019_A_vs_time_aug15$Rep)


str(diurnals_2019_A_vs_time_aug15$interval)


diurnals_2019_A_vs_time_aug15$interval<-as.character(diurnals_2019_A_vs_time_aug15$interval)

diurnals_2019_A_vs_time_aug15$interval<-format(diurnals_2019_A_vs_time_aug15$interval)

diurnals_2019_A_vs_time_aug15$interval<-as.POSIXct(diurnals_2019_A_vs_time_aug15$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_A_vs_time_aug15$interval)


diurnals_2019_A_vs_time_aug15_anova<-diurnals_2019_A_vs_time_aug15


diurnals_2019_A_vs_time_aug15_anova$treatment <- as.character(diurnals_2019_A_vs_time_aug15_anova$treatment)
str(diurnals_2019_A_vs_time_aug15_anova$treatment)

diurnals_2019_A_vs_time_aug15_anova_tally<-diurnals_2019_A_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_A_vs_time_aug15_anova$treatment <- as.factor(diurnals_2019_A_vs_time_aug15_anova$treatment)

str(diurnals_2019_A_vs_time_aug15_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_A_vs_time_aug15_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_aug_15_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_A_vs_time_aug15_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_aug_15_bh_2019 <- rbind(interval_results,results_df_A_diurnal_aug_15_bh_2019)
}

results_df_A_diurnal_aug_15_bh_2019$interval <- as.POSIXct(results_df_A_diurnal_aug_15_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_aug_15_bh_2019$interval)


str(results_df_A_diurnal_aug_15_bh_2019)
results_df_A_diurnal_aug_15_bh_2019$Mean_sem <- paste(round(results_df_A_diurnal_aug_15_bh_2019$mean, 2), "±", round(results_df_A_diurnal_aug_15_bh_2019$standard_error, 2),results_df_A_diurnal_aug_15_bh_2019$letters_ordered.groups)


str(results_df_A_diurnal_aug_15_bh_2019)

write.csv(results_df_A_diurnal_aug_15_bh_2019,"data_output/results_df_A_diurnal_aug_15_bh_2019.csv")

pd<- position_dodge(1400)

A_vs_time_aug_15_BH_2019_diurnal<-
  ggplot(results_df_A_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("August 15 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave(A_vs_time_aug_15_BH_2019_diurnal, filename = "figures/A_vs_time_aug_15_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_aug_15_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_aug_15_BH_2019_diurnal_with_letters <- A_vs_time_aug_15_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_aug_15_BH_2019_diurnal_with_letters, filename = "figures/A_vs_time_aug_15_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)




####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL AUG 20 2019 #####

diurnals_2019_A_vs_time_aug20<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "232") 

diurnals_2019_A_vs_time_aug20$interval <-cut(diurnals_2019_A_vs_time_aug20$datetime, breaks= "147 min", labels = c ("08-20-2019 6:00", "08-20-2019 9:00","08-20-2019 11:15", "08-20-2019 13:30", "08-20-2019 17:00", "08-20-2019 17:00"))


str(diurnals_2019_A_vs_time_aug20$interval)


diurnals_2019_A_vs_time_aug20 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_aug20$treatment<- reorder(diurnals_2019_A_vs_time_aug20$treatment, diurnals_2019_A_vs_time_aug20$datetime) 

tz(diurnals_2019_A_vs_time_aug20$datetime)
diurnals_2019_A_vs_time_aug20$Rep<- format(diurnals_2019_A_vs_time_aug20$Rep)
as.character(diurnals_2019_A_vs_time_aug20$Rep)
str(diurnals_2019_A_vs_time_aug20$Rep)


str(diurnals_2019_A_vs_time_aug20$interval)


diurnals_2019_A_vs_time_aug20$interval<-as.character(diurnals_2019_A_vs_time_aug20$interval)

diurnals_2019_A_vs_time_aug20$interval<-format(diurnals_2019_A_vs_time_aug20$interval)

diurnals_2019_A_vs_time_aug20$interval<-as.POSIXct(diurnals_2019_A_vs_time_aug20$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_A_vs_time_aug20$interval)


diurnals_2019_A_vs_time_aug20_anova<-diurnals_2019_A_vs_time_aug20


diurnals_2019_A_vs_time_aug20_anova$treatment <- as.character(diurnals_2019_A_vs_time_aug20_anova$treatment)
str(diurnals_2019_A_vs_time_aug20_anova$treatment)

diurnals_2019_A_vs_time_aug20_anova_tally<-diurnals_2019_A_vs_time_aug20_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_A_vs_time_aug20_anova$treatment <- as.factor(diurnals_2019_A_vs_time_aug20_anova$treatment)

str(diurnals_2019_A_vs_time_aug20_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_A_vs_time_aug20_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_aug_20_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_A_vs_time_aug20_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_aug_20_bh_2019 <- rbind(interval_results,results_df_A_diurnal_aug_20_bh_2019)
}

results_df_A_diurnal_aug_20_bh_2019$interval <- as.POSIXct(results_df_A_diurnal_aug_20_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_aug_20_bh_2019$interval)


str(results_df_A_diurnal_aug_20_bh_2019)
results_df_A_diurnal_aug_20_bh_2019$Mean_sem <- paste(round(results_df_A_diurnal_aug_20_bh_2019$mean, 2), "±", round(results_df_A_diurnal_aug_20_bh_2019$standard_error, 2),results_df_A_diurnal_aug_20_bh_2019$letters_ordered.groups)


str(results_df_A_diurnal_aug_20_bh_2019)

write.csv(results_df_A_diurnal_aug_20_bh_2019,"data_output/results_df_A_diurnal_aug_20_bh_2019.csv")

pd<- position_dodge(1400)

A_vs_time_aug_20_BH_2019_diurnal<-
  ggplot(results_df_A_diurnal_aug_20_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("August 20 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave(A_vs_time_aug_20_BH_2019_diurnal, filename = "figures/A_vs_time_aug_20_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_aug_20_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_aug_20_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_aug_20_BH_2019_diurnal_with_letters <- A_vs_time_aug_20_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_aug_20_BH_2019_diurnal_with_letters, filename = "figures/A_vs_time_aug_20_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL SEP 5 2019 #####

diurnals_2019_A_vs_time_sep5<- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment, Rep) %>%
  filter(day == "248") 

diurnals_2019_A_vs_time_sep5$interval <-cut(diurnals_2019_A_vs_time_sep5$datetime, breaks= "145 min", labels = c ("09-05-2019 6:00", "09-05-2019 9:00","09-05-2019 11:15", "09-05-2019 13:30", "09-05-2019 17:00"))

str(diurnals_2019_A_vs_time_sep5$interval)


diurnals_2019_A_vs_time_sep5 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_A_vs_time_sep5$treatment<- reorder(diurnals_2019_A_vs_time_sep5$treatment, diurnals_2019_A_vs_time_sep5$datetime) 

tz(diurnals_2019_A_vs_time_sep5$datetime)
diurnals_2019_A_vs_time_sep5$Rep<- format(diurnals_2019_A_vs_time_sep5$Rep)
as.character(diurnals_2019_A_vs_time_sep5$Rep)
str(diurnals_2019_A_vs_time_sep5$Rep)


str(diurnals_2019_A_vs_time_sep5$interval)


diurnals_2019_A_vs_time_sep5$interval<-as.character(diurnals_2019_A_vs_time_sep5$interval)

diurnals_2019_A_vs_time_sep5$interval<-format(diurnals_2019_A_vs_time_sep5$interval)

diurnals_2019_A_vs_time_sep5$interval<-as.POSIXct(diurnals_2019_A_vs_time_sep5$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_A_vs_time_sep5$interval)


diurnals_2019_A_vs_time_sep5_anova<-diurnals_2019_A_vs_time_sep5


diurnals_2019_A_vs_time_sep5_anova$treatment <- as.character(diurnals_2019_A_vs_time_sep5_anova$treatment)
str(diurnals_2019_A_vs_time_sep5_anova$treatment)

diurnals_2019_A_vs_time_sep5_anova_tally<-diurnals_2019_A_vs_time_sep5_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_A_vs_time_sep5_anova$treatment <- as.factor(diurnals_2019_A_vs_time_sep5_anova$treatment)

str(diurnals_2019_A_vs_time_sep5_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_A_vs_time_sep5_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_sep_05_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_A_vs_time_sep5_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_sep_05_bh_2019 <- rbind(interval_results,results_df_A_diurnal_sep_05_bh_2019)
}

results_df_A_diurnal_sep_05_bh_2019$interval <- as.POSIXct(results_df_A_diurnal_sep_05_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_sep_05_bh_2019$interval)


str(results_df_A_diurnal_sep_05_bh_2019)
results_df_A_diurnal_sep_05_bh_2019$Mean_sem <- paste(round(results_df_A_diurnal_sep_05_bh_2019$mean, 2), "±", round(results_df_A_diurnal_sep_05_bh_2019$standard_error, 2),results_df_A_diurnal_sep_05_bh_2019$letters_ordered.groups)


str(results_df_A_diurnal_sep_05_bh_2019)

write.csv(results_df_A_diurnal_sep_05_bh_2019,"data_output/results_df_A_diurnal_sep_05_bh_2019.csv")

pd<- position_dodge(1400)

A_vs_time_sep_05_BH_2019_diurnal<-
  ggplot(results_df_A_diurnal_sep_05_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("September 5 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave(A_vs_time_sep_05_BH_2019_diurnal, filename = "figures/A_vs_time_sep_05_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_sep_05_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_sep_05_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1))  # Ensure spacing starts from max_mean

# Create the plot with letters of significance above the max mean points
A_vs_time_sep_05_BH_2019_diurnal_with_letters <- A_vs_time_sep_05_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_sep_05_BH_2019_diurnal_with_letters, filename = "figures/A_vs_time_sep_05_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)
####ANOVA AND TUKEYS HSD GSW DIURNAL JULY 1 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_gsw_vs_time_jul1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(gsw)) %>%
  select(datetime, day, gsw, Fv.Fm, E, pixel_number, round, treatment, Rep) %>%
  filter(day == "182") %>%
  filter(gsw < 1) %>%
  mutate(interval = case_when(
    round == 1 ~ "7/1/2019  7:15",
    round == 2 ~ "7/1/2019  11:00",
    round == 3 ~ "7/1/2019  14:00", 
    round == 4 ~ "7/1/2019  17:15",
    round == 5 ~ "7/1/2019  19:10"
  ))


str(diurnals_2019_gsw_vs_time_jul1$interval)


diurnals_2019_gsw_vs_time_jul1 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_gsw_vs_time_jul1$treatment<- reorder(diurnals_2019_gsw_vs_time_jul1$treatment, diurnals_2019_gsw_vs_time_jul1$datetime) 

tz(diurnals_2019_gsw_vs_time_jul1$datetime)
diurnals_2019_gsw_vs_time_jul1$Rep<- format(diurnals_2019_gsw_vs_time_jul1$Rep)
as.character(diurnals_2019_gsw_vs_time_jul1$Rep)
str(diurnals_2019_gsw_vs_time_jul1$Rep)


str(diurnals_2019_gsw_vs_time_jul1$interval)


diurnals_2019_gsw_vs_time_jul1$interval<-as.character(diurnals_2019_gsw_vs_time_jul1$interval)

diurnals_2019_gsw_vs_time_jul1$interval<-format(diurnals_2019_gsw_vs_time_jul1$interval)

diurnals_2019_gsw_vs_time_jul1$interval<-as.POSIXct(diurnals_2019_gsw_vs_time_jul1$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2019_gsw_vs_time_jul1$interval)


diurnals_2019_gsw_vs_time_jul1_anova<-diurnals_2019_gsw_vs_time_jul1


diurnals_2019_gsw_vs_time_jul1_anova$treatment <- as.character(diurnals_2019_gsw_vs_time_jul1_anova$treatment)
str(diurnals_2019_gsw_vs_time_jul1_anova$treatment)

diurnals_2019_gsw_vs_time_jul1_anova_tally<-diurnals_2019_gsw_vs_time_jul1_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_gsw_vs_time_jul1_anova$treatment <- as.factor(diurnals_2019_gsw_vs_time_jul1_anova$treatment)

str(diurnals_2019_gsw_vs_time_jul1_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_gsw_vs_time_jul1_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_july_1_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_gsw_vs_time_jul1_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_july_1_bh_2019 <- rbind(interval_results,results_df_gsw_diurnal_july_1_bh_2019)
}

results_df_gsw_diurnal_july_1_bh_2019$interval <- as.POSIXct(results_df_gsw_diurnal_july_1_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_july_1_bh_2019$interval)


str(results_df_gsw_diurnal_july_1_bh_2019)
results_df_gsw_diurnal_july_1_bh_2019$Mean_sem <- paste(round(results_df_gsw_diurnal_july_1_bh_2019$mean, 2), "±", round(results_df_gsw_diurnal_july_1_bh_2019$standard_error, 2),results_df_gsw_diurnal_july_1_bh_2019$letters_ordered.groups)


str(results_df_gsw_diurnal_july_1_bh_2019)

write.csv(results_df_gsw_diurnal_july_1_bh_2019,"data_output/results_df_gsw_diurnal_july_1_bh_2019.csv")

pd<- position_dodge(1400)

gsw_vs_time_july_1_BH_2019_diurnal<-
  ggplot(results_df_gsw_diurnal_july_1_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("July 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif", color = "white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(legend.position = "none") 

ggsave(gsw_vs_time_july_1_BH_2019_diurnal, filename = "figures/gsw_vs_time_july_1_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_july_1_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_july_1_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_july_1_BH_2019_diurnal_with_letters <- gsw_vs_time_july_1_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_july_1_BH_2019_diurnal_with_letters, filename = "figures/gsw_vs_time_july_1_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

####ANOVA AND TUKEYS HSD GSW DIURNAL JULY 25 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_gsw_vs_time_Jul25<- diurnals_2019_F_vs_round %>%
  filter(!is.na(gsw)) %>%
  select(datetime, day, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "206") 

diurnals_2019_gsw_vs_time_Jul25$interval <-cut(diurnals_2019_gsw_vs_time_Jul25$datetime, breaks= "135 min", labels = c ("07-25-2019 6:00", "07-25-2019 8:30","07-25-2019 11:00", "07-25-2019 13:00", "07-25-2019 17:00", "07-25-2019 17:00"))

str(diurnals_2019_gsw_vs_time_Jul25$interval)


diurnals_2019_gsw_vs_time_Jul25 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_gsw_vs_time_Jul25$treatment<- reorder(diurnals_2019_gsw_vs_time_Jul25$treatment, diurnals_2019_gsw_vs_time_Jul25$datetime) 

tz(diurnals_2019_gsw_vs_time_Jul25$datetime)
diurnals_2019_gsw_vs_time_Jul25$Rep<- format(diurnals_2019_gsw_vs_time_Jul25$Rep)
as.character(diurnals_2019_gsw_vs_time_Jul25$Rep)
str(diurnals_2019_gsw_vs_time_Jul25$Rep)


str(diurnals_2019_gsw_vs_time_Jul25$interval)


diurnals_2019_gsw_vs_time_Jul25$interval<-as.character(diurnals_2019_gsw_vs_time_Jul25$interval)

diurnals_2019_gsw_vs_time_Jul25$interval<-format(diurnals_2019_gsw_vs_time_Jul25$interval)

diurnals_2019_gsw_vs_time_Jul25$interval<-as.POSIXct(diurnals_2019_gsw_vs_time_Jul25$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_gsw_vs_time_Jul25$interval)


diurnals_2019_gsw_vs_time_Jul25_anova<-diurnals_2019_gsw_vs_time_Jul25


diurnals_2019_gsw_vs_time_Jul25_anova$treatment <- as.character(diurnals_2019_gsw_vs_time_Jul25_anova$treatment)
str(diurnals_2019_gsw_vs_time_Jul25_anova$treatment)

diurnals_2019_gsw_vs_time_Jul25_anova_tally<-diurnals_2019_gsw_vs_time_Jul25_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_gsw_vs_time_Jul25_anova$treatment <- as.factor(diurnals_2019_gsw_vs_time_Jul25_anova$treatment)

str(diurnals_2019_gsw_vs_time_Jul25_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_gsw_vs_time_Jul25_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_july_25_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_gsw_vs_time_Jul25_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_july_25_bh_2019 <- rbind(interval_results,results_df_gsw_diurnal_july_25_bh_2019)
}

results_df_gsw_diurnal_july_25_bh_2019$interval <- as.POSIXct(results_df_gsw_diurnal_july_25_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_july_25_bh_2019$interval)


str(results_df_gsw_diurnal_july_25_bh_2019)
results_df_gsw_diurnal_july_25_bh_2019$Mean_sem <- paste(round(results_df_gsw_diurnal_july_25_bh_2019$mean, 2), "±", round(results_df_gsw_diurnal_july_25_bh_2019$standard_error, 2),results_df_gsw_diurnal_july_25_bh_2019$letters_ordered.groups)


str(results_df_gsw_diurnal_july_25_bh_2019)

write.csv(results_df_gsw_diurnal_july_25_bh_2019,"data_output/results_df_gsw_diurnal_july_25_bh_2019.csv")

pd<- position_dodge(1400)

gsw_vs_time_july_25_BH_2019_diurnal<-
  ggplot(results_df_gsw_diurnal_july_25_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("July 25 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif", color = "white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(legend.position = "none")  +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 

ggsave(gsw_vs_time_july_25_BH_2019_diurnal, filename = "figures/gsw_vs_time_july_25_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_july_25_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_july_25_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_july_25_BH_2019_diurnal_with_letters <- gsw_vs_time_july_25_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_july_25_BH_2019_diurnal_with_letters, filename = "figures/gsw_vs_time_july_25_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

####ANOVA AND TUKEYS HSD GSW DIURNAL JULY 28 2019 #####

# Sample data frame structure (replace this with your actual data frame)


diurnals_2019_gsw_vs_time_Jul28<- diurnals_2019_F_vs_round %>%
  filter(!is.na(gsw)) %>%
  select(datetime, day, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "209") 

diurnals_2019_gsw_vs_time_Jul28$interval <-cut(diurnals_2019_gsw_vs_time_Jul28$datetime, breaks= "150 min", labels = c ("07-28-2019 6:00", "07-28-2019 9:00","07-28-2019 11:30", "07-28-2019 14:00", "07-28-2019 17:00", "07-28-2019 17:00"))


str(diurnals_2019_gsw_vs_time_Jul28$interval)


diurnals_2019_gsw_vs_time_Jul28 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_gsw_vs_time_Jul28$treatment<- reorder(diurnals_2019_gsw_vs_time_Jul28$treatment, diurnals_2019_gsw_vs_time_Jul28$datetime) 

tz(diurnals_2019_gsw_vs_time_Jul28$datetime)
diurnals_2019_gsw_vs_time_Jul28$Rep<- format(diurnals_2019_gsw_vs_time_Jul28$Rep)
as.character(diurnals_2019_gsw_vs_time_Jul28$Rep)
str(diurnals_2019_gsw_vs_time_Jul28$Rep)


str(diurnals_2019_gsw_vs_time_Jul28$interval)


diurnals_2019_gsw_vs_time_Jul28$interval<-as.character(diurnals_2019_gsw_vs_time_Jul28$interval)

diurnals_2019_gsw_vs_time_Jul28$interval<-format(diurnals_2019_gsw_vs_time_Jul28$interval)

diurnals_2019_gsw_vs_time_Jul28$interval<-as.POSIXct(diurnals_2019_gsw_vs_time_Jul28$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_gsw_vs_time_Jul28$interval)


diurnals_2019_gsw_vs_time_Jul28_anova<-diurnals_2019_gsw_vs_time_Jul28


diurnals_2019_gsw_vs_time_Jul28_anova$treatment <- as.character(diurnals_2019_gsw_vs_time_Jul28_anova$treatment)
str(diurnals_2019_gsw_vs_time_Jul28_anova$treatment)

diurnals_2019_gsw_vs_time_Jul28_anova_tally<-diurnals_2019_gsw_vs_time_Jul28_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_gsw_vs_time_Jul28_anova$treatment <- as.factor(diurnals_2019_gsw_vs_time_Jul28_anova$treatment)

str(diurnals_2019_gsw_vs_time_Jul28_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_gsw_vs_time_Jul28_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_july_28_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_gsw_vs_time_Jul28_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_july_28_bh_2019 <- rbind(interval_results,results_df_gsw_diurnal_july_28_bh_2019)
}

results_df_gsw_diurnal_july_28_bh_2019$interval <- as.POSIXct(results_df_gsw_diurnal_july_28_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_july_28_bh_2019$interval)


str(results_df_gsw_diurnal_july_28_bh_2019)
results_df_gsw_diurnal_july_28_bh_2019$Mean_sem <- paste(round(results_df_gsw_diurnal_july_28_bh_2019$mean, 2), "±", round(results_df_gsw_diurnal_july_28_bh_2019$standard_error, 2),results_df_gsw_diurnal_july_28_bh_2019$letters_ordered.groups)


str(results_df_gsw_diurnal_july_28_bh_2019)

write.csv(results_df_gsw_diurnal_july_28_bh_2019,"data_output/results_df_gsw_diurnal_july_28_bh_2019.csv")

pd<- position_dodge(1400)

gsw_vs_time_july_28_BH_2019_diurnal<-
  ggplot(results_df_gsw_diurnal_july_28_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("July 28 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif", color = "white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(legend.position = "none")  +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 


ggsave(gsw_vs_time_july_28_BH_2019_diurnal, filename = "figures/gsw_vs_time_july_28_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_july_28_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_july_28_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_july_28_BH_2019_diurnal_with_letters <- gsw_vs_time_july_28_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_july_28_BH_2019_diurnal_with_letters, filename = "figures/gsw_vs_time_july_28_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)
####ANOVA AND TUKEYS HSD GSW DIURNAL AUG 1 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_gsw_vs_time_aug1<- diurnals_2019_F_vs_round %>%
  filter(!is.na(gsw)) %>%
  select(datetime, day, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "213") %>%
  filter(gsw < 2)

diurnals_2019_gsw_vs_time_aug1$interval <-cut(diurnals_2019_gsw_vs_time_aug1$datetime, breaks= "142 min", labels = c ("08-01-2019 6:00", "08-01-2019 8:30","08-01-2019 10:40", "08-01-2019 13:30", "08-01-2019 17:00", "08-01-2019 17:00"))



str(diurnals_2019_gsw_vs_time_aug1$interval)


diurnals_2019_gsw_vs_time_aug1 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_gsw_vs_time_aug1$treatment<- reorder(diurnals_2019_gsw_vs_time_aug1$treatment, diurnals_2019_gsw_vs_time_aug1$datetime) 

tz(diurnals_2019_gsw_vs_time_aug1$datetime)
diurnals_2019_gsw_vs_time_aug1$Rep<- format(diurnals_2019_gsw_vs_time_aug1$Rep)
as.character(diurnals_2019_gsw_vs_time_aug1$Rep)
str(diurnals_2019_gsw_vs_time_aug1$Rep)


str(diurnals_2019_gsw_vs_time_aug1$interval)


diurnals_2019_gsw_vs_time_aug1$interval<-as.character(diurnals_2019_gsw_vs_time_aug1$interval)

diurnals_2019_gsw_vs_time_aug1$interval<-format(diurnals_2019_gsw_vs_time_aug1$interval)

diurnals_2019_gsw_vs_time_aug1$interval<-as.POSIXct(diurnals_2019_gsw_vs_time_aug1$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_gsw_vs_time_aug1$interval)


diurnals_2019_gsw_vs_time_aug1_anova<-diurnals_2019_gsw_vs_time_aug1


diurnals_2019_gsw_vs_time_aug1_anova$treatment <- as.character(diurnals_2019_gsw_vs_time_aug1_anova$treatment)
str(diurnals_2019_gsw_vs_time_aug1_anova$treatment)

diurnals_2019_gsw_vs_time_aug1_anova_tally<-diurnals_2019_gsw_vs_time_aug1_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_gsw_vs_time_aug1_anova$treatment <- as.factor(diurnals_2019_gsw_vs_time_aug1_anova$treatment)

str(diurnals_2019_gsw_vs_time_aug1_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_gsw_vs_time_aug1_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_aug_1_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_gsw_vs_time_aug1_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_aug_1_bh_2019 <- rbind(interval_results,results_df_gsw_diurnal_aug_1_bh_2019)
}

results_df_gsw_diurnal_aug_1_bh_2019$interval <- as.POSIXct(results_df_gsw_diurnal_aug_1_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_aug_1_bh_2019$interval)


str(results_df_gsw_diurnal_aug_1_bh_2019)
results_df_gsw_diurnal_aug_1_bh_2019$Mean_sem <- paste(round(results_df_gsw_diurnal_aug_1_bh_2019$mean, 2), "±", round(results_df_gsw_diurnal_aug_1_bh_2019$standard_error, 2),results_df_gsw_diurnal_aug_1_bh_2019$letters_ordered.groups)


str(results_df_gsw_diurnal_aug_1_bh_2019)

write.csv(results_df_gsw_diurnal_aug_1_bh_2019,"data_output/results_df_gsw_diurnal_aug_1_bh_2019.csv")

pd<- position_dodge(1400)

gsw_vs_time_aug_1_BH_2019_diurnal<-
  ggplot(results_df_gsw_diurnal_aug_1_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("August 1 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif", color = "white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(legend.position = "none")  +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 


ggsave(gsw_vs_time_aug_1_BH_2019_diurnal, filename = "figures/gsw_vs_time_aug_1_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_aug_1_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_aug_1_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_aug_1_BH_2019_diurnal_with_letters <- gsw_vs_time_aug_1_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_aug_1_BH_2019_diurnal_with_letters, filename = "figures/gsw_vs_time_aug_1_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)



####ANOVA AND TUKEYS HSD GSW DIURNAL AUG 15 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_gsw_vs_time_aug15<- diurnals_2019_F_vs_round %>%
  filter(!is.na(gsw))%>%
  select(datetime, day, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "227")%>%
  filter(gsw < 2)


diurnals_2019_gsw_vs_time_aug15$interval <-cut(diurnals_2019_gsw_vs_time_aug15$datetime, breaks= "140 min", labels = c ("08-15-2019 6:00", "08-15-2019 9:00","08-15-2019 11:15", "08-15-2019 13:30", "08-15-2019 17:00", "08-15-2019 17:00"))

str(diurnals_2019_gsw_vs_time_aug15$interval)


diurnals_2019_gsw_vs_time_aug15 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_gsw_vs_time_aug15$treatment<- reorder(diurnals_2019_gsw_vs_time_aug15$treatment, diurnals_2019_gsw_vs_time_aug15$datetime) 

tz(diurnals_2019_gsw_vs_time_aug15$datetime)
diurnals_2019_gsw_vs_time_aug15$Rep<- format(diurnals_2019_gsw_vs_time_aug15$Rep)
as.character(diurnals_2019_gsw_vs_time_aug15$Rep)
str(diurnals_2019_gsw_vs_time_aug15$Rep)


str(diurnals_2019_gsw_vs_time_aug15$interval)


diurnals_2019_gsw_vs_time_aug15$interval<-as.character(diurnals_2019_gsw_vs_time_aug15$interval)

diurnals_2019_gsw_vs_time_aug15$interval<-format(diurnals_2019_gsw_vs_time_aug15$interval)

diurnals_2019_gsw_vs_time_aug15$interval<-as.POSIXct(diurnals_2019_gsw_vs_time_aug15$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_gsw_vs_time_aug15$interval)


diurnals_2019_gsw_vs_time_aug15_anova<-diurnals_2019_gsw_vs_time_aug15


diurnals_2019_gsw_vs_time_aug15_anova$treatment <- as.character(diurnals_2019_gsw_vs_time_aug15_anova$treatment)
str(diurnals_2019_gsw_vs_time_aug15_anova$treatment)

diurnals_2019_gsw_vs_time_aug15_anova_tally<-diurnals_2019_gsw_vs_time_aug15_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_gsw_vs_time_aug15_anova$treatment <- as.factor(diurnals_2019_gsw_vs_time_aug15_anova$treatment)

str(diurnals_2019_gsw_vs_time_aug15_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_gsw_vs_time_aug15_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_aug_15_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_gsw_vs_time_aug15_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_aug_15_bh_2019 <- rbind(interval_results,results_df_gsw_diurnal_aug_15_bh_2019)
}

results_df_gsw_diurnal_aug_15_bh_2019$interval <- as.POSIXct(results_df_gsw_diurnal_aug_15_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_aug_15_bh_2019$interval)


str(results_df_gsw_diurnal_aug_15_bh_2019)
results_df_gsw_diurnal_aug_15_bh_2019$Mean_sem <- paste(round(results_df_gsw_diurnal_aug_15_bh_2019$mean, 2), "±", round(results_df_gsw_diurnal_aug_15_bh_2019$standard_error, 2),results_df_gsw_diurnal_aug_15_bh_2019$letters_ordered.groups)


str(results_df_gsw_diurnal_aug_15_bh_2019)

write.csv(results_df_gsw_diurnal_aug_15_bh_2019,"data_output/results_df_gsw_diurnal_aug_15_bh_2019.csv")

pd<- position_dodge(1400)

gsw_vs_time_aug_15_BH_2019_diurnal<-
  ggplot(results_df_gsw_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("August 15 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif", color = "white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(legend.position = "none") 


ggsave(gsw_vs_time_aug_15_BH_2019_diurnal, filename = "figures/gsw_vs_time_aug_15_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_aug_15_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_aug_15_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_aug_15_BH_2019_diurnal_with_letters <- gsw_vs_time_aug_15_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_aug_15_BH_2019_diurnal_with_letters, filename = "figures/gsw_vs_time_aug_15_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####ANOVA AND TUKEYS HSD GSW DIURNAL AUG 20 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_gsw_vs_time_aug20<- diurnals_2019_F_vs_round %>%
  filter(!is.na(gsw))%>%
  select(datetime, day, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "232")%>%
  filter(gsw < 0.9)

diurnals_2019_gsw_vs_time_aug20$interval <-cut(diurnals_2019_gsw_vs_time_aug20$datetime, breaks= "147 min", labels = c ("08-20-2019 6:00", "08-20-2019 9:00","08-20-2019 11:15", "08-20-2019 13:30", "08-20-2019 17:00"))


str(diurnals_2019_gsw_vs_time_aug20$interval)


diurnals_2019_gsw_vs_time_aug20 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_gsw_vs_time_aug20$treatment<- reorder(diurnals_2019_gsw_vs_time_aug20$treatment, diurnals_2019_gsw_vs_time_aug20$datetime) 

tz(diurnals_2019_gsw_vs_time_aug20$datetime)
diurnals_2019_gsw_vs_time_aug20$Rep<- format(diurnals_2019_gsw_vs_time_aug20$Rep)
as.character(diurnals_2019_gsw_vs_time_aug20$Rep)
str(diurnals_2019_gsw_vs_time_aug20$Rep)


str(diurnals_2019_gsw_vs_time_aug20$interval)


diurnals_2019_gsw_vs_time_aug20$interval<-as.character(diurnals_2019_gsw_vs_time_aug20$interval)

diurnals_2019_gsw_vs_time_aug20$interval<-format(diurnals_2019_gsw_vs_time_aug20$interval)

diurnals_2019_gsw_vs_time_aug20$interval<-as.POSIXct(diurnals_2019_gsw_vs_time_aug20$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_gsw_vs_time_aug20$interval)


diurnals_2019_gsw_vs_time_aug20_anova<-diurnals_2019_gsw_vs_time_aug20


diurnals_2019_gsw_vs_time_aug20_anova$treatment <- as.character(diurnals_2019_gsw_vs_time_aug20_anova$treatment)
str(diurnals_2019_gsw_vs_time_aug20_anova$treatment)

diurnals_2019_gsw_vs_time_aug20_anova_tally<-diurnals_2019_gsw_vs_time_aug20_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_gsw_vs_time_aug20_anova$treatment <- as.factor(diurnals_2019_gsw_vs_time_aug20_anova$treatment)

str(diurnals_2019_gsw_vs_time_aug20_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_gsw_vs_time_aug20_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_aug_20_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_gsw_vs_time_aug20_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_aug_20_bh_2019 <- rbind(interval_results,results_df_gsw_diurnal_aug_20_bh_2019)
}

results_df_gsw_diurnal_aug_20_bh_2019$interval <- as.POSIXct(results_df_gsw_diurnal_aug_20_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_aug_20_bh_2019$interval)


str(results_df_gsw_diurnal_aug_20_bh_2019)
results_df_gsw_diurnal_aug_20_bh_2019$Mean_sem <- paste(round(results_df_gsw_diurnal_aug_20_bh_2019$mean, 2), "±", round(results_df_gsw_diurnal_aug_20_bh_2019$standard_error, 2),results_df_gsw_diurnal_aug_20_bh_2019$letters_ordered.groups)


str(results_df_gsw_diurnal_aug_20_bh_2019)

write.csv(results_df_gsw_diurnal_aug_20_bh_2019,"data_output/results_df_gsw_diurnal_aug_20_bh_2019.csv")

pd<- position_dodge(1400)

gsw_vs_time_aug_20_BH_2019_diurnal<-
  ggplot(results_df_gsw_diurnal_aug_20_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("August 20 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif", color = "white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(legend.position = "none")  +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 


ggsave(gsw_vs_time_aug_20_BH_2019_diurnal, filename = "figures/gsw_vs_time_aug_20_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_aug_20_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_aug_20_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_aug_20_BH_2019_diurnal_with_letters <- gsw_vs_time_aug_20_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_aug_20_BH_2019_diurnal_with_letters, filename = "figures/gsw_vs_time_aug_20_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####ANOVA AND TUKEYS HSD GSW DIURNAL SEP 5 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_gsw_vs_time_sep5<- diurnals_2019_F_vs_round %>%
  filter(!is.na(gsw))%>%
  select(datetime, day, gsw, pixel_number, round, treatment, Rep) %>%
  filter(day == "248") 

diurnals_2019_gsw_vs_time_sep5$interval <-cut(diurnals_2019_gsw_vs_time_sep5$datetime, breaks= "145 min", labels = c ("09-05-2019 6:00", "09-05-2019 9:00","09-05-2019 11:15", "09-05-2019 13:30", "09-05-2019 17:00"))


str(diurnals_2019_gsw_vs_time_sep5$interval)


diurnals_2019_gsw_vs_time_sep5 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2019_gsw_vs_time_sep5$treatment<- reorder(diurnals_2019_gsw_vs_time_sep5$treatment, diurnals_2019_gsw_vs_time_sep5$datetime) 

tz(diurnals_2019_gsw_vs_time_sep5$datetime)
diurnals_2019_gsw_vs_time_sep5$Rep<- format(diurnals_2019_gsw_vs_time_sep5$Rep)
as.character(diurnals_2019_gsw_vs_time_sep5$Rep)
str(diurnals_2019_gsw_vs_time_sep5$Rep)


str(diurnals_2019_gsw_vs_time_sep5$interval)


diurnals_2019_gsw_vs_time_sep5$interval<-as.character(diurnals_2019_gsw_vs_time_sep5$interval)

diurnals_2019_gsw_vs_time_sep5$interval<-format(diurnals_2019_gsw_vs_time_sep5$interval)

diurnals_2019_gsw_vs_time_sep5$interval<-as.POSIXct(diurnals_2019_gsw_vs_time_sep5$interval, format = "%m-%d-%Y %H:%M")

str(diurnals_2019_gsw_vs_time_sep5$interval)


diurnals_2019_gsw_vs_time_sep5_anova<-diurnals_2019_gsw_vs_time_sep5


diurnals_2019_gsw_vs_time_sep5_anova$treatment <- as.character(diurnals_2019_gsw_vs_time_sep5_anova$treatment)
str(diurnals_2019_gsw_vs_time_sep5_anova$treatment)

diurnals_2019_gsw_vs_time_sep5_anova_tally<-diurnals_2019_gsw_vs_time_sep5_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2019_gsw_vs_time_sep5_anova$treatment <- as.factor(diurnals_2019_gsw_vs_time_sep5_anova$treatment)

str(diurnals_2019_gsw_vs_time_sep5_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2019_gsw_vs_time_sep5_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_sep_5_bh_2019<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2019_gsw_vs_time_sep5_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_sep_5_bh_2019 <- rbind(interval_results,results_df_gsw_diurnal_sep_5_bh_2019)
}

results_df_gsw_diurnal_sep_5_bh_2019$interval <- as.POSIXct(results_df_gsw_diurnal_sep_5_bh_2019$interval, format = "%m-%d-%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_sep_5_bh_2019$interval)


str(results_df_gsw_diurnal_sep_5_bh_2019)
results_df_gsw_diurnal_sep_5_bh_2019$Mean_sem <- paste(round(results_df_gsw_diurnal_sep_5_bh_2019$mean, 2), "±", round(results_df_gsw_diurnal_sep_5_bh_2019$standard_error, 2),results_df_gsw_diurnal_sep_5_bh_2019$letters_ordered.groups)


str(results_df_gsw_diurnal_sep_5_bh_2019)

write.csv(results_df_gsw_diurnal_sep_5_bh_2019,"data_output/results_df_gsw_diurnal_sep_5_bh_2019.csv")

pd<- position_dodge(1400)

gsw_vs_time_sep_5_BH_2019_diurnal<-
  ggplot(results_df_gsw_diurnal_sep_5_bh_2019, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("August 20 2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif", color = "white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=24, family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(legend.position = "none")  +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 


ggsave(gsw_vs_time_sep_5_BH_2019_diurnal, filename = "figures/gsw_vs_time_sep_5_BH_2019_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_sep_5_bh_2019, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_sep_5_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_sep_5_BH_2019_diurnal_with_letters <- gsw_vs_time_sep_5_BH_2019_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_sep_5_BH_2019_diurnal_with_letters, filename = "figures/gsw_vs_time_sep_5_BH_2019_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####Plot TOGETHER DIURNALS HW1 - HW2 2019 #####

library(cowplot)

panel_plot_gas_exchange_time_HW1_july1_2019 <- plot_grid(
  A_vs_time_july_1_BH_2019_diurnal_with_letters,
  A_vs_time_july_25_BH_2019_diurnal_with_letters,
  A_vs_time_july_28_BH_2019_diurnal_with_letters,
  A_vs_time_aug_01_BH_2019_diurnal_with_letters,
  gsw_vs_time_july_1_BH_2019_diurnal_with_letters, 
  gsw_vs_time_july_25_BH_2019_diurnal_with_letters, 
  gsw_vs_time_july_28_BH_2019_diurnal_with_letters, 
  gsw_vs_time_aug_1_BH_2019_diurnal_with_letters,
  labels =c ("          A","B   Pre-Heatwave","   C     Heatwave","D     Post-Heatwave","          E","          F","          G","          H"),
  vjust = 4,
  hjust = -0.5, 
  label_size = 18,
  ncol = 4, 
  nrow = 2, 
  align = "hv",
  axis = "tblr" 
)
ggsave(panel_plot_gas_exchange_time_HW1_july1_2019  , filename = "figures/panel_plot_gas_exchange_time_HW1_july1_2019.pdf", device = cairo_pdf, width = 24, height = 13)

ggsave(panel_plot_gas_exchange_time_HW1_july1_2019  , filename = "figures/panel_plot_gas_exchange_time_HW1_july1_2019.png", width = 24, height = 13)

panel_plot_gas_exchange_time_HW2_2019 <- plot_grid(
  A_vs_time_aug_15_BH_2019_diurnal_with_letters,
  A_vs_time_aug_20_BH_2019_diurnal_with_letters,
  A_vs_time_sep_05_BH_2019_diurnal_with_letters,
  gsw_vs_time_aug_15_BH_2019_diurnal_with_letters, 
  gsw_vs_time_aug_20_BH_2019_diurnal_with_letters, 
  gsw_vs_time_sep_5_BH_2019_diurnal_with_letters, 
  labels =c ("      A Heatwave","   B Post-Heatwave","      C Recovery","            D","            E","            F"),
  vjust = 3,
  hjust = -0.5, 
  label_size = 18,
  ncol = 3, 
  nrow = 2, 
  align = "hv",
  axis = "tblr" 
)
ggsave(panel_plot_gas_exchange_time_HW2_2019  , filename = "figures/panel_plot_gas_exchange_time_HW2_2019.pdf", device = cairo_pdf, width = 18, height = 13)


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

#####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL MAY 27 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_A_vs_time_may27<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, round, treatment) %>%
  filter(day == "148") %>%
  mutate(interval = case_when(
    round == 1 ~ "5/27/2020 6:00",
    round == 2 ~ "5/27/2020 9:00",
    round == 3 ~ "5/27/2020 12:00", 
    round == 4 ~ "5/27/2020 15:00",
    round == 5 ~ "5/27/2020 17:30"
  ))

diurnals_2020_A_vs_time_may27$interval <- format((diurnals_2020_A_vs_time_may27$interval))
diurnals_2020_A_vs_time_may27$interval<- as.factor(diurnals_2020_A_vs_time_may27$interval)

str(diurnals_2020_A_vs_time_may27$interval)

diurnals_2020_A_vs_time_may27$interval<-as.character(diurnals_2020_A_vs_time_may27$interval)

diurnals_2020_A_vs_time_may27$interval<-format(diurnals_2020_A_vs_time_may27$interval)

diurnals_2020_A_vs_time_may27$interval<-as.POSIXct(diurnals_2020_A_vs_time_may27$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_may27$interval)


diurnals_2020_A_vs_time_may27 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_may27$treatment<- reorder(diurnals_2020_A_vs_time_may27$treatment, diurnals_2020_A_vs_time_may27$datetime) 


diurnals_2020_A_vs_time_may27_anova<-diurnals_2020_A_vs_time_may27


diurnals_2020_A_vs_time_may27_anova$treatment <- as.character(diurnals_2020_A_vs_time_may27_anova$treatment)
str(diurnals_2020_A_vs_time_may27_anova$treatment)

diurnals_2020_A_vs_time_may27_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_may27_anova$treatment <- as.factor(diurnals_2020_A_vs_time_may27_anova$treatment)

str(diurnals_2020_A_vs_time_may27_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_may27_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_may_27_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_may27_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_may_27_bh_2020 <- rbind(interval_results,results_df_A_diurnal_may_27_bh_2020)
}

results_df_A_diurnal_may_27_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_may_27_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_may_27_bh_2020$interval)


str(results_df_A_diurnal_may_27_bh_2020)
results_df_A_diurnal_may_27_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_may_27_bh_2020$mean, 2), "±", round(results_df_A_diurnal_may_27_bh_2020$standard_error, 2),results_df_A_diurnal_may_27_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_may_27_bh_2020)

write.csv(results_df_A_diurnal_may_27_bh_2020,"data_output/results_df_A_diurnal_may_27_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_may_27_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_may_27_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("May 27 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave(A_vs_time_may_27_BH_2020_diurnal, filename = "figures/A_vs_time_may_27_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_may_27_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_may_27_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_may_27_BH_2020_diurnal_with_letters <- A_vs_time_may_27_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_may_27_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_may_27_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

##### ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL SEP 7 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_A_vs_time_sep7<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, gsw, Fv.Fm, pixel_number, round, treatment) %>%
  filter(day == "251")  %>%
  mutate(interval = case_when(
    round == 1 ~ "9/7/2020  6:00",
    round == 2 ~ "9/7/2020  8:45",
    round == 3 ~ "9/7/2020  13:30", 
  )) %>% 
  filter(!(round == "3" & A < 1)) 

diurnals_2020_A_vs_time_sep7$interval <- format((diurnals_2020_A_vs_time_sep7$interval))
diurnals_2020_A_vs_time_sep7$interval<- as.factor(diurnals_2020_A_vs_time_sep7$interval)

str(diurnals_2020_A_vs_time_sep7$interval)

diurnals_2020_A_vs_time_sep7$interval<-as.character(diurnals_2020_A_vs_time_sep7$interval)

diurnals_2020_A_vs_time_sep7$interval<-format(diurnals_2020_A_vs_time_sep7$interval)

diurnals_2020_A_vs_time_sep7$interval<-as.POSIXct(diurnals_2020_A_vs_time_sep7$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_sep7$interval)


diurnals_2020_A_vs_time_sep7 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_sep7$treatment<- reorder(diurnals_2020_A_vs_time_sep7$treatment, diurnals_2020_A_vs_time_sep7$datetime) 


diurnals_2020_A_vs_time_sep7_anova<-diurnals_2020_A_vs_time_sep7


diurnals_2020_A_vs_time_sep7_anova$treatment <- as.character(diurnals_2020_A_vs_time_sep7_anova$treatment)
str(diurnals_2020_A_vs_time_sep7_anova$treatment)

diurnals_2020_A_vs_time_sep7_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_sep7_anova$treatment <- as.factor(diurnals_2020_A_vs_time_sep7_anova$treatment)

str(diurnals_2020_A_vs_time_sep7_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_sep7_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_sep_7_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_sep7_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_sep_7_bh_2020 <- rbind(interval_results,results_df_A_diurnal_sep_7_bh_2020)
}

results_df_A_diurnal_sep_7_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_sep_7_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_sep_7_bh_2020$interval)


str(results_df_A_diurnal_sep_7_bh_2020)
results_df_A_diurnal_sep_7_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_sep_7_bh_2020$mean, 2), "±", round(results_df_A_diurnal_sep_7_bh_2020$standard_error, 2),results_df_A_diurnal_sep_7_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_sep_7_bh_2020)

write.csv(results_df_A_diurnal_sep_7_bh_2020,"data_output/results_df_A_diurnal_sep_7_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_sep_7_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_sep_7_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 2500, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("September 7 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) +
  theme(legend.position = "none")

ggsave(A_vs_time_sep_7_BH_2020_diurnal, filename = "figures/A_vs_time_sep_7_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_sep_7_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_sep_7_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability


vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_sep_7_BH_2020_diurnal_with_letters <- A_vs_time_sep_7_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_sep_7_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_sep_7_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


#####ANOVA AND TUKEYS HSD GSW DIURNAL MAY 27 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_may27<- diurnals_2020_A_vs_time %>%
  filter(!is.na(gsw)) %>%
  select(datetime, day, gsw, round, treatment) %>%
  filter(day == "148") %>%
  mutate(interval = case_when(
    round == 1 ~ "5/27/2020 6:00",
    round == 2 ~ "5/27/2020 9:00",
    round == 3 ~ "5/27/2020 12:00", 
    round == 4 ~ "5/27/2020 15:00",
    round == 5 ~ "5/27/2020 17:30"
  ))

diurnals_2020_gsw_vs_time_may27$interval <- format((diurnals_2020_gsw_vs_time_may27$interval))
diurnals_2020_gsw_vs_time_may27$interval<- as.factor(diurnals_2020_gsw_vs_time_may27$interval)

str(diurnals_2020_gsw_vs_time_may27$interval)

diurnals_2020_gsw_vs_time_may27$interval<-as.character(diurnals_2020_gsw_vs_time_may27$interval)

diurnals_2020_gsw_vs_time_may27$interval<-format(diurnals_2020_gsw_vs_time_may27$interval)

diurnals_2020_gsw_vs_time_may27$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_may27$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_may27$interval)


diurnals_2020_gsw_vs_time_may27 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_may27$treatment<- reorder(diurnals_2020_gsw_vs_time_may27$treatment, diurnals_2020_gsw_vs_time_may27$datetime) 


diurnals_2020_gsw_vs_time_may27_anova<-diurnals_2020_gsw_vs_time_may27


diurnals_2020_gsw_vs_time_may27_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_may27_anova$treatment)
str(diurnals_2020_gsw_vs_time_may27_anova$treatment)

diurnals_2020_gsw_vs_time_may27_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_may27_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_may27_anova$treatment)

str(diurnals_2020_gsw_vs_time_may27_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_may27_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_may_27_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_may27_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_may_27_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_may_27_bh_2020)
}

results_df_gsw_diurnal_may_27_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_may_27_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_may_27_bh_2020$interval)


str(results_df_gsw_diurnal_may_27_bh_2020)
results_df_gsw_diurnal_may_27_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_may_27_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_may_27_bh_2020$standard_error, 2),results_df_gsw_diurnal_may_27_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_may_27_bh_2020)

write.csv(results_df_gsw_diurnal_may_27_bh_2020,"data_output/results_df_gsw_diurnal_may_27_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_may_27_BH_2020_diurnal<-
  ggplot(results_df_gsw_diurnal_may_27_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("May 27 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")

ggsave(gsw_vs_time_may_27_BH_2020_diurnal, filename = "figures/gsw_vs_time_may_27_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_may_27_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_may_27_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_may_27_BH_2020_diurnal_with_letters <- gsw_vs_time_may_27_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_may_27_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_may_27_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


#####ANOVA AND TUKEYS HSD GSW DIURNAL SEPTEMBER 7 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_sep7<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, gsw, Fv.Fm, pixel_number, round, treatment) %>%
  filter(day == "251")  %>%
  mutate(interval = case_when(
    round == 1 ~ "9/7/2020  6:00",
    round == 2 ~ "9/7/2020  8:45",
    round == 3 ~ "9/7/2020  13:30", 
  ))

diurnals_2020_gsw_vs_time_sep7$interval <- format((diurnals_2020_gsw_vs_time_sep7$interval))
diurnals_2020_gsw_vs_time_sep7$interval<- as.factor(diurnals_2020_gsw_vs_time_sep7$interval)

str(diurnals_2020_gsw_vs_time_sep7$interval)

diurnals_2020_gsw_vs_time_sep7$interval<-as.character(diurnals_2020_gsw_vs_time_sep7$interval)

diurnals_2020_gsw_vs_time_sep7$interval<-format(diurnals_2020_gsw_vs_time_sep7$interval)

diurnals_2020_gsw_vs_time_sep7$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_sep7$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_sep7$interval)


diurnals_2020_gsw_vs_time_sep7 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_sep7$treatment<- reorder(diurnals_2020_gsw_vs_time_sep7$treatment, diurnals_2020_gsw_vs_time_sep7$datetime) 


diurnals_2020_gsw_vs_time_sep7_anova<-diurnals_2020_gsw_vs_time_sep7


diurnals_2020_gsw_vs_time_sep7_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_sep7_anova$treatment)
str(diurnals_2020_gsw_vs_time_sep7_anova$treatment)

diurnals_2020_gsw_vs_time_sep7_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_sep7_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_sep7_anova$treatment)

str(diurnals_2020_gsw_vs_time_sep7_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_sep7_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_sep_7_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_sep7_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_sep_7_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_sep_7_bh_2020)
}

results_df_gsw_diurnal_sep_7_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_sep_7_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_sep_7_bh_2020$interval)


str(results_df_gsw_diurnal_sep_7_bh_2020)
results_df_gsw_diurnal_sep_7_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_sep_7_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_sep_7_bh_2020$standard_error, 2),results_df_gsw_diurnal_sep_7_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_sep_7_bh_2020)

write.csv(results_df_gsw_diurnal_sep_7_bh_2020,"data_output/results_df_gsw_diurnal_sep_7_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_sep7_BH_2020_diurnal <-
  ggplot(results_df_gsw_diurnal_sep_7_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 2500, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("September 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 

ggsave(gsw_vs_time_sep7_BH_2020_diurnal, filename = "figures/gsw_vs_time_sep7_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_sep_7_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_sep_7_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <-  0.03 # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_sep7_BH_2020_diurnal_with_letters <- gsw_vs_time_sep7_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_sep7_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_sep7_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####Plot together Diurnals HW1 & HW4 2020####


# Combine the plots with the shared legend
panel_plot_gas_exchange_time_HW1_HW4_2020 <- plot_grid(
  A_vs_time_may_27_BH_2020_diurnal_with_letters,
  A_vs_time_sep_7_BH_2020_diurnal_with_letters,
  gsw_vs_time_may_27_BH_2020_diurnal_with_letters, 
  gsw_vs_time_sep7_BH_2020_diurnal_with_letters, 
  labels = c("      A HW1  ", "      B HW4", "         C ", "          D"),
  vjust = 3,
  hjust = -0.5, 
  label_size = 18,
  ncol = 2, 
  nrow = 2, 
  align = "hv",
  axis = "tblr"
)
ggsave(panel_plot_gas_exchange_time_HW1_HW4_2020  , filename = "figures/panel_plot_gas_exchange_time_HW1_HW4_2020.pdf", device = cairo_pdf, width = 15, height = 13)


extract_legend <- function(plot) {
  gtable <- ggplotGrob(plot)
  guide_box <- which(sapply(gtable$grobs, function(x) x$name) == "guide-box")
  legend <- gtable$grobs[[guide_box]]
  return(legend)
}

legend <- extract_legend(A_vs_time_may_27_BH_2020_diurnal_with_letters + theme(legend.position = "bottom"))

# Add the legend to the combined plot
final_plot <-plot_grid(panel_plot_gas_exchange_time_HW1_HW4_2020, legend, ncol = 1, rel_heights = c(1, 0.1))


#####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL July 10 2020 #####

diurnals_2020_A_vs_time_Jul10<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment) %>%
  filter(day == "192") %>%
  mutate(interval = case_when(
    round == 1 ~ "7/10/2020 6:00",
    round == 2 ~ "7/10/2020 9:00",
    round == 3 ~ "7/10/2020 11:30", 
    round == 4 ~ "7/10/2020 14:00",
    round == 5 ~ "7/10/2020 17:00"
  ))

diurnals_2020_A_vs_time_Jul10$interval <- format((diurnals_2020_A_vs_time_Jul10$interval))
diurnals_2020_A_vs_time_Jul10$interval<- as.factor(diurnals_2020_A_vs_time_Jul10$interval)

str(diurnals_2020_A_vs_time_Jul10$interval)

diurnals_2020_A_vs_time_Jul10$interval<-as.character(diurnals_2020_A_vs_time_Jul10$interval)

diurnals_2020_A_vs_time_Jul10$interval<-format(diurnals_2020_A_vs_time_Jul10$interval)

diurnals_2020_A_vs_time_Jul10$interval<-as.POSIXct(diurnals_2020_A_vs_time_Jul10$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_Jul10$interval)


diurnals_2020_A_vs_time_Jul10 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_Jul10$treatment<- reorder(diurnals_2020_A_vs_time_Jul10$treatment, diurnals_2020_A_vs_time_Jul10$datetime) 


diurnals_2020_A_vs_time_Jul10_anova<-diurnals_2020_A_vs_time_Jul10


diurnals_2020_A_vs_time_Jul10_anova$treatment <- as.character(diurnals_2020_A_vs_time_Jul10_anova$treatment)
str(diurnals_2020_A_vs_time_Jul10_anova$treatment)

diurnals_2020_A_vs_time_Jul10_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_Jul10_anova$treatment <- as.factor(diurnals_2020_A_vs_time_Jul10_anova$treatment)

str(diurnals_2020_A_vs_time_Jul10_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_Jul10_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_july_10_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_Jul10_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_july_10_bh_2020 <- rbind(interval_results,results_df_A_diurnal_july_10_bh_2020)
}

results_df_A_diurnal_july_10_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_july_10_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_july_10_bh_2020$interval)


str(results_df_A_diurnal_july_10_bh_2020)
results_df_A_diurnal_july_10_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_july_10_bh_2020$mean, 2), "±", round(results_df_A_diurnal_july_10_bh_2020$standard_error, 2),results_df_A_diurnal_july_10_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_july_10_bh_2020)

write.csv(results_df_A_diurnal_july_10_bh_2020,"data_output/results_df_A_diurnal_july_10_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_july_10_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_july_10_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("July 10 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave(A_vs_time_july_10_BH_2020_diurnal, filename = "figures/A_vs_time_july_10_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_july_10_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_july_10_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_july_10_BH_2020_diurnal_with_letters <- A_vs_time_july_10_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_july_10_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_july_10_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL July 12 2020 #####

diurnals_2020_A_vs_time_jul12<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment) %>%
  filter(day == "194")  %>%
  mutate(interval = case_when(
    round == 1 ~ "7/12/2020 5:30",
    round == 2 ~ "7/12/2020 9:00",
    round == 3 ~ "7/12/2020 11:00", 
    round == 4 ~ "7/12/2020 13:30",
    round == 5 ~ "7/12/2020 17:00"
  ))

diurnals_2020_A_vs_time_jul12$interval <- format((diurnals_2020_A_vs_time_jul12$interval))
diurnals_2020_A_vs_time_jul12$interval<- as.factor(diurnals_2020_A_vs_time_jul12$interval)

str(diurnals_2020_A_vs_time_jul12$interval)

diurnals_2020_A_vs_time_jul12$interval<-as.character(diurnals_2020_A_vs_time_jul12$interval)

diurnals_2020_A_vs_time_jul12$interval<-format(diurnals_2020_A_vs_time_jul12$interval)

diurnals_2020_A_vs_time_jul12$interval<-as.POSIXct(diurnals_2020_A_vs_time_jul12$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_jul12$interval)


diurnals_2020_A_vs_time_jul12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_jul12$treatment<- reorder(diurnals_2020_A_vs_time_jul12$treatment, diurnals_2020_A_vs_time_jul12$datetime) 


diurnals_2020_A_vs_time_jul12_anova<-diurnals_2020_A_vs_time_jul12


diurnals_2020_A_vs_time_jul12_anova$treatment <- as.character(diurnals_2020_A_vs_time_jul12_anova$treatment)
str(diurnals_2020_A_vs_time_jul12_anova$treatment)

diurnals_2020_A_vs_time_jul12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_jul12_anova$treatment <- as.factor(diurnals_2020_A_vs_time_jul12_anova$treatment)

str(diurnals_2020_A_vs_time_jul12_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_jul12_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_july_12_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_jul12_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_july_12_bh_2020 <- rbind(interval_results,results_df_A_diurnal_july_12_bh_2020)
}

results_df_A_diurnal_july_12_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_july_12_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_july_12_bh_2020$interval)


str(results_df_A_diurnal_july_12_bh_2020)
results_df_A_diurnal_july_12_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_july_12_bh_2020$mean, 2), "±", round(results_df_A_diurnal_july_12_bh_2020$standard_error, 2),results_df_A_diurnal_july_12_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_july_12_bh_2020)

write.csv(results_df_A_diurnal_july_12_bh_2020,"data_output/results_df_A_diurnal_july_12_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_july_12_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("July 12 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 

ggsave(A_vs_time_july_12_BH_2020_diurnal, filename = "figures/A_vs_time_july_12_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_july_12_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_july_12_BH_2020_diurnal_with_letters <- A_vs_time_july_12_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_july_12_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_july_12_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL July 20 2020 #####

diurnals_2020_A_vs_time_JUL20<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A))%>%
  select(datetime, day, A, pixel_number, round, treatment) %>%
  filter(day == "202")  %>%
  mutate(interval = case_when(
    round == 1 ~ "7/20/2020 5:30",
    round == 2 ~ "7/20/2020 8:30",
    round == 3 ~ "7/20/2020 11:00", 
    round == 4 ~ "7/20/2020 13:30",
    round == 5 ~ "7/20/2020 16:30"
  ))



diurnals_2020_A_vs_time_JUL20$interval <- format((diurnals_2020_A_vs_time_JUL20$interval))
diurnals_2020_A_vs_time_JUL20$interval<- as.factor(diurnals_2020_A_vs_time_JUL20$interval)

str(diurnals_2020_A_vs_time_JUL20$interval)

diurnals_2020_A_vs_time_JUL20$interval<-as.character(diurnals_2020_A_vs_time_JUL20$interval)

diurnals_2020_A_vs_time_JUL20$interval<-format(diurnals_2020_A_vs_time_JUL20$interval)

diurnals_2020_A_vs_time_JUL20$interval<-as.POSIXct(diurnals_2020_A_vs_time_JUL20$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_JUL20$interval)


diurnals_2020_A_vs_time_JUL20 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_JUL20$treatment<- reorder(diurnals_2020_A_vs_time_JUL20$treatment, diurnals_2020_A_vs_time_JUL20$datetime) 


diurnals_2020_A_vs_time_JUL20_anova<-diurnals_2020_A_vs_time_JUL20


diurnals_2020_A_vs_time_JUL20_anova$treatment <- as.character(diurnals_2020_A_vs_time_JUL20_anova$treatment)
str(diurnals_2020_A_vs_time_JUL20_anova$treatment)

diurnals_2020_A_vs_time_JUL20_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_JUL20_anova$treatment <- as.factor(diurnals_2020_A_vs_time_JUL20_anova$treatment)

str(diurnals_2020_A_vs_time_JUL20_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_JUL20_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_july_20_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_JUL20_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_july_20_bh_2020 <- rbind(interval_results,results_df_A_diurnal_july_20_bh_2020)
}

results_df_A_diurnal_july_20_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_july_20_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_july_20_bh_2020$interval)


str(results_df_A_diurnal_july_20_bh_2020)
results_df_A_diurnal_july_20_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_july_20_bh_2020$mean, 2), "±", round(results_df_A_diurnal_july_20_bh_2020$standard_error, 2),results_df_A_diurnal_july_20_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_july_20_bh_2020)

write.csv(results_df_A_diurnal_july_20_bh_2020,"data_output/results_df_A_diurnal_july_20_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_july_20_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_july_20_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("July 20 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 

ggsave(A_vs_time_july_20_BH_2020_diurnal, filename = "figures/A_vs_time_july_20_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_july_20_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_july_20_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_july_20_BH_2020_diurnal_with_letters <- A_vs_time_july_20_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_july_20_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_july_20_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


#####ANOVA AND TUKEYS HSD GSW DIURNAL July 10 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_Jul10<- diurnals_2020_A_vs_time %>%
  filter(!is.na(gsw)) %>%
  select(datetime, day, gsw, pixel_number, round, treatment) %>%
  filter(day == "192") %>%
  mutate(interval = case_when(
    round == 1 ~ "7/10/2020 6:00",
    round == 2 ~ "7/10/2020 9:00",
    round == 3 ~ "7/10/2020 11:30", 
    round == 4 ~ "7/10/2020 14:00",
    round == 5 ~ "7/10/2020 17:00"
  ))

diurnals_2020_gsw_vs_time_Jul10$interval <- format((diurnals_2020_gsw_vs_time_Jul10$interval))
diurnals_2020_gsw_vs_time_Jul10$interval<- as.factor(diurnals_2020_gsw_vs_time_Jul10$interval)

str(diurnals_2020_gsw_vs_time_Jul10$interval)

diurnals_2020_gsw_vs_time_Jul10$interval<-as.character(diurnals_2020_gsw_vs_time_Jul10$interval)

diurnals_2020_gsw_vs_time_Jul10$interval<-format(diurnals_2020_gsw_vs_time_Jul10$interval)

diurnals_2020_gsw_vs_time_Jul10$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_Jul10$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_Jul10$interval)


diurnals_2020_gsw_vs_time_Jul10 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_Jul10$treatment<- reorder(diurnals_2020_gsw_vs_time_Jul10$treatment, diurnals_2020_gsw_vs_time_Jul10$datetime) 


diurnals_2020_gsw_vs_time_Jul10_anova<-diurnals_2020_gsw_vs_time_Jul10


diurnals_2020_gsw_vs_time_Jul10_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_Jul10_anova$treatment)
str(diurnals_2020_gsw_vs_time_Jul10_anova$treatment)

diurnals_2020_gsw_vs_time_Jul10_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_Jul10_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_Jul10_anova$treatment)

str(diurnals_2020_gsw_vs_time_Jul10_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_Jul10_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_july_10_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_Jul10_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_july_10_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_july_10_bh_2020)
}

results_df_gsw_diurnal_july_10_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_july_10_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_july_10_bh_2020$interval)


str(results_df_gsw_diurnal_july_10_bh_2020)
results_df_gsw_diurnal_july_10_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_july_10_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_july_10_bh_2020$standard_error, 2),results_df_gsw_diurnal_july_10_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_july_10_bh_2020)

write.csv(results_df_gsw_diurnal_july_10_bh_2020,"data_output/results_df_gsw_diurnal_july_10_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_july_10_BH_2020_diurnal<-
  ggplot(results_df_gsw_diurnal_july_10_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("May 27 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")

ggsave(gsw_vs_time_july_10_BH_2020_diurnal, filename = "figures/gsw_vs_time_july_10_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_july_10_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_july_10_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_july_10_BH_2020_diurnal_with_letters <- gsw_vs_time_july_10_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_july_10_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_july_10_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD GSW DIURNAL July 12 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_jul12<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, gsw, pixel_number, round, treatment) %>%
  filter(day == "194")  %>%
  mutate(interval = case_when(
    round == 1 ~ "7/12/2020 5:30",
    round == 2 ~ "7/12/2020 9:00",
    round == 3 ~ "7/12/2020 11:00", 
    round == 4 ~ "7/12/2020 13:30",
    round == 5 ~ "7/12/2020 17:00"
  ))

diurnals_2020_gsw_vs_time_jul12$interval <- format((diurnals_2020_gsw_vs_time_jul12$interval))
diurnals_2020_gsw_vs_time_jul12$interval<- as.factor(diurnals_2020_gsw_vs_time_jul12$interval)

str(diurnals_2020_gsw_vs_time_jul12$interval)

diurnals_2020_gsw_vs_time_jul12$interval<-as.character(diurnals_2020_gsw_vs_time_jul12$interval)

diurnals_2020_gsw_vs_time_jul12$interval<-format(diurnals_2020_gsw_vs_time_jul12$interval)

diurnals_2020_gsw_vs_time_jul12$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_jul12$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_jul12$interval)


diurnals_2020_gsw_vs_time_jul12 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_jul12$treatment<- reorder(diurnals_2020_gsw_vs_time_jul12$treatment, diurnals_2020_gsw_vs_time_jul12$datetime) 


diurnals_2020_gsw_vs_time_jul12_anova<-diurnals_2020_gsw_vs_time_jul12


diurnals_2020_gsw_vs_time_jul12_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_jul12_anova$treatment)
str(diurnals_2020_gsw_vs_time_jul12_anova$treatment)

diurnals_2020_gsw_vs_time_jul12_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_jul12_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_jul12_anova$treatment)

str(diurnals_2020_gsw_vs_time_jul12_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_jul12_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_july_12_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_jul12_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_july_12_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_july_12_bh_2020)
}

results_df_gsw_diurnal_july_12_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_july_12_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_july_12_bh_2020$interval)


str(results_df_gsw_diurnal_july_12_bh_2020)
results_df_gsw_diurnal_july_12_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_july_12_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_july_12_bh_2020$standard_error, 2),results_df_gsw_diurnal_july_12_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_july_12_bh_2020)

write.csv(results_df_gsw_diurnal_july_12_bh_2020,"data_output/results_df_gsw_diurnal_july_12_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_july_12_BH_2020_diurnal<-
  ggplot(results_df_gsw_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("July 12 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 

ggsave(gsw_vs_time_july_12_BH_2020_diurnal, filename = "figures/gsw_vs_time_july_12_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_july_12_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_july_12_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_july_12_BH_2020_diurnal_with_letters <- gsw_vs_time_july_12_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_july_12_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_july_12_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD GSW DIURNAL July 20 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_JUL20<- diurnals_2020_A_vs_time %>%
  filter(!is.na(gsw))%>%
  select(datetime, day, gsw, pixel_number, round, treatment) %>%
  filter(day == "202")  %>%
  mutate(interval = case_when(
    round == 1 ~ "7/20/2020 5:30",
    round == 2 ~ "7/20/2020 8:30",
    round == 3 ~ "7/20/2020 11:00", 
    round == 4 ~ "7/20/2020 13:30",
    round == 5 ~ "7/20/2020 16:30"
  ))

diurnals_2020_gsw_vs_time_JUL20$interval <- format((diurnals_2020_gsw_vs_time_JUL20$interval))
diurnals_2020_gsw_vs_time_JUL20$interval<- as.factor(diurnals_2020_gsw_vs_time_JUL20$interval)

str(diurnals_2020_gsw_vs_time_JUL20$interval)

diurnals_2020_gsw_vs_time_JUL20$interval<-as.character(diurnals_2020_gsw_vs_time_JUL20$interval)

diurnals_2020_gsw_vs_time_JUL20$interval<-format(diurnals_2020_gsw_vs_time_JUL20$interval)

diurnals_2020_gsw_vs_time_JUL20$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_JUL20$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_JUL20$interval)


diurnals_2020_gsw_vs_time_JUL20 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_JUL20$treatment<- reorder(diurnals_2020_gsw_vs_time_JUL20$treatment, diurnals_2020_gsw_vs_time_JUL20$datetime) 


diurnals_2020_gsw_vs_time_JUL20_anova<-diurnals_2020_gsw_vs_time_JUL20


diurnals_2020_gsw_vs_time_JUL20_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_JUL20_anova$treatment)
str(diurnals_2020_gsw_vs_time_JUL20_anova$treatment)

diurnals_2020_gsw_vs_time_JUL20_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_JUL20_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_JUL20_anova$treatment)

str(diurnals_2020_gsw_vs_time_JUL20_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_JUL20_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_july_20_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_JUL20_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_july_20_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_july_20_bh_2020)
}

results_df_gsw_diurnal_july_20_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_july_20_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_july_20_bh_2020$interval)


str(results_df_gsw_diurnal_july_20_bh_2020)
results_df_gsw_diurnal_july_20_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_july_20_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_july_20_bh_2020$standard_error, 2),results_df_gsw_diurnal_july_20_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_july_20_bh_2020)

write.csv(results_df_gsw_diurnal_july_20_bh_2020,"data_output/results_df_gsw_diurnal_july_20_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_july_20_BH_2020_diurnal<-
  ggplot(results_df_gsw_diurnal_july_20_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("July 20 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 

ggsave(gsw_vs_time_july_20_BH_2020_diurnal, filename = "figures/gsw_vs_time_july_20_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_july_20_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_july_20_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_july_20_BH_2020_diurnal_with_letters <- gsw_vs_time_july_20_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_july_20_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_july_20_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


####Plot all together DIURNALS HW2 2020####

panel_plot_gas_exchange_time_HW2_2020 <- plot_grid(
  A_vs_time_july_10_BH_2020_diurnal_with_letters,
  A_vs_time_july_12_BH_2020_diurnal_with_letters,
  A_vs_time_july_20_BH_2020_diurnal_with_letters,
  gsw_vs_time_july_10_BH_2020_diurnal_with_letters, 
  gsw_vs_time_july_12_BH_2020_diurnal_with_letters, 
  gsw_vs_time_july_20_BH_2020_diurnal_with_letters, 
  labels =c ("      A Pre-Heatwave","   B Heatwave","      C Post-Heatwave","            D","            E","            F"),
  vjust = 3,
  hjust = -0.5, 
  label_size = 18,
  ncol = 3, 
  nrow = 2, 
  align = "hv",
  axis = "tblr" 
)
ggsave(panel_plot_gas_exchange_time_HW2_2020  , filename = "figures/panel_plot_gas_exchange_time_HW2_2020.pdf", device = cairo_pdf, width = 18, height = 13)

#####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL Aug 13 2020 #####

diurnals_2020_A_vs_time_aug13<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment) %>%
  filter(day == "226")  %>%
  mutate(interval = case_when(
    round == 1 ~ "8/13/2020 5:30",
    round == 2 ~ "8/13/2020 9:00",
    round == 3 ~ "8/13/2020 11:30", 
    round == 4 ~ "8/13/2020 13:30",
    round == 5 ~ "8/13/2020 17:00"
  )) %>%
  filter(!A < -10)


diurnals_2020_A_vs_time_aug13$interval <- format((diurnals_2020_A_vs_time_aug13$interval))
diurnals_2020_A_vs_time_aug13$interval<- as.factor(diurnals_2020_A_vs_time_aug13$interval)

str(diurnals_2020_A_vs_time_aug13$interval)

diurnals_2020_A_vs_time_aug13$interval<-as.character(diurnals_2020_A_vs_time_aug13$interval)

diurnals_2020_A_vs_time_aug13$interval<-format(diurnals_2020_A_vs_time_aug13$interval)

diurnals_2020_A_vs_time_aug13$interval<-as.POSIXct(diurnals_2020_A_vs_time_aug13$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_aug13$interval)


diurnals_2020_A_vs_time_aug13 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_aug13$treatment<- reorder(diurnals_2020_A_vs_time_aug13$treatment, diurnals_2020_A_vs_time_aug13$datetime) 


diurnals_2020_A_vs_time_aug13_anova<-diurnals_2020_A_vs_time_aug13


diurnals_2020_A_vs_time_aug13_anova$treatment <- as.character(diurnals_2020_A_vs_time_aug13_anova$treatment)
str(diurnals_2020_A_vs_time_aug13_anova$treatment)

diurnals_2020_A_vs_time_aug13_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_aug13_anova$treatment <- as.factor(diurnals_2020_A_vs_time_aug13_anova$treatment)

str(diurnals_2020_A_vs_time_aug13_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_aug13_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_aug_13_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_aug13_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_aug_13_bh_2020 <- rbind(interval_results,results_df_A_diurnal_aug_13_bh_2020)
}

results_df_A_diurnal_aug_13_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_aug_13_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_aug_13_bh_2020$interval)


str(results_df_A_diurnal_aug_13_bh_2020)
results_df_A_diurnal_aug_13_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_aug_13_bh_2020$mean, 2), "±", round(results_df_A_diurnal_aug_13_bh_2020$standard_error, 2),results_df_A_diurnal_aug_13_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_aug_13_bh_2020)

write.csv(results_df_A_diurnal_aug_13_bh_2020,"data_output/results_df_A_diurnal_aug_13_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_aug_13_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_aug_13_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("August 13 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave(A_vs_time_aug_13_BH_2020_diurnal, filename = "figures/A_vs_time_aug_13_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_aug_13_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_aug_13_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.2  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.2)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_aug_13_BH_2020_diurnal_with_letters <- A_vs_time_aug_13_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_aug_13_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_aug_13_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


#####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL Aug 19 2020 #####

diurnals_2020_A_vs_time_aug19<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, pixel_number, round, treatment) %>%
  filter(day == "232")  %>%
  mutate(interval = case_when(
    round == 1 ~ "8/19/2020 6:00",
    round == 2 ~ "8/19/2020 9:00",
    round == 3 ~ "8/19/2020 11:30", 
    round == 4 ~ "8/19/2020 13:30",
    round == 5 ~ "8/19/2020 17:00"
  ))


diurnals_2020_A_vs_time_aug19$interval <- format((diurnals_2020_A_vs_time_aug19$interval))
diurnals_2020_A_vs_time_aug19$interval<- as.factor(diurnals_2020_A_vs_time_aug19$interval)

str(diurnals_2020_A_vs_time_aug19$interval)

diurnals_2020_A_vs_time_aug19$interval<-as.character(diurnals_2020_A_vs_time_aug19$interval)

diurnals_2020_A_vs_time_aug19$interval<-format(diurnals_2020_A_vs_time_aug19$interval)

diurnals_2020_A_vs_time_aug19$interval<-as.POSIXct(diurnals_2020_A_vs_time_aug19$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_aug19$interval)


diurnals_2020_A_vs_time_aug19 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_aug19$treatment<- reorder(diurnals_2020_A_vs_time_aug19$treatment, diurnals_2020_A_vs_time_aug19$datetime) 


diurnals_2020_A_vs_time_aug19_anova<-diurnals_2020_A_vs_time_aug19


diurnals_2020_A_vs_time_aug19_anova$treatment <- as.character(diurnals_2020_A_vs_time_aug19_anova$treatment)
str(diurnals_2020_A_vs_time_aug19_anova$treatment)

diurnals_2020_A_vs_time_aug19_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_aug19_anova$treatment <- as.factor(diurnals_2020_A_vs_time_aug19_anova$treatment)

str(diurnals_2020_A_vs_time_aug19_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_aug19_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_aug_19_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_aug19_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_aug_19_bh_2020 <- rbind(interval_results,results_df_A_diurnal_aug_19_bh_2020)
}

results_df_A_diurnal_aug_19_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_aug_19_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_aug_19_bh_2020$interval)


str(results_df_A_diurnal_aug_19_bh_2020)
results_df_A_diurnal_aug_19_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_aug_19_bh_2020$mean, 2), "±", round(results_df_A_diurnal_aug_19_bh_2020$standard_error, 2),results_df_A_diurnal_aug_19_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_aug_19_bh_2020)

write.csv(results_df_A_diurnal_aug_19_bh_2020,"data_output/results_df_A_diurnal_aug_19_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_aug_19_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_aug_19_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("August 19 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(A_vs_time_aug_19_BH_2020_diurnal, filename = "figures/A_vs_time_aug_19_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_aug_19_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_aug_19_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.2)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_aug_19_BH_2020_diurnal_with_letters <- A_vs_time_aug_19_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_aug_19_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_aug_19_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD PHOTOSYNTHESIS DIURNAL Aug 26 2020 #####

diurnals_2020_A_vs_time_aug26<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, A, gsw, pixel_number, round, treatment) %>%
  filter(day == "239") %>%
  mutate(interval = case_when(
    round == 1 ~ "8/26/2020 6:00",
    round == 2 ~ "8/26/2020 8:45",
    round == 3 ~ "8/26/2020 11:15", 
    round == 4 ~ "8/26/2020 14:00",
    round == 5 ~ "8/26/2020 17:00"
  ))


diurnals_2020_A_vs_time_aug26$interval <- format((diurnals_2020_A_vs_time_aug26$interval))
diurnals_2020_A_vs_time_aug26$interval<- as.factor(diurnals_2020_A_vs_time_aug26$interval)

str(diurnals_2020_A_vs_time_aug26$interval)

diurnals_2020_A_vs_time_aug26$interval<-as.character(diurnals_2020_A_vs_time_aug26$interval)

diurnals_2020_A_vs_time_aug26$interval<-format(diurnals_2020_A_vs_time_aug26$interval)

diurnals_2020_A_vs_time_aug26$interval<-as.POSIXct(diurnals_2020_A_vs_time_aug26$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_A_vs_time_aug26$interval)


diurnals_2020_A_vs_time_aug26 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_A_vs_time_aug26$treatment<- reorder(diurnals_2020_A_vs_time_aug26$treatment, diurnals_2020_A_vs_time_aug26$datetime) 


diurnals_2020_A_vs_time_aug26_anova<-diurnals_2020_A_vs_time_aug26


diurnals_2020_A_vs_time_aug26_anova$treatment <- as.character(diurnals_2020_A_vs_time_aug26_anova$treatment)
str(diurnals_2020_A_vs_time_aug26_anova$treatment)

diurnals_2020_A_vs_time_aug26_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_A_vs_time_aug26_anova$treatment <- as.factor(diurnals_2020_A_vs_time_aug26_anova$treatment)

str(diurnals_2020_A_vs_time_aug26_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_A_vs_time_aug26_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_A_diurnal_aug_26_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_A_vs_time_aug26_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(A ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(A ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_A_diurnal_aug_26_bh_2020 <- rbind(interval_results,results_df_A_diurnal_aug_26_bh_2020)
}

results_df_A_diurnal_aug_26_bh_2020$interval <- as.POSIXct(results_df_A_diurnal_aug_26_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_A_diurnal_aug_26_bh_2020$interval)


str(results_df_A_diurnal_aug_26_bh_2020)
results_df_A_diurnal_aug_26_bh_2020$Mean_sem <- paste(round(results_df_A_diurnal_aug_26_bh_2020$mean, 2), "±", round(results_df_A_diurnal_aug_26_bh_2020$standard_error, 2),results_df_A_diurnal_aug_26_bh_2020$letters_ordered.groups)


str(results_df_A_diurnal_aug_26_bh_2020)

write.csv(results_df_A_diurnal_aug_26_bh_2020,"data_output/results_df_A_diurnal_aug_26_bh_2020.csv")

pd<- position_dodge(1400)

A_vs_time_aug_26_BH_2020_diurnal<-
  ggplot(results_df_A_diurnal_aug_26_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("August 26 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 0, color = "darkgrey", size = 0.5, linetype ="dashed") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(A_vs_time_aug_26_BH_2020_diurnal, filename = "figures/A_vs_time_aug_26_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_A_diurnal_aug_26_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_A_diurnal_aug_26_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <-1.5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.2)) 

# Create the plot with letters of significance above the max mean points
A_vs_time_aug_26_BH_2020_diurnal_with_letters <- A_vs_time_aug_26_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-2,22,3), limits = c (-2,22)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(A_vs_time_aug_26_BH_2020_diurnal_with_letters, filename = "figures/A_vs_time_aug_26_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD GSW DIURNAL Aug 13 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_aug13<- diurnals_2020_A_vs_time %>%
  filter(!is.na(gsw)) %>%
  select(datetime, day, gsw, pixel_number, round, treatment) %>%
  filter(day == "226")  %>%
  mutate(interval = case_when(
    round == 1 ~ "8/13/2020 5:30",
    round == 2 ~ "8/13/2020 9:00",
    round == 3 ~ "8/13/2020 11:30", 
    round == 4 ~ "8/13/2020 13:30",
    round == 5 ~ "8/13/2020 17:00"
  )) %>%
  filter(!gsw < 0)

diurnals_2020_gsw_vs_time_aug13$interval <- format((diurnals_2020_gsw_vs_time_aug13$interval))
diurnals_2020_gsw_vs_time_aug13$interval<- as.factor(diurnals_2020_gsw_vs_time_aug13$interval)

str(diurnals_2020_gsw_vs_time_aug13$interval)

diurnals_2020_gsw_vs_time_aug13$interval<-as.character(diurnals_2020_gsw_vs_time_aug13$interval)

diurnals_2020_gsw_vs_time_aug13$interval<-format(diurnals_2020_gsw_vs_time_aug13$interval)

diurnals_2020_gsw_vs_time_aug13$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_aug13$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_aug13$interval)


diurnals_2020_gsw_vs_time_aug13 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_aug13$treatment<- reorder(diurnals_2020_gsw_vs_time_aug13$treatment, diurnals_2020_gsw_vs_time_aug13$datetime) 


diurnals_2020_gsw_vs_time_aug13_anova<-diurnals_2020_gsw_vs_time_aug13


diurnals_2020_gsw_vs_time_aug13_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_aug13_anova$treatment)
str(diurnals_2020_gsw_vs_time_aug13_anova$treatment)

diurnals_2020_gsw_vs_time_aug13_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_aug13_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_aug13_anova$treatment)

str(diurnals_2020_gsw_vs_time_aug13_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_aug13_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_aug_13_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_aug13_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_aug_13_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_aug_13_bh_2020)
}

results_df_gsw_diurnal_aug_13_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_aug_13_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_aug_13_bh_2020$interval)


str(results_df_gsw_diurnal_aug_13_bh_2020)
results_df_gsw_diurnal_aug_13_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_aug_13_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_aug_13_bh_2020$standard_error, 2),results_df_gsw_diurnal_aug_13_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_aug_13_bh_2020)

write.csv(results_df_gsw_diurnal_aug_13_bh_2020,"data_output/results_df_gsw_diurnal_aug_13_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_aug_13_BH_2020_diurnal<-
  ggplot(results_df_gsw_diurnal_aug_13_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("August 13 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")

ggsave(gsw_vs_time_aug_13_BH_2020_diurnal, filename = "figures/gsw_vs_time_aug_13_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_aug_13_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_aug_13_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_aug_13_BH_2020_diurnal_with_letters <- gsw_vs_time_aug_13_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_aug_13_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_aug_13_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)


#####ANOVA AND TUKEYS HSD GSW DIURNAL Aug 19 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_aug19<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, gsw, pixel_number, round, treatment) %>%
  filter(day == "232")  %>%
  mutate(interval = case_when(
    round == 1 ~ "8/19/2020 6:00",
    round == 2 ~ "8/19/2020 9:00",
    round == 3 ~ "8/19/2020 11:30", 
    round == 4 ~ "8/19/2020 13:30",
    round == 5 ~ "8/19/2020 17:00"
  ))

diurnals_2020_gsw_vs_time_aug19$interval <- format((diurnals_2020_gsw_vs_time_aug19$interval))
diurnals_2020_gsw_vs_time_aug19$interval<- as.factor(diurnals_2020_gsw_vs_time_aug19$interval)

str(diurnals_2020_gsw_vs_time_aug19$interval)

diurnals_2020_gsw_vs_time_aug19$interval<-as.character(diurnals_2020_gsw_vs_time_aug19$interval)

diurnals_2020_gsw_vs_time_aug19$interval<-format(diurnals_2020_gsw_vs_time_aug19$interval)

diurnals_2020_gsw_vs_time_aug19$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_aug19$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_aug19$interval)


diurnals_2020_gsw_vs_time_aug19 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_aug19$treatment<- reorder(diurnals_2020_gsw_vs_time_aug19$treatment, diurnals_2020_gsw_vs_time_aug19$datetime) 


diurnals_2020_gsw_vs_time_aug19_anova<-diurnals_2020_gsw_vs_time_aug19


diurnals_2020_gsw_vs_time_aug19_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_aug19_anova$treatment)
str(diurnals_2020_gsw_vs_time_aug19_anova$treatment)

diurnals_2020_gsw_vs_time_aug19_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_aug19_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_aug19_anova$treatment)

str(diurnals_2020_gsw_vs_time_aug19_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_aug19_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_aug_19_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_aug19_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_aug_19_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_aug_19_bh_2020)
}

results_df_gsw_diurnal_aug_19_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_aug_19_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_aug_19_bh_2020$interval)


str(results_df_gsw_diurnal_aug_19_bh_2020)
results_df_gsw_diurnal_aug_19_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_aug_19_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_aug_19_bh_2020$standard_error, 2),results_df_gsw_diurnal_aug_19_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_aug_19_bh_2020)

write.csv(results_df_gsw_diurnal_aug_19_bh_2020,"data_output/results_df_gsw_diurnal_aug_19_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_aug_19_BH_2020_diurnal<-
  ggplot(results_df_gsw_diurnal_aug_19_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("August 19 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(gsw_vs_time_aug_19_BH_2020_diurnal, filename = "figures/gsw_vs_time_aug_19_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_aug_19_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_aug_19_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_aug_19_BH_2020_diurnal_with_letters <- gsw_vs_time_aug_19_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_aug_19_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_aug_19_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

#####ANOVA AND TUKEYS HSD GSW DIURNAL Aug 26 2020 #####

# Sample data frame structure (replace this with your actual data frame)
diurnals_2020_gsw_vs_time_aug26<- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  select(datetime, day, gsw, gsw, pixel_number, round, treatment) %>%
  filter(day == "239") %>%
  mutate(interval = case_when(
    round == 1 ~ "8/26/2020 6:00",
    round == 2 ~ "8/26/2020 8:45",
    round == 3 ~ "8/26/2020 11:15", 
    round == 4 ~ "8/26/2020 14:00",
    round == 5 ~ "8/26/2020 17:00"
  ))

diurnals_2020_gsw_vs_time_aug26$interval <- format((diurnals_2020_gsw_vs_time_aug26$interval))
diurnals_2020_gsw_vs_time_aug26$interval<- as.factor(diurnals_2020_gsw_vs_time_aug26$interval)

str(diurnals_2020_gsw_vs_time_aug26$interval)

diurnals_2020_gsw_vs_time_aug26$interval<-as.character(diurnals_2020_gsw_vs_time_aug26$interval)

diurnals_2020_gsw_vs_time_aug26$interval<-format(diurnals_2020_gsw_vs_time_aug26$interval)

diurnals_2020_gsw_vs_time_aug26$interval<-as.POSIXct(diurnals_2020_gsw_vs_time_aug26$interval, format = "%m/%d/%Y %H:%M")

str(diurnals_2020_gsw_vs_time_aug26$interval)


diurnals_2020_gsw_vs_time_aug26 %>%
  group_by(interval, treatment, round) %>%
  tally()

diurnals_2020_gsw_vs_time_aug26$treatment<- reorder(diurnals_2020_gsw_vs_time_aug26$treatment, diurnals_2020_gsw_vs_time_aug26$datetime) 


diurnals_2020_gsw_vs_time_aug26_anova<-diurnals_2020_gsw_vs_time_aug26


diurnals_2020_gsw_vs_time_aug26_anova$treatment <- as.character(diurnals_2020_gsw_vs_time_aug26_anova$treatment)
str(diurnals_2020_gsw_vs_time_aug26_anova$treatment)

diurnals_2020_gsw_vs_time_aug26_anova%>%
  group_by(interval, treatment)%>%
  tally() 

diurnals_2020_gsw_vs_time_aug26_anova$treatment <- as.factor(diurnals_2020_gsw_vs_time_aug26_anova$treatment)

str(diurnals_2020_gsw_vs_time_aug26_anova$treatment)


# Initialize an empty data frame to store results
intervals <- unique(diurnals_2020_gsw_vs_time_aug26_anova$interval) 
str(intervals)
# Create an empty dataframe to store results

results_df_gsw_diurnal_aug_26_bh_2020<- data.frame()

# Loop over each interval
for (current_interval in intervals) {
  
  # Filter data for the current interval
  data_subset <- diurnals_2020_gsw_vs_time_aug26_anova %>%
    filter(interval == current_interval) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(gsw ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(gsw ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_gsw_diurnal_aug_26_bh_2020 <- rbind(interval_results,results_df_gsw_diurnal_aug_26_bh_2020)
}

results_df_gsw_diurnal_aug_26_bh_2020$interval <- as.POSIXct(results_df_gsw_diurnal_aug_26_bh_2020$interval, format = "%m/%d/%Y %H:%M")

# Check the structure to confirm the conversion
str(results_df_gsw_diurnal_aug_26_bh_2020$interval)


str(results_df_gsw_diurnal_aug_26_bh_2020)
results_df_gsw_diurnal_aug_26_bh_2020$Mean_sem <- paste(round(results_df_gsw_diurnal_aug_26_bh_2020$mean, 2), "±", round(results_df_gsw_diurnal_aug_26_bh_2020$standard_error, 2),results_df_gsw_diurnal_aug_26_bh_2020$letters_ordered.groups)


str(results_df_gsw_diurnal_aug_26_bh_2020)

write.csv(results_df_gsw_diurnal_aug_26_bh_2020,"data_output/results_df_gsw_diurnal_aug_26_bh_2020.csv")

pd<- position_dodge(1400)

gsw_vs_time_aug_26_BH_2020_diurnal<-
  ggplot(results_df_gsw_diurnal_aug_26_bh_2020, aes(x = interval, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 3800, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("August 26 2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif",color ="white")) +
  xlab("Time") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) +
  scale_x_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y  = element_blank())

ggsave(gsw_vs_time_aug_26_BH_2020_diurnal, filename = "figures/gsw_vs_time_aug_26_BH_2020_diurnal.pdf", device = cairo_pdf, width = 8, height = 6)

plot_means <- ggplot(results_df_gsw_diurnal_aug_26_bh_2020, aes(x = interval, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_gsw_diurnal_aug_26_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(interval, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(interval) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
gsw_vs_time_aug_26_BH_2020_diurnal_with_letters <- gsw_vs_time_aug_26_BH_2020_diurnal +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = interval, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (0,0.6)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave(gsw_vs_time_aug_26_BH_2020_diurnal_with_letters, filename = "figures/gsw_vs_time_aug_26_BH_2020_diurnal_with_letters.pdf", device = cairo_pdf, width = 8, height = 7)

####Plot all together DIURNALS HW3 2020####

panel_plot_gas_exchange_time_HW3_2020 <- plot_grid(
  A_vs_time_aug_13_BH_2020_diurnal_with_letters,
  A_vs_time_aug_19_BH_2020_diurnal_with_letters,
  A_vs_time_aug_26_BH_2020_diurnal_with_letters,
  gsw_vs_time_aug_13_BH_2020_diurnal_with_letters, 
  gsw_vs_time_aug_19_BH_2020_diurnal_with_letters, 
  gsw_vs_time_aug_26_BH_2020_diurnal_with_letters, 
  labels =c ("      A Pre-Heatwave","   B Heatwave","      C Post-Heatwave","            D","            E","            F"),
  vjust = 3,
  hjust = -0.5, 
  label_size = 18,
  ncol = 3, 
  nrow = 2, 
  align = "hv",
  axis = "tblr" 
)
ggsave(panel_plot_gas_exchange_time_HW3_2020  , filename = "figures/panel_plot_gas_exchange_time_HW3_2020.pdf", device = cairo_pdf, width = 18, height = 13)

ggsave(panel_plot_gas_exchange_time_HW3_2020  , filename = "figures/panel_plot_gas_exchange_time_HW3_2020.png", width = 18, height = 13)
