library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggpmisc)
library(agricolae)


#####Transpiration and WUE for middays for all 3 years BH#####
####2019 data
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

str(diurnals_2019_F_vs_round)



##### ANOVA AND TUKEYS HSD Midday A 2019#####

BH_middays_2019_A <- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, Rep,BH_Vine,BH_Block, BH_Leaf)

BH_middays_2019_A_anova<-BH_middays_2019_A


BH_middays_2019_A_anova_tally<-BH_middays_2019_A_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_A_anova_tally,"data_output/BH_middays_2019_A_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    filter({
      Q1 <- quantile(A, 0.25, na.rm = TRUE)
      Q3 <- quantile(A, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      A >= lower_bound & A <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}
  
BH_middays_2019_A_anova <- BH_middays_2019_A_anova %>%
    group_by(treatment, date) %>%  # Grouping by treatment and date
    do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
    ungroup()  # Ungroup the data frame
  
  
BH_middays_2019_A_tally_no_outliers<-BH_middays_2019_A_anova%>%
    group_by(date, treatment)%>%
    tally() 

write.csv(BH_middays_2019_A_tally_no_outliers,"data_output/BH_middays_2019_A_tally_no_outliers.csv")


BH_middays_2019_A$treatment <- as.character(BH_middays_2019_A$treatment)
str(BH_middays_2019_A$treatment)

BH_middays_2019_A_anova_tally<-BH_middays_2019_A_anova%>%
  group_by(date, treatment)%>%
  tally() 

BH_middays_2019_A_anova$treatment <- as.factor(BH_middays_2019_A_anova$treatment)

str(BH_middays_2019_A_anova$treatment)
str(BH_middays_2019_A_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2019_A_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2019_A<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2019_A_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment/BH_Vine, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2019_A <- rbind(date_results,results_df_BH_middays_2019_A)
}

results_df_BH_middays_2019_A$date <- as.Date(results_df_BH_middays_2019_A$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2019_A$date)


str(results_df_BH_middays_2019_A)
results_df_BH_middays_2019_A$Mean_sem <- paste(round(results_df_BH_middays_2019_A$mean, 2), "±", round(results_df_BH_middays_2019_A$standard_error, 2),results_df_BH_middays_2019_A$letters_ordered.groups)


str(results_df_BH_middays_2019_A)

write.csv(results_df_BH_middays_2019_A,"data_output/results_df_BH_middays_2019_A.csv")

pd<- position_dodge(0.1)

BH_middays_2019_A_plot<-
  ggplot(results_df_BH_middays_2019_A, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(4,20,3), limits = c (4,21)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-07-01"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-07-01", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 4, ymax = 21,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 4, ymax = 21,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 21, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 21, label ="HW2", size = 7) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
    axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave("figures/BH_middays_2019_A_plot.jpg", plot =BH_middays_2019_A_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_A, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_A %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.6  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.2))

# Create the plot with letters of significance above the max mean points
BH_middays_2019_A_plot_with_letters <- BH_middays_2019_A_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(4,20,3), limits = c (4,21))
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2019_A_plot_with_letters.jpg", plot =BH_middays_2019_A_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday GSW 2019#####

BH_middays_2019_gsw <- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, Rep,BH_Vine,BH_Block, BH_Leaf)

BH_middays_2019_gsw_anova<-BH_middays_2019_gsw


BH_middays_2019_gsw_anova_tally<-BH_middays_2019_gsw_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_gsw_anova_tally,"data_output/BH_middays_2019_gsw_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    filter({
      Q1 <- quantile(gsw, 0.25, na.rm = TRUE)
      Q3 <- quantile(gsw, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      gsw >= lower_bound & gsw <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

BH_middays_2019_gsw_anova <- BH_middays_2019_gsw_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2019_gsw_tally_no_outliers<-BH_middays_2019_gsw_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_gsw_tally_no_outliers,"data_output/BH_middays_2019_gsw_tally_no_outliers.csv")

BH_middays_2019_gsw_anova$treatment <- as.factor(BH_middays_2019_gsw_anova$treatment)

str(BH_middays_2019_gsw_anova$treatment)
str(BH_middays_2019_gsw_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2019_gsw_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2019_gsw<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2019_gsw_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment/BH_Vine, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2019_gsw <- rbind(date_results,results_df_BH_middays_2019_gsw)
}

results_df_BH_middays_2019_gsw$date <- as.Date(results_df_BH_middays_2019_gsw$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2019_gsw$date)


str(results_df_BH_middays_2019_gsw)
results_df_BH_middays_2019_gsw$Mean_sem <- paste(round(results_df_BH_middays_2019_gsw$mean, 2), "±", round(results_df_BH_middays_2019_gsw$standard_error, 2),results_df_BH_middays_2019_gsw$letters_ordered.groups)


str(results_df_BH_middays_2019_gsw)

write.csv(results_df_BH_middays_2019_gsw,"data_output/results_df_BH_middays_2019_gsw.csv")

pd<- position_dodge(0.1)

BH_middays_2019_gsw_plot<-
  ggplot(results_df_BH_middays_2019_gsw, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
#  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.45,0.1), limits = c (0,0.45)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-07-01"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-07-01", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 0, ymax = 0.45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 0, ymax = 0.45,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 0.45, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 0.45, label ="HW2", size = 7) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave("figures/BH_middays_2019_gsw_plot.jpg", plot =BH_middays_2019_gsw_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_gsw, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_gsw %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.017 # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2019_gsw_plot_with_letters <- BH_middays_2019_gsw_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.45,0.1), limits = c (0,0.45))
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2019_gsw_plot_with_letters.jpg", plot =BH_middays_2019_gsw_plot_with_letters, width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday instantaneous WUE 2019 #####

BH_middays_2019_WUE <- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / (E))) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, Rep, BH_Vine, BH_Block, BH_Leaf)
#%>%
#  filter(!date == "2019-07-01")

BH_middays_2019_WUE_anova<-BH_middays_2019_WUE



BH_middays_2019_WUE_anova_tally<-BH_middays_2019_WUE_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_WUE_anova_tally,"data_output/BH_middays_2019_WUE_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2019_WUE_anova <- BH_middays_2019_WUE_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2019_WUE_tally_no_outliers<-BH_middays_2019_WUE_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_WUE_tally_no_outliers, "data_output/BH_middays_2019_WUE_tally_no_outliers.csv")
BH_middays_2019_WUE_anova$treatment <- as.factor(BH_middays_2019_WUE_anova$treatment)

str(BH_middays_2019_WUE_anova$treatment)
str(BH_middays_2019_WUE_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2019_WUE_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2019_WUE<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2019_WUE_anova %>%
    filter(date == current_date) 
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE,
    standard_error = standard_errors$WUE,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2019_WUE <- rbind(date_results,results_df_BH_middays_2019_WUE)
}

results_df_BH_middays_2019_WUE$date <- as.Date(results_df_BH_middays_2019_WUE$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2019_WUE$date)


str(results_df_BH_middays_2019_WUE)
results_df_BH_middays_2019_WUE$Mean_sem <- paste(round(results_df_BH_middays_2019_WUE$mean, 2), "±", round(results_df_BH_middays_2019_WUE$standard_error, 2),results_df_BH_middays_2019_WUE$letters_ordered.groups)


str(results_df_BH_middays_2019_WUE)

write.csv(results_df_BH_middays_2019_WUE,"data_output/results_df_BH_middays_2019_WUE.csv")

pd<- position_dodge(0.1)

BH_middays_2019_WUE_plot<-
  ggplot(results_df_BH_middays_2019_WUE, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/E ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
#  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(500,5000,1000), limits = c (500,5000)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-07-01"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-07-01", "2019-09-16")))+
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 5000, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 5000, label ="HW2", size = 7) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
 theme(legend.position = "none")

ggsave("figures/BH_middays_2019_WUE_plot.jpg", plot =BH_middays_2019_WUE_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_WUE, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_WUE %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 200  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2019_WUE_plot_with_letters <- BH_middays_2019_WUE_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(500,5000,1000), limits = c (500,5000))
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2019_WUE_plot_with_letters.jpg", plot =BH_middays_2019_WUE_plot_with_letters, width = 14, height = 8, dpi = 600)

#####remove July 1######

str(results_df_BH_middays_2019_WUE)
results_df_BH_middays_2019_WUE_july_1_removed<-results_df_BH_middays_2019_WUE%>%
  filter(date !="2019-07-01")

BH_middays_2019_WUE_plot<-
  ggplot(results_df_BH_middays_2019_WUE_july_1_removed, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("WUE") +
  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,4000,1000), limits = c (0,4000)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 0, ymax = 4000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 0, ymax = 4000,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 4000, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 4000, label ="HW2", size = 7) 

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_middays_2019_WUE_plot.jpg", plot =BH_middays_2019_WUE_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_WUE, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_WUE %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 200  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2019_WUE_plot_with_letters_July_1_removed <- BH_middays_2019_WUE_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,4000,1000), limits = c (0,4000))
# + theme(legend.position = c(0.2, 0.2))
ggsave("figures/BH_middays_2019_WUE_plot_with_letters_July_1_removed .jpg", plot =BH_middays_2019_WUE_plot_with_letters_July_1_removed , width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday intrinsic WUE 2019 #####

BH_middays_2019_WUE_int <- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  mutate(WUE_int = (A / (gsw))) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE_int, gsw, pixel_number, round, treatment, Rep, BH_Vine, BH_Block, BH_Leaf)
#%>%
#  filter(!date == "2019-07-01")

BH_middays_2019_WUE_int_anova<-BH_middays_2019_WUE_int


BH_middays_2019_WUE_int$treatment <- as.character(BH_middays_2019_WUE_int$treatment)
str(BH_middays_2019_WUE_int$treatment)

BH_middays_2019_WUE_int_anova_tally<-BH_middays_2019_WUE_int_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_WUE_int_anova_tally,"data_output/BH_middays_2019_WUE_int_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2019_WUE_int_anova <- BH_middays_2019_WUE_int_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2019_WUE_int_tally_no_outliers<-BH_middays_2019_WUE_int_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_WUE_int_tally_no_outliers,"data_output/BH_middays_2019_WUE_int_tally_no_outliers.csv")
BH_middays_2019_WUE_int_anova$treatment <- as.factor(BH_middays_2019_WUE_int_anova$treatment)

str(BH_middays_2019_WUE_int_anova$treatment)
str(BH_middays_2019_WUE_int_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2019_WUE_int_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2019_WUE_int<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2019_WUE_int_anova %>%
    filter(date == current_date) 
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE_int,
    standard_error = standard_errors$WUE_int,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2019_WUE_int <- rbind(date_results,results_df_BH_middays_2019_WUE_int)
}

results_df_BH_middays_2019_WUE_int$date <- as.Date(results_df_BH_middays_2019_WUE_int$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2019_WUE_int$date)


str(results_df_BH_middays_2019_WUE_int)
results_df_BH_middays_2019_WUE_int$Mean_sem <- paste(round(results_df_BH_middays_2019_WUE_int$mean, 2), "±", round(results_df_BH_middays_2019_WUE_int$standard_error, 2),results_df_BH_middays_2019_WUE_int$letters_ordered.groups)


str(results_df_BH_middays_2019_WUE_int)

write.csv(results_df_BH_middays_2019_WUE_int,"data_output/results_df_BH_middays_2019_WUE_int.csv")

pd<- position_dodge(0.1)

BH_middays_2019_WUE_int_plot<-
  ggplot(results_df_BH_middays_2019_WUE_int, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/g[s] ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
  #  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(20,160,40), limits = c (20,160)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-07-01"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-07-01", "2019-09-16")))+
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") ,  ymin = 20, ymax = 160,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 160, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 160, label ="HW2", size = 7) +
  theme(legend.position = "none")

ggsave("figures/BH_middays_2019_WUE_int_plot.jpg", plot =BH_middays_2019_WUE_int_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_WUE_int, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_WUE_int %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.2)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2019_WUE_int_plot_with_letters <- BH_middays_2019_WUE_int_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(20,160,40), limits = c (20,160))
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2019_WUE_int_plot_with_letters.jpg", plot =BH_middays_2019_WUE_int_plot_with_letters, width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday Transpiration 2019#####

BH_middays_2019_E <- diurnals_2019_F_vs_round %>%
  filter(!is.na(A)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, gsw, pixel_number, round, treatment, Rep, BH_Vine, BH_Block,BH_Leaf)

BH_middays_2019_E_anova<-BH_middays_2019_E

BH_middays_2019_E_anova_tally<-BH_middays_2019_E_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_E_anova_tally,"data_output/BH_middays_2019_E_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2019_E_anova <- BH_middays_2019_E_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2019_E_tally_no_outliers<-BH_middays_2019_E_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_E_tally_no_outliers,"data_output/BH_middays_2019_E_tally_no_outliers.csv")

BH_middays_2019_E_anova$treatment <- as.factor(BH_middays_2019_E_anova$treatment)

str(BH_middays_2019_E_anova$treatment)
str(BH_middays_2019_E_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2019_E_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2019_E<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2019_E_anova %>%
    filter(date == current_date) 
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$E,
    standard_error = standard_errors$E,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2019_E <- rbind(date_results,results_df_BH_middays_2019_E)
}

results_df_BH_middays_2019_E$date <- as.Date(results_df_BH_middays_2019_E$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2019_E$date)


str(results_df_BH_middays_2019_E)
results_df_BH_middays_2019_E$Mean_sem <- paste(round(results_df_BH_middays_2019_E$mean, 2), "±", round(results_df_BH_middays_2019_E$standard_error, 2),results_df_BH_middays_2019_E$letters_ordered.groups)


str(results_df_BH_middays_2019_E)

write.csv(results_df_BH_middays_2019_E,"data_output/results_df_BH_middays_2019_E.csv")

pd<- position_dodge(0.1)

BH_middays_2019_E_plot<-
  ggplot(results_df_BH_middays_2019_E, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Transpiration~(mol~m^{-2}~s^{-1}))) +
  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.014,0.003), limits = c (0,0.014)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-07-01"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-07-01", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 0.014, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 0.014, label ="HW2", size = 7) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
       axis.ticks.x = element_blank()) +
  theme(legend.position = "none")

ggsave("figures/BH_middays_2019_E_plot.jpg", plot =BH_middays_2019_E_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_E, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_E %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.0005  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.0))

# Create the plot with letters of significance above the max mean points
BH_middays_2019_E_plot_with_letters <- BH_middays_2019_E_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.014,0.003), limits = c (0,0.014)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2019_E_plot_with_letters.jpg", plot =BH_middays_2019_E_plot_with_letters, width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday LEAF Temperature 2019#####

BH_middays_2019_Tleaf <- diurnals_2019_F_vs_round %>%
  filter(!is.na(Tleaf)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, Tleaf, gsw, pixel_number, round, treatment, Rep)

BH_middays_2019_Tleaf_anova<-BH_middays_2019_Tleaf


BH_middays_2019_Tleaf_anova_tally<-BH_middays_2019_Tleaf_anova%>%
  group_by(date, treatment)%>%
  tally() 

BH_middays_2019_Tleaf_anova$treatment <- as.factor(BH_middays_2019_Tleaf_anova$treatment)

str(BH_middays_2019_Tleaf_anova$treatment)
str(BH_middays_2019_Tleaf_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2019_Tleaf_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2019_Tleaf<- data.frame()

str(BH_middays_2019_Tleaf_anova$Tleaf)
sum(is.na(BH_middays_2019_Tleaf_anova$Tleaf))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2019_Tleaf_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Tleaf ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = "TRUE")
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Tleaf ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Tleaf ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$Tleaf,
    standard_error = standard_errors$Tleaf,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2019_Tleaf <- rbind(date_results,results_df_BH_middays_2019_Tleaf)
}
results_df_BH_middays_2019_Tleaf$date <- as.Date(results_df_BH_middays_2019_Tleaf$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2019_Tleaf$date)


str(results_df_BH_middays_2019_Tleaf)
results_df_BH_middays_2019_Tleaf$Mean_sem <- paste(round(results_df_BH_middays_2019_Tleaf$mean, 2), "±", round(results_df_BH_middays_2019_Tleaf$standard_error, 2),results_df_BH_middays_2019_Tleaf$letters_ordered.groups)


str(results_df_BH_middays_2019_Tleaf)

write.csv(results_df_BH_middays_2019_Tleaf,"data_output/results_df_BH_middays_2019_Tleaf.csv")

pd<- position_dodge(0.1)

BH_middays_2019_Tleaf_plot<-
  ggplot(results_df_BH_middays_2019_Tleaf, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("LI-COR leaf temperature (°C)") +
  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(25,48,3), limits = c (34,48)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 34, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 34, ymax = 48,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 48, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 48, label ="HW2", size = 7) 

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_middays_2019_Tleaf_plot.jpg", plot =BH_middays_2019_Tleaf_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_Tleaf, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_Tleaf %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.4  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 2)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2019_Tleaf_plot_with_letters <- BH_middays_2019_Tleaf_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(34,48,2), limits = c (34,48)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2019_Tleaf_plot_with_letters.jpg", plot =BH_middays_2019_Tleaf_plot_with_letters, width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday LEAF Temperature IRT 2019#####

BH_middays_2019_Tleaf_IRT <- diurnals_2019_F_vs_round %>%
  filter(!is.na(leaf_temp_C)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, leaf_temp_C, gsw, pixel_number, round, treatment, Rep,BH_Vine)

BH_middays_2019_Tleaf_IRT_anova<-BH_middays_2019_Tleaf_IRT

BH_middays_2019_Tleaf_IRT_anova$leaf_temp_C<-as.numeric(BH_middays_2019_Tleaf_IRT_anova$leaf_temp_C)

BH_middays_2019_Tleaf_IRT_anova_tally<-BH_middays_2019_Tleaf_IRT_anova%>%
  group_by(date, treatment)%>%
  tally()

write.csv(BH_middays_2019_Tleaf_IRT_anova_tally,"data_output/BH_middays_2019_Tleaf_IRT_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

str(BH_middays_2019_Tleaf_IRT_anova)
BH_middays_2019_Tleaf_IRT_anova <- BH_middays_2019_Tleaf_IRT_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2019_Tleaf_IRT_anova_tally_no_outliers<-BH_middays_2019_Tleaf_IRT_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2019_Tleaf_IRT_anova_tally_no_outliers,"data_output/BH_middays_2019_Tleaf_IRT_anova_tally_no_outliers.csv")


BH_middays_2019_Tleaf_IRT_anova$treatment <- as.factor(BH_middays_2019_Tleaf_IRT_anova$treatment)

str(BH_middays_2019_Tleaf_IRT_anova$treatment)
str(BH_middays_2019_Tleaf_IRT_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2019_Tleaf_IRT_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2019_Tleaf_IRT<- data.frame()

str(BH_middays_2019_Tleaf_IRT_anova$leaf_temp_C)
BH_middays_2019_Tleaf_IRT_anova$leaf_temp_C<-as.numeric(BH_middays_2019_Tleaf_IRT_anova$leaf_temp_C)
sum(is.na(BH_middays_2019_Tleaf_IRT_anova$leaf_temp_C))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2019_Tleaf_IRT_anova %>%
    filter(date == current_date) 
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_temp_C,
    standard_error = standard_errors$leaf_temp_C,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2019_Tleaf_IRT <- rbind(date_results,results_df_BH_middays_2019_Tleaf_IRT)
}
results_df_BH_middays_2019_Tleaf_IRT$date <- as.Date(results_df_BH_middays_2019_Tleaf_IRT$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2019_Tleaf_IRT$date)


str(results_df_BH_middays_2019_Tleaf_IRT)
results_df_BH_middays_2019_Tleaf_IRT$Mean_sem <- paste(round(results_df_BH_middays_2019_Tleaf_IRT$mean, 2), "±", round(results_df_BH_middays_2019_Tleaf_IRT$standard_error, 2),results_df_BH_middays_2019_Tleaf_IRT$letters_ordered.groups)


str(results_df_BH_middays_2019_Tleaf_IRT)

write.csv(results_df_BH_middays_2019_Tleaf_IRT,"data_output/results_df_BH_middays_2019_Tleaf_IRT.csv")

pd<- position_dodge(0.1)

BH_middays_2019_Tleaf_IRT_plot<-
  ggplot(results_df_BH_middays_2019_Tleaf_IRT, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("IRT leaf temperature (°C)") +
# ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(25,48,4), limits = c (25,48)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-07-01"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-07-01", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 25, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 25, ymax = 48,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 48, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 48, label ="HW2", size = 7) +
  theme(legend.position = "none")

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_middays_2019_Tleaf_IRT_plot.jpg", plot =BH_middays_2019_Tleaf_IRT_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2019_Tleaf_IRT, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2019_Tleaf_IRT %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.1  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.85)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2019_Tleaf_IRT_plot_with_letters <- BH_middays_2019_Tleaf_IRT_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(25,48,4), limits = c (25,48)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2019_Tleaf_IRT_plot_with_letters.jpg", plot =BH_middays_2019_Tleaf_IRT_plot_with_letters, width = 14, height = 8, dpi = 600)
  
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

##### ANOVA AND TUKEYS HSD Midday PHOTSYNTHETIC RATE 2020#####

BH_middays_2020_A <- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)
#%>%
#  filter(!A<0)

BH_middays_2020_A_anova<-BH_middays_2020_A



BH_middays_2020_A_anova_tally<-BH_middays_2020_A_anova%>%
  group_by(date, treatment)%>%
  tally() 


write.csv(BH_middays_2020_A_anova_tally,"data_output/BH_middays_2020_A_anova_tally.csv")


remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    filter({
      Q1 <- quantile(A, 0.25, na.rm = TRUE)
      Q3 <- quantile(A, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      A >= lower_bound & A <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

BH_middays_2020_A_anova <- BH_middays_2020_A_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2020_A_tally_no_outliers<-BH_middays_2020_A_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_A_tally_no_outliers,"data_output/BH_middays_2020_A_tally_no_outliers.csv")
BH_middays_2020_A_anova$treatment <- as.factor(BH_middays_2020_A_anova$treatment)

str(BH_middays_2020_A_anova$treatment)
str(BH_middays_2020_A_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2020_A_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2020_A<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2020_A_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2020_A <- rbind(date_results,results_df_BH_middays_2020_A)
}

results_df_BH_middays_2020_A$date <- as.Date(results_df_BH_middays_2020_A$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2020_A$date)


str(results_df_BH_middays_2020_A)
results_df_BH_middays_2020_A$Mean_sem <- paste(round(results_df_BH_middays_2020_A$mean, 2), "±", round(results_df_BH_middays_2020_A$standard_error, 2),results_df_BH_middays_2020_A$letters_ordered.groups)


str(results_df_BH_middays_2020_A)

write.csv(results_df_BH_middays_2020_A,"data_output/results_df_BH_middays_2020_A.csv")

pd<- position_dodge(0.1)

BH_middays_2020_A_plot<-
  ggplot(results_df_BH_middays_2020_A, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(4,20,3), limits = c (4,21)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 4, ymax = 21,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = 4, ymax = 21,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 4, ymax = 21,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 4, ymax = 21,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 21, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 21, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 21, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 21, label ="HW4", size = 7) + 
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2020_A_plot.jpg", plot =BH_middays_2020_A_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2020_A, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2020_A %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.3  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.35)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2020_A_plot_with_letters <- BH_middays_2020_A_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(4,20,3), limits = c (4,21)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2020_A_plot_with_letters.jpg", plot =BH_middays_2020_A_plot_with_letters, width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday GSW 2020#####

BH_middays_2020_gsw <- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)%>%
  filter(!A<0)

BH_middays_2020_gsw_anova<-BH_middays_2020_gsw



BH_middays_2020_gsw_anova_tally<-BH_middays_2020_gsw_anova%>%
  group_by(date, treatment)%>%
  tally() 


write.csv(BH_middays_2020_gsw_anova_tally,"data_output/BH_middays_2020_gsw_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    filter({
      Q1 <- quantile(gsw, 0.25, na.rm = TRUE)
      Q3 <- quantile(gsw, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      gsw >= lower_bound & gsw <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

BH_middays_2020_gsw_anova <- BH_middays_2020_gsw_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2020_gsw_tally_no_outliers<-BH_middays_2020_gsw_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_gsw_tally_no_outliers,"data_output/BH_middays_2020_gsw_tally_no_outliers.csv")
BH_middays_2020_gsw_anova$treatment <- as.factor(BH_middays_2020_gsw_anova$treatment)

str(BH_middays_2020_gsw_anova$treatment)
str(BH_middays_2020_gsw_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2020_gsw_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2020_gsw<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2020_gsw_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2020_gsw <- rbind(date_results,results_df_BH_middays_2020_gsw)
}

results_df_BH_middays_2020_gsw$date <- as.Date(results_df_BH_middays_2020_gsw$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2020_gsw$date)


str(results_df_BH_middays_2020_gsw)
results_df_BH_middays_2020_gsw$Mean_sem <- paste(round(results_df_BH_middays_2020_gsw$mean, 2), "±", round(results_df_BH_middays_2020_gsw$standard_error, 2),results_df_BH_middays_2020_gsw$letters_ordered.groups)


str(results_df_BH_middays_2020_gsw)

write.csv(results_df_BH_middays_2020_gsw,"data_output/results_df_BH_middays_2020_gsw.csv")

pd<- position_dodge(0.1)

BH_middays_2020_gsw_plot<-
  ggplot(results_df_BH_middays_2020_gsw, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
#  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.45,0.1), limits = c (0,0.45)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 0, ymax = 0.45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = 0, ymax = 0.45,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 0, ymax = 0.45,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 0, ymax = 0.45,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 0.45, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 0.45, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 0.45, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 0.45, label ="HW4", size = 7) + 
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2020_gsw_plot.jpg", plot =BH_middays_2020_gsw_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2020_gsw, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2020_gsw %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.025  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.3)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2020_gsw_plot_with_letters <- BH_middays_2020_gsw_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.45,0.1), limits = c (0,0.45)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2020_gsw_plot_with_letters.jpg", plot =BH_middays_2020_gsw_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday Instantaneous WUE 2020#####

BH_middays_2020_WUE <- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)

BH_middays_2020_WUE_anova<-BH_middays_2020_WUE


BH_middays_2020_WUE$treatment <- as.character(BH_middays_2020_WUE$treatment)
str(BH_middays_2020_WUE$treatment)

BH_middays_2020_WUE_anova_tally<-BH_middays_2020_WUE_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_WUE_anova_tally,"data_output/BH_middays_2020_WUE_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2020_WUE_anova <- BH_middays_2020_WUE_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2020_WUE_tally_no_outliers<-BH_middays_2020_WUE_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_WUE_tally_no_outliers,"data_output/BH_middays_2020_WUE_tally_no_outliers.csv")

BH_middays_2020_WUE_anova$treatment <- as.factor(BH_middays_2020_WUE_anova$treatment)

str(BH_middays_2020_WUE_anova$treatment)
str(BH_middays_2020_WUE_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2020_WUE_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2020_WUE<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2020_WUE_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE,
    standard_error = standard_errors$WUE,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2020_WUE <- rbind(date_results,results_df_BH_middays_2020_WUE)
}

results_df_BH_middays_2020_WUE$date <- as.Date(results_df_BH_middays_2020_WUE$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2020_WUE$date)


str(results_df_BH_middays_2020_WUE)
results_df_BH_middays_2020_WUE$Mean_sem <- paste(round(results_df_BH_middays_2020_WUE$mean, 2), "±", round(results_df_BH_middays_2020_WUE$standard_error, 2),results_df_BH_middays_2020_WUE$letters_ordered.groups)


str(results_df_BH_middays_2020_WUE)

write.csv(results_df_BH_middays_2020_WUE,"data_output/results_df_BH_middays_2020_WUE.csv")

pd<- position_dodge(0.1)

BH_middays_2020_WUE_plot<-
  ggplot(results_df_BH_middays_2020_WUE, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/E ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
#  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(500,5000,1000), limits = c (500,5000)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 5000, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 5000, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 5000, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 5000, label ="HW4", size = 7) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
 theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2020_WUE_plot.jpg", plot =BH_middays_2020_WUE_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2020_WUE, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2020_WUE %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 200  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() --0.8)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2020_WUE_plot_with_letters <- BH_middays_2020_WUE_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(500,5000,1000), limits = c (500,5000)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2020_WUE_plot_with_letters.jpg", plot =BH_middays_2020_WUE_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday Intrinsic WUE 2020#####

BH_middays_2020_WUE_int <- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  mutate(WUE_int = (A / gsw)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, WUE_int, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)

BH_middays_2020_WUE_int_anova<-BH_middays_2020_WUE_int


BH_middays_2020_WUE_int$treatment <- as.character(BH_middays_2020_WUE_int$treatment)
str(BH_middays_2020_WUE_int$treatment)

BH_middays_2020_WUE_int_anova_tally<-BH_middays_2020_WUE_int_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_WUE_int_anova_tally,"data_output/BH_middays_2020_WUE_int_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2020_WUE_int_anova <- BH_middays_2020_WUE_int_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2020_WUE_int_tally_no_outliers<-BH_middays_2020_WUE_int_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_WUE_int_tally_no_outliers,"data_output/BH_middays_2020_WUE_int_tally_no_outliers.csv")

BH_middays_2020_WUE_int_anova$treatment <- as.factor(BH_middays_2020_WUE_int_anova$treatment)

str(BH_middays_2020_WUE_int_anova$treatment)
str(BH_middays_2020_WUE_int_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2020_WUE_int_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2020_WUE_int<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2020_WUE_int_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE_int ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE_int,
    standard_error = standard_errors$WUE_int,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2020_WUE_int <- rbind(date_results,results_df_BH_middays_2020_WUE_int)
}

results_df_BH_middays_2020_WUE_int$date <- as.Date(results_df_BH_middays_2020_WUE_int$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2020_WUE_int$date)


str(results_df_BH_middays_2020_WUE_int)
results_df_BH_middays_2020_WUE_int$Mean_sem <- paste(round(results_df_BH_middays_2020_WUE_int$mean, 2), "±", round(results_df_BH_middays_2020_WUE_int$standard_error, 2),results_df_BH_middays_2020_WUE_int$letters_ordered.groups)


str(results_df_BH_middays_2020_WUE_int)

write.csv(results_df_BH_middays_2020_WUE_int,"data_output/results_df_BH_middays_2020_WUE_int.csv")

pd<- position_dodge(0.1)

BH_middays_2020_WUE_int_plot<-
  ggplot(results_df_BH_middays_2020_WUE_int, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/g[s] ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
  #  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(20,160,40), limits = c (20,160)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 160, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 160, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 160, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 160, label ="HW4", size = 7) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2020_WUE_int_plot.jpg", plot =BH_middays_2020_WUE_int_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2020_WUE_int, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2020_WUE_int %>%
  filter(p_value < 0.05)%>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 6  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.2)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2020_WUE_int_plot_with_letters <- BH_middays_2020_WUE_int_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(20,160,40), limits = c (20,160)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2020_WUE_int_plot_with_letters.jpg", plot =BH_middays_2020_WUE_int_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday Transpiration 2020#####

BH_middays_2020_E <- diurnals_2020_A_vs_time %>%
  filter(!is.na(A)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
  ungroup() %>%
  select(date, day, A, E, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)

BH_middays_2020_E_anova<-BH_middays_2020_E


BH_middays_2020_E_anova_tally<-BH_middays_2020_E_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_E_anova_tally,"data_output/BH_middays_2020_E_anova_tally.csv")


remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2020_E_anova <- BH_middays_2020_E_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2020_E_tally_no_outliers<-BH_middays_2020_E_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_E_tally_no_outliers,"data_output/BH_middays_2020_E_tally_no_outliers.csv")

BH_middays_2020_E_anova$treatment <- as.factor(BH_middays_2020_E_anova$treatment)

str(BH_middays_2020_E_anova$treatment)
str(BH_middays_2020_E_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2020_E_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2020_E<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2020_E_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(E ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$E,
    standard_error = standard_errors$E,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2020_E <- rbind(date_results,results_df_BH_middays_2020_E)
}

results_df_BH_middays_2020_E$date <- as.Date(results_df_BH_middays_2020_E$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2020_E$date)


str(results_df_BH_middays_2020_E)
results_df_BH_middays_2020_E$Mean_sem <- paste(round(results_df_BH_middays_2020_E$mean, 2), "±", round(results_df_BH_middays_2020_E$standard_error, 2),results_df_BH_middays_2020_E$letters_ordered.groups)


str(results_df_BH_middays_2020_E)

write.csv(results_df_BH_middays_2020_E,"data_output/results_df_BH_middays_2020_E.csv")

pd<- position_dodge(0.1)

BH_middays_2020_E_plot<-
  ggplot(results_df_BH_middays_2020_E, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Transpiration~(mol~m^{-2}~s^{-1}))) +
  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.014,0.003), limits = c (0,0.014)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 0.014, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 0.014, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-15", "%Y-%m-%d"), y = 0.014, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 0.014, label ="HW4", size = 7) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
       axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank()) 

ggsave("figures/BH_middays_2020_E_plot.jpg", plot =BH_middays_2020_E_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2020_E, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2020_E %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.0007  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2020_E_plot_with_letters <- BH_middays_2020_E_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.014,0.003), limits = c (0,0.014)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2020_E_plot_with_letters.jpg", plot =BH_middays_2020_E_plot_with_letters, width = 14, height = 8, dpi = 600)



##### ANOVA AND TUKEYS HSD Midday LEAF Temperature 2020#####


BH_middays_2020_Tleaf_test<- diurnals_2020_A_vs_time %>%
  filter(!is.na(Tleaf)) %>%
  group_by(date) %>%
  filter(round ==5)


BH_middays_2020_Tleaf <- diurnals_2020_A_vs_time %>%
  filter(!is.na(Tleaf)) %>%
  group_by(date) %>%
  filter(case_when(
    any(round == 5) ~ round == 5,  # If round 5 exists, use it
    any(round == 3) ~ round == 3,  # Otherwise, use round 3
    any(round == 4) ~ round == 4   # If neither round 5 nor round 3 exists, use round 4
  )) %>%
  ungroup() %>%
  select(date, day, A, E, Tleaf, gsw, pixel_number, round, treatment)


BH_middays_2020_Tleaf_anova<-BH_middays_2020_Tleaf

BH_middays_2020_Tleaf_anova_tally<-BH_middays_2020_Tleaf_anova%>%
  group_by(date, treatment)%>%
  tally() 

BH_middays_2020_Tleaf_anova$treatment <- as.factor(BH_middays_2020_Tleaf_anova$treatment)

str(BH_middays_2020_Tleaf_anova$treatment)
str(BH_middays_2020_Tleaf_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2020_Tleaf_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2020_Tleaf<- data.frame()

str(BH_middays_2020_Tleaf_anova$Tleaf)
sum(is.na(BH_middays_2020_Tleaf_anova$Tleaf))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2020_Tleaf_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Tleaf ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Tleaf ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Tleaf ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Tleaf,
    standard_error = standard_errors$Tleaf,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2020_Tleaf <- rbind(date_results,results_df_BH_middays_2020_Tleaf)
}
results_df_BH_middays_2020_Tleaf$date <- as.Date(results_df_BH_middays_2020_Tleaf$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2020_Tleaf$date)


str(results_df_BH_middays_2020_Tleaf)
results_df_BH_middays_2020_Tleaf$Mean_sem <- paste(round(results_df_BH_middays_2020_Tleaf$mean, 2), "±", round(results_df_BH_middays_2020_Tleaf$standard_error, 2),results_df_BH_middays_2020_Tleaf$letters_ordered.groups)


str(results_df_BH_middays_2020_Tleaf)

write.csv(results_df_BH_middays_2020_Tleaf,"data_output/results_df_BH_middays_2020_Tleaf.csv")

pd<- position_dodge(0.1)

BH_middays_2020_Tleaf_plot<-
  ggplot(results_df_BH_middays_2020_Tleaf, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("LI-COR leaf temperature (°C)") +
  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(34,48,2), limits = c (34,48)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 34, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 34, ymax = 48,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 34, ymax = 48,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 34, ymax = 48,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 48, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 48, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 48, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 48, label ="HW4", size = 7) 

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_middays_2020_Tleaf_plot.jpg", plot =BH_middays_2020_Tleaf_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2020_Tleaf, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2020_Tleaf %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.4  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 2)) 

# Create the plot with letters of significance above the max mean points
BH_latest_round_for_diurnal_2020_Tleaf_plot_with_letters <- BH_middays_2020_Tleaf_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(34,48,2), limits = c (34,48)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_latest_round_for_diurnal_2020_Tleaf_plot_with_letters.jpg", plot =BH_latest_round_for_diurnal_2020_Tleaf_plot_with_letters, width = 14, height = 8, dpi = 600)



##### ANOVA AND TUKEYS HSD Midday LEAF Temperature IRT 2020#####


BH_middays_2020_Tleaf_test<- diurnals_2020_A_vs_time %>%
  filter(!is.na(leaf_temp)) %>%
  group_by(date) %>%
  filter(round ==5)


BH_middays_2020_Tleaf_IRT <- diurnals_2020_A_vs_time %>%
  filter(!is.na(leaf_temp)) %>%
  group_by(date) %>%
  filter(if (any(round == 4)) round == 4 else round == 3) %>%
#  filter(case_when(
#    any(round == 5) ~ round == 5,  # If round 5 exists, use it
#    any(round == 3) ~ round == 3,  # Otherwise, use round 3
#    any(round == 4) ~ round == 4   # If neither round 5 nor round 3 exists, use round 4
#  )) %>%
  ungroup() %>%
  select(date, day, A, E, leaf_temp, gsw, pixel_number, round, treatment,VINE, BLOCK, LEAF)


BH_middays_2020_Tleaf_IRT_anova<-BH_middays_2020_Tleaf_IRT

BH_middays_2020_Tleaf_IRT_anova_tally<-BH_middays_2020_Tleaf_IRT_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_Tleaf_IRT_anova_tally,"data_output/BH_middays_2020_Tleaf_IRT_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2020_Tleaf_IRT_anova <- BH_middays_2020_Tleaf_IRT_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2020_Tleaf_IRT_tally_no_outliers<-BH_middays_2020_Tleaf_IRT_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2020_Tleaf_IRT_tally_no_outliers,"data_output/BH_middays_2020_Tleaf_IRT_tally_no_outliers.csv")


BH_middays_2020_Tleaf_IRT_anova$treatment <- as.factor(BH_middays_2020_Tleaf_IRT_anova$treatment)

str(BH_middays_2020_Tleaf_IRT_anova$treatment)
str(BH_middays_2020_Tleaf_IRT_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2020_Tleaf_IRT_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2020_Tleaf_IRT<- data.frame()

str(BH_middays_2020_Tleaf_IRT_anova$leaf_temp)
sum(is.na(BH_middays_2020_Tleaf_IRT_anova$leaf_temp))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2020_Tleaf_IRT_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_temp ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = TRUE)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_temp ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_temp ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_temp,
    standard_error = standard_errors$leaf_temp,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2020_Tleaf_IRT <- rbind(date_results,results_df_BH_middays_2020_Tleaf_IRT)
}
results_df_BH_middays_2020_Tleaf_IRT$date <- as.Date(results_df_BH_middays_2020_Tleaf_IRT$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2020_Tleaf_IRT$date)


str(results_df_BH_middays_2020_Tleaf_IRT)
results_df_BH_middays_2020_Tleaf_IRT$Mean_sem <- paste(round(results_df_BH_middays_2020_Tleaf_IRT$mean, 2), "±", round(results_df_BH_middays_2020_Tleaf_IRT$standard_error, 2),results_df_BH_middays_2020_Tleaf_IRT$letters_ordered.groups)


str(results_df_BH_middays_2020_Tleaf_IRT)

write.csv(results_df_BH_middays_2020_Tleaf_IRT,"data_output/results_df_BH_middays_2020_Tleaf_IRT.csv")

pd<- position_dodge(0.1)

BH_middays_2020_Tleaf_IRT_plot<-
  ggplot(results_df_BH_middays_2020_Tleaf_IRT, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("IRT leaf temperature (°C)") +
#  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(25,48,4), limits = c (25,48)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16")))+
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 25, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 25, ymax = 48,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 25, ymax = 48,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 25, ymax = 48,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 48, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 48, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 48, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 48, label ="HW4", size = 7) +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
      axis.ticks.y = element_blank()) +
  theme(legend.position = "none")

ggsave("figures/BH_middays_2020_Tleaf_IRT_plot.jpg", plot =BH_middays_2020_Tleaf_IRT_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2020_Tleaf_IRT, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2020_Tleaf_IRT %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.1  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2020_Tleaf_IRT_plot_with_letters <- BH_middays_2020_Tleaf_IRT_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(25,48,4), limits = c (25,48)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2020_Tleaf_IRT_plot_with_letters.jpg", plot =BH_middays_2020_Tleaf_IRT_plot_with_letters, width = 14, height = 8, dpi = 600)

####2021 data 


data_physiology_complete_BH_2021_middays<-read.csv("data_output/data_physiology_complete_BH_2021_middays.csv", header = TRUE)


data_physiology_complete_BH_2021_middays$date<- ymd(data_physiology_complete_BH_2021_middays$date)
str(data_physiology_complete_BH_2021_middays$date)

##### ANOVA AND TUKEYS HSD Midday PHOTOSYNTHETIC RATE 2021#####

BH_middays_2021_A<- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)
#%>%
#  filter(date != "2021-06-04")

BH_middays_2021_A_anova<-BH_middays_2021_A



BH_middays_2021_A_anova_tally<-BH_middays_2021_A_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_A_anova_tally,"data_output/BH_middays_2021_A_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    filter({
      Q1 <- quantile(A, 0.25, na.rm = TRUE)
      Q3 <- quantile(A, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      A >= lower_bound & A <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

BH_middays_2021_A_anova <- BH_middays_2021_A_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2021_A_tally_no_outliers<-BH_middays_2021_A_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_A_tally_no_outliers,"data_output/BH_middays_2021_A_tally_no_outliers.csv")


BH_middays_2021_A_anova$treatment <- as.factor(BH_middays_2021_A_anova$treatment)

str(BH_middays_2021_A_anova$treatment)
str(BH_middays_2021_A_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2021_A_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2021_A<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2021_A_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(A ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$A,
    standard_error = standard_errors$A,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2021_A <- rbind(date_results,results_df_BH_middays_2021_A)
}

results_df_BH_middays_2021_A$date <- as.Date(results_df_BH_middays_2021_A$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2021_A$date)


str(results_df_BH_middays_2021_A)
results_df_BH_middays_2021_A$Mean_sem <- paste(round(results_df_BH_middays_2021_A$mean, 2), "±", round(results_df_BH_middays_2021_A$standard_error, 2),results_df_BH_middays_2021_A$letters_ordered.groups)


str(results_df_BH_middays_2021_A)

write.csv(results_df_BH_middays_2021_A,"data_output/results_df_BH_middays_2021_A.csv")

pd<- position_dodge(0.1)

BH_middays_2021_A_plot<-
  ggplot(results_df_BH_middays_2021_A, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  scale_y_continuous(breaks=seq(4,20,3), limits = c (4,21)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 21, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 21, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 21, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 4, ymax = 21,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") ,  ymin = 4, ymax = 21,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") ,  ymin = 4, ymax = 21,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
    axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2021_A_plot.jpg", plot =BH_middays_2021_A_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2021_A, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2021_A %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 1.1  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2021_A_plot_with_letters <- BH_middays_2021_A_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(4,20,3), limits = c (4,21)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2021_A_plot_with_letters.jpg", plot =BH_middays_2021_A_plot_with_letters, width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday GSW 2021#####

BH_middays_2021_gsw <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)
#%>%
#  filter(date != "2021-06-04")

BH_middays_2021_gsw_anova<-BH_middays_2021_gsw


BH_middays_2021_gsw$treatment <- as.character(BH_middays_2021_gsw$treatment)
str(BH_middays_2021_gsw$treatment)

BH_middays_2021_gsw_anova_tally<-BH_middays_2021_gsw_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_gsw_anova_tally,"data_output/BH_middays_2021_gsw_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    filter({
      Q1 <- quantile(gsw, 0.25, na.rm = TRUE)
      Q3 <- quantile(gsw, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      gsw >= lower_bound & gsw <= upper_bound
    }) %>%
    ungroup()  # Ungroup after filtering
}

BH_middays_2021_gsw_anova <- BH_middays_2021_gsw_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2021_gsw_tally_no_outliers<-BH_middays_2021_gsw_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_gsw_tally_no_outliers,"data_output/BH_middays_2021_gsw_tally_no_outliers.csv")


BH_middays_2021_gsw_anova$treatment <- as.factor(BH_middays_2021_gsw_anova$treatment)

str(BH_middays_2021_gsw_anova$treatment)
str(BH_middays_2021_gsw_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2021_gsw_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2021_gsw<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2021_gsw_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(gsw ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$gsw,
    standard_error = standard_errors$gsw,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2021_gsw <- rbind(date_results,results_df_BH_middays_2021_gsw)
}

results_df_BH_middays_2021_gsw$date <- as.Date(results_df_BH_middays_2021_gsw$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2021_gsw$date)


str(results_df_BH_middays_2021_gsw)
results_df_BH_middays_2021_gsw$Mean_sem <- paste(round(results_df_BH_middays_2021_gsw$mean, 2), "±", round(results_df_BH_middays_2021_gsw$standard_error, 2),results_df_BH_middays_2021_gsw$letters_ordered.groups)


str(results_df_BH_middays_2021_gsw)

write.csv(results_df_BH_middays_2021_gsw,"data_output/results_df_BH_middays_2021_gsw.csv")

pd<- position_dodge(0.1)

BH_middays_2021_gsw_plot<-
  ggplot(results_df_BH_middays_2021_gsw, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
#  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  scale_y_continuous(breaks=seq(0,0.45,0.10), limits = c (0,0.45)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 0.45, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 0.45, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 0.45, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0, ymax = 0.45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") ,  ymin = 0, ymax = 0.45,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") ,  ymin = 0, ymax = 0.45,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2021_gsw_plot.jpg", plot =BH_middays_2021_gsw_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2021_gsw, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2021_gsw %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.9)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2021_gsw_plot_with_letters <- BH_middays_2021_gsw_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.45,0.1), limits = c (0,0.45)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2021_gsw_plot_with_letters.jpg", plot =BH_middays_2021_gsw_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday Instantaneous WUE 2021#####

BH_middays_2021_WUE <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(A)) %>%
  mutate(WUE = (A / E)) %>%
  select(date, day, A, E, WUE, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)%>%
  filter(date != "2021-06-04")

BH_middays_2021_WUE_anova<-BH_middays_2021_WUE


BH_middays_2021_WUE$treatment <- as.character(BH_middays_2021_WUE$treatment)
str(BH_middays_2021_WUE$treatment)

BH_middays_2021_WUE_anova_tally<-BH_middays_2021_WUE_anova%>%
  group_by(date, treatment)%>%
  tally() 


write.csv(BH_middays_2021_WUE_anova_tally,"data_output/BH_middays_2021_WUE_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2021_WUE_anova <- BH_middays_2021_WUE_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2021_WUE_tally_no_outliers<-BH_middays_2021_WUE_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_WUE_tally_no_outliers,"data_output/BH_middays_2021_WUE_tally_no_outliers.csv")


BH_middays_2021_WUE_anova$treatment <- as.factor(BH_middays_2021_WUE_anova$treatment)

str(BH_middays_2021_WUE_anova$treatment)
str(BH_middays_2021_WUE_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2021_WUE_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2021_WUE<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2021_WUE_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE,
    standard_error = standard_errors$WUE,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2021_WUE <- rbind(date_results,results_df_BH_middays_2021_WUE)
}

results_df_BH_middays_2021_WUE$date <- as.Date(results_df_BH_middays_2021_WUE$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2021_WUE$date)


str(results_df_BH_middays_2021_WUE)
results_df_BH_middays_2021_WUE$Mean_sem <- paste(round(results_df_BH_middays_2021_WUE$mean, 2), "±", round(results_df_BH_middays_2021_WUE$standard_error, 2),results_df_BH_middays_2021_WUE$letters_ordered.groups)


str(results_df_BH_middays_2021_WUE)

write.csv(results_df_BH_middays_2021_WUE,"data_output/results_df_BH_middays_2021_WUE.csv")

pd<- position_dodge(0.1)

BH_middays_2021_WUE_plot<-
  ggplot(results_df_BH_middays_2021_WUE, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(A/E ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
#  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  scale_y_continuous(breaks=seq(500,5000,1000), limits = c (500,5000)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 5000, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-11-2021", format= "%m-%d-%Y"), y = 5000, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 5000, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 500, ymax = 5000,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
 theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2021_WUE_plot.jpg", plot =BH_middays_2021_WUE_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2021_WUE, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2021_WUE %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 200  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.48)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2021_WUE_plot_with_letters <- BH_middays_2021_WUE_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(500,5000,1000), limits = c (500,5000)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2021_WUE_plot_with_letters.jpg", plot =BH_middays_2021_WUE_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday Intrinsic WUE 2021#####

BH_middays_2021_WUE_int <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(A)) %>%
  mutate(WUE_int = (A / gsw)) %>%
  select(date, day, A, E, WUE_int, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)%>%
  filter(date != "2021-06-04")

BH_middays_2021_WUE_int_anova<-BH_middays_2021_WUE_int


BH_middays_2021_WUE_int$treatment <- as.character(BH_middays_2021_WUE_int$treatment)
str(BH_middays_2021_WUE_int$treatment)

BH_middays_2021_WUE_int_anova_tally<-BH_middays_2021_WUE_int_anova%>%
  group_by(date, treatment)%>%
  tally() 


write.csv(BH_middays_2021_WUE_int_anova_tally,"data_output/BH_middays_2021_WUE_int_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2021_WUE_int_anova <- BH_middays_2021_WUE_int_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2021_WUE_int_tally_no_outliers<-BH_middays_2021_WUE_int_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_WUE_int_tally_no_outliers,"data_output/BH_middays_2021_WUE_int_tally_no_outliers.csv")


BH_middays_2021_WUE_int_anova$treatment <- as.factor(BH_middays_2021_WUE_int_anova$treatment)

str(BH_middays_2021_WUE_int_anova$treatment)
str(BH_middays_2021_WUE_int_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2021_WUE_int_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2021_WUE_int<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2021_WUE_int_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(WUE_int ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$WUE_int,
    standard_error = standard_errors$WUE_int,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2021_WUE_int <- rbind(date_results,results_df_BH_middays_2021_WUE_int)
}

results_df_BH_middays_2021_WUE_int$date <- as.Date(results_df_BH_middays_2021_WUE_int$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2021_WUE_int$date)


str(results_df_BH_middays_2021_WUE_int)
results_df_BH_middays_2021_WUE_int$Mean_sem <- paste(round(results_df_BH_middays_2021_WUE_int$mean, 2), "±", round(results_df_BH_middays_2021_WUE_int$standard_error, 2),results_df_BH_middays_2021_WUE_int$letters_ordered.groups)


str(results_df_BH_middays_2021_WUE_int)

write.csv(results_df_BH_middays_2021_WUE_int,"data_output/results_df_BH_middays_2021_WUE_int.csv")

pd<- position_dodge(0.1)

BH_middays_2021_WUE_int_plot<-
  ggplot(results_df_BH_middays_2021_WUE_int, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "90-120% ET", "120-180% ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "90-120% ET", "120-180% ET"))+
  ylab(expression(A/E ~ (mu * mol ~ CO[2] ~ mol^{-1} ~ H[2]*O)))+
  #  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  scale_y_continuous(breaks=seq(20,160,40), limits = c (20,160)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 160, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 160, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 160, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 20, ymax = 160,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2021_WUE_int_plot.jpg", plot =BH_middays_2021_WUE_int_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2021_WUE_int, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2021_WUE_int %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.48)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2021_WUE_int_plot_with_letters <- BH_middays_2021_WUE_int_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(20,160,40), limits = c (20,160)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2021_WUE_int_plot_with_letters.jpg", plot =BH_middays_2021_WUE_int_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday Transpiration 2021#####

BH_middays_2021_E <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(A)) %>%
  ungroup() %>%
  select(date, day, A, E, gsw, pixel_number, round, treatment, LEAF, VINE, BLOCK)

BH_middays_2021_E_anova<-BH_middays_2021_E


BH_middays_2021_E_anova_tally<-BH_middays_2021_E_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_E_anova_tally,"data_output/BH_middays_2021_E_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2021_E_anova <- BH_middays_2021_E_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2021_E_tally_no_outliers<-BH_middays_2021_E_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_E_tally_no_outliers,"data_output/BH_middays_2021_E_tally_no_outliers.csv")




BH_middays_2021_E_anova$treatment <- as.factor(BH_middays_2021_E_anova$treatment)

str(BH_middays_2021_E_anova$treatment)
str(BH_middays_2021_E_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2021_E_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2021_E<- data.frame()

# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2021_E_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(E ~ treatment/VINE, data = data_subset)
  
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
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$E,
    standard_error = standard_errors$E,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2021_E <- rbind(date_results,results_df_BH_middays_2021_E)
}

results_df_BH_middays_2021_E$date <- as.Date(results_df_BH_middays_2021_E$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2021_E$date)


str(results_df_BH_middays_2021_E)
results_df_BH_middays_2021_E$Mean_sem <- paste(round(results_df_BH_middays_2021_E$mean, 2), "±", round(results_df_BH_middays_2021_E$standard_error, 2),results_df_BH_middays_2021_E$letters_ordered.groups)


str(results_df_BH_middays_2021_E)

write.csv(results_df_BH_middays_2021_E,"data_output/results_df_BH_middays_2021_E.csv")

pd<- position_dodge(0.1)

BH_middays_2021_E_plot<-
  ggplot(results_df_BH_middays_2021_E, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Transpiration~(mol~m^{-2}~s^{-1}))) +
  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0,0.014,0.003), limits = c (0,0.014)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 0.014, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 0.014, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 0.014, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 0, ymax = 0.014,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
 theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
        axis.ticks.y = element_blank())

ggsave("figures/BH_middays_2021_E_plot.jpg", plot =BH_middays_2021_E_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2021_E, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2021_E %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.0009 # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.8)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2021_E_plot_with_letters <- BH_middays_2021_E_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.014,0.003), limits = c (0,0.014)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2021_E_plot_with_letters.jpg", plot =BH_middays_2021_E_plot_with_letters, width = 14, height = 8, dpi = 600)


##### ANOVA AND TUKEYS HSD Midday LEAF Temperature 2021#####

BH_middays_2021_Tleaf <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(Tleaf)) %>%
  select(date, day, A, E, Tleaf, gsw, pixel_number, round, treatment)

BH_middays_2021_Tleaf_anova<-BH_middays_2021_Tleaf




BH_middays_2021_Tleaf_anova_tally<-BH_middays_2021_Tleaf_anova%>%
  group_by(date, treatment)%>%
  tally() 

BH_middays_2021_Tleaf_anova$treatment <- as.factor(BH_middays_2021_Tleaf_anova$treatment)

str(BH_middays_2021_Tleaf_anova$treatment)
str(BH_middays_2021_Tleaf_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2021_Tleaf_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2021_Tleaf<- data.frame()

str(BH_middays_2021_Tleaf_anova$Tleaf)
sum(is.na(BH_middays_2021_Tleaf_anova$Tleaf))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2021_Tleaf_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Tleaf ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = TRUE)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Tleaf ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Tleaf ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Tleaf,
    standard_error = standard_errors$Tleaf,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2021_Tleaf <- rbind(date_results,results_df_BH_middays_2021_Tleaf)
}
results_df_BH_middays_2021_Tleaf$date <- as.Date(results_df_BH_middays_2021_Tleaf$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2021_Tleaf$date)


str(results_df_BH_middays_2021_Tleaf)
results_df_BH_middays_2021_Tleaf$Mean_sem <- paste(round(results_df_BH_middays_2021_Tleaf$mean, 2), "±", round(results_df_BH_middays_2021_Tleaf$standard_error, 2),results_df_BH_middays_2021_Tleaf$letters_ordered.groups)


str(results_df_BH_middays_2021_Tleaf)

write.csv(results_df_BH_middays_2021_Tleaf,"data_output/results_df_BH_middays_2021_Tleaf.csv")

pd<- position_dodge(0.1)

BH_middays_2021_Tleaf_plot<-
  ggplot(results_df_BH_middays_2021_Tleaf, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("LI-COR leaf temperature (°C)") +
  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(34,48,2), limits = c (34,48)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 48, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 48, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 48, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 34, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 34, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 34, ymax = 48,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_middays_2021_Tleaf_plot.jpg", plot =BH_middays_2021_Tleaf_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2021_Tleaf, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2021_Tleaf %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.4  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 2)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2021_Tleaf_plot_with_letters <- BH_middays_2021_Tleaf_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(34,48,2), limits = c (34,48)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2021_Tleaf_plot_with_letters.jpg", plot =BH_middays_2021_Tleaf_plot_with_letters, width = 14, height = 8, dpi = 600)

##### ANOVA AND TUKEYS HSD Midday LEAF IRT Temperature 2021#####

BH_middays_2021_Tleaf_IRT <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(leaf_temp)) %>%
  select(date, day, A, E, leaf_temp, gsw, pixel_number, round, treatment)

BH_middays_2021_Tleaf_IRT_anova<-BH_middays_2021_Tleaf_IRT


BH_middays_2021_Tleaf_IRT_anova_tally<-BH_middays_2021_Tleaf_IRT_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_Tleaf_IRT_anova_tally,"data_output/BH_middays_2021_Tleaf_IRT_anova_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
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

BH_middays_2021_Tleaf_IRT_anova <- BH_middays_2021_Tleaf_IRT_anova %>%
  group_by(treatment, date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


BH_middays_2021_Tleaf_IRT_tally_no_outliers<-BH_middays_2021_Tleaf_IRT_anova%>%
  group_by(date, treatment)%>%
  tally() 

write.csv(BH_middays_2021_Tleaf_IRT_tally_no_outliers,"data_output/BH_middays_2021_Tleaf_IRT_tally_no_outliers.csv")



BH_middays_2021_Tleaf_IRT_anova$treatment <- as.factor(BH_middays_2021_Tleaf_IRT_anova$treatment)

write.csv(BH_middays_2021_Tleaf_IRT_anova_tally,"data_output/BH_middays_2021_Tleaf_IRT_anova_tally.csv")

str(BH_middays_2021_Tleaf_IRT_anova$treatment)
str(BH_middays_2021_Tleaf_IRT_anova$date)

# Initialize an empty data frame to store results
dates <- unique(BH_middays_2021_Tleaf_IRT_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_middays_2021_Tleaf_IRT<- data.frame()

str(BH_middays_2021_Tleaf_IRT_anova$leaf_temp)
sum(is.na(BH_middays_2021_Tleaf_IRT_anova$leaf_temp))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- BH_middays_2021_Tleaf_IRT_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_temp ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = TRUE)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_temp ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_temp ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_temp,
    standard_error = standard_errors$leaf_temp,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_middays_2021_Tleaf_IRT <- rbind(date_results,results_df_BH_middays_2021_Tleaf_IRT)
}
results_df_BH_middays_2021_Tleaf_IRT$date <- as.Date(results_df_BH_middays_2021_Tleaf_IRT$date)

# Check the structure to confirm the conversion
str(results_df_BH_middays_2021_Tleaf_IRT$date)


str(results_df_BH_middays_2021_Tleaf_IRT)
results_df_BH_middays_2021_Tleaf_IRT$Mean_sem <- paste(round(results_df_BH_middays_2021_Tleaf_IRT$mean, 2), "±", round(results_df_BH_middays_2021_Tleaf_IRT$standard_error, 2),results_df_BH_middays_2021_Tleaf_IRT$letters_ordered.groups)


str(results_df_BH_middays_2021_Tleaf_IRT)

write.csv(results_df_BH_middays_2021_Tleaf_IRT,"data_output/results_df_BH_middays_2021_Tleaf_IRT.csv")

pd<- position_dodge(0.1)

BH_middays_2021_Tleaf_IRT_plot<-
  ggplot(results_df_BH_middays_2021_Tleaf_IRT, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("IRT leaf temperature (°C)") +
#  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(25,48,4), limits = c (25,48)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 48, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 48, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 48, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 25, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 25, ymax = 48,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 25, ymax = 48,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"),
   axis.ticks.y = element_blank()) +
  theme(legend.position = "none")

ggsave("figures/BH_middays_2021_Tleaf_IRT_plot.jpg", plot =BH_middays_2021_Tleaf_IRT_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_middays_2021_Tleaf_IRT, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_middays_2021_Tleaf_IRT %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.9  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.6)) 

# Create the plot with letters of significance above the max mean points
BH_middays_2021_Tleaf_IRT_plot_with_letters <- BH_middays_2021_Tleaf_IRT_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(25,48,4), limits = c (25,48)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_middays_2021_Tleaf_IRT_plot_with_letters.jpg", plot =BH_middays_2021_Tleaf_IRT_plot_with_letters, width = 14, height = 8, dpi = 600)


###### ANOVA AND TUKEYS HSD TERMAL DATA CERES FLIGHTS 2021 ####
 
bh_ceres_data_2021<-read.csv("data/BH_ceres_data_2021_v2.csv", header = TRUE)
str(bh_ceres_data_2021)
bh_ceres_data_2021<-bh_ceres_data_2021%>%
  mutate(block_id = case_when(
    pixel_id == "4" ~ "B1R1",
    pixel_id == "13" ~ "B1R1",
    pixel_id == "6" ~ "B1R1",
    pixel_id == "15" ~ "B1R1",
    pixel_id == "14" ~ "B2R1",
    pixel_id == "23" ~ "B2R1",
    pixel_id == "16" ~ "B2R1",
    pixel_id == "25" ~ "B2R1",
    pixel_id == "24" ~ "B2R2",
    pixel_id == "33" ~ "B2R2",
    pixel_id == "26" ~ "B2R2",
    pixel_id == "35" ~ "B2R2",
    pixel_id == "34" ~ "B3R2",
    pixel_id == "43" ~ "B3R2",
    pixel_id == "36" ~ "B3R2",
    pixel_id == "45" ~ "B3R2",
    pixel_id == "44" ~ "B1R3",
    pixel_id == "53" ~ "B1R3",
    pixel_id == "46" ~ "B1R3",
    pixel_id == "55" ~ "B1R3",
    pixel_id == "54" ~ "B3R3",
    pixel_id == "63" ~ "B3R3",
    pixel_id == "56" ~ "B3R3",
    pixel_id == "65" ~ "B3R3",
    pixel_id == "18" ~ "B3R1",
    pixel_id == "27" ~ "B3R1",
    pixel_id == "20" ~ "B3R1",
    pixel_id == "29" ~ "B3R1",
    pixel_id == "47" ~ "B2R3",
    pixel_id == "48" ~ "B2R3",
    pixel_id == "49" ~ "B2R3",
    pixel_id == "50" ~ "B2R3",
    pixel_id == "58" ~ "B1R4",
    pixel_id == "67" ~ "B1R4",
    pixel_id == "60" ~ "B1R4",
    pixel_id == "69" ~ "B1R4",
  ))%>%
  mutate(treatment = case_when(
    block_id == "B1R1" ~ 1,
    block_id == "B1R3" ~ 1,
    block_id == "B1R4" ~ 1, 
    block_id == "B2R1" ~ 2,
    block_id== "B2R2" ~ 2,
    block_id == "B2R3" ~ 2,
    block_id == "B3R1" ~ 3,
    block_id == "B3R2" ~ 3,
    block_id == "B3R3" ~ 3))


bh_ceres_data_2021<-bh_ceres_data_2021%>%
  filter(!is.na(block_id))

bh_ceres_data_2021_tally<-bh_ceres_data_2021%>%
  group_by(ImageDate,treatment)%>%
  tally()


bh_ceres_data_2021$ImageDate<-ymd(bh_ceres_data_2021$ImageDate)
str(bh_ceres_data_2021)

bh_ceres_data_2021_thermal<- bh_ceres_data_2021 %>%
  filter(!is.na(Temperature)) %>%
  select(ImageDate, Temperature, NDVI, block_id, treatment)

bh_ceres_data_2021_thermal_anova<-bh_ceres_data_2021_thermal


bh_ceres_data_2021_thermal_anova$treatment <- as.factor(bh_ceres_data_2021_thermal_anova$treatment)

str(bh_ceres_data_2021_thermal_anova$treatment)
str(bh_ceres_data_2021_thermal_anova$ImageDate)

# Initialize an empty data frame to store results
dates <- unique(bh_ceres_data_2021_thermal_anova$ImageDate) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_ceres_flights_2021<- data.frame()

str(bh_ceres_data_2021_thermal_anova$Temperature)
sum(is.na(bh_ceres_data_2021_thermal_anova$Temperature))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- bh_ceres_data_2021_thermal_anova %>%
    filter(ImageDate == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Temperature ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Temperature ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Temperature ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Temperature,
    standard_error = standard_errors$Temperature,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_ceres_flights_2021 <- rbind(date_results, results_df_BH_ceres_flights_2021)
}
results_df_BH_ceres_flights_2021$date <- as.Date(results_df_BH_ceres_flights_2021$date)

# Check the structure to confirm the conversion
str(results_df_BH_ceres_flights_2021$date)


str(results_df_BH_ceres_flights_2021)
results_df_BH_ceres_flights_2021$Mean_sem <- paste(round(results_df_BH_ceres_flights_2021$mean, 2), "±", round(results_df_BH_ceres_flights_2021$standard_error, 2),results_df_BH_ceres_flights_2021$letters_ordered.groups)


str(results_df_BH_ceres_flights_2021)

write.csv(results_df_BH_ceres_flights_2021,"data_output/results_df_BH_ceres_flights_2021.csv")

pd<- position_dodge(0.1)

BH_ceres_flights_2021_plot<-
  ggplot(results_df_BH_ceres_flights_2021, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("Integrated Soil/Canopy Temperature (°C)") +
  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(25,55,5), limits = c (25,55)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-04-19"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-04-19", "2021-09-16"))) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 55, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 55, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 55, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 25, ymax = 55,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 25, ymax = 55,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 25, ymax = 55,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_ceres_flights_2021_plot.jpg", plot =BH_ceres_flights_2021_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_ceres_flights_2021, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_ceres_flights_2021 %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 2  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 2)) 

# Create the plot with letters of significance above the max mean points
BH_ceres_flights_2021_plot_with_letters <- BH_ceres_flights_2021_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(25,55,5), limits = c (25,55)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_ceres_flights_2021_plot_with_letters.jpg", plot =BH_ceres_flights_2021_plot_with_letters, width = 14, height = 8, dpi = 600)


####NDVI 2021#####

results_df_BH_ceres_flights_2021_ndvi<-NULL

bh_ceres_data_2021_NDVI_anova<-bh_ceres_data_2021_thermal_anova
str(bh_ceres_data_2021_NDVI_anova)
# Loop over each interval
# Loop over each interval


for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- bh_ceres_data_2021_NDVI_anova %>%
    filter(ImageDate == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(NDVI ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(NDVI ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(NDVI ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$NDVI,
    standard_error = standard_errors$NDVI,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_ceres_flights_2021_ndvi <- rbind(date_results, results_df_BH_ceres_flights_2021_ndvi)
}
results_df_BH_ceres_flights_2021_ndvi$date <- as.Date(results_df_BH_ceres_flights_2021_ndvi$date)

# Check the structure to confirm the conversion
str(results_df_BH_ceres_flights_2021_ndvi$date)


str(results_df_BH_ceres_flights_2021_ndvi)
results_df_BH_ceres_flights_2021_ndvi$Mean_sem <- paste(round(results_df_BH_ceres_flights_2021_ndvi$mean, 2), "±", round(results_df_BH_ceres_flights_2021_ndvi$standard_error, 2),results_df_BH_ceres_flights_2021_ndvi$letters_ordered.groups)


str(results_df_BH_ceres_flights_2021)

write.csv(results_df_BH_ceres_flights_2021,"data_output/results_df_BH_ceres_flights_2021.csv")

pd<- position_dodge(0.1)

BH_ceres_flights_2021_plot_ndvi<-
  ggplot(results_df_BH_ceres_flights_2021_ndvi, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("NDVI") +
  ggtitle("2021") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0.3,0.7,0.1), limits = c (0.3,0.7)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-04-19"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-04-19", "2021-09-16"))) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 0.7, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 0.7, label = "HW2", size = 6) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 0.7, label = "HW1", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 0.3, ymax = 0.7,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 0.3, ymax = 0.7,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = 0.3, ymax = 0.7,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_ceres_flights_2021_plot_ndvi.jpg", plot =BH_ceres_flights_2021_plot_ndvi, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_ceres_flights_2021_ndvi, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_ceres_flights_2021_ndvi %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.05  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by date and descending order of significance letters
  group_by(date) %>%
  mutate(
    max_mean = ifelse(all(is.na(mean)), NA, max(mean, na.rm = TRUE)),  # Handle cases where all values are NA
    y_position = max_mean + vertical_offset * (row_number() - 0.5)
  )

# Create the plot with letters of significance above the max mean points
BH_ceres_flights_2021_plot_ndvi_with_letters <- BH_ceres_flights_2021_plot_ndvi +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0.3,0.7,0.1), limits = c (0.3,0.7)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_ceres_flights_2021_plot_with_letters.jpg", plot =BH_ceres_flights_2021_plot_with_letters, width = 14, height = 8, dpi = 600)



###### ANOVA AND TUKEYS HSD TERMAL DATA CERES FLIGHTS 2020 ####

bh_ceres_data_2020<-read.csv("data/BH_ceres_data_2020.csv", header = TRUE)
str(bh_ceres_data_2020)
bh_ceres_data_2020<-bh_ceres_data_2020%>%
  mutate(block_id = case_when(
    pixel_id == "4" ~ "B1R1",
    pixel_id == "13" ~ "B1R1",
    pixel_id == "6" ~ "B1R1",
    pixel_id == "15" ~ "B1R1",
    pixel_id == "14" ~ "B2R1",
    pixel_id == "23" ~ "B2R1",
    pixel_id == "16" ~ "B2R1",
    pixel_id == "25" ~ "B2R1",
    pixel_id == "24" ~ "B2R2",
    pixel_id == "33" ~ "B2R2",
    pixel_id == "26" ~ "B2R2",
    pixel_id == "35" ~ "B2R2",
    pixel_id == "34" ~ "B3R2",
    pixel_id == "43" ~ "B3R2",
    pixel_id == "36" ~ "B3R2",
    pixel_id == "45" ~ "B3R2",
    pixel_id == "44" ~ "B1R3",
    pixel_id == "53" ~ "B1R3",
    pixel_id == "46" ~ "B1R3",
    pixel_id == "55" ~ "B1R3",
    pixel_id == "54" ~ "B3R3",
    pixel_id == "63" ~ "B3R3",
    pixel_id == "56" ~ "B3R3",
    pixel_id == "65" ~ "B3R3",
    pixel_id == "18" ~ "B3R1",
    pixel_id == "27" ~ "B3R1",
    pixel_id == "20" ~ "B3R1",
    pixel_id == "29" ~ "B3R1",
    pixel_id == "47" ~ "B2R3",
    pixel_id == "48" ~ "B2R3",
    pixel_id == "49" ~ "B2R3",
    pixel_id == "50" ~ "B2R3",
    pixel_id == "58" ~ "B1R4",
    pixel_id == "67" ~ "B1R4",
    pixel_id == "60" ~ "B1R4",
    pixel_id == "69" ~ "B1R4",
  ))%>%
  mutate(treatment = case_when(
    block_id == "B1R1" ~ 1,
    block_id == "B1R3" ~ 1,
    block_id == "B1R4" ~ 1, 
    block_id == "B2R1" ~ 2,
    block_id== "B2R2" ~ 2,
    block_id == "B2R3" ~ 2,
    block_id == "B3R1" ~ 3,
    block_id == "B3R2" ~ 3,
    block_id == "B3R3" ~ 3))


bh_ceres_data_2020<-bh_ceres_data_2020%>%
  filter(!is.na(block_id))

bh_ceres_data_2020_tally<-bh_ceres_data_2020%>%
  group_by(ImageDate,treatment)%>%
  tally()


bh_ceres_data_2020$ImageDate<-ymd(bh_ceres_data_2020$ImageDate)
str(bh_ceres_data_2020)

bh_ceres_data_2020_thermal<- bh_ceres_data_2020 %>%
  filter(!is.na(Temperature)) %>%
  select(ImageDate, Temperature, NDVI, block_id, treatment)

bh_ceres_data_2020_thermal_anova<-bh_ceres_data_2020_thermal


bh_ceres_data_2020_thermal_anova$treatment <- as.factor(bh_ceres_data_2020_thermal_anova$treatment)

str(bh_ceres_data_2020_thermal_anova$treatment)
str(bh_ceres_data_2020_thermal_anova$ImageDate)

# Initialize an empty data frame to store results
dates <- unique(bh_ceres_data_2020_thermal_anova$ImageDate) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_ceres_flights_2020<- data.frame()

str(bh_ceres_data_2020_thermal_anova$Temperature)
sum(is.na(bh_ceres_data_2020_thermal_anova$Temperature))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- bh_ceres_data_2020_thermal_anova %>%
    filter(ImageDate == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Temperature ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Temperature ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Temperature ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Temperature,
    standard_error = standard_errors$Temperature,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_ceres_flights_2020 <- rbind(date_results, results_df_BH_ceres_flights_2020)
}
results_df_BH_ceres_flights_2020$date <- as.Date(results_df_BH_ceres_flights_2020$date)

# Check the structure to confirm the conversion
str(results_df_BH_ceres_flights_2020$date)


str(results_df_BH_ceres_flights_2020)
results_df_BH_ceres_flights_2020$Mean_sem <- paste(round(results_df_BH_ceres_flights_2020$mean, 2), "±", round(results_df_BH_ceres_flights_2020$standard_error, 2),results_df_BH_ceres_flights_2020$letters_ordered.groups)


str(results_df_BH_ceres_flights_2020)

write.csv(results_df_BH_ceres_flights_2020,"data_output/results_df_BH_ceres_flights_2020.csv")

pd<- position_dodge(0.1)

BH_ceres_flights_2020_plot<-
  ggplot(results_df_BH_ceres_flights_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("Integrated Soil/Canopy Temperature (°C)") +
  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(25,55,5), limits = c (25,55)) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 25, ymax = 55,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 25, ymax = 55,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 25, ymax = 55,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 25, ymax = 55,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 55, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 55, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 55, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 55, label ="HW4", size = 7) 
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_ceres_flights_2020_plot.jpg", plot =BH_ceres_flights_2020_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_ceres_flights_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_ceres_flights_2020 %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 2  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
BH_ceres_flights_2020_plot_with_letters <- BH_ceres_flights_2020_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(25,55,5), limits = c (25,55)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_ceres_flights_2020_plot_with_letters.jpg", plot =BH_ceres_flights_2020_plot_with_letters, width = 14, height = 8, dpi = 600)

##### NDVI 2020####
results_df_BH_ceres_flights_2020_ndvi<-NULL

bh_ceres_data_2020_NDVI_anova<-bh_ceres_data_2020_thermal_anova
str(bh_ceres_data_2020_NDVI_anova)
# Loop over each interval
# Loop over each interval


for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- bh_ceres_data_2020_NDVI_anova %>%
    filter(ImageDate == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(NDVI ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(NDVI ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(NDVI ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$NDVI,
    standard_error = standard_errors$NDVI,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_ceres_flights_2020_ndvi <- rbind(date_results, results_df_BH_ceres_flights_2020_ndvi)
}
results_df_BH_ceres_flights_2020_ndvi$date <- as.Date(results_df_BH_ceres_flights_2020_ndvi$date)

# Check the structure to confirm the conversion
str(results_df_BH_ceres_flights_2020_ndvi$date)


str(results_df_BH_ceres_flights_2020_ndvi)
results_df_BH_ceres_flights_2020_ndvi$Mean_sem <- paste(round(results_df_BH_ceres_flights_2020_ndvi$mean, 2), "±", round(results_df_BH_ceres_flights_2020_ndvi$standard_error, 2),results_df_BH_ceres_flights_2020_ndvi$letters_ordered.groups)


str(results_df_BH_ceres_flights_2020)

write.csv(results_df_BH_ceres_flights_2020,"data_output/results_df_BH_ceres_flights_2020.csv")

pd<- position_dodge(0.1)

BH_ceres_flights_2020_plot_ndvi<-
  ggplot(results_df_BH_ceres_flights_2020_ndvi, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("NDVI") +
  ggtitle("2020") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0.6,0.8,0.05), limits = c (0.6,0.8)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-04-19"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-04-19", "2020-09-16"))) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = 0.6, ymax = 0.8,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,   ymin = 0.6, ymax = 0.8,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,   ymin = 0.6, ymax = 0.8,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 0.6, ymax = 0.8,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 0.8, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 0.8, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 0.8, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 0.8, label ="HW4", size = 7) 

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_ceres_flights_2020_plot_ndvi.jpg", plot =BH_ceres_flights_2020_plot_ndvi, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_ceres_flights_2020_ndvi, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_ceres_flights_2020_ndvi %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.05  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by date and descending order of significance letters
  group_by(date) %>%
  mutate(
    max_mean = ifelse(all(is.na(mean)), NA, max(mean, na.rm = TRUE)),  # Handle cases where all values are NA
    y_position = max_mean + vertical_offset * (row_number() - 0.5)
  )


# Create the plot with letters of significance above the max mean points
BH_ceres_flights_2020_plot_ndvi_with_letters <- BH_ceres_flights_2020_plot_ndvi +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0.6,0.8,0.05), limits = c (0.6,0.8)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_ceres_flights_2020_plot_ndvi_with_letters.jpg", plot =BH_ceres_flights_2020_plot_ndvi_with_letters, width = 14, height = 8, dpi = 600)




###### ANOVA AND TUKEYS HSD TERMAL DATA CERES FLIGHTS 2019 ####

bh_ceres_data_2019<-read.csv("data/BH_ceres_data_2019.csv", header = TRUE)
str(bh_ceres_data_2019)
bh_ceres_data_2019<-bh_ceres_data_2019%>%
  mutate(block_id = case_when(
    pixel_id == "23" ~ "B2R1",
    pixel_id == "35" ~ "B2R2",
    pixel_id == "45" ~ "B3R2",
    pixel_id == "55" ~ "B1R3",
    pixel_id == "54" ~ "B3R3",
    pixel_id == "27" ~ "B3R1",
    pixel_id == "48" ~ "B2R3",
    pixel_id == "67" ~ "B1R4",
  ))%>%
  mutate(treatment = case_when(
    block_id == "B1R3" ~ 1,
    block_id == "B1R4" ~ 1, 
    block_id == "B2R1" ~ 2,
    block_id== "B2R2" ~ 2,
    block_id == "B2R3" ~ 2,
    block_id == "B3R1" ~ 3,
    block_id == "B3R2" ~ 3,
    block_id == "B3R3" ~ 3))


bh_ceres_data_2019<-bh_ceres_data_2019%>%
  filter(!is.na(block_id))

bh_ceres_data_2019_tally<-bh_ceres_data_2019%>%
  group_by(ImageDate,treatment)%>%
  tally()


bh_ceres_data_2019$ImageDate<-ymd(bh_ceres_data_2019$ImageDate)
str(bh_ceres_data_2019)

bh_ceres_data_2019_thermal<- bh_ceres_data_2019 %>%
  filter(!is.na(Temperature)) %>%
  select(ImageDate, Temperature, NDVI, block_id, treatment)

bh_ceres_data_2019_thermal_anova<-bh_ceres_data_2019_thermal


bh_ceres_data_2019_thermal_anova$treatment <- as.factor(bh_ceres_data_2019_thermal_anova$treatment)

str(bh_ceres_data_2019_thermal_anova$treatment)
str(bh_ceres_data_2019_thermal_anova$ImageDate)

# Initialize an empty data frame to store results
dates <- unique(bh_ceres_data_2019_thermal_anova$ImageDate) 
str(dates)
# Create an empty dataframe to store results

results_df_BH_ceres_flights_2019<- data.frame()

str(bh_ceres_data_2019_thermal_anova$Temperature)
sum(is.na(bh_ceres_data_2019_thermal_anova$Temperature))
# Loop over each interval
# Loop over each interval
for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- bh_ceres_data_2019_thermal_anova %>%
    filter(ImageDate == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Temperature ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Temperature ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Temperature ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Temperature,
    standard_error = standard_errors$Temperature,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_ceres_flights_2019 <- rbind(date_results, results_df_BH_ceres_flights_2019)
}
results_df_BH_ceres_flights_2019$date <- as.Date(results_df_BH_ceres_flights_2019$date)

# Check the structure to confirm the conversion
str(results_df_BH_ceres_flights_2019$date)


str(results_df_BH_ceres_flights_2019)
results_df_BH_ceres_flights_2019$Mean_sem <- paste(round(results_df_BH_ceres_flights_2019$mean, 2), "±", round(results_df_BH_ceres_flights_2019$standard_error, 2),results_df_BH_ceres_flights_2019$letters_ordered.groups)


str(results_df_BH_ceres_flights_2019)

write.csv(results_df_BH_ceres_flights_2019,"data_output/results_df_BH_ceres_flights_2019.csv")

pd<- position_dodge(0.1)

BH_ceres_flights_2019_plot<-
  ggplot(results_df_BH_ceres_flights_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("Integrated Soil/Canopy Temperature (°C)") +
  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(20,55,8), limits = c (20,55)) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 20, ymax = 55,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 20, ymax = 55,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 55, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 55, label ="HW2", size = 7)
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_ceres_flights_2019_plot.jpg", plot =BH_ceres_flights_2019_plot, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_ceres_flights_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_ceres_flights_2019 %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 2  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points
BH_ceres_flights_2019_plot_with_letters <- BH_ceres_flights_2019_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(20,55,8), limits = c (20,55)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_ceres_flights_2019_plot_with_letters.jpg", plot =BH_ceres_flights_2019_plot_with_letters, width = 14, height = 8, dpi = 600)

##### NDVI 2019####
results_df_BH_ceres_flights_2019_ndvi<-NULL

bh_ceres_data_2019_NDVI_anova<-bh_ceres_data_2019_thermal_anova
str(bh_ceres_data_2019_NDVI_anova)
# Loop over each interval
# Loop over each interval


for (current_date in dates) {
  
  # Filter data for the current interval
  data_subset <- bh_ceres_data_2019_NDVI_anova %>%
    filter(ImageDate == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(NDVI ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(NDVI ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(NDVI ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$NDVI,
    standard_error = standard_errors$NDVI,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current interval to the main results dataframe
  results_df_BH_ceres_flights_2019_ndvi <- rbind(date_results, results_df_BH_ceres_flights_2019_ndvi)
}
results_df_BH_ceres_flights_2019_ndvi$date <- as.Date(results_df_BH_ceres_flights_2019_ndvi$date)

# Check the structure to confirm the conversion
str(results_df_BH_ceres_flights_2019_ndvi$date)


str(results_df_BH_ceres_flights_2019_ndvi)
results_df_BH_ceres_flights_2019_ndvi$Mean_sem <- paste(round(results_df_BH_ceres_flights_2019_ndvi$mean, 2), "±", round(results_df_BH_ceres_flights_2019_ndvi$standard_error, 2),results_df_BH_ceres_flights_2019_ndvi$letters_ordered.groups)


str(results_df_BH_ceres_flights_2019)

write.csv(results_df_BH_ceres_flights_2019,"data_output/results_df_BH_ceres_flights_2019.csv")

pd<- position_dodge(0.1)

BH_ceres_flights_2019_plot_ndvi<-
  ggplot(results_df_BH_ceres_flights_2019_ndvi, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1), size =1.1, width = 10, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("NDVI") +
  ggtitle("2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(0.4,0.8,0.1), limits = c (0.4,0.8)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-04-19"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-04-19", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = 0.4, ymax = 0.8,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = 0.4, ymax = 0.8,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 0.8, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 0.8, label ="HW2", size = 7)

#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +
#  theme(legend.position = "none")

ggsave("figures/BH_ceres_flights_2019_plot_ndvi.jpg", plot =BH_ceres_flights_2019_plot_ndvi, width = 10, height = 6, dpi = 600)

plot_means <- ggplot(results_df_BH_ceres_flights_2019_ndvi, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_BH_ceres_flights_2019_ndvi %>%
  filter(p_value < 0.05) %>%
  group_by(date) %>%
  filter(n_distinct(letters_ordered.groups) > 1) %>%
  ungroup()
# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.03  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by date and descending order of significance letters
  group_by(date) %>%
  mutate(
    max_mean = ifelse(all(is.na(mean)), NA, max(mean, na.rm = TRUE)),  # Handle cases where all values are NA
    y_position = max_mean + vertical_offset * (row_number() - 1)
  )


# Create the plot with letters of significance above the max mean points
BH_ceres_flights_2019_plot_ndvi_with_letters <- BH_ceres_flights_2019_plot_ndvi +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 5, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0.4,0.8,0.1), limits = c (0.4,0.8)) 
# + theme(legend.position = c(0.2, 0.2))


ggsave("figures/BH_ceres_flights_2019_plot_ndvi_with_letters.jpg", plot =BH_ceres_flights_2019_plot_ndvi_with_letters, width = 14, height = 8, dpi = 600)


##### PRE-HW , HWs and POST HWS TEMPERATURE RESPONSE CURVES #####

#### A vs LICOR leaf T PRE HWs  2019 round 3-4 ####

diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4<- diurnals_2019_F_vs_round %>%
  filter(!is.na(Tleaf)) %>%
  select( date,day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(date == "2019-07-25") %>%
  filter(round > 2) %>%
  filter(round < 5)

diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4%>%
  group_by(treatment)%>%
  tally()


str(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$Tleaf)


diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment<- reorder(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment, diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$Tleaf)

str(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$Rep<-format(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$Rep)
as.character(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$Rep)

diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment)

str(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4$treatment)


## Plot first HW RESPONSE


A_vs_leafT_licor_PREHWs_2019_r_3_4 <-ggplot(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("Pre-HWS 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(35,48,3), limits = c (35,48)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (-1,30))  +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
# theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +


write.csv(diurnals_2019_A_vs_leafT_licor_PREHW_r_3_4,"data_output/A_vs_leafT_licor_PREHWs_2019_r_3_4.csv")


ggsave(A_vs_leafT_licor_PREHWs_2019_r_3_4, filename = "figures/A_vs_leafT_licor_PREHWs_r_3_4.jpg", 
       width = 8, height = 6, dpi =600)


#### A vs LICOR leaf T HWS 2019 round 3-4 ####

str(diurnals_2019_F_vs_round)
print(unique(diurnals_2019_F_vs_round$date))

diurnals_2019_A_vs_leafT_licor_HWS_r_3_4 <- diurnals_2019_F_vs_round %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine, datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2019-07-28") | date == as.Date("2019-08-15")) %>%
  filter(round > 2 & round < 5)



diurnals_2019_A_vs_leafT_licor_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$Tleaf)


diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment<- reorder(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment, diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$Tleaf)

str(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$Rep<-format(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$Rep)
as.character(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$Rep)

diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment)

str(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_HWS_2019_r_3_4 <-ggplot(diurnals_2019_A_vs_leafT_licor_HWS_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("HWS 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(35,48,3), limits = c (35,48)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(A_vs_leafT_licor_HWS_2019_r_3_4, filename = "figures/A_vs_leafT_licor_HWS_2019_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


#### A vs LICOR leaf T POST-HWS 2019 round 3-4 ####

str(diurnals_2019_F_vs_round)
print(unique(diurnals_2019_F_vs_round$date))

diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4 <- diurnals_2019_F_vs_round %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine, datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2019-08-01") | date == as.Date("2019-08-20")) %>%
  filter(round > 2 & round < 5)



diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)


diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<- reorder(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment, diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)

str(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$Rep<-format(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$Rep)
as.character(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$Rep)

diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

str(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_post_HWS_2019_r_3_4 <-ggplot(diurnals_2019_A_vs_leafT_licor__POST_HWS_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("POST HWS 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(28,41,3), limits = c (28,41)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(A_vs_leafT_licor_post_HWS_2019_r_3_4, filename = "figures/A_vs_leafT_licor_post_HWS_2019_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)




#### GSW vs LICOR leaf T PRE HWs  2019 round 3-4 ####

diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4<- diurnals_2019_F_vs_round %>%
  filter(!is.na(Tleaf)) %>%
  select( date,day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(date == "2019-07-25") %>%
  filter(round > 2) %>%
  filter(round < 5)

diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4%>%
  group_by(treatment)%>%
  tally()


str(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$Tleaf)


diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<- reorder(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment, diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$Tleaf)

str(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$Rep<-format(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$Rep)
as.character(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$Rep)

diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

str(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)


## Plot first HW RESPONSE


gsw_vs_leafT_licor_PREHWs_2019_r_3_4 <-ggplot(diurnals_2019_gsw_vs_leafT_licor_PREHW_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("Pre-HWS 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(35,48,3), limits = c (35,48)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
# theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +


ggsave(gsw_vs_leafT_licor_PREHWs_2019_r_3_4, filename = "figures/gsw_vs_leafT_licor_PREHWs_r_3_4.jpg", 
       width = 8, height = 6, dpi =600)


#### GSW vs LICOR leaf T HWS 2019 round 3-4 ####

str(diurnals_2019_F_vs_round)
print(unique(diurnals_2019_F_vs_round$date))

diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4 <- diurnals_2019_F_vs_round %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine, datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2019-07-28") | date == as.Date("2019-08-15")) %>%
  filter(round > 2 & round < 5)



diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$Tleaf)


diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment<- reorder(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment, diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$Tleaf)

str(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$Rep<-format(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$Rep)
as.character(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$Rep)

diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

str(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_HWS_2019_r_3_4 <-ggplot(diurnals_2019_gsw_vs_leafT_licor_HWS_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("HWS 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(35,48,3), limits = c (35,48)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(gsw_vs_leafT_licor_HWS_2019_r_3_4, filename = "figures/gsw_vs_leafT_licor_HWS_2019_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


#### GSW vs LICOR leaf T POST-HWS 2019 round 3-4 ####

str(diurnals_2019_F_vs_round)
print(unique(diurnals_2019_F_vs_round$date))

diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4 <- diurnals_2019_F_vs_round %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine, datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2019-08-01") | date == as.Date("2019-08-20")) %>%
  filter(round > 2 & round < 5)



diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)


diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<- reorder(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment, diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)

str(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$Rep<-format(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$Rep)
as.character(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$Rep)

diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

str(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_post_HWS_2019_r_3_4 <-ggplot(diurnals_2019_gsw_vs_leafT_licor__POST_HWS_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("POST HWS 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(28,41,3), limits = c (28,41)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(gsw_vs_leafT_licor_post_HWS_2019_r_3_4, filename = "figures/gsw_vs_leafT_licor_post_HWS_2019_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


library(cowplot)


panel_plot_A_and_gsw_vs_tleaf_HWS_2019 <- plot_grid (A_vs_leafT_licor_PREHWs_2019_r_3_4,A_vs_leafT_licor_HWS_2019_r_3_4,A_vs_leafT_licor_post_HWS_2019_r_3_4,gsw_vs_leafT_licor_PREHWs_2019_r_3_4,gsw_vs_leafT_licor_HWS_2019_r_3_4,gsw_vs_leafT_licor_post_HWS_2019_r_3_4,
                                                     labels =c ("A","B","C","D"," E","  F"),vjust = 4,
                                                     hjust = -6, 
                                                     label_size = 18,
                                                     ncol = 3, 
                                                     nrow = 2, 
                                                     align = "hv",
                                                     axis = "tblr" )


ggsave(panel_plot_A_and_gsw_vs_tleaf_HWS_2019 , filename = "figures/panel_plot_A_and_gsw_vs_tleaf_HWS_2019.jpg", width = 20, height = 12, dpi=600)



#### A vs LICOR leaf T PRE HWs  2020 round 3-4 ####

diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4<- diurnals_2020_A_vs_time %>%
  filter(!is.na(Tleaf)) %>%
  select( date,day, Tleaf, A, pixel_number, round, treatment, LEAF, BLOCK, VINE) %>%
  filter(date == "2020-07-10"|date =="2020-08-13") %>%
  filter(round > 2) %>%
  filter(round < 5)%>%
  filter(!A < 0)

diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4%>%
  group_by(treatment)%>%
  tally()


str(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$Tleaf)


diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment<- reorder(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment, diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$Tleaf)

str(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$Rep<-format(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$Rep)
as.character(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$Rep)

diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment)

str(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4$treatment)


## Plot first HW RESPONSE


A_vs_leafT_licor_PREHWs_2020_r_3_4 <-ggplot(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("Pre-HWS 2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(34,46,3), limits = c (34,46)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (-1,30))  +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
# theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +


write.csv(diurnals_2020_A_vs_leafT_licor_PREHW_r_3_4,"data_output/A_vs_leafT_licor_PREHWs_2020_r_3_4.csv")


ggsave(A_vs_leafT_licor_PREHWs_2020_r_3_4, filename = "figures/A_vs_leafT_licor_PREHWs_r_3_4.jpg", 
       width = 8, height = 6, dpi =600)


#### A vs LICOR leaf T HWS 2020 round 3-4 ####


diurnals_2020_A_vs_leafT_licor_HWS_r_3_4 <- diurnals_2020_A_vs_time %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, A, pixel_number, round, treatment,LEAF,BLOCK,VINE, datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2020-05-27") | date == as.Date("2020-07-12")|date == as.Date("2020-08-19") | date == as.Date("2020-09-07")) %>%
  filter(round > 2 & round < 5)



diurnals_2020_A_vs_leafT_licor_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$Tleaf)


diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment<- reorder(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment, diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$Tleaf)

str(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$Rep<-format(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$Rep)
as.character(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$Rep)

diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment)

str(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_HWS_2020_r_3_4 <-ggplot(diurnals_2020_A_vs_leafT_licor_HWS_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("HWS 2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(33,54,4), limits = c (33,54)) +
  scale_y_continuous(breaks=seq(-5,30,5), limits = c (-6,30)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(A_vs_leafT_licor_HWS_2020_r_3_4, filename = "figures/A_vs_leafT_licor_HWS_2020_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


#### A vs LICOR leaf T POST-HWS 2020 round 3-4 ####



diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4 <- diurnals_2020_A_vs_time %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, A, pixel_number, round, treatment, LEAF, BLOCK,VINE, datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2020-06-05") | date == as.Date("2020-07-20")|date == as.Date("2020-08-26") | date == as.Date("2020-09-15")) %>%
  filter(round > 2 & round < 5)



diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)


diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<- reorder(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment, diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)

str(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$Rep<-format(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$Rep)
as.character(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$Rep)

diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

str(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_post_HWS_2020_r_3_4 <-ggplot(diurnals_2020_A_vs_leafT_licor__POST_HWS_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("POST HWS 2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(27,41,3), limits = c (27,41)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(A_vs_leafT_licor_post_HWS_2020_r_3_4, filename = "figures/A_vs_leafT_licor_post_HWS_2020_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)




#### GSW vs LICOR leaf T PRE HWs  2020 round 3-4 ####

diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4<- diurnals_2020_A_vs_time%>%
  filter(!is.na(Tleaf)) %>%
  select( date,day, Tleaf, gsw, pixel_number, round, treatment, LEAF,VINE,BLOCK) %>%
  filter(date == "2020-07-10"|date =="2020-08-13") %>%
  filter(round > 2) %>%
  filter(round < 5)

diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4%>%
  group_by(treatment)%>%
  tally()


str(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$Tleaf)


diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<- reorder(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment, diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$Tleaf)

str(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$Rep<-format(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$Rep)
as.character(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$Rep)

diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

str(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)


## Plot first HW RESPONSE


gsw_vs_leafT_licor_PREHWs_2020_r_3_4 <-ggplot(diurnals_2020_gsw_vs_leafT_licor_PREHW_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("Pre-HWS 2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(34,46,3), limits = c (34,45)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
# theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +


ggsave(gsw_vs_leafT_licor_PREHWs_2020_r_3_4, filename = "figures/gsw_vs_leafT_licor_PREHWs_r_3_4.jpg", 
       width = 8, height = 6, dpi =600)


#### GSW vs LICOR leaf T HWS 2020 round 3-4 ####


diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4 <- diurnals_2020_A_vs_time %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, gsw, pixel_number, round, treatment,LEAF,VINE,BLOCK , datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2020-05-27") | date == as.Date("2020-07-12")|date == as.Date("2020-08-19") | date == as.Date("2020-09-07")) %>%
  filter(round > 2 & round < 5)



diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$Tleaf)


diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment<- reorder(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment, diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$Tleaf)

str(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$Rep<-format(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$Rep)
as.character(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$Rep)

diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

str(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_HWS_2020_r_3_4 <-ggplot(diurnals_2020_gsw_vs_leafT_licor_HWS_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("HWS 2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(33,54,4), limits = c (33,54)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.7))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(gsw_vs_leafT_licor_HWS_2020_r_3_4, filename = "figures/gsw_vs_leafT_licor_HWS_2020_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


#### GSW vs LICOR leaf T POST-HWS 2020 round 3-4 ####


diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4 <- diurnals_2020_A_vs_time %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, gsw, pixel_number, round, treatment, LEAF,VINE,BLOCK, datetime) %>%
  mutate(date = as.Date(datetime)) %>%  # Use `datetime` column to extract the date
  filter(date == as.Date("2020-06-05") | date == as.Date("2020-07-20")|date == as.Date("2020-08-26") | date == as.Date("2020-09-15")) %>%
  filter(round > 2 & round < 5)



diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)


diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<- reorder(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment, diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)

str(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$Rep<-format(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$Rep)
as.character(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$Rep)

diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

str(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_post_HWS_2020_r_3_4 <-ggplot(diurnals_2020_gsw_vs_leafT_licor__POST_HWS_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("POST HWS 2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(27,41,3), limits = c (27,41)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(gsw_vs_leafT_licor_post_HWS_2020_r_3_4, filename = "figures/gsw_vs_leafT_licor_post_HWS_2020_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


library(cowplot)


panel_plot_A_and_gsw_vs_tleaf_HWS_2020 <- plot_grid (A_vs_leafT_licor_PREHWs_2020_r_3_4,A_vs_leafT_licor_HWS_2020_r_3_4,A_vs_leafT_licor_post_HWS_2020_r_3_4,gsw_vs_leafT_licor_PREHWs_2020_r_3_4,gsw_vs_leafT_licor_HWS_2020_r_3_4,gsw_vs_leafT_licor_post_HWS_2020_r_3_4,
                                                     labels =c ("A","B","C","D","E"," F"),vjust = 4,
                                                     hjust = -6, 
                                                     label_size = 18,
                                                     ncol = 3, 
                                                     nrow = 2, 
                                                     align = "hv",
                                                     axis = "tblr" )


ggsave(panel_plot_A_and_gsw_vs_tleaf_HWS_2020 , filename = "figures/panel_plot_A_and_gsw_vs_tleaf_HWS_2020.jpg", width = 20, height = 12, dpi=600)




#### A vs LICOR leaf T PRE HWs  2021 round 3-4 ####

str(data_physiology_complete_BH_2021_middays)

diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4<- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(Tleaf)) %>%
  select( date,day, Tleaf, A, treatment, LEAF, BLOCK, VINE) %>%
  filter(date == "2021-06-17"|date =="2021-07-07") 

diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4%>%
  group_by(treatment)%>%
  tally()


str(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$Tleaf)


diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment<- reorder(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment, diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$Tleaf)

str(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment)

str(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4$treatment)


## Plot first HW RESPONSE


A_vs_leafT_licor_PREHWs_2021_r_3_4 <-ggplot(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("Pre-HWS 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(36,48,3), limits = c (36,48)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (-1,30))  +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
# theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +


write.csv(diurnals_2021_A_vs_leafT_licor_PREHW_r_3_4,"data_output/A_vs_leafT_licor_PREHWs_2021_r_3_4.csv")


ggsave(A_vs_leafT_licor_PREHWs_2021_r_3_4, filename = "figures/A_vs_leafT_licor_PREHWs_r_3_4.jpg", 
       width = 8, height = 6, dpi =600)


#### A vs LICOR leaf T HWS 2021 round 3-4 ####


diurnals_2021_A_vs_leafT_licor_HWS_r_3_4 <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, A, treatment, BLOCK, VINE, LEAF, date) %>%
  mutate(date = as.Date(date)) %>%  # Convert the `date` column to Date format
  filter(date %in% as.Date(c("2021-06-22", "2021-07-10", "2021-09-08")))

diurnals_2021_A_vs_leafT_licor_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$Tleaf)


diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment<- reorder(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment, diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$Tleaf)

str(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment)


diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment)

str(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_HWS_2021_r_3_4 <-ggplot(diurnals_2021_A_vs_leafT_licor_HWS_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("HWS 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(35,50,3), limits = c (35,50)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(A_vs_leafT_licor_HWS_2021_r_3_4, filename = "figures/A_vs_leafT_licor_HWS_2021_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


#### A vs LICOR leaf T POST-HWS 2021 round 3-4 ####
diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4 <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, A, treatment, BLOCK, VINE, LEAF, date) %>%  # Ensure `date` column is selected
  mutate(date = as.Date(date)) %>%  # Convert `date` column to Date format
  filter(date %in% as.Date(c("2021-06-30", "2021-07-14")))


diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)


diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<- reorder(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment, diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)

str(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)

str(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_post_HWS_2021_r_3_4 <-ggplot(diurnals_2021_A_vs_leafT_licor__POST_HWS_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("POST HWS 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(34,42,3), limits = c (34,42)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (-1,30)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(A_vs_leafT_licor_post_HWS_2021_r_3_4, filename = "figures/A_vs_leafT_licor_post_HWS_2021_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)




#### GSW vs LICOR leaf T PRE HWs  2021 round 3-4 ####

diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4<- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(Tleaf)) %>%
  select( date,day, Tleaf, gsw, treatment, LEAF, BLOCK, VINE) %>%
  filter(date == "2021-06-17"|date =="2021-07-07") 

diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4%>%
  group_by(treatment)%>%
  tally()


str(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$Tleaf)


diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<- reorder(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment, diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$Tleaf)

str(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)


diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment<-format(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)
as.character(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)

str(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4$treatment)


## Plot first HW RESPONSE


gsw_vs_leafT_licor_PREHWs_2021_r_3_4 <-ggplot(diurnals_2021_gsw_vs_leafT_licor_PREHW_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("Pre-HWS 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(36,48,3), limits = c (36,48)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
# theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
#        axis.ticks.x = element_blank()) +


ggsave(gsw_vs_leafT_licor_PREHWs_2021_r_3_4, filename = "figures/gsw_vs_leafT_licor_PREHWs_r_3_4.jpg", 
       width = 8, height = 6, dpi =600)


#### GSW vs LICOR leaf T HWS 2021 round 3-4 ####


diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4 <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, gsw, treatment, BLOCK, VINE, LEAF, date) %>%
  mutate(date = as.Date(date)) %>%  # Convert the `date` column to Date format
  filter(date %in% as.Date(c("2021-06-22", "2021-07-10", "2021-09-08")))



diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$Tleaf)


diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment<- reorder(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment, diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$Tleaf)

str(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment)


diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment<-format(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment)
as.character(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment)

str(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_HWS_2021_r_3_4 <-ggplot(diurnals_2021_gsw_vs_leafT_licor_HWS_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("HWS 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(35,50,3), limits = c (35,50)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(gsw_vs_leafT_licor_HWS_2021_r_3_4, filename = "figures/gsw_vs_leafT_licor_HWS_2021_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


#### GSW vs LICOR leaf T POST-HWS 2021 round 3-4 ####

diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4 <- data_physiology_complete_BH_2021_middays %>%
  filter(!is.na(Tleaf)) %>%
  select(day, Tleaf, gsw, treatment, BLOCK, VINE, LEAF, date) %>%  # Ensure `date` column is selected
  mutate(date = as.Date(date)) %>%  # Convert `date` column to Date format
  filter(date %in% as.Date(c("2021-06-30", "2021-07-14")))



diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)


diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<- reorder(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment, diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$Tleaf)

str(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)


diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment<-format(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)
as.character(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)

str(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_post_HWS_2021_r_3_4 <-ggplot(diurnals_2021_gsw_vs_leafT_licor__POST_HWS_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  ggtitle("POST HWS 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(34,42,3), limits = c (34,42)) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), limits = c (-0.1,0.6))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 
#  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
#  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+


ggsave(gsw_vs_leafT_licor_post_HWS_2021_r_3_4, filename = "figures/gsw_vs_leafT_licor_post_HWS_2021_r_3_4.jpg",
       width = 8, height = 6, dpi= 600)


library(cowplot)


panel_plot_A_and_gsw_vs_tleaf_HWS_2021 <- plot_grid (A_vs_leafT_licor_PREHWs_2021_r_3_4,A_vs_leafT_licor_HWS_2021_r_3_4,A_vs_leafT_licor_post_HWS_2021_r_3_4,gsw_vs_leafT_licor_PREHWs_2021_r_3_4,gsw_vs_leafT_licor_HWS_2021_r_3_4,gsw_vs_leafT_licor_post_HWS_2021_r_3_4,
                                                     labels =c ("A","B","C","D"," E"," F"),vjust = 4,
                                                     hjust = -6, 
                                                     label_size = 18,
                                                     ncol = 3, 
                                                     nrow = 2, 
                                                     align = "hv",
                                                     axis = "tblr" )


ggsave(panel_plot_A_and_gsw_vs_tleaf_HWS_2021 , filename = "figures/panel_plot_A_and_gsw_vs_tleaf_HWS_2021.jpg", width = 20, height = 12, dpi=600)

#####Figure 5 A GSW IRT TEMP MIDDAYS 2019-2021######
library(cowplot)
panel_plot_gas_exchange_and_leaf_temp_bh_2019_2020_2021 <- plot_grid(
  plot_grid(BH_middays_2019_A_plot_with_letters, BH_middays_2020_A_plot_with_letters,BH_middays_2021_A_plot_with_letters, BH_middays_2019_gsw_plot_with_letters, BH_middays_2020_gsw_plot_with_letters,BH_middays_2021_gsw_plot_with_letters,
            BH_middays_2019_Tleaf_IRT_plot_with_letters, BH_middays_2020_Tleaf_IRT_plot_with_letters, BH_middays_2021_Tleaf_IRT_plot_with_letters,  labels = c("A", "B", "C", "D", "E", "F", "G","H"," I"),ncol = 3, 
            vjust = 1.5, 
            hjust = -9.6, 
            label_size = 18,
            align = "v", axis = "l") # Adjust the height of the last row separately
)


ggsave(panel_plot_gas_exchange_and_leaf_temp_bh_2019_2020_2021, filename = "figures/panel_plot_gas_exchange_and_leaf_temp_bh_2019_2020_2021_outliers_removed.jpg", width = 38, height =30, dpi = 600)


#####Figure 6 E ,WUE AND WUE INT MIDDAYS 2019-2021######
library(cowplot)
panel_plot_E_and_WUE_bh_2019_2020_2021 <- plot_grid(
  plot_grid(BH_middays_2019_E_plot_with_letters, BH_middays_2020_E_plot_with_letters,BH_middays_2021_E_plot_with_letters, BH_middays_2019_WUE_plot_with_letters, BH_middays_2020_WUE_plot_with_letters,BH_middays_2021_WUE_plot_with_letters,
            BH_middays_2019_WUE_int_plot_with_letters, BH_middays_2020_WUE_int_plot_with_letters, BH_middays_2021_WUE_int_plot_with_letters,  labels = c("A", "B", "C", "D", "E", "F", "G","H"," I"),ncol = 3, 
            vjust = 1.5, 
            hjust = -9.6, 
            label_size = 18,
            align = "v", axis = "l") # Adjust the height of the last row separately
)


ggsave(panel_plot_E_and_WUE_bh_2019_2020_2021, filename = "figures/panel_plot_E_and_WUE_bh_2019_2020_2021.jpg", width = 38, height =28, dpi = 600)

plot(BH_middays_2021_WUE_int_plot)


legend_figure_5_and_6 <- get_legend(BH_middays_2021_WUE_int_plot)
legend_plot <- ggdraw(legend_figure_5_and_6)

ggsave(legend_figure_5_and_6, filename = "figures/legend_figure_5_and_6.jpg", width = 4, height =4, dpi =600)
