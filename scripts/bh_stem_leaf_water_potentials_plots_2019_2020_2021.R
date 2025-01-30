library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(xtable)
library(agricolae)



diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

str(diurnals_borden_hills_2019)

diurnals_borden_hills_2019<- diurnals_borden_hills_2019 %>%
  filter(!pixel_number == 34 ) %>%
  mutate(Leaf_wp_MPa = Leaf_wp_bar/10)%>%
  mutate(Stem_wp_MPa = Stem_wp_bar/10)

diurnals_borden_hills_2019$date1 <- as.Date(paste("2019", diurnals_borden_hills_2019$day), format = "%Y %j")


str(diurnals_borden_hills_2019$date1)

diurnals_2019_lwp_vs_time <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!day ==186)%>%
  filter(!day ==182)

diurnals_2019_lwp_vs_time$time<- format(strptime(diurnals_2019_lwp_vs_time$time,"%H:%M:%S"), format = "%H:%M")

diurnals_2019_lwp_vs_time$datetime <- paste(diurnals_2019_lwp_vs_time$date, " ", diurnals_2019_lwp_vs_time$time, sep = "")

glimpse(diurnals_2019_lwp_vs_time) 

diurnals_2019_lwp_vs_time$datetime <- ymd_hm(diurnals_2019_lwp_vs_time$datetime, tz = "UTC")

diurnals_2019_lwp_vs_time$round<-format(diurnals_2019_lwp_vs_time$round)
diurnals_2019_lwp_vs_time$round<-as.numeric(as.factor(diurnals_2019_lwp_vs_time$round))

str(diurnals_2019_lwp_vs_time$round)

str(diurnals_2019_lwp_vs_time)

tz(diurnals_2019_lwp_vs_time$datetime)

str(diurnals_2019_lwp_vs_time$datetime)

#### Plotting swp vs day with boxplots diurnals 2019 and plot with error bars####


diurnals_2019_swp_vs_time<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Stem_wp_MPa)) %>%
  select(datetime, day, Stem_wp_MPa, treatment,date1, BH_Block, BH_Vine, BH_Leaf)

diurnals_2019_swp_vs_time$treatment<- reorder(diurnals_2019_swp_vs_time$treatment, diurnals_2019_swp_vs_time$day)

diurnals_2019_swp_vs_time$day<-factor(diurnals_2019_swp_vs_time$day, 
                                      labels = c ("Jul 12"," Jul 25","Jul 28", "Aug 1", "Aug 15","Aug 20", "Sep 05"))

str(diurnals_2019_swp_vs_time)

diurnals_2019_swp_vs_time$treatment <- format(diurnals_2019_swp_vs_time$treatment )
as.character(diurnals_2019_swp_vs_time$treatment)


diurnals_2019_swp_vs_time%>%
  group_by(day, treatment)%>%
  tally()


pd2 <- position_dodge(0.75)

bh_swp_2019<-diurnals_2019_swp_vs_time %>%
  ggplot(aes(day, Stem_wp_MPa))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(paste(psi["stem"],  "  (MPa)"))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=24, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=24, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous(breaks=seq(-1.7,0,0.2), limits = c (-1.7,0)) + 
  geom_vline(xintercept = 3.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 4.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 5.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 6.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  annotate("text", x = "Jul 28", y = 0, label = "HW1", size = 6) +
  annotate("text", x = "Aug 15", y = 0, label ="HW2", size = 6) + 
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  theme(legend.position = "none") 


ggsave(bh_swp_2019, filename = "figures/bh_swp_2019_without_leaky_pixel.pdf", device = cairo_pdf, 
       width = 11, height = 6)

####ANOVA AND TUKEYS HSD STEM WP 2019 #####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_stem_vs_time_anova<-diurnals_2019_swp_vs_time


diurnals_2019_stem_vs_time_anova<-diurnals_2019_stem_vs_time_anova%>%
  mutate(Date = date1)

diurnals_2019_stem_vs_time_anova$treatment <- as.character(diurnals_2019_stem_vs_time_anova$treatment)
str(diurnals_2019_stem_vs_time_anova$treatment)

diurnals_2019_stem_vs_time_anova_tally<-diurnals_2019_stem_vs_time_anova%>%
  group_by(Date, treatment)%>%
  tally() 

write.csv(diurnals_2019_stem_vs_time_anova_tally,"data_output/BH_2019_stem_wp_midday_anova_tally.csv")

library(dplyr)

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    mutate(
      Q1 = quantile(Stem_wp_MPa, 0.25, na.rm = TRUE),
      Q3 = quantile(Stem_wp_MPa, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR
    ) %>%
    filter(Stem_wp_MPa >= lower_bound & Stem_wp_MPa <= upper_bound) %>%
    select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) %>%  # Clean up intermediate columns
    ungroup()  # Remove grouping after filtering
}




diurnals_2019_stem_vs_time_anova <- diurnals_2019_stem_vs_time_anova %>%
  group_by(treatment, Date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame


diurnals_2019_stem_vs_time_anova_tally_no_outliers<-diurnals_2019_stem_vs_time_anova%>%
  group_by(Date, treatment)%>%
  tally() 

write.csv(diurnals_2019_stem_vs_time_anova_tally_no_outliers,"data_output/BH_2019_stem_wp_midday_anova_tally_outliers_removed.csv")

diurnals_2019_stem_vs_time_anova$treatment <- as.factor(diurnals_2019_stem_vs_time_anova$treatment)

diurnals_2019_stem_vs_time_anova$BH_Vine<-as.factor(diurnals_2019_stem_vs_time_anova$BH_Vine)

str(diurnals_2019_stem_vs_time_anova$treatment)
str(diurnals_2019_stem_vs_time_anova$BH_Vine)
str(diurnals_2019_stem_vs_time_anova)

# Initialize an empty data frame to store results
dates <- unique(diurnals_2019_stem_vs_time_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_stem_wp_middays_bh_2019<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- diurnals_2019_stem_vs_time_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Stem_wp_MPa ~ treatment / BH_Vine, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Stem_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Stem_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Stem_wp_MPa,
    standard_error = standard_errors$Stem_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_stem_wp_middays_bh_2019 <- rbind(date_results,results_df_stem_wp_middays_bh_2019)
}

results_df_stem_wp_middays_bh_2019$date<-as.Date(results_df_stem_wp_middays_bh_2019$date)

str(results_df_stem_wp_middays_bh_2019)
results_df_stem_wp_middays_bh_2019$Mean_sem <- paste(round(results_df_stem_wp_middays_bh_2019$mean, 2), "±", round(results_df_stem_wp_middays_bh_2019$standard_error, 2),results_df_stem_wp_middays_bh_2019$letters_ordered.groups)


str(results_df_stem_wp_middays_bh_2019)

write.csv(results_df_stem_wp_middays_bh_2019,"data_output/results_df_stem_wp_middays_bh_2019.csv")
pd<- position_dodge(0.1)

BH_2019_swp_midday_plot<-
  ggplot(results_df_stem_wp_middays_bh_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[stem]~(MPa))) +
#  ggtitle("2019") +
  theme(text=element_text(family="Helvetica")) +
#  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-1.6,-0.4,0.2), limits = c (-1.6,-0.4,-0.2)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = -1.6, ymax = -0.4,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = -1.6, ymax = -0.4,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = -0.4, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = -0.4, label ="HW2", size = 7) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) +
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-10), name = "Secondary Y-Axis (Dummy)")
  ,breaks=seq(-1.6,-0.4,0.2), limits = c (-1.6,-0.4,-0.2)) +
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_line(color = "white"))


ggsave(BH_2019_swp_midday_plot, filename = "figures/BH_2019_swp_midday_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_stem_wp_middays_bh_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_stem_wp_middays_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
  # Rotate x-axis labels for better readability

vertical_offset <- 0.05  # Adjust this value based on your desired spacing


# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 



# Create the plot with letters of significance above the max mean points
BH_2019_swp_midday_plot_with_letters <- BH_2019_swp_midday_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -1.5,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks = seq(-1.6, -0.4, 0.2), limits = c(-1.6, -0.2))+ 
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-10), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-1.6,-0.4,0.2), limits = c (-1.6,-0.4,-0.2))
# + theme(legend.position = c(0.2, 0.2))




ggsave(BH_2019_swp_midday_plot_with_letters, filename = "figures/BH_2019_swp_midday_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)



##### midday leaf water potential 2019 ####

diurnals_2019_lwp_midday<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  filter(round == "4" )%>%
  select(datetime, day, Leaf_wp_MPa, treatment,date1,BH_Block,BH_Leaf,BH_Vine)

diurnals_2019_lwp_midday$treatment<-as.character(diurnals_2019_lwp_midday$treatment)
str(diurnals_2019_lwp_midday$treatment)

diurnals_2019_lwp_midday_tally<- diurnals_2019_lwp_midday%>%
  group_by(date1, treatment)%>%
  tally()

####ANOVA AND TUKEY HSD LEAF WATER POTENTIAL BH 2019 MIDDAYS####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_leaf_vs_time_anova_midday<-diurnals_2019_lwp_midday

diurnals_2019_leaf_vs_time_anova_midday$treatment <- as.factor(diurnals_2019_leaf_vs_time_anova_midday$treatment)
diurnals_2019_leaf_vs_time_anova_midday$BH_Vine <- as.factor(diurnals_2019_leaf_vs_time_anova_midday$BH_Vine)

str(diurnals_2019_leaf_vs_time_anova_midday$BH_Vine )
str(diurnals_2019_leaf_vs_time_anova_midday$treatment)

diurnals_2019_leaf_vs_time_anova_midday<-diurnals_2019_leaf_vs_time_anova_midday%>%
  mutate(Date = date1)


diurnals_2019_leaf_vs_time_anova_midday_tally<-diurnals_2019_leaf_vs_time_anova_midday%>%
  group_by(treatment,Date)%>%
  tally()

# Initialize an empty data frame to store results
dates <- unique(diurnals_2019_leaf_vs_time_anova_midday$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_leaf_wp_middays_bh_2019<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- diurnals_2019_leaf_vs_time_anova_midday %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Leaf_wp_MPa ~ treatment / BH_Vine, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Leaf_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Leaf_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$Leaf_wp_MPa,
    standard_error = standard_errors$Leaf_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_leaf_wp_middays_bh_2019 <- rbind(date_results,results_df_leaf_wp_middays_bh_2019)
}

results_df_leaf_wp_middays_bh_2019$date<-as.Date(results_df_leaf_wp_middays_bh_2019$date)

str(results_df_leaf_wp_middays_bh_2019)
results_df_leaf_wp_middays_bh_2019$Mean_sem <- paste(round(results_df_leaf_wp_middays_bh_2019$mean, 2), "±", round(results_df_leaf_wp_middays_bh_2019$standard_error, 2),results_df_leaf_wp_middays_bh_2019$letters_ordered.groups)


str(results_df_leaf_wp_middays_bh_2019)


pd<- position_dodge(0.1)

BH_2019_leaf_midday_plot<-
  ggplot(results_df_leaf_wp_middays_bh_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[leaf]~(MPa))) +
  ggtitle( "2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-1.8,-0.7,0.2), limits = c (-1.8,-0.7,-0.2)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = -1.8, ymax = -0.7,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = -1.8, ymax = -0.7,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = -0.7, label = "HW1", size = 6) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = -0.7, label ="HW2", size = 6)+
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) 


ggsave(BH_2019_leaf_midday_plot, filename = "figures/BH_2019_leaf_midday_plot.pdf", device = cairo_pdf, width = 12, height = 8) 

plot_means <- ggplot(results_df_leaf_wp_middays_bh_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_leaf_wp_middays_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))# Rotate x-axis labels for better readability


BH_2019_leaf_midday_plot_with_letters <- BH_2019_leaf_midday_plot +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.7, hjust= -0.5, size = 6,color = "black", position = position_dodge(width = 0.8)) 

ggsave(BH_2019_leaf_midday_plot_with_letters, filename = "figures/BH_2019_leaf_midday_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)



##### predawn leaf water potential 2019 ####
diurnals_2019_lwp_predawn<- diurnals_2019_lwp_vs_time %>%
  filter(!is.na(Leaf_wp_bar)) %>%
  filter(round == "1")%>%
  select(datetime, day, Leaf_wp_MPa, treatment,date1,BH_Block, BH_Vine, BH_Leaf) 

diurnals_2019_lwp_predawn_tally<-diurnals_2019_lwp_predawn%>%
  group_by(date1, treatment) %>%
  tally()

####ANOVA AND TUKEY HSD LEAF WATER POTENTIAL BH 2019 PREDAWNS####
# Sample data frame structure (replace this with your actual data frame)

diurnals_2019_leaf_vs_time_anova_predawn<-diurnals_2019_lwp_predawn

diurnals_2019_leaf_vs_time_anova_predawn$treatment <- as.factor(diurnals_2019_leaf_vs_time_anova_predawn$treatment)

diurnals_2019_leaf_vs_time_anova_predawn$BH_Vine <- as.factor(diurnals_2019_leaf_vs_time_anova_predawn$BH_Vine)

str(diurnals_2019_leaf_vs_time_anova_predawn$BH_Vine)
str(diurnals_2019_leaf_vs_time_anova_predawn$treatment)

diurnals_2019_leaf_vs_time_anova_predawn<-diurnals_2019_leaf_vs_time_anova_predawn%>%
  mutate(Date = date1)%>%
  mutate(leaf_wp_MPa = Leaf_wp_MPa )

str(diurnals_2019_leaf_vs_time_anova_predawn$leaf_wp_MPa)
diurnals_2019_leaf_vs_time_anova_predawn_tally<-diurnals_2019_leaf_vs_time_anova_predawn%>%
  group_by(treatment,Date)%>%
  tally()


write.csv(diurnals_2019_leaf_vs_time_anova_predawn_tally,"data_output/bh_2019_leaf_wp_predawn_anova_predawn_tally.csv")

remove_outliers_iqr_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    mutate(
      Q1 = quantile(leaf_wp_MPa, 0.25, na.rm = TRUE),
      Q3 = quantile(leaf_wp_MPa, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR
    ) %>%
    filter(leaf_wp_MPa >= lower_bound & leaf_wp_MPa <= upper_bound) %>%
    select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) %>%  # Remove intermediate columns
    ungroup()  # Ungroup the data
}



diurnals_2019_leaf_vs_time_anova_predawn <- diurnals_2019_leaf_vs_time_anova_predawn %>%
  group_by(treatment, Date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame

diurnals_2019_leaf_vs_time_anova_predawn_tally_no_outliers<-diurnals_2019_leaf_vs_time_anova_predawn%>%
  group_by(treatment,Date)%>%
  tally()

write.csv(diurnals_2019_leaf_vs_time_anova_predawn_tally_no_outliers,"data_output/bh_2019_leaf_wp_predawn_anova_predawn_tally_outliers_removed.csv")

# Initialize an empty data frame to store results
dates <- unique(diurnals_2019_leaf_vs_time_anova_predawn$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_leaf_wp_predawns_bh_2019<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- diurnals_2019_leaf_vs_time_anova_predawn %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_wp_MPa ~ treatment / BH_Vine, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.051, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_wp_MPa,
    standard_error = standard_errors$leaf_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_leaf_wp_predawns_bh_2019 <- rbind(date_results,results_df_leaf_wp_predawns_bh_2019)
}

results_df_leaf_wp_predawns_bh_2019$date<-as.Date(results_df_leaf_wp_predawns_bh_2019$date)

str(results_df_leaf_wp_predawns_bh_2019)
results_df_leaf_wp_predawns_bh_2019$Mean_sem <- paste(round(results_df_leaf_wp_predawns_bh_2019$mean, 2), "±", round(results_df_leaf_wp_predawns_bh_2019$standard_error, 2),results_df_leaf_wp_predawns_bh_2019$letters_ordered.groups)


str(results_df_leaf_wp_predawns_bh_2019)

write.csv(results_df_leaf_wp_predawns_bh_2019,"data_output/results_df_leaf_wp_predawns_bh_2019.csv")
pd<- position_dodge(0.1)

BH_2019_leaf_predawn_plot<-
  ggplot(results_df_leaf_wp_predawns_bh_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[leaf]~(MPa))) +
  ggtitle( "2019") +
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, face = "bold", family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2019-05-15"), as.Date("2019-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2019-05-15", "2019-09-16"))) +
  annotate("rect", xmin =  as.Date("2019-07-25", "%Y-%m-%d"), xmax = as.Date("2019-07-29", "%Y-%m-%d") , ymin = -0.9, ymax = 0.0,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2019-08-13", "%Y-%m-%d"), xmax = as.Date("2019-08-17", "%Y-%m-%d") , ymin = -0.9, ymax = 0.0,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2019-07-27", "%Y-%m-%d"), y = 0.0, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2019-08-15", "%Y-%m-%d"), y = 0.0, label ="HW2", size = 7)+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) +
  theme(legend.position = "none") +
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-100), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0)) +
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_line(color ="white"))


ggsave(BH_2019_leaf_predawn_plot, filename = "figures/BH_2019_leaf_predawn_plot.pdf", device = cairo_pdf, width = 13, height = 8) 

plot_means <- ggplot(results_df_leaf_wp_predawns_bh_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_leaf_wp_predawns_bh_2019 %>%
  filter(p_value < 0.05)



# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -1.5, size = 4, position = position_dodge(width = 3))# Rotate x-axis labels for better readability

vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.4)) 



# Create the plot with letters of significance above the max mean points
BH_2019_leaf_predawn_plot_with_letters <- BH_2019_leaf_predawn_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -1.5,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0))  +
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-100), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0))
# + theme(legend.position = c(0.2, 0.2))

ggsave(BH_2019_leaf_predawn_plot_with_letters, filename = "figures/BH_2019_leaf_predawn_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)


#### BH 2020 stem water potentials#######


diurnals_borden_hills_2020 <-read.csv("data_output/data_physiology_all_complete_BH_2020.csv", header = TRUE)


diurnals_2020_stem_wp_MPa_vs_time <- diurnals_borden_hills_2020 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time))

se <- function(x) sqrt(var(x)/length(x))


diurnals_2020_stem_wp_MPa_vs_time$time<-hms(diurnals_2020_stem_wp_MPa_vs_time$time)
diurnals_2020_stem_wp_MPa_vs_time$date<- ymd(diurnals_2020_stem_wp_MPa_vs_time$date)

str(diurnals_2020_stem_wp_MPa_vs_time$date)
str(diurnals_2020_stem_wp_MPa_vs_time$time)

diurnals_2020_stem_wp_MPa_vs_time$datetime <- paste(diurnals_2020_stem_wp_MPa_vs_time$date, " ", diurnals_2020_stem_wp_MPa_vs_time$time, sep = "")

str(diurnals_2020_stem_wp_MPa_vs_time$datetime)

glimpse(diurnals_2020_stem_wp_MPa_vs_time) 

diurnals_2020_stem_wp_MPa_vs_time$datetime <- ymd_hms(diurnals_2020_stem_wp_MPa_vs_time$datetime,  tz = "UTC")
str(diurnals_2020_stem_wp_MPa_vs_time$datetime)

diurnals_2020_stem_wp_MPa_vs_time$round<-format(diurnals_2020_stem_wp_MPa_vs_time$round)
diurnals_2020_stem_wp_MPa_vs_time$round<-as.numeric(as.factor(diurnals_2020_stem_wp_MPa_vs_time$round))

str(diurnals_2020_stem_wp_MPa_vs_time$round)
str(diurnals_2020_stem_wp_MPa_vs_time)

tz(diurnals_2020_stem_wp_MPa_vs_time$datetime)

tz(diurnals_2020_stem_wp_MPa_vs_time$time)

str(diurnals_2020_stem_wp_MPa_vs_time$datetime)


#### Plot stem_wp_MPa vs date with middays measurements ####


diurnals_borden_hills_2020$date<- ymd(diurnals_borden_hills_2020$date)
str(diurnals_borden_hills_2020$date)

BH_2020_middays_physiology<- diurnals_borden_hills_2020%>%
  filter ((date == "2020-09-07" & round == 3 ) | (date =="2020-09-15" & round ==4) | (date < "2020-09-06" & round == 4)) %>%
  filter(!is.na(stem_wp_MPa))

str(BH_2020_middays_physiology$round)

se <- function(x) sqrt(var(x)/length(x))

se1<- function (x) sd (x)/sqrt(length(x))


BH_2020_middays_physiology_tally<-BH_2020_middays_physiology%>%
  group_by(date, treatment)%>%
  tally()

BH_2020_middays_physiology$date<-factor(BH_2020_middays_physiology$date)
str(BH_2020_middays_physiology$date)


BH_2020_middays_physiology$treatment <-format(BH_2020_middays_physiology$treatment)
as.character(BH_2020_middays_physiology$treatment)

str(BH_2020_middays_physiology$treatment)


BH_2020_middays_physiology$date<-factor(BH_2020_middays_physiology$date, 
                                        labels = c ("May 27","Jun 5"," Jun 11","Jun 19", "Jun 25", "Jul 2","Jul 10", "Jul 12", "Jul 20", "Jul 31", "Aug 10", "Aug 13", "Aug 19", "Aug 26", "Sep 7", "Sep 15"))

pd2 <- position_dodge(0.75)

BH_stem_wp_MPa_middays_2020_boxplot<-BH_2020_middays_physiology%>%
  ggplot(aes(date,stem_wp_MPa))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab(expression(paste(psi[stem]~(MPa)))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Date") +
  scale_y_continuous(breaks=seq(-1.8,-0.3,0.2), limits = c (-1.8,-0.3)) +
  theme(axis.title.y = element_text(size=20, family = "serif")) +
  theme(axis.title.x = element_text(size=20, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  theme(legend.title.align = 0.5) +
  geom_vline(xintercept = 0.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 1.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 7.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 8.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 12.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 13.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 14.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  geom_vline(xintercept = 15.5, color = "darkgrey", size = 0.5, linetype ="dashed") +
  annotate("text", x = "May 27", y = -0.4, label = "HW1", size = 6) +
  annotate("text", x = "Jul 12", y = -0.4, label ="HW2", size = 6) +
  annotate("text", x = "Aug 19", y = -0.4, label = "HW3", size = 6) +
  annotate("text", x = "Sep 7", y = -0.4, label = "HW4", size = 6) 


ggsave(BH_stem_wp_MPa_middays_2020_boxplot, filename = "figures/BH_stem_wp_MPa_middays_2020_boxplot.pdf", 
       width = 16, height = 9)

######ANOVA AND TUKEYS HSD FOR STEM MIDDAYS WP 2020######

# Sample data frame structure (replace this with your actual data frame)

diurnals_2020_stem_vs_time_anova<-diurnals_borden_hills_2020%>%
  filter(!is.na(stem_wp_MPa))

diurnals_2020_stem_vs_time_anova$treatment <- as.factor(diurnals_2020_stem_vs_time_anova$treatment)
diurnals_2020_stem_vs_time_anova$VINE <- as.factor(diurnals_2020_stem_vs_time_anova$VINE)
str(diurnals_2020_stem_vs_time_anova$treatment)
str(diurnals_2020_stem_vs_time_anova$VINE)

diurnals_2020_stem_vs_time_anova<-diurnals_2020_stem_vs_time_anova%>%
  mutate(Date = date)

diurnals_2020_stem_vs_time_anova_tally<-diurnals_2020_stem_vs_time_anova%>%
  group_by(treatment,Date)%>%
  tally()

write.csv(diurnals_2020_stem_vs_time_anova_tally,"data_output/bh_2020_stem_wp_midday_anova_tally.csv")
remove_outliers_iqr_by_treatment_stem_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    mutate(
      Q1 = quantile(stem_wp_MPa, 0.25, na.rm = TRUE),
      Q3 = quantile(stem_wp_MPa, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR
    ) %>%
    filter(stem_wp_MPa >= lower_bound & stem_wp_MPa <= upper_bound) %>%
    select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) %>%  # Clean up intermediate columns
    ungroup()  # Ungroup the data after processing
}


diurnals_2020_stem_vs_time_anova <- diurnals_2020_stem_vs_time_anova %>%
  group_by(treatment, Date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment_stem_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame

diurnals_2020_stem_vs_time_anova_tally_no_otuliers<-diurnals_2020_stem_vs_time_anova%>%
  group_by(treatment,Date)%>%
  tally()
write.csv(diurnals_2020_stem_vs_time_anova_tally_no_otuliers,"data_output/bh_2020_stem_wp_midday_anova_tally_outliers_removed.csv")


# Initialize an empty data frame to store results
dates <- unique(diurnals_2020_stem_vs_time_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_stem_wp_middays_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- diurnals_2020_stem_vs_time_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(stem_wp_MPa ~ treatment /VINE, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(stem_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(stem_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$stem_wp_MPa,
    standard_error = standard_errors$stem_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_stem_wp_middays_bh_2020 <- rbind(date_results,results_df_stem_wp_middays_bh_2020)
}

results_df_stem_wp_middays_bh_2020$date<-as.Date(results_df_stem_wp_middays_bh_2020$date)

str(results_df_stem_wp_middays_bh_2020)
results_df_stem_wp_middays_bh_2020$Mean_sem <- paste(round(results_df_stem_wp_middays_bh_2020$mean, 2), "±", round(results_df_stem_wp_middays_bh_2020$standard_error, 2),results_df_stem_wp_middays_bh_2020$letters_ordered.groups)

write.csv(results_df_stem_wp_middays_bh_2020,"data_output/results_df_stem_wp_middays_bh_2020.csv")
#####
str(results_df_stem_wp_middays_bh_2020)

pd<- position_dodge(0.1)


BH_2020_swp_midday_plot<-
  ggplot(results_df_stem_wp_middays_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[stem]~(MPa))) +
 # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
 # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-1.6,-0.5,0.2), limits = c (-1.6,-0.5,-0.2)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = -1.6, ymax = -0.5,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = -1.6, ymax = -0.5,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = -1.6, ymax = -0.5,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = -1.6, ymax = -0.5,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = -0.5, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = -0.5, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = -0.5, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = -0.5, label ="HW4", size = 7) +
  theme(legend.position = "none")  +
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-10), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-1.6,-0.5,0.2), limits = c (-1.6,-0.5,-0.2)) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_line(color= "white"))


ggsave(BH_2020_swp_midday_plot, filename = "figures/BH_2020_swp_midday_plot.pdf", device = cairo_pdf, width = 13, height = 8)


plot_means <- ggplot(results_df_stem_wp_middays_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_stem_wp_middays_bh_2020 %>%
  filter(p_value < 0.05)

#%>%  filter(!date =="2020-09-07")


#significant_diffs_2 <- results_df_stem_wp_middays_bh_2020 %>%
#  filter(p_value < 0.05)%>%
#  filter(date =="2020-09-07")

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability

vertical_offset <- 0.06  # Adjust this value based on your desired spacing


# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.6)) 

# Create the plot with letters of significance above the max mean points
BH_2020_swp_midday_plot_with_letters <- BH_2020_swp_midday_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -1.5,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks = seq(-1.6, -0.4, 0.2), limits = c(-1.6, -0.2))+
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-10), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-1.6,-0.5,0.2), limits = c (-1.6,-0.5,-0.2))
# + theme(legend.position = c(0.2, 0.2))


ggsave(BH_2020_swp_midday_plot_with_letters, filename = "figures/BH_2020_swp_midday_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)




######ANOVA AND TUKEYS HSD FOR LEAF MIDDAYS WP 2020######


BH_2020_middays_physiology_lwp_anova<- diurnals_borden_hills_2020%>%
  filter ((date == "2020-09-07" & round == 3 ) | (date =="2020-09-15" & round ==4) | (date < "2020-09-06" & round == 4)) %>%
  filter(!is.na(leaf_wp_MPa))%>%
  mutate(Date =date)


se <- function(x) sqrt(var(x)/length(x))


BH_2020_middays_physiology_lwp_anova_tally<-BH_2020_middays_physiology_lwp_anova%>% group_by(Date, treatment)%>%
  tally()




BH_2020_middays_physiology_lwp_anova$treatment <- as.factor(BH_2020_middays_physiology_lwp_anova$treatment)
BH_2020_middays_physiology_lwp_anova$VINE <- as.factor(BH_2020_middays_physiology_lwp_anova$VINE)


str(BH_2020_middays_physiology_lwp_anova$VINE)
str(BH_2020_middays_physiology_lwp_anova$treatment)
str(BH_2020_middays_physiology_lwp_anova$Date)


# Initialize an empty data frame to store results
dates <- unique(BH_2020_middays_physiology_lwp_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_leaf_wp_middays_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- BH_2020_middays_physiology_lwp_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_wp_MPa ~ treatment/VINE, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_wp_MPa,
    standard_error = standard_errors$leaf_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_leaf_wp_middays_bh_2020 <- rbind(date_results,results_df_leaf_wp_middays_bh_2020)
}

results_df_leaf_wp_middays_bh_2020$date<-as.Date(results_df_leaf_wp_middays_bh_2020$date)

str(results_df_leaf_wp_middays_bh_2020)
results_df_leaf_wp_middays_bh_2020$Mean_sem <- paste(round(results_df_leaf_wp_middays_bh_2020$mean, 2), "±", round(results_df_leaf_wp_middays_bh_2020$standard_error, 2),results_df_leaf_wp_middays_bh_2020$letters_ordered.groups)


str(results_df_leaf_wp_middays_bh_2020)

pd<- position_dodge(0.1)

plot_means <- ggplot(results_df_leaf_wp_middays_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_leaf_wp_middays_bh_2020 %>%
  filter(p_value < 0.05)


# Add letters of significance to the plot
plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))# Rotate x-axis labels for better readability


BH_2020_lwp_midday_plot<-
  ggplot(results_df_leaf_wp_middays_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[leaf]~(MPa))) +
  ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-1.8,-0.7,0.2), limits = c (-1.8,-0.7,-0.2)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = -1.8, ymax = -0.7,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = -1.8, ymax = -0.7,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = -1.8, ymax = -0.7,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = -1.8, ymax = -0.7,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = -0.7, label = "HW1", size = 6) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = -0.7, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = -0.7, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = -0.7, label ="HW4", size = 6) +
  theme(legend.position = "none") 


ggsave(BH_2020_lwp_midday_plot, filename = "figures/BH_2020_lwp_midday_plot.pdf", device = cairo_pdf, width = 12, height = 8)


BH_2020_lwp_midday_plot_with_letters <- BH_2020_lwp_midday_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, x = date, y = mean), 
            vjust = -1.5, size = 6, 
            position = position_dodge(width = 0.8), 
            color = "black")


ggsave(BH_2020_lwp_midday_plot_with_letters, filename = "figures/BH_2020_lwp_midday_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)

######ANOVA AND TUKEYS HSD FOR LEAF PREDAWNS WP 2020######

####predawn

BH_2020_predawn_physiology_lwp<- diurnals_borden_hills_2020%>%
  filter (round == 1) %>%
  filter(!is.na(leaf_wp_MPa))%>%
  mutate(Date = date)%>%
  select(Date, treatment, VINE, BLOCK, LEAF, stem_wp_MPa, leaf_wp_MPa)

#str(BH_2020_predawn_physiology_lwp$round)

BH_2020_predawn_physiology_lwp_tally<-BH_2020_predawn_physiology_lwp%>%
  group_by(Date, treatment)%>%
  tally()

write.csv(BH_2020_predawn_physiology_lwp_tally,"data_output/BH_2020_predawn_wp_tally.csv")

str(BH_2020_predawn_physiology_lwp)


remove_outliers_iqr_by_treatment_leaf_by_treatment <- function(data) {
  data %>%
    group_by(treatment) %>%  # Group by treatment
    mutate(
      Q1 = quantile(leaf_wp_MPa, 0.25, na.rm = TRUE),
      Q3 = quantile(leaf_wp_MPa, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR
    ) %>%
    filter(leaf_wp_MPa >= lower_bound & leaf_wp_MPa <= upper_bound) %>%
    select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) %>%  # Remove temporary columns
    ungroup()  # Ungroup the data for further analysis
}

BH_2020_predawn_physiology_lwp <- BH_2020_predawn_physiology_lwp %>%
  group_by(treatment, Date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment_leaf_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame

BH_2020_predawn_physiology_lwp_tally_no_outliers<-BH_2020_predawn_physiology_lwp%>%
  group_by(Date, treatment)%>%
  tally()

write.csv(BH_2020_predawn_physiology_lwp_tally_no_outliers,"data_output/BH_2020_predawn_wp_tally_outliers_removed.csv")

BH_2020_predawn_physiology_lwp$treatment <- as.factor(BH_2020_predawn_physiology_lwp$treatment)
BH_2020_predawn_physiology_lwp$VINE <- as.factor(BH_2020_predawn_physiology_lwp$VINE)

str(BH_2020_predawn_physiology_lwp$VINE)
str(BH_2020_predawn_physiology_lwp$treatment)
str(BH_2020_predawn_physiology_lwp$Date)


# Initialize an empty data frame to store results
dates <- unique(BH_2020_predawn_physiology_lwp$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_leaf_wp_predawns_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- BH_2020_predawn_physiology_lwp %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_wp_MPa ~ treatment/VINE, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_wp_MPa,
    standard_error = standard_errors$leaf_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_leaf_wp_predawns_bh_2020 <- rbind(date_results,results_df_leaf_wp_predawns_bh_2020)
}

results_df_leaf_wp_predawns_bh_2020$date<-as.Date(results_df_leaf_wp_predawns_bh_2020$date)

str(results_df_leaf_wp_predawns_bh_2020)
results_df_leaf_wp_predawns_bh_2020$Mean_sem <- paste(round(results_df_leaf_wp_predawns_bh_2020$mean, 2), "±", round(results_df_leaf_wp_predawns_bh_2020$standard_error, 2),results_df_leaf_wp_predawns_bh_2020$letters_ordered.groups)


str(results_df_leaf_wp_predawns_bh_2020)

write.csv(results_df_leaf_wp_predawns_bh_2020,"data_output/results_df_leaf_wp_predawns_bh_2020.csv")

pd<- position_dodge(0.1)

plot_means <- ggplot(results_df_leaf_wp_predawns_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_leaf_wp_predawns_bh_2020 %>%
  filter(p_value < 0.05)


# Add letters of significance to the plot
plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.8, size = 4, position = position_dodge(width = 0.8))# Rotate x-axis labels for better readability


BH_2020_lwp_predawn_plot<-
  ggplot(results_df_leaf_wp_predawns_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[leaf]~(MPa))) +
 ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
 theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0)) + 
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2020-05-15", "2020-09-16"))) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20)) +
  annotate("rect", xmin =  as.Date("2020-05-26", "%Y-%m-%d"), xmax = as.Date("2020-05-28", "%Y-%m-%d") , ymin = -0.9, ymax = 0.0,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") , ymin = -0.9, ymax = 0.0,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") , ymin = -0.9, ymax = 0.0,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") , ymin = -0.9, ymax = 0.0,
           alpha = .2)+
  annotate("text", x = as.Date("2020-05-27", "%Y-%m-%d"), y = 0.0, label = "HW1", size = 7) +
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 0.0, label ="HW2", size = 7) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 0.0, label ="HW3", size = 7)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 0.0, label ="HW4", size = 7) +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) +
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-100), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0)) +
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_line(color ="white")) +
  theme(axis.title.y.left  = element_text(color = "white"), axis.text.y.left  = element_text(color = "white"), axis.ticks.y = element_blank())



ggsave(BH_2020_lwp_predawn_plot, filename = "figures/BH_2020_lwp_predawn_plot.pdf", device = cairo_pdf, width = 13, height = 8)

vertical_offset <- 0.05  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.5)) 



# Create the plot with letters of significance above the max mean points
BH_2020_lwp_predawn_plot_with_letters <- BH_2020_lwp_predawn_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -1.5,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0))+
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-100), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0))
# + theme(legend.position = c(0.2, 0.2))


ggsave(BH_2020_lwp_predawn_plot_with_letters, filename = "figures/BH_2020_lwp_predawn_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)




#####Final figure irrgation + water potentials 2019 -202#####


panel_plot_irrigation_water_potentials_bh_2019_2020 <- plot_grid(
  plot_grid(BH_2019_leaf_predawn_plot_with_letters, BH_2020_lwp_predawn_plot_with_letters, BH_2019_swp_midday_plot_with_letters, BH_2020_swp_midday_plot_with_letters,
            daily_irr_tmax_2019_HW_plot_date_selection, daily_irr_tmax_2020_HW_plot_date_selection,  labels = c("A", "B", "C", "D", "E", "F"),ncol = 2, 
            vjust = 1.5, 
            hjust = -9.6, 
            label_size = 18,
            align = "v", axis = "l"),
  plot_grid(phenological_bar_plot_2019, phenological_bar_plot_2020,
            ncol = 2),
  ncol = 1,
  rel_heights = c(4, 0.2) # Adjust the height of the last row separately
)


ggsave(panel_plot_irrigation_water_potentials_bh_2019_2020, filename = "figures/panel_plot_irrigation_water_potentials_bh_2019_2020.pdf", device = cairo_pdf, width = 24, height =22)


ggsave(panel_plot_irrigation_water_potentials_bh_2019_2020, filename = "figures/panel_plot_irrigation_water_potentials_bh_2019_2020.png", width = 23, height =21)


###### WATER POTENTIALS 2021 ######




borden_hills_2021_middays <-read.csv("data_output/data_physiology_complete_BH_2021_middays.csv", header = TRUE)

borden_hills_2021_predawns <-read.csv("data_output/data_physiology_complete_BH_2021_predawn.csv", header = TRUE)


bh_2021_stem_wp_MPa_midday <- borden_hills_2021_middays %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) 

se <- function(x) sqrt(var(x)/length(x))


bh_2021_stem_wp_MPa_midday$time<-hms(bh_2021_stem_wp_MPa_midday$time)
bh_2021_stem_wp_MPa_midday$date<- ymd(bh_2021_stem_wp_MPa_midday$date)

str(bh_2021_stem_wp_MPa_midday$date)
str(bh_2021_stem_wp_MPa_midday$time)

bh_2021_stem_wp_MPa_midday$datetime <- paste(bh_2021_stem_wp_MPa_midday$date, " ", bh_2021_stem_wp_MPa_midday$time, sep = "")

str(bh_2021_stem_wp_MPa_midday$datetime)

glimpse(bh_2021_stem_wp_MPa_midday) 

bh_2021_stem_wp_MPa_midday$datetime <- ymd_hms(bh_2021_stem_wp_MPa_midday$datetime,  tz = "UTC")
str(bh_2021_stem_wp_MPa_midday$datetime)


str(bh_2021_stem_wp_MPa_midday)

tz(bh_2021_stem_wp_MPa_midday$datetime)

tz(bh_2021_stem_wp_MPa_midday$time)

str(bh_2021_stem_wp_MPa_midday$datetime)


bh_2021_leaf_wp_MPa_predawn <- borden_hills_2021_predawns %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) 

se <- function(x) sqrt(var(x)/length(x))


bh_2021_leaf_wp_MPa_predawn$time<-hms(bh_2021_leaf_wp_MPa_predawn$time)
bh_2021_leaf_wp_MPa_predawn$date<- ymd(bh_2021_leaf_wp_MPa_predawn$date)

str(bh_2021_leaf_wp_MPa_predawn$date)
str(bh_2021_leaf_wp_MPa_predawn$time)

bh_2021_leaf_wp_MPa_predawn$datetime <- paste(bh_2021_leaf_wp_MPa_predawn$date, " ", bh_2021_leaf_wp_MPa_predawn$time, sep = "")

str(bh_2021_leaf_wp_MPa_predawn$datetime)

glimpse(bh_2021_leaf_wp_MPa_predawn) 

bh_2021_leaf_wp_MPa_predawn$datetime <- ymd_hms(bh_2021_leaf_wp_MPa_predawn$datetime,  tz = "UTC")
str(bh_2021_leaf_wp_MPa_predawn$datetime)


str(bh_2021_leaf_wp_MPa_predawn)

tz(bh_2021_leaf_wp_MPa_predawn$datetime)

tz(bh_2021_leaf_wp_MPa_predawn$time)



#####ANOVA AND TUKEY HSD STEM WATER POTENTIALS 2021#####

# Sample data frame structure (replace this with your actual data frame)

diurnals_2021_stem_vs_time_anova<-bh_2021_stem_wp_MPa_midday%>%
  filter(!is.na(stem_wp_MPa))%>%
  select(date, treatment, VINE, LEAF, BLOCK, stem_wp_MPa, leaf_wp_MPa)

diurnals_2021_stem_vs_time_anova$treatment <- as.factor(diurnals_2021_stem_vs_time_anova$treatment)
diurnals_2021_stem_vs_time_anova$VINE <- as.factor(diurnals_2021_stem_vs_time_anova$VINE)
str(diurnals_2021_stem_vs_time_anova$treatment)
str(diurnals_2021_stem_vs_time_anova$VINE)

sum(is.na(diurnals_2021_stem_vs_time_anova$VINE))
diurnals_2021_stem_vs_time_anova<-diurnals_2021_stem_vs_time_anova%>%
  mutate(Date = date)%>%
  filter(!Date == "2021-09-08")

diurnals_2021_stem_vs_time_anova_tally<-diurnals_2021_stem_vs_time_anova%>%
  group_by(treatment,Date)%>%
  tally()

write.csv(diurnals_2021_stem_vs_time_anova_tally,"data_output/bh_2021_stem_wp_midday_anova_tally.csv")

diurnals_2021_stem_vs_time_anova_no_outliers <- diurnals_2021_stem_vs_time_anova %>%
  group_by(treatment, Date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment_stem_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame

diurnals_2021_stem_vs_time_anova_no_outliers_tally<-diurnals_2021_stem_vs_time_anova_no_outliers%>%
  group_by(date, treatment)%>%
  tally()

write.csv(diurnals_2021_stem_vs_time_anova_tally,"data_output/bh_2021_stem_wp_midday_anova_tally_outliers_removed.csv")

str(diurnals_2021_stem_vs_time_anova_no_outliers)


# Initialize an empty data frame to store results
dates <- unique(diurnals_2021_stem_vs_time_anova_no_outliers$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_stem_wp_middays_bh_2021<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- diurnals_2021_stem_vs_time_anova_no_outliers %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(stem_wp_MPa ~ treatment/VINE , data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(stem_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(stem_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$stem_wp_MPa,
    standard_error = standard_errors$stem_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_stem_wp_middays_bh_2021 <- rbind(date_results,results_df_stem_wp_middays_bh_2021)
}

results_df_stem_wp_middays_bh_2021$date<-as.Date(results_df_stem_wp_middays_bh_2021$date)

str(results_df_stem_wp_middays_bh_2021)
results_df_stem_wp_middays_bh_2021$Mean_sem <- paste(round(results_df_stem_wp_middays_bh_2021$mean, 2), "±", round(results_df_stem_wp_middays_bh_2021$standard_error, 2),results_df_stem_wp_middays_bh_2021$letters_ordered.groups)

write.csv(results_df_stem_wp_middays_bh_2021,"data_output/results_df_stem_wp_middays_bh_2021.csv")

str(results_df_stem_wp_middays_bh_2021)

pd<- position_dodge(0.1)


BH_2021_swp_midday_plot<-
  ggplot(results_df_stem_wp_middays_bh_2021, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[stem]~(MPa))) +
  # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-1.6,-0.5,0.2), limits = c (-1.6,-0.5,-0.2)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = -0.5, label = "HW3", size = 7) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = -0.5, label = "HW2", size = 7) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = -0.5, label = "HW1", size = 7) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = -1.6, ymax = -0.5,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = -1.6, ymax = -0.5,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = -1.6, ymax = -0.5,
           alpha = .2)+
  theme(legend.position = "none")  +
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-10), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-1.6,-0.5,0.2), limits = c (-1.6,-0.5,-0.2)) +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_line(color= "white"))


ggsave(BH_2021_swp_midday_plot, filename = "figures/BH_2021_swp_midday_plot.pdf", device = cairo_pdf, width = 13, height = 8)


plot_means <- ggplot(results_df_stem_wp_middays_bh_2021, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_stem_wp_middays_bh_2021 %>%
  filter(p_value < 0.05)

#%>%  filter(!date =="2020-09-07")


#significant_diffs_2 <- results_df_stem_wp_middays_bh_2021 %>%
#  filter(p_value < 0.05)%>%
#  filter(date =="2020-09-07")

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability

vertical_offset <- 0.05  # Adjust this value based on your desired spacing


# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.7)) 

# Create the plot with letters of significance above the max mean points
BH_2021_swp_midday_plot_with_letters <- BH_2021_swp_midday_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -1.5,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks = seq(-1.6, -0.4, 0.2), limits = c(-1.6, -0.2))+
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-10), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-1.6,-0.5,0.2), limits = c (-1.6,-0.5,-0.2))
# + theme(legend.position = c(0.2, 0.2))


ggsave(BH_2020_swp_midday_plot_with_letters, filename = "figures/BH_2020_swp_midday_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)

##### ANOVA AND TUKEY HSD PREDAWNS WATER POTENTIALS 2021######

BH_2021_predawn_physiology_lwp<- bh_2021_leaf_wp_MPa_predawn%>%
  filter(!is.na(leaf_wp_MPa))%>%
  mutate(Date = date)%>%
  select(Date,date, VINE, BLOCK, LEAF, treatment, stem_wp_MPa, leaf_wp_MPa)


BH_2021_predawn_physiology_lwp_tally<-BH_2021_predawn_physiology_lwp%>%
  group_by(date, treatment)%>%
  tally()


write.csv(BH_2021_predawn_physiology_lwp_tally,"data_output/BH_2021_predawn_wp_tally.csv")
BH_2021_predawn_physiology_lwp_no_outliers <- BH_2021_predawn_physiology_lwp %>%
  group_by(treatment, Date) %>%  # Grouping by treatment and date
  do(remove_outliers_iqr_by_treatment_leaf_by_treatment(.)) %>%  # Apply the outlier removal function
  ungroup()  # Ungroup the data frame

BH_2021_predawn_physiology_lwp_no_outliers_tally<-BH_2021_predawn_physiology_lwp_no_outliers%>%
  group_by(date, treatment)%>%
  tally()

write.csv(BH_2021_predawn_physiology_lwp_tally,"data_output/BH_2021_predawn_wp_tally_outliers_removed.csv")

BH_2021_predawn_physiology_lwp_no_outliers$treatment <- as.factor(BH_2021_predawn_physiology_lwp_no_outliers$treatment)
BH_2021_predawn_physiology_lwp_no_outliers$VINE <- as.factor(BH_2021_predawn_physiology_lwp_no_outliers$VINE)

str(BH_2021_predawn_physiology_lwp_no_outliers$VINE)
str(BH_2021_predawn_physiology_lwp_no_outliers$treatment)
str(BH_2021_predawn_physiology_lwp_no_outliers$Date)

str(BH_2021_predawn_physiology_lwp_no_outliers)
# Initialize an empty data frame to store results
dates <- unique(BH_2021_predawn_physiology_lwp_no_outliers$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_leaf_wp_predawns_bh_2021<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- BH_2021_predawn_physiology_lwp_no_outliers %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(leaf_wp_MPa ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_wp_MPa ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_wp_MPa,
    standard_error = standard_errors$leaf_wp_MPa,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_leaf_wp_predawns_bh_2021 <- rbind(date_results,results_df_leaf_wp_predawns_bh_2021)
}

results_df_leaf_wp_predawns_bh_2021$date<-as.Date(results_df_leaf_wp_predawns_bh_2021$date)

str(results_df_leaf_wp_predawns_bh_2021)
results_df_leaf_wp_predawns_bh_2021$Mean_sem <- paste(round(results_df_leaf_wp_predawns_bh_2021$mean, 2), "±", round(results_df_leaf_wp_predawns_bh_2021$standard_error, 2),results_df_leaf_wp_predawns_bh_2021$letters_ordered.groups)


str(results_df_leaf_wp_predawns_bh_2021)

write.csv(results_df_leaf_wp_predawns_bh_2021,"data_output/results_df_leaf_wp_predawns_bh_2021.csv")

write.csv(results_df_leaf_wp_predawns_bh_2020,"data_output/results_df_leaf_wp_predawns_bh_2020.csv")

write.csv(results_df_leaf_wp_predawns_bh_2019,"data_output/results_df_leaf_wp_predawns_bh_2019.csv")

pd<- position_dodge(0.1)

plot_means <- ggplot(results_df_leaf_wp_predawns_bh_2021, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_leaf_wp_predawns_bh_2021 %>%
  filter(p_value < 0.05)


# Add letters of significance to the plot
plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.8, size = 4, position = position_dodge(width = 0.8))# Rotate x-axis labels for better readability


BH_2021_lwp_predawn_plot<-
  ggplot(results_df_leaf_wp_predawns_bh_2021, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(psi[leaf]~(MPa))) +
  ggtitle("2021") +
  theme(text=element_text(family="Helvetica")) +
  theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0)) + 
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  coord_cartesian(xlim = as.Date(c("2021-05-15", "2021-09-16"))) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 0.0, label = "HW3", size = 7) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 0.0, label = "HW2", size = 7) +
  annotate("text", x = as.Date("06-18-2021", format= "%m-%d-%Y"), y = 0.0, label = "HW1", size = 7) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = -0.9, ymax = 0,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = -0.9, ymax = 0,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-06-17", "%Y-%m-%d"), xmax = as.Date("2021-06-20", "%Y-%m-%d") , ymin = -0.9, ymax = 0,
           alpha = .2)+
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank()) +
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-100), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0)) +
  theme(
    axis.title.y.right = element_text(color = "white"),
    axis.text.y.right =  element_text(color = "white"),
    axis.ticks.y.right = element_blank(),
    axis.line.y.right = element_line(color ="white")) +
  theme(axis.title.y.left  = element_text(color = "white"), axis.text.y.left  = element_text(color = "white"), axis.ticks.y = element_blank())



ggsave(BH_2021_lwp_predawn_plot, filename = "figures/BH_2021_lwp_predawn_plot.pdf", device = cairo_pdf, width = 13, height = 8)

vertical_offset <- 0.05  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 0.5)) 



# Create the plot with letters of significance above the max mean points
BH_2021_lwp_predawn_plot_with_letters <- BH_2021_lwp_predawn_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -1.5,  # Adjust vertical justification
            size = 7, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0))+
  scale_y_continuous(
    sec.axis = sec_axis(~ .*(-100), name = "Secondary Y-Axis (Dummy)")
    ,breaks=seq(-0.9,0.0,0.2), limits = c (-0.9,0.0))
# + theme(legend.position = c(0.2, 0.2))


ggsave(BH_2021_lwp_predawn_plot_with_letters, filename = "figures/BH_2021_lwp_predawn_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)



#####NEW FIGURE WATER POTENTIAL WITH IRRIGATION FOR 2019, 2020 AND 2021 BH HEATWAVE######



#####Final figure irrgation + water potentials 2019 -2021#####
library(cowplot)

panel_plot_irrigation_water_potentials_bh_2019_2020_2021 <- plot_grid(
  plot_grid(BH_2019_leaf_predawn_plot_with_letters, BH_2020_lwp_predawn_plot_with_letters,BH_2021_lwp_predawn_plot_with_letters, BH_2019_swp_midday_plot_with_letters, BH_2020_swp_midday_plot_with_letters,BH_2021_swp_midday_plot_with_letters,
            daily_irr_tmax_2019_HW_plot_date_selection, daily_irr_tmax_2020_HW_plot_date_selection, daily_irr_tmax_2021_HW_plot_date_selection,  labels = c("A", "B", "C", "D", "E", "F", "G","H"," I"),ncol = 3, 
            vjust = 1.5, 
            hjust = -9.6, 
            label_size = 18,
            align = "v", axis = "l"),
  plot_grid(phenological_bar_plot_2019, phenological_bar_plot_2020, phenological_bar_plot_2020,
            ncol = 3),
  ncol = 1,
  rel_heights = c(4, 0.2) # Adjust the height of the last row separately
)


ggsave(panel_plot_irrigation_water_potentials_bh_2019_2020_2021, filename = "figures/panel_plot_irrigation_water_potentials_bh_2019_2020_2021.jpg", width = 36, height =24, dpi = 600)





