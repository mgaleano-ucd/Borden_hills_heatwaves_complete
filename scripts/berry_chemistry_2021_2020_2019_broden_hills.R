library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(agricolae)

##### BH 2019 PRIMARY CHEMISTRY #####

berry_chemistry_borden_hills_2019 <-read.csv("data/berry_chemistry_2019_3.csv", header = TRUE)

str(berry_chemistry_borden_hills_2019)

berry_chemistry_borden_hills_2019 <-berry_chemistry_borden_hills_2019%>%
  filter(!is.na(Rep)) %>%
  filter(!pixel_number == (34))

berry_chemistry_borden_hills_2019$date<- mdy(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

tz(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

se <- function(x) sqrt(var(x)/length(x))

berry_chemistry_borden_hills_2019 %>%
  group_by(treatment, date) %>%
  tally()

berry_chemistry_borden_hills_2019$date[berry_chemistry_borden_hills_2019$date == "2019-09-12"] <- "2019-09-19"

berry_chemistry_borden_hills_2019$treatment<- as.factor(berry_chemistry_borden_hills_2019$treatment)

str(berry_chemistry_borden_hills_2019$treatment)
str(berry_chemistry_borden_hills_2019$date)


####ANOVA AND TUKEYS HSD BRIX 2019 #####

berry_chemistry_borden_hills_brix_2019_anova<-berry_chemistry_borden_hills_2019%>%
  mutate(brix = Brix)


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_brix_2019_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_brix_bh_2019<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_brix_2019_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( brix~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(brix ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(brix ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$brix,
    standard_error = standard_errors$brix,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_brix_bh_2019 <- rbind(date_results,results_df_brix_bh_2019)
}

results_df_brix_bh_2019$date<-as.Date(results_df_brix_bh_2019$date)

str(results_df_brix_bh_2019)
results_df_brix_bh_2019$Mean_sem <- paste(round(results_df_brix_bh_2019$mean, 2), "±", round(results_df_brix_bh_2019$standard_error, 2),results_df_brix_bh_2019$letters_ordered.groups)


str(results_df_brix_bh_2019)

results_df_brix_bh_2019$date[results_df_brix_bh_2019$treatment == "1" & results_df_brix_bh_2019$date == "2019-09-19"] <- "2019-09-12"

write.csv(results_df_brix_bh_2019,"data_output/results_df_brix_bh_2019.csv")
pd<- position_dodge(0.1)

BH_2019_brix_plot<-
  ggplot(results_df_brix_bh_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Total soluble solids (ºBx)") +
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
  scale_y_continuous(breaks=seq(12,28,2), limits = c (12,28)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20)) +
  annotate("rect", xmin =  as.Date("08-12-2019", format ="%m-%d-%Y"), xmax = as.Date("08-16-2019", format ="%m-%d-%Y") ,  ymin = 12, ymax = 28,
           alpha = .2)+
  annotate("text",  x = as.Date("08-14-2019", format ="%m-%d-%Y"), y = 28, label ="HW2", size = 8) +
  theme(legend.position = "none")


ggsave(BH_2019_brix_plot, filename = "figures/BH_2019_brix_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_brix_bh_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_brix_bh_2019 %>%
  filter(p_value < 0.05)

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
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points

brix_bh_2019_with_letters <- BH_2019_brix_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 8, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(12,28,2), limits = c (12,28)) 

ggsave(brix_bh_2019_with_letters, filename = "figures/brix_bh_2019_with_letters.pdf", device = cairo_pdf, width = 10, height = 8)

####ANOVA AND TUKEYS HSD TA 2019 #####

berry_chemistry_borden_hills_TA_2019_anova<-berry_chemistry_borden_hills_2019


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_TA_2019_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_TA_bh_2019<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_TA_2019_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( TA~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(TA ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(TA ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$TA,
    standard_error = standard_errors$TA,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_TA_bh_2019 <- rbind(date_results,results_df_TA_bh_2019)
}

results_df_TA_bh_2019$date<-as.Date(results_df_TA_bh_2019$date)

str(results_df_TA_bh_2019)
results_df_TA_bh_2019$Mean_sem <- paste(round(results_df_TA_bh_2019$mean, 2), "±", round(results_df_TA_bh_2019$standard_error, 2),results_df_TA_bh_2019$letters_ordered.groups)


str(results_df_TA_bh_2019)

results_df_TA_bh_2019$date[results_df_TA_bh_2019$treatment == "1" & results_df_TA_bh_2019$date == "2019-09-19"] <- "2019-09-12"

write.csv(results_df_TA_bh_2019,"data_output/results_df_TA_bh_2019.csv")
pd<- position_dodge(0.1)

BH_2019_TA_plot<-
  ggplot(results_df_TA_bh_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Titratable acidity (g/L)") +
  theme(text=element_text(family="Helvetica")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2,20,3), limits = c (2,20)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20)) +
  annotate("rect", xmin =  as.Date("08-12-2019", format ="%m-%d-%Y"), xmax = as.Date("08-16-2019", format ="%m-%d-%Y") ,  ymin = 2, ymax = 20,
           alpha = .2)+
  annotate("text",  x = as.Date("08-14-2019", format ="%m-%d-%Y"), y = 20, label ="HW2", size = 8) +
  theme(legend.position = "none")


ggsave(BH_2019_TA_plot, filename = "figures/BH_2019_TA_plot.pdf", device = cairo_pdf, width = 8, height = 7)

plot_means <- ggplot(results_df_TA_bh_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_TA_bh_2019 %>%
  filter(p_value < 0.05)

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
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points

TA_bh_2019_with_letters <- BH_2019_TA_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 8, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(2,20,3), limits = c (2,20)) 

ggsave(TA_bh_2019_with_letters, filename = "figures/TA_bh_2019_with_letters.pdf", device = cairo_pdf, width = 9, height = 7)


####ANOVA AND TUKEYS HSD PH 2019 #####

berry_chemistry_borden_hills_pH_2019_anova<-berry_chemistry_borden_hills_2019


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_pH_2019_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_pH_bh_2019<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_pH_2019_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( pH~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(pH ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(pH ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = TRUE)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$pH,
    standard_error = standard_errors$pH,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_pH_bh_2019 <- rbind(date_results,results_df_pH_bh_2019)
}

results_df_pH_bh_2019$date<-as.Date(results_df_pH_bh_2019$date)

str(results_df_pH_bh_2019)
results_df_pH_bh_2019$Mean_sem <- paste(round(results_df_pH_bh_2019$mean, 2), "±", round(results_df_pH_bh_2019$standard_error, 2),results_df_pH_bh_2019$letters_ordered.groups)


str(results_df_pH_bh_2019)

results_df_pH_bh_2019$date[results_df_pH_bh_2019$treatment == "1" & results_df_pH_bh_2019$date == "2019-09-19"] <- "2019-09-12"

write.csv(results_df_pH_bh_2019,"data_output/results_df_pH_bh_2019.csv")
pd<- position_dodge(0.1)

BH_2019_pH_plot<-
  ggplot(results_df_pH_bh_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "pH") +
  theme(text=element_text(family="Helvetica")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=25, family = "serif")) +
  theme(axis.title.x = element_text(size=25, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2.7,3.9,0.2), limits = c (2.7,3.9)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  annotate("rect", xmin =  as.Date("08-12-2019", format ="%m-%d-%Y"), xmax = as.Date("08-16-2019", format ="%m-%d-%Y") ,  ymin = 2.7, ymax = 3.9,
           alpha = .2)+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  annotate("text",  x = as.Date("08-14-2019", format ="%m-%d-%Y"), y = 3.9, label ="HW2", size = 8) +
  theme(legend.position = "none")


ggsave(BH_2019_pH_plot, filename = "figures/BH_2019_pH_plot.pdf", device = cairo_pdf, width = 9, height = 7)

plot_means <- ggplot(results_df_pH_bh_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_pH_bh_2019 %>%
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

pH_bh_2019_with_letters <- BH_2019_pH_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 8, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(2.7,3.9,0.2), limits = c (2.7,3.9)) 

ggsave(pH_bh_2019_with_letters, filename = "figures/pH_bh_2019_with_letters.pdf", device = cairo_pdf, width = 9, height = 7)

#####COMBINE BRIX, TA, PH 2019####

library(cowplot)

panel_plot_berry_chemistry_BH_2019<- plot_grid(brix_bh_2019_with_letters, TA_bh_2019_with_letters, pH_bh_2019_with_letters, labels = c("A","B","C"), align = "v", axis = "l",ncol = 3, 
                                               vjust = 1.5, 
                                               hjust = -7, 
                                               label_size = 18
)

ggsave(panel_plot_berry_chemistry_BH_2019, filename = "figures/panel_plot_berry_chemistry_BH_2019_letters.pdf", device = cairo_pdf, width = 25, height =8)


######PRIMARY CHEMISTRY BH 2020####
berry_chemistry_borden_hills_2020 <-read.csv("data/borden_hills_berry_chemistry_2020.csv", header = TRUE)

str(berry_chemistry_borden_hills_2020)

berry_chemistry_borden_hills_2020 <-berry_chemistry_borden_hills_2020%>%
  filter(!is.na (rep))

berry_chemistry_borden_hills_2020$date<- mdy(berry_chemistry_borden_hills_2020$date)

str(berry_chemistry_borden_hills_2020$date)

tz(berry_chemistry_borden_hills_2020$date)

str(berry_chemistry_borden_hills_2020$date)

se <- function(x) sqrt(var(x)/length(x))

berry_chemistry_borden_hills_2020$treatment<-as.character(berry_chemistry_borden_hills_2020$treatment)

str(berry_chemistry_borden_hills_2020$treatment)

berry_chemistry_borden_hills_2020_avg_se <-berry_chemistry_borden_hills_2020 %>%
  group_by(treatment, date) %>%
  summarise(avg_brix = mean(brix), se_brix = se(brix), avg_ph = mean(pH), se_ph = se (pH), avg_ta = mean(TA), se_ta = se (TA))

write.csv(berry_chemistry_borden_hills_2020_avg_se,"data_output/berry_chemistry_borden_hills_2020_avg_se.csv")

berry_chemistry_borden_hills_2020 %>%
  group_by(treatment, date) %>%
  tally()

str(berry_chemistry_borden_hills_2020_avg_se)

berry_chemistry_borden_hills_2020_avg_se$treatment<-as.character(berry_chemistry_borden_hills_2020_avg_se$treatment)

berry_chemistry_borden_hills_2020_avg_se$date<-ymd(berry_chemistry_borden_hills_2020_avg_se$date)

str(berry_chemistry_borden_hills_2020_avg_se$date)
str(berry_chemistry_borden_hills_2020_avg_se$treatment)

pd <- position_dodge(1.3)




####ANOVA AND TUKEYS HSD BRIX 2020 #####

# Sample data frame structure (replace this with your actual data frame)

berry_chemistry_borden_hills_brix_2020_anova<-berry_chemistry_borden_hills_2020

berry_chemistry_borden_hills_brix_2020_anova%>%
  group_by(date, treatment)%>%
  tally() 

berry_chemistry_borden_hills_brix_2020_anova$treatment <- as.factor(berry_chemistry_borden_hills_brix_2020_anova$treatment)

str(berry_chemistry_borden_hills_brix_2020_anova$treatment)


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_brix_2020_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_brix_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_brix_2020_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( brix~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(brix ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(brix ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$brix,
    standard_error = standard_errors$brix,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_brix_bh_2020 <- rbind(date_results,results_df_brix_bh_2020)
}

results_df_brix_bh_2020$date<-as.Date(results_df_brix_bh_2020$date)

str(results_df_brix_bh_2020)
results_df_brix_bh_2020$Mean_sem <- paste(round(results_df_brix_bh_2020$mean, 2), "±", round(results_df_brix_bh_2020$standard_error, 2),results_df_brix_bh_2020$letters_ordered.groups)


str(results_df_brix_bh_2020)

write.csv(results_df_brix_bh_2020,"data_output/results_df_brix_bh_2020.csv")
pd<- position_dodge(0.1)

BH_2020_brix_plot<-
  ggplot(results_df_brix_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Total soluble solids (ºBx)") +
  # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(4,28,3), limits = c (4,28)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 4, ymax = 28,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 4, ymax = 28,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 4, ymax = 28,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 28, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 28, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 28, label ="HW4", size = 6)+
  theme(legend.position = "none") 

ggsave(BH_2020_brix_plot, filename = "figures/BH_2020_brix_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_brix_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_brix_bh_2020 %>%
  filter(p_value < 0.05)

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
         y_position = max_mean + vertical_offset * (row_number() - 2.5)) 



brix_bh_2020_with_letters <- BH_2020_brix_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 8, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(4,30,3), limits = c (4,30)) 

ggsave(brix_bh_2020_with_letters, filename = "figures/brix_bh_2020_with_letters.pdf", device = cairo_pdf, width = 10, height = 8)

####ANOVA AND TUKEYS HSD TA 2020 #####

# Sample data frame structure (replace this with your actual data frame)

berry_chemistry_borden_hills_TA_2020_anova<-berry_chemistry_borden_hills_2020

berry_chemistry_borden_hills_TA_2020_anova%>%
  group_by(date, treatment)%>%
  tally() 

berry_chemistry_borden_hills_TA_2020_anova$treatment <- as.factor(berry_chemistry_borden_hills_TA_2020_anova$treatment)

str(berry_chemistry_borden_hills_TA_2020_anova$treatment)


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_TA_2020_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_TA_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_TA_2020_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( TA~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(TA ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(TA ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$TA,
    standard_error = standard_errors$TA,
    letters_ordered = letters_ordered,
    significance =significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_TA_bh_2020 <- rbind(date_results,results_df_TA_bh_2020)
}

results_df_TA_bh_2020$date<-as.Date(results_df_TA_bh_2020$date)

str(results_df_TA_bh_2020)
results_df_TA_bh_2020$Mean_sem <- paste(round(results_df_TA_bh_2020$mean, 2), "±", round(results_df_TA_bh_2020$standard_error, 2),results_df_TA_bh_2020$letters_ordered.groups)


str(results_df_TA_bh_2020)

write.csv(results_df_TA_bh_2020,"data_output/results_df_TA_bh_2020.csv")
pd<- position_dodge(0.1)

BH_2020_TA_plot<-
  ggplot(results_df_TA_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Titratable acidity (g/L)") +
  # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2,46,4), limits = c (2,46)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 2, ymax = 46,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 2, ymax = 46,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,   ymin = 2, ymax = 46,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 46, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 46, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 46, label ="HW4", size = 6)+
  theme(legend.position = "none") 

ggsave(BH_2020_TA_plot, filename = "figures/BH_2020_TA_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_TA_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_TA_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability


vertical_offset <- 2.5  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 2)) 


TA_bh_2020_with_letters <- BH_2020_TA_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 8, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(2,46,4), limits = c (2,48))



ggsave(TA_bh_2020_with_letters, filename = "figures/TA_bh_2020_with_letters.pdf", device = cairo_pdf, width = 10, height = 8)


####ANOVA AND TUKEYS HSD pH 2020 #####

# Sample data frame structure (replace this with your actual data frame)

berry_chemistry_borden_hills_pH_2020_anova<-berry_chemistry_borden_hills_2020

berry_chemistry_borden_hills_pH_2020_anova%>%
  group_by(date, treatment)%>%
  tally() 

berry_chemistry_borden_hills_pH_2020_anova$treatment <- as.factor(berry_chemistry_borden_hills_pH_2020_anova$treatment)

str(berry_chemistry_borden_hills_pH_2020_anova$treatment)


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_pH_2020_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_pH_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_pH_2020_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( pH~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(pH ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(pH ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$pH,
    standard_error = standard_errors$pH,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_pH_bh_2020 <- rbind(date_results,results_df_pH_bh_2020)
}

results_df_pH_bh_2020$date<-as.Date(results_df_pH_bh_2020$date)

str(results_df_pH_bh_2020)
results_df_pH_bh_2020$Mean_sem <- paste(round(results_df_pH_bh_2020$mean, 2), "±", round(results_df_pH_bh_2020$standard_error, 2),results_df_pH_bh_2020$letters_ordered.groups)


str(results_df_pH_bh_2020)

write.csv(results_df_pH_bh_2020,"data_output/results_df_pH_bh_2020.csv")

pd<- position_dodge(0.1)

BH_2020_pH_plot<-
  ggplot(results_df_pH_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "pH") +
  # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2.4,3.8,0.3), limits = c (2.4,3.8)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 2.4, ymax = 3.8,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 2.4, ymax = 3.8,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,    ymin = 2.4, ymax = 3.8,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 3.8, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 3.8, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 3.8, label ="HW4", size = 6)+
  theme(legend.position = "none") 

ggsave(BH_2020_pH_plot, filename = "figures/BH_2020_pH_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_pH_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_pH_bh_2020 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability




vertical_offset <- 0.06# Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1.7)) 


pH_bh_2020_with_letters <- BH_2020_pH_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 8, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(2.4,3.8,0.3), limits = c (2.4,3.8))


ggsave(pH_bh_2020_with_letters, filename = "figures/pH_bh_2020_with_letters.pdf", device = cairo_pdf, width = 10, height = 8)


##### Combined plot with Bx, pH, TA 2020 ####

library(cowplot)


panel_plot_berry_chemistry_BH_2020<- plot_grid(brix_bh_2020_with_letters,TA_bh_2020_with_letters, pH_bh_2020_with_letters, labels = c("A","B","C"), align = "v", axis = "l",ncol = 3, 
                                               vjust = 1.5, 
                                               hjust = -9.6, 
                                               label_size = 18
)

ggsave(panel_plot_berry_chemistry_BH_2020, filename = "figures/panel_plot_berry_chemistry_BH_2020_no_letters.pdf", device = cairo_pdf, width = 25, height =8)


######PRIMARY CHEMISTRY BH 2021####
berry_chemistry_borden_hills_2021 <-read.csv("data/Primary_chemistry_bh_2021.csv", header = TRUE)

str(berry_chemistry_borden_hills_2021)

berry_chemistry_borden_hills_2021$date<- mdy(berry_chemistry_borden_hills_2021$date)

str(berry_chemistry_borden_hills_2021$date)

str(berry_chemistry_borden_hills_2021$pH<-as.numeric(berry_chemistry_borden_hills_2021$pH))
str(berry_chemistry_borden_hills_2021)

berry_chemistry_borden_hills_2021_tally<-berry_chemistry_borden_hills_2021%>%
  group_by(treatment,date)%>%
  tally()

se <- function(x) sqrt(var(x)/length(x))

berry_chemistry_borden_hills_2021$treatment<-as.character(berry_chemistry_borden_hills_2021$treatment)

str(berry_chemistry_borden_hills_2021$treatment)

berry_chemistry_borden_hills_2021_avg_se <-berry_chemistry_borden_hills_2021 %>%
  group_by(treatment, date) %>%
  summarise(avg_brix = mean(brix), se_brix = se(brix), avg_ph = mean(pH), se_ph = se (pH), avg_ta = mean(TA), se_ta = se (TA))

write.csv(berry_chemistry_borden_hills_2021_avg_se,"data_output/berry_chemistry_borden_hills_2021_avg_se.csv")

berry_chemistry_borden_hills_2021 %>%
  group_by(treatment, date) %>%
  tally()

str(berry_chemistry_borden_hills_2021_avg_se)

pd <- position_dodge(1.3)




####ANOVA AND TUKEYS HSD BRIX 2021 #####

# Sample data frame structure (replace this with your actual data frame)

berry_chemistry_borden_hills_brix_2021_anova<-berry_chemistry_borden_hills_2021

berry_chemistry_borden_hills_brix_2021_anova%>%
  group_by(date, treatment)%>%
  tally() 

berry_chemistry_borden_hills_brix_2021_anova$treatment <- as.factor(berry_chemistry_borden_hills_brix_2021_anova$treatment)

str(berry_chemistry_borden_hills_brix_2021_anova$treatment)


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_brix_2021_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_brix_bh_2021<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_brix_2021_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( brix~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(brix ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(brix ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$brix,
    standard_error = standard_errors$brix,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_brix_bh_2021 <- rbind(date_results,results_df_brix_bh_2021)
}

results_df_brix_bh_2021$date<-as.Date(results_df_brix_bh_2021$date)

str(results_df_brix_bh_2021)
results_df_brix_bh_2021$Mean_sem <- paste(round(results_df_brix_bh_2021$mean, 2), "±", round(results_df_brix_bh_2021$standard_error, 2),results_df_brix_bh_2021$letters_ordered.groups)


str(results_df_brix_bh_2021)

write.csv(results_df_brix_bh_2021,"data_output/results_df_brix_bh_2021.csv")
pd<- position_dodge(0.1)

BH_2021_brix_plot<-
  ggplot(results_df_brix_bh_2021, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Total soluble solids (ºBx)") +
  # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(4,28,3), limits = c (4,28)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 28, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 28, label = "HW2", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 4, ymax = 28,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 4, ymax = 28,
           alpha = .2) +
  theme(legend.position = "none") 

ggsave(BH_2021_brix_plot, filename = "figures/BH_2021_brix_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_brix_bh_2021, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_brix_bh_2021 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability


brix_bh_2021_with_letters <- BH_2021_brix_plot +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.7, hjust= -0.5, size = 6,color = "black", position = position_dodge(width = 0.8)) 

ggsave(brix_bh_2021_with_letters, filename = "figures/brix_bh_2021_with_letters.pdf", device = cairo_pdf, width = 10, height = 8)

####ANOVA AND TUKEYS HSD TA 2021 #####

# Sample data frame structure (replace this with your actual data frame)

berry_chemistry_borden_hills_TA_2021_anova<-berry_chemistry_borden_hills_2021

berry_chemistry_borden_hills_TA_2021_anova%>%
  group_by(date, treatment)%>%
  tally() 

berry_chemistry_borden_hills_TA_2021_anova$treatment <- as.factor(berry_chemistry_borden_hills_TA_2021_anova$treatment)

str(berry_chemistry_borden_hills_TA_2021_anova$treatment)


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_TA_2021_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_TA_bh_2021<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_TA_2021_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( TA~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(TA ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(TA ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$TA,
    standard_error = standard_errors$TA,
    letters_ordered = letters_ordered,
    significance =significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_TA_bh_2021 <- rbind(date_results,results_df_TA_bh_2021)
}

results_df_TA_bh_2021$date<-as.Date(results_df_TA_bh_2021$date)

str(results_df_TA_bh_2021)
results_df_TA_bh_2021$Mean_sem <- paste(round(results_df_TA_bh_2021$mean, 2), "±", round(results_df_TA_bh_2021$standard_error, 2),results_df_TA_bh_2021$letters_ordered.groups)


str(results_df_TA_bh_2021)

write.csv(results_df_TA_bh_2021,"data_output/results_df_TA_bh_2021.csv")
pd<- position_dodge(0.1)

BH_2021_TA_plot<-
  ggplot(results_df_TA_bh_2021, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "Titratable acidity (g/L)") +
  # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2,46,6), limits = c (2,46)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2021-05-15"), as.Date("2021-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("text", x = as.Date("09-08-2021", format= "%m-%d-%Y"), y = 46, label = "HW3", size = 6) +
  annotate("text", x = as.Date("07-10-2021", format= "%m-%d-%Y"), y = 46, label = "HW2", size = 6) +
  annotate("rect", xmin =  as.Date("2021-09-07", "%Y-%m-%d"), xmax = as.Date("2021-09-09", "%Y-%m-%d") , ymin = 2, ymax = 46,
           alpha = .2) +
  annotate("rect", xmin =  as.Date("2021-07-08", "%Y-%m-%d"), xmax = as.Date("2021-07-11", "%Y-%m-%d") , ymin = 2, ymax = 46,
           alpha = .2) +
  theme(legend.position = "none") 

ggsave(BH_2021_TA_plot, filename = "figures/BH_2021_TA_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_TA_bh_2021, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_TA_bh_2021 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability


TA_bh_2021_with_letters <- BH_2021_TA_plot +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.7, hjust= -0.5, size = 6,color = "black", position = position_dodge(width = 0.8)) 

ggsave(TA_bh_2021_with_letters, filename = "figures/TA_bh_2021_with_letters.pdf", device = cairo_pdf, width = 10, height = 8)


####ANOVA AND TUKEYS HSD pH 2021 #####

# Sample data frame structure (replace this with your actual data frame)

berry_chemistry_borden_hills_pH_2021_anova<-berry_chemistry_borden_hills_2021

berry_chemistry_borden_hills_pH_2021_anova%>%
  group_by(date, treatment)%>%
  tally() 

berry_chemistry_borden_hills_pH_2021_anova$treatment <- as.factor(berry_chemistry_borden_hills_pH_2021_anova$treatment)

str(berry_chemistry_borden_hills_pH_2021_anova$treatment)


# Initialize an empty data frame to store results
dates <- unique(berry_chemistry_borden_hills_pH_2021_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_pH_bh_2021<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- berry_chemistry_borden_hills_pH_2021_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( pH~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(pH ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(pH ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
  # Extract letters of significance
  test<- HSD.test(anova_result, trt = "treatment", alpha =0.05)
  letters<-(test$groups)
  initial_row_numbers <- as.numeric(row.names(letters))
  letters_ordered <- letters[order(initial_row_numbers), ]
  
  # Determine significance levels based on p-value
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*", "ns")))
  # Create a dataframe for results of the current date
  date_results <- data.frame(
    date = current_date,
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$pH,
    standard_error = standard_errors$pH,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_pH_bh_2021 <- rbind(date_results,results_df_pH_bh_2021)
}

results_df_pH_bh_2021$date<-as.Date(results_df_pH_bh_2021$date)

str(results_df_pH_bh_2021)
results_df_pH_bh_2021$Mean_sem <- paste(round(results_df_pH_bh_2021$mean, 2), "±", round(results_df_pH_bh_2021$standard_error, 2),results_df_pH_bh_2021$letters_ordered.groups)


str(results_df_pH_bh_2021)

write.csv(results_df_pH_bh_2021,"data_output/results_df_pH_bh_2021.csv")

pd<- position_dodge(0.1)

BH_2021_pH_plot<-
  ggplot(results_df_pH_bh_2021, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(label = "pH") +
  # ggtitle("2020") +
  theme(text=element_text(family="Helvetica")) +
  # theme(plot.title = element_text(hjust = 0.5, size = 25, face = "bold", family = "serif")) +
  xlab("Date") +
  theme(axis.title.y = element_text(size=23, family = "serif")) +
  theme(axis.title.x = element_text(size=23, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_y_continuous(breaks=seq(2.4,3.8,0.3), limits = c (2.4,3.8)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 2.4, ymax = 3.8,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 2.4, ymax = 3.8,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,    ymin = 2.4, ymax = 3.8,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 3.8, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 3.8, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 3.8, label ="HW4", size = 6)+
  theme(legend.position = "none") 

ggsave(BH_2021_pH_plot, filename = "figures/BH_2021_pH_plot.pdf", device = cairo_pdf, width = 13, height = 8)

plot_means <- ggplot(results_df_pH_bh_2021, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_pH_bh_2021 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability


pH_bh_2021_with_letters <- BH_2021_pH_plot +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.7, hjust= -0.5, size = 6,color = "black", position = position_dodge(width = 0.8)) 

ggsave(pH_bh_2021_with_letters, filename = "figures/pH_bh_2021_with_letters.pdf", device = cairo_pdf, width = 10, height = 8)


##### Combined plot with Bx, pH, TA 2020 ####

library(cowplot)


panel_plot_berry_chemistry_BH_2020<- plot_grid(BH_2021_brix_plot, BH_2021_TA_plot, BH_2021_pH_plot, labels = c("A","B","C"), align = "v", axis = "l",ncol = 3, 
                                               vjust = 1.5, 
                                               hjust = -9.6, 
                                               label_size = 18
)

ggsave(panel_plot_berry_chemistry_BH_2020, filename = "figures/panel_plot_berry_chemistry_BH_2020_no_letters.pdf", device = cairo_pdf, width = 25, height =8)
