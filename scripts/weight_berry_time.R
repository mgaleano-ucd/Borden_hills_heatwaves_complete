library(tidyverse)
library(grDevices)
library(lubridate)
library(ggplot2)
library(agricolae)

####berry weight 2019#####

berry_weight_vs_time<- read.csv ("data/mcp_berry_phenolics.csv", header = TRUE)

se <- function(x) sqrt(var(x)/length(x))

berry_weight_vs_time_table<- berry_weight_vs_time %>%
  select(Date_sampled, Block_id, Rep,Berry_weight, Skin_weight_aft, treatment) %>%
  filter(!Block_id == "B1R2")%>%
  mutate(berry_weight_per_berry = (Berry_weight/60))



berry_weight_vs_time_table$Date_sampled<- mdy(berry_weight_vs_time_table$Date_sampled)

str(berry_weight_vs_time_table$Date_sampled)

tz(berry_weight_vs_time_table$Date_sampled)

berry_weight_vs_time_table$treatment <-format(berry_weight_vs_time_table$treatment)
as.character(berry_weight_vs_time_table$treatment)

berry_weight_vs_time_table$Date_sampled[berry_weight_vs_time_table$Date_sampled == "2019-09-12"] <- "2019-09-19"

berry_weight_vs_time_table<- berry_weight_vs_time_table%>%
  mutate(Date = Date_sampled)

berry_weight_vs_time_table$treatment <-
as.factor(berry_weight_vs_time_table$treatment)


BH_2019_berry_weight_anova<-berry_weight_vs_time_table
str(BH_2019_berry_weight_anova$treatment)
str(BH_2019_berry_weight_anova$Date)
str(BH_2019_berry_weight_anova$berry_weight_per_berry)

BH_2019_berry_weight_anova%>%
  group_by(Date, treatment)%>%
  tally()

pd<- position_dodge(0.5)


# Initialize an empty data frame to store results
dates <- unique(BH_2019_berry_weight_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_berry_weight_bh_2019<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- BH_2019_berry_weight_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(berry_weight_per_berry ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(berry_weight_per_berry ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(berry_weight_per_berry ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$berry_weight_per_berry,
    standard_error = standard_errors$berry_weight_per_berry,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_berry_weight_bh_2019 <- rbind(date_results,results_df_berry_weight_bh_2019)
}

results_df_berry_weight_bh_2019$date<-as.Date(results_df_berry_weight_bh_2019$date)

str(results_df_berry_weight_bh_2019)
results_df_berry_weight_bh_2019$Mean_sem <- paste(round(results_df_berry_weight_bh_2019$mean, 2), "±", round(results_df_berry_weight_bh_2019$standard_error, 2),results_df_berry_weight_bh_2019$letters_ordered.groups)


str(results_df_berry_weight_bh_2019)

results_df_berry_weight_bh_2019$date[results_df_berry_weight_bh_2019$treatment == "1" & results_df_berry_weight_bh_2019$date == "2019-09-19"] <- "2019-09-12"



BH_2019_berry_weight_plot<-
  ggplot(results_df_berry_weight_bh_2019, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression("Berry weight (gr)")) +
  ggtitle("2019") +
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
  scale_y_continuous(breaks=seq(0,1.3,0.2), limits = c (0,1.3)) +
  scale_x_date(date_labels="%b %d", date_breaks = ("7 day")) +
  annotate("text", x = as.Date("08-14-2019", format= "%m-%d-%Y"), y = 1.3, label = "HW2", size = 6) +
  annotate("rect", xmin =  as.Date("08-12-2019", format ="%m-%d-%Y"), xmax = as.Date("08-16-2019", format ="%m-%d-%Y") ,  ymin = 0.0, ymax = 1.3,
           alpha = .2)+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))


ggsave(BH_2019_berry_weight_plot, filename = "figures/BH_2019_berry_weight_plot.pdf", device = cairo_pdf, width = 9, height = 7)

plot_means <- ggplot(results_df_berry_weight_bh_2019, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Time", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_berry_weight_bh_2019 %>%
  filter(p_value < 0.05)

# Add letters of significance to the plot

plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))
# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points

BH_2019_berry_weight_plot_with_letters <- BH_2019_berry_weight_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,1.3,0.2), limits = c (0,1.3)) 

ggsave(BH_2019_berry_weight_plot_with_letters, filename = "figures/BH_2019_berry_weight_plot_with_letters.pdf", device = cairo_pdf, width = 9, height = 7)


####berry weight 2020#####

mcp_data_2020<- read.csv("data/bh_mcp_2020_complete_data.csv", header = TRUE)

mcp_data_2020<- mcp_data_2020%>%
  mutate(dilution_factor = (Extract_final_vol/Extract_ini_vol))%>%
  mutate(total_tannin_mg_l = ((((Control-Treated)-0.01107)/0.00033)*dilution_factor))%>%
  mutate(Total_tannin_mg_berry = ((total_tannin_mg_l*Extract_ini_vol)/(1000*Berry_numb)))%>%
  mutate(Total_tannin_mg_g_berry_weight =((total_tannin_mg_l*Extract_ini_vol)/(1000*Berry_weight)))%>%
  mutate(Total_tannin_mg_g_skin = ((total_tannin_mg_l*Extract_ini_vol)/(1000*Skin_weight_aft)))%>%
  mutate( treatment = case_when(
    Block_id == "B1R1" ~ 1,
    Block_id == "B1R3" ~ 1,
    Block_id == "B1R4" ~ 1, 
    Block_id == "B2R1" ~ 2,
    Block_id == "B2R2" ~ 2,
    Block_id == "B2R3" ~ 2,
    Block_id == "B3R1" ~ 3,
    Block_id == "B3R2" ~ 3,
    Block_id == "B3R3" ~ 3,
  ))%>%
  filter(!is.na(Total_tannin_mg_g_berry_weight)) 



mcp_data_2020<-  mcp_data_2020[-292, ] ##skin weight was 12 grams for sample. Some error during extraction 

str(mcp_data_2020$Date_sample)


mcp_data_2020_grouped<-mcp_data_2020%>%
  group_by(Date_sampled, treatment)%>%
  tally()
mcp_data_2020$Date_sampled<-mdy(mcp_data_2020$Date_sampled)
str(mcp_data_2020$Date_sampled)


berry_weight_bh_2020<-mcp_data_2020%>%
  mutate(berry_weight_per_berry = Berry_weight/Berry_numb)

str(berry_weight_bh_2020$berry_weight_per_berry)

berry_weight_bh_2020$treatment<-as.character(berry_weight_bh_2020$treatment)

str(berry_weight_bh_2020$treatment)

BH_2020_berry_weight_anova<- berry_weight_bh_2020%>%
  mutate(Date = Date_sampled)


se <- function(x) sqrt(var(x)/length(x))


BH_2020_berry_weight_anova_tally<-BH_2020_berry_weight_anova%>% group_by(Date, treatment)%>%
  tally()


BH_2020_berry_weight_anova$treatment <- as.factor(BH_2020_berry_weight_anova$treatment)

str(BH_2020_berry_weight_anova$treatment)
str(BH_2020_berry_weight_anova$Date)


# Initialize an empty data frame to store results
dates <- unique(BH_2020_berry_weight_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_berry_weight_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- BH_2020_berry_weight_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(berry_weight_per_berry ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(berry_weight_per_berry ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(berry_weight_per_berry ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$berry_weight_per_berry,
    standard_error = standard_errors$berry_weight_per_berry,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_berry_weight_bh_2020 <- rbind(date_results,results_df_berry_weight_bh_2020)
}

results_df_berry_weight_bh_2020$date<-as.Date(results_df_berry_weight_bh_2020$date)

str(results_df_berry_weight_bh_2020)
results_df_berry_weight_bh_2020$Mean_sem <- paste(round(results_df_berry_weight_bh_2020$mean, 2), "±", round(results_df_berry_weight_bh_2020$standard_error, 2),results_df_berry_weight_bh_2020$letters_ordered.groups)


str(results_df_berry_weight_bh_2020)

pd<- position_dodge(0.1)

plot_means <- ggplot(results_df_berry_weight_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_berry_weight_bh_2020 %>%
  filter(p_value < 0.05)


# Add letters of significance to the plot
plot_with_letters <- plot_means +
  geom_text(data = significant_diffs, aes(label = letters_ordered.groups), vjust = -0.5, hjust= -0.5, size = 4, position = position_dodge(width = 0.8))# Rotate x-axis labels for better readability


BH_2020_berry_weight_plot<-
  ggplot(results_df_berry_weight_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression("Berry weight (gr)")) +
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
  scale_y_continuous(breaks=seq(0,1.3,0.2), limits = c (0,1.3,0.2)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 0.0, ymax = 1.3,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 0.0, ymax = 1.3,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 0.0, ymax = 1.3,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 1.3, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 1.3, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 1.3, label ="HW4", size = 6) +
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank()) 


ggsave(BH_2020_berry_weight_plot, filename = "figures/BH_2020_berry_weight_plot.pdf", device = cairo_pdf, width = 12, height = 8)



# Rotate x-axis labels for better readability
vertical_offset <- 0.04  # Adjust this value based on your desired spacing

# Calculate the maximum mean value for each date
significant_diffs <- significant_diffs %>%
  arrange(date, desc(letters_ordered.groups)) %>%  # Sort by interval and descending order of significance letters
  group_by(date) %>%
  mutate(max_mean = max(mean),
         y_position = max_mean + vertical_offset * (row_number() - 1)) 

# Create the plot with letters of significance above the max mean points

BH_2020_berry_weight_plot_with_letters <- BH_2020_berry_weight_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,1.3,0.2), limits = c (0,1.3)) 


ggsave(BH_2020_berry_weight_plot_with_letters, filename = "figures/BH_2020_berry_weight_plot_with_letters.pdf", device = cairo_pdf, width = 12, height = 8)


#####Plot Berry weight together 2019 and 2020######

library(cowplot)

panel_plot_berry_weight_2019_2020_BH <- plot_grid(
  BH_2019_berry_weight_plot_with_letters,
  BH_2020_berry_weight_plot_with_letters,
  labels =c ("     A","     B"),
  vjust = 4,
  hjust = -1.5, 
  label_size = 18,
  ncol = 2, 
  align = "hv",
  axis = "tblr" 
)

ggsave(panel_plot_berry_weight_2019_2020_BH , filename = "figures/panel_plot_berry_weight_2019_2020_BH.pdf", device = cairo_pdf, width = 16, height = 8)


