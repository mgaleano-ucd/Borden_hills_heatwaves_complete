library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(agricolae)



monomeric_antho_concentration_2020_complete_season<- read.csv("data/raw_data_monomeric_anthocyanins_2020_complete_season_copy.csv", header =TRUE)

str(monomeric_antho_concentration_2020_complete_season$Date)

monomeric_antho_concentration_2020_complete_season$Date<-mdy(monomeric_antho_concentration_2020_complete_season$Date)

str(monomeric_antho_concentration_2020_complete_season)


monomeric_antho_concentration_2020_complete_season<-monomeric_antho_concentration_2020_complete_season%>%
filter(!is.na(Berry_numb))%>%
  filter(!is.na(mAU))
#  filter(!mAU == 0) %>%
#  filter(!Date == "2020-07-21")


monomeric_antho_concentration_2020_complete_season$mAU<-format(monomeric_antho_concentration_2020_complete_season$mAU)
monomeric_antho_concentration_2020_complete_season$mAU<-as.numeric(monomeric_antho_concentration_2020_complete_season$mAU)




monomeric_antho_concentration_2020_complete_season<-monomeric_antho_concentration_2020_complete_season%>%
  mutate(dilution_factor = (Extract_final_vol/Extract_ini_vol))%>%
  mutate(concetration_mg_l = ((mAU+297.43)/44.065)*dilution_factor)%>%
  mutate(antho_mg_berry = ((concetration_mg_l*Extract_ini_vol)/(1000*Berry_numb)))%>%
  mutate(antho_mg_g_berry_weight =((concetration_mg_l*Extract_ini_vol)/(1000*Berry_weight)))%>%
  mutate(antho_mg_g_Skin = ((concetration_mg_l*Extract_ini_vol)/(1000*Skin_weight_aft))) %>%
  mutate(Block_id = Sample)%>%
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
  )) %>%
  mutate(main_antho = case_when(
    short_name_antho == "D3G" ~ "Delphinidins",
    short_name_antho == "C3G" ~ "Cyanidins",
    short_name_antho == "C3Gac" ~ "Cyanidins",
    short_name_antho == "D3Gac" ~ "Delphinidins",
    short_name_antho == "M3G" ~ "Malvidins",
    short_name_antho == "M3Gac" ~ "Malvidins",
    short_name_antho == "M3Gpc" ~ "Malvidins",
    short_name_antho == "Peo3G" ~ "Peonidins",
    short_name_antho == "Peo3Gac" ~ "Peonidins",
    short_name_antho == "Peo3Gpc" ~ "Peonidins",
    short_name_antho == "Pet3G" ~ "Petunidins",
    short_name_antho == "Pet3Gac" ~ "Petunidins",
  )) %>%
  mutate(hydroxylation =case_when(
    short_name_antho == "D3G" ~ "trihydroxylated",
    short_name_antho == "C3G" ~ "dihydroxylated",
    short_name_antho == "C3Gac" ~ "dihydroxylated",
    short_name_antho == "D3Gac" ~ "trihydroxylated",
    short_name_antho == "M3G" ~ "trihydroxylated",
    short_name_antho == "M3Gac" ~ "trihydroxylated",
    short_name_antho == "M3Gpc" ~ "trihydroxylated",
    short_name_antho == "Peo3G" ~ "dihydroxylated",
    short_name_antho == "Peo3Gac" ~ "dihydroxylated",
    short_name_antho == "Peo3Gpc" ~ "dihydroxylated",
    short_name_antho == "Pet3G" ~ "trihydroxylated",
    short_name_antho == "Pet3Gac" ~ "trihydroxylated"
  )) %>%
  group_by(Block_id, Rep,treatment,Date)%>%
  mutate(total_antho_mg_berry = sum(antho_mg_berry))

write.csv(monomeric_antho_concentration_2020_complete_season,"data_output/monomeric_antho_concentration_2020_complete_season.csv")



total_antho_concentration_2020_summary <- monomeric_antho_concentration_2020_complete_season %>%
  mutate(dilution_factor = (Extract_final_vol/Extract_ini_vol)) %>%
  mutate(concetration_mg_l = ((mAU + 297.43) / 44.065) * dilution_factor) %>%
  mutate(antho_mg_berry = ((concetration_mg_l * Extract_ini_vol) / (1000 * Berry_numb))) %>%
  mutate(antho_mg_g_berry_weight = ((concetration_mg_l * Extract_ini_vol) / (1000 * Berry_weight))) %>%
  mutate(antho_mg_g_Skin = ((concetration_mg_l * Extract_ini_vol) / (1000 * Skin_weight_aft))) %>%
  mutate(Block_id = Sample) %>%
  mutate(treatment = case_when(
    Block_id == "B1R1" ~ 1,
    Block_id == "B1R3" ~ 1,
    Block_id == "B1R4" ~ 1, 
    Block_id == "B2R1" ~ 2,
    Block_id == "B2R2" ~ 2,
    Block_id == "B2R3" ~ 2,
    Block_id == "B3R1" ~ 3,
    Block_id == "B3R2" ~ 3,
    Block_id == "B3R3" ~ 3
  )) %>%
  mutate(main_antho = case_when(
    short_name_antho == "D3G" ~ "Delphinidins",
    short_name_antho == "C3G" ~ "Cyanidins",
    short_name_antho == "C3Gac" ~ "Cyanidins",
    short_name_antho == "D3Gac" ~ "Delphinidins",
    short_name_antho == "M3G" ~ "Malvidins",
    short_name_antho == "M3Gac" ~ "Malvidins",
    short_name_antho == "M3Gpc" ~ "Malvidins",
    short_name_antho == "Peo3G" ~ "Peonidins",
    short_name_antho == "Peo3Gac" ~ "Peonidins",
    short_name_antho == "Peo3Gpc" ~ "Peonidins",
    short_name_antho == "Pet3G" ~ "Petunidins",
    short_name_antho == "Pet3Gac" ~ "Petunidins"
  )) %>%
  mutate(hydroxylation = case_when(
    short_name_antho == "D3G" ~ "trihydroxylated",
    short_name_antho == "C3G" ~ "dihydroxylated",
    short_name_antho == "C3Gac" ~ "dihydroxylated",
    short_name_antho == "D3Gac" ~ "trihydroxylated",
    short_name_antho == "M3G" ~ "trihydroxylated",
    short_name_antho == "M3Gac" ~ "trihydroxylated",
    short_name_antho == "M3Gpc" ~ "trihydroxylated",
    short_name_antho == "Peo3G" ~ "dihydroxylated",
    short_name_antho == "Peo3Gac" ~ "dihydroxylated",
    short_name_antho == "Peo3Gpc" ~ "dihydroxylated",
    short_name_antho == "Pet3G" ~ "trihydroxylated",
    short_name_antho == "Pet3Gac" ~ "trihydroxylated"
  )) %>%
  group_by(Block_id, Rep, treatment, Date) %>%
  summarise(total_antho_mg_berry = sum(antho_mg_berry, na.rm = TRUE)) %>%
  ungroup()


total_antho_concentration_2020_summary_tally<-total_antho_concentration_2020_summary%>%
  group_by(treatment, Date)%>%
  tally()

total_antho_concentration_2020_summary_anova<-total_antho_concentration_2020_summary%>%
  mutate(date = Date)

str(total_antho_concentration_2020_summary_anova)


# Initialize an empty data frame to store results
dates <- unique(total_antho_concentration_2020_summary_anova$date) 
str(dates)
# Create an empty dataframe to store results

results_df_total_antho_hplc_bh_2020<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- total_antho_concentration_2020_summary_anova %>%
    filter(date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(total_antho_mg_berry~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- HSD.test(anova_result , trt = "treatment", alpha =0.05)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(total_antho_mg_berry ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(total_antho_mg_berry ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$total_antho_mg_berry,
    standard_error = standard_errors$total_antho_mg_berry,
    letters_ordered = letters_ordered,
    significance = significance
  )
  
  # Append results of the current date to the main results dataframe
  results_df_total_antho_hplc_bh_2020 <- rbind(date_results,results_df_total_antho_hplc_bh_2020)
}

results_df_total_antho_hplc_bh_2020$date<-as.Date(results_df_total_antho_hplc_bh_2020$date)

str(results_df_total_antho_hplc_bh_2020)
results_df_total_antho_hplc_bh_2020$Mean_sem <- paste(round(results_df_total_antho_hplc_bh_2020$mean, 2), "±", round(results_df_total_antho_hplc_bh_2020$standard_error, 2),results_df_total_antho_hplc_bh_2020$letters_ordered.groups)


str(results_df_total_antho_hplc_bh_2020)

write.csv(results_df_total_antho_hplc_bh_2020,"data_output/results_df_total_antho_hplc_bh_2020.csv")



BH_2020_total_antho_hplc_plot<-
  ggplot(results_df_total_antho_hplc_bh_2020, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "90-120% ET", "120-180% ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "90-120% ET", "120-180% ET"))+
  ylab(label = "Total anthocyanins (mg/berry)") +
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
  scale_y_continuous(breaks=seq(0,0.9,0.2), limits = c (0,0.9)) +
  scale_x_date(date_labels = "%b %d", breaks = seq(as.Date("2020-05-15"), as.Date("2020-09-16"), by = "12 days")) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18)) +
  annotate("rect", xmin =  as.Date("2020-07-09", "%Y-%m-%d"), xmax = as.Date("2020-07-12", "%Y-%m-%d") ,  ymin = 0, ymax = 0.9,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-08-13", "%Y-%m-%d"), xmax = as.Date("2020-08-19", "%Y-%m-%d") ,  ymin = 0, ymax = 0.9,
           alpha = .2)+
  annotate("rect", xmin =  as.Date("2020-09-05", "%Y-%m-%d"), xmax = as.Date("2020-09-08", "%Y-%m-%d") ,  ymin = 0, ymax = 0.9,
           alpha = .2)+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  annotate("text",  x = as.Date("2020-07-10", "%Y-%m-%d"), y = 0.9, label ="HW2", size = 6) +
  annotate("text",  x = as.Date("2020-08-16", "%Y-%m-%d"), y = 0.9, label ="HW3", size = 6)  +
  annotate("text",  x = as.Date("2020-09-07", "%Y-%m-%d"), y = 0.9, label ="HW4", size = 6)

ggsave(BH_2020_total_antho_hplc_plot, filename = "figures/BH_2020_total_antho_hplc_plot.jpg",  width = 13, height =8, dpi =600)


plot_means <- ggplot(results_df_total_antho_hplc_bh_2020, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_total_antho_hplc_bh_2020 %>%
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
         y_position = max_mean + vertical_offset * (row_number() - 0.06)) 



BH_2020_total_antho_hplc_plot_with_letters <- BH_2020_total_antho_hplc_plot +
  geom_text(data = significant_diffs, 
            aes(label = letters_ordered.groups, 
                x = date, 
                y = y_position), 
            vjust = -2,  # Adjust vertical justification
            size = 6, 
            position = position_dodge(width = 0), 
            color = "black") +
  scale_y_continuous(breaks=seq(0,0.9,0.2), limits = c (0,0.9)) 

ggsave(BH_2020_total_antho_hplc_plot_with_letters, filename = "figures/BH_2020_total_antho_hplc_plot_with_letters.jpg", width = 13, height =8, dpi =600)





