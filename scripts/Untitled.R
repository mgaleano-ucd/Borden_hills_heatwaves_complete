
library(tidyverse)
library(grDevices)
library(ggpubr)
library(lubridate)
library(agricolae)
library(viridis)

##### MCP 2021#######

mcp_data_2021<- read.csv("data/bh_tannins_2021.csv", header = TRUE)


str(mcp_data_2021)

mcp_data_2021<- mcp_data_2021%>%
  mutate(total_tannin_mg_l = (((Abs_control-Abs_treatment)-0.0092)/0.0003))%>%
  mutate(Total_tannin_mg_berry = ((total_tannin_mg_l*final_extract_vol)/(1000*berry_number)))%>%
  mutate( treatment = case_when(
    block == "B1R1" ~ 1,
    block == "B1R3" ~ 1,
    block == "B1R4" ~ 1, 
    block == "B2R1" ~ 2,
    block == "B2R2" ~ 2,
    block == "B2R3" ~ 2,
    block == "B3R1" ~ 3,
    block == "B3R2" ~ 3,
    block == "B3R3" ~ 3,
  ))

  
  

se <- function(x) sqrt(var(x)/length(x))

str(mcp_data_2021$date)
mcp_data_2021$date<-mdy(mcp_data_2021$date)
str(mcp_data_2021$date)

mcp_data_2021_grouped<-mcp_data_2021%>%
  group_by(date, treatment)%>%
  tally()

####ANOVA AND TUKEYS HSD MCP 2021 #####

# Sample data frame structure (replace this with your actual data frame)

mcp_data_2021_anova<-mcp_data_2021


mcp_data_2021_anova<-mcp_data_2021_anova%>%
  mutate(Date = date)

mcp_data_2021_anova$treatment <- as.character(mcp_data_2021_anova$treatment)
str(mcp_data_2021_anova$treatment)

mcp_data_2021_anova_tally<-mcp_data_2021_anova%>%
  group_by(Date, treatment)%>%
  tally() 

mcp_data_2021_anova$treatment <- as.factor(mcp_data_2021_anova$treatment)

str(mcp_data_2021_anova$treatment)


# Initialize an empty data frame to store results
dates <- unique(mcp_data_2021_anova$Date) 
str(dates)
# Create an empty dataframe to store results

results_df_tannin_bh_2021<- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- mcp_data_2021_anova %>%
    filter(Date == current_date) 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov(Total_tannin_mg_berry ~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(Total_tannin_mg_berry ~ treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(Total_tannin_mg_berry ~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
    mean = means$Total_tannin_mg_berry,
    standard_error = standard_errors$Total_tannin_mg_berry,
    letters_ordered = letters_ordered
  )
  
  # Append results of the current date to the main results dataframe
  results_df_tannin_bh_2021 <- rbind(date_results,results_df_tannin_bh_2021)
}

results_df_tannin_bh_2021$date<-as.Date(results_df_tannin_bh_2021$date)

str(results_df_tannin_bh_2021)
results_df_tannin_bh_2021$Mean_sem <- paste(round(results_df_tannin_bh_2021$mean, 2), "±", round(results_df_tannin_bh_2021$standard_error, 2),results_df_tannin_bh_2021$letters_ordered.groups)


str(results_df_tannin_bh_2021)

write.csv(results_df_tannin_bh_2021,"data_output/results_df_tannin_bh_2021.csv")

pd<- position_dodge(0.1)

BH_2019_swp_midday_plot<-
  ggplot(results_df_tannin_bh_2021, aes(x = date, y = mean, group = treatment, color =treatment)) +
  geom_point(position = position_dodge(width = 0.8), size = 3, alpha =0.6) + 
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 0.8), size =1.1, width = 5, alpha =0.6) +
  geom_line(alpha =0.6, size =1.1, linetype = "solid", stat = "identity") +
  scale_shape_manual(values = c(16,15,17), name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab("Total tannins (mg/berry)")+
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
  scale_y_continuous(breaks=seq(-,-0.4,0.2), limits = c (-1.6,-0.4,-0.2)) +
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

plot_means <- ggplot(results_df_tannin_bh_2021, aes(x = date, y = mean, group = treatment)) +
  geom_point(position = position_dodge(width = 1.8), size = 3) +  # Points for means
  geom_errorbar(aes(ymin = mean - standard_error, ymax = mean + standard_error), 
                position = position_dodge(width = 1.8), width = 0.2) +  # Error bars for standard errors
  theme_minimal() +
  labs(x = "Date", y = "Mean ± Standard Error") +  # Axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Filter significant differences for annotation
significant_diffs <- results_df_tannin_bh_2021 %>%
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



