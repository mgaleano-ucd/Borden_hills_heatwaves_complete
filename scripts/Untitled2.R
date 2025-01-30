results_df_soil_temp_2020_MAX <- data.frame()

# Loop over each date
for (current_date in dates) {
  
  # Filter data for the current date
  data_subset <- BH_2020_soil_temp_5cm_anova %>%
    filter(Date == current_date) 
  
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Check if there are at least two levels in treatment and enough rows
  if (nlevels(data_subset$treatment) > 1 && nrow(data_subset) > 1) {
    # Perform one-way ANOVA
    anova_result <- aov(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset)
    
    # Check if ANOVA was valid before proceeding
    if (!any(is.na(summary(anova_result)[[1]]$`Pr(>F)`))) {
      # Perform Tukey's HSD test
      tukey_result <- HSD.test(anova_result, trt = "treatment", alpha = 0.05, unbalanced = TRUE)
      
      # Extract p-values
      p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
      
      # Extract means
      means <- aggregate(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset, FUN = mean)
      
      # Extract standard errors
      standard_errors <- aggregate(Daily_Max_Soil_Temp_TCAV ~ treatment, data = data_subset, FUN = function(x) sd(x) / sqrt(length(x)))
      
      # Extract letters of significance
      letters <- tukey_result$groups
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
      
      # Append results of the current date to the main results dataframe
      results_df_soil_temp_2020_MAX <- rbind(results_df_soil_temp_2020_MAX, date_results)
    } else {
      message(paste("ANOVA could not run due to NA p-values for date:", current_date))
    }
  } else {
    message(paste("Not enough levels in treatment or insufficient data for date:", current_date))
  }
}

# Print the final results
print(results_df_soil_temp_2020_MAX)

