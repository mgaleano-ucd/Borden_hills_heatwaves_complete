library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(ggpmisc)

diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019 <-diurnals_borden_hills_2019%>%
  filter(!pixel_number == 34 )


diurnals_borden_hills_2019$round<-format(diurnals_borden_hills_2019$round)
diurnals_borden_hills_2019$round<-as.numeric(as.factor(diurnals_borden_hills_2019$round))

str(diurnals_borden_hills_2019$round)

str(diurnals_borden_hills_2019)

diurnals_2019_A_vs_leafT <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-")


diurnals_2019_A_vs_leafT$time<- format(strptime(diurnals_2019_A_vs_leafT$time,"%H:%M:%S"), format = "%H:%M", tz = "UTC")

str(diurnals_2019_A_vs_leafT$time)



diurnals_2019_A_vs_leafT$datetime <- paste(diurnals_2019_A_vs_leafT$date, " ", diurnals_2019_A_vs_leafT$time, sep = "")

glimpse(diurnals_2019_A_vs_leafT) 

diurnals_2019_A_vs_leafT$datetime <- ymd_hm(diurnals_2019_A_vs_leafT$datetime, tz = "UTC")

str(diurnals_2019_A_vs_leafT)

tz(diurnals_2019_A_vs_leafT$datetime)

tz(diurnals_2019_A_vs_leafT$time)

diurnals_2019_A_vs_leafT$leaf_temp_C <- format(diurnals_2019_A_vs_leafT $leaf_temp_C)
diurnals_2019_A_vs_leafT$leaf_temp_C<-as.numeric(diurnals_2019_A_vs_leafT$leaf_temp_C)

str(diurnals_2019_A_vs_leafT$datetime)

str(diurnals_2019_A_vs_leafT$leaf_temp_C)




#### A vs LICOR leaf T FISRT HW  2019 round 3-4 ####

diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 215) %>%
  filter(day > 205) %>%
  filter(round > 2) %>%
  filter(round < 5)

diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4%>%
  group_by(treatment)%>%
  tally()


str(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$Tleaf)


diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment<- reorder(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment, diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$Tleaf)

str(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$Rep<-format(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$Rep)
as.character(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$Rep)

diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment)

str(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4$treatment)


## Plot first HW RESPONSE


A_vs_leafT_licor_first_HW_r_3_4 <-ggplot(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Net~photosynthesis~(µmol~m^{-2}~s^{-1}))) +
  ggtitle("HW1 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(30,50,5), limits = c (30,50)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (-1,30))  +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none") 

write.csv(diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4,"data_output/diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4.csv")


ggsave(A_vs_leafT_licor_first_HW_r_3_4, filename = "figures/A_vs_leafT_licor_first_HW_r_3_4_without_leaky_pixel.pdf", device = cairo_pdf, 
       width = 8, height = 6)

### Finding percentage of decrease from A max to highest Tleaf temp####

# Function to calculate A for a given temperature range using the fitted model
calculate_A_values <- function(model, Tleaf_range) {
  a <- coef(model)[3]
  b <- coef(model)[2]
  c <- coef(model)[1]
  A_values <- a * Tleaf_range^2 + b * Tleaf_range + c
  data.frame(Tleaf = Tleaf_range, A = A_values)
}

# Fit quadratic models for each treatment group and calculate the desired values
max_A_values <- diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarise(
    model = list(lm(A ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
    min_Tleaf = min(Tleaf),
    max_Tleaf = max(Tleaf)
  ) %>%
  rowwise() %>%
  mutate(
    # Calculate A values within the observed range of Tleaf
    Tleaf_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
    fitted_values = list(calculate_A_values(model, Tleaf_range)),
    
    # Find the max A value within the observed Tleaf range
    max_A_within_range = max(fitted_values$A, na.rm = TRUE),
    Tleaf_at_max_A = fitted_values$Tleaf[which.max(fitted_values$A)],
    
    # Calculate A at the maximum Tleaf observed
    A_at_max_Tleaf = predict(model, newdata = data.frame(Tleaf = max_Tleaf)),
    
    # Calculate percentage decrease
    percentage_decrease = 100 * (max_A_within_range - A_at_max_Tleaf) / max_A_within_range
  ) %>%
  select(treatment, max_A_within_range, Tleaf_at_max_A, A_at_max_Tleaf, percentage_decrease)

print(max_A_values)
####95% CONFIDENCE INTERVALS #####

# Fit quadratic models for each treatment group
models <- diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(A ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Function to calculate predicted A and confidence intervals for given temperature range
calculate_predictions <- function(model, temp_range) {
  pred_data <- data.frame(Tleaf = temp_range)
  predictions <- predict(model, newdata = pred_data, interval = "confidence", level = 0.95)
  data.frame(Tleaf = temp_range, A = predictions[, "fit"], lwr = predictions[, "lwr"], upr = predictions[, "upr"])
}

# Calculate predictions for the observed range and find the max A
max_values <- models %>%
  rowwise() %>%
  mutate(temp_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
         fitted_values = list(calculate_predictions(model, temp_range)),
         max_A = max(fitted_values$A),
         max_Tleaf = fitted_values$Tleaf[which.max(fitted_values$A)],
         max_lwr = fitted_values$lwr[which.max(fitted_values$A)],
         max_upr = fitted_values$upr[which.max(fitted_values$A)]) %>%
  select(treatment, max_Tleaf, max_A, max_lwr, max_upr)

print(max_values)

### non overlapping confindence intervals ####

# Fit quadratic models for each treatment group
models <- diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(A ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Define the temperature range (25 to 50 degrees Celsius)
temp_range <- seq(25, 50, by = 0.1)

# Function to calculate predicted A and confidence intervals for a given temperature range
calculate_predictions <- function(model, temp_range) {
  pred_data <- data.frame(Tleaf = temp_range)
  predictions <- predict(model, newdata = pred_data, interval = "confidence", level = 0.95)
  data.frame(Tleaf = temp_range, A = predictions[, "fit"], lwr = predictions[, "lwr"], upr = predictions[, "upr"])
}

# Calculate predictions for the temperature range for each treatment group
predictions <- models %>%
  rowwise() %>%
  mutate(predictions = list(calculate_predictions(model, temp_range))) %>%
  select(treatment, predictions) %>%
  unnest(predictions)

# Plot the fitted curves with confidence intervals
A_vs_leafT_licor_first_HW_r_3_4_confidence_intervals <- ggplot(predictions, aes(x = Tleaf, y = A, group = treatment, color = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab("Net photosynthesis (mol m⁻² s⁻¹)") +
  ggtitle("HW2 2019") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif"),
    axis.title.y = element_text(size = 22, family = "serif"),
    axis.title.x = element_text(size = 22, family = "serif"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.2, "cm"),
    legend.justification = "center",
    legend.position = "right",
    legend.title.align = 0)


print(A_vs_leafT_licor_first_HW_r_3_4)

# Find non-overlapping intervals of treatment 1 with treatments 2 and 3
non_overlapping_intervals <- predictions %>%
  filter(treatment == 1) %>%
  rename(lwr_1 = lwr, upr_1 = upr, A_1 = A) %>%
  left_join(predictions %>% filter(treatment == 2) %>% rename(lwr_2 = lwr, upr_2 = upr, A_2 = A), by = "Tleaf") %>%
  left_join(predictions %>% filter(treatment == 3) %>% rename(lwr_3 = lwr, upr_3 = upr, A_3 = A), by = "Tleaf") %>%
  filter(
    (upr_1 < lwr_2 | lwr_1 > upr_2) &
      (upr_1 < lwr_3 | lwr_1 > upr_3)
  ) %>%
  select(Tleaf, A_1, lwr_1, upr_1)

print(non_overlapping_intervals)

#####Non overlapping confinde intervals in temperature ranges ####



# Fit quadratic models for each treatment group
models <- diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(A ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Define the temperature range (25 to 50 degrees Celsius)
temp_range <- seq(25, 50, by = 0.1)

# Function to calculate predicted A and confidence intervals for a given temperature range
calculate_predictions <- function(model, temp_range) {
  pred_data <- data.frame(Tleaf = temp_range)
  predictions <- predict(model, newdata = pred_data, interval = "confidence", level = 0.95)
  data.frame(Tleaf = temp_range, A = predictions[, "fit"], lwr = predictions[, "lwr"], upr = predictions[, "upr"])
}

# Calculate predictions for the temperature range for each treatment group
predictions <- models %>%
  rowwise() %>%
  mutate(predictions = list(calculate_predictions(model, temp_range))) %>%
  select(treatment, predictions) %>%
  unnest(predictions)

# Plot the fitted curves with confidence intervals
A_vs_leafT_licor_first_HW_r_3_4_fitted_curve <- ggplot(predictions, aes(x = Tleaf, y = A, group = treatment, color = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET")) +
  ylab("Net photosynthesis (mol m⁻² s⁻¹)") +
  ggtitle("HW2 2019") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif"),
    axis.title.y = element_text(size = 22, family = "serif"),
    axis.title.x = element_text(size = 22, family = "serif"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.2, "cm"),
    legend.justification = "center",
    legend.position = "right",
    legend.title.align = 0
  ) 

print(A_vs_leafT_licor_first_HW_r_3_4)

# Find non-overlapping intervals of treatment 1 with treatments 2 and 3
non_overlapping_intervals <- predictions %>%
  filter(treatment == 1) %>%
  rename(lwr_1 = lwr, upr_1 = upr, A_1 = A) %>%
  left_join(predictions %>% filter(treatment == 2) %>%
              rename(lwr_2 = lwr, upr_2 = upr, A_2 = A), by = "Tleaf") %>%
  left_join(predictions %>% filter(treatment == 3) %>%
              rename(lwr_3 = lwr, upr_3 = upr, A_3 = A), by = "Tleaf") %>%
  filter((upr_1 < lwr_2 | lwr_1 > upr_2) &
           (upr_1 < lwr_3 | lwr_1 > upr_3)) %>%
  select(Tleaf, A_1, lwr_1, upr_1)

# Identify contiguous non-overlapping intervals
non_overlapping_intervals <- non_overlapping_intervals %>%
  mutate(group = cumsum(c(TRUE, diff(Tleaf) != 0.1)))

# Split into ranges
ranges <- non_overlapping_intervals %>%
  group_by(group) %>%
  summarize(start_Tleaf = min(Tleaf), end_Tleaf = max(Tleaf), .groups = 'drop')

print(ranges)

#### A vs LICOR leaf T SECOND HW  2019 round 3-4 ####

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, A, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 214) %>%
  filter(round > 2) %>%
  filter(round < 5)

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4%>%
  group_by(treatment)%>%
  tally()

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Tleaf)


diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment<- reorder(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment, diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Tleaf)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Rep<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Rep)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$Rep)

diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment<-format(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)
as.character(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)

str(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4$treatment)


## Plot second HW RESPONSE
A_vs_leafT_licor_second_HW_r_3_4 <-ggplot(diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4, aes(Tleaf, A, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = A, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Net photosynthesis (mol m⁻² s⁻¹)") +
  ggtitle("HW2 2019")+
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(25,50,5), limits = c (25,50)) +
  scale_y_continuous(breaks=seq(0,30,5), limits = c (0,30)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
  theme(axis.title.x = element_text(color = "white"), axis.text.x = element_text(color = "white"), axis.ticks.x = element_blank())+
  theme(legend.position = "none") 

ggsave(A_vs_leafT_licor_second_HW_r_3_4, filename = "figures/A_vs_leafT_licor_second_HW_r_3_4_without_leaky_pixel.pdf", device = cairo_pdf, 
       width = 8, height = 6)

# Fit quadratic models for each treatment group
models <- diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(A ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Function to calculate A for given temperature range
calculate_A <- function(model, temp_range) {
  a <- coef(model)[3]
  b <- coef(model)[2]
  c <- coef(model)[1]
  A_values <- a * temp_range^2 + b * temp_range + c
  data.frame(Tleaf = temp_range, A = A_values)
}

# Calculate A values for the observed range and find the max A
max_values <- models %>%
  rowwise() %>%
  mutate(temp_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
         fitted_values = list(calculate_A(model, temp_range)),
         max_A = max(fitted_values$A),
         max_Tleaf = fitted_values$Tleaf[which.max(fitted_values$A)]) %>%
  select(treatment, max_Tleaf, max_A)

print(max_values)

### Finding percentage of decrease from A max to highest Tleaf temp####
# Function to calculate A for a given temperature range using the fitted model
calculate_A_values <- function(model, Tleaf_range) {
  a <- coef(model)[3]
  b <- coef(model)[2]
  c <- coef(model)[1]
  A_values <- a * Tleaf_range^2 + b * Tleaf_range + c
  data.frame(Tleaf = Tleaf_range, A = A_values)
}

# Fit quadratic models for each treatment group and calculate the desired values
max_A_values <- diurnals_2019_A_vs_leafT_licor_second_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarise(
    model = list(lm(A ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
    min_Tleaf = min(Tleaf),
    max_Tleaf = max(Tleaf)
  ) %>%
  rowwise() %>%
  mutate(
    # Calculate A values within the observed range of Tleaf
    Tleaf_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
    fitted_values = list(calculate_A_values(model, Tleaf_range)),
    
    # Find the max A value within the observed Tleaf range
    max_A_within_range = max(fitted_values$A, na.rm = TRUE),
    Tleaf_at_max_A = fitted_values$Tleaf[which.max(fitted_values$A)],
    
    # Calculate A at the maximum Tleaf observed
    A_at_max_Tleaf = predict(model, newdata = data.frame(Tleaf = max_Tleaf)),
    
    # Calculate percentage decrease
    percentage_decrease = 100 * (max_A_within_range - A_at_max_Tleaf) / max_A_within_range
  ) %>%
  select(treatment, max_A_within_range, Tleaf_at_max_A, A_at_max_Tleaf, percentage_decrease)

print(max_A_values)
####95% CONFIDENCE INTERVALS #####

# Fit quadratic models for each treatment group
models <- diurnals_2019_A_vs_leafT_licor_first_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(A ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Function to calculate predicted A and confidence intervals for given temperature range
calculate_predictions <- function(model, temp_range) {
  pred_data <- data.frame(Tleaf = temp_range)
  predictions <- predict(model, newdata = pred_data, interval = "confidence", level = 0.95)
  data.frame(Tleaf = temp_range, A = predictions[, "fit"], lwr = predictions[, "lwr"], upr = predictions[, "upr"])
}

# Calculate predictions for the observed range and find the max A
max_values <- models %>%
  rowwise() %>%
  mutate(temp_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
         fitted_values = list(calculate_predictions(model, temp_range)),
         max_A = max(fitted_values$A),
         max_Tleaf = fitted_values$Tleaf[which.max(fitted_values$A)],
         max_lwr = fitted_values$lwr[which.max(fitted_values$A)],
         max_upr = fitted_values$upr[which.max(fitted_values$A)]) %>%
  select(treatment, max_Tleaf, max_A, max_lwr, max_upr)

print(max_values)

####GSW VS T HEATWAVES BH 2019####
diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019 <-diurnals_borden_hills_2019%>%
  filter(!pixel_number == 34 )%>%
  select(-date)

str(diurnals_borden_hills_2019)

diurnals_borden_hills_2019$date <- as.Date(paste(diurnals_borden_hills_2019$day, "2019"), format = " %j %Y")

str(diurnals_borden_hills_2019$date)

diurnals_borden_hills_2019$round<-format(diurnals_borden_hills_2019$round)
diurnals_borden_hills_2019$round<-as.numeric(as.factor(diurnals_borden_hills_2019$round))

str(diurnals_borden_hills_2019$round)


str(diurnals_borden_hills_2019)

diurnals_2019_gsw_vs_leafT <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time)) %>%
  filter(!is.na(leaf_temp_C)) %>%
  filter(!leaf_temp_C == "-")


diurnals_2019_gsw_vs_leafT$time<- format(strptime(diurnals_2019_gsw_vs_leafT$time,"%H:%M:%S"), format = "%H:%M", tz = "UTC")

str(diurnals_2019_gsw_vs_leafT$time)



diurnals_2019_gsw_vs_leafT$datetime <- paste(diurnals_2019_gsw_vs_leafT$date, " ", diurnals_2019_gsw_vs_leafT$time, sep = "")

glimpse(diurnals_2019_gsw_vs_leafT) 

diurnals_2019_gsw_vs_leafT$datetime <- ymd_hm(diurnals_2019_gsw_vs_leafT$datetime, tz = "UTC")

str(diurnals_2019_gsw_vs_leafT)

tz(diurnals_2019_gsw_vs_leafT$datetime)

tz(diurnals_2019_gsw_vs_leafT$time)

diurnals_2019_gsw_vs_leafT$leaf_temp_C <- format(diurnals_2019_gsw_vs_leafT $leaf_temp_C)
diurnals_2019_gsw_vs_leafT$leaf_temp_C<-as.numeric(diurnals_2019_gsw_vs_leafT$leaf_temp_C)

str(diurnals_2019_gsw_vs_leafT$datetime)

str(diurnals_2019_gsw_vs_leafT$leaf_temp_C)


####GSW  vs LICOR leaf T FISRT HW round 3-4 ####

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 215) %>%
  filter(day > 205) %>%
  filter(round > 2) %>%
  filter(round < 5)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4%>%
  group_by(treatment)%>%
  tally()
str(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Tleaf)


diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment<- reorder(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment, diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Tleaf)

str(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Rep<-format(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Rep)
as.character(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$Rep)

diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)

str(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4$treatment)


## Plot first HW RESPONSE
gsw_vs_leafT_licor_first_HW_r_3_4 <-ggplot(diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(30,50,5), limits = c (30,50)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1), limits = c (0,0.78)) +
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20)) +
  theme(legend.position = "none") 

ggsave(gsw_vs_leafT_licor_first_HW_r_3_4, filename = "figures/gsw_vs_leafT_licor_first_HW_r_3_4_without_leaky_pixel.pdf", device = cairo_pdf, 
       width = 8, height = 6)

#####Confidence intervarls upper and lower####

# Fit quadratic models for each treatment group
models <- diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(gsw ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Function to calculate A for given temperature range
calculate_gsw <- function(model, temp_range) {
  a <- coef(model)[3]
  b <- coef(model)[2]
  c <- coef(model)[1]
  gsw_values <- a * temp_range^2 + b * temp_range + c
  data.frame(Tleaf = temp_range, gsw = gsw_values)
}

# Calculate A values for the observed range and find the max A
max_values <- models %>%
  rowwise() %>%
  mutate(temp_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
         fitted_values = list(calculate_gsw(model, temp_range)),
         max_gsw = max(fitted_values$gsw),
         max_Tleaf = fitted_values$Tleaf[which.max(fitted_values$gsw)]) %>%
  select(treatment, max_Tleaf, max_gsw)

print(max_values)


# Fit quadratic models for each treatment group
models <- diurnals_2019_gsw_vs_leafT_licor_first_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(gsw ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Function to calculate predicted A and confidence intervals for given temperature range
calculate_predictions <- function(model, temp_range) {
  pred_data <- data.frame(Tleaf = temp_range)
  predictions <- predict(model, newdata = pred_data, interval = "confidence", level = 0.95)
  data.frame(Tleaf = temp_range, gsw = predictions[, "fit"], lwr = predictions[, "lwr"], upr = predictions[, "upr"])
}

# Calculate predictions for the observed range and find the max A
max_values <- models %>%
  rowwise() %>%
  mutate(temp_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
         fitted_values = list(calculate_predictions(model, temp_range)),
         max_gsw = max(fitted_values$gsw),
         max_Tleaf = fitted_values$Tleaf[which.max(fitted_values$gsw)],
         max_lwr = fitted_values$lwr[which.max(fitted_values$gsw)],
         max_upr = fitted_values$upr[which.max(fitted_values$gsw)]) %>%
  select(treatment, max_Tleaf, max_gsw, max_lwr, max_upr)

print(max_values)

####GSW vs LICOR leaf T SECOND HW round 3-4 ####

diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day < 233) %>%
  filter(day > 214) %>%
  filter(round > 2) %>%
  filter(round < 5)

str(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$Tleaf)


diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4%>%
  group_by(treatment)%>%
  tally()

diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment<- reorder(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment, diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$Tleaf)

str(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment)

diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$Rep<-format(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$Rep)
as.character(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$Rep)

diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment)

str(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_second_HW_r_3_4 <-ggplot(diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label), stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab ("Stomatal conductance ( mol m⁻² s⁻¹)") +
  theme(plot.title = element_text(hjust = 0.5, size = 23, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=22, family = "serif")) +
  theme(axis.title.x = element_text(size=22, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(25,50,5), limits = c (25,50)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1), limits = c (0,0.78))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())+
  theme(legend.position = "none") 


ggsave(gsw_vs_leafT_licor_second_HW_r_3_4, filename = "figures/gsw_vs_leafT_licor_second_HW_r_3_4_without_leaky_pixel.pdf", device = cairo_pdf, 
       width = 8, height = 6)



#####Confidence intervarls upper and lower####

# Fit quadratic models for each treatment group
models <- diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(gsw ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Function to calculate A for given temperature range
calculate_gsw <- function(model, temp_range) {
  a <- coef(model)[3]
  b <- coef(model)[2]
  c <- coef(model)[1]
  gsw_values <- a * temp_range^2 + b * temp_range + c
  data.frame(Tleaf = temp_range, gsw = gsw_values)
}

# Calculate A values for the observed range and find the max A
max_values <- models %>%
  rowwise() %>%
  mutate(temp_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
         fitted_values = list(calculate_gsw(model, temp_range)),
         max_gsw = max(fitted_values$gsw),
         max_Tleaf = fitted_values$Tleaf[which.max(fitted_values$gsw)]) %>%
  select(treatment, max_Tleaf, max_gsw)

print(max_values)


# Fit quadratic models for each treatment group
models <- diurnals_2019_gsw_vs_leafT_licor_second_HW_r_3_4 %>%
  group_by(treatment) %>%
  summarize(model = list(lm(gsw ~ poly(Tleaf, 2, raw = TRUE), data = cur_data())),
            min_Tleaf = min(Tleaf),
            max_Tleaf = max(Tleaf))

# Function to calculate predicted A and confidence intervals for given temperature range
calculate_predictions <- function(model, temp_range) {
  pred_data <- data.frame(Tleaf = temp_range)
  predictions <- predict(model, newdata = pred_data, interval = "confidence", level = 0.95)
  data.frame(Tleaf = temp_range, gsw = predictions[, "fit"], lwr = predictions[, "lwr"], upr = predictions[, "upr"])
}

# Calculate predictions for the observed range and find the max A
max_values <- models %>%
  rowwise() %>%
  mutate(temp_range = list(seq(min_Tleaf, max_Tleaf, by = 0.1)),
         fitted_values = list(calculate_predictions(model, temp_range)),
         max_gsw = max(fitted_values$gsw),
         max_Tleaf = fitted_values$Tleaf[which.max(fitted_values$gsw)],
         max_lwr = fitted_values$lwr[which.max(fitted_values$gsw)],
         max_upr = fitted_values$upr[which.max(fitted_values$gsw)]) %>%
  select(treatment, max_Tleaf, max_gsw, max_lwr, max_upr)

print(max_values)

####Plotting all together####

library(cowplot)

panel_plot_A_and_gsw_vs_tleaf_both_HWS <- plot_grid (A_vs_leafT_licor_first_HW_r_3_4,A_vs_leafT_licor_second_HW_r_3_4,gsw_vs_leafT_licor_first_HW_r_3_4,gsw_vs_leafT_licor_second_HW_r_3_4, 
                                                     labels =c ("A","B","C","D"),vjust = 4,
                                                     hjust = -6, 
                                                     label_size = 18,
                                                     ncol = 2, 
                                                     nrow = 2, 
                                                     align = "hv",
                                                     axis = "tblr" )


ggsave(panel_plot_A_and_gsw_vs_tleaf_both_HWS , filename = "figures/panel_plot_A_and_gsw_vs_tleaf_both_HWS.pdf", device = cairo_pdf, width = 16, height = 11)


ggsave(panel_plot_A_and_gsw_vs_tleaf_both_HWS , filename = "figures/panel_plot_A_and_gsw_vs_tleaf_both_HWS.png", width = 16, height = 11)

