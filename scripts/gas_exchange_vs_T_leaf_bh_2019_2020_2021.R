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


##### 2019 HW GSW VS TLEAF NEW PHYTOLOGIST ####


diurnals_2019_gsw_vs_leafT_licor_second_HW<- diurnals_borden_hills_2019 %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine) %>%
  filter(day == 227) 
 
  
  
  # filter(day > 214) %>%
  #filter(round > 2) %>%
  #filter(round < 5)

str(diurnals_2019_gsw_vs_leafT_licor_second_HW$Tleaf)


diurnals_2019_gsw_vs_leafT_licor_second_HW%>%
  group_by(treatment)%>%
  tally()

diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment)

diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment<- reorder(diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment, diurnals_2019_gsw_vs_leafT_licor_second_HW$Tleaf)

str(diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment)

diurnals_2019_gsw_vs_leafT_licor_second_HW$Rep<-format(diurnals_2019_gsw_vs_leafT_licor_second_HW$Rep)
as.character(diurnals_2019_gsw_vs_leafT_licor_second_HW$Rep)

diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment<-format(diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment)
as.character(diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment)

str(diurnals_2019_gsw_vs_leafT_licor_second_HW$treatment)


## Plot second HW RESPONSE
gsw_vs_leafT_licor_second_HW2_2019 <-ggplot(diurnals_2019_gsw_vs_leafT_licor_second_HW, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label), stat(eq.label), sep = "*\", \"*")),
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
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1), limits = c (-0.1,0.78))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 


ggsave(gsw_vs_leafT_licor_second_HW2_2019, filename = "figures/gsw_vs_leafT_licor_second_HW2_2019.jpg",  width = 8, height =6, dpi =600) 



gsw_vs_leafT_licor_second_HW2_2019_no_outliers <-ggplot(diurnals_2019_gsw_vs_leafT_licor_second_HW, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  stat_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label), stat(eq.label), sep = "*\", \"*")),
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
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,0.5,0.1), limits = c (-0.1,0.5))+
  theme(axis.text.x = element_text(size =20))+
  theme(axis.text.y = element_text(size =20))+
  theme(legend.position = "none") 

ggsave(gsw_vs_leafT_licor_second_HW2_2019_no_outliers, filename = "figures/gsw_vs_leafT_licor_second_HW2_2019_no_outliers.jpg",  width = 8, height =6, dpi =600) 


##2020 data####

diurnals_borden_hills_2020 <-read.csv("data_output/data_physiology_all_complete_BH_2020.csv", header = TRUE)

str(diurnals_borden_hills_2020)

diurnals_2020_A_vs_time <- diurnals_borden_hills_2020 %>%
  mutate(time = hhmmss) %>%
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

str(diurnals_2020_A_vs_time$leaf_temp)

###2020 HWS#####

diurnals_2020_gas_exchange_vs_LICOR_temp <- diurnals_borden_hills_2020 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss)

diurnals_2020_gas_exchange_vs_LICOR_temp$date1 <- as.Date(paste("2020", diurnals_2020_gas_exchange_vs_LICOR_temp$day), format = "%Y %j")

se <- function(x) sqrt(var(x)/length(x))

se1<- function (x) sd (x)/sqrt(length(x))

diurnals_2020_gas_exchange_vs_LICOR_temp$time<-hms(diurnals_2020_gas_exchange_vs_LICOR_temp$time)
diurnals_2020_gas_exchange_vs_LICOR_temp$date<- ymd(diurnals_2020_gas_exchange_vs_LICOR_temp$date)

str(diurnals_2020_gas_exchange_vs_LICOR_temp$date)
str(diurnals_2020_gas_exchange_vs_LICOR_temp$time)

diurnals_2020_gas_exchange_vs_LICOR_temp$datetime <- paste(diurnals_2020_gas_exchange_vs_LICOR_temp$date, " ", diurnals_2020_gas_exchange_vs_LICOR_temp$time, sep = "")

str(diurnals_2020_gas_exchange_vs_LICOR_temp$datetime)

glimpse(diurnals_2020_gas_exchange_vs_LICOR_temp) 

diurnals_2020_gas_exchange_vs_LICOR_temp$datetime <- ymd_hms(diurnals_2020_gas_exchange_vs_LICOR_temp$datetime,  tz = "UTC")
str(diurnals_2020_gas_exchange_vs_LICOR_temp$datetime)

diurnals_2020_gas_exchange_vs_LICOR_temp$round<-format(diurnals_2020_gas_exchange_vs_LICOR_temp$round)
diurnals_2020_gas_exchange_vs_LICOR_temp$round<-as.numeric(as.factor(diurnals_2020_gas_exchange_vs_LICOR_temp$round))

str(diurnals_2020_gas_exchange_vs_LICOR_temp$round)
str(diurnals_2020_gas_exchange_vs_LICOR_temp)

tz(diurnals_2020_gas_exchange_vs_LICOR_temp$datetime)

tz(diurnals_2020_gas_exchange_vs_LICOR_temp$time)

str(diurnals_2020_gas_exchange_vs_LICOR_temp$datetime)






#### gsw vs T leaf Aug 13 diurnal BH 2020  HW3####
diurnals_2020_gas_exchange_vs_leafT_aug_19<- diurnals_2020_gas_exchange_vs_LICOR_temp %>%
  filter(!is.na(Tleaf)) %>%
  select( day, Tleaf, A, gsw,pixel_number, round, treatment, BLOCK, VINE, LEAF, date1) %>%
  filter(day == 232) %>% 
  filter(!A < -10)

diurnals_2020_gas_exchange_vs_leafT_aug_19%>%
  group_by(treatment)%>%
  tally()



diurnals_2020_gas_exchange_vs_leafT_aug_19$treatment<-
  as.character(diurnals_2020_gas_exchange_vs_leafT_aug_19$treatment)
str(diurnals_2020_gas_exchange_vs_leafT_aug_19$treatment)


gsw_vs_leafT_licor_aug_19_2020 <-ggplot(diurnals_2020_gas_exchange_vs_leafT_aug_19, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1), limits = c (-0.1,0.78))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  theme(legend.position = "none")  +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())


ggsave(gsw_vs_leafT_licor_aug_19_2020, filename = "figures/gsw_vs_leafT_licor_aug_19_2020_HW3.jpg",  width = 8, height =6, dpi =600)




gsw_vs_leafT_licor_aug_19_2020_no_outliers <-ggplot(diurnals_2020_gas_exchange_vs_leafT_aug_19, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,0.5,0.1), limits = c (-0.1,0.5))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())


ggsave(gsw_vs_leafT_licor_aug_19_2020_no_outliers, filename = "figures/gsw_vs_leafT_licor_aug_19_2020_no_outliers_HW3.jpg",  width = 8, height =6, dpi =600)





####2021 data ####


data_physiology_complete_BH_2021_middays<-read.csv("data_output/data_physiology_complete_BH_2021_middays.csv", header = TRUE)

data_physiology_complete_BH_2021_predawn<-read.csv("data_output/data_physiology_complete_BH_2021_predawn.csv", header = TRUE)

total_physiology_2021_bh<-rbind(data_physiology_complete_BH_2021_middays,data_physiology_complete_BH_2021_predawn)


diurnals_2021_gas_exchange_vs_LICOR_temp <- total_physiology_2021_bh

diurnals_2021_gas_exchange_vs_LICOR_temp$date<- ymd(diurnals_2021_gas_exchange_vs_LICOR_temp$date)
str(diurnals_2021_gas_exchange_vs_LICOR_temp$date)

####HW2 2021####
diurnals_2021_gas_exchange_vs_leafT_HW2_july_10<- diurnals_2021_gas_exchange_vs_LICOR_temp %>%
  filter(!is.na(Tleaf)) %>%
  select( date, Tleaf, A, gsw, treatment, BLOCK, VINE, LEAF) %>%
  filter(date == "2021-07-10") 

diurnals_2021_gas_exchange_vs_leafT_HW2_july_10%>%
  group_by(treatment)%>%
  tally()

diurnals_2021_gas_exchange_vs_leafT_HW2_july_10$treatment<-
  as.character(diurnals_2021_gas_exchange_vs_leafT_HW2_july_10$treatment)
str(diurnals_2021_gas_exchange_vs_leafT_HW2_july_10$treatment)


gsw_vs_leafT_licor_july_10_2021_HW2 <-ggplot(diurnals_2021_gas_exchange_vs_leafT_HW2_july_10, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,0.7,0.1), limits = c (-0.1,0.78))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())


ggsave(gsw_vs_leafT_licor_july_10_2021_HW2, filename = "figures/gsw_vs_leafT_licor_july_10_2021_HW2.jpg",  width = 8, height =6, dpi =600)


###smaller scale#####


gsw_vs_leafT_licor_july_10_2021_HW2_smaller_scale <-ggplot(diurnals_2021_gas_exchange_vs_leafT_HW2_july_10, aes(Tleaf, gsw, group = treatment, color = treatment)) +
  geom_point(alpha =0.6, size = 1.7 ) + 
  geom_smooth(aes(y = gsw, group = treatment, color = treatment),method = "lm", formula = y ~ x + I(x^2), size = 1, alpha =0.4, level=0.95) +
  #  stat_poly_eq(aes(label =  paste(stat(rr.label),stat(p.value.label),stat(eq.label), sep = "*\", \"*")),
  #               formula =  y ~ x + I(x^2), parse = TRUE, size = 5, label.x.npc = "left", vjust =-0.1)+
  theme_classic() +
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  ylab(expression(Stomatal~conductance~(mol~m^{-2}~s^{-1}))) +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("LI-COR Leaf temperature (ºC)") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.position = "right") +
  theme(legend.title.align = 0)+
  scale_x_continuous(breaks=seq(20,50,5), limits = c (20,50)) +
  scale_y_continuous(breaks=seq(0,0.5,0.1), limits = c (-0.1,0.5))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))+
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(color = "white"), axis.text.y = element_text(color = "white"), axis.ticks.y = element_blank())


ggsave(gsw_vs_leafT_licor_july_10_2021_HW2_smaller_scale, filename = "figures/gsw_vs_leafT_licor_july_10_2021_HW_smaller_scale.jpg",  width = 8, height =6, dpi =600)


###combine plots######

library(cowplot)

panel_plot_gas_exchange_vs_leafT_licor_main_HWS_2019_2020_2021<- plot_grid(
  gsw_vs_leafT_licor_second_HW2_2019 + ggtitle("HW2 2019"), gsw_vs_leafT_licor_aug_19_2020 +ggtitle("HW3 2020"),gsw_vs_leafT_licor_july_10_2021_HW2 + ggtitle(" HW2 2021"),
  labels = c("A", "B", "C"), 
  nrow = 1, 
  vjust = 1.5, 
  hjust = -9.6, 
  label_size = 20
)


ggsave(panel_plot_gas_exchange_vs_leafT_licor_main_HWS_2019_2020_2021, filename = "figures/panel_plot_gas_exchange_vs_leafT_licor_main_HWS_2019_2020_2021.jpg", width = 20, height =8, dpi =600)


panel_plot_gas_exchange_vs_leafT_licor_main_HWS_2019_2020_2021_adjusted<- plot_grid(
  gsw_vs_leafT_licor_second_HW2_2019_no_outliers + ggtitle("HW2 2019"), gsw_vs_leafT_licor_aug_19_2020_no_outliers +ggtitle("HW3 2020"),gsw_vs_leafT_licor_july_10_2021_HW2_smaller_scale + ggtitle(" HW2 2021"),
  labels = c("A", "B", "C"), 
  nrow = 1, 
  vjust = 1.5, 
  hjust = -9.6, 
  label_size = 20
)


ggsave(panel_plot_gas_exchange_vs_leafT_licor_main_HWS_2019_2020_2021_adjusted, filename = "figures/panel_plot_gas_exchange_vs_leafT_licor_main_HWS_2019_2020_2021_adjusted.jpg", width = 20, height =6, dpi =600)

#####Boxplots #####

diurnals_2019_IRT_temp_second_HW <-diurnals_borden_hills_2019%>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine,leaf_temp_C) %>%
  filter(day == 227)%>%
  filter(round == 4|round==5)

diurnals_2019_IRT_temp_second_HW_irt_only<-diurnals_2019_IRT_temp_second_HW%>%
  select(leaf_temp_C,treatment)%>%
  filter(!is.na(leaf_temp_C))


diurnals_2019_IRT_temp_anova<-diurnals_2019_IRT_temp_second_HW_irt_only

diurnals_2019_IRT_temp_anova%>%
  group_by(treatment)%>%
  tally() 

diurnals_2019_IRT_temp_anova$treatment <- as.factor(diurnals_2019_IRT_temp_anova$treatment)
diurnals_2019_IRT_temp_anova$leaf_temp_C <- as.numeric(diurnals_2019_IRT_temp_anova$leaf_temp_C)
str(diurnals_2019_IRT_temp_anova$treatment)
str(diurnals_2019_IRT_temp_anova)

  # Filter data for the current date
  data_subset <- diurnals_2019_IRT_temp_anova 
  # Convert treatment to factor
  data_subset$treatment <- as.factor(data_subset$treatment)
  
  # Perform one-way ANOVA
  anova_result <- aov( leaf_temp_C~ treatment, data = data_subset)
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract p-values
  p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Extract means
  means <- aggregate(leaf_temp_C~treatment, data = data_subset, FUN = mean)
  
  # Extract standard errors
  standard_errors <- aggregate(leaf_temp_C~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))
  
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
  date_results_leaf_temp_C_2019 <- data.frame(
    treatment = levels(data_subset$treatment),
    p_value = p_values,
    mean = means$leaf_temp_C,
    standard_error = standard_errors$leaf_temp_C,
    letters_ordered = letters_ordered,
    significance = significance
  )

str(date_results_leaf_temp_C_2019 )
date_results_leaf_temp_C_2019$Mean_sem <- paste(round(date_results_leaf_temp_C_2019$mean, 2), "±", round(date_results_leaf_temp_C_2019$standard_error, 2),date_results_leaf_temp_C_2019$letters_ordered.groups)


#####HW3 2020#####


diurnals_2020_IRT_temp_third_HW <-diurnals_borden_hills_2020%>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, BLOCK, VINE, LEAF,leaf_temp) %>%
  mutate(leaf_temp_C = leaf_temp)%>%
  filter(day == 226) %>%
  filter(round ==4|round ==5)

diurnals_2020_IRT_temp_third_HW_irt_only<-diurnals_2020_IRT_temp_third_HW%>%
  select(leaf_temp_C,treatment)%>%
  filter(!is.na(leaf_temp_C))


diurnals_2020_IRT_temp_anova<-diurnals_2020_IRT_temp_third_HW_irt_only

diurnals_2020_IRT_temp_anova%>%
  group_by(treatment)%>%
  tally() 

diurnals_2020_IRT_temp_anova$treatment <- as.factor(diurnals_2020_IRT_temp_anova$treatment)
diurnals_2020_IRT_temp_anova$leaf_temp_C <- as.numeric(diurnals_2020_IRT_temp_anova$leaf_temp_C)
str(diurnals_2020_IRT_temp_anova$treatment)
str(diurnals_2020_IRT_temp_anova)

# Filter data for the current date
data_subset <- diurnals_2020_IRT_temp_anova 
# Convert treatment to factor
data_subset$treatment <- as.factor(data_subset$treatment)

# Perform one-way ANOVA
anova_result <- aov( leaf_temp_C~ treatment, data = data_subset)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract p-values
p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]

# Extract means
means <- aggregate(leaf_temp_C~treatment, data = data_subset, FUN = mean)

# Extract standard errors
standard_errors <- aggregate(leaf_temp_C~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))

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
date_results_leaf_temp_C_2020 <- data.frame(
  treatment = levels(data_subset$treatment),
  p_value = p_values,
  mean = means$leaf_temp_C,
  standard_error = standard_errors$leaf_temp_C,
  letters_ordered = letters_ordered,
  significance = significance
)

str(date_results_leaf_temp_C_2020 )
date_results_leaf_temp_C_2020$Mean_sem <- paste(round(date_results_leaf_temp_C_2020$mean, 2), "±", round(date_results_leaf_temp_C_2020$standard_error, 2),date_results_leaf_temp_C_2020$letters_ordered.groups)



#####HW2 2021#####


diurnals_2021_IRT_temp_second_HW <-data_physiology_complete_BH_2021_middays%>%
  select( date, Tleaf, A, gsw, treatment, BLOCK, VINE, LEAF, leaf_temp) %>%
  filter(date == "2021-07-10") %>%
  mutate(leaf_temp_C = leaf_temp)



diurnals_2021_IRT_temp_second_HW_irt_only<-diurnals_2021_IRT_temp_second_HW%>%
  select(leaf_temp_C,treatment)%>%
  filter(!is.na(leaf_temp_C))


diurnals_2021_IRT_temp_anova<-diurnals_2021_IRT_temp_second_HW_irt_only

diurnals_2021_IRT_temp_anova%>%
  group_by(treatment)%>%
  tally() 

diurnals_2021_IRT_temp_anova$treatment <- as.factor(diurnals_2021_IRT_temp_anova$treatment)
diurnals_2021_IRT_temp_anova$leaf_temp_C <- as.numeric(diurnals_2021_IRT_temp_anova$leaf_temp_C)
str(diurnals_2021_IRT_temp_anova$treatment)
str(diurnals_2021_IRT_temp_anova)

# Filter data for the current date
data_subset <- diurnals_2021_IRT_temp_anova 
# Convert treatment to factor
data_subset$treatment <- as.factor(data_subset$treatment)

# Perform one-way ANOVA
anova_result <- aov( leaf_temp_C~ treatment, data = data_subset)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract p-values
p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]

# Extract means
means <- aggregate(leaf_temp_C~treatment, data = data_subset, FUN = mean)

# Extract standard errors
standard_errors <- aggregate(leaf_temp_C~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))

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
date_results_leaf_temp_C_2021 <- data.frame(
  treatment = levels(data_subset$treatment),
  p_value = p_values,
  mean = means$leaf_temp_C,
  standard_error = standard_errors$leaf_temp_C,
  letters_ordered = letters_ordered,
  significance = significance
)

str(date_results_leaf_temp_C_2021 )
date_results_leaf_temp_C_2021$Mean_sem <- paste(round(date_results_leaf_temp_C_2021$mean, 2), "±", round(date_results_leaf_temp_C_2021$standard_error, 2),date_results_leaf_temp_C_2021$letters_ordered.groups)



diurnals_2019_IRT_temp_second_HW_irt_only$year<-2019
diurnals_2020_IRT_temp_third_HW_irt_only$year<-2020
diurnals_2021_IRT_temp_second_HW_irt_only$year<-2021

middays_2019_2020_2021_IRT_main_HWS<-rbind(diurnals_2019_IRT_temp_second_HW_irt_only, diurnals_2020_IRT_temp_third_HW_irt_only, diurnals_2021_IRT_temp_second_HW_irt_only)

middays_2019_2020_2021_IRT_main_HWS$treatment<-as.character(middays_2019_2020_2021_IRT_main_HWS$treatment)
str(middays_2019_2020_2021_IRT_main_HWS$treatment)

middays_2019_2020_2021_IRT_main_HWS$leaf_temp_C<-as.numeric(middays_2019_2020_2021_IRT_main_HWS$leaf_temp_C)

middays_2019_2020_2021_IRT_main_HWS$year<-as.factor(middays_2019_2020_2021_IRT_main_HWS$year)



str(middays_2019_2020_2021_IRT_main_HWS)




sign_2019 <- data.frame(year = "2019", treatment = c("1", "2", "3"), significance = date_results_leaf_temp_C_2019$letters_ordered.groups)
sign_2020 <- data.frame(year = "2020", treatment = c("1", "2", "3"), significance = date_results_leaf_temp_C_2020$letters_ordered.groups)
sign_2021 <- data.frame(year = "2021", treatment = c("1", "2", "3"), significance = date_results_leaf_temp_C_2021$letters_ordered.groups)


str(middays_2019_2020_2021_IRT_main_HWS)
# Combine into one dataframe
sign <- rbind(sign_2019, sign_2020, sign_2021)




IRT_main_HWS_three_years_bh<-ggplot(middays_2019_2020_2021_IRT_main_HWS, aes(x = year, y = leaf_temp_C, fill = treatment)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.85)) + 
  theme_classic() +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "90%-120% ET", "120%-180% ET")) +
  labs(x = "Year", y = "IRT leaf temperature (°C)") +
  theme(axis.title.y = element_text(size = 21, family = "serif")) +
  theme(axis.title.x = element_text(size = 21, family = "serif")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.key.width = unit(0.2, "cm")) +
  theme(legend.justification = "center") +
  theme(legend.position = "right") +
  theme(legend.title.align = 0) +
  scale_y_continuous(breaks = seq(25, 55, 5), limits = c(25, 55)) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  theme(legend.text = element_text(size = 14)) + 
  theme(legend.title = element_text(size = 16)) + 
  # Add significance letters, properly dodged by treatment
  geom_text(data = sign, aes(x = year, y = 54, label = significance, group = treatment), 
            position = position_dodge(width = 0.87), size = 5, inherit.aes = FALSE)+
  annotate("text", x = as.factor("2019"), y = 50, label = "August HW2", size = 5) +
  annotate("text", x = as.factor("2019"), y = 48, label = "Tmax 40.5°C", size = 5) +
  annotate("text", x = as.factor("2020"), y = 50, label = "August HW3", size = 5) +
  annotate("text", x = as.factor("2020"), y = 48, label = "Tmax 37.1°C", size = 5) +
  annotate("text", x = as.factor("2021"), y = 50, label = "July HW2", size = 5) +
  annotate("text", x = as.factor("2021"), y = 48, label = "Tmax 40.4°C", size = 5) 

ggsave(IRT_main_HWS_three_years_bh, filename = "figures/IRT_main_HWS_three_years_bh.jpg", width = 16, height =8, dpi =600)



#####Boxplots LICOR TEMPERTATURE#####

diurnals_2019_LICOR_temp_second_HW <-diurnals_borden_hills_2019%>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, Rep, BH_Leaf, BH_Block, BH_Vine,leaf_temp_C) %>%
  filter(day == 227)%>%
  filter(round == 4)

diurnals_2019_LICOR_temp_second_HW_LICOR_only<-diurnals_2019_LICOR_temp_second_HW%>%
  select(Tleaf,treatment)


diurnals_2019_LICOR_temp_anova<-diurnals_2019_LICOR_temp_second_HW_LICOR_only

diurnals_2019_LICOR_temp_anova%>%
  group_by(treatment)%>%
  tally() 

diurnals_2019_LICOR_temp_anova$treatment <- as.factor(diurnals_2019_LICOR_temp_anova$treatment)

str(diurnals_2019_LICOR_temp_anova$treatment)
str(diurnals_2019_LICOR_temp_anova)

# Filter data for the current date
data_subset <- diurnals_2019_LICOR_temp_anova 
# Convert treatment to factor
data_subset$treatment <- as.factor(data_subset$treatment)

# Perform one-way ANOVA
anova_result <- aov( Tleaf~ treatment, data = data_subset)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract p-values
p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]

# Extract means
means <- aggregate(Tleaf~treatment, data = data_subset, FUN = mean)

# Extract standard errors
standard_errors <- aggregate(Tleaf~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))

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
date_results_Tleaf_2019 <- data.frame(
  treatment = levels(data_subset$treatment),
  p_value = p_values,
  mean = means$Tleaf,
  standard_error = standard_errors$Tleaf,
  letters_ordered = letters_ordered,
  significance = significance
)

str(date_results_Tleaf_2019 )
date_results_Tleaf_2019$Mean_sem <- paste(round(date_results_Tleaf_2019$mean, 2), "±", round(date_results_Tleaf_2019$standard_error, 2),date_results_Tleaf_2019$letters_ordered.groups)


#####HW3 2020#####


diurnals_2020_LICOR_temp_third_HW <-diurnals_borden_hills_2020%>%
  select( day, Tleaf, gsw, pixel_number, round, treatment, BLOCK, VINE, LEAF,leaf_temp) %>%
  filter(day == 232) %>%
  filter(round ==4)

diurnals_2020_LICOR_temp_third_HW_LICOR_only<-diurnals_2020_LICOR_temp_third_HW%>%
  select(Tleaf,treatment)%>%
  filter(!is.na(Tleaf))


diurnals_2020_LICOR_temp_anova<-diurnals_2020_LICOR_temp_third_HW_LICOR_only

diurnals_2020_LICOR_temp_anova%>%
  group_by(treatment)%>%
  tally() 

diurnals_2020_LICOR_temp_anova$treatment <- as.factor(diurnals_2020_LICOR_temp_anova$treatment)
diurnals_2020_LICOR_temp_anova$Tleaf <- as.numeric(diurnals_2020_LICOR_temp_anova$Tleaf)
str(diurnals_2020_LICOR_temp_anova$treatment)
str(diurnals_2020_LICOR_temp_anova)

# Filter data for the current date
data_subset <- diurnals_2020_LICOR_temp_anova 
# Convert treatment to factor
data_subset$treatment <- as.factor(data_subset$treatment)

# Perform one-way ANOVA
anova_result <- aov( Tleaf~ treatment, data = data_subset)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract p-values
p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]

# Extract means
means <- aggregate(Tleaf~treatment, data = data_subset, FUN = mean)

# Extract standard errors
standard_errors <- aggregate(Tleaf~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))

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
date_results_Tleaf_2020 <- data.frame(
  treatment = levels(data_subset$treatment),
  p_value = p_values,
  mean = means$Tleaf,
  standard_error = standard_errors$Tleaf,
  letters_ordered = letters_ordered,
  significance = significance
)

str(date_results_Tleaf_2020 )
date_results_Tleaf_2020$Mean_sem <- paste(round(date_results_Tleaf_2020$mean, 2), "±", round(date_results_Tleaf_2020$standard_error, 2),date_results_Tleaf_2020$letters_ordered.groups)



#####HW2 2021#####


diurnals_2021_LICOR_temp_second_HW <-data_physiology_complete_BH_2021_middays%>%
  select( date, Tleaf, A, gsw, treatment, BLOCK, VINE, LEAF, leaf_temp) %>%
  filter(date == "2021-07-10")



diurnals_2021_LICOR_temp_second_HW_LICOR_only<-diurnals_2021_LICOR_temp_second_HW%>%
  select(Tleaf,treatment)%>%
  filter(!is.na(Tleaf))


diurnals_2021_LICOR_temp_anova<-diurnals_2021_LICOR_temp_second_HW_LICOR_only

diurnals_2021_LICOR_temp_anova%>%
  group_by(treatment)%>%
  tally() 

diurnals_2021_LICOR_temp_anova$treatment <- as.factor(diurnals_2021_LICOR_temp_anova$treatment)
diurnals_2021_LICOR_temp_anova$Tleaf <- as.numeric(diurnals_2021_LICOR_temp_anova$Tleaf)
str(diurnals_2021_LICOR_temp_anova$treatment)
str(diurnals_2021_LICOR_temp_anova)

# Filter data for the current date
data_subset <- diurnals_2021_LICOR_temp_anova 
# Convert treatment to factor
data_subset$treatment <- as.factor(data_subset$treatment)

# Perform one-way ANOVA
anova_result <- aov( Tleaf~ treatment, data = data_subset)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract p-values
p_values <- summary(anova_result)[[1]]$`Pr(>F)`[1]

# Extract means
means <- aggregate(Tleaf~treatment, data = data_subset, FUN = mean)

# Extract standard errors
standard_errors <- aggregate(Tleaf~ treatment, data = data_subset, FUN = function(x) sd(x)/sqrt(length(x)))

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
date_results_Tleaf_2021 <- data.frame(
  treatment = levels(data_subset$treatment),
  p_value = p_values,
  mean = means$Tleaf,
  standard_error = standard_errors$Tleaf,
  letters_ordered = letters_ordered,
  significance = significance
)

str(date_results_Tleaf_2021 )
date_results_Tleaf_2021$Mean_sem <- paste(round(date_results_Tleaf_2021$mean, 2), "±", round(date_results_Tleaf_2021$standard_error, 2),date_results_Tleaf_2021$letters_ordered.groups)



diurnals_2019_LICOR_temp_second_HW_LICOR_only$year<-2019
diurnals_2020_LICOR_temp_third_HW_LICOR_only$year<-2020
diurnals_2021_LICOR_temp_second_HW_LICOR_only$year<-2021

middays_2019_2020_2021_LICOR_main_HWS<-rbind(diurnals_2019_LICOR_temp_second_HW_LICOR_only, diurnals_2020_LICOR_temp_third_HW_LICOR_only, diurnals_2021_LICOR_temp_second_HW_LICOR_only)

middays_2019_2020_2021_LICOR_main_HWS$treatment<-as.character(middays_2019_2020_2021_LICOR_main_HWS$treatment)
str(middays_2019_2020_2021_LICOR_main_HWS$treatment)

middays_2019_2020_2021_LICOR_main_HWS$Tleaf<-as.numeric(middays_2019_2020_2021_LICOR_main_HWS$Tleaf)

middays_2019_2020_2021_LICOR_main_HWS$year<-as.factor(middays_2019_2020_2021_LICOR_main_HWS$year)



str(middays_2019_2020_2021_LICOR_main_HWS)




sign_2019 <- data.frame(year = "2019", treatment = c("1", "2", "3"), significance = date_results_Tleaf_2019$letters_ordered.groups)
sign_2020 <- data.frame(year = "2020", treatment = c("1", "2", "3"), significance = date_results_Tleaf_2020$letters_ordered.groups)
sign_2021 <- data.frame(year = "2021", treatment = c("1", "2", "3"), significance = date_results_Tleaf_2021$letters_ordered.groups)

# Combine into one dataframe
sign <- rbind(sign_2019, sign_2020, sign_2021)




LICOR_main_HWS_three_years_bh<-ggplot(middays_2019_2020_2021_LICOR_main_HWS, aes(x = year, y = Tleaf, fill = treatment)) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.85)) + 
  theme_classic() +
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "90%-120% ET", "120%-180% ET")) +
  labs(x = "Year", y = "LICOR leaf temperature (°C)") +
  theme(axis.title.y = element_text(size = 21, family = "serif")) +
  theme(axis.title.x = element_text(size = 21, family = "serif")) +
  theme(legend.key.size = unit(0.5, "cm")) +
  theme(legend.key.width = unit(0.2, "cm")) +
  theme(legend.justification = "center") +
  theme(legend.position = "right") +
  theme(legend.title.align = 0) +
  scale_y_continuous(breaks = seq(35, 50, 5), limits = c(35, 50)) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.text.y = element_text(size = 18)) +
  
  # Add significance letters, properly dodged by treatment
  geom_text(data = sign, aes(x = year, y = 50, label = significance, group = treatment), 
            position = position_dodge(width = 0.87), size = 5, inherit.aes = FALSE)


ggsave(LICOR_main_HWS_three_years_bh, filename = "figures/LICOR_main_HWS_three_years_bh.jpg", width = 16, height =8, dpi =600)



