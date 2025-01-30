library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(agricolae)

###### ANOVA BERRY PRIMARY CHEMISTRY 2019 AT HARVEST ####

berry_chemistry_borden_hills_2019 <-read.csv("data/berry_chemistry_2019_3.csv", header = TRUE)

str(berry_chemistry_borden_hills_2019)

berry_chemistry_borden_hills_2019 <-berry_chemistry_borden_hills_2019%>%
  filter(!pixel_number == (34)) 

berry_chemistry_borden_hills_2019$date<- mdy(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

tz(berry_chemistry_borden_hills_2019$date)

str(berry_chemistry_borden_hills_2019$date)

##### Anova 2019 

berry_chemistry_borden_hills_2019 <- berry_chemistry_borden_hills_2019%>%
  filter(date >= "2019-09-12") %>%
  select(Brix,pH, TA, treatment) 

berry_chemistry_borden_hills_2019 %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2019$treatment)

berry_chemistry_borden_hills_2019$treatment<- format(berry_chemistry_borden_hills_2019$treatment)
berry_chemistry_borden_hills_2019$treatment<- as.factor(berry_chemistry_borden_hills_2019$treatment)

is.factor(berry_chemistry_borden_hills_2019$treatment)

str(berry_chemistry_borden_hills_2019)

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(berry_chemistry_borden_hills_2019)
variables<-c("Brix", "pH", "TA")

result_df_brix_2019 <- NULL
result_df_pH_2019 <- NULL
result_df_TA_2019 <- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = berry_chemistry_borden_hills_2019)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), berry_chemistry_borden_hills_2019))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))
  tukey_yield_2019<-as.data.frame(test$groups)
  mean_sd_harvest_2019<-as.data.frame(test$means) 
  mean_sd_harvest_2019$Treatment<-c(1, 2, 3)
  se_df <- berry_chemistry_borden_hills_2019 %>%
    group_by(treatment) %>% 
    summarise(sem = se(!!sym(variable)))
  variable_result_df_1<-merge(mean_sd_harvest_2019,
                              tukey_yield_2019, by = variable)
  variable_result_df2<- cbind(variable_result_df_1,
                              p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$Treatment), ]
  variable_result_df<- cbind(ordered_df, se_df)
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
      TRUE ~ ""
    ))
  # Decide which table to append based on the variable
  if (variable == "Brix") {
    result_df_brix_2019 <- variable_result_df
  } else if (variable == "pH") {
    result_df_pH_2019 <- variable_result_df
  }
  else if (variable == "TA") {
    result_df_TA_2019 <- variable_result_df
  }
  else {
    print(paste("Error processing", variable))
  }
}

result_df_brix_2019$Mean_sem <- paste(round(result_df_brix_2019$Brix, 2), "±", round(result_df_brix_2019$sem, 2),result_df_brix_2019$groups)


result_df_pH_2019$Mean_sem <- paste(round(result_df_pH_2019$pH, 2), "±", round(result_df_pH_2019$sem, 2),result_df_pH_2019$groups)

result_df_TA_2019$Mean_sem <- paste(round(result_df_TA_2019$TA, 2), "±", round(result_df_TA_2019$sem, 2),result_df_TA_2019$groups)


write.csv(result_df_brix_2019,"data_output/result_df_brix_2019_anova_harvest.csv")
write.csv(result_df_pH_2019,"data_output/result_df_pH_2019_anova_harvest.csv")
write.csv(result_df_TA_2019,"data_output/result_df_TA_2019_anova_harvest.csv")

###### ANOVA BERRY PRIMARY CHEMISTRY 2020 at HARVEST####

berry_chemistry_borden_hills_2020 <-read.csv("data/borden_hills_berry_chemistry_2020.csv", header = TRUE)

str(berry_chemistry_borden_hills_2020)

berry_chemistry_borden_hills_2020 <-berry_chemistry_borden_hills_2020%>%
  filter(!is.na (rep))

berry_chemistry_borden_hills_2020$date<- mdy(berry_chemistry_borden_hills_2020$date)

str(berry_chemistry_borden_hills_2020$date)

tz(berry_chemistry_borden_hills_2020$date)

str(berry_chemistry_borden_hills_2020$date)

berry_chemistry_borden_hills_2020$DOY <- as.numeric(format(berry_chemistry_borden_hills_2020$date, "%j"))

berry_chemistry_borden_hills_2020_tally<-berry_chemistry_borden_hills_2020 %>%
  group_by(treatment, date, DOY) %>%
  tally()

berry_chemistry_borden_hills_2020_avg_se <-berry_chemistry_borden_hills_2020 %>%
  group_by(treatment, date) %>%
  summarise(avg_brix = mean(brix), se_brix = se(brix), avg_ph = mean(pH), se_ph = se (pH), avg_ta = mean(TA), se_ta = se (TA))


write.csv(berry_chemistry_borden_hills_2020_avg_se,"data_output/berry_chemistry_borden_hills_2020_avg_se.csv")

berry_chemistry_borden_hills_2020 %>%
  group_by(treatment, date) %>%
  tally()

##### Anova 2020 

berry_chemistry_borden_hills_2020 <- berry_chemistry_borden_hills_2020%>%
  filter(date == "2020-09-12") %>%
  select(brix,pH, TA, treatment) 

berry_chemistry_borden_hills_2020 %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_chemistry_borden_hills_2020$treatment)

berry_chemistry_borden_hills_2020$treatment<- format(berry_chemistry_borden_hills_2020$treatment)
berry_chemistry_borden_hills_2020$treatment<- as.factor(berry_chemistry_borden_hills_2020$treatment)

is.factor(berry_chemistry_borden_hills_2020$treatment)

str(berry_chemistry_borden_hills_2020)


se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(berry_chemistry_borden_hills_2020)
variables<-c("brix", "pH", "TA")

result_df_brix_2020 <- NULL
result_df_pH_2020 <- NULL
result_df_TA_2020 <- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = berry_chemistry_borden_hills_2020)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), berry_chemistry_borden_hills_2020))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- berry_chemistry_borden_hills_2020 %>%
    group_by(treatment) %>% 
    summarise(sem = se(!!sym(variable)))
  variable_result_df_1<-merge(mean_sd_harvest,
                              tukey_yield, by = variable)
  variable_result_df2<- cbind(variable_result_df_1,
                              p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$Treatment), ]
  variable_result_df<- cbind(ordered_df, se_df)
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
      TRUE ~ ""
    ))
  # Decide which table to append based on the variable
  if (variable == "brix") {
    result_df_brix_2020 <- variable_result_df
  } else if (variable == "pH") {
    result_df_pH_2020 <- variable_result_df
  }
  else if (variable == "TA") {
    result_df_TA_2020 <- variable_result_df
  }
  else {
    print(paste("Error processing", variable))
  }
}

result_df_brix_2020$Mean_sem <- paste(round(result_df_brix_2020$brix, 2), "±", round(result_df_brix_2020$sem, 2),result_df_brix_2020$groups)


result_df_pH_2020$Mean_sem <- paste(round(result_df_pH_2020$pH, 2), "±", round(result_df_pH_2020$sem, 2),result_df_pH_2020$groups)

result_df_TA_2020$Mean_sem <- paste(round(result_df_TA_2020$TA, 2), "±", round(result_df_TA_2020$sem, 2),result_df_TA_2020$groups)


write.csv(result_df_brix_2020,"data_output/result_df_brix_2020_anova_harvest.csv")
write.csv(result_df_pH_2020,"data_output/result_df_pH_2020_anova_harvest.csv")
write.csv(result_df_TA_2020,"data_output/result_df_TA_2020_anova_harvest.csv")

###### ANOVA BERRY PRIMARY CHEMISTRY 2021 AT HARVEST####

berry_chemistry_borden_hills_2021 <-read.csv("data/borden_hills_berry_chemistry_2021.csv", header = TRUE)
bh_TA_2021_harvest<-read.csv("data/bh_2021_TA_harvest.csv", header =TRUE)


str(berry_chemistry_borden_hills_2021)
str(bh_TA_2021_harvest)

berry_chemistry_borden_hills_2021_avg_se <-berry_chemistry_borden_hills_2021 %>%
  group_by(treatment) %>%
  summarise(avg_brix = mean(Brix), se_brix = se(Brix), avg_ph = mean(pH), se_ph = se (pH)) 



write.csv(berry_chemistry_borden_hills_2021_avg_se,"data_output/berry_chemistry_borden_hills_2021_avg_se.csv")

berry_chemistry_borden_hills_2021 %>%
  group_by(treatment) %>%
  tally()

##### Anova 2021

berry_chemistry_borden_hills_2021 <- berry_chemistry_borden_hills_2021%>%
  select(Brix,pH, treatment) 


str(berry_chemistry_borden_hills_2021)

# Verify the number of replicates per treatment
berry_chemistry_borden_hills_2021 %>%
  group_by(treatment) %>%
  tally()


is.factor(berry_chemistry_borden_hills_2021$treatment)

berry_chemistry_borden_hills_2021$treatment<- format(berry_chemistry_borden_hills_2021$treatment)
berry_chemistry_borden_hills_2021$treatment<- as.factor(berry_chemistry_borden_hills_2021$treatment)

is.factor(berry_chemistry_borden_hills_2021$treatment)

str(berry_chemistry_borden_hills_2021)


se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(berry_chemistry_borden_hills_2021)
variables<-c("Brix", "pH")

result_df_brix_2021 <- NULL
result_df_pH_2021 <- NULL


for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = berry_chemistry_borden_hills_2021)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), berry_chemistry_borden_hills_2021))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- berry_chemistry_borden_hills_2021 %>%
    group_by(treatment) %>% 
    summarise(sem = se(!!sym(variable)))
  variable_result_df_1<-merge(mean_sd_harvest,
                              tukey_yield, by = variable)
  variable_result_df2<- cbind(variable_result_df_1,
                              p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$Treatment), ]
  variable_result_df<- cbind(ordered_df, se_df)
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
      TRUE ~ ""
    ))
  # Decide which table to append based on the variable
  if (variable == "Brix") {
    result_df_brix_2021 <- variable_result_df
  } else if (variable == "pH") {
    result_df_pH_2021 <- variable_result_df
  } else if (variable == "TA") {
    result_df_TA_2021 <- variable_result_df
  }
  else {
    print(paste("Error processing", variable))
  }
}

result_df_brix_2021$Mean_sem <- paste(round(result_df_brix_2021$Brix, 2), "±", round(result_df_brix_2021$sem, 2),result_df_brix_2021$groups)


result_df_pH_2021$Mean_sem <- paste(round(result_df_pH_2021$pH, 2), "±", round(result_df_pH_2021$sem, 2),result_df_pH_2021$groups)


write.csv(result_df_brix_2021,"BH_2021/data_output/result_df_brix_2021_anova_harvest.csv")
write.csv(result_df_pH_2021,"BH_2021/data_output/result_df_pH_2021_anova_harvest.csv")



##### Anova 2021 TA

bh_TA_2021_harvest <- bh_TA_2021_harvest%>%
  select(TA, treatment) 

bh_TA_2021_harvest %>%
  group_by(treatment)%>%
  tally()

is.factor(bh_TA_2021_harvest$treatment)

bh_TA_2021_harvest$treatment<- format(bh_TA_2021_harvest$treatment)
bh_TA_2021_harvest$treatment<- as.factor(bh_TA_2021_harvest$treatment)

is.factor(bh_TA_2021_harvest$treatment)

str(bh_TA_2021_harvest)


se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(bh_TA_2021_harvest)

  anova_result <- aov((TA~ treatment), data = bh_TA_2021_harvest)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov((TA ~ treatment), bh_TA_2021_harvest))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- bh_TA_2021_harvest %>%
    group_by(treatment) %>% 
    summarise(sem = se(TA))
  
  variable_result_df_1<-merge(mean_sd_harvest,
                              tukey_yield, by = "TA")
  
  variable_result_df_1 <- unique(variable_result_df_1)
  variable_result_df_1 <- distinct(variable_result_df_1)
  variable_result_df_1 <- variable_result_df_1 %>%
    distinct(treatment, .keep_all = TRUE)
  
  variable_result_df2<- cbind(variable_result_df_1,
                              p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$Treatment), ]
  variable_result_df<- cbind(ordered_df, se_df)
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
      TRUE ~ ""
    ))
    result_df_TA_2021 <- variable_result_df

result_df_TA_2021$Mean_sem <- paste(round(result_df_TA_2021$TA, 2), "±", round(result_df_TA_2021$sem, 2),result_df_TA_2021$groups)

write.csv(result_df_TA_2021,"data_output/result_df_TA_2021_anova_harvest.csv")

