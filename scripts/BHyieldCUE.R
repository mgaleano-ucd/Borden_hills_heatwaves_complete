###script to calculate crop water use efficiency and plot by treatment and year###
###Borden Hills Study
###November 2024
#install.packages('ggplot2')
#require(ggplot2)
library(ggplot2)
library(agricolae)

dat <- read.csv('data/table_irrigation_and_yield_bh_NO_REMOVAL_OF_OUTLIERS.csv')
head(dat)
dat$cue <- dat$Yield_kg_per_vine/dat$gallon_per_acre
dat$cue_mm <- dat$Yield_kg_per_vine/dat$mm_per_day_hectare

###work on gallons per vine calculation; 30 x 30 meter pixel has 170 vines, 4046.86 sqare meters per acre (using gall per acre numbers)
###(4046.86*170/900 = 764.4069 vines/acre)

dat$gal_vine<- (dat$gallon_per_acre/764.4069)
dat$yield_gal <- dat$Yield_kg_per_vine/dat$gal_vine
head(dat)
dat_2019 <- dat[which(dat$year==2019),]
dat_2020 <- dat[which(dat$year==2020),]
dat_2021 <- dat[which(dat$year==2021),]
###plotting treatment and year for kg/gal 
p2019<-ggplot(dat_2019, aes(x=reorder(treatment,trt_code), y=cue, color=treatment)) +
  geom_boxplot() 
p2020<-ggplot(dat_2020, aes(x=reorder(treatment,trt_code), y=cue, color=treatment)) +
  geom_boxplot() 
p2021<-ggplot(dat_2021, aes(x=reorder(treatment,trt_code), y=cue, color=treatment)) +
  geom_boxplot() 
p_all<-ggplot(dat, aes(x=reorder(treatment,trt_code), y=cue, color=treatment)) +
  geom_boxplot()  


str(dat_2019)
#####Anova table 2019 CUE####
cue_2019_anova<-dat_2019%>%
  select (trt_code, yield_gal)

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(cue_2019_anova)
variables<-c("yield_gal")

result_df_cue_2019 <- NULL

str(cue_2019_anova$trt_code)

cue_2019_anova$trt_code<-as.factor(cue_2019_anova$trt_code)

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ trt_code")), data = cue_2019_anova)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ trt_code")), cue_2019_anova))
  (test<- HSD.test(anova_result, trt = "trt_code", alpha =0.05, unbalanced = "TRUE"))
  tukey_yield_2019<-as.data.frame(test$groups)
  mean_sd_harvest_2019<-as.data.frame(test$means) 
  mean_sd_harvest_2019$trt_code<-c(1, 2, 3)
  se_df <- cue_2019_anova %>%
    group_by(trt_code) %>% 
    summarise(sem = se(!!sym(variable)))
  variable_result_df_1<-merge(mean_sd_harvest_2019,
                              tukey_yield_2019, by = variable)
  variable_result_df2<- cbind(variable_result_df_1,
                              p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$trt_code), ]
  variable_result_df<- cbind(ordered_df, se_df)
  colnames(variable_result_df) <- make.unique(colnames(variable_result_df))
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
      p_values > 0.05 ~ "ns"
    ))
  # Decide which table to append based on the variable
  if (variable == "yield_gal") {
    # Append to result_df_weight_kg
    result_df_cue_2019 <- variable_result_df
  } 
  else {
    print(paste("Error processing", variable))
  }
}

result_df_cue_2019$Mean_sem <- paste(round(result_df_cue_2019$yield_gal, 3), "±", round(result_df_cue_2019$sem, 3),result_df_cue_2019$groups)

write.csv(result_df_cue_2019,"data_output/result_df_cue_yield_gallons_2019.csv")


#####Anova table 2020 CUE####
cue_2020_anova<-dat_2020%>%
  select (trt_code, yield_gal)

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(cue_2020_anova)
variables<-c("yield_gal")

result_df_cue_2020 <- NULL

str(cue_2020_anova$trt_code)

cue_2020_anova$trt_code<-as.factor(cue_2020_anova$trt_code)

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ trt_code")), data = cue_2020_anova)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ trt_code")), cue_2020_anova))
  (test<- HSD.test(anova_result, trt = "trt_code", alpha =0.05, unbalanced = "TRUE"))
  tukey_yield_2020<-as.data.frame(test$groups)
  mean_sd_harvest_2020<-as.data.frame(test$means) 
  mean_sd_harvest_2020$trt_code<-c(1, 2, 3)
  se_df <- cue_2020_anova %>%
    group_by(trt_code) %>% 
    summarise(sem = se(!!sym(variable)))
  variable_result_df_1<-merge(mean_sd_harvest_2020,
                              tukey_yield_2020, by = variable)
  variable_result_df2<- cbind(variable_result_df_1,
                              p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$trt_code), ]
  variable_result_df<- cbind(ordered_df, se_df)
  colnames(variable_result_df) <- make.unique(colnames(variable_result_df))
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
      p_values > 0.05 ~ "ns"
    ))
  # Decide which table to append based on the variable
  if (variable == "yield_gal") {
    # Append to result_df_weight_kg
    result_df_cue_2020 <- variable_result_df
  } 
  else {
    print(paste("Error processing", variable))
  }
}

result_df_cue_2020$Mean_sem <- paste(round(result_df_cue_2020$yield_gal, 3), "±", round(result_df_cue_2020$sem, 3),result_df_cue_2019$groups)

write.csv(result_df_cue_2020,"data_output/result_df_cue_yield_gallons_2020.csv")


#####Anova table 2021 CUE####
cue_2021_anova<-dat_2021%>%
  select (trt_code, yield_gal)

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(cue_2021_anova)
variables<-c("yield_gal")

result_df_cue_2021 <- NULL

str(cue_2021_anova$trt_code)

cue_2021_anova$trt_code<-as.factor(cue_2021_anova$trt_code)

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ trt_code")), data = cue_2021_anova)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ trt_code")), cue_2021_anova))
  (test<- HSD.test(anova_result, trt = "trt_code", alpha =0.05, unbalanced = "TRUE"))
  tukey_yield_2021<-as.data.frame(test$groups)
  mean_sd_harvest_2021<-as.data.frame(test$means) 
  mean_sd_harvest_2021$trt_code<-c(1, 2, 3)
  se_df <- cue_2021_anova %>%
    group_by(trt_code) %>% 
    summarise(sem = se(!!sym(variable)))
  variable_result_df_1<-merge(mean_sd_harvest_2021,
                              tukey_yield_2021, by = variable)
  variable_result_df2<- cbind(variable_result_df_1,
                              p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$trt_code), ]
  variable_result_df<- cbind(ordered_df, se_df)
  colnames(variable_result_df) <- make.unique(colnames(variable_result_df))
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
      p_values > 0.05 ~ "ns"
    ))
  # Decide which table to append based on the variable
  if (variable == "yield_gal") {
    # Append to result_df_weight_kg
    result_df_cue_2021 <- variable_result_df
  } 
  else {
    print(paste("Error processing", variable))
  }
}

result_df_cue_2021$Mean_sem <- paste(round(result_df_cue_2021$yield_gal, 3), "±", round(result_df_cue_2021$sem, 3),result_df_cue_2019$groups)

write.csv(result_df_cue_2021,"data_output/result_df_cue_yield_gallons_2021.csv")
