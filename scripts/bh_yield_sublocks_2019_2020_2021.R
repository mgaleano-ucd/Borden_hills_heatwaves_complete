library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(xtable)
library(agricolae)
library(gtools)


###2019####
yield_bh_2019<- read.csv("data/Yield_borden_hills_2019.csv", header =TRUE)

yield_bh_2019_grouped2<- yield_bh_2019 %>%
  mutate(gr_cluster = (Weight_kg/Clusters)*1000) %>%
  mutate(kg_cluster =Weight_kg/Clusters)%>%
  filter(!pixel_number == 34 ) %>%
  filter(!is.na(Rep))

yield_bh_2019_grouped2$Treatment<-format(yield_bh_2019_grouped2$Treatment)
as.character(yield_bh_2019_grouped2$Treatment)

str(yield_bh_2019_grouped2$Treatment)


yield_bh_2019_grouped2$Rep<-format(yield_bh_2019_grouped2$Rep)
as.character(yield_bh_2019_grouped2$Rep)

str(yield_bh_2019_grouped2$Rep)

str(yield_bh_2019_grouped2)

yield_bh_2019_grouped2%>%
  group_by(Treatment)%>%
  tally()

yield_bh_2019_grouped2<-yield_bh_2019_grouped2%>%
  mutate(block_sublock= case_when(
    Block_ID == "B1R1" ~ "B1R1 NE",
    Block_ID == "B1R3" ~ "B1R3 SE",
    Block_ID == "B1R4" ~ "B1R4 NE",
    Block_ID == "B2R1" ~ "B2R1 NE",
    Block_ID == "B2R2" ~ "B2R2 SE",
    Block_ID == "B2R3" ~ "B2R3 NE",
    Block_ID == "B3R1" ~ "B3R1 NE",
    Block_ID == "B3R2" ~ "B3R2 SE",
    Block_ID == "B3R3" ~ "B3R3 NW",))


str(yield_bh_2019_grouped2)

yield_bh_2019_grouped2 %>%
  group_by(block_sublock) %>%
  tally()

is.factor(yield_bh_2019_grouped2$block_sublock)

yield_bh_2019_grouped2$block_sublock<- as.factor(yield_bh_2019_grouped2$block_sublock)

yield_bh_2019_grouped2$year<-2019


####2020####

harvest_sept_15_2020_cleaned<- read.csv("data/blocks_harvest_sept_15_2020 _cleaned.csv", header = TRUE)

harvest_sept_16_2020_cleaned<- read.csv("data/blocks_harvest_sept_16_2020_cleaned.csv", header = TRUE)

harvest_sept_17_2020_cleaned<- read.csv("data/blocks_harvest_sept_17_2020_cleaned.csv", header = TRUE) %>%
  select(!initials)


yield_2020_cleaned<- rbind(harvest_sept_15_2020_cleaned, harvest_sept_16_2020_cleaned, harvest_sept_17_2020_cleaned)

yield_2020_cleaned<- yield_2020_cleaned%>%
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
  ))%>%
  filter(!is.na(treatment))%>%
  mutate(Weight_kg = weight*0.453592)%>%
  mutate(gr_cluster = (Weight_kg/total.clusters)*1000) %>%
  mutate(kg_cluster =Weight_kg/total.clusters) %>%
  filter(!is.na(weight)) 

yield_2020_cleaned<- yield_2020_cleaned%>%
  mutate(block_sublock = paste(yield_2020_cleaned$block,yield_2020_cleaned$sub.block))

yield_2020_cleaned$block_sublock<-as.factor(yield_2020_cleaned$block_sublock)

str(yield_2020_cleaned$block_sublock)
str(yield_2020_cleaned)

yield_2020_cleaned_tally<-yield_2020_cleaned%>%
  group_by(treatment)%>%
  tally()

yield_2020_cleaned$year<-2020


berry_diameter_BH_2020<- read.csv("data/berry_diameter_2020_harvest_blocks.csv", header =TRUE)

str(berry_diameter_BH_2020$treatment)  


berry_diameter_BH_2020<- berry_diameter_BH_2020%>%
  mutate( treatment = case_when(
    block_id == "B1R1" ~ 1,
    block_id == "B1R3" ~ 1,
    block_id == "B1R4" ~ 1,
    block_id == "B2R1" ~ 2,
    block_id == "B2R2" ~ 2,
    block_id == "B2R3" ~ 2,
    block_id == "B3R1" ~ 3,
    block_id == "B3R2" ~ 3,
    block_id == "B3R3" ~ 3,
  ))%>%
  filter(!is.na(berry_diameter))

berry_diameter_BH_2020<- berry_diameter_BH_2020%>%
  mutate(block_sublock = paste(berry_diameter_BH_2020$block_id,berry_diameter_BH_2020$sub.block))

berry_diameter_BH_2020$block_sublock<-as.factor(berry_diameter_BH_2020$block_sublock)
str(berry_diameter_BH_2020)

berry_diameter_BH_2020%>%
  group_by(treatment)%>%
  tally()


berry_diameter_BH_2020$year<-2020

####2021####


harvest_sept_13_2021<-read.csv("data/Blocks_harvest_sept_13_2021.csv", header=TRUE)

harvest_sept_14_2021<-read.csv("data/Blocks_harvest_sept_14_2021.csv", header=TRUE)%>%
  select(!initials)

harvest_sept_15_2021<-read.csv("data/Blocks_harvest_sept_15_2021.csv", header=TRUE)%>%
  select(!initials)

harvest_sept_16_2021<-read.csv("data/Blocks_harvest_sept_16_2021.csv", header=TRUE)%>%
  select(!initials)

str(harvest_sept_13_2021)
str(harvest_sept_14_2021)
str(harvest_sept_16_2021)

yield_2021<- rbind(harvest_sept_13_2021,harvest_sept_14_2021, harvest_sept_15_2021,harvest_sept_16_2021)%>%
  filter(!is.na(total.clusters)) 

str(yield_2021)

yield_2021$total.clusters<-as.numeric(yield_2021$total.clusters)
str(yield_2021$total.clusters)

yield_2021_total <- yield_2021%>%
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
  ))%>%
  filter(!is.na(treatment))%>%
  mutate(Weight_kg = weight*0.453592)%>%
  mutate(gr_cluster = (Weight_kg/total.clusters)*1000) %>%
  mutate(kg_cluster =Weight_kg/total.clusters) %>%
  filter(!is.na(total.clusters)) 

yield_2021_total<- yield_2021_total%>%
  mutate(block_sublock = paste(yield_2021_total$block,yield_2021_total$sub.block))%>%
  filter(!is.na(gr_cluster))

yield_2021_total$block_sublock<-as.factor(yield_2021_total$block_sublock)

yield_2021_total_tally<-yield_2021_total%>%
  group_by(block_sublock)%>%
  tally()

yield_2021_total$year<-2021

berry_diameter_BH_2021<- read.csv("data/berry_diameter_2021_harvest_blocks.csv", header =TRUE)
str(berry_diameter_BH_2021$treatment)  


str(berry_diameter_BH_2021$berry_diameter)

berry_diameter_BH_2021<- berry_diameter_BH_2021%>%
  mutate( treatment = case_when(
    block_id == "B1R1" ~ 1,
    block_id == "B1R3" ~ 1,
    block_id == "B1R4" ~ 1,
    block_id == "B2R1" ~ 2,
    block_id == "B2R2" ~ 2,
    block_id == "B2R3" ~ 2,
    block_id == "B3R1" ~ 3,
    block_id == "B3R2" ~ 3,
    block_id == "B3R3" ~ 3,
  ))%>%
  filter(!is.na(berry_diameter))%>%
  mutate(berry_diameter1 = berry_diameter*(10))%>%
  select(!berry_diameter)%>%
  mutate( berry_diameter =berry_diameter1)

berry_diameter_BH_2021<- berry_diameter_BH_2021%>%
  mutate(block_sublock = paste(berry_diameter_BH_2021$block_id,berry_diameter_BH_2021$sub.block))

berry_diameter_BH_2021$block_sublock<-as.factor(berry_diameter_BH_2021$block_sublock)

berry_diameter_BH_2021$year<-2021
str(berry_diameter_BH_2021)

berry_diameter_BH_2021%>%
  group_by(block_sublock )%>%
  tally()


yield_bh_2019_grouped2<-yield_bh_2019_grouped2%>%
mutate(treatment = Treatment)

yield_bh_2019_grouped2$treatment<-as.numeric(yield_bh_2019_grouped2$treatment)

str(yield_bh_2019_grouped2)
str(yield_2020_cleaned)
str(berry_diameter_BH_2020)
str(yield_2021_total)
str(berry_diameter_BH_2021)

yield_bh_2019_grouped2<-yield_bh_2019_grouped2%>%
  mutate(total.clusters = Clusters)



bh_yield_2019_sublocks<-yield_bh_2019_grouped2%>%
  select(year, block_sublock,gr_cluster,Weight_kg,treatment,total.clusters)
bh_berry_diameter_2020_sublocks<-berry_diameter_BH_2020%>%
  select(year, block_sublock,berry_diameter,treatment)
bh_yield_2020_sublocks<-yield_2020_cleaned%>%
  select(year, block_sublock,gr_cluster,Weight_kg,treatment,total.clusters)
bh_berry_diameter_2021_sublocks<-berry_diameter_BH_2021%>%
  select(year, block_sublock,berry_diameter,treatment)
bh_yield_2021_sublocks<-yield_2021_total%>%
  select(year, block_sublock,gr_cluster,Weight_kg,treatment,total.clusters)


bh_yield_2019_sublocks_tally<- bh_yield_2019_sublocks%>%
  group_by(treatment)%>%
  tally()

bh_yield_2020_sublocks_tally<- bh_yield_2020_sublocks%>%
  group_by(treatment)%>%
  tally()

bh_yield_2021_sublocks_tally<- bh_yield_2021_sublocks%>%
  group_by(treatment)%>%
  tally()





str(bh_yield_2019_sublocks)
str(bh_berry_diameter_2020_sublocks)
str(bh_yield_2020_sublocks)
str(bh_berry_diameter_2021_sublocks)
str(bh_yield_2021_sublocks)

bh_yield_2019_2020_2021_sublocks<- bind_rows(
  bh_yield_2019_sublocks, 
  bh_yield_2020_sublocks, 
  bh_yield_2021_sublocks
)

str(bh_yield_2019_2020_2021_sublocks)

bh_berry_diameter_2020_2021_sublocks<-bind_rows(bh_berry_diameter_2020_sublocks,bh_berry_diameter_2021_sublocks)


bh_yield_2019_2020_2021_sublocks_tally<-bh_yield_2019_2020_2021_sublocks%>%
  group_by(year,block_sublock)%>%
  tally()

write.csv(bh_yield_2019_2020_2021_sublocks_tally,"data_output/bh_yield_2019_2020_2021_sublocks_tally.csv")



#####TWO WAY ANOVA WEIGHT_KG#####


# Ensure the necessary columns are factors
bh_yield_2019_2020_2021_sublocks$year <- as.factor(bh_yield_2019_2020_2021_sublocks$year)
bh_yield_2019_2020_2021_sublocks$treatment <- as.factor(bh_yield_2019_2020_2021_sublocks$treatment)
bh_yield_2019_2020_2021_sublocks$block_sublock <- as.factor(bh_yield_2019_2020_2021_sublocks$block_sublock)

str(bh_yield_2019_2020_2021_sublocks)
# Define the model formula for the two-way ANOVA
two.way <- aov(Weight_kg ~ year + treatment , data = bh_yield_2019_2020_2021_sublocks)
interaction <- aov(Weight_kg ~ year * treatment , data = bh_yield_2019_2020_2021_sublocks)
blocking <- aov(Weight_kg ~ year * treatment + block_sublock, data = bh_yield_2019_2020_2021_sublocks)


library(AICcmodavg)

model.set <- list(two.way, interaction, blocking)
model.names <- c("two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)


model_formula <- Weight_kg ~ year * treatment

# Run the ANOVA
anova_result <- aov(model_formula, data = bh_yield_2019_2020_2021_sublocks)

# Summarize the results

summary(anova_result)

# Tukey HSD for treatment
tukey_treatment <- TukeyHSD(anova_result, "treatment")
(test <- HSD.test(anova_result, "treatment", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year
tukey_block_sublock <- TukeyHSD(anova_result, "year")
(test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE))


# Tukey HSD for year:treatment interaction
interaction_term_treatment <- interaction(bh_yield_2019_2020_2021_sublocks$year, bh_yield_2019_2020_2021_sublocks$treatment)
anova_result_interaction <- aov(Weight_kg ~ interaction_term_treatment + treatment, data = bh_yield_2019_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term_treatment")


test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE)


tukey.two.way<-TukeyHSD(anova_result)
tukey.two.way


tukey.plot.aov<-aov( Weight_kg ~ year:treatment, data=bh_yield_2019_2020_2021_sublocks)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

str(tukey.plot.aov)


mean.yield.data <- bh_yield_2019_2020_2021_sublocks%>%
  group_by(year,treatment) %>%
  summarise(
    Weight_kg = mean(Weight_kg)
  )%>%
  mutate(year.t = year:treatment)

mean.yield.data$year.t<-as.character(mean.yield.data$year.t)
str(mean.yield.data)

(test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE))
letters <- data.frame(year = rownames(test$groups), letters = test$groups$groups)
str(letters)
letters <- letters %>%
  mutate(year.t = str_replace(year, "\\.", ":"))

str(letters) 


mean.yield.data <- merge(mean.yield.data, letters, by = "year.t", all.x = TRUE)
mean.yield.data$year<-mean.yield.data$year.x
str(mean.yield.data)

two.way.plot <- ggplot(bh_yield_2019_2020_2021_sublocks, aes(x = year, y = Weight_kg, group=treatment)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange')

two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$letters, vjust = -8, size = 5) +
  facet_wrap(~ treatment)+theme_classic()


ggsave(two.way.plot, filename = "figures/two.way.plot_ANOVA_treatment_year.pdf", device = cairo_pdf, width = 10, height = 8)
#####THREEE WAY ANOVA WEIGHT_KG#####


# Ensure the necessary columns are factors
bh_yield_2019_2020_2021_sublocks$year <- as.factor(bh_yield_2019_2020_2021_sublocks$year)
bh_yield_2019_2020_2021_sublocks$treatment <- as.factor(bh_yield_2019_2020_2021_sublocks$treatment)
bh_yield_2019_2020_2021_sublocks$block_sublock <- as.factor(bh_yield_2019_2020_2021_sublocks$block_sublock)

str(bh_yield_2019_2020_2021_sublocks)
# Define the model formula for the three-way ANOVA
model_formula <- Weight_kg ~ year * treatment * block_sublock

# Run the ANOVA
anova_result <- aov(model_formula, data = bh_yield_2019_2020_2021_sublocks)

# Summarize the results

summary(anova_result)

# Tukey HSD for treatment
tukey_treatment <- TukeyHSD(anova_result, "treatment")
(test <- HSD.test(anova_result, "treatment", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for block_sublock
tukey_block_sublock <- TukeyHSD(anova_result, "block_sublock")
(test <- HSD.test(anova_result, "block_sublock", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year
tukey_block_sublock <- TukeyHSD(anova_result, "year")
(test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year:block_sublock interaction
interaction_term <- interaction(bh_yield_2019_2020_2021_sublocks$year, bh_yield_2019_2020_2021_sublocks$block_sublock)
anova_result_interaction <- aov(Weight_kg ~ interaction_term + treatment, data = bh_yield_2019_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term")

(test <- HSD.test(anova_result_interaction, "interaction_term", alpha = 0.05, unbalanced = TRUE))


# Tukey HSD for year:treatment interaction
interaction_term_treatment <- interaction(bh_yield_2019_2020_2021_sublocks$year, bh_yield_2019_2020_2021_sublocks$treatment)
anova_result_interaction <- aov(Weight_kg ~ interaction_term_treatment + treatment, data = bh_yield_2019_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term_treatment")

(test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE))

str(bh_yield_2019_2020_2021_sublocks)
####ONE WAY ANOVA-TUKEY HSD OF EACH SUBLOCK WITH YEAR AS A FACTOR FOR KG/PLANT ####
block_sublocks <- unique(bh_yield_2019_2020_2021_sublocks$block_sublock)


# Initialize an empty data frame to store the results
results <- data.frame()

# Initialize an empty results data frame
results <- data.frame()

# Iterate over each unique block_sublock
for (block in block_sublocks) {
  # Subset data for the current block_sublock
  subset_data <- subset(bh_yield_2019_2020_2021_sublocks, block_sublock == block)
  
  # Check if there are at least two levels of 'year'
  if (length(unique(subset_data$year)) < 2) {
    # Skip this iteration if there are fewer than 2 levels
    next
  }
  
  # Perform one-way ANOVA with year as the factor
  model_formula <- Weight_kg ~ year
  anova_result <- aov(model_formula, data = subset_data)
  
  # Extract p-value for the year factor
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result, "year")
  
  # Extract means
  means <- aggregate(subset_data$Weight_kg, by = list(subset_data$year), FUN = mean)
  colnames(means) <- c("year", "mean")
  
  # Extract standard errors
  standard_errors <- aggregate(subset_data$Weight_kg, by = list(subset_data$year), FUN = function(x) sd(x)/sqrt(length(x)))
  colnames(standard_errors) <- c("year", "standard_error")
  
  # Extract letters of significance 
  test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE)
  letters <- data.frame(year = rownames(test$groups), letters = test$groups$groups)
  
  # Create a dataframe for results of the current variable
  variable_result_df <- data.frame(
    block_sublock = block,
    year = levels(subset_data$year),
    p_value = p_value,
    stringsAsFactors = FALSE
  )
  
  # Merge means, standard errors, and letters with variable_result_df
  variable_result_df <- merge(variable_result_df, means, by = "year", all.x = TRUE)
  variable_result_df <- merge(variable_result_df, standard_errors, by = "year", all.x = TRUE)
  variable_result_df <- merge(variable_result_df, letters, by = "year", all.x = TRUE)
  
  # Add significance based on p-value
  variable_result_df$Significance <- case_when(
    variable_result_df$p_value < 0.001 ~ "***",
    variable_result_df$p_value < 0.01 ~ "**",
    variable_result_df$p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
  
  # Append the summary statistics to the results data frame
  results <- rbind(results, variable_result_df)
}


results$Mean_sem <- paste(round(results$mean, 2), "±", round(results$standard_error, 2),results$letters)

result_sublocks_one_way_anova_year_factor_weight_kg<- results
write.csv(result_sublocks_one_way_anova_year_factor_weight_kg,"data_output/result_sublocks_one_way_anova_year_factor_weight_kg.csv")
####TWO WAY ANOVA gr/cluster#####

two.way <- aov(gr_cluster ~ year + treatment , data = bh_yield_2019_2020_2021_sublocks)
interaction <- aov(gr_cluster ~ year * treatment , data = bh_yield_2019_2020_2021_sublocks)
blocking <- aov(gr_cluster ~ year * treatment + block_sublock, data = bh_yield_2019_2020_2021_sublocks)


library(AICcmodavg)

model.set <- list(two.way, interaction, blocking)
model.names <- c("two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)


str(bh_yield_2019_2020_2021_sublocks)
# Define the model formula for the three-way ANOVA
model_formula <- gr_cluster ~ year * treatment 

# Run the ANOVA
anova_result <- aov(model_formula, data = bh_yield_2019_2020_2021_sublocks)

# Summarize the results

summary(anova_result)

# Tukey HSD for treatment
tukey_treatment <- TukeyHSD(anova_result, "treatment")
(test <- HSD.test(anova_result, "treatment", alpha = 0.05, unbalanced = TRUE))
# Tukey HSD for block_sublock
tukey_block_sublock <- TukeyHSD(anova_result, "block_sublock")
(test <- HSD.test(anova_result, "block_sublock", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year
tukey_block_sublock <- TukeyHSD(anova_result, "year")
(test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE))


# Tukey HSD for year:treatment interaction
interaction_term_treatment <- interaction(bh_yield_2019_2020_2021_sublocks$year, bh_yield_2019_2020_2021_sublocks$treatment)
anova_result_interaction <- aov(gr_cluster ~ interaction_term_treatment + treatment, data = bh_yield_2019_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term_treatment")

(test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE))

tukey.plot.aov<-aov( gr_cluster ~ year:treatment, data=bh_yield_2019_2020_2021_sublocks)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

str(tukey.plot.aov)


mean.yield.data <- bh_yield_2019_2020_2021_sublocks%>%
  group_by(year,treatment) %>%
  summarise(
    gr_cluster = mean(gr_cluster)
  )%>%
  mutate(year.t = year:treatment)

mean.yield.data$year.t<-as.character(mean.yield.data$year.t)
str(mean.yield.data)

(test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE))
letters <- data.frame(year = rownames(test$groups), letters = test$groups$groups)
str(letters)
letters <- letters %>%
  mutate(year.t = str_replace(year, "\\.", ":"))

str(letters) 


mean.yield.data <- merge(mean.yield.data, letters, by = "year.t", all.x = TRUE)
mean.yield.data$year<-mean.yield.data$year.x
str(mean.yield.data)

two.way.plot <- ggplot(bh_yield_2019_2020_2021_sublocks, aes(x = year, y = gr_cluster, group=treatment)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange')

two.way.plot <- two.way.plot +
  geom_text(data=mean.yield.data, label=mean.yield.data$letters, vjust = -8, size = 5) +
  facet_wrap(~ treatment)+theme_classic()



ggsave(two.way.plot, filename = "figures/two.way.plot_ANOVA_treatment_year_gr_cluster.pdf", device = cairo_pdf, width = 10, height = 8)
#####THREEE WAY ANOVA gr/cluster#####


str(bh_yield_2019_2020_2021_sublocks)
# Define the model formula for the three-way ANOVA
model_formula <- gr_cluster ~ year * treatment * block_sublock

# Run the ANOVA
anova_result <- aov(model_formula, data = bh_yield_2019_2020_2021_sublocks)

# Summarize the results

summary(anova_result)

# Tukey HSD for treatment
tukey_treatment <- TukeyHSD(anova_result, "treatment")
(test <- HSD.test(anova_result, "treatment", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for block_sublock
tukey_block_sublock <- TukeyHSD(anova_result, "block_sublock")
(test <- HSD.test(anova_result, "block_sublock", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year
tukey_block_sublock <- TukeyHSD(anova_result, "year")
(test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year:block_sublock interaction
interaction_term <- interaction(bh_yield_2019_2020_2021_sublocks$year, bh_yield_2019_2020_2021_sublocks$block_sublock)
anova_result_interaction <- aov(gr_cluster ~ interaction_term + treatment, data = bh_yield_2019_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term")

(test <- HSD.test(anova_result_interaction, "interaction_term", alpha = 0.05, unbalanced = TRUE))


# Tukey HSD for year:treatment interaction
interaction_term_treatment <- interaction(bh_yield_2019_2020_2021_sublocks$year, bh_yield_2019_2020_2021_sublocks$treatment)
anova_result_interaction <- aov(gr_cluster ~ interaction_term_treatment + treatment, data = bh_yield_2019_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term_treatment")

(test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE))



####ONE WAY ANOVA-TUKEY HSD OF EACH SUBLOCK WITH YEAR AS A FACTOR FOR GR/CLUSTER ####
block_sublocks <- unique(bh_yield_2019_2020_2021_sublocks$block_sublock)


# Initialize an empty data frame to store the results
results <- data.frame()


# Iterate over each unique block_sublock
for (block in block_sublocks) {
  # Subset data for the current block_sublock
  subset_data <- subset(bh_yield_2019_2020_2021_sublocks, block_sublock == block)
  
  # Check if there are at least two levels of 'year'
  if (length(unique(subset_data$year)) < 2) {
    # Skip this iteration if there are fewer than 2 levels
    next
  }
  
  # Perform one-way ANOVA with year as the factor
  model_formula <- gr_cluster ~ year
  anova_result <- aov(model_formula, data = subset_data)
  
  # Extract p-value for the year factor
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result, "year")
  
  # Extract means
  means <- aggregate(subset_data$gr_cluster, by = list(subset_data$year), FUN = mean)
  colnames(means) <- c("year", "mean")
  
  # Extract standard errors
  standard_errors <- aggregate(subset_data$gr_cluster, by = list(subset_data$year), FUN = function(x) sd(x)/sqrt(length(x)))
  colnames(standard_errors) <- c("year", "standard_error")
  
  # Extract letters of significance 
  test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE)
  letters <- data.frame(year = rownames(test$groups), letters = test$groups$groups)
  
  # Create a dataframe for results of the current variable
  variable_result_df <- data.frame(
    block_sublock = block,
    year = levels(subset_data$year),
    p_value = p_value,
    stringsAsFactors = FALSE
  )
  
  # Merge means, standard errors, and letters with variable_result_df
  variable_result_df <- merge(variable_result_df, means, by = "year", all.x = TRUE)
  variable_result_df <- merge(variable_result_df, standard_errors, by = "year", all.x = TRUE)
  variable_result_df <- merge(variable_result_df, letters, by = "year", all.x = TRUE)
  
  # Add significance based on p-value
  variable_result_df$Significance <- case_when(
    variable_result_df$p_value < 0.001 ~ "***",
    variable_result_df$p_value < 0.01 ~ "**",
    variable_result_df$p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
  
  # Append the summary statistics to the results data frame
  results <- rbind(results, variable_result_df)
}


results$Mean_sem <- paste(round(results$mean, 2), "±", round(results$standard_error, 2),results$letters)

result_sublocks_one_way_anova_year_factor_gr_cluster<- results
write.csv(result_sublocks_one_way_anova_year_factor_gr_cluster,"data_output/result_sublocks_one_way_anova_year_factor_gr_cluster.csv")

#####THREE-WAY ANOVA BERRY DIAMETER####
# Ensure the necessary columns are factors
bh_berry_diameter_2020_2021_sublocks$year <- as.factor(bh_berry_diameter_2020_2021_sublocks$year)
bh_berry_diameter_2020_2021_sublocks$treatment <- as.factor(bh_berry_diameter_2020_2021_sublocks$treatment)
bh_berry_diameter_2020_2021_sublocks$block_sublock <- as.factor(bh_berry_diameter_2020_2021_sublocks$block_sublock)

str(bh_berry_diameter_2020_2021_sublocks)
# Define the model with explicit three-way interaction term
model_formula <- berry_diameter ~ year * treatment * block_sublock

# Run the ANOVA
anova_result <- aov(model_formula, data = bh_berry_diameter_2020_2021_sublocks)

# Summarize the results to see all interaction terms
summary(anova_result, split = list(year = list("main" = 1, "year:treatment" = 2, "year:block_sublock" = 3, "year:treatment:block_sublock" = 4)))


# Define the model formula for the three-way ANOVA

model_formula <- berry_diameter ~ year * treatment * block_sublock

# Run the ANOVA
anova_result <- aov(model_formula, data = bh_berry_diameter_2020_2021_sublocks)

# Summarize the results

summary(anova_result)

# Tukey HSD for treatment
tukey_treatment <- TukeyHSD(anova_result, "treatment")
(test <- HSD.test(anova_result, "treatment", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for block_sublock
tukey_block_sublock <- TukeyHSD(anova_result, "block_sublock")
(test <- HSD.test(anova_result, "block_sublock", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year
tukey_block_sublock <- TukeyHSD(anova_result, "year")
(test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year:block_sublock interaction
interaction_term <- interaction(bh_berry_diameter_2020_2021_sublocks$year, bh_berry_diameter_2020_2021_sublocks$block_sublock)
anova_result_interaction <- aov(berry_diameter ~ interaction_term + treatment, data = bh_berry_diameter_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term")

(test <- HSD.test(anova_result_interaction, "interaction_term", alpha = 0.05, unbalanced = TRUE))


# Tukey HSD for year:treatment interaction
interaction_term_treatment <- interaction(bh_berry_diameter_2020_2021_sublocks$year, bh_berry_diameter_2020_2021_sublocks$treatment)
anova_result_interaction <- aov(berry_diameter ~ interaction_term_treatment + treatment, data = bh_berry_diameter_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term_treatment")

(test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE))

####ONE WAY ANOVA-TUKEY HSD OF EACH SUBLOCK WITH YEAR AS A FACTOR FOR TOTAL CLUSTERS ####
block_sublocks <- unique(bh_yield_2019_2020_2021_sublocks$block_sublock)

# Initialize an empty data frame to store the results
results <- data.frame()

# Iterate over each unique block_sublock
for (block in block_sublocks) {
  # Subset data for the current block_sublock
  subset_data <- subset(bh_yield_2019_2020_2021_sublocks, block_sublock == block)
  
  # Check if there are at least two levels of 'year'
  if (length(unique(subset_data$year)) < 2) {
    # Skip this iteration if there are fewer than 2 levels
    next
  }
  
  # Perform one-way ANOVA with year as the factor
  model_formula <- total.clusters ~ year
  anova_result <- aov(model_formula, data = subset_data)
  
  # Extract p-value for the year factor
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  
  # Perform Tukey's HSD test
  tukey_result <- TukeyHSD(anova_result, "year")
  
  # Extract means
  means <- aggregate(subset_data$total.clusters, by = list(subset_data$year), FUN = mean)
  colnames(means) <- c("year", "mean")
  
  # Extract standard errors
  standard_errors <- aggregate(subset_data$total.clusters, by = list(subset_data$year), FUN = function(x) sd(x)/sqrt(length(x)))
  colnames(standard_errors) <- c("year", "standard_error")
  
  # Extract letters of significance 
  test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE)
  letters <- data.frame(year = rownames(test$groups), letters = test$groups$groups)
  
  # Create a dataframe for results of the current variable
  variable_result_df <- data.frame(
    block_sublock = block,
    year = levels(subset_data$year),
    p_value = p_value,
    stringsAsFactors = FALSE
  )
  
  # Merge means, standard errors, and letters with variable_result_df
  variable_result_df <- merge(variable_result_df, means, by = "year", all.x = TRUE)
  variable_result_df <- merge(variable_result_df, standard_errors, by = "year", all.x = TRUE)
  variable_result_df <- merge(variable_result_df, letters, by = "year", all.x = TRUE)
  
  # Add significance based on p-value
  variable_result_df$Significance <- case_when(
    variable_result_df$p_value < 0.001 ~ "***",
    variable_result_df$p_value < 0.01 ~ "**",
    variable_result_df$p_value < 0.05 ~ "*",
    TRUE ~ ""
  )
  
  # Append the summary statistics to the results data frame
  results <- rbind(results, variable_result_df)
}

results$Mean_sem <- paste(round(results$mean, 2), "±", round(results$standard_error, 2),results$letters)

result_sublocks_one_way_anova_year_factor_total.clusters<- results
write.csv(result_sublocks_one_way_anova_year_factor_total.clusters,"data_output/result_sublocks_one_way_anova_year_factor_total.clusters.csv")

#####TWO-WAY ANOVA BERRY DIAMETER####
# Ensure the necessary columns are factors
bh_berry_diameter_2020_2021_sublocks$year <- as.factor(bh_berry_diameter_2020_2021_sublocks$year)
bh_berry_diameter_2020_2021_sublocks$treatment <- as.factor(bh_berry_diameter_2020_2021_sublocks$treatment)
bh_berry_diameter_2020_2021_sublocks$block_sublock <- as.factor(bh_berry_diameter_2020_2021_sublocks$block_sublock)

str(bh_berry_diameter_2020_2021_sublocks)
# Define the model formula for the two-way ANOVA
model_formula <- berry_diameter ~ year * treatment 

# Run the ANOVA
anova_result <- aov(model_formula, data = bh_berry_diameter_2020_2021_sublocks)

# Summarize the results

summary(anova_result)

# Tukey HSD for treatment
tukey_treatment <- TukeyHSD(anova_result, "treatment")
(test <- HSD.test(anova_result, "treatment", alpha = 0.05, unbalanced = TRUE))

# Tukey HSD for year
tukey_block_sublock <- TukeyHSD(anova_result, "year")
(test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE))


# Tukey HSD for year:treatment interaction
interaction_term_treatment <- interaction(bh_berry_diameter_2020_2021_sublocks$year, bh_berry_diameter_2020_2021_sublocks$treatment)
anova_result_interaction <- aov(berry_diameter ~ interaction_term_treatment + treatment, data = bh_berry_diameter_2020_2021_sublocks)
tukey_interaction <- TukeyHSD(anova_result_interaction, "interaction_term_treatment")

(test <- HSD.test(anova_result_interaction, "interaction_term_treatment", alpha = 0.05, unbalanced = TRUE))



####ONE WAY ANOVA-TUKEY HSD OF EACH SUBLOCK WITH YEAR AS A FACTOR FOR BERRY DIAMETER ####
block_sublocks <- unique(bh_berry_diameter_2020_2021_sublocks$block_sublock)

# Initialize an empty data frame to store the results
results <- data.frame()

# Initialize an empty data frame to store the results
results <- data.frame()

# Iterate over each unique block_sublock
for (block in block_sublocks) {
  # Subset data for the current block_sublock
  subset_data <- subset(bh_berry_diameter_2020_2021_sublocks, block_sublock == block)
  
  # Check if the year factor has at least two levels
  if (length(unique(subset_data$year)) > 1) {
    # Perform one-way ANOVA with year as the factor
    model_formula <- berry_diameter ~ year
    anova_result <- aov(model_formula, data = subset_data)
    
    # Extract p-value for the year factor
    p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
    
    # Perform Tukey's HSD test
    tukey_result <- TukeyHSD(anova_result, "year")
    
    # Extract means
    means <- aggregate(subset_data$berry_diameter, by = list(subset_data$year), FUN = mean)
    colnames(means) <- c("year", "mean")
    
    # Extract standard errors
    standard_errors <- aggregate(subset_data$berry_diameter, by = list(subset_data$year), FUN = function(x) sd(x)/sqrt(length(x)))
    colnames(standard_errors) <- c("year", "standard_error")
    
    # Extract letters of significance 
    test <- HSD.test(anova_result, "year", alpha = 0.05, unbalanced = TRUE)
    letters <- data.frame(year = rownames(test$groups), letters = test$groups$groups)
    
    # Create a dataframe for results of the current variable
    variable_result_df <- data.frame(
      block_sublock = block,
      year = means$year,
      p_value = p_value,
      mean = means$mean,
      standard_error = standard_errors$standard_error,
      letters = letters$letters,
      stringsAsFactors = FALSE
    )
    
    # Add significance based on p-value
    variable_result_df$Significance <- case_when(
      variable_result_df$p_value < 0.001 ~ "***",
      variable_result_df$p_value < 0.01 ~ "**",
      variable_result_df$p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
    
    # Append the summary statistics to the results data frame
    results <- rbind(results, variable_result_df)
  } else {
    # Handle the case where the year factor does not have enough levels
    variable_result_df <- data.frame(
      block_sublock = block,
      year = unique(subset_data$year),
      p_value = NA,
      mean = mean(subset_data$berry_diameter),
      standard_error = sd(subset_data$berry_diameter) / sqrt(length(subset_data$berry_diameter)),
      letters = NA,
      Significance = NA,
      stringsAsFactors = FALSE
    )
    
    # Append the summary statistics to the results data frame
    results <- rbind(results, variable_result_df)
  }
}


results$Mean_sem <- paste(round(results$mean, 2), "±", round(results$standard_error, 2),results$letters)

result_sublocks_one_way_anova_year_factor_berry_diameter<- results
write.csv(result_sublocks_one_way_anova_year_factor_berry_diameter,"data_output/result_sublocks_one_way_anova_year_factor_berry_diameter.csv")

