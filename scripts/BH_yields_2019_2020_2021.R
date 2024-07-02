#BH YIELD 2019 - 2020 -2021
#####Yield 2019 ####
library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(xtable)
library(agricolae)
library(gtools)

pd2 <- position_dodge(0.75)
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

yield_bh_2019_tally<-yield_bh_2019_grouped2%>%
  group_by(Treatment)%>%
  tally()


yield_bh_2019_boxplot_2<-yield_bh_2019_grouped2 %>%
  ggplot(aes(Treatment, gr_cluster))+
  geom_boxplot(alpha =0.75, aes(fill = Treatment, group = Treatment))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  geom_point(alpha =0.9, aes(color = Treatment, group =Treatment, shape =Rep), size = 2.3)+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic()+
  scale_shape_manual(name = "Replication", values = c( 15, 16, 17),labels = c ("Rep 1", "Rep 2", "Rep 3")) +
  theme_classic()+
  ylab("grs/cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  scale_x_discrete(labels=c("1" = "Baseline (60% ET)", "2" = "2x baseline ET",  "3" = "3x baseline ET")) +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(40,160,20), limits = c (40,160)) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(yield_bh_2019_boxplot_2, filename = "figures/yield_bh_2019_boxplot_2.pdf", device = cairo_pdf, 
       width = 9, height = 7)

##### Kg/vine boxplot


yield_bh_2019_boxplot_2_kg_vine<-yield_bh_2019_grouped2 %>%
  ggplot(aes(Treatment, Weight_kg))+
  geom_boxplot(alpha =0.75, aes(fill = Treatment, group = Treatment))+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  geom_point(alpha =0.9, aes(color = Treatment, group =Treatment, shape =Rep), size = 2.3)+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  theme_classic()+
  scale_shape_manual(name = "Replication", values = c( 15, 16, 17),labels = c ("Rep 1", "Rep 2", "Rep 3")) +
  theme_classic()+
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  scale_x_discrete(labels=c("1" = "Baseline (60% ET)", "2" = "2x baseline ET",  "3" = "3x baseline ET")) +
  theme(axis.title.y = element_text(size=14, family = "serif")) +
  theme(axis.title.x = element_text(size=14, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(2,16,2), limits = c (2,16)) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))
  


ggsave(yield_bh_2019_boxplot_2_kg_vine, filename = "figures/yield_bh_2019_boxplot_2_kg_vine.pdf", device = cairo_pdf, 
       width = 9, height = 7)
library(cowplot)

yield_bh_2019_final_Color <- plot_grid (yield_bh_2019_boxplot_2_kg_vine,yield_bh_2019_boxplot_2, ncol=2, nrow = 1)

ggsave(yield_bh_2019_final_Color , filename = "figures/yield_bh_2019_final_Color.pdf", device = cairo_pdf, width = 14, height = 6)


#### Mean and SE yield components

yield_bh_2019_grouped2 %>%
  group_by(Treatment)%>%
  tally()

se <- function(x) sqrt(var(x)/length(x))

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

yield_bh_2019_avg_se<- yield_bh_2019_grouped2 %>%
  group_by(Treatment)%>%
  summarise(avg_kg_vine = mean(Weight_kg), sev_kg_vine = se(Weight_kg), avg_num_clusters = mean(Clusters), sev_num_clusters = se(Clusters),avg_gr_cluster = mean(gr_cluster), sev_gr_cluster = se(gr_cluster))

write.csv(yield_bh_2019_avg_se,"data_output/yield_bh_2019_avg_se.csv")


#####ANOVA yield 2019######

#### KG PER VINE
yield_bh_2019_grouped2 %>%
  group_by(Treatment) %>%
  tally()


is.factor(yield_bh_2019_grouped2$Treatment)

yield_bh_2019_grouped2$Treatment<- format(yield_bh_2019_grouped2$Treatment)
yield_bh_2019_grouped2$Treatment<- as.factor(yield_bh_2019_grouped2$Treatment)

is.factor(yield_bh_2019_grouped2$Treatment)

ggplot(yield_bh_2019_grouped2, aes (Treatment,Weight_kg, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()

anova_yield_kg_vine <- aov (Weight_kg~Treatment, yield_bh_2019_grouped2)
summary  (anova_yield_kg_vine)



tukey<-TukeyHSD(aov(Weight_kg~Treatment, yield_bh_2019_grouped2))

(test<- HSD.test(anova_yield_kg_vine, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

str(test)
as.data.frame(test$groups)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped2_hist$Weight_kg)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped2_hist$Weight_kg)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped2_hist$Weight_kg)


#library(xtable)
#anova_yield_kg_vine_print<-xtable(anova_yield_kg_vine)


#write.csv(anova_yield_kg_vine_print,"data_output/anova_yield_kg_vine_2019.csv")

#tukey_yield_weight_kg_harvest_2019<-as.data.frame(test$groups)
#tukey_yield_weight_kg_harvest_2019<-reorder(tukey_yield_weight_kg_harvest_2019,col(1))
#str(tukey_yield_weight_kg_harvest_2019)

#write.csv(tukey_yield_weight_kg_harvest_2019,"data_output/tukey_yield_weight_kg_harvest_2019.csv")

#mean_sd_weight_kg_harvest_2019<-as.data.frame(test$means)
#write.csv(mean_sd_weight_kg_harvest_2019,"data_output/mean_sd_weight_kg_harvest_2019.csv")

#se_df<- yield_2019_anova%>%
#  group_by(Treatment)%>% 
#  summarise(sev =se(Weight_kg))

#####Anova table 2019 yield kg/vine and gr/cluster
yield_2019_anova<-yield_bh_2019_grouped2%>%
  select (Treatment, Weight_kg, gr_cluster)

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(yield_2019_anova)
variables<-c("Weight_kg", "gr_cluster")

result_df_weight_kg_2019 <- NULL
result_df_gr_cluster_2019 <- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ Treatment")), data = yield_2019_anova)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ Treatment")), yield_2019_anova))
  (test<- HSD.test(anova_result, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))
  tukey_yield_2019<-as.data.frame(test$groups)
  mean_sd_harvest_2019<-as.data.frame(test$means) 
  mean_sd_harvest_2019$treatment<-c(1, 2, 3)
  se_df <- yield_2019_anova %>%
    group_by(Treatment) %>% 
    summarise(sem = se(!!sym(variable)))
  variable_result_df_1<-merge(mean_sd_harvest_2019,
                              tukey_yield_2019, by = variable)
  variable_result_df2<- cbind(variable_result_df_1,
                             p_value_df)
  ordered_df <- variable_result_df2[order(variable_result_df2$treatment), ]
  variable_result_df<- cbind(ordered_df, se_df)
  variable_result_df <- variable_result_df %>%
    mutate(Significance = case_when(
      p_values < 0.001 ~ "***",
      p_values < 0.01 ~ "**",
      p_values < 0.05 ~ "*",
    ))
  # Decide which table to append based on the variable
  if (variable == "Weight_kg") {
    # Append to result_df_weight_kg
    result_df_weight_kg_2019 <- variable_result_df
  } else if (variable == "gr_cluster") {
    # Append to result_df_gr_cluster
    result_df_gr_cluster_2019 <- variable_result_df
  }
 else {
  print(paste("Error processing", variable))
 }
}

result_df_weight_kg_2019$Mean_sem <- paste(round(result_df_weight_kg_2019$Weight_kg, 2), "±", round(result_df_weight_kg_2019$sem, 2),result_df_weight_kg_2019$groups)


result_df_gr_cluster_2019$Mean_sem <- paste(round(result_df_gr_cluster_2019$gr_cluster, 2), "±", round(result_df_gr_cluster_2019$sem, 2),result_df_gr_cluster_2019$groups)


write.csv(result_df_weight_kg_2019,"data_output/result_df_weight_kg_2019_anova.csv")
write.csv(result_df_gr_cluster_2019,"data_output/result_df_gr_cluster_2019_anova.csv")

#### gr PER CLUSTER

yield_bh_2019_grouped2 %>%
  group_by(Treatment) %>%
  tally()


is.factor(yield_bh_2019_grouped2$Treatment)

yield_bh_2019_grouped2$Treatment<- format(yield_bh_2019_grouped2$Treatment)
yield_bh_2019_grouped2$Treatment<- as.factor(yield_bh_2019_grouped2$Treatment)

is.factor(yield_bh_2019_grouped2$Treatment)

ggplot(yield_bh_2019_grouped2, aes (Treatment,gr_cluster, group =Treatment)) +
  geom_boxplot(aes(fill = Treatment, color =Treatment)) +
  geom_point()

anova_yield_gr_cluster <- aov (gr_cluster~Treatment, yield_bh_2019_grouped2)
summary  (anova_yield_gr_cluster)

str(anova_yield_gr_cluster)




tukey<-TukeyHSD(aov(gr_cluster~Treatment, yield_bh_2019_grouped2))

(test<- HSD.test(anova_yield_gr_cluster, trt = "Treatment", alpha =0.05, unbalanced = "TRUE"))

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 1)
hist(yield_bh_2019_grouped2_hist$gr_cluster)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 2)
hist(yield_bh_2019_grouped2_hist$gr_cluster)

yield_bh_2019_grouped2_hist<-yield_bh_2019_grouped2 %>%
  filter(Treatment == 3)
hist(yield_bh_2019_grouped2_hist$gr_cluster)

#library(xtable)
#anova_yield_gr_cluster_print<-xtable(anova_yield_gr_cluster)


#write.csv(anova_yield_gr_cluster_print,"data_output/anova_yield_gr_cluster_2019.csv")

#tukey_yield_gr_cluster_harvest_2019<-as.data.frame(test$groups)
#write.csv(tukey_yield_gr_cluster_harvest_2019,"data_output/tukey_yield_gr_cluster_harvest_2019.csv")



####Yield_2020_cleaned####
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


yield_2020_cleaned$date<-format(yield_2020_cleaned$date)
yield_2020_cleaned$date<-as.factor(yield_2020_cleaned$date)


yield_2020_cleaned$treatment<-format(yield_2020_cleaned$treatment)
yield_2020_cleaned$treatment<-as.character(yield_2020_cleaned$treatment)

str(yield_2020_cleaned)


yield_2020_cleaned_tally<-yield_2020_cleaned%>%
  group_by(treatment)%>%
  tally()
write.csv(yield_2020_cleaned_tally,"data_output/yield_2020_cleaned_tally.csv")


yield_kg_vine_2020_cleaned<-yield_2020_cleaned%>%
  ggplot(aes(treatment,Weight_kg))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(4,14,2), limits = c (4,14)) +
  scale_x_discrete(labels = c('Baseline (60% ET)', '2x baseline ET', '3x baseline ET'))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))



ggsave(yield_kg_vine_2020_cleaned, filename = "figures/yield_kg_vine_2020_cleaned.pdf", device = cairo_pdf, 
       width = 8, height = 6)

yield_kg_vine_2020_block_cleaned<-yield_2020_cleaned%>%
  ggplot(aes(block,Weight_kg))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Block") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(4,14,2), limits = c (4,14))  +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))


ggsave(yield_kg_vine_2020_block_cleaned, filename = "figures/yield_kg_vine_2020_block_cleaned.pdf", device = cairo_pdf, 
       width = 8, height = 6)



yield_kg_vine_2020_subblock_cleaned<-yield_2020_cleaned%>%
  ggplot(aes(block_sublock,Weight_kg))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Subblock") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(4,14,2), limits = c (4,14))   +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(yield_kg_vine_2020_subblock_cleaned, filename = "figures/yield_kg_vine_2020_subblock_cleaned.pdf", device = cairo_pdf, 
       width = 18, height = 8)


yield_grams_cluster_2020_cleaned<-yield_2020_cleaned%>%
  ggplot(aes(treatment,gr_cluster))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("grs/cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_x_discrete(labels = c('Baseline (60% ET)', '2x baseline ET', '3x baseline ET'))+  
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(yield_grams_cluster_2020_cleaned, filename = "figures/yield_grams_cluster_2020_cleaned.pdf", device = cairo_pdf, 
       width = 8, height = 6)

####anova yield 2020 cleaned#####
yield_anova_2020_cleaned<-yield_2020_cleaned%>%
  select (treatment, Weight_kg, gr_cluster)

yield_anova_2020_cleaned$treatment<- format(yield_anova_2020_cleaned$treatment)
yield_anova_2020_cleaned$treatment<- as.factor(yield_anova_2020_cleaned$treatment)


yield_anova_2020_cleaned %>%
  group_by(treatment)%>%
  tally()

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(yield_anova_2020_cleaned)
variables<-c("Weight_kg", "gr_cluster")

result_df_weight_kg_2020_cleaned <- NULL
result_df_gr_cluster_2020_cleaned <- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = yield_anova_2020_cleaned)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), yield_anova_2020_cleaned))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- yield_anova_2020_cleaned %>%
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
  if (variable == "Weight_kg") {
    # Append to result_df_weight_kg
    result_df_weight_kg_2020_cleaned <- variable_result_df
  } else if (variable == "gr_cluster") {
    # Append to result_df_gr_cluster
    result_df_gr_cluster_2020_cleaned <- variable_result_df
  }
  else {
    print(paste("Error processing", variable))
  }
}

result_df_weight_kg_2020_cleaned$Mean_sem <- paste(round(result_df_weight_kg_2020_cleaned$Weight_kg, 2), "±", round(result_df_weight_kg_2020_cleaned$sem, 2),result_df_weight_kg_2020_cleaned$groups)


result_df_gr_cluster_2020_cleaned$Mean_sem <- paste(round(result_df_gr_cluster_2020_cleaned$gr_cluster, 2), "±", round(result_df_gr_cluster_2020_cleaned$sem, 2),result_df_gr_cluster_2020_cleaned$groups)

write.csv(result_df_weight_kg_2020_cleaned,"data_output/result_df_weight_kg_2020_anova_cleaned.csv")
write.csv(result_df_gr_cluster_2020_cleaned,"data_output/result_df_gr_cluster_2020_anova_cleaned.csv")

#weight_kg
library(agricolae)

yield_anova<-yield_2020_cleaned

str(yield_anova$Weight_kg) 

yield_anova %>%
  group_by(treatment)%>%
  tally()

is.factor(yield_anova$treatment)

yield_anova$treatment<- format(yield_anova$treatment)
yield_anova$treatment<- as.factor(yield_anova$treatment)

is.factor(yield_anova$treatment)

str(yield_anova)

ggplot(yield_anova, aes (treatment,Weight_kg, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_Weight_kg<- aov (Weight_kg~treatment, yield_anova)
summary (anova_Weight_kg)

str(test)

TukeyHSD(aov(Weight_kg~treatment, yield_anova))

(test<- HSD.test(anova_weight_kg , trt = "treatment", alpha =0.05))


str(test)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 1)
hist(yield_anova_hist$Weight_kg)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 2)
hist(yield_anova_hist$Weight_kg)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 3)
hist(yield_anova_hist$Weight_kg)

library(xtable)
anova_Weight_kg<-xtable(anova_Weight_kg)

write.csv(anova_Weight_kg,"data_output/anova_Weight_kg_cleaned.csv")

tukey_yield_weight_kgs_harvest_2020_cleaned<-as.data.frame(test$groups)
write.csv(tukey_yield_weight_kgs_harvest_2020_cleaned,"data_output/tukey_yield_weight_kgs_harvest_2020_cleaned.csv")



#####gr/cluster

yield_anova<-yield_2020_cleaned

str(yield_anova$Weight_kg) 

yield_anova %>%
  group_by(treatment)%>%
  tally()

is.factor(yield_anova$treatment)

yield_anova$treatment<- format(yield_anova$treatment)
yield_anova$treatment<- as.factor(yield_anova$treatment)

is.factor(yield_anova$treatment)

str(yield_anova)

ggplot(yield_anova, aes (treatment,gr_cluster, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_gr_cluster<- aov (gr_cluster~treatment, yield_anova)
summary (anova_gr_cluster)

TukeyHSD(aov(gr_cluster~treatment, yield_anova))

(test<- HSD.test(anova_gr_cluster , trt = "treatment", alpha =0.05))


str(test)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 1)
hist(yield_anova_hist$gr_cluster)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 2)
hist(yield_anova_hist$gr_cluster)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 3)
hist(yield_anova_hist$gr_cluster)

library(xtable)
anova_gr_cluster<-xtable(anova_gr_cluster)

write.csv(anova_gr_cluster,"data_output/anova_gr_cluster_2020_cleaned.csv")

tukey_yield_gr_cluster_harvest_2020_cleaned<-as.data.frame(test$groups)
write.csv(tukey_yield_gr_cluster_harvest_2020_cleaned,"data_output/tukey_yield_gr_cluster_harvest_2020_cleaned.csv")


#####Berry diameter 2020#####


berry_diameter_BH_2020<- read.csv("data/berry_diameter_2020_harvest_blocks.csv", header =TRUE)
str(berry_diameter_BH_2020$treatment)  

berry_diameter_BH_2020<-berry_diameter_BH_2020%>%
  select(!treatment)


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


berry_diameter_BH_2020$date<-format(berry_diameter_BH_2020$date)
berry_diameter_BH_2020$date<-as.factor(berry_diameter_BH_2020$date)


berry_diameter_BH_2020$treatment<-format(berry_diameter_BH_2020$treatment)
berry_diameter_BH_2020$treatment<-as.character(berry_diameter_BH_2020$treatment)

str(berry_diameter_BH_2020)


berry_diameter_BH_2020%>%
  group_by(treatment)%>%
  tally()

berry_diameter_BH_2020_plot<-berry_diameter_BH_2020%>%
  ggplot(aes(treatment,berry_diameter))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "2x baseline ET", "3x baseline ET"))+ 
  ylab("Berry diameter (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(6,16,2), limits = c (6,16)) +
  scale_x_discrete(labels = c('Baseline (60% ET)', '2x baseline ET', '3x baseline ET'))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(berry_diameter_BH_2020_plot, filename = "figures/berry_diameter_BH_2020_plot.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#####ANOVA berry diameter

berry_anova_2020<-berry_diameter_BH_2020

str(berry_anova_2020$berry_diameter) 

berry_anova_2020 %>%
  group_by(treatment)%>%
  tally()

berry_anova_2020<-berry_anova_2020%>%
  select (treatment, berry_diameter)

berry_anova_2020$treatment<- format(berry_anova_2020$treatment)
berry_anova_2020$treatment<- as.factor(berry_anova_2020$treatment)


berry_anova_2020 %>%
  group_by(treatment)%>%
  tally()

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(berry_anova_2020)
variables<-c("berry_diameter")

result_df_berry_diameter_2020<- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = berry_anova_2020)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), berry_anova_2020))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- berry_anova_2020 %>%
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
    ))
  # Decide which table to append based on the variable
  if (variable == "berry_diameter") {
    result_df_berry_diameter_2020 <- variable_result_df
  } 
  else {
    print(paste("Error processing", variable))
  }
}

str(result_df_berry_diameter_2020$p_values)
result_df_berry_diameter_2020$Mean_sem <- paste(round(result_df_berry_diameter_2020$berry_diameter, 2), "±", round(result_df_berry_diameter_2020$sem, 2),result_df_berry_diameter_2020$groups)


write.csv(result_df_berry_diameter_2020,"data_output/result_df_berry_diameter_2020.csv")



berry_anova<-berry_diameter_BH_2020

str(berry_anova$berry_diameter) 

berry_anova %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_anova$treatment)

berry_anova$treatment<- format(berry_anova$treatment)
berry_anova$treatment<- as.factor(berry_anova$treatment)

is.factor(berry_anova$treatment)

str(berry_anova)

ggplot(berry_anova, aes (treatment,berry_diameter, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_berry_diameter<- aov (berry_diameter~treatment, berry_anova)
summary (anova_berry_diameter)

TukeyHSD(aov(berry_diameter~treatment, berry_anova))

(test<- HSD.test(anova_berry_diameter , trt = "treatment", alpha =0.05))

str(test)

berry_anova_hist<-berry_anova %>%
  filter(treatment == 1)
hist(berry_anova_hist$berry_diameter)

berry_anova_hist<-berry_anova %>%
  filter(treatment == 2)
hist(berry_anova_hist$berry_diameter)

berry_anova_hist<-berry_anova %>%
  filter(treatment == 3)
hist(berry_anova_hist$berry_diameter)

library(xtable)
anova_berry_diameter<-xtable(anova_berry_diameter)

write.csv(anova_berry_diameter,"data_output/anova_berry_diameter_2020.csv")

tukey_yield_berry_diameter_harvest_2020<-as.data.frame(test$groups)
write.csv(tukey_yield_berry_diameter_harvest_2020,"data_output/tukey_yield_berry_diameter_harvest_2020.csv")




##### MCP 2020 for berry weight 2020#######

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


se <- function(x) sqrt(var(x)/length(x))

str(mcp_data_2020$Date_sample)


mcp_data_2020_grouped<-mcp_data_2020%>%
  group_by(Date_sampled, treatment)%>%
  tally()

berry_weight_2020_anova<-mcp_data_2020

str(berry_weight_2020_anova)
berry_weight_2020_anova$Date_sampled<-mdy(berry_weight_2020_anova$Date_sampled)

berry_weight_2020_anova<-berry_weight_2020_anova%>%
  filter(Date_sampled == "2020-09-08")%>%
  select (treatment,Berry_weight, Date_sampled)%>%
  mutate(berry_weight_berry = Berry_weight/60)


berry_weight_2020_anova$treatment<- format(berry_weight_2020_anova$treatment)
berry_weight_2020_anova$treatment<- as.factor(berry_weight_2020_anova$treatment)


berry_weight_2020_anova %>%
  group_by(treatment)%>%
  tally()


se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(berry_weight_2020_anova)
variables<-c("berry_weight_berry")

result_df_berry_weight_2020<- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = berry_weight_2020_anova)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), berry_weight_2020_anova))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- berry_weight_2020_anova %>%
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
    ))
  # Decide which table to append based on the variable
  if (variable == "berry_weight_berry") {
    result_df_berry_weight_2020 <- variable_result_df
  } 
  else {
    print(paste("Error processing", variable))
  }
}

str(result_df_berry_weight_2020$p_values)
result_df_berry_weight_2020$Mean_sem <- paste(round(result_df_berry_weight_2020$berry_weight_berry, 2), "±", round(result_df_berry_weight_2020$sem, 2),result_df_berry_weight_2020$groups)


write.csv(result_df_berry_weight_2020,"data_output/result_df_berry_weight_2020.csv")


ggplot(berry_weight_2020_anova, aes (treatment,Berry_weight, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

berry_w_2020_anova<- aov (Berry_weight~treatment, berry_weight_2020_anova)
summary (berry_w_2020_anova)

TukeyHSD(aov(Berry_weight~treatment, berry_weight_2020_anova))

(test<- HSD.test(berry_w_2020_anova , trt = "treatment", alpha =0.05))

str(test)



##### Yield 2021#####

harvest_sept_13_2021<-read.csv("=data/Blocks_harvest_sept_13_2021.csv", header=TRUE)%>%
  select(!initials)

harvest_sept_14_2021<-read.csv("=data/Blocks_harvest_sept_14_2021.csv", header=TRUE)%>%
  select(!initials)

harvest_sept_15_2021<-read.csv("data/Blocks_harvest_sept_15_2021.csv", header=TRUE)%>%
  select(!initials)

harvest_sept_16_2021<-read.csv("data/Blocks_harvest_sept_16_2021.csv", header=TRUE)%>%
  select(!initials)


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
  filter(!is.na(weight)) 

yield_2021_total<- yield_2021_total%>%
  mutate(block_sublock = paste(yield_2021_total$block,yield_2021_total$sub.block))%>%
  filter(!is.na(gr_cluster))



str(yield_2021_total)


yield_2021_total%>%
  group_by(treatment)%>%
  tally()

write.csv(yield_2021_total,"data_output/yield_2021.total.csv")

yield_kg_vine_2021<-yield_2021_total%>%
  ggplot(aes(treatment,Weight_kg))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+ 
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(0,16,3), limits = c (0,17)) +
  scale_x_discrete(labels = c('Baseline (60% ET)', '1.5x baseline ET', '2x baseline ET'))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))



ggsave(yield_kg_vine_2021, filename = "BH_2021/figures/yield_kg_vine_2021.pdf", device = cairo_pdf, 
       width = 8, height = 6)

yield_kg_vine_2021_block<-yield_2021_total%>%
  ggplot(aes(block,Weight_kg))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+ 
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Block") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(0,16,3), limits = c (0,17)) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))


ggsave(yield_kg_vine_2021_block, filename = "BH_2021/figures/yield_kg_vine_2021_block.pdf", device = cairo_pdf, 
       width = 9, height = 6)



yield_kg_vine_2021_subblock<-yield_2021_total%>%
  ggplot(aes(block_sublock,Weight_kg))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+ 
  ylab("Kg/vine") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Subblock") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(0,16,3), limits = c (0,17)) +
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(yield_kg_vine_2021_subblock, filename = "BH_2021/figures/yield_kg_vine_2021_subblock.pdf", device = cairo_pdf, 
       width = 18, height = 8)


yield_grams_cluster_2021<-yield_2021_total%>%
  ggplot(aes(treatment,gr_cluster))+
  scale_y_continuous (breaks=seq(25,250,50), limits = c (25,250)) +
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+ 
  ylab("grs/cluster") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_x_discrete(labels = c('Baseline (60% ET)', '1.5x baseline ET', '2x baseline ET'))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(yield_grams_cluster_2021, filename = "figures/yield_grams_cluster_2021.pdf", device = cairo_pdf, 
       width = 8, height = 6)

###anova yield 2021#####

yield_anova_2021<-yield_2021_total%>%
  select (treatment, Weight_kg, gr_cluster)

yield_anova_2021$treatment<- format(yield_anova_2021$treatment)
yield_anova_2021$treatment<- as.factor(yield_anova_2021$treatment)


yield_anova_2021 %>%
  group_by(treatment)%>%
  tally()

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(yield_anova_2021)
variables<-c("Weight_kg", "gr_cluster")

result_df_weight_kg_2021 <- NULL
result_df_gr_cluster_2021 <- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = yield_anova_2021)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), yield_anova_2021))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05, unbalanced = "TRUE"))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- yield_anova_2021 %>%
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
  if (variable == "Weight_kg") {
    # Append to result_df_weight_kg
    result_df_weight_kg_2021 <- variable_result_df
  } else if (variable == "gr_cluster") {
    # Append to result_df_gr_cluster
    result_df_gr_cluster_2021 <- variable_result_df
  }
  else {
    print(paste("Error processing", variable))
  }
}

result_df_weight_kg_2021$Mean_sem <- paste(round(result_df_weight_kg_2021$Weight_kg, 2), "±", round(result_df_weight_kg_2021$sem, 2),result_df_weight_kg_2021$groups)


result_df_gr_cluster_2021$Mean_sem <- paste(round(result_df_gr_cluster_2021$gr_cluster, 2), "±", round(result_df_gr_cluster_2021$sem, 2),result_df_gr_cluster_2021$groups)

write.csv(result_df_weight_kg_2021,"BH_2021/data_output/result_df_weight_kg_2021_anova.csv")
write.csv(result_df_gr_cluster_2021,"BH_2021/data_output/result_df_gr_cluster_2021_anova.csv")

#weight_kg


yield_anova<-yield_2021_total

str(yield_anova$Weight_kg) 

yield_anova %>%
  group_by(treatment)%>%
  tally()

is.factor(yield_anova$treatment)

yield_anova$treatment<- format(yield_anova$treatment)
yield_anova$treatment<- as.factor(yield_anova$treatment)

is.factor(yield_anova$treatment)

str(yield_anova)

ggplot(yield_anova, aes (treatment,Weight_kg, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_Weight_kg<- aov (Weight_kg~treatment, yield_anova)
summary (anova_Weight_kg)

str(test)

TukeyHSD(aov(Weight_kg~treatment, yield_anova))

(test<- HSD.test(anova_weight_kg , trt = "treatment", alpha =0.05, unbalanced = TRUE))


str(test)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 1)
hist(yield_anova_hist$Weight_kg)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 2)
hist(yield_anova_hist$Weight_kg)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 3)
hist(yield_anova_hist$Weight_kg)

library(xtable)
anova_Weight_kg<-xtable(anova_Weight_kg)

write.csv(anova_Weight_kg,"BH_2021/data_output/anova_Weight_kg_2021.csv")

tukey_yield_weight_kgs_harvest_2021<-as.data.frame(test$groups)
write.csv(tukey_yield_weight_kgs_harvest_2021,"BH_2021/data_output/tukey_yield_weight_kgs_harvest_2021.csv")



#####gr/cluster

yield_anova<-yield_2021_total

str(yield_anova$Weight_kg) 

yield_anova %>%
  group_by(treatment)%>%
  tally()

is.factor(yield_anova$treatment)

yield_anova$treatment<- format(yield_anova$treatment)
yield_anova$treatment<- as.factor(yield_anova$treatment)

is.factor(yield_anova$treatment)

str(yield_anova)

ggplot(yield_anova, aes (treatment,gr_cluster, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_gr_cluster<- aov (gr_cluster~treatment, yield_anova)
summary (anova_gr_cluster)

TukeyHSD(aov(gr_cluster~treatment, yield_anova))

(test<- HSD.test(anova_gr_cluster , trt = "treatment", alpha =0.05, unbalanced = TRUE))


str(test)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 1)
hist(yield_anova_hist$gr_cluster)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 2)
hist(yield_anova_hist$gr_cluster)

yield_anova_hist<-yield_anova %>%
  filter(treatment == 3)
hist(yield_anova_hist$gr_cluster)

library(xtable)
anova_gr_cluster<-xtable(anova_gr_cluster)

write.csv(anova_gr_cluster,"BH_2021/data_output/anova_gr_cluster_2021.csv")

tukey_yield_gr_cluster_harvest_2021<-as.data.frame(test$groups)
write.csv(tukey_yield_gr_cluster_harvest_2021,"BH_2021/data_output/tukey_yield_gr_cluster_harvest_2021.csv")

####Berry diameter 2021####


berry_diameter_BH_2021<- read.csv("data/berry_diameter_2021_harvest_blocks.csv", header =TRUE)
str(berry_diameter_BH_2021$treatment)  

berry_diameter_BH_2021<-berry_diameter_BH_2021%>%
  select(!treatment)


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


berry_diameter_BH_2021$date<-format(berry_diameter_BH_2021$date)
berry_diameter_BH_2021$date<-as.factor(berry_diameter_BH_2021$date)


berry_diameter_BH_2021$treatment<-format(berry_diameter_BH_2021$treatment)
berry_diameter_BH_2021$treatment<-as.character(berry_diameter_BH_2021$treatment)

str(berry_diameter_BH_2021)


berry_diameter_BH_2021%>%
  group_by(treatment)%>%
  tally()

berry_diameter_BH_2021_plot<-berry_diameter_BH_2021%>%
  ggplot(aes(treatment,berry_diameter))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+ 
  ylab("Berry diameter (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(6,16,2), limits = c (6,16)) +
  scale_x_discrete(labels = c('Baseline (60% ET)', '1.5x baseline ET', '2x baseline ET'))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(berry_diameter_BH_2021_plot, filename = "BH_2021/figures/berry_diameter_BH_2021_plot.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#####ANOVA berry diameter
berry_anova_2021<-berry_diameter_BH_2021

str(berry_anova_2021$berry_diameter) 

berry_anova_2021 %>%
  group_by(treatment)%>%
  tally()

berry_anova_2021<-berry_anova_2021%>%
  select (treatment, berry_diameter)

berry_anova_2021$treatment<- format(berry_anova_2021$treatment)
berry_anova_2021$treatment<- as.factor(berry_anova_2021$treatment)


berry_anova_2021 %>%
  group_by(treatment)%>%
  tally()

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(berry_anova_2021)
variables<-c("berry_diameter")

result_df_berry_diameter_2021<- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = berry_anova_2021)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), berry_anova_2021))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- berry_anova_2021 %>%
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
  if (variable == "berry_diameter") {
    result_df_berry_diameter_2021 <- variable_result_df
  } 
  else {
    print(paste("Error processing", variable))
  }
}

result_df_berry_diameter_2021$Mean_sem <- paste(round(result_df_berry_diameter_2021$berry_diameter, 2), "±", round(result_df_berry_diameter_2021$sem, 2),result_df_berry_diameter_2021$groups)


write.csv(result_df_berry_diameter_2021,"BH_2021/data_output/result_df_berry_diameter_2021.csv")



berry_anova<-berry_diameter_BH_2021

str(berry_anova$berry_diameter) 

berry_anova %>%
  group_by(treatment)%>%
  tally()

is.factor(berry_anova$treatment)

berry_anova$treatment<- format(berry_anova$treatment)
berry_anova$treatment<- as.factor(berry_anova$treatment)

is.factor(berry_anova$treatment)

str(berry_anova)

ggplot(berry_anova, aes (treatment,berry_diameter, group =treatment)) +
  geom_boxplot(aes(fill = treatment, color =treatment)) +
  geom_point()

anova_berry_diameter<- aov (berry_diameter~treatment, berry_anova)
summary (anova_berry_diameter)

TukeyHSD(aov(berry_diameter~treatment, berry_anova))

(test<- HSD.test(anova_berry_diameter , trt = "treatment", alpha =0.05))

str(test)  

berry_anova_hist<-berry_anova %>%
  filter(treatment == 1)
hist(berry_anova_hist$berry_diameter)

berry_anova_hist<-berry_anova %>%
  filter(treatment == 2)
hist(berry_anova_hist$berry_diameter)

berry_anova_hist<-berry_anova %>%
  filter(treatment == 3)
hist(berry_anova_hist$berry_diameter)

library(xtable)
anova_berry_diameter_2021<-xtable(anova_berry_diameter)

write.csv(anova_berry_diameter_2021,"BH_2021/data_output/anova_berry_diameter_2021.csv")

tukey_yield_berry_diameter_harvest_2021<-as.data.frame(test$groups)
write.csv(tukey_yield_berry_diameter_harvest_2021,"BH_2021/data_output/tukey_yield_berry_diameter_harvest_2021.csv")


####Berry weight 2021####


  berry_weight_BH_2021<- read.csv("data/BH2021_sept08_berry_weight_harvest.csv", header =TRUE)
str(berry_weight_BH_2021)  



berry_weight_BH_2021<- berry_weight_BH_2021%>%
  mutate(berry_weight_per_berry = berry_weight_g/berry_num)


berry_weight_BH_2021$date<-as.factor(berry_weight_BH_2021$date)
berry_weight_BH_2021$treatment<-as.character(berry_weight_BH_2021$treatment)

str(berry_weight_BH_2021)


berry_weight_per_berry_mean_2021<- berry_weight_BH_2021%>%
  group_by(treatment)%>%
  summarise(mean(berry_weight_per_berry))



berry_weight_BH_2021_plot<-berry_weight_BH_2021%>%
  ggplot(aes(treatment,berry_weight_per_berry))+
  geom_boxplot(alpha =0.7, aes(fill = treatment))+
  geom_point(alpha =0.9,position =pd2, aes(color = treatment, group =treatment, shape =treatment), size =2) +
  scale_shape_manual(values = c(16,15,17),name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET" )) +
  theme_classic()+
  scale_fill_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+
  scale_colour_viridis_d(direction = -1, begin = 0.05, end = 0.93, name = "Treatment", labels = c("Baseline (60% ET)", "1.5x baseline ET", "2x baseline ET"))+ 
  ylab("Berry diameter (mm)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold", family = "serif")) +
  xlab("Treatment") +
  theme(axis.title.y = element_text(size=21, family = "serif")) +
  theme(axis.title.x = element_text(size=21, family = "serif")) +
  theme(legend.key.size = unit (0.5, "cm")) +
  theme(legend.key.width = unit(0.2,"cm"))+
  theme(legend.justification = "center")+
  theme(legend.title.align = 0.5) +
  scale_y_continuous (breaks=seq(0.8,1.4,0.2), limits = c (0.8,1.4)) +
  scale_x_discrete(labels = c('Baseline (60% ET)', '1.5x baseline ET', '2x baseline ET'))+
  theme(axis.text.x = element_text(size =18))+
  theme(axis.text.y = element_text(size =18))

ggsave(berry_weight_BH_2021_plot, filename = "figures/berry_weight_BH_2021_plot.pdf", device = cairo_pdf, 
       width = 8, height = 6)


#####ANOVA berry diameter
berry_anova_2021<-berry_weight_BH_2021

str(berry_anova_2021$berry_diameter) 

berry_anova_2021 %>%
  group_by(treatment)%>%
  tally()

berry_anova_2021<-berry_anova_2021%>%
  select (treatment, berry_weight_per_berry)

berry_anova_2021$treatment<- format(berry_anova_2021$treatment)
berry_anova_2021$treatment<- as.factor(berry_anova_2021$treatment)


berry_anova_2021 %>%
  group_by(treatment)%>%
  tally()

se <- function(x) {
  s <- sd(x)  # Calculate standard deviation
  n <- length(x)  # Sample size
  sqrt(s^2 / n)  # Calculate standard error
}

str(berry_anova_2021)
variables<-c("berry_weight_per_berry")

result_df_berry_weight_2021<- NULL

for (variable in variables) {
  anova_result <- aov(as.formula(paste(variable, "~ treatment")), data = berry_anova_2021)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_value_df<-df <- data.frame(p_values = c(p_value,p_value,p_value))
  tukey<-TukeyHSD(aov(as.formula(paste(variable, "~ treatment")), berry_anova_2021))
  (test<- HSD.test(anova_result, trt = "treatment", alpha =0.05))
  tukey_yield<-as.data.frame(test$groups)
  mean_sd_harvest<-as.data.frame(test$means) 
  mean_sd_harvest$Treatment<-c(1, 2, 3)
  se_df <- berry_anova_2021 %>%
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
  if (variable == "berry_weight_per_berry") {
    result_df_berry_weight_2021 <- variable_result_df
  } 
  else {
    print(paste("Error processing", variable))
  }
}

result_df_berry_weight_2021$Mean_sem <- paste(round(result_df_berry_weight_2021$berry_weight_per_berry, 2), "±", round(result_df_berry_weight_2021$sem, 2),result_df_berry_weight_2021$groups)


write.csv(result_df_berry_weight_2021,"data_output/result_df_berry_weight_2021.csv")
