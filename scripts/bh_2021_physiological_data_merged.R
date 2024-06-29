#### All physilogical measurements of BH 2021 all together in one mega-spreadsheet ###

library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(xtable)
library(agricolae)
library(gtools)
library(dplyr)
library(plyr)
library(readr)

#### WATER POTENTIALS BH 2021####

water_potentials_bh_2021<- list.files(path = "data/water_potentials/", pattern = ".*csv", full.names = TRUE)%>%
  lapply(read_csv)%>%
  bind_rows

water_potentials_bh_2021_complete<- water_potentials_bh_2021 %>%
  mutate( BLOCK = case_when(
    block_id == "B1R1_NW" ~ "B1R1",
    block_id == "B1R1_NE" ~ "B1R1",
    block_id == "B1R1_SE" ~ "B1R1",
    block_id == "B1R1_SW" ~ "B1R1",
    block_id == "B1R3_NW" ~ "B1R3",
    block_id == "B1R3_NE" ~ "B1R3",
    block_id == "B1R3_SW" ~ "B1R3",
    block_id == "B1R3_SE" ~ "B1R3",
    block_id == "B1R4_NW" ~ "B1R4",
    block_id == "B1R4_NE" ~ "B1R4",
    block_id == "B1R4_SW" ~ "B1R4",
    block_id == "B1R4_SE" ~ "B1R4",
    block_id == "B2R1_NW" ~ "B2R1",
    block_id == "B2R1_NE" ~ "B2R1",
    block_id == "B2R1_SW" ~ "B2R1",
    block_id == "B2R1_SE" ~ "B2R1",
    block_id == "B2R2_NW" ~ "B2R2",
    block_id == "B2R2_NE" ~ "B2R2",
    block_id == "B2R2_SW" ~ "B2R2",
    block_id == "B2R2_SE" ~ "B2R2",
    block_id == "B2R3_NW" ~ "B2R3",
    block_id == "B2R3_NE" ~ "B2R3",
    block_id == "B2R3_SW" ~ "B2R3",
    block_id == "B2R3_SE" ~ "B2R3",
    block_id == "B3R1_NW" ~ "B3R1",
    block_id == "B3R1_NE" ~ "B3R1",
    block_id == "B3R1_SW" ~ "B3R1",
    block_id == "B3R1_SE" ~ "B3R1",
    block_id == "B3R2_NW" ~ "B3R2",
    block_id == "B3R2_NE" ~ "B3R2",
    block_id == "B3R2_SW" ~ "B3R2",
    block_id == "B3R2_SE" ~ "B3R2",
    block_id == "B3R3_NW" ~ "B3R3",
    block_id == "B3R3_NE" ~ "B3R3",
    block_id == "B3R3_SW" ~ "B3R3",
    block_id == "B3R3_SE" ~ "B3R3", )) %>%
  mutate( SUB_BLOCK = case_when(
    block_id == "B1R1_NW" ~ "_NW",
    block_id == "B1R1_NE" ~ "_NE",
    block_id == "B1R1_SE" ~ "_SE",
    block_id == "B1R1_SW" ~ "_SW",
    block_id == "B1R3_NW" ~ "_NW",
    block_id == "B1R3_NE" ~ "_NE",
    block_id == "B1R3_SW" ~ "_SW",
    block_id == "B1R3_SE" ~ "_SE",
    block_id == "B1R4_NW" ~ "_NW",
    block_id == "B1R4_NE" ~ "_NE",
    block_id == "B1R4_SW" ~ "_SW",
    block_id == "B1R4_SE" ~ "_SE",
    block_id == "B2R1_NW" ~ "_NW",
    block_id == "B2R1_NE" ~ "_NE",
    block_id == "B2R1_SW" ~ "_SW",
    block_id == "B2R1_SE" ~ "_SE",
    block_id == "B2R2_NW" ~ "_NW",
    block_id == "B2R2_NE" ~ "_NE",
    block_id == "B2R2_SW" ~ "_SW",
    block_id == "B2R2_SE" ~ "_SE",
    block_id == "B2R3_NW" ~ "_NW",
    block_id == "B2R3_NE" ~ "_NE",
    block_id == "B2R3_SW" ~ "_SW",
    block_id == "B2R3_SE" ~ "_SE",
    block_id == "B3R1_NW" ~ "_NW",
    block_id == "B3R1_NE" ~ "_NE",
    block_id == "B3R1_SW" ~ "_SW",
    block_id == "B3R1_SE" ~ "_SE",
    block_id == "B3R2_NW" ~ "_NW",
    block_id == "B3R2_NE" ~ "_NE",
    block_id == "B3R2_SW" ~ "_SW",
    block_id == "B3R2_SE" ~ "_SE",
    block_id == "B3R3_NW" ~ "_NW",
    block_id == "B3R3_NE" ~ "_NE",
    block_id == "B3R3_SW" ~ "_SW",
    block_id == "B3R3_SE" ~ "_SE", )) %>%
  mutate(LEAF = "_1")%>%
  mutate(VINE = vine) %>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar)) %>%
  select(!(vine))


water_potentials_bh_2021_complete<- water_potentials_bh_2021_complete %>%
  mutate( VINE = case_when(
    VINE == 5.1 ~ "_5.10",
    VINE == 5.3 ~ "_5.3",
    VINE == 5.4 ~ "_5.4",
    VINE == 5.5 ~ "_5.5",
    VINE == 5.6 ~ "_5.6",
    VINE == 5.7 ~ "_5.7",
    VINE == 5.8 ~ "_5.8",
    VINE == 5.9 ~ "_5.9",
    VINE == 5.10 ~ "_5.10",
    VINE == 5.11 ~ "_5.11",
    VINE == 5.12 ~ "_5.12",
    VINE == 5.13 ~ "_5.13",
    VINE == 5.14 ~ "_5.14",
    VINE == 5.15 ~ "_5.15",
  ))


str(water_potentials_bh_2021_complete)

write.csv(water_potentials_bh_2021_complete,"data_output/water_potentials_bh_2021_complete.csv")


#### LI-COR MIDDAYS MEASUREMENTS BH 2021 ####

data_frames_middays <- list.files(path = "data/clean_csv_files_licor_all_dates/middays/", pattern = ".*csv", full.names = TRUE) %>%
  lapply(function(file) {
    df <- read.csv(file, sep = ",", skip = 13)
    if ("Qmax_d" %in% colnames(df)) {
      df$Qmax_d <- as.character(df$Qmax_d)
    }
    if ("Fv.Fm" %in% colnames(df)) {
      df$Fv.Fm <- as.character(df$Fv.Fm)
    }
    if ("Qmax" %in% colnames(df)) {
      df$Qmax <- as.character(df$Qmax)
    }
    if ("PhiPS2" %in% colnames(df)) {
      df$PhiPS2 <- as.character(df$PhiPS2)
    }
    if ("Fv..Fm." %in% colnames(df)) {
      df$Fv..Fm. <- as.character(df$Fv..Fm.)
    }
    if ("NPQ" %in% colnames(df)) {
      df$NPQ <- as.character(df$NPQ)
    }
    if ("Fo." %in% colnames(df)) {
      df$Fo. <- as.character(df$Fo.)
    }
    if ("qP" %in% colnames(df)) {
      df$qP <- as.character(df$qP)
    }
    if ("qN" %in% colnames(df)) {
      df$qN <- as.character(df$qN)
    }
    if ("qP_Fo" %in% colnames(df)) {
      df$qP_Fo <- as.character(df$qP_Fo)
    }
    if ("qL" %in% colnames(df)) {
      df$qL <- as.character(df$qL)
    }
    if ("qN_Fo" %in% colnames(df)) {
      df$qN_Fo <- as.character(df$qN_Fo)
    }
    if ("X1.qL" %in% colnames(df)) {
      df$X1.qL <- as.character(df$X1.qL)
    }
    if ("alt..Fo." %in% colnames(df)) {
      df$alt..Fo. <- as.character(df$alt..Fo.)
    }
    return(df)
  })

# Combine the pre-processed data frames
licor_files_middays_2021 <- bind_rows(data_frames_middays)

licor_files_middays_2021_complete<-licor_files_middays_2021%>%
  filter(!is.na(obs))

licor_files_middays_2021_complete$date<-ymd_hms(licor_files_middays_2021_complete$date,tz = "UTC")

str(licor_files_middays_2021_complete$date)

licor_files_middays_2021_complete$date<-as.Date(licor_files_middays_2021_complete$date, format = "%Y-%m-%d")

str(licor_files_middays_2021_complete$date)

water_potentials_bh_2021_middays<- water_potentials_bh_2021_complete%>%
  filter(round == 4)

str(water_potentials_bh_2021_middays$date)


licor_files_middays_2021_complete_data <- licor_files_middays_2021_complete%>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo, ETR) %>%
  mutate( treatment = case_when(
    BLOCK == "B1R1" ~ 1,
    BLOCK == "B1R3" ~ 1,
    BLOCK == "B1R4" ~ 1,
    BLOCK == "B2R1" ~ 2,
    BLOCK == "B2R2" ~ 2,
    BLOCK == "B2R3" ~ 2,
    BLOCK == "B3R1" ~ 3,
    BLOCK == "B3R2" ~ 3,
    BLOCK == "B3R3" ~ 3,
  ))

#COMBINED WATER POTENTIALS AND LICOR MIDDAYS MEASUREMENTS 

water_potentials_bh_2021_middays$date<-mdy(water_potentials_bh_2021_middays$date)

str(water_potentials_bh_2021_middays$date)

common_col_names <- intersect(names(licor_files_middays_2021_complete_data), names(water_potentials_bh_2021_middays))

data_physiology_complete_BH_2021_middays <- merge(licor_files_middays_2021_complete_data, water_potentials_bh_2021_middays, by= common_col_names, all = TRUE)


str(data_physiology_complete_BH_2021_middays)

write.csv(data_physiology_complete_BH_2021_middays,"data_output/data_physiology_complete_BH_2021_middays.csv")

#LICOR PREDAWNS BH 2021


data_frames <- list.files(path = "data/clean_csv_files_licor_all_dates/predawn/", pattern = ".*csv", full.names = TRUE) %>%
  lapply(function(file) {
    df <- read.csv(file, sep = ",", skip = 13)
    # Ensure "Fo." column is character type
    if ("Fo." %in% colnames(df)) {
      df$Fo. <- as.character(df$Fo.)
    }
    if ("qN" %in% colnames(df)) {
      df$qN <- as.character(df$qN)
    }
    return(df)
  })

# Combine the pre-processed data frames
cleaned_licor_files_predawn_2021 <- bind_rows(data_frames)

licor_files_predawn_2021_complete<-cleaned_licor_files_predawn_2021%>%
  filter(!is.na(obs))

licor_files_predawn_2021_complete$date<-ymd_hms(licor_files_predawn_2021_complete$date,tz = "UTC")

str(licor_files_predawn_2021_complete$date)

licor_files_predawn_2021_complete$date<-as.Date(licor_files_predawn_2021_complete$date, format = "%Y-%m-%d")

str(licor_files_predawn_2021_complete$date)

water_potentials_bh_2021_predawn<- water_potentials_bh_2021_complete%>%
  filter(round == 1)

str(water_potentials_bh_2021_predawn$date) 

licor_files_predawn_2021_complete_data <- licor_files_predawn_2021_complete%>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo, ETR) %>%
  mutate( treatment = case_when(
    BLOCK == "B1R1" ~ 1,
    BLOCK == "B1R3" ~ 1,
    BLOCK == "B1R4" ~ 1,
    BLOCK == "B2R1" ~ 2,
    BLOCK == "B2R2" ~ 2,
    BLOCK == "B2R3" ~ 2,
    BLOCK == "B3R1" ~ 3,
    BLOCK == "B3R2" ~ 3,
    BLOCK == "B3R3" ~ 3,
  ))

#COMBINED PREDAWN LICOR AND WATER POTENTIALS BH 2021 MEASUREMENTS

water_potentials_bh_2021_predawn$date<-mdy(water_potentials_bh_2021_predawn$date)

str(water_potentials_bh_2021_predawn$date)

common_col_names <- intersect(names(licor_files_middays_2021_complete_data), names(water_potentials_bh_2021_middays))

data_physiology_complete_BH_2021_predawn<- merge(licor_files_predawn_2021_complete_data, water_potentials_bh_2021_predawn, by= common_col_names, all = TRUE)


str(data_physiology_complete_BH_2021_predawn)

write.csv(data_physiology_complete_BH_2021_predawn,"data_output/data_physiology_complete_BH_2021_predawn.csv")
