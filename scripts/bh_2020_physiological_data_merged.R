#### All physilogical measurements of BH 2020 all together in one mega-spreadsheet ###

library(grDevices)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(xtable)
library(agricolae)
library(gtools)

#### WATER POTENTIALS BH 2020 ####

water_potentials_bh_2020_0527_to_0720<- read.csv("data/water_potentials_2020_may27_to_jul_20.csv", header = TRUE)


water_potentials_bh_2020_0527_to_0720<- water_potentials_bh_2020_0527_to_0720 %>%
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
  select(!(vine))%>%
  filter(!is.na(leaf_wp_MPa))

water_potentials_bh_2020_0819<- read.csv("data/water_potentials_20200819.csv", header = TRUE)

water_potentials_bh_2020_0819 <-water_potentials_bh_2020_0819 %>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar)) %>%
  filter(!is.na(leaf_wp_MPa))

water_potentials_bh_2020_0816 <- read.csv("data/water_potentials_20200816.csv", header = TRUE)

water_potentials_bh_2020_0816<- water_potentials_bh_2020_0816%>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar)) %>%
  filter(!is.na(leaf_wp_MPa))

water_potentials_bh_2020_0813 <- read.csv("data/water_potentials_20200813.csv", header =TRUE)

water_potentials_bh_2020_0813<- water_potentials_bh_2020_0813%>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar))%>%
  filter(!is.na(leaf_wp_MPa))

water_potentials_bh_2020_0810 <- read.csv("data/water_potentials_20200810.csv", header = TRUE)

water_potentials_bh_2020_0810<- water_potentials_bh_2020_0810%>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar)) %>%
  filter(!is.na(leaf_wp_MPa))

water_potentials_bh_2020_0826<- read.csv("data/water_potentials_20200826.csv", header = TRUE)

water_potentials_bh_2020_0826<- water_potentials_bh_2020_0826%>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar))%>%
  filter(!is.na(leaf_wp_MPa))

water_potentials_bh_2020_0907 <- read.csv("data/water_potentials_20200907.csv", header =  TRUE)

water_potentials_bh_2020_0907<- water_potentials_bh_2020_0907%>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar))%>%
  filter(!is.na(leaf_wp_MPa))


water_potentials_bh_2020_0915 <- read.csv("data/water_potentials_20200915.csv", header = TRUE)

water_potentials_bh_2020_0915<- water_potentials_bh_2020_0915%>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar))%>%
  filter(!is.na(leaf_wp_MPa))



###COMBINED WATER POTENTIALS BH 2020 #### 

water_potentials_BH_2020 <- rbind(water_potentials_bh_2020_0527_to_0720, water_potentials_bh_2020_0810, water_potentials_bh_2020_0813, water_potentials_bh_2020_0816, water_potentials_bh_2020_0819, water_potentials_bh_2020_0826, water_potentials_bh_2020_0907, water_potentials_bh_2020_0915)

str(water_potentials_BH_2020)



water_potentials_BH_2020<- water_potentials_BH_2020 %>%
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


#### LI-COR MEASUREMENTS BH 2020 ####

####DIURNAL MAY 27 ####

#####round 1

table1_r1_may_27 <- read.csv("data/2020-05-27-0459_BH_diurnal1_0.csv",header = TRUE, sep =",", skip = 13)
table2_r1_may_27 <- read.csv("data/2020-05-27-0504_BH_1_1.csv",header = TRUE, sep =",", skip = 13)
table3_r1_may_27 <- read.csv("data/2020-05-27-0504_BH_1_2.csv",header = TRUE, sep =",", skip = 13)
table4_r1_may_27 <- read.csv("data/2020-05-27-0504_BH_1_0.csv",header = TRUE, sep =",", skip = 13)

table1_r1_may_27<-table1_r1_may_27 [-1, ] 

str(table1_r1_may_27)

table1_r1_may_27_sel <- table1_r1_may_27  %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =1) %>%
  mutate(LEAF = "_1") %>%
  mutate( BLOCK = case_when(
    newDef_2 == "B1R1SE" ~ "B1R1",
    newDef_2 == "B1R1SW" ~ "B1R1",
    newDef_2 == "B1R3SW" ~ "B1R3",
    newDef_2 == "B1R3SE" ~ "B1R3",
    newDef_2 == "B1R4NW" ~ "B1R4",
    newDef_2 == "B1R4NE" ~ "B1R4",
    newDef_2 == "B2R1SE" ~ "B2R1",
    newDef_2 == "B2R2SW" ~ "B2R2",
    newDef_2 == "B2R2SE" ~ "B2R2",
    newDef_2 == "B2R3NE" ~ "B2R3",
    newDef_2 == "B3R1NW" ~ "B3R1",
    newDef_2 == "B3R1NE" ~ "B3R1",
    newDef_2 == "B3R2SE" ~ "B3R2",
    newDef_2 == "B3R3SE" ~ "B3R3", )) %>%
  mutate( SUB_BLOCK = case_when(
    newDef_2 == "B1R1NW" ~ "_NW",
    newDef_2 == "B1R1NE" ~ "_NE",
    newDef_2 == "B1R1SE" ~ "_SE",
    newDef_2 == "B1R1SW" ~ "_SW",
    newDef_2 == "B1R3NW" ~ "_NW",
    newDef_2 == "B1R3NE" ~ "_NE",
    newDef_2 == "B1R3SW" ~ "_SW",
    newDef_2 == "B1R3SE" ~ "_SE",
    newDef_2 == "B1R4NW" ~ "_NW",
    newDef_2 == "B1R4NE" ~ "_NE",
    newDef_2 == "B1R4SW" ~ "_SW",
    newDef_2 == "B1R4SE" ~ "_SE",
    newDef_2 == "B2R1NW" ~ "_NW",
    newDef_2 == "B2R1NE" ~ "_NE",
    newDef_2 == "B2R1SW" ~ "_SW",
    newDef_2 == "B2R1SE" ~ "_SE",
    newDef_2 == "B2R2NW" ~ "_NW",
    newDef_2 == "B2R2NE" ~ "_NE",
    newDef_2 == "B2R2SW" ~ "_SW",
    newDef_2 == "B2R2SE" ~ "_SE",
    newDef_2 == "B2R3NW" ~ "_NW",
    newDef_2 == "B2R3NE" ~ "_NE",
    newDef_2 == "B2R3SW" ~ "_SW",
    newDef_2 == "B2R3SE" ~ "_SE",
    newDef_2 == "B3R1NW" ~ "_NW",
    newDef_2 == "B3R1NE" ~ "_NE",
    newDef_2 == "B3R1SW" ~ "_SW",
    newDef_2 == "B3R1SE" ~ "_SE",
    newDef_2 == "B3R2NW" ~ "_NW",
    newDef_2 == "B3R2NE" ~ "_NE",
    newDef_2 == "B3R2SW" ~ "_SW",
    newDef_2 == "B3R2SE" ~ "_SE",
    newDef_2 == "B3R3NW" ~ "_NW",
    newDef_2 == "B3R3NE" ~ "_NE",
    newDef_2 == "B3R3SW" ~ "_SW",
    newDef_2 == "B3R3SE" ~ "_SE", )) %>%
  mutate(VINE = case_when(
    newDef_2 == "B1R1SE" ~ "_5.10",
    newDef_2 == "B1R1SW" ~ "_5.8",
    newDef_2 == "B1R3SW" ~ "_5.4",
    newDef_2 == "B1R3SE" ~ "_5.6",
    newDef_2 == "B1R4NW" ~ "_5.14",
    newDef_2 == "B1R4NE" ~ "_5.14",
    newDef_2 == "B2R1SE" ~ "_5.7",
    newDef_2 == "B2R2SW" ~ "_5.13",
    newDef_2 == "B2R2SE" ~ "_5.3",
    newDef_2 == "B2R3NE" ~ "_5.9",
    newDef_2 == "B3R1NW" ~ "_5.6",
    newDef_2 == "B3R1NE" ~ "_5.5",
    newDef_2 == "B3R2SE" ~ "_5.7",
    newDef_2 == "B3R3SE" ~ "_5.3", 
  )) %>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)


table1_r1_may_27_sel <- table1_r1_may_27_sel  %>%
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

table2_r1_may_27_sel <- table2_r1_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(LEAF = "_1") %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_may_27_sel<-table2_r1_may_27_sel [-1, ] %>%
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

table3_r1_may_27_sel <- table3_r1_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(LEAF = "_1") %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r1_may_27_sel <- table3_r1_may_27_sel [-1, ]   %>%
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


table4_r1_may_27_sel <- table4_r1_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(LEAF = "_1") %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table4_r1_may_27_sel <- table4_r1_may_27_sel [-1, ]   %>%
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

table_comb_r1_may_27 <-rbind(table1_r1_may_27_sel,table2_r1_may_27_sel, table3_r1_may_27_sel, table4_r1_may_27_sel)

table_comb_r1_may_27%>%
  group_by(treatment, round)%>%
  tally()


### round 2

table1_r2_may_27 <- read.csv("data/2020-05-27-0826_BH455-diurnal2.csv",header = TRUE, sep =",", skip = 13 )
table2_r2_may_27 <- read.csv("data/2020-05-27-0832_BH_diurnal2.csv",header = TRUE, sep =",", skip = 13 )
table3_r2_may_27 <- read.csv("data/2020-05-27-1038_diurnal2_second.csv",header = TRUE, sep =",", skip = 13 )


table1_r2_may_27_sel <- table1_r2_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_may_27_sel <- table1_r2_may_27_sel[-1, ]   %>%
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


table2_r2_may_27_sel <- table2_r2_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_may_27_sel <- table2_r2_may_27_sel[-1, ]   %>%
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


table3_r2_may_27_sel <- table3_r2_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r2_may_27_sel <- table3_r2_may_27_sel[-1, ]   %>%
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

table_comb_r2_may_27 <-rbind(table1_r2_may_27_sel,table2_r2_may_27_sel, table3_r2_may_27_sel)

table_comb_r2_may_27%>%
  group_by(treatment, round)%>%
  tally()

####round 3

table1_r3_may_27 <- read.csv("data/2020-05-27-1101_bh455-diurnal3.csv",header = TRUE, sep =",", skip = 13 )
table2_r3_may_27 <- read.csv("data/2020-05-27-1122_BH_diurnal3.csv",header = TRUE, sep =",", skip = 13 )


table1_r3_may_27_sel <- table1_r3_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_may_27_sel <- table1_r3_may_27_sel[-1, ]   %>%
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


table2_r3_may_27_sel <- table2_r3_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_may_27_sel <- table2_r3_may_27_sel[-1, ]   %>%
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


table_comb_r3_may_27 <-rbind(table1_r3_may_27_sel,table2_r3_may_27_sel)

table_comb_r3_may_27%>%
  group_by(treatment, round)%>%
  tally()

####ROUND 4 

table1_r4_may_27 <- read.csv("data/2020-05-27-1350_BH_diurnal4.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_may_27 <- read.csv("data/2020-05-27-1403_logdata_BH455_diurnal4.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_may_27_sel <- table1_r4_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_may_27_sel <- table1_r4_may_27_sel[-1, ]   %>%
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


table2_r4_may_27_sel <- table2_r4_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_may_27_sel <- table2_r4_may_27_sel[-1, ]   %>%
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


table_comb_r4_may_27 <-rbind(table1_r4_may_27_sel,table2_r4_may_27_sel)

table_comb_r4_may_27%>%
  group_by(treatment, round)%>%
  tally()


####round 5


table1_r5_may_27 <- read.csv("data/2020-05-27-1719_bh455_diurnal5.csv",header = TRUE, sep =",", skip = 13 )
table2_r5_may_27 <- read.csv("data/2020-05-27-1720_BH_diurnal5.csv",header = TRUE, sep =",", skip = 13 )


table1_r5_may_27_sel <- table1_r5_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r5_may_27_sel <- table1_r5_may_27_sel[-1, ]   %>%
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


table2_r5_may_27_sel <- table2_r5_may_27 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "05-27-2020", day = 148, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r5_may_27_sel <- table2_r5_may_27_sel[-1, ]   %>%
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


table_comb_r5_may_27 <-rbind(table1_r5_may_27_sel,table2_r5_may_27_sel)

table_comb_r5_may_27%>%
  group_by(treatment, round, day, date, BLOCK)%>%
  tally()

names(table_comb_r1_may_27) <- names(table_comb_r2_may_27) 
names(table_comb_r2_may_27) <- names(table_comb_r3_may_27)
names(table_comb_r3_may_27) <- names(table_comb_r4_may_27)
names(table_comb_r4_may_27) <- names(table_comb_r5_may_27)

identical(names(table_comb_r1_may_27), names (table_comb_r2_may_27))
identical(names(table_comb_r2_may_27), names (table_comb_r3_may_27))
identical(names(table_comb_r3_may_27), names (table_comb_r4_may_27))
identical(names(table_comb_r4_may_27), names (table_comb_r5_may_27))


table_diurnals_may_27_2020 <-rbind(table_comb_r1_may_27, table_comb_r2_may_27, table_comb_r3_may_27, table_comb_r4_may_27,table_comb_r5_may_27)


#### Midday Jun 05 ####

####Midday 


table1_r4_jun_05 <- read.csv("data/2020-06-05-1156_bh455_midday_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jun_05  <- read.csv("data/2020-06-05-1200_BH455_midday_ejf.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_jun_05_sel <- table1_r4_jun_05  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-05-2020", day = 158, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jun_05_sel <-table1_r4_jun_05_sel[-1, ]   %>%
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


table2_r4_jun_05_sel <- table2_r4_jun_05 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-05-2020", day = 158, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jun_05_sel <-table2_r4_jun_05_sel [-1, ]   %>%
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


table_comb_r4_jun_05 <-rbind(table1_r4_jun_05_sel, table2_r4_jun_05_sel)

table_comb_r4_jun_05%>%
  group_by(treatment, round,date, day)%>%
  tally()

#### Midday Jun 11 ##### 

table1_r4_jun_11 <- read.csv("data/2020-06-11-1137_logdata_BH455_midday_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jun_11  <- read.csv("data/2020-06-11-1149_BH455_midday_EJF.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_jun_11_sel <- table1_r4_jun_11  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-11-2020", day = 163, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jun_11_sel <-table1_r4_jun_11_sel[-1, ]   %>%
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


table2_r4_jun_11_sel <- table2_r4_jun_11 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-11-2020", day = 163, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jun_11_sel <-table2_r4_jun_11_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r4_jun_11 <-rbind(table1_r4_jun_11_sel, table2_r4_jun_11_sel)

table_comb_r4_jun_11%>%
  group_by(treatment, round, date, day)%>%
  tally()


####Midday jun 19 ####


table1_r4_jun_19 <- read.csv("data/2020-06-19-1148_BH455_midday_AJM.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jun_19  <- read.csv("data/2020-06-19-1153_BH455_midday_EJF.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_jun_19_sel <- table1_r4_jun_19  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-19-2020", day = 171, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jun_19_sel <-table1_r4_jun_19_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_jun_19_sel <- table2_r4_jun_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-19-2020", day = 171, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jun_19_sel <-table2_r4_jun_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r4_jun_19 <-rbind(table1_r4_jun_19_sel, table2_r4_jun_19_sel)

table_comb_r4_jun_19%>%
  group_by(treatment, round, date, day)%>%
  tally()

#### midday Jun 25 ####



table1_r4_jun_25 <- read.csv("data/2020-06-25-1126_logdata_BH455_midday_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jun_25 <- read.csv("data/2020-06-25-1133_logdata_BH455_midday_ejf.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_jun_25_sel <- table1_r4_jun_25  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-25-2020", day = 176, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jun_25_sel <-table1_r4_jun_25_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_jun_25_sel <- table2_r4_jun_25 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "06-25-2020", day = 176, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jun_25_sel <-table2_r4_jun_25_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r4_jun_25 <-rbind(table1_r4_jun_25_sel, table2_r4_jun_25_sel)

table_comb_r4_jun_25%>%
  group_by(treatment, round, date, day)%>%
  tally()

#### Midday jul 02 ####

table1_r4_jul_02 <- read.csv("data/2020-07-02-1138_BH455_midday_ejf.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jul_02 <- read.csv("data/2020-07-02-1142_BH455_midday_aim.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_jul_02_sel <- table1_r4_jul_02  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-02-2020", day = 184, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jul_02_sel <-table1_r4_jul_02_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_jul_02_sel <- table2_r4_jul_02 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-02-2020", day = 184, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jul_02_sel <-table2_r4_jul_02_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r4_jul_02 <-rbind(table1_r4_jul_02_sel, table2_r4_jul_02_sel)

table_comb_r4_jul_02%>%
  group_by(treatment, round, date, day)%>%
  tally()

##### Diurnal Jul 10 #####

#### Round 1 

table1_r1_jul_10 <- read.csv("data/2020-07-10-0523_BH455_round1_EJF.csv",header = TRUE, sep =",", skip = 13 )
table2_r1_jul_10 <- read.csv("data/2020-07-10-0531_BH455_round1_ajm.csv",header = TRUE, sep =",", skip = 13 )


table1_r1_jul_10_sel <- table1_r1_jul_10  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_jul_10_sel <-table1_r1_jul_10_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_jul_10_sel <- table2_r1_jul_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_jul_10_sel <-table2_r1_jul_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r1_jul_10 <-rbind(table1_r1_jul_10_sel, table2_r1_jul_10_sel)

table_comb_r1_jul_10%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 2 


table1_r2_jul_10 <- read.csv("data/2020-07-10-0754_BH455_round2_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r2_jul_10 <- read.csv("data/2020-07-10-0755_BH455_round2_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table3_r2_jul_10 <- read.csv("data/2020-07-10-0901_AJM_Roun2cont.csv",header = TRUE, sep =",", skip = 13 )


table1_r2_jul_10_sel <- table1_r2_jul_10  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_jul_10_sel <-table1_r2_jul_10_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r2_jul_10_sel <- table2_r2_jul_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_jul_10_sel <-table2_r2_jul_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table3_r2_jul_10_sel <- table3_r2_jul_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r2_jul_10_sel <-table3_r2_jul_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r2_jul_10 <-rbind(table1_r2_jul_10_sel, table2_r2_jul_10_sel, table3_r2_jul_10_sel)

table_comb_r2_jul_10%>%
  group_by(treatment, round, date, day)%>%
  tally()

#### Round 3 


table1_r3_jul_10 <- read.csv("data/2020-07-10-1031_BH455_round3_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table2_r3_jul_10 <- read.csv("data/2020-07-10-1035_ajm_roun3.csv",header = TRUE, sep =",", skip = 13 )



table1_r3_jul_10_sel <- table1_r3_jul_10  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_jul_10_sel <-table1_r3_jul_10_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r3_jul_10_sel <- table2_r3_jul_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_jul_10_sel <-table2_r3_jul_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r3_jul_10 <-rbind(table1_r3_jul_10_sel, table2_r3_jul_10_sel)

table_comb_r3_jul_10%>%
  group_by(treatment, round, date, day)%>%
  tally()

####round 4 


table1_r4_jul_10 <- read.csv("data/2020-07-10-1243_round4_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jul_10 <- read.csv("data/2020-07-10-1248_ajm_round4.csv",header = TRUE, sep =",", skip = 13 )



table1_r4_jul_10_sel <- table1_r4_jul_10  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jul_10_sel <-table1_r4_jul_10_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_jul_10_sel <- table2_r4_jul_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jul_10_sel <-table2_r4_jul_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r4_jul_10 <-rbind(table1_r4_jul_10_sel, table2_r4_jul_10_sel)

table_comb_r4_jul_10%>%
  group_by(treatment, round, date, day)%>%
  tally()


#####Round 5


table1_r5_jul_10 <- read.csv("data/2020-07-10-1605_ajm_round5.csv",header = TRUE, sep =",", skip = 13 )
table2_r5_jul_10 <- read.csv("data/2020-07-10-1631_BH455_round5_EJF1.csv",header = TRUE, sep =",", skip = 13 )



table1_r5_jul_10_sel <- table1_r5_jul_10  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r5_jul_10_sel <-table1_r5_jul_10_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r5_jul_10_sel <- table2_r5_jul_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-10-2020", day = 192, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r5_jul_10_sel <-table2_r5_jul_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r5_jul_10 <-rbind(table1_r5_jul_10_sel, table2_r5_jul_10_sel)

table_comb_r5_jul_10%>%
  group_by(treatment, round, date, day)%>%
  tally()


table_diurnals_jul_10_2020 <-rbind(table_comb_r1_jul_10, table_comb_r2_jul_10, table_comb_r3_jul_10, table_comb_r4_jul_10,table_comb_r5_jul_10)


##### Diurnal July 12 #####

#### Round 1

table1_r1_jul_12 <- read.csv("data/2020-07-12-0438_BH455_diurnal_round1_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table2_r1_jul_12 <- read.csv("data/2020-07-12-0438_diurnal_round1_ajm.csv",header = TRUE, sep =",", skip = 13 )



table1_r1_jul_12_sel <- table1_r1_jul_12  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_jul_12_sel <-table1_r1_jul_12_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_jul_12_sel <- table2_r1_jul_12 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_jul_12_sel <-table2_r1_jul_12_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r1_jul_12 <-rbind(table1_r1_jul_12_sel, table2_r1_jul_12_sel)

table_comb_r1_jul_12%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 2 

table1_r2_jul_12 <- read.csv("data/2020-07-12-0753_BH455_round2_ejf.csv",header = TRUE, sep =",", skip = 13 )
table2_r2_jul_12 <- read.csv("data/2020-07-12-0754_diurnal_round2_ajm.csv",header = TRUE, sep =",", skip = 13 )



table1_r2_jul_12_sel <- table1_r2_jul_12  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_jul_12_sel <-table1_r2_jul_12_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r2_jul_12_sel <- table2_r2_jul_12 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_jul_12_sel <-table2_r2_jul_12_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r2_jul_12 <-rbind(table1_r2_jul_12_sel, table2_r2_jul_12_sel)

table_comb_r2_jul_12%>%
  group_by(treatment, round, date, day)%>%
  tally()


####Round 3 



table1_r3_jul_12 <- read.csv("data/2020-07-12-1019_BH455_diurnal_round3_AJM.csv",header = TRUE, sep =",", skip = 13 )
table2_r3_jul_12 <- read.csv("data/2020-07-12-1020_BH455_diurnal_round3_EJF1.csv",header = TRUE, sep =",", skip = 13 )



table1_r3_jul_12_sel <- table1_r3_jul_12  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_jul_12_sel <-table1_r3_jul_12_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r3_jul_12_sel <- table2_r3_jul_12 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_jul_12_sel <-table2_r3_jul_12_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r3_jul_12 <-rbind(table1_r3_jul_12_sel, table2_r3_jul_12_sel)

table_comb_r3_jul_12%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 4 



table1_r4_jul_12 <- read.csv("data/2020-07-12-1241_BH455_round4_ejm.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jul_12 <- read.csv("data/2020-07-12-1242_diurnal_round4_ajm.csv",header = TRUE, sep =",", skip = 13 )



table1_r4_jul_12_sel <- table1_r4_jul_12  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jul_12_sel <-table1_r4_jul_12_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_jul_12_sel <- table2_r4_jul_12 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jul_12_sel <-table2_r4_jul_12_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r4_jul_12 <-rbind(table1_r4_jul_12_sel, table2_r4_jul_12_sel)

table_comb_r4_jul_12%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 5 

table1_r5_jul_12 <- read.csv("data/2020-07-12-1551_BH455_diurnal_round5_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table2_r5_jul_12 <- read.csv("data/2020-07-12-1559_diurnal_round5_ajm.csv",header = TRUE, sep =",", skip = 13 )



table1_r5_jul_12_sel <- table1_r5_jul_12  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r5_jul_12_sel <-table1_r5_jul_12_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r5_jul_12_sel <- table2_r5_jul_12 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-12-2020", day = 194, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r5_jul_12_sel <-table2_r5_jul_12_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r5_jul_12 <-rbind(table1_r5_jul_12_sel, table2_r5_jul_12_sel)

table_comb_r5_jul_12%>%
  group_by(treatment, round, date, day)%>%
  tally()

table_diurnals_jul_12_2020 <-rbind(table_comb_r1_jul_12, table_comb_r2_jul_12, table_comb_r3_jul_12, table_comb_r4_jul_12,table_comb_r5_jul_12)



##### Diurnal Jul 20 ######

####Round 1 

table1_r1_jul_20 <- read.csv("data/2020-07-20-0422_diurnal_round1_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r1_jul_20 <- read.csv("data/2020-07-20-0422_diurnal_round1_ejf2.csv",header = TRUE, sep =",", skip = 13 )



table1_r1_jul_20_sel <- table1_r1_jul_20  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_jul_20_sel <-table1_r1_jul_20_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_jul_20_sel <- table2_r1_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_jul_20_sel <-table2_r1_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r1_jul_20 <-rbind(table1_r1_jul_20_sel, table2_r1_jul_20_sel)

table_comb_r1_jul_20%>%
  group_by(treatment, round, date, day)%>%
  tally()


####Round 2 

table1_r2_jul_20 <- read.csv("data/2020-07-20-0743_BH455_diurnal_round2_efj2.csv",header = TRUE, sep =",", skip = 13 )
table2_r2_jul_20 <- read.csv("data/2020-07-20-0743_BH455_diurnal_round2_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table3_r2_jul_20 <- read.csv("data/2020-07-20-0745_BH455_diurnal_round2_ajm.csv",header = TRUE, sep =",", skip = 13 )


table1_r2_jul_20_sel <- table1_r2_jul_20  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_jul_20_sel <-table1_r2_jul_20_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r2_jul_20_sel <- table2_r2_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_jul_20_sel <-table2_r2_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table3_r2_jul_20_sel <- table3_r2_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r2_jul_20_sel <-table3_r2_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table_comb_r2_jul_20 <-rbind(table1_r2_jul_20_sel, table2_r2_jul_20_sel, table3_r2_jul_20_sel)

table_comb_r2_jul_20%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 3


table1_r3_jul_20 <- read.csv("data/2020-07-20-1017_BH455_diurnal_round3_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r3_jul_20 <- read.csv("data/2020-07-20-1019_diurnal_round3_AJM1.csv",header = TRUE, sep =",", skip = 13 )
table3_r3_jul_20 <- read.csv("data/2020-07-20-1021_diurnal_Round3_EJF2.csv",header = TRUE, sep =",", skip = 13 )


table1_r3_jul_20_sel <- table1_r3_jul_20  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_jul_20_sel <-table1_r3_jul_20_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r3_jul_20_sel <- table2_r3_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_jul_20_sel <-table2_r3_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table3_r3_jul_20_sel <- table3_r3_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r3_jul_20_sel <-table3_r3_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table_comb_r3_jul_20 <-rbind(table1_r3_jul_20_sel, table2_r3_jul_20_sel, table3_r3_jul_20_sel)

table_comb_r3_jul_20%>%
  group_by(treatment, round, date, day)%>%
  tally()


####Round 4

table1_r4_jul_20 <- read.csv("data/2020-07-20-1237_BH455_diurnal_round4_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_jul_20 <- read.csv("data/2020-07-20-1241_BH455_diurnal_round4_ejf4.csv",header = TRUE, sep =",", skip = 13 )
table3_r4_jul_20 <- read.csv("data/2020-07-20-1250_diurnal_round4_AJM1.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_jul_20_sel <- table1_r4_jul_20  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_jul_20_sel <-table1_r4_jul_20_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_jul_20_sel <- table2_r4_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_jul_20_sel <-table2_r4_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table3_r4_jul_20_sel <- table3_r4_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r4_jul_20_sel <-table3_r4_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table_comb_r4_jul_20 <-rbind(table1_r4_jul_20_sel, table2_r4_jul_20_sel, table3_r4_jul_20_sel)

table_comb_r4_jul_20%>%
  group_by(treatment, round, date, day)%>%
  tally()

####round 5

table1_r5_jul_20 <- read.csv("data/2020-07-20-1549_BH455_diurnal_round5_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r5_jul_20 <- read.csv("data/2020-07-20-1551_BH455_diurnal_round5_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table3_r5_jul_20 <- read.csv("data/2020-07-20-1558_BH455_diurnal_round5_ejf2.csv",header = TRUE, sep =",", skip = 13 )


table1_r5_jul_20_sel <- table1_r5_jul_20  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r5_jul_20_sel <-table1_r5_jul_20_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r5_jul_20_sel <- table2_r5_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r5_jul_20_sel <-table2_r5_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table3_r5_jul_20_sel <- table3_r5_jul_20 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-20-2020", day = 202, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r5_jul_20_sel <-table3_r5_jul_20_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table_comb_r5_jul_20 <-rbind(table1_r5_jul_20_sel, table2_r5_jul_20_sel, table3_r5_jul_20_sel)

table_comb_r5_jul_20%>%
  group_by(treatment, round, date, day)%>%
  tally()

table_diurnals_jul_20_2020 <-rbind(table_comb_r1_jul_20, table_comb_r2_jul_20, table_comb_r3_jul_20, table_comb_r4_jul_20,table_comb_r5_jul_20)


####Midday Aug 10 #####

table1_r4_aug_10 <- read.csv("data/2020-08-10-1207_BH455_midday_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_aug_10 <- read.csv("data/2020-08-10-1207_BH455_midday_ejf1_0.csv",header = TRUE, sep =",", skip = 13 )
table3_r4_aug_10 <- read.csv("data/2020-08-10-1229_BH455_ejf2_midday_log2.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_aug_10_sel <- table1_r4_aug_10  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-10-2020", day = 223, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_aug_10_sel <-table1_r4_aug_10_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_aug_10_sel <- table2_r4_aug_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-10-2020", day = 223, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_aug_10_sel <-table2_r4_aug_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table3_r4_aug_10_sel <- table3_r4_aug_10 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-10-2020", day = 223, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r4_aug_10_sel <-table3_r4_aug_10_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table_comb_r4_aug_10 <-rbind(table1_r4_aug_10_sel, table2_r4_aug_10_sel, table3_r4_aug_10_sel)

table_comb_r4_aug_10%>%
  group_by(treatment, round, date, day)%>%
  tally()


#### Diurnal aug 13 ####

####Round 1 

table1_r1_aug_13 <- read.csv("data/2020-08-13-0501_BH455_diurnal_round1_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r1_aug_13 <- read.csv("data/2020-08-13-0501_BH455_diurnal_round1_ejf.csv",header = TRUE, sep =",", skip = 13 )


table1_r1_aug_13_sel <- table1_r1_aug_13  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_aug_13_sel <-table1_r1_aug_13_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_aug_13_sel <- table2_r1_aug_13 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_aug_13_sel <-table2_r1_aug_13_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r1_aug_13 <-rbind(table1_r1_aug_13_sel, table2_r1_aug_13_sel)

table_comb_r1_aug_13%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 2

table1_r2_aug_13 <- read.csv("data/2020-08-13-0818_BH455_diurnal_round2_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r2_aug_13 <- read.csv("data/2020-08-13-0818_BH455_diurnal_round2_ejf1.csv",header = TRUE, sep =",", skip = 13 )


table1_r2_aug_13_sel <- table1_r2_aug_13  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_aug_13_sel <-table1_r2_aug_13_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r2_aug_13_sel <- table2_r2_aug_13 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_aug_13_sel <-table2_r2_aug_13_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r2_aug_13 <-rbind(table1_r2_aug_13_sel, table2_r2_aug_13_sel)

table_comb_r2_aug_13%>%
  group_by(treatment, round, date, day)%>%
  tally()

#### Round 3 

table1_r3_aug_13 <- read.csv("data/2020-08-13-1030_BH455_diurnal_round3_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r3_aug_13 <- read.csv("data/2020-08-13-1032_BH455_diurnal_round3_ajm.csv",header = TRUE, sep =",", skip = 13 )


table1_r3_aug_13_sel <- table1_r3_aug_13  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_aug_13_sel <-table1_r3_aug_13_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r3_aug_13_sel <- table2_r3_aug_13 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_aug_13_sel <-table2_r3_aug_13_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r3_aug_13 <-rbind(table1_r3_aug_13_sel, table2_r3_aug_13_sel)

table_comb_r3_aug_13%>%
  group_by(treatment, round, date, day)%>%
  tally()


####Round 4 


table1_r4_aug_13 <- read.csv("data/2020-08-13-1241_BH455_diurnal_round4_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_aug_13 <- read.csv("data/2020-08-13-1242_BH455_diurnal_round4_ajm.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_aug_13_sel <- table1_r4_aug_13  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_aug_13_sel <-table1_r4_aug_13_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_aug_13_sel <- table2_r4_aug_13 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_aug_13_sel <-table2_r4_aug_13_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r4_aug_13 <-rbind(table1_r4_aug_13_sel, table2_r4_aug_13_sel)

table_comb_r4_aug_13%>%
  group_by(treatment, round, date, day)%>%
  tally()



#####Round 5 


table1_r5_aug_13 <- read.csv("data/2020-08-13-1558_BH455_diurnal_round5_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r5_aug_13 <- read.csv("data/2020-08-13-1605_BH455_diurnal_round5_ajm.csv",header = TRUE, sep =",", skip = 13 )


table1_r5_aug_13_sel <- table1_r5_aug_13  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r5_aug_13_sel <-table1_r5_aug_13_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r5_aug_13_sel <- table2_r5_aug_13 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-13-2020", day = 226, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r5_aug_13_sel <-table2_r5_aug_13_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r5_aug_13 <-rbind(table1_r5_aug_13_sel, table2_r5_aug_13_sel)

table_comb_r5_aug_13%>%
  group_by(treatment, round, date, day)%>%
  tally()


table_diurnals_aug_13_2020 <-rbind(table_comb_r1_aug_13, table_comb_r2_aug_13, table_comb_r3_aug_13, table_comb_r4_aug_13,table_comb_r5_aug_13)


#####Failed diurnal aug 16 #####

####Round 1 


table1_r1_aug_16 <- read.csv("data/2020-08-16-0455_BH455_diurnal_round1_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r1_aug_16 <- read.csv("data/2020-08-16-0455_BH455_diurnal_round1_ejf1.csv",header = TRUE, sep =",", skip = 13 )


table1_r1_aug_16_sel <- table1_r1_aug_16  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-16-2020", day = 229, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_aug_16_sel <-table1_r1_aug_16_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_aug_16_sel <- table2_r1_aug_16 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-16-2020", day = 229, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_aug_16_sel <-table2_r1_aug_16_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r1_aug_16 <-rbind(table1_r1_aug_16_sel, table2_r1_aug_16_sel)

table_comb_r1_aug_16%>%
  group_by(treatment, round, date, day)%>%
  tally()

#####Diurnal Aug 19 #####

###Round 1 

table1_r1_aug_19 <- read.csv("data/2020-08-19-0501_BH455_diurnal_round1_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table2_r1_aug_19 <- read.csv("data/2020-08-19-0517_BH455_diurnal_round1_ajm.csv",header = TRUE, sep =",", skip = 13 )


table1_r1_aug_19_sel <- table1_r1_aug_19  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_aug_19_sel <-table1_r1_aug_19_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_aug_19_sel <- table2_r1_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_aug_19_sel <-table2_r1_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r1_aug_19 <-rbind(table1_r1_aug_19_sel, table2_r1_aug_19_sel)

table_comb_r1_aug_19%>%
  group_by(treatment, round, date, day)%>%
  tally()

#### Round 2 

table1_r2_aug_19 <- read.csv("data/2020-08-19-0816_BH455_round2_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table2_r2_aug_19 <- read.csv("data/2020-08-19-0852_BH455_diurnal_round2_ajm.csv",header = TRUE, sep =",", skip = 13 )
table3_r2_aug_19 <- read.csv("data/2020-08-19-1047_R2.csv",header = TRUE, sep =",", skip = 13 )


table1_r2_aug_19_sel <- table1_r2_aug_19  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_aug_19_sel <-table1_r2_aug_19_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r2_aug_19_sel <- table2_r2_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_aug_19_sel <-table2_r2_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table3_r2_aug_19_sel <- table3_r2_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r2_aug_19_sel <-table3_r2_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r2_aug_19 <-rbind(table1_r2_aug_19_sel, table2_r2_aug_19_sel, table3_r2_aug_19_sel)

table_comb_r2_aug_19%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 3 

table1_r3_aug_19 <- read.csv("data/2020-08-19-1036_BH45_diurnal_round3_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r3_aug_19 <- read.csv("data/2020-08-19-1041_BH455_round3_ejf1.csv",header = TRUE, sep =",", skip = 13 )
table3_r3_aug_19 <- read.csv("data/2020-08-19-1236_R3.csv",header = TRUE, sep =",", skip = 13 )


table1_r3_aug_19_sel <- table1_r3_aug_19  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_aug_19_sel <-table1_r3_aug_19_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r3_aug_19_sel <- table2_r3_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_aug_19_sel <-table2_r3_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table3_r3_aug_19_sel <- table3_r3_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r3_aug_19_sel <-table3_r3_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r3_aug_19 <-rbind(table1_r3_aug_19_sel, table2_r3_aug_19_sel, table3_r3_aug_19_sel)

table_comb_r3_aug_19%>%
  group_by(treatment, round, date, day)%>%
  tally()

####Round 4 

table1_r4_aug_19 <- read.csv("data/2020-08-19-1252_BH455_diurnal_round4_ajm.csv",header = TRUE, sep =",", skip = 13 )
table2_r4_aug_19 <- read.csv("data/2020-08-19-1255_BH455_round4_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table3_r4_aug_19 <- read.csv("data/2020-08-19-1456_R4.csv",header = TRUE, sep =",", skip = 13 )


table1_r4_aug_19_sel <- table1_r4_aug_19  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_aug_19_sel <-table1_r4_aug_19_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_aug_19_sel <- table2_r4_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_aug_19_sel <-table2_r4_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table3_r4_aug_19_sel <- table3_r4_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r4_aug_19_sel <-table3_r4_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r4_aug_19 <-rbind(table1_r4_aug_19_sel, table2_r4_aug_19_sel, table3_r4_aug_19_sel)

table_comb_r4_aug_19%>%
  group_by(treatment, round, date, day)%>%
  tally()


##### Round 5


table1_r5_aug_19 <- read.csv("data/2020-08-19-1559_BH455_round5_EJF1.csv",header = TRUE, sep =",", skip = 13 )
table2_r5_aug_19 <- read.csv("data/2020-08-19-1614_BH455_diurnal_round5.csv",header = TRUE, sep =",", skip = 13 )
table3_r5_aug_19 <- read.csv("data/2020-08-19-1800_R5.csv",header = TRUE, sep =",", skip = 13 )


table1_r5_aug_19_sel <- table1_r5_aug_19  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r5_aug_19_sel <-table1_r5_aug_19_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r5_aug_19_sel <- table2_r5_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r5_aug_19_sel <-table2_r5_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))



table3_r5_aug_19_sel <- table3_r5_aug_19 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "08-19-2020", day = 232, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r5_aug_19_sel <-table3_r5_aug_19_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))

table_comb_r5_aug_19 <-rbind(table1_r5_aug_19_sel, table2_r5_aug_19_sel, table3_r5_aug_19_sel)

table_comb_r5_aug_19%>%
  group_by(treatment, round, date, day)%>%
  tally()


table_diurnals_aug_19_2020 <-rbind(table_comb_r1_aug_19, table_comb_r2_aug_19, table_comb_r3_aug_19, table_comb_r4_aug_19,table_comb_r5_aug_19)

#### Diurnal aug 26   #####

#### round 1

table1_r1_aug_26 <- read.csv("data/2020-08-26-0448_BH455_diurnal_round1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_aug_26<- read.csv("data/2020-08-26-0651_BH455_diurnal_round1.csv", header= TRUE, sep =",", skip = 13)


table1_r1_aug_26_sel <- table1_r1_aug_26  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_aug_26_sel <-table1_r1_aug_26_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_aug_26_sel <- table2_r1_aug_26 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_aug_26_sel <-table2_r1_aug_26_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))




table_comb_r1_aug_26 <-rbind(table1_r1_aug_26_sel, table2_r1_aug_26_sel)

table_comb_r1_aug_26%>%
  group_by(treatment, round, date, day)%>%
  tally()


####aUG 26 round 2####

table1_r2_aug_26 <- read.csv("data/2020-08-26-0751_BH455_diurnal_round2_ejf1.csv", header = TRUE, sep =",", skip = 13)
table2_r2_aug_26<- read.csv("data/2020-08-26-0752_BH455_diurnal_round2.csv", header= TRUE, sep =",", skip = 13)


table1_r2_aug_26_sel <- table1_r2_aug_26  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_aug_26_sel <-table1_r2_aug_26_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r2_aug_26_sel <- table2_r2_aug_26 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_aug_26_sel <-table2_r2_aug_26_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))




table_comb_r2_aug_26 <-rbind(table1_r2_aug_26_sel, table2_r2_aug_26_sel)

table_comb_r2_aug_26%>%
  group_by(treatment, round, date, day)%>%
  tally()

####aUG 26 round 3####

table1_r3_aug_26 <- read.csv("data/2020-08-26-1022_BH455_diurnal_round3_EJF1.csv", header = TRUE, sep =",", skip = 13)
table2_r3_aug_26<- read.csv("data/2020-08-26-1119_BH455_diurnal_round3_loaner_second.csv", header= TRUE, sep =",", skip = 13)
table3_r3_aug_26<- read.csv("data/2020-08-26-1014_BH455_diurnal_round3_loaner.csv", header= TRUE, sep =",", skip = 61)


table1_r3_aug_26_sel <- table1_r3_aug_26  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_aug_26_sel <-table1_r3_aug_26_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r3_aug_26_sel <- table2_r3_aug_26 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_aug_26_sel <-table2_r3_aug_26_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table3_r3_aug_26_sel <- table3_r3_aug_26 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_r3_aug_26_sel <-table3_r3_aug_26_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r3_aug_26 <-rbind(table1_r3_aug_26_sel, table2_r3_aug_26_sel, table3_r3_aug_26_sel)

table_comb_r3_aug_26%>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()


###aUG 26 rOUND 4#####


table1_r4_aug_26 <- read.csv("data/2020-08-26-1255_BH455_diurnal_round4_ajm.csv", header = TRUE, sep =",", skip = 13)
table2_r4_aug_26<- read.csv("data/2020-08-26-1305_BH455_diurnal_round4_EJF1.csv", header= TRUE, sep =",", skip = 13)


table1_r4_aug_26_sel <- table1_r4_aug_26  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r4_aug_26_sel <-table1_r4_aug_26_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r4_aug_26_sel <- table2_r4_aug_26 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r4_aug_26_sel <-table2_r4_aug_26_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r4_aug_26 <-rbind(table1_r4_aug_26_sel, table2_r4_aug_26_sel)

table_comb_r4_aug_26%>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()

###aUG 26 rOUND 5#####


table1_r5_aug_26 <- read.csv("data/2020-08-26-1550_BH455_diurnal_round5_ajm.csv", header = TRUE, sep =",", skip = 13)
table2_r5_aug_26<- read.csv("data/2020-08-26-1553_BH455_diurnal_round5_ejf1.csv", header= TRUE, sep =",", skip = 13)


table1_r5_aug_26_sel <- table1_r5_aug_26  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r5_aug_26_sel <-table1_r5_aug_26_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r5_aug_26_sel <- table2_r5_aug_26 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date ="08-26-2020", day = 239, year =2020, round =5)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r5_aug_26_sel <-table2_r5_aug_26_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r5_aug_26 <-rbind(table1_r5_aug_26_sel, table2_r5_aug_26_sel)

table_comb_r5_aug_26%>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()


table_diurnals_aug_26_2020 <-rbind(table_comb_r1_aug_26, table_comb_r2_aug_26, table_comb_r3_aug_26, table_comb_r4_aug_26,table_comb_r5_aug_26)



##### Diurnal sept 7 #####
####Round 1

table1_r1_sept_7 <- read.csv("data/2020-09-07-0516_BH455_diurnal_round1_loaner.csv", header = TRUE, sep =",", skip = 13)
table2_r1_sept_7<- read.csv("data/2020-09-07-0517_BH455_diurnal_round1_ejf1.csv", header= TRUE, sep =",", skip = 13)


table1_r1_sept_7_sel <- table1_r1_sept_7   %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-07-2020", day = 251, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r1_sept_7_sel <-table1_r1_sept_7_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r1_sept_7_sel <- table2_r1_sept_7  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-07-2020", day = 251, year =2020, round =1)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r1_sept_7_sel <-table2_r1_sept_7_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r1_sept_7  <-rbind(table1_r1_sept_7_sel, table2_r1_sept_7_sel)

table_comb_r1_sept_7 %>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()


####Sept 07 round 2####

table1_r2_sept_7 <- read.csv("data/2020-09-07-0809_BH455_diurnal_round2_loaner.csv", header = TRUE, sep =",", skip = 13)
table2_r2_sept_7<- read.csv("data/2020-09-07-0804_BH455_diurnal_round2_ejf1.csv", header= TRUE, sep =",", skip = 13)


table1_r2_sept_7_sel <- table1_r2_sept_7   %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-07-2020", day = 251, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r2_sept_7_sel <-table1_r2_sept_7_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r2_sept_7_sel <- table2_r2_sept_7  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-07-2020", day = 251, year =2020, round =2)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r2_sept_7_sel <-table2_r2_sept_7_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r2_sept_7  <-rbind(table1_r2_sept_7_sel, table2_r2_sept_7_sel)

table_comb_r2_sept_7 %>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()


####Sept 07 round 3####

table1_r3_sept_7 <- read.csv("data/2020-09-07-1023_BH455_diurnal_round3_loaner.csv", header = TRUE, sep =",", skip = 13)
table2_r3_sept_7<- read.csv("data/2020-09-07-1034_BH455_diurnal_round3_ejf1.csv", header= TRUE, sep =",", skip = 13)


table1_r3_sept_7_sel <- table1_r3_sept_7  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-07-2020", day = 251, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_r3_sept_7_sel <-table1_r3_sept_7_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_r3_sept_7_sel <- table2_r3_sept_7  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-07-2020", day = 251, year =2020, round =3)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_r3_sept_7_sel <-table2_r3_sept_7_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_r3_sept_7  <-rbind(table1_r3_sept_7_sel, table2_r3_sept_7_sel)

table_comb_r3_sept_7 %>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()


table_comb_r1_sept_7<- table_comb_r1_sept_7%>%
  mutate(Fv..Fm.= case_when(
    Fv..Fm. == "#DIV/0!" ~ NA,)) %>%
  mutate(PhiPS2= case_when(
    PhiPS2 == "#DIV/0!" ~ NA,))%>%
  mutate(ETR= case_when(
    ETR == "#DIV/0!" ~ NA,))%>%
  mutate(NPQ= case_when(
    NPQ == "#DIV/0!" ~ NA,)) %>%
  mutate(qP= case_when(
    qP == "#DIV/0!" ~ NA,))


table_diurnals_sept_7_2020 <-rbind(table_comb_r1_sept_7, table_comb_r2_sept_7, table_comb_r3_sept_7)

####Sep 15 midday ####

table1_midday_sept_15 <- read.csv("data/2020-09-15-1229_logdata.csv", header = TRUE, sep =",", skip = 13)
table2_midday_sept_15<- read.csv("data/2020-09-15-1356_BH455_midday_loaner.csv", header= TRUE, sep =",", skip = 13)


table1_midday_sept_15_sel <- table1_midday_sept_15  %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-15-2020", day = 259, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_midday_sept_15_sel <-table1_midday_sept_15_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_midday_sept_15_sel <- table2_midday_sept_15 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "09-15-2020", day = 259, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_midday_sept_15_sel <-table2_midday_sept_15_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_midday_sept_15 <-rbind(table1_midday_sept_15_sel, table2_midday_sept_15_sel)

table_comb_midday_sept_15%>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()

table_midday_sept_15_2020 <-rbind(table1_midday_sept_15_sel, table2_midday_sept_15_sel)

table_midday_sept_15_2020%>%
  group_by(treatment, round, date, day)%>%
  tally()


names(table_diurnals_sept_7_2020) <- names(table_midday_sept_15_2020) 

identical(names(table_diurnals_sept_7_2020), names (table_midday_sept_15_2020))


#####All middays and diurnals together from LI-COR files #####

licor_physiology_BH_2020<-rbind (table_diurnals_may_27_2020,table_comb_r4_jun_05, table_comb_r4_jun_11, table_comb_r4_jun_19, table_comb_r4_jun_25, table_comb_r4_jul_02, table_diurnals_jul_10_2020, table_diurnals_jul_12_2020, table_diurnals_jul_20_2020, table_comb_r4_aug_10, table_diurnals_aug_13_2020, table_comb_r1_aug_16, table_diurnals_aug_19_2020, table_diurnals_aug_26_2020,table_diurnals_sept_7_2020,table_midday_sept_15_2020)

str(licor_physiology_BH_2020)


licor_physiology_BH_2020$A<-format(licor_physiology_BH_2020$A)
licor_physiology_BH_2020$A<-as.numeric(licor_physiology_BH_2020$A)

str(licor_physiology_BH_2020$A)


licor_physiological_measurements_BH_2020<- licor_physiology_BH_2020 %>% filter(!(BLOCK == "B1R4" & hhmmss == "15:10:46"& date == "09-07-2020"))%>%
  filter(!(BLOCK == "B1R4" & hhmmss == "15:14:28" & date == "09-07-2020")) %>%
  filter(!(BLOCK == "B1R4" & hhmmss == "15:17:48" & date =="09-07-2020")) %>%
  filter(!(BLOCK == "B1R4" & hhmmss == "15:20:59" & date == "09-07-2020")) %>%
  filter(!(BLOCK == "B1R4" & hhmmss == "15:25:10" & date == "09-07-2020")) %>%
  filter(!(BLOCK == "B1R4" & hhmmss == "15:27:31" & date == "09-07-2020")) %>%
  filter(!(BLOCK == "B2R3" & hhmmss == "15:35:22" & date == "09-07-2020")) %>%
  filter(!(BLOCK == "B2R3" & hhmmss == "15:37:37" & date == "09-07-2020")) %>%
  filter(!(BLOCK == "B2R3" & hhmmss == "15:39:25" & date == "09-07-2020"))




#### WATER POTENTIALS and LICOR MEASUREMENTS ALL TOEGETHER #####

licor_physiological_measurements_BH_2020$date<-format(licor_physiological_measurements_BH_2020$date)
licor_physiological_measurements_BH_2020$date<- mdy(licor_physiological_measurements_BH_2020$date)


str(licor_physiological_measurements_BH_2020$date)

water_potentials_BH_2020$date<-format(water_potentials_BH_2020$date)
water_potentials_BH_2020$date<- mdy(water_potentials_BH_2020$date)

str(water_potentials_BH_2020$date)

common_col_names <- intersect(names(licor_physiological_measurements_BH_2020), names(water_potentials_BH_2020))

data_physiology_complete_BH_2020 <- merge(licor_physiological_measurements_BH_2020, water_potentials_BH_2020, by= common_col_names, all = TRUE)


str(data_physiology_complete_BH_2020)
str(water_potentials_BH_2020)


write.csv(data_physiology_complete_BH_2020,"data_output/data_physiology_complete_BH_2020.csv")



#### midday Jul 31 ####


#### water potentials
water_potentials_bh_2020_0731 <- read.csv("data/water_potentials_20200731.csv", header =TRUE)

water_potentials_bh_2020_0731<- water_potentials_bh_2020_0731%>%
  mutate(LEAF = "_1")%>%
  mutate(stem_wp_MPa = (stem_wp_bar*0.1)*-1) %>%
  mutate(leaf_wp_MPa = (leaf_wp_bar*0.1 )*-1) %>%
  select(!(stem_wp_bar))%>%
  select(!(leaf_wp_bar))%>%
  filter(!is.na(leaf_wp_MPa)) %>%
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
    block_id == "B3R3_SE" ~ "_SE", ))

water_potentials_bh_2020_0731<-water_potentials_bh_2020_0731 %>%
  mutate( VINE = case_when(
    vine == 5.1 ~ "_5.10",
    vine == 5.3 ~ "_5.3",
    vine == 5.4 ~ "_5.4",
    vine == 5.5 ~ "_5.5",
    vine == 5.6 ~ "_5.6",
    vine == 5.7 ~ "_5.7",
    vine == 5.8 ~ "_5.8",
    vine == 5.9 ~ "_5.9",
    vine == 5.10 ~ "_5.10",
    vine == 5.11 ~ "_5.11",
    vine == 5.12 ~ "_5.12",
    vine == 5.13 ~ "_5.13",
    vine == 5.14 ~ "_5.14",
    vine == 5.15 ~ "_5.15",
  )) %>%
  select(!(vine))

####licor

table1_midday_jul_31<- read.csv("data/2020-07-31-1201_midday_ejf2.csv", header = TRUE, sep =",", skip = 13)
table2_midday_jul_31<- read.csv("data/2020-07-31-1201_midday_ejf2_0.csv", header= TRUE, sep =",", skip = 13)
table3_midday_jul_31<- read.csv("data/2020-07-31-1215_BH455_midday_ejf1.csv", header= TRUE, sep =",", skip = 13)


table1_midday_jul_31_sel <- table1_midday_jul_31 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-31-2020", day = 213, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table1_midday_jul_31_sel <-table1_midday_jul_31_sel[-1, ]   %>%
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
  ))%>%
  filter(!is.na(treatment))


table2_midday_jul_31_sel <- table2_midday_jul_31 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-31-2020", day = 213, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table2_midday_jul_31_sel <-table2_midday_jul_31_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table3_midday_jul_31_sel <- table3_midday_jul_31 %>% 
  mutate(SUB_BLOCK = SUB.BLOCK) %>%
  mutate(date = "07-31-2020", day = 213, year =2020, round =4)%>%
  select(date, hhmmss, BLOCK, SUB_BLOCK, VINE, LEAF,round,day, year, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham, DarkAdaptedID, Fo, Fm, Fv, Fv.Fm, Adark, LightAdaptedID, Fs, Fm., PhiPS2, PS2.1, Qabs_fs, Afs, ETR, Fv..Fm., PhiCO2, NPQ, DarkPulseID, Fo., Fv., qP, qN, qP_Fo, qN_Fo)

table3_midday_jul_31_sel <-table3_midday_jul_31_sel [-1, ]   %>%
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
  )) %>%
  filter(!is.na(treatment))


table_comb_midday_jul_31 <-rbind(table1_midday_jul_31_sel, table2_midday_jul_31_sel, table3_midday_jul_31_sel)

table_comb_midday_jul_31%>%
  group_by(treatment, round, date, day, BLOCK)%>%
  tally()

table_midday_jul_31_2020 <-table_comb_midday_jul_31

####Combined water pontentials and licor measurements Jul 31 #####

table_midday_jul_31_2020$date<-format(table_midday_jul_31_2020$date)
table_midday_jul_31_2020$date<- mdy(table_midday_jul_31_2020$date)


str(table_midday_jul_31_2020$date)

water_potentials_bh_2020_0731$date<-format(water_potentials_bh_2020_0731$date)
water_potentials_bh_2020_0731$date<- mdy(water_potentials_bh_2020_0731$date)

str(water_potentials_bh_2020_0731$date)

common_col_names <- intersect(names(table_midday_jul_31_2020), names(water_potentials_bh_2020_0731))

data_physiology_jul_31_BH_2020 <- merge(table_midday_jul_31_2020,water_potentials_bh_2020_0731 ,by= common_col_names, all = TRUE)

data_physiology_all_complete_BH_2020<- rbind(data_physiology_complete_BH_2020, data_physiology_jul_31_BH_2020)


write.csv(data_physiology_all_complete_BH_2020,"data_output/data_physiology_all_complete_BH_2020.csv")

