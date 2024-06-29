####combing diurnals and water potentials data before blockid change

library(tidyverse)

####July 1 2019#####

#jUL1_R1

table1_r1_jul2 <- read.csv("data/2019-07-02-0607_logdata_BH445_run1 missing.csv", header = TRUE, sep =",", skip = 54)
table2_r1_jul2<- read.csv("data/2019-07-02-0608_logdata_BH1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_jul2_sel <-table1_r1_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =1)
table1_r1_jul2_sel <- table1_r1_jul2_sel [-1, ]

table2_r1_jul2_sel <-table2_r1_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =1)
table2_r1_jul2_sel <- table2_r1_jul2_sel [-1, ]

names(table1_r1_jul2_sel) <- names(table2_r1_jul2_sel ) 
identical(names(table1_r1_jul2_sel), names (table2_r1_jul2_sel ))

table_comb__r1_Jul2 <- rbind (table1_r1_jul2_sel, table2_r1_jul2_sel)

#jUL1_R2

table1_r2_jul2 <- read.csv("data/2019-07-02-0942_BH455_round2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_jul2<- read.csv("data/2019-07-02-0942_BH455_round2_0.csv", header= TRUE, sep =",", skip = 15)
table3_r2_jul2<- read.csv("data/2019-07-02-1007_BH445_round2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul2_sel <-table1_r2_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =2)
table1_r2_jul2_sel <- table1_r2_jul2_sel [-1, ]

table2_r2_jul2_sel <-table2_r2_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =2)
table2_r2_jul2_sel <- table2_r2_jul2_sel [-1, ]

table3_r2_jul2_sel <-table3_r2_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =2)
table3_r2_jul2_sel <- table3_r2_jul2_sel [-1, ]

names(table1_r2_jul2_sel) <- names(table2_r2_jul2_sel ) 
identical(names(table1_r2_jul2_sel), names (table2_r2_jul2_sel ))
names(table2_r2_jul2_sel) <- names(table3_r2_jul2_sel )
identical(names(table2_r2_jul2_sel), names (table3_r2_jul2_sel ))

table_comb_r2_Jul2 <- rbind(table1_r2_jul2_sel,table2_r2_jul2_sel,table3_r2_jul2_sel)

#jUL1_R3

table1_r3_jul2 <- read.csv("data/2019-07-02-1338_BH455_diurnals_round3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_jul2<- read.csv("data/2019-07-02-1339_BH455_round3.csv", header= TRUE, sep =",", skip = 15)

table1_r3_jul2_sel <-table1_r3_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =3)
table1_r3_jul2_sel <- table1_r3_jul2_sel [-1, ]

table2_r3_jul2_sel <-table2_r3_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =3)
table2_r3_jul2_sel <- table2_r3_jul2_sel [-1, ]


names(table1_r3_jul2_sel) <- names(table2_r3_jul2_sel ) 
identical(names(table1_r3_jul2_sel), names (table2_r3_jul2_sel ))

table_comb_r3_Jul2 <-rbind(table2_r3_jul2_sel, table1_r3_jul2_sel)

#jUL1_R4


table1_r4_jul2 <- read.csv("data/2019-07-02-1617_BH445_round4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_jul2<- read.csv("data/2019-07-02-1625_BH455_round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul2_sel <-table1_r4_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =4)
table1_r4_jul2_sel <- table1_r4_jul2_sel [-1, ]

table2_r4_jul2_sel <-table2_r4_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =4)
table2_r4_jul2_sel <- table2_r4_jul2_sel [-1, ]


names(table1_r4_jul2_sel) <- names(table2_r4_jul2_sel ) 
identical(names(table1_r4_jul2_sel), names (table2_r4_jul2_sel ))

table_comb_r4_Jul2 <-rbind(table1_r4_jul2_sel, table2_r4_jul2_sel)


#jUL1_R5

table1_r5_jul2 <- read.csv("data/2019-07-02-1845_BH445_round5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_jul2<- read.csv("data/2019-07-02-1855_logdata_BH455_Round5.csv", header= TRUE, sep =",", skip = 13)
table3_r5_jul2<- read.csv("data/2019-07-02-1855_logdata_BH455_Round5_0.csv", header= TRUE, sep =",", skip = 13)

table1_r5_jul2_sel <-table1_r5_jul2 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =5)
table1_r5_jul2_sel <- table1_r5_jul2_sel [-1, ]

table2_r5_jul2_sel <-table2_r5_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =5)
table2_r5_jul2_sel <- table2_r5_jul2_sel [-1, ]

table3_r5_jul2_sel <-table3_r5_jul2 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-01-2019", day = 182, year =2019, round =5)
table3_r5_jul2_sel <- table3_r5_jul2_sel [-1, ]

names(table1_r5_jul2_sel) <- names(table2_r5_jul2_sel ) 
names(table2_r5_jul2_sel) <- names(table3_r5_jul2_sel )
identical(names(table1_r5_jul2_sel), names (table2_r5_jul2_sel ))
identical(names(table2_r5_jul2_sel), names (table3_r5_jul2_sel ))

table_comb_r5_Jul2 <-rbind(table1_r5_jul2_sel, table2_r5_jul2_sel, table3_r5_jul2_sel)

#Combining all rounds fro diurnal jul 1
#####Repeat until names are identical TRUE ######
names(table_comb__r1_Jul2) <- names(table_comb_r2_Jul2)
names(table_comb_r2_Jul2) <- names(table_comb_r3_Jul2)
names(table_comb_r3_Jul2) <- names(table_comb_r4_Jul2)
names(table_comb_r4_Jul2) <- names(table_comb_r5_Jul2)

identical(names(table_comb__r1_Jul2), names (table_comb_r2_Jul2))
identical(names(table_comb_r2_Jul2), names (table_comb_r3_Jul2))
identical(names(table_comb_r3_Jul2), names (table_comb_r4_Jul2))
identical(names(table_comb_r4_Jul2), names (table_comb_r5_Jul2))
#####Repeat until names are identical TRUE ######
table_diurnals_Jul2 <-rbind(table_comb__r1_Jul2, table_comb_r2_Jul2, table_comb_r3_Jul2, table_comb_r4_Jul2, table_comb_r5_Jul2)

write.csv(table_diurnals_Jul2, "data_output/table_diurnals_Jul1")



#####jUL5_Midday 2019 #####

library(tidyverse)

table1_midday_jul5 <- read.csv("data/2019-07-05-1200_BH_midday.csv", header = TRUE, sep =",", skip = 13)

table1_midday_jul5_sel <-table1_midday_jul5 %>%
  select (date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-05-2019", day = 186, year =2019, round = "midday")
table1_midday_jul5_sel  <- table1_midday_jul5_sel  [-1, ]

write.csv(table1_midday_jul5_sel,"data_output/table_midday_jul5")


###July 12 2019#####
#jUL12_r1 

table1_r1_jul12 <- read.csv("data/2019-07-12-0536_logdata_BH_R1.csv", header = TRUE, sep =",", skip = 15)
table2_r1_jul12<- read.csv("data/2019-07-12-0551_logdata_BH_Round1.csv", header= TRUE, sep =",", skip = 53)

table1_r1_jul12_sel <-table1_r1_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =1)
table1_r1_jul12_sel <- table1_r1_jul12_sel [-1, ]

table2_r1_jul12_sel <-table2_r1_jul12 %>%
  select(date, hhmmss,BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =1)
table2_r1_jul12_sel <- table2_r1_jul12_sel [-1, ]


names(table1_r1_jul12_sel) <- names(table2_r1_jul12_sel ) 
identical(names(table1_r1_jul12_sel), names (table2_r1_jul12_sel ))

table_comb_r1_Jul12 <-rbind(table1_r1_jul12_sel, table2_r1_jul12_sel)

#Jul12_r2 

table1_r2_jul12 <- read.csv("data/2019-07-12-0758_logdata_BH_R2 missing.csv", header = TRUE, sep =",", skip = 54)
table2_r2_jul12<- read.csv("data/2019-07-12-0804_logdata_BH_Round2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul12_sel <-table1_r2_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =2)
table1_r2_jul12_sel <- table1_r2_jul12_sel [-1, ]

table2_r2_jul12_sel <-table2_r2_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =2)
table2_r2_jul12_sel <- table2_r2_jul12_sel [-1, ]


names(table1_r2_jul12_sel) <- names(table2_r2_jul12_sel ) 
identical(names(table1_r2_jul12_sel), names (table2_r2_jul12_sel ))

table_comb_r2_Jul12 <-rbind(table1_r2_jul12_sel, table2_r2_jul12_sel)

#jul12_r3

table1_r3_jul12 <- read.csv("data/2019-07-12-1046_logdata_BH_R3 missing.csv", header = TRUE, sep =",", skip = 54)
table2_r3_jul12<- read.csv("data/2019-07-12-1053_BH_R3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_jul12_sel <-table1_r3_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =3)
table1_r3_jul12_sel <- table1_r3_jul12_sel [-1, ]

table2_r3_jul12_sel <-table2_r3_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =3)
table2_r3_jul12_sel <- table2_r3_jul12_sel [-1, ]


names(table1_r3_jul12_sel) <- names(table2_r3_jul12_sel ) 
identical(names(table1_r3_jul12_sel), names (table2_r3_jul12_sel ))

table_comb_r3_Jul12 <-rbind(table1_r3_jul12_sel, table2_r3_jul12_sel)

#jul12_r4

table1_r4_jul12 <- read.csv("data/2019-07-12-1318_logdata_BH_R4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_jul12<- read.csv("data/2019-07-12-1812_logdata_BH_Round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul12_sel <-table1_r4_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =4)
table1_r4_jul12_sel <- table1_r4_jul12_sel [-1, ]

table2_r4_jul12_sel <-table2_r4_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =4)
table2_r4_jul12_sel <- table2_r4_jul12_sel [-1, ]


names(table1_r4_jul12_sel) <- names(table2_r4_jul12_sel ) 
identical(names(table1_r4_jul12_sel), names (table2_r4_jul12_sel ))

table_comb_r4_Jul12 <-rbind(table1_r4_jul12_sel, table2_r4_jul12_sel)

#jul12_r5

table1_r5_jul12 <- read.csv("data/2019-07-12-1618_logdata_BH_R5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_jul12<- read.csv("data/2019-07-12-2107_BH_R5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_jul12_sel <-table1_r5_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =5)
table1_r5_jul12_sel <- table1_r5_jul12_sel [-1, ]

table2_r5_jul12_sel <-table2_r5_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =5)
table2_r5_jul12_sel <- table2_r5_jul12_sel [-1, ]

names(table1_r5_jul12_sel) <- names(table2_r5_jul12_sel ) 
identical(names(table1_r5_jul12_sel), names (table2_r5_jul12_sel ))

table_comb_r5_Jul12 <-rbind(table1_r5_jul12_sel, table2_r5_jul12_sel)

#Jul12_r6

table1_r6_jul12 <- read.csv("data/2019-07-12-1840_logdata_BH_R6.csv", header = TRUE, sep =",", skip = 15)
table2_r6_jul12<- read.csv("data/2019-07-12-2349_logdata_BH_Round6-last log low CO2.csv", header= TRUE, sep =",", skip = 13)

table1_r6_jul12_sel <-table1_r6_jul12 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =6)
table1_r6_jul12_sel <- table1_r6_jul12_sel [-1, ]

table2_r6_jul12_sel <-table2_r6_jul12 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-12-2019", day = 193, year =2019, round =6)
table2_r6_jul12_sel <- table2_r6_jul12_sel [-1, ]

names(table1_r6_jul12_sel) <- names(table2_r6_jul12_sel ) 
identical(names(table1_r6_jul12_sel), names (table2_r6_jul12_sel ))

table_comb_r6_Jul12 <-rbind(table1_r6_jul12_sel, table2_r6_jul12_sel)

#Combine all rounds diurnal jul 12

names(table_comb_r1_Jul12) <- names(table_comb_r2_Jul12) 
names(table_comb_r2_Jul12) <- names(table_comb_r3_Jul12)
names(table_comb_r3_Jul12) <- names(table_comb_r4_Jul12)
names(table_comb_r4_Jul12) <- names(table_comb_r5_Jul12)
names(table_comb_r5_Jul12) <- names(table_comb_r6_Jul12)

identical(names(table_comb_r1_Jul12), names (table_comb_r2_Jul12))
identical(names(table_comb_r2_Jul12), names (table_comb_r3_Jul12))
identical(names(table_comb_r3_Jul12), names (table_comb_r4_Jul12))
identical(names(table_comb_r4_Jul12), names (table_comb_r5_Jul12))
identical(names(table_comb_r5_Jul12), names (table_comb_r6_Jul12))

table_diurnals_Jul12 <-rbind(table_comb_r1_Jul12, table_comb_r2_Jul12, table_comb_r3_Jul12, table_comb_r4_Jul12, table_comb_r5_Jul12, table_comb_r6_Jul12)

write.csv(table_diurnals_Jul12, "data_output/table_diurnal_jul12")

##### Diurnals before change in block's ID ####

diurnals_jul2 <-read.csv("data_output/table_diurnals_Jul2", header = TRUE, sep = ",")
Midday_jul5 <- read.csv("data_output/table_midday_jul5", header = TRUE, sep = ",")
diurnals_jul12 <- read.csv("data_output/table_diurnal_jul12", header = TRUE, sep = ",")

names(diurnals_jul2) <- names(Midday_jul5) 
names(Midday_jul5) <- names(diurnals_jul12)

identical(names(diurnals_jul2), names(Midday_jul5))
identical(names(Midday_jul5), names(diurnals_jul12))

names(diurnals_jul2) <- names(Midday_jul5) 
names(Midday_jul5) <- names(diurnals_jul12)

identical(names(diurnals_jul2), names(Midday_jul5))
identical(names(Midday_jul5), names(diurnals_jul12))

diurnals_before_blockid_change <- rbind(diurnals_jul2, Midday_jul5, diurnals_jul12)

diurnals_before_blockid_change <- diurnals_before_blockid_change%>%
  mutate(pixel_number = case_when(
    BH_Block == "B1_R2" ~ 45,
    BH_Block == "B1_R3" ~ 55,
    BH_Block == "B1_R1" ~ 35, 
    BH_Block == "B2_R1" ~ 13,
    BH_Block == "B2_R2" ~ 27,
    BH_Block == "B2_R3" ~ 75,
    BH_Block == "B3_R1" ~ 23,
    BH_Block == "B3_R2" ~ 78,
    BH_Block == "B3_R3" ~ 48,
  ))%>%
  mutate( treatment= case_when(
    pixel_number == 55 ~ 1,
    pixel_number == 13 ~ 1,
    pixel_number == 48 ~ 2,
    pixel_number == 35 ~ 2,
    pixel_number == 23 ~ 2,
    pixel_number == 78 ~ 2,
    pixel_number == 45 ~ 3,
    pixel_number == 27 ~ 3,
    pixel_number == 75 ~ 3,))%>%
  mutate(Rep= case_when(
    pixel_number == 55 ~ 1,
    pixel_number == 13 ~ 2,
    pixel_number == 48 ~ 1,
    pixel_number == 35 ~ 2,
    pixel_number == 23 ~ 3,
    pixel_number == 78 ~ 4,
    pixel_number == 45 ~ 1,
    pixel_number == 27 ~ 2,
    pixel_number == 75 ~ 3,))%>%
  filter(!pixel_number == 78)

diurnals_before_blockid_change_tally<-diurnals_before_blockid_change%>%
  group_by(treatment, date, round)%>%
  tally()

write.csv(diurnals_before_blockid_change, "data_output/diurnals_before_blockid_change.csv")


####JULY 25 2019####

# Combining with csv diurnals Jul 25


#Jul_25 R1

table1_r1_jul25 <- read.csv("data/2019-07-25-0431_BH_R1.csv", header = TRUE, sep =",", skip = 15)
table2_r1_jul25<- read.csv("data/2019-07-25-0927_BH_R1.csv", header= TRUE, sep =",", skip = 13)
table3_r1_jul25<- read.csv("data/2019-07-25-0426_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_jul25_sel <-table1_r1_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =1)
table1_r1_jul25_sel <- table1_r1_jul25_sel [-1, ]

table2_r1_jul25_sel <-table2_r1_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =1)
table2_r1_jul25_sel <- table2_r1_jul25_sel [-1, ]

table3_r1_jul25_sel <-table3_r1_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =1)
table3_r1_jul25_sel <- table3_r1_jul25_sel [-1, ]

names(table1_r1_jul25_sel ) <- names(table2_r1_jul25_sel)
identical(names(table1_r1_jul25_sel), names (table2_r1_jul25_sel ))
names(table2_r1_jul25_sel ) <- names(table3_r1_jul25_sel)
identical(names(table2_r1_jul25_sel), names (table3_r1_jul25_sel ))

table_comb_r1_jul25<-rbind(table1_r1_jul25_sel, table2_r1_jul25_sel, table3_r1_jul25_sel)
table_comb_r1_jul25 <- table_comb_r1_jul25 %>%
  filter(!is.na (Fv.Fm))

#Jul25_r2

table1_r2_jul25 <- read.csv("data/2019-07-25-0743_BH_R2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_jul25<- read.csv("data/2019-07-25-0751_BH_R2.csv", header= TRUE, sep =",", skip = 13)
table3_r2_jul25<- read.csv("data/2019-07-25-1257_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul25_sel <-table1_r2_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =2)
table1_r2_jul25_sel <- table1_r2_jul25_sel [-1, ]

table2_r2_jul25_sel <-table2_r2_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =2)
table2_r2_jul25_sel <- table2_r2_jul25_sel [-1, ]

table3_r2_jul25_sel <-table3_r2_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =2)
table3_r2_jul25_sel <- table3_r2_jul25_sel [-1, ]

names(table1_r2_jul25_sel ) <- names(table2_r2_jul25_sel)
identical(names(table1_r2_jul25_sel), names (table2_r2_jul25_sel ))
names(table2_r2_jul25_sel ) <- names(table3_r2_jul25_sel)
identical(names(table2_r2_jul25_sel), names (table3_r2_jul25_sel ))

table_comb_r2_jul25<-rbind(table1_r2_jul25_sel, table2_r2_jul25_sel, table3_r2_jul25_sel)
table_comb_r2_jul25 <- table_comb_r2_jul25 %>%
  filter(!is.na (Fv.Fm))

#Jul25_r3

table1_r3_jul25 <- read.csv("data/2019-07-25-1007_BH_R3_andrew.csv", header = TRUE, sep =",", skip = 15)
table2_r3_jul25<- read.csv("data/2019-07-25-1007_BH_R3.csv", header= TRUE, sep =",", skip = 13)
table3_r3_jul25<- read.csv("data/2019-07-25-1008_BH_R3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_jul25_sel <-table1_r3_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =3)
table1_r3_jul25_sel <- table1_r3_jul25_sel [-1, ]

table2_r3_jul25_sel <-table2_r3_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =3)
table2_r3_jul25_sel <- table2_r3_jul25_sel [-1, ]

table3_r3_jul25_sel <-table3_r3_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =3)
table3_r2_jul25_sel <- table3_r2_jul25_sel [-1, ]

names(table1_r3_jul25_sel ) <- names(table2_r3_jul25_sel)
identical(names(table1_r3_jul25_sel), names (table2_r3_jul25_sel ))
names(table2_r3_jul25_sel ) <- names(table3_r3_jul25_sel)
identical(names(table2_r3_jul25_sel), names (table3_r3_jul25_sel ))

table_comb_r3_jul25<-rbind(table1_r3_jul25_sel, table2_r3_jul25_sel, table3_r3_jul25_sel)
table_comb_r3_jul25 <- table_comb_r3_jul25 %>%
  filter(!is.na (Fv.Fm))


#Jul25_r4

table1_r4_jul25 <- read.csv("data/2019-07-25-1216_logdata_BH_R4.csv", header = TRUE, sep =",", skip = 53)
table2_r4_jul25<- read.csv("data/2019-07-25-1203_BH_R4.csv", header= TRUE, sep =",", skip = 15)
table3_r4_jul25<- read.csv("data/2019-07-25-1212_BH_R4.csv", header= TRUE, sep =",", skip = 13)
table4_r4_jul25<- read.csv("data/2019-07-25-1247_logdata_BH_R4.2.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul25_sel <-table1_r4_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table1_r4_jul25_sel <- table1_r4_jul25_sel [-1, ]

table2_r4_jul25_sel <-table2_r4_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table2_r4_jul25_sel <- table2_r4_jul25_sel [-1, ]

table3_r4_jul25_sel <-table3_r4_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table3_r4_jul25_sel <- table3_r4_jul25_sel [-1, ]

table4_r4_jul25_sel <-table4_r4_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =4)
table4_r4_jul25_sel <- table4_r4_jul25_sel [-1, ]

names(table1_r4_jul25_sel ) <- names(table2_r4_jul25_sel)
identical(names(table1_r4_jul25_sel), names (table2_r4_jul25_sel ))
names(table2_r4_jul25_sel ) <- names(table3_r4_jul25_sel)
identical(names(table2_r4_jul25_sel), names (table3_r4_jul25_sel ))
names(table3_r4_jul25_sel ) <- names(table4_r4_jul25_sel)
identical(names(table3_r4_jul25_sel), names (table4_r4_jul25_sel ))


names(table1_r4_jul25_sel ) <- names(table2_r4_jul25_sel)
identical(names(table1_r4_jul25_sel), names (table2_r4_jul25_sel ))
names(table2_r4_jul25_sel ) <- names(table3_r4_jul25_sel)
identical(names(table2_r4_jul25_sel), names (table3_r4_jul25_sel ))
names(table3_r4_jul25_sel ) <- names(table4_r4_jul25_sel)
identical(names(table3_r4_jul25_sel), names (table4_r4_jul25_sel ))

####Repeat names and idetical until alll TRUE

table_comb_r4_jul25<-rbind(table1_r4_jul25_sel, table2_r4_jul25_sel, table3_r4_jul25_sel, table4_r4_jul25_sel )

table_comb_r4_jul25 <- table_comb_r4_jul25 %>%
  filter(!is.na (Fv.Fm))

#Jul25_r5

table1_r5_jul25 <- read.csv("data/2019-07-25-1603_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_jul25<- read.csv("data/2019-07-25-1604_BH_R5.csv", header= TRUE, sep =",", skip = 13)
table3_r5_jul25<- read.csv("data/2019-07-25-1604_logdata_BH_R5.csv", header= TRUE, sep =",", skip = 15)

table1_r5_jul25_sel <-table1_r5_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =5)
table1_r5_jul25_sel <- table1_r5_jul25_sel [-1, ]

table2_r5_jul25_sel <-table2_r5_jul25 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =5)
table2_r5_jul25_sel <- table2_r5_jul25_sel [-1, ]

table3_r5_jul25_sel <-table3_r5_jul25 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-25-2019", day = 206, year =2019, round =5)
table3_r5_jul25_sel <- table3_r5_jul25_sel [-1, ]

names(table1_r5_jul25_sel ) <- names(table2_r5_jul25_sel)
identical(names(table1_r5_jul25_sel), names (table2_r5_jul25_sel ))
names(table2_r5_jul25_sel ) <- names(table3_r5_jul25_sel)
identical(names(table2_r5_jul25_sel), names (table3_r5_jul25_sel ))

names(table1_r5_jul25_sel ) <- names(table2_r5_jul25_sel)
identical(names(table1_r5_jul25_sel), names (table2_r5_jul25_sel ))
names(table2_r5_jul25_sel ) <- names(table3_r5_jul25_sel)
identical(names(table2_r5_jul25_sel), names (table3_r5_jul25_sel ))

table_comb_r5_jul25<-rbind(table1_r5_jul25_sel, table2_r5_jul25_sel, table3_r5_jul25_sel)
table_comb_r5_jul25 <- table_comb_r5_jul25 %>%
  filter(!is.na (Fv.Fm))

#All round combined diurnals Jul 25

names(table_comb_r1_jul25) <- names(table_comb_r2_jul25) 
names(table_comb_r2_jul25) <- names(table_comb_r3_jul25)
names(table_comb_r3_jul25) <- names(table_comb_r4_jul25)
names(table_comb_r4_jul25) <- names(table_comb_r5_jul25)


identical(names(table_comb_r1_jul25), names (table_comb_r2_jul25))
identical(names(table_comb_r2_jul25), names (table_comb_r3_jul25))
identical(names(table_comb_r3_jul25), names (table_comb_r4_jul25))
identical(names(table_comb_r4_jul25), names (table_comb_r5_jul25))

####Repeat names and identical until all TRUE

table_diurnals_jul25 <-rbind(table_comb_r1_jul25,table_comb_r2_jul25, table_comb_r3_jul25, table_comb_r4_jul25, table_comb_r5_jul25)

write.csv(table_diurnals_jul25, "data_output/table_diurnals_jul25")

####July 28 2019 ####

#jul28_r1


table1_r1_jul28 <- read.csv("data/2019-07-28-0451_BH_R1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_jul28<- read.csv("data/2019-07-28-0514_BH_R1.csv", header= TRUE, sep =",", skip = 15)

table1_r1_jul28_sel <-table1_r1_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm,Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =1)
table1_r1_jul28_sel <- table1_r1_jul28_sel [-1, ]

table2_r1_jul28_sel <-table2_r1_jul28 %>%
  select(date, hhmmss, BH445_LEAF , BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =1)
table2_r1_jul28_sel <- table2_r1_jul28_sel [-1, ]


names(table1_r1_jul28_sel) <- names(table2_r1_jul28_sel )
identical(names(table1_r1_jul28_sel), names (table2_r1_jul28_sel ))

table_comb_r1_Jul28 <-rbind(table1_r1_jul28_sel, table2_r1_jul28_sel)

#Jul28_r2

table1_r2_jul28 <- read.csv("data/2019-07-28-0812_BH_R2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_jul28<- read.csv("data/2019-07-28-0820_logdata_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_jul28_sel <-table1_r2_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =2) %>%
  filter(!is.na(Fv.Fm))


table2_r2_jul28_sel <-table2_r2_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =2)
table2_r2_jul28_sel <- table2_r2_jul28_sel [-1, ]


names(table1_r2_jul28_sel) <- names(table2_r2_jul28_sel )
identical(names(table1_r2_jul28_sel), names (table2_r2_jul28_sel ))

table_comb_r2_Jul28 <-rbind(table1_r2_jul28_sel, table2_r2_jul28_sel)

#Jul28_r3

table1_r3_jul28 <- read.csv("data/2019-07-28-1105_BH_R3.csv", header = TRUE, sep =",", skip = 15)
table2_r3_jul28<- read.csv("data/2019-07-28-1107_logdata_BH_R3-low_RH_last_points.csv", header= TRUE, sep =",", skip = 13)

table1_r3_jul28_sel <-table1_r3_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =3)
table1_r3_jul28_sel <- table1_r3_jul28_sel [-1, ]

table2_r3_jul28_sel <-table2_r3_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =3)
table2_r3_jul28_sel <- table2_r3_jul28_sel [-1, ]


names(table1_r3_jul28_sel) <- names(table2_r3_jul28_sel )
identical(names(table1_r3_jul28_sel), names (table2_r3_jul28_sel ))

table_comb_r3_Jul28 <-rbind(table1_r3_jul28_sel, table2_r3_jul28_sel)


#Jul28_r4

table1_r4_jul28 <- read.csv("data/2019-07-28-1310_BH_R4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_jul28<- read.csv("data/2019-07-28-1316_logdata_BH_R4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_jul28_sel <-table1_r4_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =4)
table1_r4_jul28_sel <- table1_r4_jul28_sel [-1, ]

table2_r4_jul28_sel <-table2_r4_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =4)
table2_r4_jul28_sel <- table2_r4_jul28_sel [-1, ]


names(table1_r4_jul28_sel) <- names(table2_r4_jul28_sel )
identical(names(table1_r4_jul28_sel), names (table2_r4_jul28_sel ))

table_comb_r4_Jul28 <-rbind(table1_r4_jul28_sel, table2_r4_jul28_sel)

#Jul28_r5

table1_r5_jul28 <- read.csv("data/2019-07-28-1606_BH_R5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_jul28<- read.csv("data/2019-07-28-1610_logdata_BH_R5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_jul28_sel <-table1_r5_jul28 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =5)
table1_r5_jul28_sel <- table1_r5_jul28_sel [-1, ]

table2_r5_jul28_sel <-table2_r5_jul28 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "07-28-2019", day = 209, year =2019, round =5)
table2_r5_jul28_sel <- table2_r5_jul28_sel [-1, ]


names(table1_r5_jul28_sel) <- names(table2_r5_jul28_sel )
identical(names(table1_r5_jul28_sel), names (table2_r5_jul28_sel ))

table_comb_r5_Jul28 <-rbind(table1_r5_jul28_sel, table2_r5_jul28_sel)

#Combine all rounds diurnals jul 28

names(table_comb_r1_Jul28) <- names(table_comb_r2_Jul28) 
names(table_comb_r2_Jul28) <- names(table_comb_r3_Jul28)
names(table_comb_r3_Jul28) <- names(table_comb_r4_Jul28)
names(table_comb_r4_Jul28) <- names(table_comb_r5_Jul28)

identical(names(table_comb_r1_Jul28), names (table_comb_r2_Jul28))
identical(names(table_comb_r2_Jul28), names (table_comb_r3_Jul28))
identical(names(table_comb_r3_Jul28), names (table_comb_r4_Jul28))
identical(names(table_comb_r4_Jul28), names (table_comb_r5_Jul28))

table_diurnals_Jul28 <-rbind(table_comb_r1_Jul28, table_comb_r2_Jul28, table_comb_r3_Jul28, table_comb_r4_Jul28, table_comb_r5_Jul28)

write.csv(table_diurnals_Jul28, "data_output/table_diurnal_jul28")


####August 01 2019####
#Aug01_r1

table1_r1_aug1 <- read.csv("data/2019-08-01-0456_BH_R1.csv", header = TRUE, sep =",", skip = 15)
table2_r1_aug1<- read.csv("data/2019-08-01-0500_logdata_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_aug1_sel <-table1_r1_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =1)
table1_r1_aug1_sel <- table1_r1_aug1_sel [-1, ]

table2_r1_aug1_sel <-table2_r1_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =1)
table2_r1_aug1_sel <- table2_r1_aug1_sel [-1, ]


names(table1_r1_aug1_sel) <- names(table2_r1_aug1_sel )
identical(names(table1_r1_aug1_sel), names (table2_r1_aug1_sel ))

table_comb_r1_aug1 <-rbind(table1_r1_aug1_sel, table2_r1_aug1_sel)

#Aug1_r2

table1_r2_aug1 <- read.csv("data/2019-08-01-0732_BH_R2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_aug1<- read.csv("data/2019-08-01-0714_logdata_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_aug1_sel <-table1_r2_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =2)
table1_r2_aug1_sel <- table1_r2_aug1_sel [-1, ]

table2_r2_aug1_sel <-table2_r2_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =2)
table2_r2_aug1_sel <- table2_r2_aug1_sel [-1, ]

names(table1_r2_aug1_sel) <- names(table2_r2_aug1_sel )
identical(names(table1_r2_aug1_sel), names (table2_r2_aug1_sel ))

table_comb_r2_aug1 <-rbind(table1_r2_aug1_sel, table2_r2_aug1_sel)

#Aug1_r3

table1_r3_aug1 <- read.csv("data/2019-08-01-0954_BH_R3.csv", header = TRUE, sep =",", skip = 15)
table2_r3_aug1<- read.csv("data/2019-08-01-0958_logdata_BH_R3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_aug1_sel <-table1_r3_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =3)
table1_r3_aug1_sel <- table1_r3_aug1_sel [-1, ]

table2_r3_aug1_sel <-table2_r3_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =3)
table2_r3_aug1_sel <- table2_r3_aug1_sel [-1, ]

names(table1_r3_aug1_sel) <- names(table2_r3_aug1_sel )
identical(names(table1_r3_aug1_sel), names (table2_r3_aug1_sel ))

table_comb_r3_aug1 <-rbind(table1_r3_aug1_sel, table2_r3_aug1_sel)

#Aug1_r4

table1_r4_aug1 <- read.csv("data/2019-08-01-1235_BH_R4.csv", header = TRUE, sep =",", skip = 15)
table2_r4_aug1<- read.csv("data/2019-08-01-1236_logdata_BH_R4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_aug1_sel <-table1_r4_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =4)
table1_r4_aug1_sel <- table1_r4_aug1_sel [-1, ]

table2_r4_aug1_sel <-table2_r4_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =4)
table2_r4_aug1_sel <- table2_r4_aug1_sel [-1, ]

names(table1_r4_aug1_sel) <- names(table2_r4_aug1_sel )
identical(names(table1_r4_aug1_sel), names (table2_r4_aug1_sel ))

table_comb_r4_aug1 <-rbind(table1_r4_aug1_sel, table2_r4_aug1_sel)

#Aug1_r5

table1_r5_aug1 <- read.csv("data/2019-08-01-1601_BH_R5.csv", header = TRUE, sep =",", skip = 15)
table2_r5_aug1<- read.csv("data/2019-08-01-1601_logdata_BH_R5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_aug1_sel <-table1_r5_aug1 %>%
  select(date, hhmmss, BH445_LEAF, BH445_BLOCK, BH445_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =5)
table1_r5_aug1_sel <- table1_r5_aug1_sel [-1, ]

table2_r5_aug1_sel <-table2_r5_aug1 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-01-2019", day = 213, year =2019, round =5)
table2_r5_aug1_sel <- table2_r5_aug1_sel [-1, ]

names(table1_r5_aug1_sel) <- names(table2_r5_aug1_sel )
identical(names(table1_r5_aug1_sel), names (table2_r5_aug1_sel ))

table_comb_r5_aug1 <-rbind(table1_r5_aug1_sel, table2_r5_aug1_sel)

names(table_comb_r1_aug1) <- names(table_comb_r2_aug1)
names(table_comb_r2_aug1) <- names(table_comb_r3_aug1)
names(table_comb_r3_aug1) <- names(table_comb_r4_aug1)
names(table_comb_r4_aug1) <- names(table_comb_r5_aug1)


identical(names(table_comb_r1_aug1), names (table_comb_r2_aug1))
identical(names(table_comb_r2_aug1), names (table_comb_r3_aug1))
identical(names(table_comb_r3_aug1), names (table_comb_r4_aug1))
identical(names(table_comb_r4_aug1), names (table_comb_r5_aug1))


table_diurnals_aug1 <-rbind(table_comb_r1_aug1, table_comb_r2_aug1, table_comb_r3_aug1, table_comb_r4_aug1, table_comb_r5_aug1)

write.csv(table_diurnals_aug1, "data_output/table_diurnals_aug1")

####August 15 2019#####

#Aug_15

table1_r1_aug15 <- read.csv("data/2019-08-15-0524_Round1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_aug15<- read.csv("data/2019-08-15-0533_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_aug15_sel <-table1_r1_aug15 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =1)
table1_r1_aug15_sel <- table1_r1_aug15_sel [-1, ]

table2_r1_aug15_sel <-table2_r1_aug15 %>%
  select(date, hhmmss, BH_Leaf , BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =1)
table2_r1_aug15_sel <- table2_r1_aug15_sel [-1, ]


names(table1_r1_aug15_sel) <- names(table2_r1_aug15_sel )
identical(names(table1_r1_aug15_sel), names (table2_r1_aug15_sel ))

table_comb_r1_aug15 <-rbind(table1_r1_aug15_sel, table2_r1_aug15_sel)

#Aug15_r2

table1_r2_aug15 <- read.csv("data/2019-08-15-0759_BH_R2.csv", header = TRUE, sep =",", skip = 13)
table2_r2_aug15<- read.csv("data/2019-08-15-0804_round2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_aug15_sel <-table1_r2_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =2)
table1_r2_aug15_sel <- table1_r2_aug15_sel [-1, ]

table2_r2_aug15_sel <-table2_r2_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =2)
table2_r2_aug15_sel <- table2_r2_aug15_sel [-1, ]


names(table1_r2_aug15_sel) <- names(table2_r2_aug15_sel )
identical(names(table1_r2_aug15_sel), names (table2_r2_aug15_sel ))

table_comb_r2_aug15 <-rbind(table1_r2_aug15_sel, table2_r2_aug15_sel)

#Aug15_r3

table1_r3_aug15 <- read.csv("data/2019-08-15-1033_BH_R3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_aug15<- read.csv("data/2019-08-15-1042_Round 3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_aug15_sel <-table1_r3_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =3)
table1_r3_aug15_sel <- table1_r3_aug15_sel [-1, ]

table2_r3_aug15_sel <-table2_r3_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =3)
table2_r3_aug15_sel <- table2_r3_aug15_sel [-1, ]


names(table1_r3_aug15_sel) <- names(table2_r3_aug15_sel )
identical(names(table1_r3_aug15_sel), names (table2_r3_aug15_sel ))

table_comb_r3_aug15 <-rbind(table1_r3_aug15_sel, table2_r3_aug15_sel)

#Aug15_r4

table1_r4_aug15 <- read.csv("data/2019-08-15-1257_BH_R4.csv", header = TRUE, sep =",", skip = 13)
table2_r4_aug15<- read.csv("data/2019-08-15-1257_round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_aug15_sel <-table1_r4_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =4)
table1_r4_aug15_sel <- table1_r4_aug15_sel [-1, ]

table2_r4_aug15_sel <-table2_r4_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =4)
table2_r4_aug15_sel <- table2_r4_aug15_sel [-1, ]


names(table1_r4_aug15_sel) <- names(table2_r4_aug15_sel )
identical(names(table1_r4_aug15_sel), names (table2_r4_aug15_sel ))

table_comb_r4_aug15 <-rbind(table1_r4_aug15_sel, table2_r4_aug15_sel)

#Aug15_r5

table1_r5_aug15 <- read.csv("data/2019-08-15-1600_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_aug15<- read.csv("data/2019-08-15-1606_Round5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_aug15_sel <-table1_r5_aug15 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =5)
table1_r5_aug15_sel <- table1_r5_aug15_sel [-1, ]

table2_r5_aug15_sel <-table2_r5_aug15 %>%
  select(date, hhmmss, BH455_leaf , BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-15-2019", day = 227, year =2019, round =5)
table2_r5_aug15_sel <- table2_r5_aug15_sel [-1, ]


names(table1_r5_aug15_sel) <- names(table2_r5_aug15_sel )
identical(names(table1_r5_aug15_sel), names (table2_r5_aug15_sel ))

table_comb_r5_aug15 <-rbind(table1_r5_aug15_sel, table2_r5_aug15_sel)

#All round combined diurnals aug 15


names(table_comb_r1_aug15) <- names(table_comb_r2_aug15) 
names(table_comb_r2_aug15) <- names(table_comb_r3_aug15)
names(table_comb_r3_aug15) <- names(table_comb_r4_aug15)
names(table_comb_r4_aug15) <- names(table_comb_r5_aug15)


identical(names(table_comb_r1_aug15), names (table_comb_r2_aug15))
identical(names(table_comb_r2_aug15), names (table_comb_r3_aug15))
identical(names(table_comb_r3_aug15), names (table_comb_r4_aug15))
identical(names(table_comb_r4_aug15), names (table_comb_r5_aug15))

table_diurnals_aug15 <-rbind(table_comb_r1_aug15, table_comb_r2_aug15, table_comb_r3_aug15, table_comb_r4_aug15, table_comb_r5_aug15)

write.csv(table_diurnals_aug15, "data_output/table_diurnals_aug15")

####August 20 2019####

#Aug20_r1

table1_r1_aug20 <- read.csv("data/2019-08-20-0500_BH_R1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_aug20<- read.csv("data/2019-08-20-0502_logdata_BH_Round1.csv", header= TRUE, sep =",", skip = 15)

table1_r1_aug20_sel <-table1_r1_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =1)
table1_r1_aug20_sel <- table1_r1_aug20_sel [-1, ]

table2_r1_aug20_sel <-table2_r1_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =1)
table2_r1_aug20_sel <- table2_r1_aug20_sel [-1, ]


names(table1_r1_aug20_sel) <- names(table2_r1_aug20_sel )
identical(names(table1_r1_aug20_sel), names (table2_r1_aug20_sel ))

table_comb_r1_aug20 <-rbind(table1_r1_aug20_sel, table2_r1_aug20_sel)

#aug20_r2

table1_r2_aug20 <- read.csv("data/2019-08-20-0804_logdata_BH_Round2.csv", header = TRUE, sep =",", skip = 15)
table2_r2_aug20<- read.csv("data/2019-08-20-0805_BH_R2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_aug20_sel <-table1_r2_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm , Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =2)
table1_r2_aug20_sel <- table1_r2_aug20_sel [-1, ]

table2_r2_aug20_sel <-table2_r2_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =2)
table2_r2_aug20_sel <- table2_r2_aug20_sel [-1, ]


names(table1_r2_aug20_sel) <- names(table2_r2_aug20_sel )
identical(names(table1_r2_aug20_sel), names (table2_r2_aug20_sel ))

table_comb_r2_aug20 <-rbind(table1_r2_aug20_sel, table2_r2_aug20_sel)

#Aug20_r3

table1_r3_aug20 <- read.csv("data/2019-08-20-1026_BH_R3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_aug20<- read.csv("data/2019-08-20-1027_logdata_BH_Round3.csv", header= TRUE, sep =",", skip = 15)

table1_r3_aug20_sel <-table1_r3_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =3)
table1_r3_aug20_sel <- table1_r3_aug20_sel [-1, ]

table2_r3_aug20_sel <-table2_r3_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =3)
table2_r3_aug20_sel <- table2_r3_aug20_sel [-1, ]


names(table1_r3_aug20_sel) <- names(table2_r3_aug20_sel )
identical(names(table1_r3_aug20_sel), names (table2_r3_aug20_sel ))

table_comb_r3_aug20 <-rbind(table1_r3_aug20_sel, table2_r3_aug20_sel)

#Aug20_r4

table1_r4_aug20 <- read.csv("data/2019-08-20-1232_BH_R4.csv", header = TRUE, sep =",", skip = 13)
table2_r4_aug20<- read.csv("data/2019-08-20-1236_logdata_BH_Round4.csv", header= TRUE, sep =",", skip = 15)

table1_r4_aug20_sel <-table1_r4_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =4)
table1_r4_aug20_sel <- table1_r4_aug20_sel [-1, ]

table2_r4_aug20_sel <-table2_r4_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =4)
table2_r4_aug20_sel <- table2_r4_aug20_sel [-1, ]


names(table1_r4_aug20_sel) <- names(table2_r4_aug20_sel )
identical(names(table1_r4_aug20_sel), names (table2_r4_aug20_sel ))

table_comb_r4_aug20 <-rbind(table1_r4_aug20_sel, table2_r4_aug20_sel)

#Aug20_r5

table1_r5_aug20 <- read.csv("data/2019-08-20-1555_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_aug20<- read.csv("data/2019-08-20-1601_logdata_BH_Round5.csv", header= TRUE, sep =",", skip = 15)

table1_r5_aug20_sel <-table1_r5_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =5)
table1_r5_aug20_sel <- table1_r5_aug20_sel [-1, ]

table2_r5_aug20_sel <-table2_r5_aug20 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "08-20-2019", day = 232, year =2019, round =5)
table2_r5_aug20_sel <- table2_r5_aug20_sel [-1, ]


names(table1_r5_aug20_sel) <- names(table2_r5_aug20_sel )
identical(names(table1_r5_aug20_sel), names (table2_r5_aug20_sel ))

table_comb_r5_aug20 <-rbind(table1_r5_aug20_sel, table2_r5_aug20_sel)

#Combine all rounds diurnals aug 20

names(table_comb_r1_aug20) <- names(table_comb_r2_aug20) 
names(table_comb_r2_aug20) <- names(table_comb_r3_aug20)
names(table_comb_r3_aug20) <- names(table_comb_r4_aug20)
names(table_comb_r4_aug20) <- names(table_comb_r5_aug20)

identical(names(table_comb_r1_aug20), names (table_comb_r2_aug20))
identical(names(table_comb_r2_aug20), names (table_comb_r3_aug20))
identical(names(table_comb_r3_aug20), names (table_comb_r4_aug20))
identical(names(table_comb_r4_aug20), names (table_comb_r5_aug20))

table_diurnals_aug20 <-rbind(table_comb_r1_aug20, table_comb_r2_aug20, table_comb_r3_aug20, table_comb_r4_aug20, table_comb_r5_aug20)

write.csv(table_diurnals_aug20,"data_output/table_diurnals_aug20")

#Sep5_r1

table1_r1_sep5 <- read.csv("data/2019-09-05-0505_BH_Round1.csv", header = TRUE, sep =",", skip = 13)
table2_r1_sep5<- read.csv("data/2019-09-05-0509_BH_R1.csv", header= TRUE, sep =",", skip = 13)

table1_r1_sep5_sel <-table1_r1_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =1)
table1_r1_sep5_sel <- table1_r1_sep5_sel [-1, ]

table2_r1_sep5_sel <-table2_r1_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =1) %>%
  filter(!is.na(Fv.Fm))



names(table1_r1_sep5_sel) <- names(table2_r1_sep5_sel )
identical(names(table1_r1_sep5_sel), names (table2_r1_sep5_sel ))

table_comb_r1_sep5 <-rbind(table1_r1_sep5_sel, table2_r1_sep5_sel)

#Sep5_r2

table1_r2_sep5 <- read.csv("data/2019-09-05-0752_BH_Round2 (with all data).csv", header = TRUE, sep =",", skip = 53)
table2_r2_sep5<- read.csv("data/2019-09-05-0750_BH_R2.csv", header= TRUE, sep =",", skip = 53)
table3_r2_sep5<- read.csv("data/2019-09-05-0938_BH_R2_#2.csv", header= TRUE, sep =",", skip = 13)

table1_r2_sep5_sel <-table1_r2_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =2)
table1_r2_sep5_sel <- table1_r2_sep5_sel [-1, ]

table2_r2_sep5_sel <-table2_r2_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =2) %>%
  filter(!is.na(Fv.Fm))

table3_r2_sep5_sel <-table3_r2_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =2) %>%
  filter(!is.na(Fv.Fm))


names(table1_r2_sep5_sel) <- names(table2_r2_sep5_sel)
names(table2_r2_sep5_sel) <- names(table3_r2_sep5_sel)
identical(names(table1_r2_sep5_sel), names (table2_r2_sep5_sel))
identical(names(table2_r2_sep5_sel), names (table3_r2_sep5_sel))

table_comb_r2_sep5 <-rbind(table1_r2_sep5_sel, table2_r2_sep5_sel, table3_r2_sep5_sel)

#Sep5_r3

table1_r3_sep5 <- read.csv("data/2019-09-05-1028_BH_R3.csv", header = TRUE, sep =",", skip = 13)
table2_r3_sep5<- read.csv("data/2019-09-05-1035_BH_Round3.csv", header= TRUE, sep =",", skip = 13)

table1_r3_sep5_sel <-table1_r3_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =3)
table1_r3_sep5_sel <- table1_r3_sep5_sel [-1, ]

table2_r3_sep5_sel <-table2_r3_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =3) %>%
  filter(!is.na(Fv.Fm))

names(table1_r3_sep5_sel) <- names(table2_r3_sep5_sel )
identical(names(table1_r1_sep5_sel), names (table2_r1_sep5_sel ))


table_comb_r3_sep5 <-rbind(table1_r3_sep5_sel, table2_r3_sep5_sel)

#Sep5_r4

table1_r4_sep5 <- read.csv("data/2019-09-05-1247_BH_R4.csv", header = TRUE, sep =",", skip = 13)
table2_r4_sep5<- read.csv("data/2019-09-05-1250_BH_Round4.csv", header= TRUE, sep =",", skip = 13)

table1_r4_sep5_sel <-table1_r4_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =4)
table1_r4_sep5_sel <- table1_r4_sep5_sel [-1, ]

table2_r4_sep5_sel <-table2_r4_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =4)
table2_r4_sep5_sel <- table2_r4_sep5_sel [-1, ]

names(table1_r4_sep5_sel) <- names(table2_r4_sep5_sel )
identical(names(table1_r4_sep5_sel), names (table2_r4_sep5_sel ))


table_comb_r4_sep5 <-rbind(table1_r4_sep5_sel, table2_r4_sep5_sel)

#Sep5_r5

table1_r5_sep5 <- read.csv("data/2019-09-05-1549_BH_R5.csv", header = TRUE, sep =",", skip = 13)
table2_r5_sep5<- read.csv("data/2019-09-05-1552_BH_Round5.csv", header= TRUE, sep =",", skip = 13)

table1_r5_sep5_sel <-table1_r5_sep5 %>%
  select(date, hhmmss, BH455_leaf, BH455_BLOCK, BH455_VINE, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =5)
table1_r5_sep5_sel <- table1_r5_sep5_sel [-1, ]

table2_r5_sep5_sel <-table2_r5_sep5 %>%
  select(date, hhmmss, BH_Leaf, BH_Block, BH_Vine, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham) %>%
  mutate(date = "09-05-2019", day = 248, year =2019, round =5)
table2_r5_sep5_sel <- table2_r5_sep5_sel [-1, ]

names(table1_r5_sep5_sel) <- names(table2_r5_sep5_sel )
identical(names(table1_r5_sep5_sel), names (table2_r5_sep5_sel ))


table_comb_r5_sep5 <-rbind(table1_r5_sep5_sel, table2_r5_sep5_sel)


#Combine all rounds diurnals sep 5

names(table_comb_r1_sep5) <- names(table_comb_r2_sep5)
names(table_comb_r2_sep5) <- names(table_comb_r3_sep5)
names(table_comb_r3_sep5) <- names(table_comb_r4_sep5)
names(table_comb_r4_sep5) <- names(table_comb_r5_sep5)

identical(names(table_comb_r1_sep5), names (table_comb_r2_sep5))
identical(names(table_comb_r2_sep5), names (table_comb_r3_sep5))
identical(names(table_comb_r3_sep5), names (table_comb_r4_sep5))
identical(names(table_comb_r4_sep5), names (table_comb_r5_sep5))

names(table_comb_r1_sep5) <- names(table_comb_r2_sep5)
names(table_comb_r2_sep5) <- names(table_comb_r3_sep5)
names(table_comb_r3_sep5) <- names(table_comb_r4_sep5)
names(table_comb_r4_sep5) <- names(table_comb_r5_sep5)

identical(names(table_comb_r1_sep5), names (table_comb_r2_sep5))
identical(names(table_comb_r2_sep5), names (table_comb_r3_sep5))
identical(names(table_comb_r3_sep5), names (table_comb_r4_sep5))
identical(names(table_comb_r4_sep5), names (table_comb_r5_sep5))

table_diurnals_sep5 <-rbind(table_comb_r1_sep5, table_comb_r2_sep5, table_comb_r3_sep5, table_comb_r4_sep5, table_comb_r5_sep5)

write.csv(table_diurnals_sep5,"data_output/table_diurnals_sep5")

#Combine all diurnals new blocks

diurnals_Jul25 <-read.csv("data_output/table_diurnals_jul25", header = TRUE, sep = ",")

diurnals_Jul28 <-read.csv("data_output/table_diurnal_jul28", header = TRUE, sep = ",")

diurnals_aug1 <-read.csv("data_output/table_diurnals_aug1", header = TRUE, sep = ",")

diurnals_aug15 <-read.csv("data_output/table_diurnals_aug15", header = TRUE, sep = ",")

diurnals_aug20 <-read.csv("data_output/table_diurnals_aug20", header = TRUE, sep = ",")

diurnals_sep5 <-read.csv("data_output/table_diurnals_sep5", header = TRUE, sep = ",")


######Repeat a few times until names identical all TRUE

names(diurnals_Jul25) <- names(diurnals_Jul28) 
names(diurnals_Jul28) <- names(diurnals_aug1)
names(diurnals_aug1) <- names(diurnals_aug15)
names(diurnals_aug15) <- names(diurnals_aug20)
names(diurnals_aug20) <- names(diurnals_sep5)

identical(names(diurnals_Jul25), names (diurnals_Jul28))
identical(names(diurnals_Jul28), names (diurnals_aug1))
identical(names(diurnals_aug1), names (diurnals_aug15))
identical(names(diurnals_aug15), names (diurnals_aug20))
identical(names(diurnals_aug20), names (diurnals_sep5))

######Repeat a few times until names identical all TRUE


diurnals_after_blockid_change <- rbind(diurnals_Jul25, diurnals_Jul28, diurnals_aug1, diurnals_aug15, diurnals_aug20, diurnals_sep5)


diurnals_after_blockid_change <- diurnals_after_blockid_change%>%
  mutate(pixel_number = case_when(
    BH_Block == "B1_R2" ~ 34,
    BH_Block == "B1_R3" ~ 55,
    BH_Block == "B1_R4" ~ 67, 
    BH_Block == "B2_R1" ~ 23,
    BH_Block == "B2_R2" ~ 35,
    BH_Block == "B2_R3" ~ 48,
    BH_Block == "B3_R1" ~ 27,
    BH_Block == "B3_R2" ~ 45,
    BH_Block == "B3_R3" ~ 54
  )) %>%
  mutate( treatment= case_when(
    BH_Block == "B1_R2" ~ 1,
    BH_Block == "B1_R3" ~ 1,
    BH_Block == "B1_R4" ~ 1, 
    BH_Block == "B2_R1" ~ 2,
    BH_Block == "B2_R2" ~ 2,
    BH_Block == "B2_R3" ~ 2,
    BH_Block == "B3_R1" ~ 3,
    BH_Block == "B3_R2" ~ 3,
    BH_Block == "B3_R3" ~ 3))%>%
  mutate(Rep= case_when(
    BH_Block == "B1_R2" ~ 2,
    BH_Block == "B1_R3" ~ 3,
    BH_Block == "B1_R4" ~ 1, 
    BH_Block == "B2_R1" ~ 1,
    BH_Block == "B2_R2" ~ 2,
    BH_Block == "B2_R3" ~ 3,
    BH_Block == "B3_R1" ~ 1,
    BH_Block == "B3_R2" ~ 2,
    BH_Block == "B3_R3" ~ 3))

write.csv(diurnals_after_blockid_change,"data_output/diurnals_after_blockid_change.csv")


##### Merge water potentials and LI-COR dataframes 2019 #####

water_potentials_after_blockid_change <- read.csv("data/Water_potentials_2019.csv", header = TRUE, sep = ",")
diurnals_after_blockid_change_w_pixels <- read.csv("data_output/diurnals_after_blockid_change.csv", header = TRUE, sep =",")

water_potentials_after_blockid_change$date<- mdy(water_potentials_after_blockid_change$date)
diurnals_after_blockid_change_w_pixels$date<-mdy(diurnals_after_blockid_change_w_pixels$date)
str(water_potentials_after_blockid_change$date)
str(diurnals_after_blockid_change_w_pixels$date)


common_col_names <- intersect(names(diurnals_after_blockid_change_w_pixels), names(water_potentials_after_blockid_change))

diurnals_wp_ge_data_new_blocks_2019 <- merge(diurnals_after_blockid_change_w_pixels, water_potentials_after_blockid_change, by=common_col_names, all =TRUE)

write.csv(diurnals_wp_ge_data_new_blocks_2019, "data_output/diurnals_wp_ge_data_new_blocks_2019")

diurnals_wp_ge_data_new_blocks_2019_final <-diurnals_wp_ge_data_new_blocks_2019 %>%
  select(date, year, day, hhmmss, pixel_number, BH_Block, BH_Vine, BH_Leaf, round, E, A, Ci, gsw, Fv.Fm,Tleaf, Tair, VPDleaf, RHcham, Leaf_wp_bar, leaf_temp_C, Stem_wp_bar, Leak, wrong_vine, treatment, Rep )

write.csv(diurnals_wp_ge_data_new_blocks_2019_final,"data_output/diurnals_wp_ge_data_new_blocks_2019_final.csv")

str(diurnals_wp_ge_data_new_blocks_2019_final$hhmmss)

#Combine gas exchange and water potential data frames old blocks

water_potentials_before_blockid_change <- read.csv("data/Water_potentials_2019_old_blocks.csv", header = TRUE, sep = ",")

water_potentials_before_blockid_change <- water_potentials_before_blockid_change%>%
  mutate(pixel_number = case_when(
    BH_Block == "B1_R2" ~ 45,
    BH_Block == "B1_R3" ~ 55,
    BH_Block == "B1_R1" ~ 35, 
    BH_Block == "B2_R1" ~ 13,
    BH_Block == "B2_R2" ~ 27,
    BH_Block == "B2_R3" ~ 75,
    BH_Block == "B3_R1" ~ 23,
    BH_Block == "B3_R2" ~ 78,
    BH_Block == "B3_R3" ~ 48,
  ))%>%
  mutate( treatment= case_when(
    pixel_number == 55 ~ 1,
    pixel_number == 13 ~ 1,
    pixel_number == 48 ~ 2,
    pixel_number == 35 ~ 2,
    pixel_number == 23 ~ 2,
    pixel_number == 78 ~ 2,
    pixel_number == 45 ~ 3,
    pixel_number == 27 ~ 3,
    pixel_number == 75 ~ 3,))%>%
  mutate(Rep= case_when(
    pixel_number == 55 ~ 1,
    pixel_number == 13 ~ 2,
    pixel_number == 48 ~ 1,
    pixel_number == 35 ~ 2,
    pixel_number == 23 ~ 3,
    pixel_number == 78 ~ 4,
    pixel_number == 45 ~ 1,
    pixel_number == 27 ~ 2,
    pixel_number == 75 ~ 3,))%>%
  filter(!pixel_number == 78)



water_potentials_before_blockid_change$date <- as.Date(paste("2019", water_potentials_before_blockid_change$day), format = "%Y %j")


str(water_potentials_before_blockid_change)

diurnals_before_blockid_change_w_pixels$date<-mdy(diurnals_before_blockid_change_w_pixels$date)
str(diurnals_before_blockid_change_w_pixels$date)

str(diurnals_before_blockid_change_w_pixels_new)

common_col_names <- intersect(names(diurnals_before_blockid_change_w_pixels), names(water_potentials_before_blockid_change))

diurnals_wp_ge_data_old_blocks_2019 <- merge(diurnals_before_blockid_change_w_pixels, water_potentials_before_blockid_change, by=common_col_names, all =TRUE)

write.csv(diurnals_wp_ge_data_old_blocks_2019, "data_output/diurnals_wp_ge_data_old_blocks_2019.csv")


str(water_potentials_before_blockid_change)

diurnals_wp_ge_data_old_blocks_2019_final <-diurnals_wp_ge_data_old_blocks_2019 %>%
  select(date, year, day, hhmmss, pixel_number, BH_Block, BH_Vine, BH_Leaf, round, E, A, Ci, gsw, Fv.Fm,Tleaf, Tair, VPDleaf, RHcham, Leaf_wp_bar, Stem_wp_bar, Leak, wrong_vine,treatment, Rep)

diurnals_wp_ge_data_old_blocks_2019_final_tally<-diurnals_wp_ge_data_old_blocks_2019_final%>%
  group_by(date, treatment, round)%>%
  tally()

write.csv(diurnals_wp_ge_data_old_blocks_2019_final,"data_output/diurnals_wp_ge_data_old_blocks_2019_final.csv")

str(diurnals_wp_ge_data_old_blocks_2019$hhmmss)

#Merge old blocks and new blocks diurnals 

diurnals_wp_ge_data_old_blocks_2019_to_combine <- read.csv("data_output/diurnals_wp_ge_data_old_blocks_2019_final.csv", header = TRUE, sep = ",")

diurnals_wp_ge_data_new_blocks_2019_to_combine <- read.csv("data_output/diurnals_wp_ge_data_new_blocks_2019_final.csv", header = TRUE, sep =",")

common_col_names <- intersect(names(diurnals_wp_ge_data_old_blocks_2019_to_combine ), names(diurnals_wp_ge_data_new_blocks_2019_to_combine ))

diurnals_2019_old_and_new_blocks<- merge(diurnals_wp_ge_data_old_blocks_2019_to_combine , diurnals_wp_ge_data_new_blocks_2019_to_combine , by=common_col_names, all =TRUE)

write.csv(diurnals_2019_old_and_new_blocks, "data_output/diurnals_2019_old_and_new_blocks.csv")

library(tidyverse)
diurnals_2019_old_and_new_blocks <-read.csv("data_output/diurnals_2019_old_and_new_blocks.csv", header = TRUE)

diurnals_2019_old_and_new_blocks_cleaned <- diurnals_2019_old_and_new_blocks%>%
  filter(!is.na(A))

write.csv(diurnals_2019_old_and_new_blocks_cleaned, "data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv")

diurnals_2019_old_and_new_blocks_cleaned_tally<-diurnals_2019_old_and_new_blocks_cleaned%>%
  group_by(date, treatment, round)%>%
  tally()



