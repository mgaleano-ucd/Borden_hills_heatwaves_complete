####2019 data
diurnals_borden_hills_2019 <-read.csv("data_output/diurnals_2019_old_and_new_blocks_cleaned_no_NAs.csv", header = TRUE)

diurnals_borden_hills_2019 <-diurnals_borden_hills_2019%>%
  filter(!pixel_number == 34 )%>%
  select(-date)

str(diurnals_borden_hills_2019)

diurnals_borden_hills_2019$date <- as.Date(paste(diurnals_borden_hills_2019$day, "2019"), format = " %j %Y")

str(diurnals_borden_hills_2019$date)

diurnals_borden_hills_2019 <- diurnals_borden_hills_2019 %>%
  mutate(time = hhmmss) %>%
  select(-hhmmss) %>%
  filter(!is.na(time))
str(diurnals_borden_hills_2019$time)

diurnals_borden_hills_2019$time<- hms(diurnals_borden_hills_2019$time)

str(diurnals_borden_hills_2019$time)

diurnals_borden_hills_2019$datetime <- paste(diurnals_borden_hills_2019$date, " ", diurnals_borden_hills_2019$time, sep = "")

str(diurnals_borden_hills_2019$datetime)

diurnals_borden_hills_2019$datetime <- ymd_hms(diurnals_borden_hills_2019$datetime, tz = "UTC")


str(diurnals_borden_hills_2019)

diurnals_borden_hills_2019<-diurnals_borden_hills_2019%>%
  mutate(leaf_temp_IRT = leaf_temp_C)%>%
  mutate(Stem_wp_MPa = (Stem_wp_bar*0.1)*-1) %>%
  mutate(Leaf_wp_MPa = (Leaf_wp_bar*0.1 )*-1) %>%
  select(- X.2, -X.1, -X, -leaf_temp_C, -Leaf_wp_bar,- Stem_wp_bar)

diurnals_borden_hills_2019$leaf_temp_IRT<-as.numeric(diurnals_borden_hills_2019$leaf_temp_IRT)

str(diurnals_borden_hills_2019$leaf_temp_IRT)

diurnals_borden_hills_2019_July_PreHW_and_HW<-diurnals_borden_hills_2019%>%
  filter(date == "2019-07-25" | date == "2019-07-28")

write.csv(diurnals_borden_hills_2019_July_PreHW_and_HW,"data_output/diurnals_borden_hills_2019_July_PreHW_and_HW.csv")


diurnals_borden_hills_2019_July_PreHW_and_HW%>%
  group_by(date, round)%>%
  tally()

#### 2020 data ######

diurnals_borden_hills_2020 <-read.csv("data_output/data_physiology_all_complete_BH_2020.csv", header = TRUE)

str(diurnals_borden_hills_2020)

diurnals_borden_hills_2020 <- diurnals_borden_hills_2020 %>%
  mutate(time = hhmmss)%>%
  select(-hhmmss)

diurnals_borden_hills_2020$time<-hms(diurnals_borden_hills_2020$time)
diurnals_borden_hills_2020$date<- ymd(diurnals_borden_hills_2020$date)

str(diurnals_borden_hills_2020$date)
str(diurnals_borden_hills_2020$time)

diurnals_borden_hills_2020$datetime <- paste(diurnals_borden_hills_2020$date, " ", diurnals_borden_hills_2020$time, sep = "")

str(diurnals_borden_hills_2020$datetime)

sum(is.na(diurnals_borden_hills_2020$datetime))
sum(is.na(diurnals_borden_hills_2020$time))

rows_with_na_time <- diurnals_borden_hills_2020 %>%
  filter(is.na(time))

str(diurnals_borden_hills_2020)

diurnals_borden_hills_2020$datetime <- ymd_hms(diurnals_borden_hills_2020$datetime,  tz = "UTC")
str(diurnals_borden_hills_2020$datetime)

column_names <- colnames(diurnals_borden_hills_2019)
column_names

str(diurnals_borden_hills_2020)
diurnals_borden_hills_2020 <- diurnals_borden_hills_2020 %>%
  mutate(
    Leaf_wp_MPa = leaf_wp_MPa,
    Stem_wp_MPa = stem_wp_MPa,
    leaf_temp_IRT = leaf_temp,
    BH_Block = BLOCK,
    BH_Vine = VINE,
    BH_Leaf = LEAF,
    BH_SUB_BLOCK = SUB_BLOCK
  ) %>%
  select(
    year, day, pixel_number, BH_Block, BH_Vine, BH_SUB_BLOCK, BH_Leaf,
    round, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham,
    Leaf_wp_MPa, Stem_wp_MPa, Leak, wrong_vine, treatment, date, time, datetime, leaf_temp_IRT
  )


diurnals_borden_hills_2020_July_PreHW_and_HW<- diurnals_borden_hills_2020%>% filter(date == "2020-07-10" | date =="2020-07-12")

diurnals_borden_hills_2020_July_PreHW_and_HW%>%
  group_by(date, round)%>%
  tally()

write.csv(diurnals_borden_hills_2020_July_PreHW_and_HW,"data_output/diurnals_borden_hills_2020_July_PreHW_and_HW.csv")
###### 2021 data #####

data_physiology_complete_BH_2021_middays<-read.csv("data_output/data_physiology_complete_BH_2021_middays.csv", header = TRUE)


data_physiology_complete_BH_2021_middays$date<- ymd(data_physiology_complete_BH_2021_middays$date)
str(data_physiology_complete_BH_2021_middays$date)



data_physiology_complete_BH_2021_middays <- data_physiology_complete_BH_2021_middays %>%
  mutate(time = hhmmss)%>%
  select(-hhmmss)

data_physiology_complete_BH_2021_middays$time<-hms(data_physiology_complete_BH_2021_middays$time)
data_physiology_complete_BH_2021_middays$date<- ymd(data_physiology_complete_BH_2021_middays$date)

str(data_physiology_complete_BH_2021_middays$date)
str(data_physiology_complete_BH_2021_middays$time)

data_physiology_complete_BH_2021_middays$datetime <- paste(data_physiology_complete_BH_2021_middays$date, " ", data_physiology_complete_BH_2021_middays$time, sep = "")

str(data_physiology_complete_BH_2021_middays$datetime)

sum(is.na(data_physiology_complete_BH_2021_middays$datetime))
sum(is.na(data_physiology_complete_BH_2021_middays$time))

rows_with_na_time <- data_physiology_complete_BH_2021_middays %>%
  filter(is.na(time))

str(data_physiology_complete_BH_2021_middays)

data_physiology_complete_BH_2021_middays$datetime <- ymd_hms(data_physiology_complete_BH_2021_middays$datetime,  tz = "UTC")
str(data_physiology_complete_BH_2021_middays$datetime)



data_physiology_complete_BH_2021_middays <- data_physiology_complete_BH_2021_middays %>%
  mutate(
    Leaf_wp_MPa = leaf_wp_MPa,
    Stem_wp_MPa = stem_wp_MPa,
    leaf_temp_IRT = leaf_temp,
    BH_Block = BLOCK,
    BH_Vine = VINE,
    BH_Leaf = LEAF,
    BH_SUB_BLOCK = SUB_BLOCK
  ) %>%
  select(
    year, day, pixel_number, BH_Block, BH_Vine, BH_SUB_BLOCK, BH_Leaf,
    round, E, A, Ci, gsw, Fv.Fm, Tleaf, Tair, VPDleaf, RHcham,
    Leaf_wp_MPa, Stem_wp_MPa, Leak, wrong_vine, treatment, date, time, datetime, leaf_temp_IRT
  )


data_physiology_complete_BH_2021_middays_July_PreHW_and_HW<- data_physiology_complete_BH_2021_middays%>% filter(date == "2021-07-07" | date =="2021-07-10")

data_physiology_complete_BH_2021_middays_July_PreHW_and_HW%>%
  group_by(date)%>%
  tally()

write.csv(data_physiology_complete_BH_2021_middays_July_PreHW_and_HW,"data_output/data_physiology_complete_BH_2021_middays_July_PreHW_and_HW.csv")