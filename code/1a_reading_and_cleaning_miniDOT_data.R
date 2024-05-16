#### miniDOT data aggregation and cleaning
### Jordan Zabrecky
## last edited: 05.16.2024

# This code pulls data from miniDOT text files and converts them into csv's.
# Additionally this code adjusts the sensor time offset from PST, removes time when
# sensor was out of water, and cleans outlying values

#### Loading packages and reading in data #### 

## loading necessary packages
lapply(c("tidyverse","lubridate","data.table","here"),
       require, character.only = T)

## Reading in data

# creating header list for csv to be produced
header_list <- c("Time_Sec", "BV_Volts", "Temp_C", "DO_mgL", "Q")

# function to load in all files from a folder
create_df <- function(source_path) {
  list.files(path = here(source_path),
             pattern = "*.txt",
             full.names = T) %>% 
    map_df(~read_csv(., skip = 3, col_names = header_list))
}

# function that convert times to appropriate timezone
time_fix <- function(flnm) {
  flnm$date_time <- as_datetime(flnm$Time_Sec, tz = "America/Los_Angeles")
  flnm %>% 
    dplyr::select(date_time, BV_Volts, Temp_C, DO_mgL, Q)
}

## Reading miniDOT data for each river site, making a data frame,
## and then applying a function to convert time into PST

# save directory path info
path <- getwd()

## 2022

# south fork eel @ miranda
sfkeel_mir_2022 <- create_df("~/metabolism-norcal-2022-23/data/miniDOT/
                             2022_raw_data/Miranda_663402")
sfkeel_mir_2022 <- time_fix(sfkeel_mir_2022)

# russian 
russian_2022 <- create_df("2022_raw_data/Russian_491496")
russian_2022 <- time_fix(russian_2022)

# salmon
salmon_2022 <- create_df(paste(path,"/data/miniDOT/2022_raw_data/Salmon_521120", sep = ""))
salmon_2022 <- time_fix(salmon_2022)

## 2023

# south fork eel @ miranda
sfkeel_mir_2023 <- create_df("~/metabolism-norcal-22-23/data/miniDOT/2023_raw_data/Miranda_663402")
sfkeel_mir_2023 <- time_fix(sfkeel_mir_2023)

# south fork eel @ standish hickey
sfkeel_sth_2023 <- create_df("data/miniDOT/2023 raw data/Standish_Hickey_521120")
sfkeel_sth_2023 <- time_fix(sfkeel_sth_2023)

# salmon
salmon_2023 <- create_df(paste(path,"/data/miniDOT/2023_raw_data/Salmon_529728", sep = ""))
salmon_2023 <- time_fix(salmon_2023)

#### Initial data cleaning ####
### (1. quality check, 2. time offsets, 3. removing maintenance periods, etc.) ###

## (1) remove sensor data where Q < 0.7 
# (PME [creator of miniDOTs] states Q > 0.7 indicates the miniDOT is operating in good condition)

# create function to remove values where Q < 0.7
quality_check <- function(miniDOT) {
  miniDOT %>% 
    subset(Q > 0.7)
}

# applying quality_check function to all dataframes

sfkeel_mir_2022_cleaning <- quality_check(sfkeel_mir_2022)
russian_2022_cleaning <- quality_check(russian_2022)
salmon_2022_cleaning <- quality_check(salmon_2022)
sfkeel_mir_2023_cleaning <- quality_check(sfkeel_mir_2023)
sfkeel_sth_2023_cleaning <- quality_check(sfkeel_sth_2023)
salmon_2023_cleaning <- quality_check(salmon_2023)
# turns out all of our data is Q > 0.7!

## (2) applying time offsets in accordance to screenshots of miniDOT time vs. actual PST time
# taken prior to launch

# south fork eel @ miranda 2022: 09:30:11 should be 09:33:58
sfkeel_mir_2022_cleaning$date_time <- sfkeel_mir_2022_cleaning$date_time + (60*3) + 47

# russian 2022: 09:14:14 should be 09:15:17
russian_2022_cleaning$date_time <- russian_2022_cleaning$date_time + (60*1) + 3

# salmon 2022: 08:45:00 should be 08:48:39
salmon_2022_cleaning$date_time <- salmon_2022_cleaning$date_time + (60*3) + 39

# south fork eel @ miranda 2023: 16:21:38 should be 16:25:43
sfkeel_mir_2023_cleaning$date_time <- sfkeel_mir_2023_cleaning$date_time + (60*4) + 5

# south fork eel @ standish hickey 2023: 16:38:00 should be 16:42:25
sfkeel_sth_2023_cleaning$date_time <- sfkeel_sth_2023_cleaning$date_time + (60*4) + 25

# salmon 2023: 16:23:38 should be 16:23:25
salmon_2023_cleaning$date_time <- salmon_2023_cleaning$date_time - 13

## (3) removing sensor maintenance and data download times

# this is done by removing recorded times from field notes
# but also using 'dygraphs' package in another script to make sure those times are correct
# this is found in another script "1b_visualizing_DO_data_cleaning.R"

# south fork eel @ miranda 2022
sfkeel_mir_2022_cleaning <- sfkeel_mir_2022_cleaning %>% 
  filter(date_time >= "2022-06-29 10:40:00") %>%  # initial launch time
  filter(date_time <= "2022-09-17 9:03:00") # retrieval time
# all maintenance times removed below
sfkeel_mir_2022_cleaning <- sfkeel_mir_2022_cleaning %>%
  filter(date_time <= "2022-07-14 9:15:00" | date_time >= "2022-07-14 9:20:00") %>% 
  filter(date_time <= "2022-07-28 9:20:00" | date_time >= "2022-07-28 10:10:00") %>% 
  filter(date_time <= "2022-08-10 8:55:00" | date_time >= "2022-08-10 9:25:00") %>% 
  filter(date_time <= "2022-08-23 9:30:00" | date_time >= "2022-08-23 11:25:00") %>% 
  filter(date_time <= "2022-09-06 9:15:00" | date_time >= "2022-09-06 10:25:00")

# russian 2022
russian_2022_cleaning <- russian_2022_cleaning %>% 
  filter(date_time >= "2022-06-24 17:15:00")  # initial launch time
  # no retrieval time because sensor was stolen :(
# all maintenance times removed below (and some weirdness right before pick-up)
russian_2022_cleaning <- russian_2022_cleaning %>% 
  filter(date_time <= "2022-07-06 9:30:00" | date_time >= "2022-07-06 10:30:00") %>% 
  filter(date_time <= "2022-07-20 6:45:00" | date_time >= "2022-07-20 9:55:00") %>% 
  filter(date_time <= "2022-08-02 8:15:00" | date_time >= "2022-08-02 10:10:00") %>% 
  filter(date_time <= "2022-08-17 7:50:00" | date_time >= "2022-08-17 10:15:00") %>% 
  # last maintenance time with no end date because sensor was stolen :(
  filter(date_time <= "2022-09-01 9:30:00")

# salmon 2022
salmon_2022_cleaning <- salmon_2022_cleaning %>% 
  filter(date_time >= "2022-06-26 19:00:00") %>%  # initial launch time
  filter(date_time <= "2022-09-21 17:50:00") # retrieval time
# all maintenance times removed below
salmon_2022_cleaning <- salmon_2022_cleaning %>% 
  filter(date_time <= "2022-07-11 18:45:00" | date_time >= "2022-07-11 19:45:00") %>%
  filter(date_time <= "2022-07-25 19:25:00" | date_time >= "2022-07-25 20:20:00")

# south fork eel @ miranda 2023
sfkeel_mir_2023_cleaning <- sfkeel_mir_2023_cleaning %>%
  filter(date_time >= "2023-06-18 11:30:00") %>%  # initial launch time
  filter(date_time <= "2023-09-27 11:25:00") # retrieval time
# all maintenance times removed below
sfkeel_mir_2023_cleaning <- sfkeel_mir_2023_cleaning %>% 
  filter(date_time <= "2023-06-25 10:45:00" | date_time >= "2023-06-25 11:55:00") %>% 
  filter(date_time <= "2023-07-03 11:55:00" | date_time >= "2023-07-03 12:30:00") %>% 
  filter(date_time <= "2023-07-10 9:20:00" | date_time >= "2023-07-10 9:50:00") %>% 
  filter(date_time <= "2023-07-17 11:26:00" | date_time >= "2023-07-17 12:05:00") %>% 
  filter(date_time <= "2023-07-24 10:56:00" | date_time >= "2023-07-24 11:45:00") %>% 
  filter(date_time <= "2023-07-31 10:26:00" | date_time >= "2023-07-31 10:57:00") %>% 
  filter(date_time <= "2023-08-07 10:25:00" | date_time >= "2023-08-07 10:55:00") %>% 
  filter(date_time <= "2023-08-14 10:46:00" | date_time >= "2023-08-14 11:20:00") %>% 
  filter(date_time <= "2023-08-22 10:42:00" | date_time >= "2023-08-22 11:15:00") %>% 
  filter(date_time <= "2023-08-28 10:48:00" | date_time >= "2023-08-28 11:19:00") %>% 
  filter(date_time <= "2023-09-04 10:28:00" | date_time >= "2023-09-04 10:45:00") %>% 
  filter(date_time <= "2023-09-12 10:30:00" | date_time >= "2023-09-12 11:05:00") %>% 
  filter(date_time <= "2023-09-18 11:09:00" | date_time >= "2023-09-18 11:49:00")

# south fork eel @ standish hickey 2023
sfkeel_sth_2023_cleaning <- sfkeel_sth_2023_cleaning %>%
  filter(date_time >= "2023-06-24 9:45:00") %>%  # initial launch time
  filter(date_time <= "2023-09-27 10:00:00") # retrieval time
# all maintenance times removed below
sfkeel_sth_2023_cleaning <- sfkeel_sth_2023_cleaning %>% 
  filter(date_time <= "2023-06-25 18:10:00" | date_time >= "2023-06-25 18:15:00") %>% # moved brick further out
  filter(date_time <= "2023-07-03 8:35:00" | date_time >= "2023-07-03 9:20:00") %>% 
  filter(date_time <= "2023-07-11 9:35:00" | date_time >= "2023-07-11 10:00:00") %>% 
  filter(date_time <= "2023-07-17 8:34:00" | date_time >= "2023-07-17 9:10:00") %>% 
  filter(date_time <= "2023-07-24 18:42:00" | date_time >= "2023-07-24 19:07:00") %>% 
  filter(date_time <= "2023-07-31 8:01:00" | date_time >= "2023-07-31 8:31:00") %>% 
  filter(date_time <= "2023-08-07 7:45:00" | date_time >= "2023-08-07 8:20:00") %>% 
  filter(date_time <= "2023-08-14 8:09:00" | date_time >= "2023-08-14 8:29:00") %>% 
  filter(date_time <= "2023-08-22 8:00:00" | date_time >= "2023-08-22 8:50:00") %>% 
  filter(date_time <= "2023-08-24 11:08:00" | date_time >= "2023-08-24 11:10:00") %>%  # toggled w/ brick to redeploy HOBO
  filter(date_time <= "2023-08-28 8:37:00" | date_time >= "2023-08-28 8:58:00") %>% 
  filter(date_time <= "2023-09-04 8:34:00" | date_time >= "2023-09-04 8:50:00") %>% 
  filter(date_time <= "2023-09-12 8:20:00" | date_time >= "2023-09-12 8:40:00") %>% 
  filter(date_time <= "2023-09-18 8:50:00" | date_time >= "2023-09-18 9:40:00")

# salmon 2023
salmon_2023_cleaning <- salmon_2023_cleaning %>% 
  filter(date_time >= "2023-06-27 19:35:00") %>% 
  filter(date_time <= "2023-09-27 11:30:00") # did not retrieve until november, 
                                             # so just using end of our field season
# all maintenance times removed below
salmon_2023_cleaning <- salmon_2023_cleaning %>% 
  filter(date_time <= "2023-07-12 18:00:00" | date_time >= "2023-07-12 19:25:00") %>% 
  filter(date_time <= "2023-07-26 18:49:00" | date_time >= "2023-07-26 19:35:00") %>% 
  filter(date_time <= "2023-08-09 18:25:00" | date_time >= "2023-08-09 19:05:00")

#### Final data cleaning ####
### (removing outliers, interpolating small amounts of missing data, removing large amounts of bad data)

# still using 'dygraphs' package in "1b_visualizing_DO_data_cleaning.R" 
# to visualize data and cleaning

# since for the most part our temperature looks good, we will try to preserve that data
# DO will then be cleaned on a separate object "..._cleaning_DO"
# DO data missing for <=10 hours (set limit!) within linear periods will then be interpolated

## (1) removing minor outliers

# salmon 2022
salmon_2022_cleaning_DO <- salmon_2022_cleaning %>% 
  filter(date_time <= "2022-07-05 13:40:00" | date_time >= "2022-07-05 18:15:00") %>%  # <5 hours
  filter(date_time <= "2022-07-28 6:40:00" | date_time >= "2022-07-28 8:15:00") # <2 hours

# salmon 2023 
salmon_2023_cleaning_DO <- salmon_2023_cleaning %>% 
  filter(date_time <= "2023-07-18 04:26:00" | date_time >= "2023-07-18 10:01:00") %>%  # <6 hours
  filter(date_time <= "2023-07-21 20:15:00" | date_time >= "2023-07-21 23:10:00") %>%  # <2 hours
  filter(date_time <= "2023-07-22 10:55:00" | date_time >= "2023-07-22 11:20:00") %>%  # <1 hour
  filter(date_time <= "2023-07-25 23:20:00" | date_time >= "2023-07-25 23:40:00") %>% # <1 hour
  filter(date_time <= "2023-08-20 4:13:00" | date_time >= "2023-08-20 6:07:00") %>% # <2 hours
  filter(date_time <= "2023-08-20 12:33:00" | date_time >= "2023-08-20 13:30:00") # <1 hour

## (2) interpolating missing data using na.approx

salmon_2022_cleaning_DO$DO_mgL <- na.approx(salmon_2022_cleaning_DO$DO_mgL)
salmon_2023_cleaning_DO$DO_mgL <- na.approx(salmon_2023_cleaning_DO$DO_mgL)

## (3) removing longer periods of biofouling, bad data, etc. that cannot be interpolated

# salmon 2022
# removing obvious biofouling visible by amplitude increases that go away after
# sensor maintenance; unlike the Russian, it's much harder to distinguish the biofouling here
# from true DO, so unfortunately we have to remove those days
salmon_2022_cleaning_DO <- salmon_2022_cleaning_DO %>% 
  filter(date_time <= "2022-07-8 11:50:00" | date_time >= "2022-07-11 19:00:00") %>% 
  filter(date_time <= "2022-07-18 13:40:00" | date_time >= "2022-07-25 19:00:00")

# salmon 2023
# removing weirdness (super fuzzy with low troughs) 7/19 and 7/22 to 7/25
# note that when we came to sensor on 7/26 it had been moved to a weird position
salmon_2023_cleaning_DO <- salmon_2023_cleaning_DO %>% 
  filter(date_time <= "2023-07-19 09:00:00" | date_time >= "2023-07-20 02:20:00") %>% 
  filter(date_time <= "2023-07-22 20:45:00" | date_time >= "2023-07-25 8:25:00")

##------------old

## south fork eel @ miranda 2022

# removing time where egg sac had been laid directly on sensor foil
# observed at 7/28/2022 pickup -> data all from ~50 hours beforehand seems wonky
# and strangeness before 7/14/2022 pickup with large oscillations on 5-min intervals
sfkeel_mir_2022_cleaning <- sfkeel_mir_2022_cleaning %>%
  filter(date_time <= "2022-07-25 10:05:00" | date_time >= "2022-07-28 9:20:00") %>% 
  filter(date_time <= "2022-07-13 19:35:00" | date_time >= "2022-07-14 9:15:00")
# removing some later misc. DO outliers; all removed periods <10 hours
sfkeel_mir_2022_cleaning_DO <- sfkeel_mir_2022_cleaning %>% 
  filter(date_time <= "2022-08-18 23:45:00" | date_time >= "2022-08-18 23:50:00") %>% 
  filter(date_time <= "2022-08-21 12:05:00" | date_time >= "2022-08-21 12:45:00") %>% 
  filter(date_time <= "2022-08-22 11:20:00" | date_time >= "2022-08-22 14:00:00") %>%
  filter(date_time <= "2022-08-28 06:20:00" | date_time >= "2022-08-28 13:15:00") %>% 
  filter(date_time <= "2022-08-30 03:20:00" | date_time >= "2022-08-30 06:20:00") %>%
  filter(date_time <= "2022-09-01 3:35:00" | date_time >= "2022-09-01 6:13:00") %>% 
  filter(date_time <= "2022-09-01 22:50:00" | date_time >= "2022-09-01 23:30:00") %>% 
  filter(date_time <= "2022-09-02 13:35:00" | date_time >= "2022-09-02 15:10:00") %>%
  filter(date_time <= "2022-09-03 10:50:00" | date_time >= "2022-09-03 11:40:00") %>% 
  filter(date_time <= "2022-09-04 14:25:00" | date_time >= "2022-09-04 14:50:00") %>% 
  filter(date_time <= "2022-09-04 18:40:00" | date_time >= "2022-09-04 19:25:00") %>%
  filter(date_time <= "2022-09-05 3:50:00" | date_time >= "2022-09-05 8:10:00") %>% 
  filter(date_time <= "2022-09-05 8:35:00" | date_time >= "2022-09-05 10:45:00") %>%
  filter(date_time <= "2022-09-05 12:30:00" | date_time >= "2022-09-05 14:45:00") %>%
  filter(date_time <= "2022-09-08 00:25:00" | date_time >= "2022-09-08 2:10:00") %>% 
  filter(date_time <= "2022-09-11 5:10:00" | date_time >= "2022-09-11 6:30:00") %>% 
  filter(date_time <= "2022-09-12 6:45:00" | date_time >= "2022-09-12 7:05:00")

## russian 2022

# removing misc. outliers and biofouling (that is removable)
# biofouling is identifiable via DO amplitude increases that disappear after
# sensor cleaning/maintenance; all removed periods <7 hours
russian_2022_cleaning_DO <- russian_2022_cleaning %>% 
  filter(date_time <= "2022-07-10 2:20:00" | date_time >= "2022-07-10 2:40:00") %>% 
  filter(date_time <= "2022-08-13 9:45:00" | date_time >= "2022-08-13 12:25:00") %>% 
  filter(date_time <= "2022-08-14 8:15:00" | date_time >= "2022-08-14 12:35:00") %>% 
  filter(date_time <= "2022-08-15 8:45:00" | date_time >= "2022-08-15 13:20:00") %>% 
  filter(date_time <= "2022-08-16 8:00:00" | date_time >= "2022-08-16 13:35:00") %>% 
  filter(date_time <= "2022-08-20 10:40:00" | date_time >= "2022-08-20 14:00:00") %>%
  filter(date_time <= "2022-08-21 10:20:00" | date_time >= "2022-08-21 13:10:00") %>%
  filter(date_time <= "2022-08-22 9:15:00" | date_time >= "2022-08-22 13:30:00") %>% 
  filter(date_time <= "2022-08-23 8:55:00" | date_time >= "2022-08-23 13:15:00") %>%
  filter(date_time <= "2022-08-24 8:00:00" | date_time >= "2022-08-24 13:15:00") %>% 
  filter(date_time <= "2022-08-25 10:18:00" | date_time >= "2022-08-25 13:40:00") %>% 
  filter(date_time <= "2022-08-26 9:15:00" | date_time >= "2022-08-26 13:20:00") %>% 
  filter(date_time <= "2022-08-27 8:00:00" | date_time >= "2022-08-27 13:35:00") %>% 
  filter(date_time <= "2022-08-28 10:40:00" | date_time >= "2022-08-28 14:00:00") %>% 
  filter(date_time <= "2022-08-29 9:40:00" | date_time >= "2022-08-29 14:30:00") %>% 
  filter(date_time <= "2022-08-30 9:05:00" | date_time >= "2022-08-30 13:30:00") %>%
  filter(date_time <= "2022-08-31 8:00:00" | date_time >= "2022-08-31 13:15:00") 


## salmon 2022

# removing obvious biofouling visible by amplitude increases that go away after
# sensor maintenance; unlike the Russian, it's much harder to distinguish the biofouling here
# from true DO, so unfortunately we have to remove those days
salmon_2022_cleaning <- salmon_2022_cleaning %>% 
  filter(date_time <= "2022-07-8 11:50:00" | date_time >= "2022-07-11 19:00:00") %>% 
  filter(date_time <= "2022-07-18 13:40:00" | date_time >= "2022-07-25 19:00:00")
  # no other maintenance after that date due to fires, but data looks decent
# removing other misc. outliers; removed period <2 hours
salmon_2022_cleaning_DO <- salmon_2022_cleaning %>% 
  filter(date_time <= "2022-07-28 6:40:00" | date_time >= "2022-07-28 8:15:00")

## south fork eel @ miranda 2023

# this data looks awesome and does not need further adjustment :)

## south fork eel @ standish hickey 2023

# removing these full days because the DO is very messy/bad
sfkeel_sth_2023_cleaning <- sfkeel_sth_2023_cleaning %>% 
  filter(date_time <= "2023-07-15 22:00:00" | date_time >= "2023-07-17 8:35:00") %>% 
  filter(date_time <= "2023-07-30 6:00:00" | date_time >= "2023-07-31 00:00:00")
# removing early season biofouling and other outliers; all removed periods <8 hours
sfkeel_sth_2023_cleaning_DO <- sfkeel_sth_2023_cleaning %>% 
  filter(date_time <= "2023-06-25 2:30:00" | date_time >= "2023-06-25 4:00:00") %>% 
  filter(date_time <= "2023-06-28 5:15:00" | date_time >= "2023-06-28 6:02:00") %>% 
  filter(date_time <= "2023-07-01 4:45:00" | date_time >= "2023-07-01 5:30:00") %>% 
  filter(date_time <= "2023-07-06 4:55:00" | date_time >= "2023-07-06 9:00:00") %>% 
  filter(date_time <= "2023-07-06 12:50:00" | date_time >= "2023-07-06 17:35:00") %>% 
  filter(date_time <= "2023-07-07 11:20:00" | date_time >= "2023-07-07 13:50:00") %>% 
  filter(date_time <= "2023-07-08 9:30:00" | date_time >= "2023-07-08 14:40:00") %>%
  filter(date_time <= "2023-07-08 4:45:00" | date_time >= "2023-07-08 6:25:00") %>% 
  filter(date_time <= "2023-07-09 12:30:00" | date_time >= "2023-07-09 19:30:00") %>% 
  filter(date_time <= "2023-07-10 15:30:00" | date_time >= "2023-07-10 20:00:00") %>% 
  filter(date_time <= "2023-07-13 15:00:00" | date_time >= "2023-07-13 19:20:00") %>% 
  filter(date_time <= "2023-07-14 12:00:00" | date_time >= "2023-07-14 19:10:00") %>% 
  filter(date_time <= "2023-07-15 16:00:00" | date_time >= "2023-07-15 19:20:00") %>%
  filter(date_time <= "2023-07-29 16:50:00" | date_time >= "2023-07-29 17:30:00") %>% 
  filter(date_time <= "2023-09-07 4:15:00" | date_time >= "2023-09-07 4:55:00")

## salmon river 2023

# removing evening 7/22 to morning 7/24 because general weirdness
# when we came to sensor on 7/26 it had been moved to a weird position
# 7/25 looks fine though
salmon_2023_cleaning <- salmon_2023_cleaning %>% 
  dplyr::filter(date_time <= "2023-07-22 20:45:00" | date_time >= "2023-07-24 3:25:00")
# removing misc. outliers
# not noticing any obvious biofouling this year
# river was much more turbid due to 2022 fires
# all removed periods <7 hours
salmon_2023_cleaning_DO <- salmon_2023_cleaning %>% 
  dplyr::filter(date_time <= "2023-07-12 18:00:00" | date_time >= "2023-07-12 19:25:00") %>% 
  dplyr::filter(date_time <= "2023-07-19 21:00:00" | date_time >= "2023-07-19 23:15:00") %>% 
  dplyr::filter(date_time <= "2023-07-22 20:45:00" | date_time >= "2023-07-24 4:30:00") %>% 
  dplyr::filter(date_time <= "2023-07-21 20:15:00" | date_time >= "2023-07-21 23:10:00") %>% 
  dplyr::filter(date_time <= "2023-07-25 23:20:00" | date_time >= "2023-07-25 23:40:00") %>% 
  dplyr::filter(date_time <= "2023-08-20 4:13:00" | date_time >= "2023-08-20 6:07:00") %>% 
  dplyr::filter(date_time <= "2023-08-20 12:33:00" | date_time >= "2023-08-20 13:30:00")

#### Merging data back together and final tidying ####

# Left join of "cleaning_DO" dataframe to the original cleaning dataframe that has preserved Temp_C
sfkeel_mir_2022_final <- left_join(sfkeel_mir_2022_cleaning, sfkeel_mir_2022_cleaning_DO, "date_time")
russian_2022_final <- left_join(russian_2022_cleaning, russian_2022_cleaning_DO, "date_time")
salmon_2022_final <- left_join(salmon_2022_cleaning, salmon_2022_cleaning_DO, "date_time")
sfkeel_mir_2023_final <- sfkeel_mir_2023_cleaning # no DO was removed separately as data was so good :)
sfkeel_sth_2023_final <- left_join(sfkeel_sth_2023_cleaning, sfkeel_sth_2023_cleaning_DO, "date_time")
salmon_2023_final <- left_join(salmon_2023_cleaning, salmon_2023_cleaning_DO, "date_time")

# making list of final dataframes
miniDOT_list <- list(salmon_2022_final, salmon_2023_final)
names(miniDOT_list) <- c("salmon_2022_miniDOT","salmon_2023_miniDOT")

# function to only keep relevant columns (and rename them)
clean_df <- function(df) {
  df %>% 
  mutate(Temp_C = Temp_C.x,
         DO_mgL = DO_mgL.y) %>% 
  select(date_time, Temp_C, DO_mgL)
}

# applying above function to each data frame
miniDOT_final <- lapply(miniDOT_list, clean_df)

# since sfkeel mir 2023 had no left_join() applied, we will do that manually
# later note-- this is not necessary??? check if this is necessary
sfkeel_mir_2023_final <- sfkeel_mir_2023_final %>% 
  dplyr::select(date_time, Temp_C, DO_mgL)

#### Saving cleaned csv's ####

path <- paste(getwd(), "/data/miniDOT/", sep = "")
lapply(names(miniDOT_final), function(x) write.csv(miniDOT_final[[x]], file = paste(path, x, ".csv", sep = ""), 
                                                   row.names = FALSE))
