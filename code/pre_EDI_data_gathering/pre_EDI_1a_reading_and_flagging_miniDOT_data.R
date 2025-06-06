#### miniDOT data aggregation and flagging
### Jordan Zabrecky
## last edited: 11.25.2024

# This code pulls data from miniDOT text files and converts them into csv's.
# Additionally this code adjusts the sensor time offset from PST, removes time when
# sensor was out of water, and flags outlying values

#### (1) Loading packages and reading in data #### 

## Loading necessary packages
lapply(c("tidyverse","lubridate","data.table","here", "zoo", "dplyr"),
       require, character.only = T)

# get rid of any potential masking!
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename

## Reading in data

# creating header list for csv to be produced
header_list <- c("Time_Sec", "BV_Volts", "Temp_C", "DO_mg_L", "Q")

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
    select(date_time, BV_Volts, Temp_C, DO_mg_L, Q)
}

## reading miniDOT data for each river site, making a data frame,
## and then applying a function to convert time into PST

## 2022

# south fork eel @ miranda
sfkeel_mir_2022 <- create_df("data/miniDOT/2022_raw_data/Miranda_663402")
sfkeel_mir_2022 <- time_fix(sfkeel_mir_2022)

# russian 
russian_2022 <- create_df("data/miniDOT/2022_raw_data/Russian_491496")
russian_2022 <- time_fix(russian_2022)

# salmon
salmon_2022 <- create_df("data/miniDOT/2022_raw_data/Salmon_521120")
salmon_2022 <- time_fix(salmon_2022)

## 2023

# south fork eel @ miranda
sfkeel_mir_2023 <- create_df("data/miniDOT/2023_raw_data/Miranda_663402")
sfkeel_mir_2023 <- time_fix(sfkeel_mir_2023)

# south fork eel @ standish hickey
sfkeel_sth_2023 <- create_df("data/miniDOT/2023_raw_data/Standish_Hickey_521120")
sfkeel_sth_2023 <- time_fix(sfkeel_sth_2023)

# salmon
salmon_2023 <- create_df("data/miniDOT/2023_raw_data/Salmon_529728")
salmon_2023 <- time_fix(salmon_2023)

#### (2) Initial data cleaning ####
### (a. quality check, b. time offsets, c. removing maintenance periods, etc.) ###

## (a) remove sensor data where Q < 0.7 
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

## (b) applying time offsets in accordance to screenshots of miniDOT time vs. actual PST time
## taken prior to launch

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

## (c) removing sensor maintenance and data download times

# this is done by removing recorded times from field notes
# but also using 'dygraphs' package in another script to make sure those times are correct
# this is found in another script "1b_visualizing_DO_data_cleaning.R"

# south fork eel @ miranda 2022
sfkeel_mir_2022_cleaning <- sfkeel_mir_2022_cleaning %>% 
  filter(date_time >= "2022-06-29 10:40:00") %>%  # initial launch time
  filter(date_time <= "2022-09-17 09:03:00") # retrieval time
# all maintenance times removed below
sfkeel_mir_2022_cleaning <- sfkeel_mir_2022_cleaning %>%
  filter(date_time <= "2022-07-14 09:15:00" | date_time >= "2022-07-14 09:20:00") %>% 
  filter(date_time <= "2022-07-28 09:20:00" | date_time >= "2022-07-28 10:10:00") %>% 
  filter(date_time <= "2022-08-10 08:55:00" | date_time >= "2022-08-10 09:25:00") %>% 
  filter(date_time <= "2022-08-23 09:30:00" | date_time >= "2022-08-23 11:25:00") %>% 
  filter(date_time <= "2022-09-06 09:15:00" | date_time >= "2022-09-06 10:25:00")

# russian 2022
russian_2022_cleaning <- russian_2022_cleaning %>% 
  filter(date_time >= "2022-06-24 17:15:00")  # initial launch time
  # no retrieval time because sensor was stolen :(
# all maintenance times removed below (and some weirdness right before pick-up)
russian_2022_cleaning <- russian_2022_cleaning %>% 
  filter(date_time <= "2022-07-06 09:30:00" | date_time >= "2022-07-06 10:30:00") %>% 
  filter(date_time <= "2022-07-20 06:45:00" | date_time >= "2022-07-20 09:55:00") %>% 
  filter(date_time <= "2022-08-02 08:15:00" | date_time >= "2022-08-02 10:10:00") %>% 
  # readjusted brick so maintenance time longer than normal here
  filter(date_time <= "2022-08-17 07:50:00" | date_time >= "2022-08-17 10:15:00") %>% 
  # last maintenance time with no end date because sensor was stolen :(
  filter(date_time <= "2022-09-01 7:40:00")

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
  filter(date_time <= "2023-09-27 11:22:00") # retrieval time
# all maintenance times removed below
sfkeel_mir_2023_cleaning <- sfkeel_mir_2023_cleaning %>% 
  filter(date_time <= "2023-06-25 10:45:00" | date_time >= "2023-06-25 11:55:00") %>% 
  filter(date_time <= "2023-07-03 11:55:00" | date_time >= "2023-07-03 12:30:00") %>% 
  filter(date_time <= "2023-07-10 09:20:00" | date_time >= "2023-07-10 09:50:00") %>% 
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
  filter(date_time >= "2023-06-24 09:45:00") %>%  # initial launch time
  filter(date_time <= "2023-09-27 10:00:00") # retrieval time
# all maintenance times removed below
sfkeel_sth_2023_cleaning <- sfkeel_sth_2023_cleaning %>% 
  filter(date_time <= "2023-06-25 18:10:00" | date_time >= "2023-06-25 18:15:00") %>% # moved brick further out
  filter(date_time <= "2023-07-03 08:35:00" | date_time >= "2023-07-03 09:20:00") %>% 
  filter(date_time <= "2023-07-11 09:35:00" | date_time >= "2023-07-11 10:00:00") %>% 
  filter(date_time <= "2023-07-17 08:34:00" | date_time >= "2023-07-17 09:10:00") %>% # forgot to turn DO sensor on this week 
  filter(date_time <= "2023-07-24 18:42:00" | date_time >= "2023-07-24 19:07:00") %>% 
  filter(date_time <= "2023-07-31 08:01:00" | date_time >= "2023-07-31 08:31:00") %>% 
  filter(date_time <= "2023-08-07 07:45:00" | date_time >= "2023-08-07 08:20:00") %>% 
  filter(date_time <= "2023-08-14 08:09:00" | date_time >= "2023-08-14 08:29:00") %>% 
  filter(date_time <= "2023-08-22 08:00:00" | date_time >= "2023-08-22 08:50:00") %>% 
  filter(date_time <= "2023-08-24 11:08:00" | date_time >= "2023-08-24 11:10:00") %>%  # toggled w/ brick to redeploy HOBO
  filter(date_time <= "2023-08-28 08:37:00" | date_time >= "2023-08-28 08:58:00") %>% 
  filter(date_time <= "2023-09-04 08:34:00" | date_time >= "2023-09-04 08:50:00") %>% 
  filter(date_time <= "2023-09-12 08:20:00" | date_time >= "2023-09-12 08:40:00") %>% 
  filter(date_time <= "2023-09-18 08:50:00" | date_time >= "2023-09-18 09:40:00")

# salmon 2023
salmon_2023_cleaning <- salmon_2023_cleaning %>% 
  filter(date_time >= "2023-06-27 19:35:00") %>% # initial launch time
  # did not retrieve until november due to wildfires, so just using end of our field season
  filter(date_time <= "2023-09-27 11:30:00") 
# all maintenance times removed below
salmon_2023_cleaning <- salmon_2023_cleaning %>% 
  filter(date_time <= "2023-07-12 18:00:00" | date_time >= "2023-07-12 19:25:00") %>% 
  filter(date_time <= "2023-07-26 18:49:00" | date_time >= "2023-07-26 19:35:00") %>% 
  filter(date_time <= "2023-08-09 18:25:00" | date_time >= "2023-08-09 19:05:00")

#### (3) Final data cleaning ####
### (a. removing minor outliers and b. large amount of data)

# still using 'dygraphs' package in "1b_visualizing_DO_data_cleaning.R" 
# to visualize data and cleaning

# since for the most part our temperature looks good, we will try to preserve that data
# DO will then be cleaned on a separate object "..._cleaning_DO"
# DO data missing for <=6 hours within linear periods will then be interpolated

## (a) removing minor outliers

# south fork eel @ miranda 2022
sfkeel_mir_2022_cleaning_DO <- sfkeel_mir_2022_cleaning %>% 
  filter(date_time <= "2022-08-18 23:45:00" | date_time >= "2022-08-18 23:50:00") %>% # <1 hour 
  filter(date_time <= "2022-08-21 12:05:00" | date_time >= "2022-08-21 12:45:00") %>% # <1 hour 
  filter(date_time <= "2022-08-22 11:20:00" | date_time >= "2022-08-22 14:00:00") %>% # <3 hours
  filter(date_time <= "2022-08-28 06:20:00" | date_time >= "2022-08-28 08:05:00") %>% # <2 hours
  filter(date_time <= "2022-08-28 08:12:00" | date_time >= "2022-08-28 10:20:00") %>% # <2 hours
  filter(date_time <= "2022-08-28 11:28:00" | date_time >= "2022-08-28 13:50:00") %>% # <3 hours
  filter(date_time <= "2022-08-30 03:20:00" | date_time >= "2022-08-30 06:20:00") %>% # <4 hours
  filter(date_time <= "2022-09-01 03:35:00" | date_time >= "2022-09-01 06:13:00") %>% # <3 hours
  filter(date_time <= "2022-09-01 22:50:00" | date_time >= "2022-09-01 23:30:00") %>% # <1 hour
  filter(date_time <= "2022-09-02 13:35:00" | date_time >= "2022-09-02 15:10:00") %>% # <2 hours
  filter(date_time <= "2022-09-03 10:50:00" | date_time >= "2022-09-03 11:40:00") %>% # <2 hours
  filter(date_time <= "2022-09-04 14:25:00" | date_time >= "2022-09-04 14:50:00") %>% # <1 hour
  filter(date_time <= "2022-09-04 18:40:00" | date_time >= "2022-09-04 19:25:00") %>% # <2 hours
  filter(date_time <= "2022-09-05 04:35:00" | date_time >= "2022-09-05 06:24:00") %>% # <2 hours 
  filter(date_time <= "2022-09-05 06:35:00" | date_time >= "2022-09-05 08:34:00") %>% # <2 hours 
  filter(date_time <= "2022-09-05 09:54:00" | date_time >= "2022-09-05 10:39:00") %>% # <2 hours
  filter(date_time <= "2022-09-05 12:40:00" | date_time >= "2022-09-05 15:08:00") %>% # <3 hours
  filter(date_time <= "2022-09-08 00:25:00" | date_time >= "2022-09-08 02:10:00") %>% # <2 hours 
  filter(date_time <= "2022-09-11 05:10:00" | date_time >= "2022-09-11 06:30:00") %>% # <2 hours
  filter(date_time <= "2022-09-12 06:45:00" | date_time >= "2022-09-12 07:05:00") %>% # <1 hour
  filter(date_time <= "2022-09-13 10:30:00" | date_time >= "2022-09-13 11:05:00") # <1 hour

# russian 2022
# removing misc. outliers and biofouling (that is removable)
# biofouling is identifiable via DO amplitude increases that disappear after
# and in this case, we are able to remove only that inflated period (unlike salmon 2022)
# sensor cleaning/maintenance; all removed periods <6 hours
russian_2022_cleaning_DO <- russian_2022_cleaning %>% 
  filter(date_time <= "2022-07-10 02:20:00" | date_time >= "2022-07-10 02:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-09 10:05:00" | date_time >= "2022-08-09 13:30:00") %>% # <3 hours
  filter(date_time <= "2022-08-13 09:45:00" | date_time >= "2022-08-13 12:25:00") %>% # <3 hours
  filter(date_time <= "2022-08-14 08:15:00" | date_time >= "2022-08-14 12:35:00") %>% # <4 hours
  filter(date_time <= "2022-08-15 08:45:00" | date_time >= "2022-08-15 13:20:00") %>% # <5 hours
  filter(date_time <= "2022-08-16 08:00:00" | date_time >= "2022-08-16 13:35:00") %>% # <6 hours
  filter(date_time <= "2022-08-20 10:40:00" | date_time >= "2022-08-20 14:00:00") %>% # <4 hours
  filter(date_time <= "2022-08-21 10:20:00" | date_time >= "2022-08-21 13:10:00") %>% # <2 hours
  filter(date_time <= "2022-08-22 09:15:00" | date_time >= "2022-08-22 13:30:00") %>% # <5 hours
  filter(date_time <= "2022-08-23 08:55:00" | date_time >= "2022-08-23 13:15:00") %>% # <5 hours
  filter(date_time <= "2022-08-24 08:00:00" | date_time >= "2022-08-24 13:15:00") %>% # <6 hours
  filter(date_time <= "2022-08-25 10:18:00" | date_time >= "2022-08-25 13:40:00") %>% # <3 hours
  filter(date_time <= "2022-08-26 09:15:00" | date_time >= "2022-08-26 13:20:00") %>% # <5 hours
  filter(date_time <= "2022-08-27 08:00:00" | date_time >= "2022-08-27 13:35:00") %>% # <6 hours
  filter(date_time <= "2022-08-28 10:40:00" | date_time >= "2022-08-28 14:00:00") %>% # <4 hours
  filter(date_time <= "2022-08-29 09:40:00" | date_time >= "2022-08-29 14:30:00") %>% # <5 hours
  filter(date_time <= "2022-08-30 09:05:00" | date_time >= "2022-08-30 13:30:00") %>% # <5 hours
  filter(date_time <= "2022-08-31 08:00:00" | date_time >= "2022-08-31 13:15:00")  # <6 hours

# salmon 2022
salmon_2022_cleaning_DO <- salmon_2022_cleaning %>% 
  filter(date_time <= "2022-07-05 13:40:00" | date_time >= "2022-07-05 18:15:00") %>%  # <5 hours
  filter(date_time <= "2022-07-28 06:40:00" | date_time >= "2022-07-28 08:15:00") # <2 hours

# south fork eel @ miranda 2023
sfkeel_mir_2023_cleaning_DO <- sfkeel_mir_2023_cleaning %>% 
  filter(date_time <= "2023-06-28 04:19:00" | date_time >= "2023-06-28 04:40:00") %>% # <1 hour
  filter(date_time <= "2023-07-03 00:45:00" | date_time >= "2023-07-03 03:07:00") %>% # <3 hours
  filter(date_time <= "2023-07-11 12:35:00" | date_time >= "2023-07-11 13:40:00") %>%  # <1 hour
  filter(date_time <= "2023-09-09 14:45:00" | date_time >= "2023-09-09 16:02:00") # <2 hours

# south fork eel @ standish hickey 2023
# removing early season biofouling and other outliers; all removed periods <8 hours
sfkeel_sth_2023_cleaning_DO <- sfkeel_sth_2023_cleaning %>% 
  filter(date_time <= "2023-06-25 02:30:00" | date_time >= "2023-06-25 04:00:00") %>% # <2 hours
  filter(date_time <= "2023-06-26 05:40:00" | date_time >= "2023-06-26 07:10:00") %>% # <2 hours
  filter(date_time <= "2023-06-28 05:15:00" | date_time >= "2023-06-28 06:02:00") %>% # <1 hour
  filter(date_time <= "2023-07-01 16:45:00" | date_time >= "2023-07-01 17:30:00") %>% # <1 hour
  filter(date_time <= "2023-07-03 06:07:00" | date_time >= "2023-07-03 08:31:00") %>% # <3 hours
  filter(date_time <= "2023-07-07 11:33:00" | date_time >= "2023-07-07 13:48:00") %>% # <2 hours
  filter(date_time <= "2023-07-07 14:29:00" | date_time >= "2023-07-07 16:57:00") %>% # <3 hours
  filter(date_time <= "2023-07-08 09:20:00" | date_time >= "2023-07-08 14:30:00") %>% # <6 hours
  filter(date_time <= "2023-07-08 04:45:00" | date_time >= "2023-07-08 06:25:00") %>% # <2 hours
  filter(date_time <= "2023-07-09 12:30:00" | date_time >= "2023-07-09 13:05:00") %>% # <1 hour
  filter(date_time <= "2023-07-09 14:35:00" | date_time >= "2023-07-09 15:20:00") %>% # <1 hour
  filter(date_time <= "2023-07-09 17:35:00" | date_time >= "2023-07-09 19:40:00") %>% # <3 hours
  filter(date_time <= "2023-07-10 15:35:00" | date_time >= "2023-07-10 16:55:00") %>% # <2 hours
  filter(date_time <= "2023-07-10 17:35:00" | date_time >= "2023-07-10 19:10:00") %>% # <2 hours
  filter(date_time <= "2023-07-13 16:00:00" | date_time >= "2023-07-13 19:15:00") %>% # <4 hours
  filter(date_time <= "2023-07-29 08:51:00" | date_time >= "2023-07-29 09:55:00") %>% # <2 hours
  filter(date_time <= "2023-07-29 16:05:00" | date_time >= "2023-07-29 18:00:00") %>% # <2 hours
  filter(date_time <= "2023-09-07 04:15:00" | date_time >= "2023-09-07 04:55:00") %>% # <1 hour
  filter(date_time <= "2023-09-17 10:50:00" | date_time >= "2023-09-17 14:04:00") # <4 hours

# salmon 2023 
salmon_2023_cleaning_DO <- salmon_2023_cleaning %>% 
  filter(date_time <= "2023-07-18 04:26:00" | date_time >= "2023-07-18 10:01:00") %>%  # <6 hours
  filter(date_time <= "2023-07-21 20:15:00" | date_time >= "2023-07-21 23:10:00") %>%  # <2 hours
  filter(date_time <= "2023-07-22 10:55:00" | date_time >= "2023-07-22 11:20:00") %>%  # <1 hour
  filter(date_time <= "2023-07-25 23:20:00" | date_time >= "2023-07-25 23:40:00") %>% # <1 hour
  filter(date_time <= "2023-08-20 04:13:00" | date_time >= "2023-08-20 06:07:00") %>% # <2 hours
  filter(date_time <= "2023-08-20 12:33:00" | date_time >= "2023-08-20 13:30:00") # <1 hour
  
## (b) removing longer periods of biofouling, bad data, etc. that cannot be interpolated

# south fork eel @ miranda 2022
# removing time where egg sac had been laid directly on sensor foil
# observed at 7/28/2022 pickup -> data all from ~50 hours beforehand seems wonky
# and strangeness before 7/14/2022 pickup with large oscillations on 5-min intervals
sfkeel_mir_2022_cleaning_DO <- sfkeel_mir_2022_cleaning_DO %>%
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-14 09:15:00") %>% 
  filter(date_time <= "2022-07-25 10:20:00" | date_time >= "2022-07-28 10:10:00")
# need to remove oscillating strangeness for temperature as well
# temperature looks normal for extended time when egg sac laid on foil
# will make new "temperature" csv for this site to allow for temperature-only flag
sfkeel_mir_2022_cleaning_temp <- sfkeel_mir_2022_cleaning %>% 
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-14 09:15:00") %>% 
  filter(date_time <= "2022-07-25 10:20:00" | date_time >= "2022-07-25 19:25:00")
# looking at the temperature though, there is a weird in between the two oscillating strangeness
# this does not correlate with decreased air temperatures, so maybe the sensor was not functioning properly?
# we also have increasing amplitude for the DO data so it may be best to just remove that week
sfkeel_mir_2022_cleaning_DO <- sfkeel_mir_2022_cleaning_DO %>%
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-28 10:20:00")
sfkeel_mir_2022_cleaning_temp <- sfkeel_mir_2022_cleaning_temp %>% 
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-28 10:20:00")

# russian 2022
# no extended periods need to be removed
  
# salmon 2022
# removing obvious biofouling visible by amplitude increases that go away after
# sensor maintenance; unlike the Russian, it's much harder to distinguish the biofouling here
# from true DO, so unfortunately we have to remove those days
salmon_2022_cleaning_DO <- salmon_2022_cleaning_DO %>% 
  filter(date_time <= "2022-07-8 11:50:00" | date_time >= "2022-07-11 19:00:00") %>% 
  filter(date_time <= "2022-07-18 13:40:00" | date_time >= "2022-07-25 19:00:00")

# south fork eel @ miranda
# no extended periods need to be removed

# south fork eel @ standish hickey
sfkeel_sth_2023_cleaning_DO <- sfkeel_sth_2023_cleaning_DO %>% 
  filter(date_time <= "2023-06-29 05:00:00" | date_time >= "2023-07-02 23:50:00") %>%
  # want to preserve at least 3 days after cleaning the sensors to have more
  # than a single day estimate between weekly maintenance
  filter(date_time <= "2023-07-06 05:00:00" | date_time >= "2023-07-11 00:35:00") %>%
  filter(date_time <= "2023-07-14 12:40:00" | date_time >= "2023-07-17 11:27:00") %>% # removing all the way to missing data
  filter(date_time <= "2023-07-30 08:30:00" | date_time >= "2023-07-31 01:00:00") # this one day looks awful

# salmon 2023
# removing weirdness (super fuzzy with low troughs) 7/19 and 7/22 to 7/25
# note that when we came to sensor on 7/26 it had been moved to a weird position
salmon_2023_cleaning_DO <- salmon_2023_cleaning_DO %>% 
  filter(date_time <= "2023-07-19 09:00:00" | date_time >= "2023-07-20 02:20:00") %>% 
  filter(date_time <= "2023-07-22 20:45:00" | date_time >= "2023-07-25 08:25:00")

#### (4) Adding flags for bad data and saving final single csv ####

# make list of site_year names in order of lists below
names_list <- c("sfkeel_mir_2022", "russian_2022", "salmon_2022", "sfkeel_mir_2023",
                "sfkeel_sth_2023", "salmon_2023")

# putting cleaning dataframes together in list (preserved all original data excluding maintenance)
preserved_list <- list(sfkeel_mir_2022_cleaning, russian_2022_cleaning,
                       salmon_2022_cleaning, sfkeel_mir_2023_cleaning,
                       sfkeel_sth_2023_cleaning, salmon_2023_cleaning)

# put cleaning_DO dataframes together in a list (which removed bad DO data)
clean_DO_list <- list(sfkeel_mir_2022_cleaning_DO, russian_2022_cleaning_DO,
                      salmon_2022_cleaning_DO, sfkeel_mir_2023_cleaning_DO,
                      sfkeel_sth_2023_cleaning_DO, salmon_2023_cleaning_DO)

# add site year info to cleaning and cleaning_DO dataframes
for(i in 1:length(preserved_list)) {
  preserved_list[[i]]$site_year <- names_list[i]
  clean_DO_list[[i]]$site_year <- names_list[i]
}

# do same for our single temperature dataframe (which removed bad temp data)
sfkeel_mir_2022_cleaning_temp$site_year <- "sfkeel_mir_2022"

# now, reduce lists into one dataframe and only keep relevant information for DO data frame
# additionally, add in column for clean DO to indicate that clean DO data is good
preserved <- bind_rows(preserved_list)
clean_DO <- bind_rows(clean_DO_list) %>% 
  select(site_year, date_time, DO_mg_L) %>% 
  mutate(DO_flag = "n")

## adding in clean temperature data

# cleaning single clean temp data frame and adding flag to indicate data is good
clean_temp <- sfkeel_mir_2022_cleaning_temp %>% 
  select(site_year, date_time, Temp_C) %>% 
  mutate(temp_flag = "n")

# all temp data not from sfkeel_mir_2022 was also good so indicate that with "n"
preserved <- preserved %>% 
  mutate(temp_flag = case_when(site_year != "sfkeel_mir_2022" ~ "n"))

# join with preserved dataset
preserved <- left_join(preserved, clean_temp, by = c("site_year", "date_time", "Temp_C"))

# have two separate temp flags now- if both are NA, the data is bad and indicate with "y"
preserved <- preserved %>% 
  mutate(temp_flag = case_when(temp_flag.x == "n" ~ "n",
                               temp_flag.y == "n" ~ "n",
                               TRUE ~ "y")) %>% 
  select(-c(temp_flag.x, temp_flag.y)) # remove now not needed columns

## adding in clean DO data

# join clean DO data with flags to preserved dataset
preserved <- left_join(preserved, clean_DO, by = c("site_year", "date_time", "DO_mg_L"))

# fill DO_flag NAs with "y" as those did not make it passed the cleaning process
preserved$DO_flag <- replace_na(preserved$DO_flag, "y")

# make sure there are no NA's anymore in dataframe
any(is.na(preserved)) # FALSE- we are good!

# make final dataset- remove columns and reorder
final <- preserved %>% 
  select(site_year, date_time, Temp_C, DO_mg_L, Q, temp_flag, DO_flag)

# changing date_time to character to avoid any saving issues like before
final$date_time <- as.character(format(final$date_time))

# save final csv to EDI data package folder
write.csv(final, "./data/EDI_data_package/miniDOT_data.csv", row.names = FALSE)
