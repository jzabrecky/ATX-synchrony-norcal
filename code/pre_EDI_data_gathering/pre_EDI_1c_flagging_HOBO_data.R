#### assembling and flagging HOBO U-24 sensor conductivity & temperature data
### Jordan Zabrecky
## last edited 12.05.24

# This code reads in csv's of conductivity data from HOBO-U24 sensors saved 
# from the HOBOware software and removes any outliers or periods where 
# the sensor was pulled out of water and saves it to a new csv

#### (1) Loading libraries and HOBO data ####

# load libraries
lapply(c("tidyverse", "lubridate", "plyr", "zoo"), require, character.only = T)

# get rid of any potential masking!
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename

## load HOBO data

read_HOBO_csvs <- function(path) {
  # list out subfolders in folder
  folders <- list.files(path)
  
  # initialize empty dataframe
  final <- data.frame()
  
  # iterate through each subfolder and add cvs's to dataframe
  for(i in 1:length(folders)) {
    temp <- ldply(list.files(paste(path, folders[i], sep = ""), pattern = ".csv"), function(filename) {
      d <- read.csv(paste(path, folders[i], "/", filename, sep = ""), header = FALSE) 
      d <- d[-1,] # remove first row
      d <- d[-(which(d[,1] == "#")),] # remove rows with column headers
      d <- d[,-1] # remove first column (which is just row names)
      d <- d[,1:4] # keep only columns with data
      d <- d[-(which(d[,2] == "")),] # remove rows that are "" or empty
      colnames(d) <- c("date_time", "low_range_cond_uS_cm", "full_range_cond_uS_cm", "temp_C")
      d$site <- folders[i] %>% stringr::str_sub(start = 10, end = nchar(folders[i])) # add name of site
      return(d)
    })
    final <- rbind(final, temp)
  }
  return(final)
}

# reading in HOBO data by year
HOBO_2022 <- read_HOBO_csvs("./data/HOBO/2022_raw_data/")
HOBO_2023 <- read_HOBO_csvs("./data/HOBO/2023_raw_data/")

# our time in the original csv was GMT -7, so our timezone is PST as we want
HOBO_2022$date_time <- mdy_hms(HOBO_2022$date_time, tz = "America/Los_Angeles")
HOBO_2023$date_time <- mdy_hms(HOBO_2023$date_time, tz = "America/Los_Angeles")

# lastly, create a column with site_year information
HOBO_2022 <- HOBO_2022 %>% 
  mutate(site_year = case_when(site == "Miranda" ~ "sfkeel_mir_2022",
                               site == "Russian" ~ "russian_2022",
                               site == "Salmon" ~ "salmon_2022"))
HOBO_2023 <- HOBO_2023 %>% 
  mutate(site_year = case_when(site == "Miranda" ~ "sfkeel_mir_2023",
                               site == "Standish" ~ "sfkeel_sth_2023",
                               site == "Salmon" ~ "salmon_2023"))

# turn into one list
HOBO <- rbind(HOBO_2022, HOBO_2023)

# convert temperature and conductivity classes to numeric
HOBO[,2:4] <- sapply(HOBO[,2:4], as.numeric)

# splitting data into list for cleaning
HOBO_list <- split(HOBO, HOBO$site_year)

#### (2) Remove maintenance periods and outliers ####

# using dygraphs package in script "1d_visualizing_HOBO_data_cleaning.R"
# while performing these steps to identify outliers and confirm maintenance times

## (a) Removing maintenance periods

# russian 2022
HOBO_list$russian_2022 <- HOBO_list$russian_2022 %>% 
  filter(date_time >= "2022-06-24 17:15:00")  # initial launch time
# no retrieval time because sensor was stolen :(
# all maintenance times removed below (and some weirdness right before pick-up)
HOBO_list$russian_2022 <- HOBO_list$russian_2022 %>% 
  filter(date_time <= "2022-07-06 09:30:00" | date_time >= "2022-07-06 10:30:00") %>% 
  filter(date_time <= "2022-07-20 06:45:00" | date_time >= "2022-07-20 09:55:00") %>% 
  filter(date_time <= "2022-08-02 08:15:00" | date_time >= "2022-08-02 10:10:00") %>% 
  # readjusted brick so maintenance time longer than normal here
  filter(date_time <= "2022-08-17 07:50:00" | date_time >= "2022-08-17 10:15:00") %>% 
  # last maintenance time with no end date because sensor was stolen :(
  filter(date_time <= "2022-09-01 7:40:00")

# south fork eel @ miranda 2022
HOBO_list$sfkeel_mir_2022 <- HOBO_list$sfkeel_mir_2022 %>% 
  filter(date_time >= "2022-06-29 10:40:00") %>%  # initial launch time
  filter(date_time <= "2022-09-17 09:03:00") # retrieval time
# all maintenance times removed below
HOBO_list$sfkeel_mir_2022 <- HOBO_list$sfkeel_mir_2022 %>%
  filter(date_time <= "2022-07-14 09:10:00" | date_time >= "2022-07-14 09:20:00") %>% 
  filter(date_time <= "2022-07-28 09:20:00" | date_time >= "2022-07-28 10:10:00") %>% 
  filter(date_time <= "2022-08-10 08:55:00" | date_time >= "2022-08-10 09:25:00") %>% 
  filter(date_time <= "2022-08-23 09:30:00" | date_time >= "2022-08-23 11:25:00") %>% 
  filter(date_time <= "2022-09-06 09:15:00" | date_time >= "2022-09-06 10:25:00")

# salmon 2022
HOBO_list$salmon_2022 <- HOBO_list$salmon_2022 %>% 
  filter(date_time >= "2022-06-26 19:00:00") %>%  # initial launch time
  filter(date_time <= "2022-09-21 17:50:00") # retrieval time
# all maintenance times removed below
HOBO_list$salmon_2022 <- HOBO_list$salmon_2022 %>% 
  filter(date_time <= "2022-07-11 18:45:00" | date_time >= "2022-07-11 19:45:00") %>%
  filter(date_time <= "2022-07-25 19:25:00" | date_time >= "2022-07-25 20:20:00")

# south fork eel @ miranda 2023
HOBO_list$sfkeel_mir_2023 <- HOBO_list$sfkeel_mir_2023 %>%
  filter(date_time >= "2023-06-18 11:30:00") %>%  # initial launch time
  filter(date_time <= "2023-09-27 11:22:00") # retrieval time
# all maintenance times removed below
HOBO_list$sfkeel_mir_2023 <- HOBO_list$sfkeel_mir_2023 %>% 
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
HOBO_list$sfkeel_sth_2023 <- HOBO_list$sfkeel_sth_2023 %>%
  filter(date_time >= "2023-06-24 09:45:00") %>%  # initial launch time
  filter(date_time <= "2023-09-27 10:00:00") # retrieval time
# all maintenance times removed below
HOBO_list$sfkeel_sth_2023 <- HOBO_list$sfkeel_sth_2023 %>% 
  filter(date_time <= "2023-06-25 18:10:00" | date_time >= "2023-06-25 18:15:00") %>% # moved brick further out
  filter(date_time <= "2023-07-03 08:35:00" | date_time >= "2023-07-03 09:20:00") %>% 
  filter(date_time <= "2023-07-11 09:35:00" | date_time >= "2023-07-11 10:00:00") %>% 
  filter(date_time <= "2023-07-17 08:34:00" | date_time >= "2023-07-17 09:10:00") %>% 
  filter(date_time <= "2023-07-24 18:42:00" | date_time >= "2023-07-24 19:07:00") %>% 
  filter(date_time <= "2023-07-31 08:01:00" | date_time >= "2023-07-31 08:31:00") %>% 
  filter(date_time <= "2023-08-07 07:45:00" | date_time >= "2023-08-07 08:20:00") %>% 
  filter(date_time <= "2023-08-14 08:09:00" | date_time >= "2023-08-14 08:29:00") %>% 
  filter(date_time <= "2023-08-22 08:00:00" | date_time >= "2023-08-22 08:50:00") %>% 
  # toggled w/ brick to redeploy HOBO; remove missing times where HOBO on but not in water
  filter(date_time <= "2023-08-24 07:40:00" | date_time >= "2023-08-24 11:10:00") %>% 
  filter(date_time <= "2023-08-28 08:37:00" | date_time >= "2023-08-28 08:58:00") %>% 
  filter(date_time <= "2023-09-04 08:34:00" | date_time >= "2023-09-04 08:50:00") %>% 
  filter(date_time <= "2023-09-12 08:20:00" | date_time >= "2023-09-12 08:40:00") %>% 
  filter(date_time <= "2023-09-18 08:50:00" | date_time >= "2023-09-18 09:40:00")

# salmon 2023
HOBO_list$salmon_2023 <- HOBO_list$salmon_2023 %>% 
  filter(date_time >= "2023-06-27 19:35:00") %>% # initial launch time
  # did not retrieve until november due to wildfires, so just using end of our field season
  filter(date_time <= "2023-09-27 11:30:00") 
# all maintenance times removed below
HOBO_list$salmon_2023 <- HOBO_list$salmon_2023 %>% 
  filter(date_time <= "2023-07-12 18:00:00" | date_time >= "2023-07-12 19:25:00") %>% 
  filter(date_time <= "2023-07-26 18:49:00" | date_time >= "2023-07-26 19:35:00") %>% 
  filter(date_time <= "2023-08-09 18:25:00" | date_time >= "2023-08-09 19:05:00")

## (b) removing outliers

# using new data frame for data without outliers

# russian 2022
russian_2022_cleaning_cond <- HOBO_list$russian_2022 %>% 
  filter(date_time <= "2022-06-27 14:10:00" | date_time >= "2022-06-27 17:50:00") %>% # <4 hours
  filter(date_time <= "2022-07-06 05:50:00" | date_time >= "2022-07-06 06:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 06:50:00" | date_time >= "2022-07-06 07:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 07:40:00" | date_time >= "2022-07-06 08:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 08:50:00" | date_time >= "2022-07-06 09:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-13 16:40:00" | date_time >= "2022-07-13 17:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-17 04:50:00" | date_time >= "2022-07-17 05:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-23 10:20:00" | date_time >= "2022-07-23 10:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-23 11:20:00" | date_time >= "2022-07-23 11:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-29 08:20:00" | date_time >= "2022-07-29 08:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-29 08:50:00" | date_time >= "2022-07-29 09:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-03 12:40:00" | date_time >= "2022-08-03 12:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-04 10:10:00" | date_time >= "2022-08-04 10:20:00") %>% # <1 hour
  filter(date_time <= "2022-08-11 14:20:00" | date_time >= "2022-08-11 15:20:00") %>% # <1 hour
  filter(date_time <= "2022-08-13 12:10:00" | date_time >= "2022-08-13 12:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-25 07:40:00" | date_time >= "2022-08-25 09:10:00") %>% # <2 hours
  filter(date_time <= "2022-08-25 09:20:00" | date_time >= "2022-08-25 09:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-25 14:20:00" | date_time >= "2022-08-25 14:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-26 18:15:00" | date_time >= "2022-08-26 19:20:00") %>% # <2 hours
  filter(date_time <= "2022-08-29 15:40:00" | date_time >= "2022-08-29 15:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-29 18:20:00" | date_time >= "2022-08-29 18:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-29 19:40:00" | date_time >= "2022-08-29 19:50:00") # <1 hour

# south fork eel @ miranda 2022
sfkeel_mir_2022_cleaning_cond <- HOBO_list$sfkeel_mir_2022 %>%
  filter(date_time <= "2022-07-02 05:20:00" | date_time >= "2022-07-02 05:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-03 15:40:00" | date_time >= "2022-07-03 16:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-04 09:40:00" | date_time >= "2022-07-04 09:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-04 17:10:00" | date_time >= "2022-07-04 18:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-04 18:50:00" | date_time >= "2022-07-04 19:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-04 20:10:00" | date_time >= "2022-07-04 21:10:00") %>% # <2 hours
  filter(date_time <= "2022-07-05 03:20:00" | date_time >= "2022-07-05 03:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-05 08:10:00" | date_time >= "2022-07-05 08:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-09 08:50:00" | date_time >= "2022-07-09 09:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-10 20:40:00" | date_time >= "2022-07-10 21:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-16 22:50:00" | date_time >= "2022-07-17 00:50:00") %>% # <4 hours
  filter(date_time <= "2022-07-17 18:20:00" | date_time >= "2022-07-17 19:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-19 07:10:00" | date_time >= "2022-07-19 08:10:00") %>% # <2 hours
  filter(date_time <= "2022-07-19 17:10:00" | date_time >= "2022-07-19 18:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-21 15:50:00" | date_time >= "2022-07-21 16:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-22 05:20:00" | date_time >= "2022-07-22 09:40:00") %>% # <5 hours
  filter(date_time <= "2022-07-25 18:20:00" | date_time >= "2022-07-25 21:10:00") %>% # <3 hours
  filter(date_time <= "2022-07-25 21:20:00" | date_time >= "2022-07-25 23:40:00") %>% # <3 hours
  filter(date_time <= "2022-07-27 00:40:00" | date_time >= "2022-07-27 02:10:00") %>% # <2 hours
  filter(date_time <= "2022-07-27 10:20:00" | date_time >= "2022-07-27 10:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-27 11:45:00" | date_time >= "2022-07-27 16:50:00") %>% # <6 hours
  filter(date_time <= "2022-08-01 07:50:00" | date_time >= "2022-08-01 20:50:00") %>% # >6 hours
  filter(date_time <= "2022-08-02 23:10:00" | date_time >= "2022-08-02 23:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-05 18:50:00" | date_time >= "2022-08-05 19:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-06 01:40:00" | date_time >= "2022-08-06 01:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-06 03:50:00" | date_time >= "2022-08-06 07:10:00") %>% # <4 hours
  filter(date_time <= "2022-08-06 21:10:00" | date_time >= "2022-08-06 21:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-08 21:20:00" | date_time >= "2022-08-08 21:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-08 23:40:00" | date_time >= "2022-08-08 23:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-10 04:50:00" | date_time >= "2022-08-10 05:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-10 08:10:00" | date_time >= "2022-08-10 08:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-12 02:20:00" | date_time >= "2022-08-12 02:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-14 00:50:00" | date_time >= "2022-08-14 02:10:00") %>% # <2 hours
  filter(date_time <= "2022-08-14 04:40:00" | date_time >= "2022-08-14 04:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-15 04:20:00" | date_time >= "2022-08-15 04:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-15 05:50:00" | date_time >= "2022-08-15 06:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-16 01:40:00" | date_time >= "2022-08-16 02:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-17 06:20:00" | date_time >= "2022-08-17 06:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-17 13:20:00" | date_time >= "2022-08-17 13:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-18 02:40:00" | date_time >= "2022-08-18 03:40:00") %>% # <2 hours
  filter(date_time <= "2022-08-18 11:20:00" | date_time >= "2022-08-18 11:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-18 14:10:00" | date_time >= "2022-08-18 14:20:00") %>% # <1 hour
  filter(date_time <= "2022-08-18 14:40:00" | date_time >= "2022-08-18 14:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-18 18:20:00" | date_time >= "2022-08-18 19:40:00") %>% # <2 hours
  filter(date_time <= "2022-08-20 04:10:00" | date_time >= "2022-08-20 05:20:00") %>% # <2 hours
  filter(date_time <= "2022-08-20 06:20:00" | date_time >= "2022-08-20 07:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-20 12:50:00" | date_time >= "2022-08-20 14:50:00") %>% # <3 hours
  filter(date_time <= "2022-08-21 22:50:00" | date_time >= "2022-08-22 00:20:00") %>% # <2 hours
  filter(date_time <= "2022-08-23 08:20:00" | date_time >= "2022-08-23 09:20:00") %>% # <2 hours
  filter(date_time <= "2022-08-26 02:10:00" | date_time >= "2022-08-26 02:20:00") %>% # <1 hour
  filter(date_time <= "2022-09-01 21:20:00" | date_time >= "2022-09-01 21:40:00") %>% # <1 hour
  filter(date_time <= "2022-09-02 09:20:00" | date_time >= "2022-09-02 09:40:00") %>% # <1 hour
  filter(date_time <= "2022-09-04 10:50:00" | date_time >= "2022-09-04 11:10:00") %>% # <1 hour
  filter(date_time <= "2022-09-12 22:20:00" | date_time >= "2022-09-12 22:40:00") %>% # <1 hour
  filter(date_time <= "2022-09-15 15:40:00" | date_time >= "2022-09-15 15:50:00") # <1 hour

# salmon 2022
salmon_2022_cleaning_cond <- HOBO_list$salmon_2022 %>% 
  filter(date_time <= "2022-07-04 21:40:00" | date_time >= "2022-07-04 21:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 08:20:00" | date_time >= "2022-07-06 09:40:00") %>% # <2 hours
  filter(date_time <= "2022-07-06 14:40:00" | date_time >= "2022-07-06 15:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 20:50:00" | date_time >= "2022-07-06 22:20:00") %>% # <2 hours
  filter(date_time <= "2022-07-07 02:40:00" | date_time >= "2022-07-07 02:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-07 05:40:00" | date_time >= "2022-07-07 06:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-07 09:20:00" | date_time >= "2022-07-07 09:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-07 14:20:00" | date_time >= "2022-07-07 15:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-07 11:10:00" | date_time >= "2022-07-07 11:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-08 08:20:00" | date_time >= "2022-07-08 08:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-08 10:10:00" | date_time >= "2022-07-08 11:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-08 13:00:00" | date_time >= "2022-07-08 13:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-08 16:50:00" | date_time >= "2022-07-08 17:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-10 09:40:00" | date_time >= "2022-07-10 09:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-10 10:10:00" | date_time >= "2022-07-10 10:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-10 13:10:00" | date_time >= "2022-07-10 13:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-10 16:20:00" | date_time >= "2022-07-10 16:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-16 09:40:00" | date_time >= "2022-07-16 09:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-17 16:10:00" | date_time >= "2022-07-17 16:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-19 13:50:00" | date_time >= "2022-07-19 14:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-11 16:50:00" | date_time >= "2022-08-11 17:20:00") %>% # <1 hour
  filter(date_time <= "2022-08-12 17:10:00" | date_time >= "2022-08-12 17:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-13 08:40:00" | date_time >= "2022-08-13 10:50:00") %>% # <3 hours
  filter(date_time <= "2022-08-14 09:40:00" | date_time >= "2022-08-14 09:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-15 13:50:00" | date_time >= "2022-08-15 15:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-15 16:10:00" | date_time >= "2022-08-15 16:20:00") %>% # <1 hour
  filter(date_time <= "2022-08-20 17:40:00" | date_time >= "2022-08-20 17:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-20 19:10:00" | date_time >= "2022-08-20 19:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-23 14:40:00" | date_time >= "2022-08-23 14:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-30 23:40:00" | date_time >= "2022-08-31 01:20:00") %>% # <2 hours
  filter(date_time <= "2022-08-31 23:50:00" | date_time >= "2022-09-01 00:10:00") %>% # <1 hour
  filter(date_time <= "2022-09-04 17:50:00" | date_time >= "2022-09-04 18:10:00") %>% # <1 hour
  filter(date_time <= "2022-09-05 14:10:00" | date_time >= "2022-09-05 14:20:00") %>% # <1 hour
  filter(date_time <= "2022-09-06 00:10:00" | date_time >= "2022-09-06 12:20:00") %>% # <1 hour
  filter(date_time <= "2022-09-13 09:10:00" | date_time >= "2022-09-13 11:40:00") %>% # <3 hours
  filter(date_time <= "2022-09-13 14:50:00" | date_time >= "2022-09-13 15:50:00") %>% # <2 hours
  filter(date_time <= "2022-09-13 17:10:00" | date_time >= "2022-09-13 17:20:00") %>% # <1 hour
  filter(date_time <= "2022-09-14 06:10:00" | date_time >= "2022-09-14 06:20:00") %>% # <1 hour
  filter(date_time <= "2022-09-14 14:20:00" | date_time >= "2022-09-14 14:40:00") %>% # <1 hour
  filter(date_time <= "2022-09-15 10:40:00" | date_time >= "2022-09-15 12:10:00") %>% # <2 hours
  filter(date_time <= "2022-09-15 23:50:00" | date_time >= "2022-09-16 00:10:00") %>% # <1 hour
  filter(date_time <= "2022-09-16 13:20:00" | date_time >= "2022-09-16 16:50:00") %>% # <4 hours
  filter(date_time <= "2022-09-16 17:50:00" | date_time >= "2022-09-16 21:40:00") %>% # <4 hours
  # just removing the rest because it's all kinda not good
  filter(date_time <= "2022-09-17 06:15:00")

# south fork eel @ miranda 2023
sfkeel_mir_2023_cleaning_cond <- HOBO_list$sfkeel_mir_2023 %>% 
  filter(date_time <= "2023-06-20 06:20:00" | date_time >= "2023-06-20 07:40:00") %>% # <1 hour
  filter(date_time <= "2023-06-20 23:50:00" | date_time >= "2023-06-21 00:10:00") %>% # <1 hour
  filter(date_time <= "2023-06-22 06:40:00" | date_time >= "2023-06-22 07:20:00") %>% # <1 hour
  filter(date_time <= "2023-06-23 16:40:00" | date_time >= "2023-06-23 16:50:00") %>% # <1 hour
  filter(date_time <= "2023-06-24 06:20:00" | date_time >= "2023-06-24 07:10:00") %>% # <1 hour
  filter(date_time <= "2023-06-24 14:50:00" | date_time >= "2023-06-24 15:10:00") %>% # <1 hour
  filter(date_time <= "2023-06-24 16:20:00" | date_time >= "2023-06-24 17:10:00") %>% # <1 hour
  filter(date_time <= "2023-06-25 03:10:00" | date_time >= "2023-06-25 03:20:00") %>% # <1 hour
  filter(date_time <= "2023-06-25 04:20:00" | date_time >= "2023-06-25 04:40:00") %>% # <1 hour
  filter(date_time <= "2023-06-25 07:40:00" | date_time >= "2023-06-25 07:50:00") %>% # <1 hour
  filter(date_time <= "2023-06-25 16:20:00" | date_time >= "2023-06-25 17:20:00") %>% # <2 hours
  filter(date_time <= "2023-06-26 18:10:00" | date_time >= "2023-06-26 18:20:00") %>% # <1 hours
  filter(date_time <= "2023-06-28 07:40:00" | date_time >= "2023-06-28 07:50:00") %>% # <1 hour
  filter(date_time <= "2023-06-28 20:40:00" | date_time >= "2023-06-28 20:50:00") %>% # <1 hour
  filter(date_time <= "2023-06-30 16:50:00" | date_time >= "2023-06-30 17:10:00") %>% # <1 hour
  filter(date_time <= "2023-07-01 05:20:00" | date_time >= "2023-07-01 05:40:00") %>% # <1 hour
  filter(date_time <= "2023-07-01 13:40:00" | date_time >= "2023-07-01 15:10:00") %>% # <2 hours
  filter(date_time <= "2023-07-01 22:20:00" | date_time >= "2023-07-01 22:40:00") %>% # <1 hour
  filter(date_time <= "2023-07-02 19:50:00" | date_time >= "2023-07-02 20:10:00") %>% # <1 hour
  filter(date_time <= "2023-07-09 03:10:00" | date_time >= "2023-07-09 03:20:00") %>% # <1 hour
  filter(date_time <= "2023-07-15 00:40:00" | date_time >= "2023-07-15 00:50:00") %>% # <1 hour
  filter(date_time <= "2023-07-22 05:10:00" | date_time >= "2023-07-22 05:20:00") %>% # <1 hour
  filter(date_time <= "2023-07-30 08:50:00" | date_time >= "2023-07-30 09:10:00") %>% # <1 hour
  filter(date_time <= "2023-07-30 15:40:00" | date_time >= "2023-07-30 15:50:00") %>% # <1 hour
  filter(date_time <= "2023-07-22 05:10:00" | date_time >= "2023-07-22 05:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-03 05:50:00" | date_time >= "2023-08-03 06:40:00") %>% # <1 hour
  filter(date_time <= "2023-08-10 03:10:00" | date_time >= "2023-08-10 03:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-10 09:40:00" | date_time >= "2023-08-10 09:50:00") %>% # <1 hour
  filter(date_time <= "2023-08-10 20:50:00" | date_time >= "2023-08-10 21:10:00") %>% # <1 hour
  filter(date_time <= "2023-08-11 00:20:00" | date_time >= "2023-08-11 01:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-10 08:50:00" | date_time >= "2023-08-10 09:10:00") %>% # <1 hour
  filter(date_time <= "2023-08-12 01:20:00" | date_time >= "2023-08-12 02:20:00") %>% # <2 hours
  filter(date_time <= "2023-08-12 23:20:00" | date_time >= "2023-08-13 00:50:00") %>% # <2 hours
  filter(date_time <= "2023-08-13 02:50:00" | date_time >= "2023-08-13 03:10:00") %>% # <1 hour
  filter(date_time <= "2023-08-13 11:50:00" | date_time >= "2023-08-13 12:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-14 06:20:00" | date_time >= "2023-08-14 06:40:00") %>% # <1 hour
  filter(date_time <= "2023-08-27 10:10:00" | date_time >= "2023-08-27 10:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-27 12:10:00" | date_time >= "2023-08-27 12:20:00") %>% # <1 hour
  filter(date_time <= "2023-09-01 05:50:00" | date_time >= "2023-09-01 06:10:00") %>% # <1 hour
  filter(date_time <= "2023-09-04 02:40:00" | date_time >= "2023-09-04 03:20:00") %>% # <1 hour
  filter(date_time <= "2023-09-10 08:40:00" | date_time >= "2023-09-10 08:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-10 13:10:00" | date_time >= "2023-09-10 13:20:00") %>% # <1 hour
  filter(date_time <= "2023-09-10 18:40:00" | date_time >= "2023-09-10 19:20:00") %>% # <1 hour
  filter(date_time <= "2023-09-11 04:50:00" | date_time >= "2023-09-11 05:10:00") %>% # <1 hour
  filter(date_time <= "2023-09-13 07:10:00" | date_time >= "2023-09-13 07:20:00") %>% # <1 hour
  filter(date_time <= "2023-09-18 04:40:00" | date_time >= "2023-09-18 04:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-23 20:50:00" | date_time >= "2023-09-23 21:20:00") # <1 hour

# south fork eel @ standish hickey 2023
sfkeel_sth_2023_cleaning_cond <- HOBO_list$sfkeel_sth_2023 %>% 
  filter(date_time <= "2023-07-15 08:50:00" | date_time >= "2023-07-15 09:10:00") %>% # <1 hour
  filter(date_time <= "2023-07-29 12:10:00" | date_time >= "2023-07-29 13:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-21 14:10:00" | date_time >= "2023-08-21 21:10:00") %>% # >7 hours
  filter(date_time <= "2023-08-27 06:50:00" | date_time >= "2023-08-27 07:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-31 23:50:00" | date_time >= "2023-09-01 00:50:00") %>% # <3 hours
  filter(date_time <= "2023-09-01 03:40:00" | date_time >= "2023-09-01 03:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-01 21:20:00" | date_time >= "2023-09-01 21:40:00") %>% # <1 hour
  filter(date_time <= "2023-09-01 22:40:00" | date_time >= "2023-09-01 23:50:00") %>% # <2 hours
  filter(date_time <= "2023-09-02 01:10:00" | date_time >= "2023-09-02 01:20:00") %>% # <1 hour 
  filter(date_time <= "2023-09-02 02:20:00" | date_time >= "2023-09-02 02:40:00") %>% # <1 hour
  filter(date_time <= "2023-09-02 12:10:00" | date_time >= "2023-09-02 12:40:00") %>% # <1 hour
  filter(date_time <= "2023-09-02 15:40:00" | date_time >= "2023-09-02 15:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-09 06:50:00" | date_time >= "2023-09-09 07:10:00") %>% # <1 hour
  filter(date_time <= "2023-09-11 09:40:00" | date_time >= "2023-09-11 09:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-12 08:10:00" | date_time >= "2023-09-02 08:40:00") %>% # <1 hour
  filter(date_time <= "2023-09-17 04:40:00" | date_time >= "2023-09-17 04:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-17 16:10:00" | date_time >= "2023-09-17 16:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-23 05:40:00" | date_time >= "2023-09-23 05:50:00") %>% # <1 hour
  filter(date_time <= "2023-09-25 17:40:00" | date_time >= "2023-09-25 17:50:00") # <1 hour

# salmon 2023
salmon_2023_cleaning_cond <- HOBO_list$salmon_2023 %>% 
  filter(date_time <= "2023-07-06 17:50:00" | date_time >= "2023-07-06 19:50:00") %>% # <3 hours
  filter(date_time <= "2023-07-07 07:40:00" | date_time >= "2023-07-07 08:10:00") %>% # <1 hour
  filter(date_time <= "2023-07-07 20:40:00" | date_time >= "2023-07-07 20:50:00") %>% # <1 hour
  filter(date_time <= "2023-07-11 16:50:00" | date_time >= "2023-07-11 17:20:00") %>% # <1 hour
  filter(date_time <= "2023-07-11 09:20:00" | date_time >= "2023-07-11 09:50:00") %>% # <1 hour
  filter(date_time <= "2023-07-20 07:40:00" | date_time >= "2023-07-20 08:10:00") %>% # <1 hour
  filter(date_time <= "2023-07-23 07:20:00" | date_time >= "2023-07-23 07:50:00") %>% # <1 hour
  filter(date_time <= "2023-07-23 10:40:00" | date_time >= "2023-07-23 11:20:00") %>% # <1 hour
  filter(date_time <= "2023-07-24 09:40:00" | date_time >= "2023-07-24 09:50:00") %>% # <1 hour
  filter(date_time <= "2023-07-24 20:40:00" | date_time >= "2023-07-24 21:40:00") %>% # <2 hours
  filter(date_time <= "2023-07-24 22:20:00" | date_time >= "2023-07-24 22:40:00") %>% # <1 hour
  filter(date_time <= "2023-08-14 13:50:00" | date_time >= "2023-08-14 14:30:00") %>% # <1 hour
  filter(date_time <= "2023-08-17 20:10:00" | date_time >= "2023-08-17 21:20:00") %>% # <2 hours
  filter(date_time <= "2023-08-18 07:40:00" | date_time >= "2023-08-18 08:20:00") %>% # <1 hour
  filter(date_time <= "2023-08-18 09:50:00" | date_time >= "2023-08-18 10:10:00") %>% # <1 hour
  filter(date_time <= "2023-08-19 02:20:00" | date_time >= "2023-08-19 04:30:00") %>% # <3 hours
  filter(date_time <= "2023-08-19 07:10:00" | date_time >= "2023-08-19 08:10:00") %>% # <2 hours
  filter(date_time <= "2023-08-19 08:20:00" | date_time >= "2023-08-19 09:20:00") %>% # <2 hours
  filter(date_time <= "2023-08-21 12:20:00" | date_time >= "2023-08-21 23:10:00") %>% # >24 hours messiness
  filter(date_time <= "2023-08-23 01:50:00" | date_time >= "2023-08-23 02:10:00") %>% # <1 hour
  filter(date_time <= "2023-08-24 19:10:00" | date_time >= "2023-08-24 21:50:00") %>% # <3 hours
  filter(date_time <= "2023-08-25 16:50:00" | date_time >= "2023-08-25 17:10:00") %>% # <1 hour
  filter(date_time <= "2023-08-26 16:40:00" | date_time >= "2023-08-26 17:10:00") %>% # <1 hour
  filter(date_time <= "2023-08-27 17:40:00" | date_time >= "2023-08-27 17:50:00") %>% # <1 hour
  filter(date_time <= "2023-08-28 23:50:00" | date_time >= "2023-08-29 01:40:00") %>% # <3 hours
  filter(date_time <= "2023-08-29 18:20:00" | date_time >= "2023-08-29 18:40:00") %>% # <1 hour
  filter(date_time <= "2023-09-06 15:40:00" | date_time >= "2023-09-07 19:20:00") %>% # >24 hours messiness
  filter(date_time <= "2023-09-09 10:10:00" | date_time >= "2023-09-09 11:10:00") %>% # <2 hours
  filter(date_time <= "2023-09-11 01:20:00" | date_time >= "2023-09-11 01:40:00") %>% # <1 hour
  filter(date_time <= "2023-09-12 12:20:00" | date_time >= "2023-09-12 13:20:00") %>% # <2 hours
  filter(date_time <= "2023-09-15 02:20:00" | date_time >= "2023-09-15 02:50:00") %>% # <1 hours
  # just removing the rest because it's all kinda not good
  filter(date_time <= "2023-09-15 16:20:00")

#### (3) Adding flags for bad/removed data and saving final csv ####

# want to reorder salmon by date (currently july is add end)
# (this is because we switched out HOBOs and the old one got read last)
HOBO_list$salmon_2023 <- dplyr::arrange(HOBO_list$salmon_2023, date_time)

# putting cleaned conductivity data into a list
clean_cond_list <- list(sfkeel_mir_2022_cleaning_cond, russian_2022_cleaning_cond,
                        salmon_2022_cleaning_cond, sfkeel_mir_2023_cleaning_cond,
                        sfkeel_sth_2023_cleaning_cond, salmon_2023_cleaning_cond)

# turn into one dataframe and add cond_flag column
clean_cond <- bind_rows(clean_cond_list) %>% 
  select(!site) %>% # column no longer relevant
  mutate(cond_flag = "n")

# now, reduce original dataframes (with maintenance times removed) into one
preserved <- bind_rows(HOBO_list) %>% 
  select(!site) # column no longer relevant

# left join data together
final <- left_join(preserved, clean_cond, by = c("date_time", 
                                                 "low_range_cond_uS_cm", 
                                                 "full_range_cond_uS_cm",
                                                 "temp_C", "site_year")) %>% 
  select(date_time, site_year, low_range_cond_uS_cm, full_range_cond_uS_cm, 
         temp_C, cond_flag)

# fill cond_flag NAs with "y" as those did not make it passed the cleaning process
final$cond_flag <- replace_na(final$cond_flag, "y")

# make sure there are no NA's anymore in dataframe
any(is.na(final)) # FALSE- we are good!

# changing date_time to character to avoid any saving issues like before
final$date_time <- as.character(format(final$date_time))

# save final csv to EDI data package folder
write.csv(final, "./data/EDI_data_package/HOBO_cond_data.csv", row.names = FALSE)
