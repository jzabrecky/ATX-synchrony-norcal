#### miniDOT data aggregation and cleaning
### Jordan Zabrecky
## last edited: 11.14.2024

# This code reads in miniDOT data from the EDI package, removes flagged times,
# and interpolates for removed time periods < 6 hours long; used in tandem with
# script "1b visualizing miniDOT_data_cleaning.R" to visualize cleaning in dygraphs

#### (1) Loading packages and reading in data #### 

# loading necessary packages
lapply(c("tidyverse","lubridate", "zoo", "dplyr"),
       require, character.only = T)

# reading in data
miniDOT_data <- read.csv("./data/EDI_data_package/miniDOT_data.csv")

# split data into a list
miniDOT_list <- split(miniDOT_data, miniDOT_data$site_year)

#### (2) Removing flagged data ####

# separating out into old DO dataframe names to use with script "1b"
# and removed flagged DO data
sfkeel_mir_2022_cleaning_DO <- miniDOT_list$sfkeel_mir_2022 %>% 
  filter(DO_flag == "n")
russian_2022_cleaning_DO <- miniDOT_list$russian_2022 %>% 
  filter(DO_flag == "n")
salmon_2022_cleaning_DO <- miniDOT_list$salmon_2022 %>% 
  filter(DO_flag == "n")
sfkeel_mir_2023_cleaning_DO <- miniDOT_list$sfkeel_mir_2023 %>% 
  filter(DO_flag == "n")
sfkeel_sth_2023_cleaning_DO <- miniDOT_list$sfkeel_sth_2023 %>% 
  filter(DO_flag == "n")
salmon_2023_cleaning_DO <- miniDOT_list$salmon_2023 %>% 
  filter(DO_flag == "n")

# repeat for temperature
sfkeel_mir_2022_cleaning_temp <- 

#### (3) Interpolating flagged data ####

#### (4) Removing periods of interpolation >6 hours ####

### old interpolation code 

## (b) interpolating missing data using other script: "1c_split_interpolate_data.R"

# create a time series for every five minutes and fill in missing
# variables via linear interpolation first with the "create_filled_TS" function
source("code/supplemental_code/S1a_split_interpolate_data.R")

# need to round to nearest 5 minutes first
round_5M <- function(df) {
  df$date_time <- round_date(df$date_time, "5 minutes")
  return(df)
}

# creating functions to interpolate DO and temperature
interpolate_DO <- function(df) {
  create_filled_TS(round_5M(df), "5M", "DO_mgL") %>% 
    select(date_time, Filled_Var) %>% 
    rename(DO_mgL = Filled_Var)
}

interpolate_temp <- function(df) {
  create_filled_TS(round_5M(df), "5M", "Temp_C") %>% 
    select(date_time, Filled_Var) %>% 
    rename(Temp_C = Filled_Var)
}

# applying functions to dataframes 
# (still separately because would have to split the list right after anyways)
## SKIP THIS SECTION WHEN RUNNING FOR EDI DATA (want to preserve removed data to flag it)

# 2022
sfkeel_mir_2022_cleaning_temp <- interpolate_temp(sfkeel_mir_2022_cleaning)
sfkeel_mir_2022_cleaning_DO <- interpolate_DO(sfkeel_mir_2022_cleaning_DO)
russian_2022_cleaning_temp <- interpolate_temp(russian_2022_cleaning)
russian_2022_cleaning_DO <- interpolate_DO(russian_2022_cleaning_DO)
salmon_2022_cleaning_temp <- interpolate_temp(salmon_2022_cleaning)
salmon_2022_cleaning_DO <- interpolate_DO(salmon_2022_cleaning_DO)

# 2023
sfkeel_mir_2023_cleaning_temp <- interpolate_temp(sfkeel_mir_2023_cleaning)
sfkeel_mir_2023_cleaning_DO <- interpolate_DO(sfkeel_mir_2023_cleaning_DO)
sfkeel_sth_2023_cleaning_temp <- interpolate_temp(sfkeel_sth_2023_cleaning)
sfkeel_sth_2023_cleaning_DO <- interpolate_DO(sfkeel_sth_2023_cleaning_DO)
salmon_2023_cleaning_temp <- interpolate_temp(salmon_2023_cleaning)
salmon_2023_cleaning_DO <- interpolate_DO(salmon_2023_cleaning_DO)

# for standish hickey, we have a week period where we were missing data, so we need
# to remove interpolation from then
sfkeel_sth_2023_cleaning_temp <- sfkeel_sth_2023_cleaning_temp %>% 
  filter(date_time <= "2023-07-17 11:26:00" | date_time >= "2023-07-24 19:07:00")
sfkeel_sth_2023_cleaning_DO <- sfkeel_sth_2023_cleaning_DO %>% 
  filter(date_time <= "2023-07-17 11:26:00" | date_time >= "2023-07-24 19:07:00")
