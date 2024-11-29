#### cleaning and assembling HOBO U-24 sensor conductivity & temperature data
### Jordan Zabrecky
## last edited 11.26.2024

# This code reads in HOBO conductivity data from the EDI data package
# removes flagged data, and linearly interpolates removed/missing data 
# in 15-minute intervals

#### (1) Loading libraries and HOBO data ####

# load libraries
lapply(c("tidyverse", "lubridate", "plyr", "zoo"), require, character.only = T)

# get rid of any potential masking!
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename

# load in HOBO data & only keep columns we care about
HOBO <- read.csv("./data/EDI_data_package/HOBO_cond_data.csv") %>% 
  rename(cond_uS_cm = low_range_cond_uS_cm) %>%  # using low range
  select(date_time, site_year, temp_C, cond_uS_cm, cond_flag)

# change date_time from character to date_time object
HOBO$date_time <- as_datetime(HOBO$date_time, tz = "America/Los_Angeles")

#### (2) Remove flagged data and interpolate missing data ####

# create another data frame with flagged conductivity removed
cleancond <- HOBO %>% 
  filter(cond_flag == "n")

# turn original and clean data frames into a list
HOBO_list <- split(HOBO, HOBO$site_year)
cleancond_list <- split(cleancond, cleancond$site_year)

# create filled time series using function from supplemental code
source("code/supplemental_code/S1a_split_interpolate_data.R")

# creating functions to interpolate conductivity and temperature
interpolate_temp <- function(df) {
  create_filled_TS(df, "15M", "temp_C") %>% 
    select(date_time, Filled_Var) %>% 
    rename(temp_C = Filled_Var)
}

# only care about low range conductivity since we are working in freshwater
interpolate_cond <- function(df) {
  create_filled_TS(df, "15M", "cond_uS_cm") %>% 
    select(date_time, Filled_Var) %>% 
    rename(cond_uS_cm = Filled_Var)
}

# interpolate temperature
temp_list <- lapply(HOBO_list, function(x) interpolate_temp(x))

# interpolate conductivity
cleancond_list <- lapply(cleancond_list, function(x) interpolate_cond(x))

## using script "3_visualizing_HOBO_data_cleaning.R" to make sure everything looks good

## NEED TO WAIT TILL METABOLISM SCRIPT IS FINISHED ON THE DESKTOP :)

## (c) linear interpolationsfkeel_sth_2023## (c) linear interpolation for missing values

# create filled time series using function from supplemental code
source("code/supplemental_code/S1a_split_interpolate_data.R")

# creating functions to interpolate conductivity and temperature
interpolate_temp <- function(df) {
  create_filled_TS(df, "15M", "temp_C") %>% 
    select(date_time, Filled_Var) %>% 
    rename(temp_C = Filled_Var)
}

# only care about low range conductivity since we are working in freshwater
interpolate_cond <- function(df) {
  create_filled_TS(df, "15M", "low_range_cond_uS_cm") %>% 
    select(date_time, Filled_Var) %>% 
    rename(low_range_cond_uS_cm = Filled_Var)
}

# applying functions to dataframes 
# (still separately because would have to split the list right after anyways)
## SKIP THIS SECTION WHEN RUNNING FOR EDI DATA (want to preserve removed data to flag it)

# 2022
sfkeel_mir_2022_cleaning_temp <- interpolate_temp(sfkeel_mir_2022_cleaning)
sfkeel_mir_2022_cleaning_cond <- interpolate_cond(sfkeel_mir_2022_cleaning_cond)
russian_2022_cleaning_temp <- interpolate_temp(russian_2022_cleaning)
russian_2022_cleaning_cond <- interpolate_cond(russian_2022_cleaning_cond)
salmon_2022_cleaning_temp <- interpolate_temp(salmon_2022_cleaning)
salmon_2022_cleaning_cond <- interpolate_cond(salmon_2022_cleaning_cond)

# 2023
sfkeel_mir_2023_cleaning_temp <- interpolate_temp(sfkeel_mir_2023_cleaning)
sfkeel_mir_2023_cleaning_cond <- interpolate_cond(sfkeel_mir_2023_cleaning_cond)
sfkeel_sth_2023_cleaning_temp <- interpolate_temp(sfkeel_sth_2023_cleaning)
sfkeel_sth_2023_cleaning_cond <- interpolate_cond(sfkeel_sth_2023_cleaning_cond)

# need to reorder salmon 2023 by date to use "spread_TS" function
salmon_2023_cleaning <- dplyr::arrange(salmon_2023_cleaning, date_time)
salmon_2023_cleaning_cond <- dplyr::arrange(salmon_2023_cleaning_cond, date_time)

# finish applying function
salmon_2023_cleaning_temp <- interpolate_temp(salmon_2023_cleaning)
salmon_2023_cleaning_cond <- interpolate_cond(salmon_2023_cleaning_cond)

## (d) removing periods longer 6 hours

# south fork eel @ miranda 2022
sfkeel_mir_2022_cleaning_cond <- sfkeel_mir_2022_cleaning_cond %>% 
  filter(date_time <= "2022-08-01 07:50:00" | date_time >= "2022-08-01 20:50:00")

# salmon 2022
# just cutting off the end of salmon sensor because it gets really messy
salmon_2022_cleaning_cond <- salmon_2022_cleaning_cond %>% 
  filter(date_time <= "2022-09-17 06:15:00")

# south fork eel @ standish hickey
# removing period where sensor was not working for whatever reason & no data was recored
sfkeel_sth_2023_cleaning_temp <- sfkeel_sth_2023_cleaning %>% 
  filter(date_time <= "2023-08-14 07:30:00" | date_time >= "2023-08-24 11:15:00")
sfkeel_sth_2023_cleaning_cond <- sfkeel_sth_2023_cleaning_cond %>% 
  filter(date_time <= "2023-08-14 07:30:00" | date_time >= "2023-08-24 11:15:00") %>% 
  filter(date_time <= "2023-08-21 14:10:00" | date_time >= "2023-08-21 21:10:00") # >7 hours

# salmon 2023
# removing period where sensor was not working & no data was recorded
salmon_2023_cleaning_temp <- salmon_2023_cleaning_temp %>% 
  filter(date_time <= "2023-07-26 18:15:00" | date_time >= "2023-08-09 19:30:00")
salmon_2023_cleaning_cond <- salmon_2023_cleaning_cond %>% 
  filter(date_time <= "2023-07-26 18:15:00" | date_time >= "2023-08-09 19:30:00") %>% 
  filter(date_time <= "2023-08-21 12:20:00" | date_time >= "2023-08-21 23:10:00") %>% # >24 hours messiness
  filter(date_time <= "2023-09-06 15:40:00" | date_time >= "2023-09-07 19:20:00") %>% # >24 hours messiness
  # just removing rest of the data because it is all super messy
  filter(date_time <= "2023-09-15 16:20:00")

#### (4) Merging data back together and saving ####

# Left join of cleaning conductivity dataframe to the cleaning dataframe that has preserved & interpolated temperature
sfkeel_mir_2022_HOBO <- left_join(sfkeel_mir_2022_cleaning_temp, sfkeel_mir_2022_cleaning_cond, "date_time")
russian_2022_HOBO <- left_join(russian_2022_cleaning_temp, russian_2022_cleaning_cond, "date_time")
salmon_2022_HOBO <- left_join(salmon_2022_cleaning_temp, salmon_2022_cleaning_cond, "date_time")
sfkeel_mir_2023_HOBO <- left_join(sfkeel_mir_2023_cleaning_temp, sfkeel_mir_2023_cleaning_cond, "date_time")
sfkeel_sth_2023_HOBO <- left_join(sfkeel_sth_2023_cleaning_temp, sfkeel_sth_2023_cleaning_cond, "date_time")
salmon_2023_HOBO <- left_join(salmon_2023_cleaning_temp, salmon_2023_cleaning_cond, "date_time")

# making list of final data frames
HOBO_final_list <- list(sfkeel_mir_2022_HOBO, russian_2022_HOBO, salmon_2022_HOBO, 
                        sfkeel_mir_2023_HOBO, sfkeel_sth_2023_HOBO, salmon_2023_HOBO)
names(HOBO_final_list) <- c("sfkeel_mir_2022_HOBO", "russian_2022_HOBO", "salmon_2022_HOBO", 
                            "sfkeel_mir_2023_HOBO", "sfkeel_sth_2023_HOBO", "salmon_2023_HOBO")

# making a function to change POSIXct column to character to avoid issue in some
# versions of R where POSIXct drops the "00:00:00" / midnight time when saved to csv
fix_time_issue <- function(df) {
  new_df <- df %>% 
    mutate(date_time = as.character(format(date_time))) %>% 
    return(new_df)
}

# applying function to list
HOBO_final_list <- lapply(HOBO_final_list, function(x) fix_time_issue(x))

# saving csv's
path <- paste(getwd(), "/data/HOBO/", sep = "")
lapply(names(HOBO_final_list), function(x) write.csv(HOBO_final_list[[x]], file = paste(path, x, ".csv", sep = ""), 
                                                  row.names = FALSE))
