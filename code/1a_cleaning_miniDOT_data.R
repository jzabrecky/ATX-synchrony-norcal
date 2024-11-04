#### miniDOT data aggregation and cleaning
### Jordan Zabrecky
## last edited: 11.04.2024

# This code reads in miniDOT data from the EDI package, removes flagged times,
# and interpolates for removed time periods < 6 hours long

#### (1) Loading packages and reading in data #### 



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
