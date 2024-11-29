#### gathering DO from external sources to compare to our miniDOT values
### Jordan Zabrecky
## last edited 11.25.2024

# This code gathers dissolved oxygen data from the USGS gage at Cloverdale
# to use to model metabolism estimates and compare with our estimates
# using our (likely somewhat biofouled) miniDOTs

#### (1) Loading packages and reading in data #### 

# loading libraries
lapply(c("dataRetrieval", "lubridate", "tidyverse", "zoo"), require, character.only = T)

# USGS dissolved oxygen in mg/L code and temperature 
param_do <- "00300"
param_temp <- "00010"

# using "dataRetrieval" package to get USGS dissolved oxygen data & temperature
USGS_DO_russian <- readNWISuv("11463000", param_do, "2022-06-20", "2022-09-24") # going a bit over our field dates
USGS_temp_russian <- readNWISuv("11463000", param_temp, "2022-06-20", "2022-09-24")

# left join them together
USGS_DO_russian <- left_join(USGS_DO_russian, USGS_temp_russian, by = "dateTime")

#### (2) Processing external data ####

## need to get DO data in 5-minute intervals and fix timezone from UTC to PST

# use "create_filled_TS" function from other script
source("code/supplemental_code/S1a_split_interpolate_data.R")

# function to apply to interpolate and clean down dataframe
clean_USGS_df <- function(df) {
  
  # change time zone to PST
  df <- df %>% mutate(date_time = as_datetime(dateTime, tz = "America/Los_Angeles"))
  
  # fill time series with dissolved oxygen every 5 minutes
  new_df <- create_filled_TS(df, "5M", "X_00300_00000") %>% 
    mutate(DO_mg_L = Filled_Var) %>% 
    dplyr::rename(Temp_C = X_00010_00000) %>% 
    dplyr::select(date_time, DO_mg_L, Temp_C)
  
  # finish interpolation for temperature
  new_df$Temp_C <- na.approx(new_df$Temp_C)
  
  # return data frame
  return(new_df)
}

# apply function to df
USGS_russian <- clean_USGS_df(USGS_DO_russian)

#### (3) Checking/cleaning final data ####

## (Using script 1b to visualize data cleaning)

# seems like there was no data for a period on 8/30/2022 so need to remove linear interpolation
USGS_russian <- USGS_russian %>% 
  dplyr::filter(date_time <= "2022-08-30 10:50:00" | date_time >= "2022-08-31 09:40:00")

#### (4) Saving external data ####

# convert date time to character to avoid issues
USGS_russian$date_time <- as.character(as.character(format(USGS_russian$date_time)))

# save dataframe into external DO folder
write.csv(USGS_russian, "./data/external_DO/russian_2022_USGS.csv", row.names = FALSE)
