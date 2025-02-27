#### miniDOT data aggregation and cleaning
### Jordan Zabrecky
## last edited: 11.25.2024

# This code reads in miniDOT data from the EDI package, removes flagged times,
# and interpolates for removed time periods < 6 hours long; used in tandem with
# script "1b visualizing miniDOT_data_cleaning.R" to visualize cleaning in dygraphs

#### (1) Loading packages and reading in data #### 

# loading necessary packages
lapply(c("tidyverse","lubridate", "zoo", "dplyr"),
       require, character.only = T)

# get rid of any dplyr masking
select <- dplyr::select
filter <- dplyr::filter

# reading in data
miniDOT_data <- read.csv("./data/EDI_data_package/miniDOT_data.csv")

# change date_time from character to date_time object
miniDOT_data$date_time <- as_datetime(miniDOT_data$date_time, tz = "America/Los_Angeles")

# split data into a list
miniDOT_list <- split(miniDOT_data, miniDOT_data$site_year)

#### (2) Removing flagged data ####

# function to filter out flagged DO data
filter_DO_flag <- function(df) {
  new_df <- df %>% 
    filter(DO_flag == "n")
  return(new_df)
}
  
# applying function to list
cleanDO_list <- lapply(miniDOT_list, function(x) filter_DO_flag(x))

# function to filter out flagged temp data
filter_temp_flag <- function(df) {
  new_df <- df %>% 
    filter(temp_flag == "n")
  return(new_df)
}

# applying function to list
cleantemp_list <- lapply(miniDOT_list, function(x) filter_temp_flag(x))

#### (3) Interpolating removed flagged data ####

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
  create_filled_TS(round_5M(df), "5M", "DO_mg_L") %>% 
    dplyr::select(date_time, Filled_Var) %>% 
    dplyr::rename(DO_mg_L = Filled_Var)
}

interpolate_temp <- function(df) {
  create_filled_TS(round_5M(df), "5M", "Temp_C") %>% 
    dplyr::select(date_time, Filled_Var) %>% 
    dplyr::rename(Temp_C = Filled_Var)
}

# applying functions to dataframe lists
cleanDO_list <- lapply(cleanDO_list, function(x) interpolate_DO(x))
cleantemp_list <- lapply(cleantemp_list, function(x) interpolate_temp(x))

#### (4) Removing periods of interpolation >6 hours ####

## To do this, I am referencing original cleaning script I made for the package
## and double checking with script "1b_visualizing_miniDOT_data_cleaning.R"

# south fork eel @ miranda 2022
# removing time where egg sac had been laid directly on sensor foil
# observed at 7/28/2022 pickup -> data all from ~50 hours beforehand seems wonky
# and strangeness before 7/14/2022 pickup with large oscillations on 5-min intervals
cleanDO_list$sfkeel_mir_2022 <- cleanDO_list$sfkeel_mir_2022 %>%
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-14 09:15:00") %>% 
  filter(date_time <= "2022-07-25 10:20:00" | date_time >= "2022-07-28 10:10:00")
# need to remove oscillating strangeness for temperature as well
cleantemp_list$sfkeel_mir_2022 <- cleantemp_list$sfkeel_mir_2022 %>% 
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-14 09:15:00") %>% 
  filter(date_time <= "2022-07-25 10:20:00" | date_time >= "2022-07-25 19:25:00")
# looking at the temperature though, there is a weird major decrease in between the
# two oscillating strangeness that does not correlate with air temperatures,
# so maybe the sensor was not functioning properly?
# we also have increasing amplitude for the DO data so it may be best to just remove that week
cleanDO_list$sfkeel_mir_2022 <- cleanDO_list$sfkeel_mir_2022 %>%
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-28 10:20:00")
cleantemp_list$sfkeel_mir_2022 <- cleantemp_list$sfkeel_mir_2022 %>% 
  filter(date_time <= "2022-07-13 19:30:00" | date_time >= "2022-07-28 10:20:00")

# russian 2022
# no extended periods need to be removed

# salmon 2022
# removing obvious biofouling visible by amplitude increases that go away after
# sensor maintenance; unlike the Russian, it's much harder to distinguish the biofouling here
# from true DO, so unfortunately we have to remove those days
cleanDO_list$salmon_2022 <- cleanDO_list$salmon_2022 %>% 
  filter(date_time <= "2022-07-08 11:50:00" | date_time >= "2022-07-11 20:00:00") %>% 
  filter(date_time <= "2022-07-18 13:40:00" | date_time >= "2022-07-25 19:00:00")

# south fork eel @ miranda 2023
# no extended periods need to be removed

# south fork eel @ standish hickey 2023
cleanDO_list$sfkeel_sth_2023 <- cleanDO_list$sfkeel_sth_2023 %>% 
  filter(date_time <= "2023-06-29 05:00:00" | date_time >= "2023-07-02 23:50:00") %>%
  # want to preserve at least 3 days after cleaning the sensors to have more
  # than a single day estimate between weekly maintenance
  filter(date_time <= "2023-07-06 05:00:00" | date_time >= "2023-07-11 00:35:00") %>%
  filter(date_time <= "2023-07-14 12:40:00" | date_time >= "2023-07-17 11:27:00") %>% # removing all the way to missing data
  filter(date_time <= "2023-07-30 08:30:00" | date_time >= "2023-07-31 01:00:00") # this one day looks awful

# remove week period where sensor was not turned on (that was interpolated)
cleantemp_list$sfkeel_sth_2023 <- cleantemp_list$sfkeel_sth_2023 %>% 
  filter(date_time <= "2023-07-17 11:26:00" | date_time >= "2023-07-24 19:07:00")
cleanDO_list$sfkeel_sth_2023 <- cleanDO_list$sfkeel_sth_2023 %>% 
  filter(date_time <= "2023-07-17 11:26:00" | date_time >= "2023-07-24 19:07:00")

# salmon 2023
# removing weirdness (super fuzzy with low troughs) 7/19 and 7/22 to 7/25
# note that when we came to sensor on 7/26 it had been moved to a weird position
cleanDO_list$salmon_2023 <- cleanDO_list$salmon_2023 %>% 
  filter(date_time <= "2023-07-19 09:00:00" | date_time >= "2023-07-20 02:20:00") %>% 
  filter(date_time <= "2023-07-22 20:45:00" | date_time >= "2023-07-25 08:25:00")

#### (5) Saving edited csvs ####

# make new dataframe list with cleaned temperature list
final_list <- cleantemp_list

# combining in cleanDO dataframe
for(i in 1:length(final_list)) {
  final_list[[i]] <- left_join(final_list[[i]], cleanDO_list[[i]])
}

# making a function to change POSIXct column to character to avoid issue in some
# versions of R where POSIXct drops the "00:00:00" / midnight time when saved to csv
fix_time_issue <- function(df) {
  new_df <- df %>% 
    mutate(date_time = as.character(format(date_time))) %>% 
    return(new_df)
}

# applying function to list
final_list <- lapply(final_list, function(x) fix_time_issue(x))

# saving csv's
path <- paste(getwd(), "/data/miniDOT/", sep = "")
lapply(names(final_list), function(x) write.csv(final_list[[x]], file = paste(path, x, "_miniDOT.csv", sep = ""), 
                                                     row.names = FALSE))
