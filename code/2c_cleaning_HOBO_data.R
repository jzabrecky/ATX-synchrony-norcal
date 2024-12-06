#### cleaning and assembling HOBO U-24 sensor conductivity & temperature data
### Jordan Zabrecky
## last edited 12.05.2024

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
HOBO <- read.csv("./data/EDI_data_package/HOBO_conductivity_data.csv") %>% 
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
cleantemp_list <- lapply(HOBO_list, function(x) interpolate_temp(x))

# interpolate conductivity
cleancond_list <- lapply(cleancond_list, function(x) interpolate_cond(x))

## using script "2c_visualizing_HOBO_data_cleaning.R" to make sure everything looks good

#### (3) Removing missing periods >6 hours that were interpolated ####

## these areas are partly informed by earlier data flagging and also script 2c

# south fork eel @ miranda 2022
cleancond_list$sfkeel_mir_2022 <- cleancond_list$sfkeel_mir_2022 %>% 
  filter(date_time <= "2022-08-01 07:50:00" | date_time >= "2022-08-01 20:50:00")

# south fork eel @ standish hickey
# removing period where sensor was not working for whatever reason & no data was recored
cleantemp_list$sfkeel_sth_2023 <- cleantemp_list$sfkeel_sth_2023 %>% 
  filter(date_time <= "2023-08-14 07:30:00" | date_time >= "2023-08-24 11:15:00")
cleancond_list$sfkeel_sth_2023 <- cleancond_list$sfkeel_sth_2023 %>% 
  filter(date_time <= "2023-08-14 07:30:00" | date_time >= "2023-08-24 11:15:00") %>% 
  filter(date_time <= "2023-08-21 14:10:00" | date_time >= "2023-08-21 21:10:00") # >7 hours

# salmon 2023
# removing period where sensor was not working & no data was recorded
cleantemp_list$salmon_2023 <- cleantemp_list$salmon_2023 %>% 
  filter(date_time <= "2023-07-26 18:15:00" | date_time >= "2023-08-09 19:30:00")
cleancond_list$salmon_2023 <- cleancond_list$salmon_2023 %>% 
  filter(date_time <= "2023-07-26 18:15:00" | date_time >= "2023-08-09 19:30:00") %>% 
  filter(date_time <= "2023-08-21 12:20:00" | date_time >= "2023-08-21 23:10:00") %>% # >24 hours messiness
  filter(date_time <= "2023-09-06 15:40:00" | date_time >= "2023-09-07 19:20:00") # >24 hours messiness

#### (4) Merging data back together and saving ####

# add site year info to cleaning and cleaning_DO dataframes
for(i in 1:length(cleantemp_list)) {
  cleantemp_list[[i]]$site_year <- names(cleantemp_list)[i]
  cleancond_list[[i]]$site_year <- names(cleancond_list)[i]
}

# create empty vector for all sites
final <- data.frame()

# combine all into a single dataframe per site
# can use indexing because all lists are in same site order
for(i in 1:length(cleancond_list)) {
  single_site <- left_join(cleantemp_list[[i]], cleancond_list[[i]], by = c("site_year", "date_time"))
  final <- rbind(final, single_site)
}

# reorder columns in final
final <- final %>% 
  relocate(site_year, .before = temp_C)


# making a function to change POSIXct column to character to avoid issue in some
# versions of R where POSIXct drops the "00:00:00" / midnight time when saved to csv
final$date_time <- as.character(format(final$date_time))

# save csv
write.csv(final, "./data/HOBO/clean_HOBO_all.csv", row.names = FALSE)
