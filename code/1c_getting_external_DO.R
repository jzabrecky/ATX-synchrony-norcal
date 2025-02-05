#### gathering DO from external sources to compare to our miniDOT values
### Jordan Zabrecky
## last edited 02.04.2024

# This code gathers dissolved oxygen data from the USGS gage at Cloverdale
# and data from the Karuk Tribe (with permission) to use to model metabolism 
# estimates and compare with our estimates with our (likely somewhat biofouled) miniDOTs

#### (1) Loading packages and reading in data #### 

# loading libraries
lapply(c("dataRetrieval", "lubridate", "tidyverse", "zoo"), require, character.only = T)

# USGS dissolved oxygen in mg/L code and temperature 
param_do <- "00300"
param_temp <- "00010"

# using "dataRetrieval" package to get USGS dissolved oxygen data & temperature
USGS_DO_russian <- readNWISuv("11463000", param_do, "2022-06-20", "2022-09-24") # going a bit over our field dates
USGS_temp_russian <- readNWISuv("11463000", param_temp, "2022-06-20", "2022-09-24")

# reading in data from karuk tribe (data is not publicly shared)
karuk_DO_salmon <- read.csv("./data/karuk_tribe_data/karuk_DO_data.csv", skip = 4)
karuk_temp_salmon <- read.csv("./data/karuk_tribe_data/karuk_temp_data.csv", skip = 4)

#### (2) Processing & cleaning final data ####

# left join them together & rename columns & select what we care about
USGS_russian <- left_join(USGS_DO_russian, USGS_temp_russian, by = "dateTime") %>% 
  dplyr::rename(DO_mg_L = X_00300_00000,
                Temp_C = X_00010_00000) %>% 
  mutate(date_time = as_datetime(dateTime, tz = "America/Los_Angeles")) %>% 
  dplyr::select(date_time, DO_mg_L, Temp_C)

karuk_salmon <- left_join(karuk_DO_salmon, karuk_temp_salmon, by = "Timestamp..UTC.08.00.") %>% 
  dplyr::rename(DO_mg_L = Value..mg.l.,
                Temp_C = Value..degC.) %>% 
  # time says UTC -8, but it's actually -7 because of summer time
  # so add an hour (discovered this by plotting against our DO data)
  mutate(date_time = force_tz(as_datetime(Timestamp..UTC.08.00.) + hours(1), 
                              tz = "America/Los_Angeles")) %>% 
  dplyr::select(date_time, DO_mg_L, Temp_C)

## (Using script 1b to visualize data cleaning)

# remove day on 7/5 that looks awful
USGS_russian <- USGS_russian %>% 
  dplyr::filter(date_time <= "2022-07-05 05:50:00" | date_time >= "2022-07-06 04:40:00")

# have one day of NA that can be filled for Russian temperature
USGS_russian$Temp_C <- na.approx(USGS_russian$Temp_C)

# remove weird midday drop on 7/5/2022 in Salmon River (does not make sense and model does not like it)
# attempting to linearly interpolate it does not seem worth it, so just not modeling GPP for that day
karuk_salmon <- karuk_salmon %>% 
  dplyr::filter(date_time <= "2022-07-05 15:45:00" | date_time >= "2022-07-05 19:55:00")
  
# have some NAs here and there to interpolate
karuk_salmon$DO_mg_L <- na.approx(karuk_salmon$DO_mg_L)
karuk_salmon$Temp_C <- na.approx(karuk_salmon$Temp_C)

#### (4) Saving external data ####

# convert date time to character to avoid issues
USGS_russian$date_time <- as.character(format(USGS_russian$date_time))
karuk_salmon$date_time <- as.character(format(karuk_salmon$date_time))

# separating salmon into two dataframes for 2022 and 2023 following dates of miniDOT dataframes
karuk_salmon_2022 <- karuk_salmon %>% 
  dplyr::filter(date_time >= "2022-06-26 19:05:00" & date_time <= "2022-09-21 17:50:00")
karuk_salmon_2023 <- karuk_salmon %>% 
  dplyr::filter(date_time >= "2023-06-27 19:40:00" & date_time <= "2023-09-27 11:30:00")

# save dataframe into external DO folder
write.csv(USGS_russian, "./data/external_DO/russian_2022_USGS.csv", row.names = FALSE)
write.csv(karuk_salmon_2022, "./data/external_DO/salmon_2022_karuk.csv", row.names = FALSE)
write.csv(karuk_salmon_2023, "./data/external_DO/salmon_2023_karuk.csv", row.names = FALSE)
