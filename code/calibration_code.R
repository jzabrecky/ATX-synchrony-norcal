#### intercalibration for miniDOTs for 2022 and 2023 field season
### Jordan Zabrecky
## last edited: 08.28.2024

# <insert description>

## For 2022 field season:
# 491496 - Russian River (no end calibration as it was stolen)
# 521120 - Salmon River
# 663402 - South Fork Eel River @ Miranda
# 162745 - roving sensor (in place of Oakton probe)

## For 2023 field season:
# 529728 - Salmon River
# 521220 - South Fork Eel River @ Standish Hickey
# 663402 - South Fork Eel River @ Miranda

#### (1) Loading libraries and calibration data ####

## Loading necessary packages
lapply(c("tidyverse", "lubridate", "data.table", "here"), require, 
       character.only = T)

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
    select(date_time, BV_Volts, Temp_C, DO_mgL, Q, miniDOT_no)
}

## 03-2023 (post 2022 field season & pre 2023 field season)
path <- "./data/miniDOT/intercalibrations/202303/"
file_list <- list.files(path)

# reading in list
post_2022_list <- lapply(file_list, function(x) create_df(paste(path, x, sep = "")))

# adding miniDOT number to column
for(i in 1:length(post_2022_list)) {
  post_2022_list[[i]]$miniDOT_no <- file_list[i]
}

# reducing list into one dataframe
post_2022 <- reduce(post_2022_list, rbind)

# appying time fix
post_2022 <- time_fix(post_2022)

## 01-2024 (post 2023 field season)

# path and file list for folder
path <- "./data/miniDOT/intercalibrations/202401/"
file_list <- list.files(path)

# reading in list
post_2023_list <- lapply(file_list, function(x) create_df(paste(path, x, sep = "")))

# adding miniDOT number to column
for(i in 1:length(post_2023_list)) {
  post_2023_list[[i]]$miniDOT_no <- file_list[i]
}

# reducing list into one dataframe
post_2023 <- reduce(post_2023_list, rbind)

# appying time fix
post_2023 <- time_fix(post_2023)

#### (2) Looking at calibration data ####

## no recorded time for bubbler being off for pre 2022 field season unfortunately 

## 03-2023 (post 2022 field season & pre 2023 field season)

post_2022_temp <- ggplot(data = post_2022, aes(x = date_time, y = Temp_C, color = miniDOT_no)) +
  geom_point() +
  scale_x_datetime(limits = as_datetime(c("2023-03-30 18:05:00", "2023-04-01 12:45:00"), 
                                        tz = "America/Los_Angeles")) +
  geom_vline(xintercept = as_datetime(c("2023-03-31 07:58:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) + # bubbler off
  geom_vline(xintercept = as_datetime(c("2023-04-01 10:31:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) # yeast added & lid closed
# all temperature is roughly in line- yay!

post_2022_DO <- ggplot(data = post_2022, aes(x = date_time, y = DO_mgL, color = miniDOT_no)) +
  geom_point() +
  scale_x_datetime(limits = as_datetime(c("2023-03-30 18:05:00", "2023-04-01 12:35:00"), 
                                        tz = "America/Los_Angeles")) +
  geom_vline(xintercept = as_datetime(c("2023-03-31 07:58:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) + # bubbler off
  geom_vline(xintercept = as_datetime(c("2023-04-01 10:31:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) # yeast added & lid closed
# all but 162475 (roving) went to zero with low-end calibration
#### NEED TO LOOK AT CALIBRATION DONE IN JUNE 2024

## 01-2024 (post 2023 field season)

post_2023_temp <- ggplot(data = post_2023, aes(x = date_time, y = Temp_C, color = miniDOT_no)) +
  geom_point() + 
  scale_x_datetime(limits = as_datetime(c("2024-01-16 19:16:00", "2024-01-18 12:45:00"), 
                                        tz = "America/Los_Angeles")) +
  geom_vline(xintercept = as_datetime(c("2024-01-17 09:23:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) + # bubbler off
  geom_vline(xintercept = as_datetime(c("2024-01-18 10:37:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) # yeast added & lid closed
# temperature for 521120 diverges part way through but maybe this is because an ice pack was placed
# closer to the other two than one because the temperature converges again after yeast was added

post_2023_DO <- ggplot(data = post_2023, aes(x = date_time, y = DO_mgL, color = miniDOT_no)) +
  geom_point() +
  scale_x_datetime(limits = as_datetime(c("2024-01-16 19:16:00", "2024-01-18 12:40:00"), 
                                        tz = "America/Los_Angeles")) +
  geom_vline(xintercept = as_datetime(c("2024-01-17 09:23:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) + # bubbler off
  geom_vline(xintercept = as_datetime(c("2024-01-18 10:37:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) # yeast added & lid closed
# all go down to zero for the low-end calibration which is great!

#### (3) Isolate a plateau after bubbler turned off ####

# we want to take a plateau period from at least 10 minutes after the sensor has been turned off
post_2022_plateau <- post_2022 %>% 
  filter(date_time >= as_datetime("2023-03-31 11:55:00", tz = "America/Los_Angeles")
         & date_time <= as_datetime("2023-03-31 12:05:00", tz = "America/Los_Angeles"))
# plots
post_2022_plateau_DO <- ggplot(data = post_2022_plateau, aes(x = date_time, y = DO_mgL, color = miniDOT_no)) +
  geom_point(size = 4) +
  geom_line()
post_2022_plateau_temp <- ggplot(data = post_2022_plateau, aes(x = date_time, y = Temp_C, color = miniDOT_no)) +
  geom_point(size = 4) +
  geom_line() # not sure if this is relevant

# doing same for 2023
post_2023_plateau <- post_2023 %>% 
  filter(date_time >= as_datetime("2024-01-17 12:00:00", tz = "America/Los_Angeles") 
         & date_time <= as_datetime("2024-01-17 12:10:00", tz = "America/Los_Angeles"))

# plots
post_2023_plateau_DO <- ggplot(data = post_2023_plateau, aes(x = date_time, y = DO_mgL, color = miniDOT_no)) +
  geom_point(size = 4) +
  geom_line()
post_2023_plateau_temp <- ggplot(data = post_2023_plateau, aes(x = date_time, y = Temp_C, color = miniDOT_no)) +
  geom_point(size = 4) +
  geom_line() # not sure if this is relevant

#### (4) Calculate oxygen saturation based on Garcia-Benson equation #####

## LEFT OFF HERE

### NEED TO DECIDE TO USE T AS AN AVERAGE OR SEPARATELY
                    