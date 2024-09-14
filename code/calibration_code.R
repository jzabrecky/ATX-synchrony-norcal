#### intercalibration for miniDOTs for 2022 and 2023 field season
### Jordan Zabrecky
## last edited: 09.11.2024

# This code calibrates miniDOT dissolved oxygen sensors via the bucket method
# and then relates the dissolved oxygen measurements to the estimated 100% DO saturation
# as calculated by the Garcia and Gordon (1992) equation. Offsets are then calculated
# and applied to the miniDOT data in the "1c_gathering_all_data.R" script

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

# ------ brief aside-- looking at later calibration to see if 162475 went to zero ------
path <- "./data/miniDOT/intercalibrations/202406/" # setting path to june folder
file_list <- list.files(path) # getting list of files in that folder
june_list <- lapply(file_list, function(x) create_df(paste(path, x, sep = ""))) # reading in files
for(i in 1:length(june_list)) {
  june_list[[i]]$miniDOT_no <- file_list[i] # adding miniDOT number to column
}
june <- reduce(june_list, rbind) # reducing list
june <- time_fix(june) # applying time fix
june_DO <- ggplot(data = june, aes(x = date_time, y = DO_mgL, color = miniDOT_no)) +
  geom_point() +
  geom_vline(xintercept = as_datetime(c("2024-06-14 14:07:00"), tz = "America/Los_Angeles"), 
             color = "darkgray", linetype = 2) # yeast added & lid closed
# this low-end calibration went to zero for this sensor during this calibration so I 
# am not worried about the sensor; possibly some bubbles were stuck on the sensor
# during the previous calibration
# ------ end aside ---------------------------------------------------------------------

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
# closer to this one than the other two because the temperature converges again after yeast was added

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

# create a df to summarize this information
post_2022_plateau_summary <- post_2022_plateau %>% 
  dplyr::group_by(miniDOT_no) %>% 
    dplyr::summarize(DO_mgL_mean = mean(DO_mgL),
              DO_mgL_sd = sd(DO_mgL),
              Temp_C_mean = mean(Temp_C),
              Temp_C_sd = sd(Temp_C),
              pressure_mmHg = 648.0) # pressure at 12pm 3/31

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

# create a df to summarize this information
post_2023_plateau_summary <- post_2023_plateau %>% 
  dplyr::group_by(miniDOT_no) %>% 
  dplyr::summarize(DO_mgL_mean = mean(DO_mgL),
            DO_mgL_sd = sd(DO_mgL),
            Temp_C_mean = mean(Temp_C),
            Temp_C_sd = sd(Temp_C),
            pressure_mmHg = 644.8) # pressure at 12:05pm 1/17

#### (4) Calculate DO for 100% saturation based on Garcia-Benson equation #####

# function to estimate DO for 100% oxygen saturation from Garcia and Gordon (1992)
# takes temperature in C and barometric pressure in mmHg
# calculation is a slight approximation because we do not account for water density
# but difference is very small (0.01 mg/L at 20 deg)
oxygen_sat <- function(temp_C , pressure_mmHg){
  sat_DO <- (exp(2.00907 + 3.22014 * (log((298.15 - temp_C) / (273.15 + temp_C))) + 4.0501 * 
               (log((298.15 - temp_C) / (273.15 + temp_C))) ^ 2 + 4.94457 * 
               (log((298.15 - temp_C) / (273.15 + temp_C))) ^ 3 - 0.256847 * 
               (log((298.15 - temp_C) / (273.15 + temp_C))) ^ 4 + 3.88767 * 
               (log((298.15 - temp_C) / (273.15 + temp_C))) ^ 5)) * 1.4276 * pressure_mmHg / 760
  
  return(sat_DO)
}

# worried that temperature average for sensor 521120 is off wrong since it was below others
# for this time period (likely because it was closer to ice pack??)
# so filling in the average of the over two temperatures here
post_2023_plateau_summary$Temp_C_mean[1] <- mean(post_2023_plateau_summary$Temp_C_mean[2:3])
post_2023_plateau_summary$Temp_C_sd[1] <- mean(post_2023_plateau_summary$Temp_C_sd[2:3])

# apply function to each summary data frame & then calculate offset from sensor's DO reading
post_2022_plateau_summary <- post_2022_plateau_summary %>% 
  mutate(est_oxygen_sat = oxygen_sat(Temp_C_mean, pressure_mmHg)) %>% 
  mutate(offset = est_oxygen_sat - DO_mgL_mean)
post_2023_plateau_summary <- post_2023_plateau_summary %>% 
  mutate(est_oxygen_sat = oxygen_sat(Temp_C_mean, pressure_mmHg)) %>% 
  mutate(offset = est_oxygen_sat - DO_mgL_mean)

#### (5) Look at offsets applied to data and save information ####

# apply offsets to data
post_2022_plateau <- post_2022_plateau %>% 
  mutate(offset_DO_mgL = case_when(miniDOT_no == "162475" ~ (DO_mgL + post_2022_plateau_summary$offset[1]),
                                   miniDOT_no == "521120" ~ (DO_mgL + post_2022_plateau_summary$offset[2]),
                                   miniDOT_no == "529728" ~ (DO_mgL + post_2022_plateau_summary$offset[3]),
                                   miniDOT_no == "663402" ~ (DO_mgL + post_2022_plateau_summary$offset[4])))

post_2023_plateau <- post_2023_plateau %>% 
  mutate(offset_DO_mgL = case_when(miniDOT_no == "521120" ~ (DO_mgL + post_2023_plateau_summary$offset[1]),
                                   miniDOT_no == "529728" ~ (DO_mgL + post_2023_plateau_summary$offset[2]),
                                   miniDOT_no == "663402" ~ (DO_mgL + post_2023_plateau_summary$offset[3])))

# plotting data with new offsets
post_2022_offset <- ggplot(data = post_2022_plateau, aes(x = date_time, y = offset_DO_mgL, color = miniDOT_no)) +
  geom_point(size = 4) +
  geom_line()
post_2023_offset <- ggplot(data = post_2023_plateau, aes(x = date_time, y = offset_DO_mgL, color = miniDOT_no)) +
  geom_point(size = 4) +
  geom_line()
                                  
### compare offsets for same sensor!! need to decide on before or after; group together
# 529728: +.700 vs. +.765
# 663402: +.478 vs. +.534
# 521120: +.582 vs. +.507
# note: offsets are positive- meaning our measured DO is less than the 
# estimated DO at 100% saturation!

# adding "site_year" information to each sensor to match with miniDOT data
post_2022_plateau_summary$site_year <- "roving_sensor"
post_2022_plateau_summary$site_year[which(post_2022_plateau_summary$miniDOT_no == 521120)] <- "salmon_2022"
post_2022_plateau_summary$site_year[which(post_2022_plateau_summary$miniDOT_no == 663402)] <- "sfkeel_mir_2022"
post_2022_plateau_summary$site_year[which(post_2022_plateau_summary$miniDOT_no == 529728)] <- NA # this sensor was not deployed in 2022
post_2023_plateau_summary$site_year <- "salmon_2023"
post_2023_plateau_summary$site_year[which(post_2023_plateau_summary$miniDOT_no == 521120)] <- "sfkeel_sth_2023"
post_2023_plateau_summary$site_year[which(post_2023_plateau_summary$miniDOT_no == 663402)] <- "sfkeel_mir_2023"

# save csv's
write.csv(post_2022_plateau_summary, "./data/miniDOT/intercalibrations/offsets_2022.csv", row.names = FALSE)
write.csv(post_2023_plateau_summary, "./data/miniDOT/intercalibrations/offsets_2023.csv", row.names = FALSE)
