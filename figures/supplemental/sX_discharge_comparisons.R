#### Figure SX. USGS discharge versus our measured discharge
### Jordan Zabrecky
## last edited: 08.30.2024

# This figure shows that our discharge measurements are in rough accordance 
# with nearby USGS gage discharge measurements

#### (1) Loading libraries, our discharge data, and USGS discharge data ####

# Loading libraries
# loading libraries
lapply(c("tidyverse", "lubridate", "dataRetrieval"), require, character.only = T)

# Loading our discharge data measured with SonTek Flowmeter 2 and calculated
# using the velocity-area method (NOTE: calculations done externally)
discharge <- read.csv("./data/field_and_lab/raw_data/sontek_discharge.csv")

# Loading USGS discharge data
USGS_gages <- c("11463000", "11476500", "11475800") # our gages
param <- "00060" # discharge param for USGS
USGS_discharge <- lapply(USGS_gages, function(x) 
  readNWISuv(x, param, "2022-06-23","2023-09-28")) # use dataRetrieval to get discharge
site_names <- c("russian", "sfkeel_mir", "sfkeel_sth") # site names in order of above
names(USGS_discharge) <- site_names # adding site names to list

#### (2) Cleaning dataframes ####

# create date_time column for our measured discharge data
discharge$date_time <- paste(discharge$date, discharge$time, sep = " ") # combining columns
discharge$date_time <- mdy_hm(discharge$date_time, tz = "America/Los_Angeles") # converting to POSIXct

# function to clean USGS discharge data frame list
clean_discharge <- function(df) {
  df <- df %>% 
    # convert time zone as data is in UTC
    mutate(date_time = as_datetime(dateTime, tz = "America/Los_Angeles")) %>% 
    rename(discharge_m3_s = X_00060_00000) %>% # rename parameter to discharge
    select(date_time, discharge_m3_s)
}

# apply function to data frame
USGS_discharge <- lapply(USGS_discharge, function(x) clean_discharge(x))

# trimming USGS_discharge dataset to fit field season we measured discharge in
USGS_discharge$russian <- USGS_discharge$russian %>% 
  filter(date_time >= "2022-06-24 00:00:00" & date_time <= "2022-09-16 00:00:00")
USGS_discharge$sfkeel_mir <- USGS_discharge$sfkeel_mir %>% 
  filter(date_time >= "2022-06-29 00:00:00" & date_time <= "2022-09-18 00:00:00")
USGS_discharge$sfkeel_sth <- USGS_discharge$sfkeel_sth %>% 
  filter(date_time >= "2023-06-24 00:00:00" & date_time <= "2023-09-28 00:00:00")

#### (3) Making figure ####

# facet wrap? on points and then add lines for discharge
# will be interesting to add lines for discharge
# also need individual scale for each plot
figure <- ggplot(data = discharge, aes(x = date_time, y = discharge_m3_s)) +
  geom_point(aes(color = site)) +
  facet_wrap(~site, scales = "free") +
  scale_color_manual(values = c("#bdb000", "#416f16", "#8bde43")) +
  theme_bw()

## LOOK AT EXAMPLES FROM HEILI!!
