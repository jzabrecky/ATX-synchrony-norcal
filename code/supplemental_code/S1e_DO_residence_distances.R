#### Calculation of 95% dissolved oxygen turnover length (3v/K)
### Jordan Zabrecky
## last edited: 11.21.2025

# This script calculates the single-station metabolism dissolved turnover
# length in which 95% of the dissolved oxygen has turned over 
# calculated as 3v/K (Hall & Hotchkiss 2017)

#### (1) Loading libraries & Data ####

# loading libraries
lapply(c("tidyverse", "plyr", "lubridate", "dataRetrieval"), require, character.only = T)

# discharge (using saved USGS downloads rather than re-downloading them)
discharge <- ldply(list.files(path = "./data/USGS/",
                              pattern = "discharge_daily"), function(filename) {
                                d = read.csv(paste("./data/USGS/", filename, sep = "")) 
                              })

  # filter discharge for values during our study period
discharge <- discharge %>% 
  mutate(date = ymd(date)) %>% 
  filter(case_when(site == "russian" ~ date >= "2022-06-24" & date <= "2022-09-15",
                   site == "sfkeel_mir" ~ date >= "2022-06-29" & date <= "2023-09-27",
                   site == "sfkeel_mir" ~ date <= "2022-09-17" | date >= "2023-06-18",
                   site == "salmon" ~ date >= "2022-06-26" & date <= "2022-09-22",
                   site == "sfkeel_sth" ~ date >= "2023-06-24" & date <= "2023-09-27"))


# cross section areas for south fork eel
kayak <- read.csv("./data/EDI_data_package/kayak_depth_width.csv") %>% 
  # not using Russian bc we kayaked at flows way higher than what we observed 
  # during our sampling in 2022
  filter(site != "SAL" & site != "RUS") 

# loading saved USGS channel information for salmon river
salmon_channel <- read.csv("./data/USGS/salmon_geomorphology.csv")

# loading SonTek measured velocities for russian river
russian_velocity <- read.csv("./data/field_and_lab/russian_velocities.csv")

# gas-exchange rate
k600 <- ldply(list.files(path = "./data/metab_model_outputs_processed/",
                         pattern = "metab"), function(filename) {
                          d = read.csv(paste("./data/metab_model_outputs_processed/", filename, sep = "")) 
                         }) %>% 
  # remove estimates with biofouling
  filter(!site_year %in% c("russian_2022", "salmon_2022", "salmon_2023"))

#### (2) Calculating channel cross-section area ####

## (a) south fork eel river

# confirming number of depth per transect (should be 8)
depth_counts <- kayak %>% 
  filter(measurement_type == "depth") %>% 
  dplyr::group_by(field_date, site, transect, measurement_type) %>% 
  dplyr::summarize(total = length(meters))
unique(depth_counts$total) # all transects have 8 measurements

## using baby integrals (sum of rectangle areas) to get cross-section measurements
## dividing width by 8 and multiplying by each depth and then adding that all together
# calculate 1/8 width measurement
width <- kayak %>% 
  filter(measurement_type == "width") %>% 
  mutate(width_eight = meters / 8) %>% 
  select(!c("meters", "measurement_type"))
# add in 1/8 width measurements, multiply by depth, and add each rectangle together
xsections <- left_join(kayak %>% filter(measurement_type == "depth"), width, 
                   by = c("field_date", "site", "transect")) %>% 
  mutate(area = meters * width_eight) %>% 
  na.omit() %>%  # remove depths that don't have an accompanying width measurement
  dplyr::group_by(site, field_date, transect) %>% 
  dplyr::summarize(xsection_m2 = sum(area))

## (b) salmon river

# remove any channel information outside of our discharge range
salmon_channel <- salmon_channel %>% 
  # remove discharges above max during our study and below min during our study
  filter(discharge_m3_s <= max(discharge$discharge_m3_s[which(discharge$site == "salmon")]) &
           discharge_m3_s >= min(discharge$discharge_m3_s[which(discharge$site == "salmon")]))

#### (2) Get Median Values ####

# get median K600 estimates
median_k600 <- k600 %>% 
  mutate(river = case_when(grepl("sfkeel_mir", site_year) ~ "sfkeel_mir",
                           grepl("sfkeel_sth", site_year) ~ "sfkeel_sth",
                           grepl("salmon", site_year) ~ "salmon",
                           grepl("russian", site_year) ~ "russian")) %>% 
  dplyr::group_by(river) %>% 
  dplyr::summarize(median_k600 = median(K600_daily_mean))

# get median discharge during study period
median_discharge <- discharge %>% 
  dplyr::rename(river = site) %>% 
  dplyr::group_by(river) %>% 
  dplyr::summarize(median_discharge_m3_s = median(discharge_m3_s))
  
# get median channel cross-section for south fork eel rivers
median_xsection <- xsections %>% 
  dplyr::rename(river = site) %>% 
  dplyr::group_by(river) %>% 
  dplyr::summarize(median_xsection_m2 = median(xsection_m2)) %>% 
  mutate(river = case_when(river == "SFE-M" ~ "sfkeel_mir",
                           river == "SFE-SH" ~ "sfkeel_sth"))

# get median velocity for salmon & russian rivers
median_velocity <- rbind(salmon_channel %>% 
                       mutate(river = "salmon") %>% 
                       dplyr::group_by(river) %>% 
                       dplyr::summarize(median_velocity_m_s = median(velocity_m_s)),
                     russian_velocity %>% 
                       mutate(river = "russian") %>% 
                       # remove 0's because those were just the end of the channel (land)
                       filter(velocity_m_s != 0) %>% 
                       dplyr::group_by(river) %>% 
                       dplyr::summarize(median_velocity_m_s = median(velocity_m_s)))

# put dataframes together
medians <- (list(median_k600, median_discharge, median_xsection, median_velocity)) %>% 
  reduce(left_join, by = c("river")) %>% 
  mutate(velocity_m_s = case_when(river == "sfkeel_mir" | river == "sfkeel_sth" ~
                                     median_discharge_m3_s / median_xsection_m2,
                                   TRUE ~ median_velocity_m_s))

#### (3) Calculate Footprint Distance ####

# calculate 3v/K
footprint_distance <- medians %>% 
  # convert velocity in m/s to m /d
  mutate(velocity_m_d = velocity_m_s * 86400) %>% 
  # note that K600 is 1/d units so * (m / d) * (d / 1), then divide by 1000 for km
  mutate(turnover_distance_km = (3 * velocity_m_d) / median_k600 / 1000) %>% 
  select(river, velocity_m_s, velocity_m_d, median_k600, turnover_distance_km)
