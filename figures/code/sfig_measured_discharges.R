#### USGS gage discharge vs. our discharge measurements
### Jordan Zabrecky
## last edited: 04.25.2025

# This figure shows that our discharge measurements are in rough accordance 
# with nearby USGS gage discharge measurements

#### (1) Loading libraries, our discharge data, and USGS discharge data ####

# Loading libraries
lapply(c("tidyverse", "lubridate", "dataRetrieval"), require, character.only = T)

# Loading our discharge data measured with SonTek Flowmeter 2 and calculated
# using the velocity-area method (NOTE: calculations done externally)
discharge <- read.csv("./data/EDI_data_package/discharge_measurements.csv")

# Loading USGS discharge data
USGS_gages <- c("11463000", "11476500", "11475800") # our gages
param <- "00060" # discharge param for USGS
USGS_discharge <- lapply(USGS_gages, function(x) 
  readNWISuv(x, param, "2022-06-23","2023-09-28")) # use dataRetrieval to get discharge
site_names <- c("RUS", "SFE-M", "SFE-SH") # site names in order of above
names(USGS_discharge) <- site_names # adding site names to list

#### (2) Cleaning dataframes ####

# create date_time column for our measured discharge data
discharge$date_time <- ymd_hms(discharge$date_time, tz = "America/Los_Angeles") # converting to POSIXct

# take second measurement (the better one) for SFE-SH on 8/24/2023
discharge <- discharge[-19,]

# function to clean USGS discharge data frame list
clean_discharge <- function(df) {
  df <- df %>% 
    # convert time zone as data is in UTC
    mutate(date_time = as_datetime(dateTime, tz = "America/Los_Angeles")) %>% 
    mutate(discharge_m3_s = X_00060_00000 / 35.31,
           hour = hour(date_time)) %>% 
    select(date_time, discharge_m3_s)
}

# apply function to data frame
USGS_discharge <- lapply(USGS_discharge, function(x) clean_discharge(x))

# trimming USGS_discharge dataset to fit field season we measured discharge in
USGS_discharge$`RUS` <- USGS_discharge$RUS %>% 
  filter(date_time >= "2022-06-24 00:00:00" & date_time <= "2022-09-16 00:00:00") %>% 
  mutate(site = "RUS")
USGS_discharge$`SFE-M` <- USGS_discharge$`SFE-M` %>% 
  filter(date_time >= "2022-06-29 00:00:00" & date_time <= "2022-09-18 00:00:00") %>% 
  mutate(site = "SFE-M")
USGS_discharge$`SFE-SH` <- USGS_discharge$`SFE-SH` %>% 
  dplyr::filter(date_time >= "2023-06-24 00:00:00" & date_time <= "2023-09-28 00:00:00") %>% 
  mutate(site = "SFE-SH")

# combining all into one data frame
discharge_all <- rbind(USGS_discharge$RUS, USGS_discharge$`SFE-M`, USGS_discharge$`SFE-SH`)

# left joining in our discharge measurements
discharge_all <- left_join(discharge_all, discharge, by = c("date_time", "site"))

#### (3) Making figure ####

# not sure if points need to be indicated that they are measured on plot or text
figure <- ggplot(data = discharge_all, aes(x = date_time, y = discharge_m3_s.x)) +
  geom_area(fill = "#d9ecff") +
  geom_point(aes(y = discharge_m3_s.y, fill = site, shape = site), 
             color = "black", size = 5, alpha = 0.9, stroke = 1.2) +
  facet_wrap(~site, ncol = 1, scales = "free", 
             labeller = as_labeller(c(`RUS` = "Russian River (RUS)", 
                                      `SFE-M` = "South Fork Eel River at Miranda (SFE-M)",
                                      `SFE-SH` = "South Fork Eel River at Standish Hickey (SFE-SH)"))) +
  scale_fill_manual(values = c("#bdb000", "#416f16", "#a8ff82")) +
  scale_shape_manual(values = c(24, 22, 23)) +
  labs(x = NULL, y = expression("Discharge (m"^3~"s"^-1*")")) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 2), axis.ticks = element_line(linewidth = 2.0),
        text = element_text(size = 24), axis.ticks.length=unit(.25, "cm"))
figure
