### temporary- messing around with streamLight

library(tidyverse)
library(StreamLightUtils)
library(StreamLight)

base_wd <- getwd()
directory <- paste(base_wd, "/data/NLDAS", sep = "") # where NLDAS .asc files are located
NLDAS_processed <- NLDAS_proc(read_dir = directory, site_list)

miranda_NLDAS <- NLDAS_processed$sfkeel_mir

# to compare w/ base NLDAS
setwd("../..")
source("code/supplemental_code/S1a_split_interpolate_data.R")

NLDAS_formatting <- function(df){
  
  df$origin <- as.Date(paste0(df$Year, "-01-01"),tz = "UTC") - days(1)
  df$Date <- as.Date(df$DOY, origin = df$origin, tz = "UTC") 
  df$DateTime_UTC <- lubridate::ymd_hms(paste(df$Date, " ", df$Hour, ":00:00"))
  df <- df[,c("DateTime_UTC","SW")] #time zone clearly wrong
  # adjust time zone
  df$date_time <- with_tz(df$DateTime_UTC, tzone = "America/Los_Angeles")
  df$PAR_surface <- df$SW
  
  light <- df[,c("date_time","PAR_surface")]
  
  # Split, interpolate, and convert
  light_5M <- create_filled_TS(light, "5M", "PAR_surface")
  light_5M <- light_5M[,c("date_time","Filled_Var")]
  colnames(light_5M) <- c("date_time","PAR_surface")
  
  return(light_5M)
  
}

miranda_NLDAS_formatted <- NLDAS_formatting(miranda_NLDAS)

### messing w/ streamLight
setwd("data/MODIS")
directory <- getwd()

site_table <- as.data.frame(rbind(c("russian", 38.806883, -123.007017),
                                  c("salmon", 41.3771369, -123.4770326),
                                  c("sfkeel_mir", 40.198173, -123.775930),
                                  c("sfkeel_sth", 39.876268, -123.727924)))
colnames(site_table) <- c("Site_ID", "Lat", "Lon")

site_table$Lat <- as.numeric(site_table$Lat)
site_table$Lon <- as.numeric(site_table$Lon)
# unpacking MODIS data obtained from NASA in zip file

MODIS_unpack <- AppEEARS_unpack_QC(zip_file = "ATX-synchrony-norcal-MODIS.zip",
                                   zip_dir = directory, site_table[,"Site_ID"])

# processing the MODIS data
MODIS_processed <- AppEEARS_proc(unpacked_LAI = MODIS_unpack,
                                 # tested all available methods
                                 # "Gu" and "Klos" were best fits
                                 # based on higher r^2 and lower RMSE
                                 fit_method = "Gu",
                                 plot=TRUE)

MODIS_miranda <- MODIS_processed$sfkeel_mir # mean of 2.94, sd of .413; max of 3.6

MODIS_standishickey <- MODIS_processed$sfkeel_sth # mean of 2.839, sd of .280; max of 3.3

# adding coordinate reference system to site table
site_table$epsg_crs <- 4326

source("../../code/supplemental_code/S1c_modified_extract_height.R")

# making new table for information necessary to use StreamLight
streamLight_info <- site_table

# getting tree height for each site
for(i in 1:nrow(streamLight_info)) {
  streamLight_info$TH[i] <- extract_height(Site_ID = streamLight_info[i, "Site_ID"], 
                                           Lat = streamLight_info[i, "Lat"], 
                                           Lon = streamLight_info[i, "Lon"], 
                                           site_crs = streamLight_info[i, "epsg_crs"], 
                                           simard_loc = "../../data/MODIS/simard2011.asc")
}

# add column to data frame
streamLight_info$bottom_width = NA

# read in data
kayak_sfkeel <- read.csv("../depth_measurements/sfkeel_kayak_measurements.csv")
### kayak salmon river insert here ###
sontek <- read.csv("../depth_measurements/russian_sontek_discharge.csv")

# converting date as string to date object
kayak_sfkeel$Date <- mdy(kayak_sfkeel$Date)

# pulling down date for russian sontek discharge data
sontek <- sontek %>% 
  mutate_all(~replace(., . == "", NA)) %>%  # fill in missing values with NA
  fill(date, .direction = "down") # pull down the date
sontek$date <- mdy(sontek$date)

# south fork eel @ miranda & standish hickey
# use average width measurement across all accurately measured transects
# just using numbers as indexing rather than names due to small amount of sites :)

filter <- dplyr::filter
select <- dplyr::select
streamLight_info$bottom_width[3] = apply(kayak_sfkeel %>% filter(Site == "SfkEel_Miranda") %>%
                                           filter(Meas_Type == "Width") %>% 
                                           select(Width_m) %>% 
                                           na.omit(), 2,  mean)
streamLight_info$bottom_width[4] = apply(kayak_sfkeel %>% filter(Site == "SfkEel_Standish") %>% 
                                           filter(Meas_Type == "Width") %>% 
                                           select(Width_m) %>% 
                                           na.omit(), 2, mean)

# russian
streamLight_info$bottom_width[1] = apply(sontek %>% filter(depth_cm == 0) %>% 
                                           mutate(start_end = case_when(is.na(time) ~ "end",
                                                                        TRUE ~ "start")) %>% 
                                           select(date, start_end, distance_m) %>% 
                                           spread(key = start_end, value = distance_m) %>% 
                                           mutate(width_m = abs(start - end)) %>%
                                           select(width_m), 2, mean)

# salmon
## just going to assume same width as sfk eel @ miranda for now
## need to get transect widths from Laurel :)
streamLight_info$bottom_width[2] = streamLight_info$bottom_width[3]

## Channel azimuths

# estimated externally using satellite images
# russian = 135, salmon = 245, sfkeel_mir = 330, sfkeel_sth = 275
streamLight_info$azimuth <- c(135, 245, 330, 275)

## Bank height, bank slope, water level, overhang, overhang height, leaf angle distribution  
## (using defaults)
streamLight_info$BH <- rep(0.1, nrow(streamLight_info))
streamLight_info$BS <- rep(100, nrow(streamLight_info))
streamLight_info$WL <- rep(0.1, nrow(streamLight_info))
streamLight_info$overhang <- streamLight_info$TH * 0.1 # default is 10% of TH
streamLight_info$overhang_height <- rep(NA, nrow(streamLight_info)) # NA will assume 75% of TH
streamLight_info$X_LAD <- rep(1, nrow(streamLight_info))

#### TEST ONE- MIRANDA NLDAS WITH MODIS AND INPUTS FROM STANDISH HICKEY
SL_driver <- make_driver(site_table, NLDAS_processed , MODIS_processed, 
                               write_output = FALSE, save_dir = NULL)

#### TEST ONE- NULL - MIRANDA NLDAS AND MODIS AND SL INPUTS
test1 <- stream_light(
  SL_driver$sfkeel_mir, 
  Lat = 40.198173, # miranda lat
  Lon = -123.775930, 
  channel_azimuth = streamLight_info[3,7], 
  bottom_width = streamLight_info[3,6], 
  BH = 0.1, 
  BS = 100, 
  WL = 0.1, 
  TH = streamLight_info[3,5], 
  overhang =  streamLight_info[3,11], 
  overhang_height = NA, 
  x_LAD = 1
)

# plot one - NULL AKA what we are working with
test1_plot <- ggplot(data = miranda_NLDAS_formatted, aes(x = date_time, y = PAR_surface)) +
  geom_line(color = "green") +
  geom_line(data = test1, aes(x = local_time, y = PAR_surface), color = "blue") +
  #geom_line(data = subset2023_mir_2, aes(x = solar.time, y = light), color = "red") +
  ggtitle(label = "NLDAS, MODIS, SL inputs all Miranda") +
  theme_bw()

sub1_NLDAS <- miranda_NLDAS_formatted %>% 
  dplyr::filter(date_time >= "2023-08-15 00:00:00" & date_time <= "2023-08-23 00:00:00")
sub1_streamLight <- test1 %>% 
  dplyr::filter(local_time >= "2023-08-15 00:00:00" & local_time <= "2023-08-23 00:00:00")

test1_subplot <- ggplot(data = sub1_NLDAS, aes(x = date_time, y = PAR_surface)) +
  geom_line(color = "green") +
  geom_line(data = sub1_streamLight, aes(x = local_time, y = PAR_surface), color = "blue") +
  #geom_line(data = subset2023_mir_2, aes(x = solar.time, y = light), color = "red") +
  ggtitle(label = "NLDAS, MODIS, SL inputs all Miranda") +
  theme_bw()
# BIG BLUE PEAKS

#### TEST TWO- MIRANDA MODIS AND STANDISH HICKEY W/ STH INPUTS
test2 <- stream_light(
  SL_driver$sfkeel_mir, 
  Lat = 40.198173, # miranda lat 
  Lon = -123.775930,
  channel_azimuth = streamLight_info[4,7], # rest of params are sth
  bottom_width = streamLight_info[4,6], 
  BH = 0.1, 
  BS = 100, 
  WL = 0.1, 
  TH = streamLight_info[4,5], 
  overhang =  streamLight_info[4,11], 
  overhang_height = NA, 
  x_LAD = 1
)

# plot two - Miranda NLDAS vs StreamLight with inputs from STH
test2_plot <- ggplot(data = miranda_NLDAS_formatted, aes(x = date_time, y = PAR_surface)) +
  geom_line(color = "green") +
  geom_line(data = test2, aes(x = local_time, y = PAR_surface), color = "blue") +
  #geom_line(data = subset2023_mir_2, aes(x = solar.time, y = light), color = "red") +
  ggtitle(label = "NLDAS, MODIS Miranda, inputs STH") +
  theme_bw()
# definitely a noticeable difference between the two here

sub2_streamLight <- test2 %>% 
  dplyr::filter(local_time >= "2023-08-15 00:00:00" & local_time <= "2023-08-23 00:00:00")

test2_subplot <- ggplot(data = sub1_NLDAS, aes(x = date_time, y = PAR_surface)) +
  geom_line(color = "green") +
  geom_line(data = sub2_streamLight, aes(x = local_time, y = PAR_surface), color = "blue") +
  #geom_line(data = subset2023_mir_2, aes(x = solar.time, y = light), color = "red") +
  ggtitle(label = "NLDAS, MODIS, SL inputs all Miranda") +
  theme_bw()

# now blue is underestimating rather than over....

#### TEST TWO- MIRANDA MODIS AND STANDISH HICKEY W/ STH INPUTS
test3 <- stream_light(
  SL_driver$sfkeel_mir, 
  Lat = 40.198173, # miranda lat 
  Lon = -123.775930,
  channel_azimuth = streamLight_info[4,7], # rest of params are sth
  bottom_width = streamLight_info[4,6], 
  BH = 0.1, 
  BS = 100, 
  WL = 0.1, 
  TH = streamLight_info[3,5], # changing tree height and overhang back
  overhang =  streamLight_info[3,11], # changing tree height and overhang back to Mir
  overhang_height = NA, 
  x_LAD = 1
)

# plot two - Miranda NLDAS vs StreamLight with inputs from STH
test3_plot <- ggplot(data = miranda_NLDAS_formatted, aes(x = date_time, y = PAR_surface)) +
  geom_line(color = "green") +
  geom_line(data = test3, aes(x = local_time, y = PAR_surface), color = "blue") +
  #geom_line(data = subset2023_mir_2, aes(x = solar.time, y = light), color = "red") +
  ggtitle(label = "NLDAS, MODIS Miranda, inputs STH") +
  theme_bw()
# looks same as test 2

sub3_streamLight <- test3 %>% 
  dplyr::filter(local_time >= "2023-08-15 00:00:00" & local_time <= "2023-08-23 00:00:00")

test3_subplot <- ggplot(data = sub1_NLDAS, aes(x = date_time, y = PAR_surface)) +
  geom_line(color = "green") +
  geom_line(data = sub3_streamLight, aes(x = local_time, y = PAR_surface), color = "blue") +
  #geom_line(data = subset2023_mir_2, aes(x = solar.time, y = light), color = "red") +
  ggtitle(label = "NLDAS, MODIS, SL inputs all Miranda") +
  theme_bw()
# looks same as test 2