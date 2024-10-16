#### OLD STREAMLIGHT CODE ####

## Loading necessary packages
lapply(c("dataRetrieval", "lubridate", "plyr", "tidyverse", "StreamLight", "StreamLightUtils",
         "zoo"), require, character.only = T)

## if "StreamLight" & "StreamLightUtils" have not yet been downloaded...
#devtools::install_github("psavoy/StreamLightUtils")
#devtools::install_github("psavoy/StreamLight")

## NOTE: "StreamLightUtils" uses the "rgdal" package which has been discontinued
## you can obtain an old version of the package as follows:
#url <- "https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-6.tar.gz"
#install.packages(url, type = "source", repos = NULL)
# However, there may be issues doing this with later version of R 
# (I had issues with R 4.4.0, but it works with 4.2.3 and 4.3.2)

#### (4) Gathering NLDAS light data & MODIS leaf area index using "StreamLight" package ####

## (a) downloading and processing NLDAS data

# making site table to use for NLDAS and MODIS data downloads
site_table <- as.data.frame(rbind(c("russian", 38.806883, -123.007017),
                                  c("salmon", 41.3771369, -123.4770326),
                                  c("sfkeel_mir", 40.198173, -123.775930),
                                  c("sfkeel_sth", 39.876268, -123.727924)))
colnames(site_table) <- c("Site_ID", "Lat", "Lon")

### CURRENT TESTING: light at current Miranda GPS point seems weird- tree shade cover?? reselecting
# something that is more representative of the entire dissolved oxygen footprint
site_table <- as.data.frame(rbind(c("russian", 38.806883, -123.007017),
                                  c("salmon", 41.3771369, -123.4770326),
                                  # note the slight change below
                                  c("sfkeel_mir", 40.198216, -123.775777),
                                  c("sfkeel_sth", 39.876268, -123.727924)))
colnames(site_table) <- c("Site_ID", "Lat", "Lon")
### only getting MODIS data not changing NLDAS for now :)

# make sure latitude and longitude are numeric to avoid future error
site_table$Lat <- as.numeric(site_table$Lat)
site_table$Lon <- as.numeric(site_table$Lon)

# downloading site NLDAS data with function from "StreamLightUtils"
NLDAS_DL_bulk(save_dir = "data/NLDAS",
              site_locs = site_table, startDate = "2022-06-15")

# making list of downloaded sites from above (getting list from folder and removing "_NLDAS.asc")
site_list <- stringr::str_sub(list.files("data/NLDAS"), 1, -11)

# processing the downloaded NLDAS data using function from "StreamLightUtils"
directory <- paste(base_wd, "/data/NLDAS", sep = "") # where NLDAS .asc files are located
NLDAS_processed <- NLDAS_proc(read_dir = directory, site_list)

#### 8/5/2024 TRYING VERSION WITH JUST NLDAS DATA
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
NLDAS_formatted <- lapply(NLDAS_processed, function(x) NLDAS_formatting(x))

## (b) downloading and processing MODIS data

# changing working directory
setwd("../../data/MODIS")
directory <- getwd() # save site table to MODIS folder

# writing table to submit data request to NASA
write.table(site_table, paste0(directory, "/sitestake2.csv"), sep = ",", row.names = FALSE,
            quote = FALSE, col.names = FALSE
)

## [EXTERNAL TO R]
# submit request following directions here: 
# https://psavoy.github.io/StreamLight/articles/2%20Download%20and%20process%20MODIS%20LAI.html

# unpacking MODIS data obtained from NASA in zip file
MODIS_unpack <- AppEEARS_unpack_QC(zip_file = "ATX-synchrony-norcal-MODIS.zip",
                                   zip_dir = directory, site_table[,"Site_ID"])
### CONTINUE HERE: will temporarily try alternate GPS point

# processing the MODIS data
MODIS_processed <- AppEEARS_proc(unpacked_LAI = MODIS_unpack,
                                 # tested all available methods
                                 # "Gu" and "Klos" were best fits
                                 # based on higher r^2 and lower RMSE
                                 fit_method = "Gu",
                                 plot=TRUE)

## (c) use "StreamLight" package

# adding coordinate reference system to site table
site_table$epsg_crs <- 4326

# make driver
SL_driver <- make_driver(site_table, NLDAS_processed, MODIS_processed, 
                         write_output = FALSE, save_dir = NULL)

## Extract tree height for each site

# use modified version of script that uses local download of Simard et al. 2011
# obtained from https://webmap.ornl.gov/ogc/dataset.jsp?ds_id=10023
# and only returns the tree height instead of a full table
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

## River width

# currently, only have kayak depth measurements for the south fork eel
# for now, will use depth measurements from measuring discharge for the russian
# since the river was much higher in 2023 when we did the kayak run
# but could also just maybe still use those widths because they may be more accurate?

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

## Run StreamLight model for each site

# change working directory
setwd("../StreamLight")

# function to batch run models
StreamLight_batch_models <- function(site, driver_file, save_dir){
  
  # get model parameters for the site
  site_p <- streamLight_info[streamLight_info[, "Site_ID"] == site, ]
  
  # get specific site driver
  site_driver <- driver_file[[site]]
  
  # run the model
  modeled <- stream_light(
    site_driver, #[site]? 
    Lat = site_p[, "Lat"], 
    Lon = site_p[, "Lon"],
    channel_azimuth = site_p[, "azimuth"], 
    bottom_width = site_p[, "bottom_width"], 
    BH = site_p[, "BH"],
    BS = site_p[, "BS"], 
    WL = site_p[, "WL"], 
    TH = site_p[, "TH"], 
    overhang = site_p[, "overhang"],
    overhang_height = site_p[, "overhang_height"], 
    x_LAD = site_p[, "X_LAD"]
  )
  
  # save the output
  saveRDS(modeled, paste(save_dir, "/", site, "_predicted.rds", sep = ""))
  
  # add object to list 
  return(modeled)
}

# set save directory
directory <- getwd()

# apply function for all sites (confirmed code success by comparing with single site runs!)
streamLight_modeled <- lapply(streamLight_info[,"Site_ID"], function(x) StreamLight_batch_models(x, SL_driver, directory))

# adding appropriate names to streamLight data list
names(streamLight_modeled) <- site_names

# function to apply to list of modeled streamLight dataframes
clean_streamLight <- function(df) {
  df <- df %>% rename(date_time = local_time)
  new_df <- create_filled_TS(df, "5M", "PAR_surface") %>% 
    select(date_time, Filled_Var) %>% 
    rename(PAR_surface = Filled_Var)
  return(new_df)
}

# applying function to dataframe list
streamLight_processed <- lapply(streamLight_modeled, function(x) clean_streamLight(x))