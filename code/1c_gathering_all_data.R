#### gathering all data to model metabolism
### Jordan Zabrecky
## last edited 05.20.2024

# This code gathers the necessary components for metabolism modeling
# including the (1) cleaned miniDOT data from "1a_reading_and_cleaning_miniDOT_data.R",
# (2) USGS gage discharge data, (3) GLDAS pressure data, 
# (4) NLDAS light and MODIS leaf area index data using StreamLight, and 
# (5) depth-discharge relationship information. In step (6) a final csv is 
# created with all this information to be used to model metabolism in "1g_modeling_metabolism"

#### (1) Loading packages and reading in data #### 

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

## Reading in miniDOT data
miniDOT_data <- ldply(list.files(path = "./data/miniDOT/", pattern = "_miniDOT.csv"), function(filename) {
  d <- read.csv(paste("data/miniDOT/", filename, sep = ""))
  d$file <- filename
  return(d)
})

# converting date_time from character to POSIXct class & indicate time zone
miniDOT_data$date_time <- as_datetime(miniDOT_data$date_time, tz = "America/Los_Angeles")

#### (2) Retreiving USGS discharge data ####

# USGS site numbers
USGS_gages <- c("11463000", "11522500", "11476500", "11475800")

# mean daily discharge in cfs
param <- "00060"

# use "DataRetrieval" to download data
discharge <- lapply(USGS_gages, function(x) readNWISuv(x, param, "2022-06-15","2023-10-01"))

# adding names of each river to list
site_names <- c("russian", "salmon", "sfkeel_mir", "sfkeel_sth")
names(discharge) <- site_names

## Create 5-minute filled time series to match miniDOT and tidy up dataframes

# use "create_filled_TS" function from other script
source("code/supplemental_code/S1a_split_interpolate_data.R")

# function to apply to list of discharge dataframes
clean_discharge <- function(df) {
  df <- df %>% mutate(date_time = as_datetime(dateTime, tz = "America/Los_Angeles"))
  new_df <- create_filled_TS(df, "5M", "X_00060_00000") %>% 
    mutate(discharge_m3_s = Filled_Var / 35.3147) %>% 
    dplyr::select(date_time, discharge_m3_s)
  return(new_df)
}

# applying function to dataframe list
discharge <- lapply(discharge, function(x) clean_discharge(x))

#### (3) Gathering GLDAS pressure data ####

# using functions from supporting script "S1b_GLDAS_associated_functions.R"
source("code/supplemental_code/S1b_GLDAS_associated_functions.R")

# setting directory specs for download
path <- "data/GLDAS/" # where we will save the file

# directory where we can access "S1a_split_interpolate_data.R" while using the GLDAS script
base_wd <- getwd() # saving our base working directory
supporting <- paste(base_wd, "/code/supplemental_code/", sep = "")

# downloading GLDAS .asc file and processing it into a saved .csv file
baro_dwld_processing("russian", 38.806883, -123.007017, "2022-06-15", 
                     "2022-10-01", path, "America/Los_Angeles")
baro_dwld_processing("salmon", 41.3771369, -123.4770326, "2022-06-15",
                     "2023-10-01", path, "America/Los_Angeles")
baro_dwld_processing("sfkeel_mir", 40.198173, -123.775930, "2022-06-15", 
                     "2023-10-01", path, "America/Los_Angeles")
baro_dwld_processing("sfkeel_sth", 39.876268, -123.727924, "2023-06-15", 
                     "2023-10-01", path, "America/Los_Angeles")

# making 5-min interpolated data frames
GLDAS_processed <- lapply(site_names, function(x) baro_make_df(path, x, "America/Los_Angeles", supporting))
names(GLDAS_processed) <- site_names # adding names to list

#### (4) Gathering NLDAS light data & MODIS leaf area index using "StreamLight" package ####

## (a) downloading and processing NLDAS data

# making site table to use for NLDAS and MODIS data downloads
site_table <- as.data.frame(rbind(c("russian", 38.806883, -123.007017),
                                  c("salmon", 41.3771369, -123.4770326),
                                  c("sfkeel_mir", 40.198173, -123.775930),
                                  c("sfkeel_sth", 39.876268, -123.727924)))
colnames(site_table) <- c("Site_ID", "Lat", "Lon")

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

## (b) downloading and processing MODIS data

# changing working directory
setwd("../../data/MODIS")
directory <- getwd() # save site table to MODIS folder

# writing table to submit data request to NASA
write.table(site_table, paste0(directory, "/sites.csv"), sep = ",", row.names = FALSE,
  quote = FALSE, col.names = FALSE
)

## [EXTERNAL TO R]
# submit request following directions here: 
# https://psavoy.github.io/StreamLight/articles/2%20Download%20and%20process%20MODIS%20LAI.html

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

# read in data
setwd("../depth_measurements")
kayak_sfkeel <- read.csv("sfkeel_kayak_measurements.csv")
### kayak salmon river insert here ###
sontek <- read.csv("russian_sontek_discharge.csv")

# converting date as string to date object
kayak_sfkeel$Date <- mdy(kayak$Date)

# pulling down date for russian sontek discharge data
sontek <- sontek %>% 
  mutate_all(~replace(., . == "", NA)) %>%  # fill in missing values with NA
  fill(date, .direction = "down") # pull down the date
sontek$date <- mdy(sontek$date)

# get rid of dplyr masking!!
filter <- dplyr::filter
select <- dplyr::select

# south fork eel @ miranda & standish hickey
# use average width measurement across all accurately measured transects
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
streamLight_info$BH <- rep(0.1, 4)
streamLight_info$BS <- rep(100, 4)
streamLight_info$WL <- rep(0.1, 4)
streamLight_info$overhang <- streamLight_info$TH * 0.1 # default is 10% of TH
streamLight_info$overhang_height <- rep(NA, 4) # NA will assume 75% of TH
streamLight_info$X_LAD <- rep(1, 4)

## Run StreamLight model for each site

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
directory <- setwd("../../data/StreamLight")

# apply function for all sites (confirmed code success by comparing with single site runs!)
streamLight_processed <- lapply(streamLight_info[,"Site_ID"], function(x) StreamLight_batch_models(x, SL_driver, directory))
names(streamLight_processed) <- site_names

#### (5) Incorporating depth-discharge relationship ####

## russian 

# using past relationship from transect of discharge measurement for now...
discharge$russian$depth_m <- (0.08601 * discharge$russian$discharge_m3_s) + 0.31433

## salmon

# using past relationship modeled with USGS channel cross-section data
discharge$salmon$depth_m <- exp((0.32207 * log(discharge$salmon$discharge_m3_s)) - 1.03866)
SAL$depth <- exp((0.32207*log(SAL$discharge)) - 1.03866)

## south fork eel @ miranda


# NEED TO REPLACE THIS
SFE$depth <- (0.13306*SFE$discharge) + 0.11178
sfkeel_mir_Q <- discharge$sfkeel_mir
sfkeel_mir_Q$depth <- (0.13306*sfkeel_mir_Q$discharge_m3_s) + 0.11178

## south fork eel @ standish hickey

#### (6) Putting it all together ####

## this is quickly done to output a couple more metabolism estimates
##


### OLD- throw out later!!!
NLDAS_formatting <- function(df){
  
  df$origin <- as.Date(paste0(df$Year, "-01-01"),tz = "UTC") - days(1) # this seems stupid
  df$Date <- as.Date(df$DOY, origin = df$origin, tz = "UTC") 
  df$DateTime_UTC <- lubridate::ymd_hms(paste(df$Date, " ", df$Hour, ":00:00"))
  df <- df[,c("DateTime_UTC","SW")] #time zone clearly wrong
  # adjust time zone
  df$date_time <- with_tz(df$DateTime_UTC, tzone = "America/Los_Angeles")
  
  light <- df[,c("date_time","SW")]
  
  # Split, interpolate, and convert
  light_5M <- create_filled_TS(light, "5M", "SW")
  light_5M <- light_5M[,c("date_time","Filled_Var")]
  colnames(light_5M) <- c("date_time","SW")
  
  return(light_5M)
  
}

NLDAS_sfkeel_mir <- NLDAS_formatting(NLDAS_processed$sfkeel_mir) %>% 
  dplyr::filter(date_time <= "2022-10-01 00:00:00")
attr(NLDAS_sfkeel_mir$date_time,"tzone")
attr(sfkeel_mir_DO$date_time,"tzone") # this shouldn't be UTC
attr(baro_sfkeel_mir$date_time, "tzone") # double check this is PST
attr(sfkeel_mir_Q$date_time, "tzone")
sfkeel_mir_DO <- miniDOT_data %>% 
  dplyr::filter(file == "sfkeel_mir_2022_miniDOT.csv") %>% 
  dplyr::select(date_time, Temp_C, DO_mgL)
class(NLDAS_sfkeel_mir$date_time)
class(sfkeel_mir_DO$date_time)
sfkeelmir <- list(sfkeel_mir_DO, NLDAS_sfkeel_mir, baro_sfkeel_mir, sfkeel_mir_Q)
sfkeelmir <- sfkeelmir %>% reduce(left_join, by = "date_time")
anyNA(sfkeelmir) # yay!

NLDAS_russian <- NLDAS_formatting(NLDAS_processed$russian)
russian_DO <- miniDOT_data %>% 
  dplyr::filter(file == "russian_2022_miniDOT.csv") %>% 
  dplyr::select(date_time, Temp_C, DO_mgL)
russian <- list(russian_DO, NLDAS_russian, baro_russian, russian_Q)
russian <- russian %>% reduce(left_join, by = "date_time")
anyNA(russian)

# hopefully don't have 00:00:00 issue....
class(sfkeelmir$date_time)
attr(sfkeelmir$date_time, "tzone")
class(russian$date_time)
attr(russian$date_time, "tzone")
getwd()
setwd("..")
write.csv(sfkeelmir, "data/metab_model_inputs/sfkeel_mir_2022_05232024.csv", row.names = FALSE)
write.csv(russian, "data/metab_model_inputs/russian_2022_05232024.csv", row.names = FALSE)
?write.csv()
## MAY WANT TO CHANGE PRESSURE TO WHAT IS NEEDED FOR METAB EARL
