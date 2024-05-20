#### gathering all data to model metabolism
### Jordan Zabrecky
## 03.02.2024

url <- "https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-7.tar.gz"
install.packages(url, type = "source", repos = NULL)

# This code gathers the necessary components for metabolism modeling
# including the (1) cleaned miniDOT data from "1_reading_and_cleaning_DO_data.R,
# (2) USGS gage discharge data, (3) GLDAS pressure data, 
# (4) NLDAS light and MODIS leaf area index data using streamLight, and 
# (5) depth-discharge relationship information. In step (6) a final csv is 
# created with all this information to be used to model metabolism in "3_modeling_metabolism.R"

#### Loading packages and reading in data #### 
lapply(c("dataRetrieval","plyr", "tidyverse", "StreamLight", "StreamLightUtils",
         "zoo"), require, character.only = T)

#### (1) Gathering clean miniDOT data and making adjustments ####

# getting miniDOT data -- will deal with as a list later for now doing Miranda only!
setwd("~/metabolism-norcal-2022-23/data/miniDOT") # pulling from miniDOT folder
miniDOT <- ldply(list.files(pattern = "_miniDOT_clean.csv"), function(filename) {
  d <- read.csv(filename)
  d$file <- filename
  return(d)
})
# !!!dealing with this as a list of multiple files will take time when the work comes
sfkeel_mir_2023_miniDOT <- read.csv("sfkeel_mir_2023_miniDOT_clean.csv")
salmon_2023_miniDOT <- read.csv("salmon_2023_miniDOT.csv")
salmon_2022_miniDOT <- read.csv("salmon_2022_miniDOT.csv")

# converting date_time from character to POSIXct
miniDOT$date_time <- as.POSIXct(miniDOT$date_time, format = "%Y-%m-%d %H:%M:%S")
sfkeel_mir_2023_miniDOT$date_time <- as.POSIXct(sfkeel_mir_2023_miniDOT$date_time, format = "%Y-%m-%d %H:%M:%S")
salmon_2023_miniDOT$date_time <- as.POSIXct(salmon_2023_miniDOT$date_time, format = "%Y-%m-%d %H:%M:%S")
salmon_2022_miniDOT$date_time <- as.POSIXct(salmon_2022_miniDOT$date_time, format = "%Y-%m-%d %H:%M:%S")

# omit missing rows (as we preserved temperature but also removed DO from some days)
salmon_2022_miniDOT <- na.omit(salmon_2022_miniDOT)
salmon_2023_miniDOT <- na.omit(salmon_2023_miniDOT)

# function to round time to the nearest five minutes
round_5M <- function(df) {
  df$date_time <- round_date(df$date_time, "5 minutes")
  return(df)
}

# applying that function to miniDOT dataframes
miniDOT <- round_5M(miniDOT)
sfkeel_mir_2023_miniDOT <- round_5M(sfkeel_mir_2023_miniDOT)
salmon_2023_miniDOT <- round_5M(salmon_2023_miniDOT)
salmon_2022_miniDOT <- round_5M(salmon_2022_miniDOT)
class(salmon_2022_miniDOT$date_time)

# lastly need to interpolate values for missing DO and temperature will want to move this back to script 1 
# so you don't fill in places with more than 3 hours or whatever missing
setwd("~/metabolism-norcal-2022-23/code") 
# will use function from supporting R script "2_split_interpolate_data"
source("2b_split_interpolate_data.R")

### AGAIN WILL MOVE THIS BACK TO SCRIPT 1 LATER
sfkeel_mir_2023_DO <- Create_Filled_TS(sfkeel_mir_2023_miniDOT, "5M", "DO_mgL") %>% 
  dplyr::select(date_time, Filled_Var) %>% 
  rename(DO_mgL = Filled_Var)
sfkeel_mir_2023_temp <- Create_Filled_TS(sfkeel_mir_2023_miniDOT_final, "5M", "Temp_C") %>% 
  dplyr::select(date_time, Filled_Var) %>% 
  rename(Temp_C = Filled_Var)
# not doing this for salmon rerun

#### (2) Gathering USGS gage data using the "dataRetrieval" package ####

# will just gather per gage site rather than separating by year
sfkeel_mir_Q <- readNWISuv(siteNumber = "11476500", parameterCd = "00060", 
                         startDate = "2022-06-29", endDate = "2023-09-28",
                         tz = "America/Los_Angeles")
salmon_Q <- readNWISuv(siteNumber = "11522500", parameterCd = "00060", 
                       startDate = "2022-06-15", endDate = "2023-09-28",
                       tz = "America/Los_Angeles")
# try to do this as a list when we have multiple data frames

# renaming column DateTime as date_time for consistency with miniDOT data
sfkeel_mir_Q <- sfkeel_mir_Q %>% 
  mutate(date_time = dateTime) # rename was not working GOT IT TO WORK ABOVE
salmon_Q <- salmon_Q %>% 
  mutate(date_time = dateTime)

# need to interpolate between 15-minute intervals
sfkeel_mir_Q <- Create_Filled_TS(sfkeel_mir_Q, "5M", "X_00060_00000")
salmon_Q <- Create_Filled_TS(salmon_Q, "5M", "X_00060_00000")

# filter above
sfkeel_mir_Q <- sfkeel_mir_Q %>% 
  mutate(discharge_m3_s = Filled_Var / 35.3147) %>% 
  dplyr::select(date_time, discharge_m3_s)

salmon_Q <- salmon_Q %>% 
  mutate(discharge_m3_s = Filled_Var / 35.3147) %>% 
  dplyr::select(date_time, discharge_m3_s)

#### (3) Gathering GLDAS pressure data ####

# using supporting R script "2_GLDAS_associated_functions.R" to download and
# process GLDAS data
source("2c_GLDAS_associated_functions.R")

# getting saving directory info and info for main folder to use other scripts
supporting <- getwd() # path that will allow us to use functions from other scripts in main folder
setwd("~/metabolism-norcal-2022-23/data/GLDAS")
path <- getwd() # path that we will save our data download and processing to

# downloading and processing data for each site
sfkeel_mir_GLDAS <- Baro_dwld_processing("sfkeel_mir", 40.198173, -123.775930,"2022-06-15",
                                         "2023-09-30",path, supporting)
salmon_GLDAS <- Baro_dwld_processing("salmon", 41.3771369, -123.4770326,"2022-06-15",
                                     "2023-09-30",path, supporting)

#### (4) Gathering NLDAS light data & MODIS leaf area index data using "StreamLight" package ####

# If not already installed, use the devtools packge to install StreamLightUtils
#devtools::install_github("psavoy/StreamLightUtils")
#devtools::install_github("psavoy/StreamLight")

## (4.1) Downloading and processing NLDAS data

# making table for sites (reuse this abovefor GLDAS for full processing)
site_table <- as.data.frame(rbind(c("sfkeelmir", 40.198173, -123.775930),
                                  c("sfkeelsth", 39.876268, -123.727924),
                                  c("salmon", 41.3771369, -123.4770326),
                                  c("russian", 38.806883, -123.007017)))
colnames(site_table) <- c("Site_ID", "Lat", "Lon")

# downloading site NLDAS data with function from "StreamLightUtils"
NLDAS_DL_bulk(save_dir = "~/metabolism-norcal-22-23/data/NLDAS",
              site_locs = site_table, startDate = "2022-06-15")

# making list of downloaded sites from above (getting list from folder and removing "_NLDAS.asc")
site_list <- stringr::str_sub(list.files("~/metabolism-norcal-22-23/data/NLDAS"),
                              1, -11)

# processing the downloaded NLDAS data using function from "StreamLightUtils"
NLDAS_processed <- NLDAS_proc(read_dir = "~/metabolism-norcal-2022-23/data/NLDAS",
                              site_list)

NLDAS_proc <- function(read_dir, Site_IDs, write_output = FALSE, save_dir = NULL){
  #Get a list of all downloaded NLDAS data
  setwd(read_dir)
  downloaded <- list.files(read_dir)[grep("*_NLDAS.asc", list.files(read_dir))]
  
  #Get the names of downloaded sites
  downloaded_names <- stringr::str_sub(downloaded, 1, -11)
  
  #Function for processing each site
  NLDAS_site <- function(file_name, write_output, save_dir){
    #Reading in the table, skipping the first 40 lines of header information
    #and removing the last row which contains a calculated mean value
    nldas <- read.table(file_name, skip = 40, nrows = length(readLines(file_name,
                                                                       warn = FALSE)) - 41)
    
    colnames(nldas) <- c("Date", "hour_raw", "light")
    
    #Adding in date and time information
    #Extracting the hour information
    nldas[, "Time"] <- as.numeric(substr(nldas[,"hour_raw"], 1, 2))
    
    #Adding a POSIX time column
    nldas[, "pos_time"] <- as.POSIXct(paste(nldas[, "Date"], " ",
                                            as.matrix(sprintf("%02d", nldas[, "Time"])), sep = ""), format = "%Y-%m-%d %H",
                                      tz = "UTC")
    
    #Adding in Year, DOY, and hour information
    nldas[, "Year"] <- as.numeric(format(nldas[, "pos_time"], format = "%Y", tz = "UTC"))
    nldas[, "DOY"] <- as.numeric(format(nldas[, "pos_time"], format = "%j", tz = "UTC"))
    nldas[, "Hour"] <- as.numeric(format(nldas[, "pos_time"], format = "%H", tz = "UTC"))
    
    #Selecting the final column
    final <- nldas[, c("Year", "DOY", "Hour", "light")]
    colnames(final)[4] <- "SW"
    
    #If write_output == TRUE, save the driver to disk
    if(write_output == TRUE){
      saveRDS(final, paste0(save_dir, "/", stringr::str_sub(file_name, 1, -11), "_NLDAS_processed.rds"))
    } else{
      return(final)
    } #End if else statement
    
  } #End NLDAS_site function
  
  #Apply the function to make all driver files
  if(write_output == TRUE){
    lapply(downloaded, FUN = NLDAS_site, write_output = write_output, save_dir = save_dir)
  } else{
    processed <- lapply(downloaded, FUN = NLDAS_site, write_output = write_output, save_dir = save_dir)
    names(processed) <- downloaded_names
    
    return(processed)
  } #End if else statement    
  
  #Notify the user with a list of sites that did not have data
  missing <- Site_IDs[!(Site_IDs %in% downloaded_names)]
  
  if(length(missing) != 0){
    print(paste("The following sites did not successfully download NLDAS data:",
                paste(missing, sep = "", collapse = ", ")))
  } #End if statement
  
} #End NLDAS_proc

### code to be deleted after using streamlight
NLDAS_formatting <- function(df){
  
  df$origin <- as.Date(paste0(df$Year, "-01-01"),tz = "UTC") - days(1) # this seems stupid
  df$Date <- as.Date(df$DOY, origin = df$origin, tz = "UTC") 
  df$DateTime_UTC <- lubridate::ymd_hms(paste(df$Date, " ", df$Hour, ":00:00"))
  df <- df[,c("DateTime_UTC","SW")] #time zone clearly wrong
  # adjust time zone
  df$date_time <- with_tz(df$DateTime_UTC, tzone = "America/Los_Angeles")
  
  light <- df[,c("date_time","SW")]
  
  # Split, interpolate, and convert
  light_5M <- Create_Filled_TS(light, "5M", "SW")
  light_5M <- light_5M[,c("date_time","Filled_Var")]
  colnames(light_5M) <- c("date_time","SW")
  
  return(light_5M)
  
}

# again will do all as a list later
sfkeel_mir_light <- NLDAS_formatting(NLDAS_processed$sfkeel_mir)
df <- NLDAS_processed$sfkeel_mir
light_data <- lapply(NLDAS_processed, function(x) NLDAS_formatting(x))
salmon_light <- NLDAS_formatting(NLDAS_processed$salmon)

## (4.2) Downloading and processing MODIS leaf area index data

# setting working directory to MODIS folder and saving path info
setwd("~/metabolism-norcal-2022-23/data/MODIS")
path <- getwd()

## If MODIS data is not already downloaded...

# modify site table for MODIS request
MODIS_site_table <- site_table
colnames(MODIS_site_table) <- c("ID","Latitude","Longitude")
write.csv(MODIS_site_table, "NASA_Light_request.csv", row.names = FALSE)

# [EXTERNAL TO R]
# submit request following directions here: https://psavoy.github.io/StreamLight/articles/2%20Download%20and%20process%20MODIS%20LAI.html
# MODIS file from NASA is in folder; skipping to prelim metabolism for abstract just using NLDAS

## If MODIS data is already downloaded, continue here

# unpacking MODIS data obtained from NASA in zip file
MOD_unpack <- AppEEARS_unpack_QC(zip_file = "MODIS-metabolism-norcal-22-23.zip",
                                               zip_dir = path, MODIS_site_table[,"ID"])

# processing the MODIS data
MOD_processed <- AppEEARS_proc(unpacked_LAI = MOD_unpack,
                               # tested all available methods and "Kros" had best fit
                               # as determined by higher r-squared and lower RMSE
                               # Gu was second best
                               fit_method = "Klos",
                               plot=TRUE)

## all methods were unable to complete sfkeel-sth so we will use
## a simple linear interpolation to fill in the rest of the data

# finding out where NA's start (where filling in data was unfinished)
which(is.na(MOD_processed$sfkeelsth$LAI_proc))[1]
MOD_processed$sfkeelsth$Date[287]

#### (5) Depth-discharge relationship #### 

## just using previous equation from ESA for now...
# SFE$depth <- (0.13306*SFE$discharge) + 0.11178
sfkeel_mir_Q <- sfkeel_mir_Q %>% 
  mutate(depth_m = 0.13306 * discharge_m3_s + 0.11178)

# using relationship joanna created for salmon
salmon_Q <- salmon_Q %>% 
  mutate(depth_m = exp((0.32207*log(discharge_m3_s)) - 1.03866))

## place holder also for the salmon

#### (6) Putting everything together into a final data frame ####
sfkeel_mir_2023 <- list(sfkeel_mir_2023_DO, sfkeel_mir_2023_temp, sfkeel_mir_Q, sfkeel_mir_GLDAS, sfkeel_mir_light)
sfkeel_mir_2023 <- sfkeel_mir_2023 %>% reduce(left_join, by = "date_time")
# format date to avoid saving issues include timezone?
anyNA(sfkeel_mir_2023)
# saving as model input
salmon_2023 <- list(salmon_2023_miniDOT, salmon_Q, salmon_GLDAS, salmon_light)
salmon_2022 <- list(salmon_2022_miniDOT, salmon_Q, salmon_GLDAS, salmon_light)
salmon_2023 <- salmon_2023 %>% reduce(left_join, by = "date_time")
salmon_2022 <- salmon_2022 %>% reduce(left_join, by = "date_time")
which(is.na(salmon_2023))
which(is.na(salmon_2022))
salmon_2023[59067:60000]
# its saying there is NA but it is beyond the dataframe size?

# having issues with midnight time (00:00:00) not being preserved as a csv
# to get rid of this bug, I will just add one second now before saving it and then subtract
# the second later
sfkeel_mir_2023$date_time <- sfkeel_mir_2023$date_time + 1
salmon_2023$date_time <- salmon_2023$date_time + 1
salmon_2022$date_time <- salmon_2022$date_time + 1
setwd("~/metabolism-norcal-2022-23/data/model_inputs")
write.csv(salmon_2023, "salmon_2023_05162024.csv", row.names = FALSE)
write.csv(salmon_2022, "salmon_2022_05162024.csv", row.names = FALSE)
