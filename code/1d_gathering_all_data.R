#### gathering all data to model metabolism
### Jordan Zabrecky
## last edited 05.20.2024

# This code gathers the necessary components for metabolism modeling
# including the (1) cleaned miniDOT data from "1a_reading_and_cleaning_miniDOT_data.R",
# (2) USGS gage discharge data, (3) GLDAS pressure data, 
# (4) NLDAS light and MODIS leaf area index data using StreamLight, and 
# (5) depth-discharge relationship information. In step (6) a final csv is 
# created with all this information to be used to model metabolism in "1e_modeling_metabolism"

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
# (I had issues with R 4.4.0, but it works with 4.2.3)

## Reading in miniDOT data
miniDOT_data <- ldply(list.files(path = "./data/miniDOT/", pattern = "_miniDOT.csv"), function(filename) {
  d <- read.csv(paste("data/miniDOT/", filename, sep = ""))
  d$file <- filename
  return(d)
})

# converting date_time from string to POSIXct class
miniDOT_data$date_time <- as_datetime(miniDOT_data$date_time)

#### (2) Retreiving USGS discharge data ####

# USGS site numbers
USGS_gages <- c("11463000", "11522500", "11476500", "11475800")

# mean daily discharge 
param <- "00060"

# use "DataRetrieval" to download data
discharge <- lapply(USGS_gages, function(x) readNWISuv(x, param, "2022-06-15","2023-10-01"))

# adding names of each river to list
names(discharge) <- c("russian", "salmon", "sfkeel_mir", "sfkeel_sth")

## Create 5-minute filled time series to match miniDOT and tidy up dataframes

# use "create_filled_TS" function from other script
source("code/1c_split_interpolate_data.R")

# function to apply to list of discharge dataframes
clean_discharge <- function(df) {
  df <- df %>% mutate(date_time = as_datetime(dateTime))
  new_df <- create_filled_TS(df, "5M", "X_00060_00000") %>% 
    mutate(discharge_m3_s = Filled_Var / 35.3147) %>% 
    dplyr::select(date_time, discharge_m3_s)
  return(new_df)
}

# applying function to dataframe list
discharge <- lapply(discharge, function(x) clean_discharge(x))

#### (3) Gathering GLDAS pressure data ####

# using functions from supporting script "1e_GLDAS_associated_functions.R"
source("code/1e_GLDAS_associated_functions.R")

# setting directory specs for download
path <- "H:/ATX-synchrony-norcal/data/GLDAS/" # where we will save the file
supporting <- "H:/ATX-synchrony-norcal/code/" # directory where we can access "1c_split_interpolate_data.R"

# downloading each site separately
baro_russian <- baro_dwld_processing("russian", 38.806883, -123.007017, "2022-06-15", 
                                     "2022-10-01", path, supporting)
baro_salmon <- baro_dwld_processing("salmon", 41.3771369, -123.4770326, "2022-06-15",
                                    "2023-10-01", path, supporting)
baro_sfkeel_mir <- baro_dwld_processing("sfkeel_mir", 40.198173, -123.775930, "2022-06-15", 
                                   "2023-10-01", path, supporting)
baro_sfkeel_sth <- baro_dwld_processing("sfkeel_sth", 39.876268, -123.727924, "2023-06-15", 
                                   "2023-10-01", path, supporting)

## I think this is all good to go now, just need to double check from beginning!