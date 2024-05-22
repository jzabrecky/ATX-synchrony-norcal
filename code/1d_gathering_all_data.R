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
path <- "data/GLDAS/" # where we will save the file
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
directory <- "H:/ATX-synchrony-norcal/data/NLDAS" # where NLDAS .asc files are located
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
source("../../code/1f_modified_extract_height.R")

# making empty data frame for tree heights for each site
tree_heights <- data.frame(matrix(ncol = 4))
colnames(tree_heights) <- c("Site_ID", "Lat", "Lon", "TH")
                           
# getting tree height for each site
for(i in 1:length(site_table)) {
  temp <- extract_height(Site_ID = site_table[i, "Site_ID"], Lat = site_table[i, "Lat"], 
                                    Lon = site_table[i, "Lon"], site_crs = site_table[i, "epsg_crs"], 
                                    simard_loc = "../../data/MODIS/simard2011.asc")
  tree_heights <- bind_rows(temp[,1:4], tree_heights)
  tree_heights[-(length(site) + 1)]
}

# removing empty row from data frame
tree_heights <- tree_heights[-(length(site_list) + 1),]

## Estimate width based on mode of discharge
#### HOW SHOULD I DO THIS??

#### (6) Incorporating depth-discharge relationship ####

## russian 

# using past relationship from transect of discharge measurement for now...
RUS$depth <- (0.08601*RUS$discharge) + 0.31433

## salmon

# using past relationship modeled with USGS channel cross-section data
SAL$depth <- exp((0.32207*log(SAL$discharge)) - 1.03866)

## south fork eel @ miranda



## south fork eel @ standish hickey