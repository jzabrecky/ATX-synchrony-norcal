#### gathering all data to model metabolism
### Jordan Zabrecky
## last edited 06.16.2025

# This code gathers the necessary components for metabolism modeling
# including the (1) cleaned miniDOT data from "1a_reading_and_cleaning_miniDOT_data.R"
# and applies calibration offsets calculated from "1c_sensor_intercalibrations.R"
# (2) USGS gage discharge data, (3) GLDAS pressure data, 
# (4) NLDAS light data, and (5) temporary depth of m = 1. 
# In step (6) a final csv is created with all this information to be used to 
# model metabolism in "1e_metabolism_estimates.R"

#### (1) Loading packages and reading in data #### 

## Loading necessary packages
lapply(c("dataRetrieval", "lubridate", "plyr", "tidyverse", "StreamLightUtils",
         "zoo", "streamMetabolizer"), require, character.only = T)

# get rid of any dplyr masking!
filter <- dplyr::filter
select <- dplyr::select
summarize <- dplyr::summarize
rename <- dplyr::rename

## if "StreamLightUtils" have not yet been downloaded...
#devtools::install_github("psavoy/StreamLightUtils")
#devtools::install_github("psavoy/StreamLight")

## NOTE: "StreamLightUtils" uses the "rgdal" package which has been discontinued
## you can obtain an old version of the package as follows:
#url <- "https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.6-6.tar.gz"
#install.packages(url, type = "source", repos = NULL)
# However, there may be issues doing this with later version of R 
# (I had issues with R 4.4.0, but it works with 4.2.3 and 4.3.2)

## (a) reading in miniDOT data
miniDOT_data <- ldply(list.files(path = "./data/miniDOT/", pattern = "_miniDOT.csv"), function(filename) {
  d <- read.csv(paste("data/miniDOT/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_miniDOT.csv")
  d$site = d$site_year %>% str_sub(end=-6)
  return(d)
})

# removing NAs 
# (salmon 2022, 2023 & south fork eel @ sth 2023 have missing DO that we removed
# due to biofouling, etc., but we kept temperature since it seemed correct)
miniDOT_data <- na.omit(miniDOT_data)

# converting date_time from character to POSIXct class & indicate time zone
miniDOT_data$date_time <- as_datetime(miniDOT_data$date_time, tz = "America/Los_Angeles")

## (b) applying offsets from intercalibrations to measured DO

# read in offset csvs
offsets <- read.csv("./data/EDI_data_package/miniDOT_calibration_offsets.csv")

# apply offsets to miniDOT data
miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "sfkeel_mir_2022")] <- 
  miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "sfkeel_mir_2022")] + offsets$offset[which(offsets$site_year == "sfkeel_mir_2022")]
# no calibration for russian 2022 as sensor was stolen :(
miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "salmon_2022")] <- 
  miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "salmon_2022")] + offsets$offset[which(offsets$site_year == "salmon_2022")]
miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "sfkeel_mir_2023")] <- 
  miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "sfkeel_mir_2023")] + offsets$offset[which(offsets$site_year == "sfkeel_mir_2023")]
miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "sfkeel_sth_2023")] <- 
  miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "sfkeel_sth_2023")] + offsets$offset[which(offsets$site_year == "sfkeel_sth_2023")]
miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "salmon_2023")] <- 
  miniDOT_data$DO_mg_L[which(miniDOT_data$site_year == "salmon_2023")] + offsets$offset[which(offsets$site_year == "salmon_2023")]

# separating large dataframe into a list of dataframes
miniDOT_list <- split(miniDOT_data, miniDOT_data$site)

## (c) reading in DO data from external sources

# reading in all external dissolved oxgyen dataframes
external_DO <- ldply(list.files(path = "./data/external_DO/", pattern = ".csv"), function(filename) {
  d <- read.csv(paste("./data/external_DO/", filename, sep = ""))
  d$site_year = filename %>% str_sub(end=-5)
  d$site = strsplit(filename, split = "_")[[1]][1]
  return(d)
})

# converting date_time from character to POSIXct class & indicate time zone
external_DO$date_time <- as_datetime(external_DO$date_time, tz = "America/Los_Angeles")

# separate large dataframe into a list of dataframes (ideally will have other external sources later so doing it group-ways)
external_DO_list <- split(external_DO, external_DO$site_year)

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
    mutate(discharge_m3_s = Filled_Var / 35.31) %>% 
    select(date_time, discharge_m3_s)
  return(new_df)
}

# applying function to dataframe list
discharge <- lapply(discharge, function(x) clean_discharge(x))

# RUN ONCE: save these in case USGS data goes offline
#for(i in 1:length(site_names)) {
#  df <- discharge[[i]] %>% 
#    mutate(date_time = as.character(format(date_time)), # to avoid 00:00:00 saving issue
#           site = site_names[[i]]) %>% 
#    relocate(site, .before = date_time)
#  write.csv(df, paste("./data/USGS/", site_names[i], "_discharge_continuous.csv", sep = ""),
#            row.names = FALSE)
#}

#### (3) Gathering GLDAS pressure data ####

# using functions from supporting script "S1b_GLDAS_associated_functions.R"
source("code/supplemental_code/S1b_GLDAS_associated_functions.R")

# setting directory specs for download
path <- "data/GLDAS/" # where we will save the file

# directory where we can access "S1a_split_interpolate_data.R" while using the GLDAS script
base_wd <- getwd() # saving our base working directory
supporting <- paste(base_wd, "/code/supplemental_code/", sep = "")

# downloading GLDAS .asc file and processing it into a saved .csv file
# ONLY NEED TO RUN ONCE
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

## our local point-measures of barometry are slightly different, likely because GLDAS is coarse
## and we are in a mountainous area, so we will adjust the GLDAS data with these measurements

# reading in extech local barometric pressure data
extech_data <- read.csv("./data/EDI_data_package/barometric_pressure.csv") %>% 
  mutate(date_time = ymd_hms(date_time, tz = "America/Los_Angeles"),
         site = case_when(grepl("RUS", site_reach) ~ "RUS", # create site codes
                          grepl("SAL", site_reach) ~ "SAL",
                          grepl("SFE-M", site_reach) ~ "SFE-M",
                          grepl("SFE-SH", site_reach) ~ "SFE-SH"))

# to preserve processed data, will make a new list
GLDAS_adjusted <- GLDAS_processed

## russian

# get russian data and merge with GLDAS processed data
extech_russian <- extech_data %>% 
  filter(site == "RUS")
extech_russian <- merge(extech_russian, GLDAS_processed$russian)

# visualize data and test correlation
plot(extech_russian$pressure_mbar, extech_russian$pressure_mbar_extech)
cor.test(extech_russian$pressure_mbar_extech, extech_russian$pressure_mbar) # highly correlated!

# linear regression model
mbar_lm_russian <- lm(pressure_mbar_extech ~ pressure_mbar, data = extech_russian)

# using regression model to obtain more locally adjusted barometric pressure
GLDAS_adjusted$russian$pressure_mbar <- mbar_lm_russian$coefficients[1] + 
  (GLDAS_processed$russian$pressure_mbar * mbar_lm_russian$coefficients[2])

## salmon

# get salmon data and merge with GLDAS processed data
extech_salmon <- extech_data %>% 
  filter(site == "SAL")
extech_salmon <- merge(extech_salmon, GLDAS_processed$salmon)

# visualize data and test correlation
plot(extech_salmon$pressure_mbar, extech_salmon$pressure_mbar_extech)
cor.test(extech_salmon$pressure_mbar_extech, extech_salmon$pressure_mbar) # highly correlated!

# linear regression model
mbar_lm_salmon <- lm(pressure_mbar_extech ~ pressure_mbar, data = extech_salmon)

# using regression model to obtain more locally adjusted barometric pressure
GLDAS_adjusted$salmon$pressure_mbar <- mbar_lm_salmon$coefficients[1] + 
  (GLDAS_processed$salmon$pressure_mbar * mbar_lm_salmon$coefficients[2]) 

## south fork eel @ miranda

# get miranda data and merge with GLDAS processed data
extech_sfkeel_mir <- extech_data %>% 
  filter(site == "SFE-M")
extech_sfkeel_mir <- merge(extech_sfkeel_mir, GLDAS_processed$sfkeel_mir)

# visualize data and test correlation
plot(extech_sfkeel_mir$pressure_mbar, extech_sfkeel_mir$pressure_mbar_extech)

# appears to be a massive outlier highly dissimilar from observations before and after
# on same day-- likely a tired field work typo
extech_sfkeel_mir <- extech_sfkeel_mir[-which(extech_sfkeel_mir$pressure_mbar_extech == max(extech_sfkeel_mir$pressure_mbar_extech)),]

# reevaluate
plot(extech_sfkeel_mir$pressure_mbar, extech_sfkeel_mir$pressure_mbar_extech)
cor.test(extech_sfkeel_mir$pressure_mbar_extech, extech_sfkeel_mir$pressure_mbar) # highly correlated!

# linear regression model
mbar_lm_sfkeel_mir <- lm(pressure_mbar_extech ~ pressure_mbar, data = extech_sfkeel_mir)

# using regression model to obtain more locally adjusted barometric pressure
GLDAS_adjusted$sfkeel_mir$pressure_mbar <- mbar_lm_sfkeel_mir$coefficients[1] + 
  (GLDAS_processed$sfkeel_mir$pressure_mbar * mbar_lm_sfkeel_mir$coefficients[2])

## south fork eel @ standish hickey

# get standish hickey data and merge with GLDAS processed data
extech_sfkeel_sth <- extech_data %>% 
  filter(site == "SFE-SH")
extech_sfkeel_sth <- merge(extech_sfkeel_sth, GLDAS_processed$sfkeel_sth)

# visualize data and test correlation
plot(extech_sfkeel_sth$pressure_mbar, extech_sfkeel_sth$pressure_mbar_extech)
cor.test(extech_sfkeel_sth$pressure_mbar_extech, extech_sfkeel_sth$pressure_mbar) # highly correlated!

# linear regression model
mbar_lm_sfkeel_sth <- lm(pressure_mbar_extech ~ pressure_mbar, data = extech_sfkeel_sth)

# using regression model to obtain more locally adjusted barometric pressure
GLDAS_adjusted$sfkeel_sth$pressure_mbar <- mbar_lm_sfkeel_sth$coefficients[1] + 
  (GLDAS_processed$sfkeel_sth$pressure_mbar * mbar_lm_sfkeel_sth$coefficients[2])

#### (4) Gathering NLDAS light data ####

# making site table to use for NLDAS data download
site_table <- as.data.frame(rbind(c("russian", 38.806883, -123.007017),
                                  c("salmon", 41.3771369, -123.4770326),
                                  c("sfkeel_mir", 40.198173, -123.775930),
                                  c("sfkeel_sth", 39.876268, -123.727924)))
colnames(site_table) <- c("Site_ID", "Lat", "Lon")

# make sure latitude and longitude are numeric to avoid future error
site_table$Lat <- as.numeric(site_table$Lat)
site_table$Lon <- as.numeric(site_table$Lon)

# downloading site NLDAS data with function from "StreamLightUtils"
# ONLY NEED TO RUN ONCE
NLDAS_DL_bulk(save_dir = "data/NLDAS",
              site_locs = site_table, startDate = "2022-06-15")

# making list of downloaded sites from above (getting list from folder and removing "_NLDAS.asc")
site_list <- stringr::str_sub(list.files("data/NLDAS"), 1, -11)

# processing the downloaded NLDAS data using function from "StreamLightUtils"
directory <- paste(base_wd, "/data/NLDAS", sep = "") # where NLDAS .asc files are located
NLDAS_processed <- NLDAS_proc(read_dir = directory, site_list)

# using a custom formatting function from another code source
source("../../code/supplemental_code/S1c_NLDAS_formatting_function.R")

# applying function to all data
NLDAS_formatted <- lapply(NLDAS_processed, function(x) NLDAS_formatting(x, supporting))

#### (5) Adding temporary depth_m = 1 ####

# instead of rerunning the metabolism model over and over again with our updated depths
# we will just run the model with m = 1 and apply the depth after metabolism models
# thus, as we are not dividing GPP by anything, our GPP will be in units g O2 m^-3 d^-1
# so to get it in g O2 m^-2 d^-1 we will multiply by depth (m/m^-3 = 1/m^-2)

# function to add depth column to each dataframe in the discharge list
add_depth <- function(df) {
  new_df <- df %>% 
    mutate(depth_m = 1)
  return(new_df)
}

# apply function across discharge list
discharge <- lapply(discharge, function(x) add_depth(x))

#### (6) Putting it all together ####

## putting together miniDOT dataframes

# create empty vector for all sites
combined <- data.frame()

# combine all into a single dataframe per site
# can use indexing because all lists are in same site order
for(i in 1:length(miniDOT_list)) {
  single_site <- (list(miniDOT_list[[i]], discharge[[i]], GLDAS_adjusted[[i]], NLDAS_formatted[[i]])) %>% 
    join_all(by = "date_time", type = "left")
  combined <- rbind(combined, single_site)
}

## putting together external data frames

# left join data together separately as indexes won't line up
russian_2022_USGS <- (list(external_DO_list$russian_2022_USGS, discharge$russian, GLDAS_adjusted$russian, 
                           NLDAS_formatted$russian)) %>% 
  join_all(by = "date_time", type = "left")
salmon_2022_karuk <- (list(external_DO_list$salmon_2022_karuk, discharge$salmon, GLDAS_adjusted$salmon, 
                           NLDAS_formatted$salmon)) %>% 
  join_all(by = "date_time", type = "left")
salmon_2023_karuk <- (list(external_DO_list$salmon_2023_karuk, discharge$salmon, GLDAS_adjusted$salmon,
                           NLDAS_formatted$salmon)) %>% 
  join_all(by = "date_time", type = "left")

# combine into one dataframe
combined <- rbind(combined, russian_2022_USGS, salmon_2022_karuk, salmon_2023_karuk) %>% 
  na.omit() # weird NAs got added to end of salmon_2022_karuk and salmon_2023_karuk?

# checking for weirdness
anyNA(combined) # no NAs!
eval(nrow(combined) == (nrow(miniDOT_data) + nrow(external_DO))) # we have all our original DO data!

#### (7) Final processing for streamMetabolizer use and saving ####

# check specific input requirements from streamMetabolizer
metab_inputs('bayes', 'data')

# function to modify data frames to match the above requirements
metab_prep <- function(df) {
  new_df <- df %>% 
    # calculate solar time function from streamMetabolizer
    # will account for our data being in PST
    mutate(solar.time = calc_solar_time(date_time, longitude),
           DO.obs = DO_mg_L,
           # calculate DO saturation using function from streamMetabolizer
           DO.sat = calc_DO_sat(Temp_C, pressure_mbar, salinity.water = 0, 
                                model = "garcia-benson"),
           depth = depth_m, 
           temp.water = Temp_C,
           # StreamLight gives us PAR in the appropriate units
           light = convert_SW_to_PAR(SW_W_m_2),
           discharge = discharge_m3_s) %>% 
    dplyr::select(site_year, solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge)
  return(new_df)
}

# need to add longitude for each site to later calculate solar time
combined <- combined %>% 
  mutate(
    longitude = case_when(site == "russian" ~ -123.007017,
                          site == "salmon" ~ -123.4770326,
                          site == "sfkeel_mir" ~ -123.775930,
                          site == "sfkeel_sth" ~ -123.727924)
  )

# apply function to dataframes
final <- metab_prep(combined)

# set working directory for saving
setwd("../metab_model_inputs")

# changing POSIXct to character to avoid any saving issues like before
final$solar.time <- as.character(format(final$solar.time))

# making a list for each site_year to save separately
final_list <- split(final, final$site_year)

# saving csvs for metabolism model input
lapply(names(final_list), function(x) write.csv(final_list[[x]], file = paste(x, "_modelinputs", ".csv", sep = ""), 
                                           row.names = FALSE))
