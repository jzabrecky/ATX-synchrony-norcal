#### gathering all data to model metabolism
### Jordan Zabrecky
## last edited 08.14.2024

# This code gathers the necessary components for metabolism modeling
# including the (1) cleaned miniDOT data from "1a_reading_and_cleaning_miniDOT_data.R",
# (2) USGS gage discharge data, (3) GLDAS pressure data, 
# (4) NLDAS light data, and depth-discharge relationship information. 
# In step (6) a final csv is created with all this information to be used to 
# model metabolism in "1d_modeling_metabolism"

#### (1) Loading packages and reading in data #### 

## Loading necessary packages
lapply(c("dataRetrieval", "lubridate", "plyr", "tidyverse", "StreamLightUtils",
         "zoo"), require, character.only = T)

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

## Reading in miniDOT data
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

# separating large dataframe into a list of dataframes
miniDOT_list <- split(miniDOT_data, miniDOT_data$site)

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
extech_data <- ldply(list.files(path = "./data/field_and_lab/raw_data/", pattern = "field_params"), function(filename) {
  df <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
  new_df <- df %>% 
    mutate(date_time = mdy_hm(paste(field_date, time), tz = "America/Los_Angeles"),
           year = year(date_time),
           pressure_mbar_extech = pressure_mmHg * 1.333) %>% 
    na.omit() %>% 
    select(date_time, year, site, pressure_mbar_extech)
})

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

# applying regression model to adjusted data list
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

# applying regression model to adjusted data list
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

# applying regression model to adjusted data list
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

# applying regression model to adjusted data list
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

#### (5) Incorporating depth-discharge relationship ####

# reading in data (what we have now...)
kayak_sfkeel <- read.csv("../depth_measurements/sfkeel_kayak_measurements.csv")

# converting date as string to date object
kayak_sfkeel$Date <- mdy(kayak_sfkeel$Date)

# function to plot and visualize depth-discharge relationship
Q_depth_plot <- function(x, y , model) {
  ggplot() +
    geom_point(aes(x = x, y = y), size = 3, color = "darkblue") + 
    geom_abline(slope = model$coefficients[[2]], intercept = model$coefficients[[1]], 
                linewidth =1.5, color="skyblue", linetype = "dotted") +
    xlab("Discharge (cms)")+
    ylab("Depth (m)")+
    theme_bw()
}

# function to clean discharge downloads
edit_Q_depth_df <- function(data) {
  new <- data %>% 
    mutate(discharge_m3_s = X_00060_00003 / 35.31) %>% 
    select(Date, depth_m, discharge_m3_s)
  return(new)
}

## russian 

# using past relationship from transect of discharge measurement for now...
discharge$russian$depth_m <- (0.08601 * discharge$russian$discharge_m3_s) + 0.31433

## salmon

# using past relationship modeled with USGS channel cross-section data
discharge$salmon$depth_m <- exp((0.32207 * log(discharge$salmon$discharge_m3_s)) - 1.03866)

## south fork eel @ miranda

# calculating average depth per kayak run
depth_Q_sfkeel_mir <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Miranda", Meas_Type == "Depth") %>%
  filter(Transect != 18) %>% # removed transects 18 as first date was half depth of later two dates
  filter(Transect != 5 & Transect != 12) %>%  # removed transects 5 & 12 as first date was ~0.3 m lower than second date
  # this may be either because our GPS was slightly off or differences when taking depths across transects between two different kayakers
  group_by(Date) %>% 
  summarize(depth_m = mean(Depth_cm_final) / 100)

# getting daily discharge data and adding it to depth-discharge data frame
depth_Q_sfkeel_mir <- left_join(depth_Q_sfkeel_mir, 
                                readNWISdv("11476500", param, depth_Q_sfkeel_mir$Date[1], depth_Q_sfkeel_mir$Date[3]))
depth_Q_sfkeel_mir <- edit_Q_depth_df(depth_Q_sfkeel_mir)

# model relationship between depth and discharge 
# log(depth) ~ log(discharge) shows most linear relationship
# likely underestimates all but summer depths, but we are not modelling metabolism then
# frequentist linear regression model
sfkeel_mir_lm <- lm(log(depth_m) ~ log(discharge_m3_s), data = depth_Q_sfkeel_mir)
# having issues with brm divergent transitions so will stick with above

# plot relationship
Q_depth_plot(log(depth_Q_sfkeel_mir$discharge_m3_s), log(depth_Q_sfkeel_mir$depth_m), sfkeel_mir_lm)

# use model to fill in depth on discharge plot
discharge$sfkeel_mir$depth_m <- exp(sfkeel_mir_lm$coefficients[[1]] + (log(discharge$sfkeel_mir[[2]]) * sfkeel_mir_lm$coefficients[[2]]))

## south fork eel @ standish hickey

# calculating average depth per kayak run
depth_Q_sfkeel_sth <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Standish", Meas_Type == "Depth") %>% 
  group_by(Date) %>%  # not seeing any weird transect issues here!
  summarize(depth_m = mean(Depth_cm_final) / 100)
# also makes sense that this site is as deep despite lower discharge-- more pools

# getting daily discharge data and adding it to depth-discharge data frame
depth_Q_sfkeel_sth <- left_join(depth_Q_sfkeel_sth, 
                                readNWISdv("11475800", param, depth_Q_sfkeel_sth$Date[1], depth_Q_sfkeel_sth$Date[3]))
depth_Q_sfkeel_sth <- edit_Q_depth_df(depth_Q_sfkeel_sth)

# model relationship between depth and discharge
# log(depth) ~ discharge shows most linear relationship
# though not as good of a fit overall as miranda
# however, due to Leopold and Maddock (1953) hydraulic theory, we'll stick with log-log
sfkeel_sth_lm <- lm(log(depth_m) ~ log(discharge_m3_s), data = depth_Q_sfkeel_sth)

# plot relationship
Q_depth_plot(log(depth_Q_sfkeel_sth$discharge_m3_s), log(depth_Q_sfkeel_sth$depth_m), sfkeel_sth_lm)

# use model to fill in depth on discharge plot NEED TO FIX THIS
discharge$sfkeel_sth$depth_m <- exp(sfkeel_sth_lm$coefficients[[1]] + (log(discharge$sfkeel_sth[[2]]) * sfkeel_sth_lm$coefficients[[2]]))

#### (6) Putting it all together ####

# set working directory
setwd("../metab_model_inputs")

# create empty vector for all sites
combined <- data.frame()

# combine all into a single dataframe per site
# can use indexing because all lists are in same site order
for(i in 1:length(miniDOT_list)) {
  single_site <- (list(miniDOT_list[[i]], discharge[[i]], GLDAS_adjusted[[i]], NLDAS_formatted[[i]])) %>% 
    join_all(by = "date_time", type = "left")
  combined <- rbind(combined, single_site)
}

# checking for weirdness
anyNA(combined) # no NAs!
eval(nrow(combined) == nrow(miniDOT_data)) # we have all our original DO data!

# changing date_time to character to avoid any saving issues like before
combined$date_time <- as.character(format(combined$date_time))

# making a list for each site_year
final <- split(combined, combined$site_year)

# saving csvs for metabolism model input
lapply(names(final), function(x) write.csv(final[[x]], file = paste(x, "_modelinputs", ".csv", sep = ""), 
                                           row.names = FALSE))