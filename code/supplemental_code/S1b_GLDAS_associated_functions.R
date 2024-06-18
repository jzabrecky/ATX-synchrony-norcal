#### functions associated with downloading and processing GLDAS data
### gathered & edited by Jordan Zabrecky
### past code authors... members of Blaszczak lab???
## last edited 06.18.2024

# This code downloads data from GLDAS, stores it, and also
# includes a function for processing and interpolating it

#### All functions ####

# function to download single site GLDAS pressure data from given "Lat" and "Lon" 
# for given "startDate" and "endDate" and save it to a given directory ("save-dir") with
# the given "Site_ID" in the title
GLDAS_press_DL <- function(save_dir, Site_ID, Lat, Lon, startDate, endDate){
  # the initial string to build the URL
  http_string <- paste("https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=GLDAS2:GLDAS_NOAH025_3H_v2.1:Psurf_f_inst")
  
  # separating the date information
  start_split <- strsplit(startDate, "-")[[1]]
  end_split <- strsplit(endDate, "-")[[1]]
  
  # build individual components of the url
  location_string <- paste0("&location=GEOM:POINT(", (Lon + 0.125), ",%20", (Lat + 0.125), ")")
  start_string <- paste0("&startDate=", start_split[1], "-", start_split[2], "-", start_split[3], "T00")
  end_string <- paste0("&endDate=", end_split[1], "-", end_split[2], "-", end_split[3], "T23")
  
  # generating the URL
  url <- paste0(http_string, start_string, end_string, location_string, "&type=asc2")    
  
  # downloading the data
  destfile <- paste(save_dir, "/", Site_ID, "_GLDAS.asc", sep = "")
  
  # error catch in case the page is inaccessible. A little inelegant at present...
  try_result <- try(download.file(url, destfile, method = "curl"), silent = FALSE)
  
  if(class(try_result) == "try-error") {file.remove(destfile)}
  
}

# this function processes downloaded GLDAS data and uses it to write a usable csv
GLDAS_proc <- function(read_dir, save_dir, Site, Lat, Lon, local_tz){
  # reading in the table, skipping the first 40 lines of header information
  # and removing the last row which contains a calculated mean value
  file_name <- paste(read_dir, Site, "_GLDAS.asc", sep = "")
  gldas <- read.table(file_name, skip = 13, nrows = length(readLines(file_name)) - 13)
  colnames(gldas) <- c("DateTime", "pressure")
  
  ## Adding in date and time information
  # splitting date and time
  library(stringr)
  datetime <- data.frame(str_split_fixed(gldas$DateTime, "T", 2))
  
  # extracting the hour information
  gldas[, "Time"] <- as.numeric(substr(datetime[,2], 1, 2))
  
  # adding a POSIX time column
  gldas[, "UTC_time"] <- as.POSIXct(paste(datetime[, 1], " ",
                                          as.matrix(sprintf("%02d", gldas[, "Time"])), sep = ""), format = "%Y-%m-%d %H",
                                    tz = "UTC")
  gldas[, "local_time"] <- as_datetime(gldas[, "UTC_time"], tz = local_tz)
  
  # selecting the final column
  final <- gldas[, c("UTC_time", "local_time", "pressure")]
  
  # writing the final output
  write.csv(final, paste(path, Site, "_GLDAS_pressurePA.csv", sep = ""), quote = FALSE, row.names = FALSE)
}

# this function combines the prior two functions to download .asc file and create
# the .csv in one line
baro_dwld_processing <- function(site, latitude, longitude, start, end, file_location, local_tz){
  
  # download .asc file from GLDAS using above function
  GLDAS_press_DL(file_location, site, latitude, longitude, start, end)
  
  # process .asc into a csv using above function
  GLDAS_proc(file_location,
             file_location,
             site, latitude, longitude, local_tz)
}

# this function reads the GLDAS csv from the "GLDAS_proc" function 
# and uses the supporting script from "S1a_split_interpolate_data.R"
# to fill in data for 5-min intervals and make it usable to model metabolism
baro_make_df <- function(file_location, site, local_tz, supporting_path) {
  
  # read csv
  baro <- read.csv(paste(file_location, site,"_GLDAS_pressurePA.csv", sep = ""))
  
  # making local_time a POSIXct object and saving the timezome information
  baro$date_time <- as_datetime(as.character(baro$local_time))
  baro$date_time <- force_tz(baro$date_time, tzone = local_tz)
  
  # subsetting dataframe
  baro <- baro[,c("date_time","pressure")]
  
  ## split, interpolate, and convert
  # accessing other script for function
  source(paste(supporting_path, "S1a_split_interpolate_data.R", sep = ""))
  # creating filled time series
  baro_5M <- create_filled_TS(baro, "5M", "pressure")
  
  # convert pressure in Pa to mmHg <- CHANGE THIS TO mbar!
  baro_5M$pressure_mmHg<-baro_5M$Filled_Var/133.322
  
  # using tidyverse to make dataframe for final output
  library(tidyverse)
  baro_final <- baro_5M %>% 
    dplyr::select(date_time, pressure_mmHg)
  
  return(baro_final)
}