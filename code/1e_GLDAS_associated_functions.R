#### functions associated with downloading and processing GLDAS data
### gathered & edited by Jordan Zabrecky
### past code authors... members of Blaszczak lab???
## last edited 05.20.2024

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
GLDAS_proc <- function(read_dir, save_dir, Site, Lat, Lon){
  # reading in the table, skipping the first 40 lines of header information
  # nd removing the last row which contains a calculated mean value
  file_name <- paste(read_dir, Site, "_GLDAS.asc", sep = "")
  nldas <- read.table(file_name, skip = 13, nrows = length(readLines(file_name)) - 13)
  colnames(nldas) <- c("DateTime", "pressure")
  
  ## Adding in date and time information
  # splitting date and time
  library(stringr)
  datetime <- data.frame(str_split_fixed(nldas$DateTime, "T", 2))
  
  # extracting the hour information
  nldas[, "Time"] <- as.numeric(substr(datetime[,2], 1, 2))
  
  # adding a POSIX time column
  nldas[, "pos_time"] <- as.POSIXct(paste(datetime[, 1], " ",
                                          as.matrix(sprintf("%02d", nldas[, "Time"])), sep = ""), format = "%Y-%m-%d %H",
                                    tz = "UTC")
  
  # converting into local time
  nldas[, "local_time"] <- format(nldas[, "pos_time"], tz = "America/Los_Angeles")
  
  # selecting the final column
  final <- nldas[, c("local_time", "pressure")]
  
  # writing the final output
  write.csv(final, paste(path, Site, "_GLDAS_pressurePA.csv", sep = ""), quote = FALSE, row.names = FALSE)
  
}

# this function combines the prior two functions and also further alters the produced csv
# note that it also calls to other script "1c_split_interpolate_data.R" to create
# the 5-minute interval time series to match miniDOT data
# so supporting_path is the pathway to that file
baro_dwld_processing <- function(site, latitude, longitude, start, end, file_location, supporting_path){
  
  GLDAS_press_DL(file_location, site, latitude, longitude, start, end)
  
  GLDAS_proc(file_location,
             file_location,
             site, latitude, longitude)
  
  baro <- read.csv(paste(file_location, site,"_GLDAS_pressurePA.csv", sep = ""))
  baro$date_time <- as.POSIXct(as.character(baro$local_time), format = "%Y-%m-%d %H:%M:%S")
  baro <- baro[,c("date_time","pressure")]
  
  ## split, interpolate, and convert
  # accessing other script for function
  source(paste(supporting_path, "1c_split_interpolate_data.R", sep = ""))
  baro_5M <- create_filled_TS(baro, "5M", "pressure")
  baro_5M$pressure_mmHg<-baro_5M$Filled_Var/133.322 # Convert pressure in Pa to mmHg
  
  # using tidyverse to make dataframe for final output
  library(tidyverse)
  baro_final <- baro_5M %>% 
    dplyr::select(date_time, pressure_mmHg)
  
  return(baro_final)
}