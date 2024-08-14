#### function for formatting NLDAS data
### edited by Jordan Zabrecky
### past code authors... members of Blaszczak lab???
## last edited 08.14.2024

# This code uses data downloaded and processed from NLDAS via the
# "StreamLightUtils" and further processes it and interpolates 

#### Function ####

# function to further process/format NLDAS data from "NLDAS_proc" function from
# "StreamLightUtils" and uses the "create_filled_TS" function from 
# "S1a_split_interpolate_data.R" via the "supporting_path" parameter to interpolate
# in five-minute intervals
NLDAS_formatting <- function(df, supporting_path){
  
  df$origin <- as.Date(paste0(df$Year, "-01-01"),tz = "UTC") - days(1)
  df$Date <- as.Date(df$DOY, origin = df$origin, tz = "UTC") 
  df$DateTime_UTC <- lubridate::ymd_hms(paste(df$Date, " ", df$Hour, ":00:00"))
  df <- df[,c("DateTime_UTC","SW")] #time zone clearly wrong
  # adjust time zone
  df$date_time <- with_tz(df$DateTime_UTC, tzone = "America/Los_Angeles")
  df$PAR_surface <- df$SW
  
  light <- df[,c("date_time","PAR_surface")]
  
  # Split, interpolate, and convert
  source(paste(supporting_path, "S1a_split_interpolate_data.R", sep = ""))
  light_5M <- create_filled_TS(light, "5M", "PAR_surface")
  light_5M <- light_5M[,c("date_time","Filled_Var")]
  colnames(light_5M) <- c("date_time","PAR_surface")
  
  return(light_5M)
  
}