### stuff to further check metab model inputs

# run 

# need to 
# (1) look at inputs unaltered
# (2) make sure solar time is converting from PST

# look at model inputs before prepping (solar.time)

starttime <- "2022-06-24 00:00:00"
endtime <- "2022-06-26 00:00:00"
df <- inputs_list$russian_2022_USGS

pre_model_prep(df, starttime, endtime) {
  
  # allow for string as parameter and convert string to datetime object
  starttime <- as_datetime(starttime)
  endtime <- as_datetime(endtime)
  
  # make plot
  plot <- ggplot(df, aes(x = date_time)) +
    geom_line(aes(y = Temp_C, color = "Temp_C")) +
    geom_line(aes(y = DO_mgL * 3, color = "DO_mgL")) +
    geom_line(aes(y = SW_W_m_2 / 40, color = "SW_W_m_2")) + 
    scale_x_datetime(limits = c(starttime, endtime)) +
    scale_color_manual("Metabolism Inputs (Pre-Prep)", 
                       values = c("green", "red", "blue")) + 
    labs(y = "scaled values", x = "date time") +
    theme_bw()
  
  return(plot)
}

rttime <- "2022-06-24 00:00:00"
endtime <- "2022-06-29 00:00:00"
df <- inputs_list$russian_2022_USGS

pre_model_prep(df, starttime, endtime) {
  
  # allow for string as parameter and convert string to datetime object
  starttime <- as_datetime(starttime)
  endtime <- as_datetime(endtime)
  
  # make plot
  plot <- ggplot(df, aes(x = date_time)) +
    geom_line(aes(y = Temp_C, color = "Temp_C")) +
    geom_line(aes(y = DO_mgL * 3, color = "DO_mgL")) +
    geom_line(aes(y = SW_W_m_2 / 40, color = "SW_W_m_2")) + 
    scale_x_datetime(limits = c(starttime, endtime)) +
    scale_color_manual("Metabolism Inputs (Pre-Prep)", 
                       values = c("green", "red", "blue")) + 
    labs(y = "scaled values", x = "date time") +
    theme_bw()
  
  return(plot)
}

# make plot for prepped and send to Laurel
# also input that plot code into metabolism coding

# look at model inputs prepped

# (2) test to account for solar.time conversion accounting for PST
time1 <- as.POSIXct("2022-07-01 03:00:00", format = "%Y-%m-%d %H:%M:%S", 
                    tz= "America/Los_Angeles")
# UTC is 7 ahead so this should make the same times below
time2 <- as.POSIXct("2022-07-01 10:00:00", format = "%Y-%m-%d %H:%M:%S",
                    tz = "UTC")
calc_solar_time(time1, -123.007017) # 2022-07-01 01:49:19 UTC
calc_solar_time(time2, -123.007017) # 2022-06-30 01:49:19 UTC
# yay that function works!