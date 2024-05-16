#### data visualization for miniDOT data cleaning
### Jordan Zabrecky
## last edited: 05.16.2024
## note that dygraphs does not display graph with current new verion of R (4.4.0)

# This supporting code helped visualize the data cleaning process with
# the code from "1_reading_and_cleaning_DO_data.R" using the 'dygraphs' package

#### Loading libraries and creating functions to quickly visualize ####

## Loading libraries
lapply(c("dygraphs","xts"), require, character.only = T)

## creating functions for quick visualizations

# viewing DO for 2022 year
view_DO_2022 <- function(x) {
  x <- subset(x, x$date_time >= "2022-06-23 0:00:00" & x$date_time <= "2022-09-23 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$DO_mgL, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing DO for 2023 year
view_DO_2023 <- function(x) {
  x <- subset(x, x$date_time >= "2023-06-18 0:00:00" & x$date_time <= "2023-09-28 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$DO_mgL, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing temperature for 2022 year
view_temp_2022 <- function(x) {
  x <- subset(x, x$date_time >= "2022-06-23 0:00:00" & x$date_time <= "2022-09-23 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$Temp_C, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing temperature for 2023 year
view_temp_2023 <- function(x) {
  x <- subset(x, x$date_time >= "2023-06-18 0:00:00" & x$date_time <= "2023-09-28 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$Temp_C, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}


#### Visualizations (dygraphs) ####

# DO cleaning
view_DO_2022(sfkeel_mir_2022_cleaning_DO) # looks decent.
view_DO_2022(russian_2022_cleaning_DO) # looks decent.
view_DO_2022(salmon_2022_cleaning_DO)  # lost a lot of days but looks decent now

view_DO_2023(sfkeel_mir_2023_cleaning) # looks good! ("_DO" file does not exist bc it was so great)
view_DO_2023(sfkeel_sth_2023_cleaning_DO) # looks decent
view_DO_2023(salmon_2023_cleaning_DO) # looks decent

# temp cleaning
view_temp_2022(sfkeel_mir_2022_cleaning) # looks good!
view_temp_2022(russian_2022_cleaning) # looks good!
view_temp_2022(salmon_2022_cleaning) # looks good! 5.16.24

view_temp_2023(sfkeel_mir_2023_cleaning) # looks good!
view_temp_2023(sfkeel_sth_2023_cleaning) # looks good!
view_temp_2023(salmon_2023_cleaning) # looks good! 5.16.24
