#### data visualization for HOBO data cleaning
### Jordan Zabrecky
## last edited: 10.02.2024
## (note that dygraphs does not seem to display graph with 4.4.0 version of R)

# This supporting code helped visualize the data cleaning process with
# the code from "3c_HOBO_c.R" using the 'dygraphs' package

#### (1) Loading libraries and creating functions to quickly visualize ####

## Loading libraries
lapply(c("dygraphs","xts"), require, character.only = T)

## creating functions for quick visualizations

# viewing full range conductivity for 2022 year
view_full_range_2022 <- function(x) {
  x <- subset(x, x$date_time >= "2022-06-23 0:00:00" & x$date_time <= "2022-09-23 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$full_range_cond_uS_cm, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing full range conductivity for 2023 year
view_full_range_2023 <- function(x) {
  x <- subset(x, x$date_time >= "2023-06-18 0:00:00" & x$date_time <= "2023-09-28 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$full_range_cond_uS_cm, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing low range conductivity for 2022 year
view_low_range_2022 <- function(x) {
  x <- subset(x, x$date_time >= "2022-06-23 0:00:00" & x$date_time <= "2022-09-23 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$low_range_cond_uS_cm, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing low range conductivity for 2023 year
view_full_range_2023 <- function(x) {
  x <- subset(x, x$date_time >= "2023-06-18 0:00:00" & x$date_time <= "2023-09-28 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$low_range_cond_uS_cm, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing temperature for 2022 year
view_temperature_2022 <- function(x) {
  x <- subset(x, x$date_time >= "2022-06-23 0:00:00" & x$date_time <= "2022-09-23 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$temp_C, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# viewing low range conductivity for 2023 year
view_temperature_2023 <- function(x) {
  x <- subset(x, x$date_time >= "2023-06-18 0:00:00" & x$date_time <= "2023-09-28 0:00:00")
  
  # then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$temp_C, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

#### (2) Visualizations (dygraphs) ####

# 2022
view_full_range_2022(russian_2022_cleaning_cond) # good enough (10.02.2024)
view_low_range_2022(russian_2022_cleaning_cond) # good enough (10.02.2024)
view_temperature_2022(russian_2022_cleaning) # looks great! (10.02.2024)
view_full_range_2022(HOBO_2022_list$salmon_2022)
view_low_range_2022(HOBO_2022_list$salmon_2022)
view_temperature_2022(HOBO_2022_list$salmon_2022)
view_full_range_2022(sfkeel_mir_2022_cleaning_cond)
view_low_range_2022(sfkeel_mir_2022_cleaning_cond)
view_temperature_2022(sfkeel_mir_2022_cleaning) # looks great! (10.02.2024)

# 2023
view_full_range_2023(HOBO_2023_list$salmon_2023)
view_low_range_2023(HOBO_2023_list$salmon_2023)
view_temperature_2023(HOBO_2023_list$salmon_2023)
view_full_range_2023(HOBO_2023_list$sfkeel_mir_2023)
view_low_range_2023(HOBO_2023_list$sfkeel_mir_2023)
view_temperature_2023(HOBO_2023_list$sfkeel_mir_2023)
view_full_range_2023(HOBO_2023_list$sfkeel_sth_2023)
view_low_range_2023(HOBO_2023_list$sfkeel_sth_2023)
view_temperature_2023(HOBO_2023_list$sfkeel_sth_2023)