#### putting together water chemistry data from field and lab measurements
### Jordan Zabrecky
## last edited: 08.07.2024

# This code combines in-situ water chemistry measurements, AQ400 nitrate, ammonium,
# and orthophosphate values, Shimadzu total dissolved carbon, dissolved organic
# carbon, and Ion Chromatography anions & cations (2022 only) for water samples

#### (1) Loading libraries and reach data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

## loading raw data

# field parameters
field_params <- ldply(list.files(path = "./data/field_and_lab/raw_data/", 
                                pattern = "field_params"), function(filename) {
  d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
  return(d)
})

# aq400 values
aq400 <- ldply(list.files(path = "./data/field_and_lab/raw_data/",
                          pattern = "aq400"), function(filename) {
  d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
  return(d)
})

# shimadzu values
shimadzu <- ldply(list.files(path = "./data/field_and_lab/raw_data/",
                             pattern = "shimadzu"), function(filename) {
  d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
  return(d)
})

# ion-chromatography cation/anion data (only one year of this data)
IC <- read.csv("./data/field_and_lab/raw_data/IC_2022.csv")
# TBD what to do with this data

#### (2) Processing AQ400 values ####


## copy and paste old ammonium code below ##

# loading libraries
library(tidyverse)
library(lubridate)

# loading in raw ammonia data with field pH and temperature
ammonia_2022 <- read.csv("data_prep/raw/ammonia_2022_raw.csv")
ammonia_2023 <- read.csv("data_prep/raw/ammonia_2023_raw.csv")

#### Function to calculate ammonium ####'

calculate_NH4 <- function(data) {
  # assign variables
  temp = data$temperature
  pH = data$pH
  NH3 = data$raw_ammonia_mg_N_L
  
  # calculate pKa
  pKa = 0.09018 + 2727.92/(temp+273.15)
  
  # calculate fraction of NH3
  f = 1/(10^(pKa-pH)+1)
  
  # calculate concentration of ammonium (NH4)
  NH4 = (1-f)*NH3
  
  # creating new column for calculated ammonium (NH4) concentration
  data$ammonium_mg_N_L <- NH4
  
  # return data with new column
  return(data)
}

#### Calculate ammonium and save new csvs ####

# running function on both df objects and creating a new df
ammonium_2022 <- calculate_NH4(ammonia_2022)
ammonium_2023 <- calculate_NH4(ammonia_2023)

# function to trim df and save as new csv
trim_and_save <- function(data, save_name) {
  data <- data %>% 
    select(site_reach, field_date, ammonium_mg_N_L)
  write.csv(data, save_name, row.names = FALSE)
}

# using above function on new ammonium dfs
trim_and_save(ammonium_2022, "data_prep/working/ammonium_2022.csv")
trim_and_save(ammonium_2023, "data_prep/working/ammonium_2023.csv")
