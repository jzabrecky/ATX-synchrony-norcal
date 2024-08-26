#### putting together water chemistry data from field and lab measurements
### Jordan Zabrecky
## last edited: 08.07.2024

# This code combines in-situ water chemistry measurements, AQ400 nitrate, ammonium,
# and orthophosphate values, Shimadzu total dissolved carbon, dissolved organic
# carbon, and Ion Chromatography anions & cations (2022 only) for water samples

#### (1) Loading libraries and reach data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

# loading raw data
field_params <- ldply(list.files(path = "./data/field_and_lab/raw_data/", 
                                pattern = "field_params"), function(filename) {
  d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
  return(d)
})
