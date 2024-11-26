#### getting field and lab data together for the EDI package
### Jordan Zabrecky
## last edited: 11.25.2024

# This code puts together raw field and lab data for release in the EDI package

#### (1) Loading packages ####

# loading packaages
lapply(c("tidyverse", "plyr"), require, character.only = T)

#### (2) Barometric pressure ####

# reading in extech local barometric pressure data
extech_data <- ldply(list.files(path = "./data/field_and_lab/raw_data/", pattern = "field_params"), function(filename) {
  df <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
  new_df <- df %>% 
    mutate(date_time = mdy_hm(paste(field_date, time), tz = "America/Los_Angeles"),
           pressure_mbar_extech = pressure_mmHg * 1.333) %>% 
    na.omit() %>% 
    select(date_time, site, pressure_mbar_extech)
})

# save csv
write.csv(extech_data, "./data/EDI_data_package/barometric_pressure.csv", row.names = FALSE)
