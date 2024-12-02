#### getting field and lab data together for the EDI package
### Jordan Zabrecky
## last edited: 11.28.2024

# This code puts together raw field and lab data for release in the EDI package

#### (1) Loading packages ####

# loading packages
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

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

# convert date time to character
extech_data$date_time <- as.character(format(extech_data$date_time))

# save csv
write.csv(extech_data, "./data/EDI_data_package/barometric_pressure.csv", row.names = FALSE)

#### (2) Percent cover ####

# loading raw % cover data
percover22 <- read.csv("./data/field_and_lab/raw_data/percover_2022.csv")
percover23 <- read.csv("./data/field_and_lab/raw_data/percover_2023.csv")

# rbind 2022 and 2023 survey data
percover <- rbind(percover22, percover23)

# turn date into date object
percover$field_date <- mdy(percover$field_date)

# making temporary data frames with a total column
percover_test <- percover %>% 
  mutate(total = green_algae + Microcoleus + Anabaena_Cylindrospermum + bare_biofilm + other_N_fixers)

# looking to see if any != 100%
which(percover_test$total != 100)
# all 100- we are all good :)

# remove the temporary data frame
rm(percover_test)

# convert date time to character
percover$date_time <- as.character(format(percover$date_time))

# save csv
write.csv(percover, "./data/EDI_data_package/benthic_surveys.csv", row.names = FALSE)

#### (3) Water quality in-situ & nutrients ####

# function to read in data from our folder to a data frame based on name
# (note that the folder path is hard-coded in)
read_data <- function(name) {
  ldply(list.files(path = "./data/field_and_lab/raw_data/", 
                   pattern = name), function(filename) {
                     d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
                     d$field_date <- mdy(d$field_date)
                     return(d)
                   })
}

# reading in all water chemistry parameters
field_params <- read_data("field_params") %>% 
  select(!pressure_mmHg)
aq400 <- read_data("aq400")
shimadzu <- read_data("shimadzu")
IC <- read_data("IC")

## (a) in-situ field parameters

# replace missing values with NA as missing value code for EDI
field_params <- field_params %>% replace(is.na(.), "NA")

# saving csv
write.csv(field_params, "./data/EDI_data_package/water_in_situ_measurements.csv",
          row.names = FALSE)

## (b) aq400 nitrate, ammonium, and phosphate data

# replace values below detection limits with half the detection limit value
aq400$raw_ammonia_ammonium_mg_N_L <- replace(aq400$raw_ammonia_ammonium_mg_N_L, 
                                             which(aq400$raw_ammonia_ammonium_mg_N_L == "<0.002"),
                                             "0.001")

# convert column to numeric for calculations
aq400$raw_ammonia_ammonium_mg_N_L <- as.numeric(aq400$raw_ammonia_ammonium_mg_N_L)

# for 2022, add in "n" on ammonium below detection limit flag
# (no samples were below detection that year)
aq400$amm_below_detection <- replace_na(aq400$amm_below_detection, "n")

# in more basic waters (like ours) a higher proportion of the ammonium
# and ammonia measured by the AQ400 is (toxic) ammonia so we want to subtract that
# out so we only have ammonium (in more acidic waters you would not do this
# because the amount would be so small)

# we unfortunately did not have a pH probe first month in the field
# so let's just assume the missing pH was roughly similar to the first time
# we were able to measure pH for each site_reach
# and call this column "assumed_pH"

# create data frame (that will we convert to a vector) to fill in missing values
assumed_pH <- aq400 %>% 
  filter(field_date < mdy("7-28-2022")) %>% 
  mutate(assumed_pH = case_when(site_reach == "RUS-1S" ~ field_params$pH[25],
                                site_reach == "SAL-1S" ~ field_params$pH[49],
                                site_reach == "SAL-2" ~ field_params$pH[50],
                                site_reach == "SAL-3" ~ field_params$pH[51],
                                site_reach == "SFE-M-1S" ~ field_params$pH[22],
                                site_reach == "SFE-M-3" ~ field_params$pH[23],
                                site_reach == "SFE-M-4" ~ field_params$pH[24],
                                site_reach == "RUS-2" ~ field_params$pH[26],
                                site_reach == "RUS-3" ~ field_params$pH[27])) %>% 
  select(assumed_pH)

# get observed (not missing) pH values to add into aq400
true_pH_and_temp <- field_params %>% 
  select(field_date, site_reach, pH, temp_C) %>% 
  dplyr::rename(assumed_pH = pH)

# add in observed pH data
aq400 <- left_join(aq400, true_pH_and_temp, by = c("field_date", "site_reach"))

# fill in missing values with above vector
for(i in 1:30) {
  aq400$assumed_pH[i] <- assumed_pH[i,]
}

# convert column to numeric for calculations
aq400$assumed_pH <- as.numeric(aq400$assumed_pH)

# function to calculate true proportions of unionized-ammonia (NH3) 
# and ionized-ammonium (NH4+) using data with assumed pH column 
# and equation from Emerson et al. (1975)
calculate_NH4 <- function(data) {
  # assign variables
  temp = data$temp_C
  pH = data$assumed_pH
  NH3 = data$raw_ammonia_ammonium_mg_N_L
  
  # calculate pKa
  pKa = 0.09018 + 2727.92/(temp+273.15)
  
  # calculate fraction of NH3
  f = 1/(10^(pKa-pH)+1)
  
  # calculate concentration of ammonium (NH4)
  NH4 = (1-f)*NH3
  
  # creating new column for calculated ammonium (NH4) concentration
  data$calculated_ammonium_mg_N_L <- round(NH4, 5) # limit decimal places to 5 like input values
  
  # return data with new column
  return(data)
}

# apply function
aq400 <- calculate_NH4(aq400)

# replace missing values with NA as missing value code for EDI
aq400 <- aq400 %>% replace(is.na(.), "NA")

# saving csv
write.csv(aq400, "./data/EDI_data_package/water_nutrient_measurements.csv",
          row.names = FALSE)

## (c) shimadzu carbon measurements

# everything is good so just need to write the csv!
write.csv(shimadzu, "./data/EDI_data_package/water_carbon_measurements.csv",
          row.names = FALSE)

## (d) IC anion/cation

# replace values below detection limits with half the detection limit value
IC$Br_mg_L <- replace(IC$Br_mg_L, which(IC$Br_mg_L == "<0.01"), "0.005")

# save csv
write.csv(IC, "./data/EDI_data_package/water_anion_cation_measurements.csv",
          row.names = FALSE)

#### (4) Kayak depths ####

## (a) South Fork Eel River
sfkeel_kayak <- read.csv("./data/field_and_lab/raw_data/sfkeel_kayak_measurements.csv")

## (b) Salmon River
## NEED TO CHECK WITH LAUREL AND SALMON RIVER

# replace site names

# want someway to put these together I think

#### (5) Pebble Count ####

#### (6) Ash-Free Dry-Mass ####

# need to put triplicates in?

