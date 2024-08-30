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
field_params <- read_data("field_params")
aq400 <- read_data("aq400")
shimadzu <- read_data("shimadzu")
IC <- read_data("IC")

#### (2) Processing AQ400 values ####

## (a) Values below detection limit
# replace values below detection limits with half the detection limit value
aq400$raw_ammonia_mg_N_L <- replace(aq400$raw_ammonia_mg_N_L, 
                                    which(aq400$raw_ammonia_mg_N_L == "<0.002"),
                                    "0.001")

# convert column to numeric for calculations
aq400$raw_ammonia_mg_N_L <- as.numeric(aq400$raw_ammonia_mg_N_L)

# (b) Calculating true ammonium values based on Emerson et al. 1975

# need to use pH and temperature data to calculate ammonium, so merge df's
water_chemistry <- left_join(field_params, aq400, by = c("field_date", "site_reach", "site", "reach"))

# we unfortunately did not have a pH probe first month in the field
# so let's just assume the missing pH was roughly similar to the first time
# we were able to measure pH for each site_reach
# and call this column "assumed_pH"

# create data frame (that will we convert to a vector) to fill in missing values
assumed_pH <- water_chemistry %>% 
  filter(field_date < mdy("7-28-2022")) %>% 
  mutate(assumed_pH = case_when(site_reach == "RUS-1S" ~ water_chemistry$pH[25],
                                site_reach == "SAL-1S" ~ water_chemistry$pH[49],
                                site_reach == "SAL-2" ~ water_chemistry$pH[50],
                                site_reach == "SAL-3" ~ water_chemistry$pH[51],
                                site_reach == "SFE-M-1S" ~ water_chemistry$pH[22],
                                site_reach == "SFE-M-3" ~ water_chemistry$pH[23],
                                site_reach == "SFE-M-4" ~ water_chemistry$pH[24],
                                site_reach == "RUS-2" ~ water_chemistry$pH[26],
                                site_reach == "RUS-3" ~ water_chemistry$pH[27])) %>% 
  select(assumed_pH)

# create column in water chemistry data frame
water_chemistry$assumed_pH <- water_chemistry$pH

# fill in missing values with above vector
for(i in 1:21) {
  water_chemistry$assumed_pH[i] <- assumed_pH[i,]
}

# function to calculate ammonium using data with assumed pH column
calculate_NH4 <- function(data) {
  # assign variables
  temp = data$temp_C
  pH = data$assumed_pH
  NH3 = data$raw_ammonia_mg_N_L
  
  # calculate pKa
  pKa = 0.09018 + 2727.92/(temp+273.15)
  
  # calculate fraction of NH3
  f = 1/(10^(pKa-pH)+1)
  
  # calculate concentration of ammonium (NH4)
  NH4 = (1-f)*NH3
  
  # creating new column for calculated ammonium (NH4) concentration
  data$ammonium_mg_N_L <- round(NH4, 5) # limit decimal places to 5 like input values
  
  # return data with new column
  return(data)
}

# apply function
water_chemistry <- calculate_NH4(water_chemistry)

#### (3) Processing Shimadzu and Ion Chromotography values


# will save this csv when I decide on final labels
# curious what to do about missing values in regards to modeling
# may talk to joanna about this

# NOTE TO SELF TO FILL IN MISSING DATA WITH DEAD HOBO INFORMATION AND FIND MINIDOT INFO ONCE MORE