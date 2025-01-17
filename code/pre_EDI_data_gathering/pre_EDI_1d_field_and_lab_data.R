#### getting field and lab data together for the EDI package
### Jordan Zabrecky
## last edited: 01.16.2025

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
    select(date_time, site_reach, pressure_mbar_extech)
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

# replace "not recorded" or missing values with NA
percover$depth_cm <- replace(percover$depth_cm, which(percover$depth_cm == "not recorded"), "NA")
percover$riffle_rapid <- replace(percover$riffle_rapid, which(percover$riffle_rapid == "not recorded"), "NA")

# convert date time to character
percover$field_date <- as.character(format(percover$field_date))

# reordering to have depth earlier in the column
percover <- percover %>% 
  relocate(depth_cm, .before = green_algae)

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
write.csv(aq400, "./data/EDI_data_package/water_nutrient_concentrations.csv",
          row.names = FALSE)

## (c) shimadzu carbon measurements

# everything is good so just need to write the csv!
write.csv(shimadzu, "./data/EDI_data_package/water_carbon_concentrations.csv",
          row.names = FALSE)

## (d) IC anion/cation

# replace values below detection limits with half the detection limit value
IC$Br_mg_L <- replace(IC$Br_mg_L, which(IC$Br_mg_L == "<0.01"), "0.005")

# save csv
write.csv(IC, "./data/EDI_data_package/water_anion_cation_concentrations.csv",
          row.names = FALSE)

#### (4) Kayak depths ####

## (a) South Fork Eel River

# read in data
sfkeel_kayak <- read.csv("./data/field_and_lab/raw_data/sfkeel_kayak_measurements.csv")

# change column names, site names, and remove columns we don't necessarily need
sfkeel_kayak <- sfkeel_kayak %>% 
  dplyr::rename(field_date = Date,
         measurement_type = Meas_Type,
         transect = Transect) %>% 
  mutate(meters = case_when(is.na(Width_m) ~ (Depth_cm_final / 100),
                            TRUE ~ Width_m),
         site = case_when(Site == "SfkEel_Miranda" ~ "SFE-M",
                          Site == "SfkEel_Standish" ~ "SFE-SH")) %>% 
  select(field_date, site, transect, measurement_type, meters)

## (b) Russian River

# read in data
russian_kayak <- read.csv("./data/field_and_lab/raw_data/russian_kayak_measurements.csv")

# change column names, site names, and remove columns we don't necessarily need
russian_kayak <- russian_kayak %>% 
  dplyr::rename(field_date = Date,
                measurement_type = Meas_Type,
                transect = Transect) %>% 
  mutate(meters = case_when(is.na(Width_m) ~ (Depth_cm_final / 100),
                            TRUE ~ Width_m),
         site = "RUS") %>% 
  select(field_date, site, transect, measurement_type, meters)

## (C) Salmon River

# read in data
salmon_kayak <- read.csv("./data/field_and_lab/raw_data/salmon_kayak_measurements.csv")

# change column names, add site name, transect, measurement type, and remove columns we don't necessarily need
salmon_kayak <- salmon_kayak %>% 
  dplyr::rename(field_date = Date, 
                meters = Depth_m) %>% 
  mutate(site = "SAL",
         measurement_type = "depth",
         transect = "NA") %>% # no transect counts for salmon river
  select(field_date, site, transect, measurement_type, meters)

# rbind two dataframes together and omit any NA (e.g. from bad GPS estimates for width)
kayak_final <- rbind(sfkeel_kayak, salmon_kayak, russian_kayak) %>% 
  na.omit() # won't omit the character put in NA's above

# convert field_date to yyyy-mm-dd
kayak_final$field_date <- mdy(kayak_final$field_date)

# save csv
write.csv(kayak_final, "./data/EDI_data_package/kayak_depth_width.csv", row.names = FALSE)

#### (5) Pebble Count ####

# read in data
pebble_count <- read.csv("./data/field_and_lab/raw_data/pebble_count.csv")

# change column and site names
pebble_count <- pebble_count %>% 
  dplyr::rename(field_date = Date,
                sample_number = X.,
                intermediate_axis_mm = Intermediate_axis._mm,
                riffle_pool = Riffle_or_pool,
                transect = Transect) %>% 
  mutate(site = case_when(Site == "SfkEel_Miranda" ~ "SFE-M",
                          Site == "SfkEel_Standish" ~ "SFE-SH")) %>% 
  mutate(site_reach = case_when(Section == "EEL-4S" ~ "SFE-M-1S",
                                Section == "EEL-BUG" ~ "SFE-M-2",
                                Section == "EEL-3UP" ~ "SFE-M-3",
                                Section == "EEL-2UP" ~ "SFE-M-4",
                                Section == "EEL-STH" ~ "SFE-SH-1S",
                                Section == "kayak_entry" ~ "SFE-SH-kayak-entry")) %>% 
  select(field_date, site, site_reach, transect, sample_number, intermediate_axis_mm, riffle_pool)

any(is.na(pebble_count))

# convert field_date to yyyy-mm-dd
pebble_count$field_date <- mdy(pebble_count$field_date)

# save csv
write.csv(pebble_count, "./data/EDI_data_package/pebble_count.csv", row.names = FALSE)

#### (6) Sontek Measured Discharge ####

# read in data
sontek <- read.csv("./data/field_and_lab/raw_data/sontek_discharge.csv")

# make date time column and select what we care about and notes column for SFE-SH
sontek <- sontek %>% 
  mutate(date_time = mdy_hm(paste(date, time, sep = " "))) %>% 
  select(site, date_time, discharge_m3_s)

# making a function to change date_time column to character to avoid issue in some
# versions of R where date_time drops the "00:00:00" / midnight time when saved to csv
sontek$date_time <- as.character(format(sontek$date_time))

# save csv
write.csv(sontek, "./data/EDI_data_package/discharge_measurements.csv", row.names = FALSE)

#### (6) Target Sample Percent Organic Matter ####

# reading in weights measured at UC-B Angelo Reserve Lab
angelo <- ldply(list.files(path = "./data/field_and_lab/raw_data/", 
                           pattern = "angelo"), function(filename) {
                             d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
                             d$field_date <- mdy(d$field_date)
                             return(d)
                             })

# calculate dry weight (both organic and inorganic matter) without tin weight
angelo$dry_sample_g <- angelo$dry_sample_tin_g - angelo$tin_g

# calculate AFDM (ash-free dry mass or organic matter)
angelo$afdm_g <- angelo$dry_sample_tin_g - angelo$combusted_sample_tin_g

# calculate percent organic matter (percent of matter that is organic/combusted)
angelo$percent_organic_matter <- (angelo$afdm_g / angelo$dry_sample_g) * 100

# round everything to reasonable decimal places
angelo <- angelo %>% 
  mutate(dry_sample_g = round(dry_sample_g, digits = 5),
         afdm_g = round(afdm_g, digits = 5),
         percent_organic_matter = round(percent_organic_matter, digits = 5))

# replace missing values with NA (samples that molded) as missing value code for EDI
angelo <- angelo %>% replace(is.na(.), "NA")

# save csv
write.csv(angelo, "./data/EDI_data_package/target_sample_percent_organic_matter.csv", row.names = FALSE)

#### (7) Target Sample Anatoxins ####

# reading in ATX data and sample metadata
anatoxins_full <- read.csv("./data/field_and_lab/raw_data/SUNY_ESF_LCMS_MS.csv")
metadata <- read.csv("./data/field_and_lab/raw_data/SUNY_ESF_metadata.csv")

## (a) cleaning ESF csv
# lots of cleaning to be done with SUNY ESF data sheet...
view(anatoxins_full)

# columns 2, 26, 27, 31, 32, 33, 36, 38, 40, 42 are important
anatoxins_reduced <- anatoxins_full[c(2, 26, 27, 34, 37, 39, 41, 43)]
# (I already know all of our samples had none of the 
# three cylindrospermopsin derivatives tested, so not including them here)

# rename these columns to be more computer-legible
labels <- c("ESF_ID", "MCY_ug_g", "MCY_det_limit_full", "ATX_det_limit_full", 
            "ATXa_ug_g", "HTXa_ug_g", "dhATXa_ug_g", "dhHTXa_ug_g")

# adding labels as column names
colnames(anatoxins_reduced) <- labels

# convert columns that are character class to numeric 
# (this will replace the "-" with NA)
anatoxins_reduced[5:8] <- sapply(anatoxins_reduced[5:8], as.numeric)

# parse detection limit column and convert to numeric
anatoxins_reduced <- anatoxins_reduced %>%
  mutate(MCY_det_limit = as.numeric(str_sub(anatoxins_reduced$MCY_det_limit_full,3, 6)),
         ATX_det_limit = as.numeric(str_sub(anatoxins_reduced$ATX_det_limit_full,3, 8))) %>% 
  select(-MCY_det_limit_full, -ATX_det_limit_full)

# fill in missing values/non-detects with "ND"
anatoxins_processed <- replace(anatoxins_reduced, is.na(anatoxins_reduced), "ND")

# we have one sample where the microcystin detection limit was not reported
anatoxins_processed$MCY_det_limit[which(anatoxins_processed$MCY_det_limit == "ND")] <- "NA"

# match ESF_ID with metadata
combined <- left_join(metadata, anatoxins_processed, by = "ESF_ID") %>% 
  dplyr::select(!ESF_ID) %>% 
  # un-needed Gloeotrichia and Microcoleus from Poudre
  filter(sample_type != "Gloeotrichia" & sample_type != "Microcoleus")

# convert field_date to yyyy-mm-dd
combined$field_date <- mdy(combined$field_date)

# convert to character to add in NAs
combined$field_date <- as.character(combined$field_date)

# replace blank sample information with NA (no information) as missing value code for EDI
combined <- combined %>% replace(is.na(.), "NA")

# put detection limit columns before values
combined <- combined %>% 
  relocate(MCY_det_limit, .before = MCY_ug_g) %>% 
  relocate(ATX_det_limit, .before = MCY_ug_g)

# save csv
write.csv(combined, './data/EDI_data_package/anatoxin_concentrations.csv', row.names = FALSE)
