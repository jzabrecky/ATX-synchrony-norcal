#### Q1: Table and Figure for across river (mean behavior) patterns
### Jordan Zabrecky
## last edited: 06.02.2025

# This script creates information for a table describing taxa-specific cover, anatoxins,
# and GPP across rivers from 2022 sampling and creates a main manuscript figure

#### (1) Loading in libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "cowplot", 
         "gridExtra", "grid"), 
       require, character.only = T)

##  loading in data

# whole river % cover summary
percover_river <- read.csv("./data/field_and_lab/percover_bysite.csv")

# 2022 field data
data_bc <- read.csv("./data/field_and_lab/allrivers22_combined.csv") %>% 
  mutate(field_date = ymd(field_date))

# also need full survey data to make whole river summary for transect observations
surveys <- read.csv("./data/EDI_data_package/benthic_surveys.csv") %>% 
  mutate(field_date = ymd(field_date))

# gpp
gpp_sfkeel <- read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2022_metab.csv")
gpp_rus <- read.csv("./data/metab_model_outputs_processed/russian_USGS_2022_metab.csv")
gpp_sal <- read.csv("./data/metab_model_outputs_processed/salmon_karuk_2022_metab.csv")
gpp <- rbind(gpp_sfkeel, gpp_rus, gpp_sal)

#### (2) Creating descriptive table

## (a) overall % of transects w/ taxa-specific cyano observations

# change "y" to 1's and "n" to 0's
surveys$Micro_pres[surveys$Micro_pres == "y"] <- 1
surveys$Ana_Cyl_pres[surveys$Ana_Cyl_pres == "y"] <- 1
surveys$Micro_pres[surveys$Micro_pres == "n"] <- 0
surveys$Ana_Cyl_pres[surveys$Ana_Cyl_pres == "n"] <- 0

# convert to numeric
surveys$Micro_pres <- as.numeric(surveys$Micro_pres)
surveys$Ana_Cyl_pres <- as.numeric(surveys$Ana_Cyl_pres)

# calculate total number of transects sampled and amount that had taxa-specific observations
transect_proportions <- surveys %>% 
  mutate(year = year(field_date)) %>% 
  filter(year == 2022) %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(total_transects = length(Micro_pres),
                   Micro_observations = sum(Micro_pres),
                   AnaCyl_observations = sum(Ana_Cyl_pres),
                   total_percent_Micro_observations = Micro_observations / total_transects * 100,
                   total_percent_AnaCyl_observations = AnaCyl_observations / total_transects * 100)

# get dates of observations

## (x) % of anatoxin detections and duration of detection

# microcoleus
atx_detection_m <- data_bc %>% 
  select(field_date, site_reach, site, TM_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # drop NA which indicates no sample taken
  dplyr::group_by(site) %>% 
  dplyr::summarize(num_samples = length(TM_ATX_all_ug_orgmat_g),
                   num_detections = sum(TM_ATX_all_ug_orgmat_g > 0),
                   percent_detection = num_detections / num_samples * 100)

# anabaena/cylindrospermum
atx_detection_ac <- data_bc %>% 
  select(field_date, site_reach, site, TAC_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # drop NA which indicates no sample taken
  dplyr::group_by(site) %>% 
  dplyr::summarize(num_samples = length(TAC_ATX_all_ug_orgmat_g),
                   num_detections = sum(TAC_ATX_all_ug_orgmat_g > 0),
                   percent_detection = num_detections / num_samples * 100)


# 

# so order:
# %transects w/ cyano observed overall
# max % transects w/ cyano observed
# mean % cover
# max % cover
# duration of observation
# date of max observation

# %samples with ATX detection
# mean ATX detection
# max ATX detection
# duration of ATX detection
# date of max ATX detection

# second table:

# GPP mean, max, date of highest GPP

# duration of atx detections

# percent of detections in samples, average proportion of transects present, max proportion of transects present

# GPP mean and max; date of max
# duration