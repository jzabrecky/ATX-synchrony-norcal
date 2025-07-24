#### Q1: Table for across river (mean behavior) patterns
### Jordan Zabrecky
## last edited: 07.24.2025

# This script creates information for a table describing taxa-specific cover, anatoxins,
# and GPP across rivers from 2022 sampling

#### (1) Loading in libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), 
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
gpp <- rbind(read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2022_metab.csv"),
             read.csv("./data/metab_model_outputs_processed/russian_USGS_2022_metab.csv"),
             read.csv("./data/metab_model_outputs_processed/salmon_karuk_2022_metab.csv"))

# anatoxins only for a supplemental table
atx <- data_bc %>% 
  select(field_date, site_reach, TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g)

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

# change 7/7 date to 7/6 as sampling at RUS-3 was supposed to be the prior
# day but field issues
data_bc[which(data_bc == "2022-07-07"),]$field_date <- ymd("2022-07-06")

# get dates of observations
duration_m <- data_bc %>% 
  mutate(present = case_when(proportion_micro_transects > 0 ~ "yes",
                             TRUE ~ "no")) %>%
  select(site_reach, field_date, present) %>%
  pivot_wider(names_from = "field_date", values_from = "present")

duration_ac <- data_bc %>% 
  mutate(present = case_when(proportion_ana_cyl_transects > 0 ~ "yes",
                             TRUE ~ "no")) %>%
  select(site_reach, field_date, present) %>%
  pivot_wider(names_from = "field_date", values_from = "present")

## (b) mean & max % cover and date of max

# calculate summary of % cover data using averages already calculated for river-level
percover_summary <- data_bc %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(mean_cover_m = mean(microcoleus),
                   max_cover_m = max(microcoleus),
                   max_date_m = field_date[which.max(microcoleus)],
                   mean_cover_ac = mean(anabaena_cylindrospermum),
                   max_cover_ac = max(anabaena_cylindrospermum),
                   max_date_ac = field_date[which.max(anabaena_cylindrospermum)])

## (c) % of anatoxin detections and duration

# % of detections
atx_detection_m <- data_bc %>% 
  select(field_date, site, TM_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # drop NA which indicates no sample taken
  dplyr::group_by(site) %>% 
  dplyr::summarize(num_samples = length(TM_ATX_all_ug_orgmat_g),
                   num_detections = sum(TM_ATX_all_ug_orgmat_g > 0),
                   percent_detection = num_detections / num_samples * 100)

atx_detection_ac <- data_bc %>% 
  select(field_date, site, TAC_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # drop NA which indicates no sample taken
  dplyr::group_by(site) %>% 
  dplyr::summarize(num_samples = length(TAC_ATX_all_ug_orgmat_g),
                   num_detections = sum(TAC_ATX_all_ug_orgmat_g > 0),
                   percent_detection = num_detections / num_samples * 100)

# duration of anatoxin detection 
# (for additional table, combine w/ presence to distinguish why there was no sample)
# (aka was it not present OR was there just not enough material)
duration_atx_m <- data_bc %>% 
  mutate(detection = case_when(TM_ATX_all_ug_orgmat_g > 0 ~ as.character(TM_ATX_all_ug_orgmat_g),
                               TM_ATX_all_ug_orgmat_g == 0.0 ~ "ND",
                               is.na(TM_ATX_all_ug_orgmat_g) ~ "no sample")) %>%
  select(site_reach, field_date, detection) %>%
  pivot_wider(names_from = "site_reach", values_from = "detection")

duration_atx_ac <- data_bc %>% 
  mutate(detection = case_when(TAC_ATX_all_ug_orgmat_g > 0 ~ as.character(TAC_ATX_all_ug_orgmat_g),
                               TAC_ATX_all_ug_orgmat_g == 0.0 ~ "ND",
                               is.na(TAC_ATX_all_ug_orgmat_g) ~ "no sample")) %>%
  select(site_reach, field_date, detection) %>%
  pivot_wider(names_from = "site_reach", values_from = "detection")

# fill NA's (regardless if from absence or ND) with 0 and to make anatoxin calculations
data_bc_filled <- data_bc %>% 
  mutate(TM_ATX_all_ug_orgmat_g = replace_na(TM_ATX_all_ug_orgmat_g, 0),
         TAC_ATX_all_ug_orgmat_g = replace_na(TAC_ATX_all_ug_orgmat_g, 0))

# calculate daily average atx at each river
daily_avg_atx_m <- data_bc_filled %>% 
  select(site, field_date, TM_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # remove when sample is not present from one of the reaches
  dplyr::group_by(site, field_date) %>% 
  dplyr::summarize(mean_TM_ATX = mean(TM_ATX_all_ug_orgmat_g))
daily_avg_atx_ac <- data_bc_filled %>% 
  select(site, field_date, TAC_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # remove when sample is not present from one of the reaches
  dplyr::group_by(site, field_date) %>% 
  dplyr::summarize(mean_TAC_ATX = mean(TAC_ATX_all_ug_orgmat_g))

## (d) max and mean atx concentrations
atx_m <- daily_avg_atx_m %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(mean_atx = mean(mean_TM_ATX),
                   max_atx = max(mean_TM_ATX),
                   max_date = field_date[which.max(mean_TM_ATX)])

atx_ac <- daily_avg_atx_ac %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarize(mean_atx = mean(mean_TAC_ATX),
                   max_atx = max(mean_TAC_ATX),
                   max_date = field_date[which.max(mean_TAC_ATX)])


## (e) gpp mean and max
gpp_max_min_mean <- gpp %>% 
  dplyr::group_by(site_year) %>% 
  dplyr::summarize(mean_gpp = mean(GPP.mean, na.rm = TRUE),
                   max_gpp = max(GPP.mean),
                   max_date = date[which.max(GPP.mean)],
                   min_gpp = min(GPP.mean),
                   min_date = date[which.min(GPP.mean)])
