#### Q2: Table and Figure for within river reach-scale patterns
### Jordan Zabrecky
## last edited: 06.10.2025

# This script creates information for a table describing taxa-specific cover, anatoxins,
# and GPP across reaches within a single river from 2023 sampling

#### (1) Loading in libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "cowplot", 
         "gridExtra", "grid"), 
       require, character.only = T)

## reading in data

# 2023 south fork eel field data
data_bc <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  mutate(field_date = ymd(field_date))

# also need original survey data to tally total number of transects sampled
surveys <- read.csv("./data/EDI_data_package/benthic_surveys.csv") %>% 
  mutate(field_date = ymd(field_date))

# gpp data
gpp_mir <- read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2023_metab.csv")
gpp_sfk <- read.csv("./data/metab_model_outputs_processed/sfkeel_sth_2023_metab.csv")
gpp <- rbind(gpp_mir, gpp_sfk)

#### (2) Creating descriptive table ####

## (a) overall % of transects w/ taxa-specific cyano observations & duration of observation

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
  filter(year == 2023) %>% 
  filter(site == "SFE-M" | site == "SFE-SH") %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(total_transects = length(Micro_pres),
                   Micro_observations = sum(Micro_pres),
                   AnaCyl_observations = sum(Ana_Cyl_pres),
                   total_percent_Micro_observations = Micro_observations / total_transects * 100,
                   total_percent_AnaCyl_observations = AnaCyl_observations / total_transects * 100)

# to make it easier to see duration for all change sampling on 7/11
# to 7/10 (was supposed to be completed that day but field difficulties!)
data_bc[which(data_bc$field_date == ymd("2023-07-11")),]$field_date <- ymd("2023-07-10")

# get dates of observations
duration_m <- data_bc %>% 
  mutate(present = case_when(proportion_micro_transects > 0 ~ "yes",
                             TRUE ~ "no")) %>%
  select(site_reach, field_date, present) %>%
  pivot_wider(names_from = "field_date", values_from = "present") %>% 
  relocate(`2023-06-20`, .before = `2023-06-25`)

duration_ac <- data_bc %>% 
  mutate(present = case_when(proportion_ana_cyl_transects > 0 ~ "yes",
                             TRUE ~ "no")) %>%
  select(site_reach, field_date, present) %>%
  pivot_wider(names_from = "field_date", values_from = "present") %>% 
  relocate(`2023-06-20`, .before = `2023-06-25`)
  
## (b) mean & max % cover and date of max

# calculate summary of % cover data
percover_summary <- data_bc %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(mean_cover_m = mean(microcoleus),
                   max_cover_m = max(microcoleus),
                   max_date_m = field_date[which.max(microcoleus)],
                   mean_cover_ac = mean(anabaena_cylindrospermum),
                   max_cover_ac = max(anabaena_cylindrospermum),
                   max_date_ac = field_date[which.max(anabaena_cylindrospermum)])

## (c) % of anatoxin detections and duration of detection

# % of detections
atx_detection_m <- data_bc %>% 
  select(field_date, site_reach, TM_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # drop NA which indicates no sample taken
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(num_samples = length(TM_ATX_all_ug_orgmat_g),
                   num_detections = sum(TM_ATX_all_ug_orgmat_g > 0),
                   percent_detection = num_detections / num_samples * 100)

atx_detection_ac <- data_bc %>% 
  select(field_date, site_reach, TAC_ATX_all_ug_orgmat_g) %>% 
  na.omit() %>% # drop NA which indicates no sample taken
  dplyr::group_by(site_reach) %>% 
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
  pivot_wider(names_from = "site_reach", values_from = "detection") %>% 
  relocate("SFE-SH-1S", .after = "SFE-M-4")

duration_atx_ac <- data_bc %>% 
  mutate(detection = case_when(TAC_ATX_all_ug_orgmat_g > 0 ~ as.character(TAC_ATX_all_ug_orgmat_g),
                               TAC_ATX_all_ug_orgmat_g == 0.0 ~ "ND",
                               is.na(TAC_ATX_all_ug_orgmat_g) ~ "no sample")) %>%
  select(site_reach, field_date, detection) %>%
  pivot_wider(names_from = "site_reach", values_from = "detection") %>% 
  relocate("SFE-SH-1S", .after = "SFE-M-4")

## (d) max and mean atx concentrations

# fill NA's (regardless if from absence or ND) with 0 and to make anatoxin calculations
data_bc_filled <- data_bc %>% 
  mutate(TM_ATX_all_ug_orgmat_g = replace_na(TM_ATX_all_ug_orgmat_g, 0),
         TAC_ATX_all_ug_orgmat_g = replace_na(TAC_ATX_all_ug_orgmat_g, 0))

# calculate mean & max for taxa-specific atx
atx_m <- data_bc_filled %>% 
  select(site_reach, field_date, TM_ATX_all_ug_orgmat_g) %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(mean_atx = mean(TM_ATX_all_ug_orgmat_g),
                   max_atx = max(TM_ATX_all_ug_orgmat_g),
                   max_date = field_date[which.max(TM_ATX_all_ug_orgmat_g)])

atx_ac <- data_bc_filled %>% 
  select(site_reach, field_date, TAC_ATX_all_ug_orgmat_g) %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(mean_atx = mean(TAC_ATX_all_ug_orgmat_g),
                   max_atx = max(TAC_ATX_all_ug_orgmat_g),
                   max_date = field_date[which.max(TAC_ATX_all_ug_orgmat_g)])

## (e) gpp mean and max
gpp_max_min_mean <- gpp %>% 
  dplyr::group_by(site_year) %>% 
  dplyr::summarize(mean_gpp = mean(GPP.mean, na.rm = TRUE),
                   max_gpp = max(GPP.mean),
                   max_date = date[which.max(GPP.mean)],
                   min_gpp = min(GPP.mean),
                   min_date = date[which.min(GPP.mean)])
