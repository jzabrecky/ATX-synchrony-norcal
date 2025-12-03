#### Used to create table on watershed characteristics and quality ranges
### Jordan Zabrecky (adapted from previous authors)
## last edited 06.18.2025

# This script grabs and summarizes data to make table describing watershed
# and water quality characteristics for all of our study rivers

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "plyr", "lubridate"), require, character.only = T)

# load landcover data
landcover <- ldply(list.files(path = "./data/NLCD19_landcover/", pattern = "_landcover.csv"), function(filename) {
  d <- read.csv(paste("./data/NLCD19_landcover/", filename, sep = ""))
  d$site = filename %>% stringr::str_remove("_landcover.csv")
  return(d)
})

# load in discharge data
discharge <- ldply(list.files(path = "./data/USGS/", pattern = "_continuous.csv"), function(filename) {
  d <- read.csv(paste("./data/USGS/", filename, sep = ""))
  d$date_time <- ymd_hms(d$date_time)
  return(d)
})

# load in nutrient data
nutrients <- rbind(read.csv("./data/field_and_lab/allrivers22_combined.csv") %>% 
                     select(field_date, site, temp_C, pH, oPhos_ug_P_L, 
                            cond_uS_cm, nitrate_mg_N_L,ammonium_mg_N_L), 
                   read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
                     select(field_date, site, temp_C, pH, oPhos_ug_P_L, 
                            cond_uS_cm, nitrate_mg_N_L,ammonium_mg_N_L))

#### (2) Landcover % ####

# pivot longer 
landcover_wider <- landcover %>% 
  dplyr::rename(percover = Coverage....) %>% 
  select(site, percover, Type) %>% 
  pivot_wider(values_from = "percover", names_from = "Type")

# change colnames to be more computer friendly
colnames(landcover_wider) <- str_replace_all(colnames(landcover_wider), fixed(" "), "")
colnames(landcover_wider) <- str_remove_all(colnames(landcover_wider), ",")
colnames(landcover_wider) <- str_remove_all(colnames(landcover_wider), "/")

# calculate summary statistics
landcover_summary <- landcover_wider %>% 
  mutate(developed = DevelopedOpenSpace + DevelopedLowIntensity + 
           DevelopedMediumIntensity + DevelopedHighIntensity,
         forest = DeciduousForest + EvergreenForest + MixedForest,
         shrub_grass = ShrubScrub + GrasslandHerbaceous,
         agriculture = PastureHay + CultivatedCrops) %>% 
  select(site, developed, forest, shrub_grass, agriculture)

# save as csv
write.csv(landcover_summary, "./data/NLCD19_landcover/summary.csv",
          row.names = FALSE)

#### (3) Discharge Range ####

# will have to use filter to make sure that range is within 
# when we were sampling

# south fork eel miranda
disc_sfkeel_mir_2022 <- discharge %>% 
  filter(site == "sfkeel_mir") %>% 
  filter(date_time > "2022-06-29 00:00:00" & date_time < "2022-09-17 11:59:00")
median(disc_sfkeel_mir_2022$discharge_m3_s)

disc_sfkeel_mir_2023 <- discharge %>% 
  filter(site == "sfkeel_mir") %>% 
  filter(date_time > "2023-06-20 00:00:00" & date_time < "2023-09-24 11:59:00")
median(disc_sfkeel_mir_2023$discharge_m3_s)

# south fork eel standish hickey
disc_sfkeel_sth <- discharge %>% 
  filter(site == "sfkeel_sth") %>% 
  filter(date_time > "2023-06-25 00:00:00" & date_time < "2023-09-24 11:59:00")

# russian river
disc_russian <- discharge %>% 
  filter(site == "russian") %>% 
  filter(date_time > "2022-06-24 00:00:00" & date_time < "2022-09-15 11:59:00")
median(disc_russian$discharge_m3_s)

# salmon (for this study only using 2022 data)
disc_salmon <- discharge %>% 
  filter(site == "salmon") %>% 
  filter(date_time > "2022-06-27 00:00:00" & date_time < "2022-09-22 11:59:00")
median(disc_salmon$discharge_m3_s)

#### (4) Water Quality Parameters ####

water_quality_summary <- nutrients %>% 
  mutate(year = year(field_date)) %>% 
  dplyr::group_by(site, year) %>% 
  dplyr::summarize(temp_C_min = min(temp_C),
                   temp_C_max = max(temp_C),
                   pH_min = min(pH, na.rm = TRUE),
                   pH_max = max(pH, na.rm = TRUE),
                   cond_min = min(cond_uS_cm, na.rm = TRUE),
                   cond_max = max(cond_uS_cm, na.rm = TRUE),
                   amm_min = min(ammonium_mg_N_L),
                   amm_max = max(ammonium_mg_N_L),
                   nitrate_min = min(nitrate_mg_N_L),
                   nitrate_max = max(nitrate_mg_N_L),
                   phos_min = min(oPhos_ug_P_L),
                   phos_max = max(oPhos_ug_P_L))
