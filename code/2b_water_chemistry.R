#### putting together water chemistry data from field and lab measurements
### Jordan Zabrecky
## last edited: 11.29.2024

# This code combines in-situ water chemistry measurements, AQ400 nitrate, ammonium,
# and orthophosphate values, Shimadzu total dissolved carbon, dissolved organic
# carbon, and Ion Chromatography anions & cations (2022 only) for water samples
# and also processes duplicates

#### (1) Loading libraries and reach data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

## loading raw data

# function to read in data from our folder to a data frame based on name
# (note that the folder path is hard-coded in)
read_data <- function(name) {
  ldply(list.files(path = "./data/EDI_data_package/", 
                   pattern = name), function(filename) {
                     d <- read.csv(paste("data/EDI_data_package/", filename, sep = ""))
                     d$field_date <- ymd(d$field_date)
                     return(d)
                   })
}

# reading in all water chemistry parameters
field_params <- read_data("in_situ")
aq400 <- read_data("nutrient") %>% 
  dplyr::rename(ammonium_mg_N_L = calculated_ammonium_mg_N_L )
shimadzu <- read_data("carbon")
IC <- read_data("anion_cation")

#### (2) Processing AQ400 values ####

## (a) analyzing instrument duplicates
aq400_instrument_dups <- aq400 %>%
  filter(instrument_duplicate == "y") %>% 
  dplyr::group_by(site_reach, site, reach, field_date, assumed_pH) %>% 
  dplyr::summarize(mean_ophos = mean(oPhos_ug_P_L),
                   sd_ophos = sd(oPhos_ug_P_L),
                   rsd_ophos = sd_ophos * 100 / mean_ophos,
                   mean_nitrate = mean(nitrate_mg_N_L),
                   sd_nitrate = sd(nitrate_mg_N_L),
                   rsd_nitrate = sd_nitrate * 100 / mean_nitrate,
                   mean_amm = mean(ammonium_mg_N_L), 
                   sd_amm = sd(ammonium_mg_N_L),
                   rsd_amm = sd_amm * 100 / mean_amm,
                   # preserved keeps value if it was only a duplicate for a
                   # different nutrient (for when we join this back in)
                   preserve_ophos = mean(na.omit(oPhos_ug_P_L)),
                   preserve_nitrate = mean(na.omit(nitrate_mg_N_L)),
                   preserve_amm = mean(na.omit(ammonium_mg_N_L)))

# look at instrument duplicate results
view(aq400_instrument_dups) #ophos and nitrate more consistent than ammonium
mean(na.omit(aq400_instrument_dups$rsd_ophos)) # average 3.44%
mean(na.omit(aq400_instrument_dups$rsd_nitrate)) # average 4.52%
mean(na.omit(aq400_instrument_dups$rsd_amm)) # average 11.31%

## (b) analyzing field_duplicates
aq400_field_dups <- aq400 %>%
  filter(field_duplicate == "y") %>% 
  dplyr::group_by(site_reach, site, reach, field_date, assumed_pH) %>% 
  dplyr::summarize(mean_ophos = mean(oPhos_ug_P_L),
                   sd_ophos = sd(oPhos_ug_P_L),
                   rsd_ophos = sd_ophos * 100 / mean_ophos,
                   mean_nitrate = mean(nitrate_mg_N_L),
                   sd_nitrate = sd(nitrate_mg_N_L),
                   rsd_nitrate = sd_nitrate * 100 / mean_nitrate,
                   mean_amm = mean(ammonium_mg_N_L), 
                   sd_amm = sd(ammonium_mg_N_L),
                   rsd_amm = sd_amm * 100 / mean_amm)

# look at field duplicate results
view(aq400_field_dups) #ophos more consistent than nitrate and ammonium
mean(aq400_field_dups$rsd_ophos) # average 5.88%
mean(aq400_field_dups$rsd_nitrate) # average 11.70%
mean(aq400_field_dups$rsd_amm) # average 24.71% which is high but field duplicate

## (c) putting aq400 values back together

# original data that does not contain duplicates
nutrients <- aq400 %>% 
  filter(instrument_duplicate == "n" & field_duplicate == "n") %>% 
  select(site_reach, site, reach, field_date, oPhos_ug_P_L, nitrate_mg_N_L,
         ammonium_mg_N_L, assumed_pH)

# instrument duplicates
aq400_instrument_dups <- aq400_instrument_dups %>% 
  dplyr::rename(oPhos_ug_P_L = preserve_ophos,
                nitrate_mg_N_L = preserve_nitrate,
                ammonium_mg_N_L = preserve_amm) %>% 
  select(site_reach, site, reach, field_date, oPhos_ug_P_L, nitrate_mg_N_L,
         ammonium_mg_N_L, assumed_pH)

# field duplicates
aq400_field_dups <- aq400_field_dups %>% 
  dplyr::rename(oPhos_ug_P_L = mean_ophos,
                nitrate_mg_N_L = mean_nitrate,
                ammonium_mg_N_L = mean_amm) %>% 
  select(site_reach, site, reach, field_date, oPhos_ug_P_L, nitrate_mg_N_L,
         ammonium_mg_N_L, assumed_pH)

# merging together data frames
nutrients <- rbind(nutrients, aq400_field_dups, aq400_instrument_dups)

# join with field parameters
water_chemistry <- left_join(field_params, nutrients, by = c("field_date",
                                                                "site_reach",
                                                                "site", "reach"))

#### (3) Processing Shimadzu data ####

## (a) analyzing field duplicates
shimadzu_field_dups <- shimadzu %>%
  filter(field_duplicate == "y") %>% 
  dplyr::group_by(site_reach, site, reach, field_date) %>% 
  dplyr::summarize(mean_TDC = mean(TDC_mg_L),
                   sd_TDC = sd(TDC_mg_L),
                   rsd_TDC = sd_TDC * 100 / mean_TDC,
                   mean_DOC = mean(DOC_mg_L),
                   sd_DOC = sd(DOC_mg_L),
                   rsd_DOC = sd_DOC * 100 / mean_DOC)

# look at field duplicate results
view(shimadzu_field_dups) #DOC worse than TDC but again field duplicates not instrument 
mean(shimadzu_field_dups$rsd_TDC) # average 12.84%
mean(shimadzu_field_dups$rsd_DOC) # average 25.07%

## (b) putting shimadzu values back together

# original data that does not contain duplicates
carbon <- shimadzu %>% 
  filter(field_duplicate == "n") %>% 
  select(site_reach, site, reach, field_date, TDC_mg_L, DOC_mg_L)

# field duplicates
shimadzu_field_dups <- shimadzu_field_dups %>% 
  dplyr::rename(TDC_mg_L = mean_TDC,
                DOC_mg_L = mean_DOC) %>% 
  select(site_reach, site, reach, field_date, TDC_mg_L, DOC_mg_L)

# merging together data frames
carbon <- rbind(carbon, shimadzu_field_dups)

# join with water chemistry
water_chemistry <- left_join(water_chemistry, carbon, by = c("field_date",
                                                             "site_reach",
                                                             "site", "reach"))

#### (4) Processing IC data ####

## (a) analyzing field duplicates
IC_field_dups <- IC %>%
  filter(field_duplicate == "y")

# just looking at them visually since there is only three and will 
# probably not use this data anyways
view(IC_field_dups)

# average out duplicates
IC_field_dups <- IC_field_dups %>% 
  dplyr::group_by(field_date, site_reach, site, reach) %>% 
  dplyr::summarize(Cl_mg_L = mean(Cl_mg_L),
                   SO4_mg_L = mean(SO4_mg_L),
                   Br_mg_L = mean(Br_mg_L),
                   Na_mg_L = mean(Na_mg_L),
                   K_mg_L = mean(K_mg_L),
                   Mg_mg_L = mean(Mg_mg_L),
                   Ca_mg_L = mean(Ca_mg_L))

# original data that does not contain duplicates
anions_cations <- IC %>% 
  filter(field_duplicate == "n") %>% 
  select(!Br_below_detection & !field_duplicate)

# join in data
anions_cations <- rbind(anions_cations, IC_field_dups)

# join with water chemistry
water_chemistry <- left_join(water_chemistry, anions_cations, by = c("field_date",
                                                             "site_reach",
                                                             "site", "reach"))

# save csv
write.csv(water_chemistry, "./data/field_and_lab/water_chemistry.csv", row.names = FALSE)
