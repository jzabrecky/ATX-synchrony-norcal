#### processing anatoxin concentrations from benthic mats
### Jordan Zabrecky
## last edited 01.02.2024

# This script takes in anatoxin concentrations, chlorophyll-a concentrations, 
# and percent organic matter from targeted samples (TM- target Microcoleus and
# TAC- target Anabaena and/or Cylindrospermum) and processes them by calculating
# amount of anatoxin per chlorophyll-a and percent organic matter

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

## (a) anatoxin data

# read in data
anatoxins <- read.csv("./data/EDI_data_package/anatoxin_concentrations.csv") %>% 
  filter(sample_type != "NT")

## where ND, fill in samples with 0
# pivot longer
anatoxins_longer <- pivot_longer(anatoxins, cols = c(11:15), names_to = "toxin", 
                                 values_to = "value")

# replace ND with 0
anatoxins_longer$value <- replace(anatoxins_longer$value, 
                            which(anatoxins_longer$value == "ND"), 0)

# convert to numeric
anatoxins_longer$value <- as.numeric(anatoxins_longer$value)

# pivot back to wider
anatoxins <- pivot_wider(anatoxins_longer, names_from = "toxin", values_from = "value")

# values below detection limit were included in our report, but we will not be
# reporting those values, so let's replace them with 0
anatoxins_processed <- anatoxins %>% 
  mutate(
    MCY_ug_g = case_when(MCY_ug_g < MCY_det_limit ~ 0,
                         TRUE ~ MCY_ug_g),
    ATXa_ug_g = case_when(ATXa_ug_g < ATX_det_limit ~ 0,
                          TRUE ~ ATXa_ug_g),
    HTXa_ug_g = case_when(HTXa_ug_g < ATX_det_limit ~ 0,
                          TRUE ~ HTXa_ug_g),
    dhATXa_ug_g = case_when(dhATXa_ug_g < ATX_det_limit ~ 0,
                            TRUE ~ dhATXa_ug_g),
    dhHTXa_ug_g = case_when(dhHTXa_ug_g < ATX_det_limit ~ 0,
                            TRUE ~ dhHTXa_ug_g)
  )

# calculate total anatoxins
anatoxins_processed <- anatoxins_processed %>% 
  mutate(ATX_all_ug_g = ATXa_ug_g + HTXa_ug_g + dhATXa_ug_g + dhHTXa_ug_g)

# check blanks to make sure they had no anatoxin detections
anatoxins_processed$ATX_all_ug_g[which(anatoxins_processed$sample_type == "BLANK")]

# blanks are zero- can remove them!
anatoxins_processed <- anatoxins_processed[-which(anatoxins_processed$sample_type == "BLANK"),]

## (b) chlorophyll-a data

# read in data
chlorophyll <- read.csv("./data/EDI_data_package/target_sample_chlorophyll.csv")

## (c) percent organic matter data

# read in data
per_org_matter <- read.csv("./data/EDI_data_package/target_sample_percent_organic_matter.csv") %>% 
  select(field_date, site_reach, sample_type, triplicate, percent_organic_matter)

# note: had two samples (6/29/2022 SFE-M-3 and SFE-M-4 that molded before we 
# had a dessicate container while we were waiting for oven use
# we will just use SFE-M-1S percent organic matter for these
per_org_matter$percent_organic_matter[2] <- per_org_matter$percent_organic_matter[1]
per_org_matter$percent_organic_matter[3] <- per_org_matter$percent_organic_matter[1]

#### (2) Processing Triplicates in data ####

## (a) anatoxin data

# calculate summary statistics for triplicates
triplicates_atx <- anatoxins_processed %>% 
  filter(triplicate == "y") %>% 
  dplyr::group_by(field_date, site_reach, sample_type) %>% 
  dplyr::summarize(mean_atxa = mean(ATXa_ug_g),
                   mean_htxa = mean(HTXa_ug_g),
                   mean_dhatxa = mean(dhATXa_ug_g),
                   # not calculating for dhatxa since there were no detections
                   mean_atx_all = mean(ATX_all_ug_g),
                   sd_atx_all = sd(ATX_all_ug_g), # only will calculate for sum
                   rsd_atx_all = sd_atx_all * 100 / mean_atx_all) %>% 
  ungroup() %>% 
  distinct()

# look at triplicate results
view(triplicates_atx) # two highest triplicates were HIGH
# 173% RSD- SAL-1S TM and SFE-M-2 TAC; very high but sort of expected?
# values for these are close to detection limits (i.e. very low)
triplicates_atx$rsd_atx_all <- replace(triplicates_atx$rsd_atx_all, is.nan(triplicates_atx$rsd_atx_all), NA)
mean(triplicates_atx$rsd_atx_all, na.rm = TRUE) # average is 36.33%
median(triplicates_atx$rsd_atx_all, na.rm = TRUE) # median is 20.46%

# take dataset and select for columns we care about before merging
triplicates_atx_final <- triplicates_atx %>% 
  dplyr::mutate(ATXa_ug_g = round(mean_atxa, 4),
                HTXa_ug_g = round(mean_htxa, 4),
                dhATXa_ug_g = round(mean_dhatxa, 4)) %>% 
  mutate(ATX_all_ug_g = ATXa_ug_g + HTXa_ug_g + dhATXa_ug_g) %>%  # adding again to make sure it adds up w/ rounding
  dplyr::select(field_date, site_reach, sample_type, ATXa_ug_g, HTXa_ug_g, dhATXa_ug_g, ATX_all_ug_g)

# remove triplicates from original dataset before joining the two
anatoxins_final <- anatoxins_processed %>% 
  filter(triplicate == "n") %>% 
  select(field_date, site_reach, sample_type, ATXa_ug_g, HTXa_ug_g, dhATXa_ug_g, ATX_all_ug_g)

# add processed/averaged triplicates back in
anatoxins_final <- rbind(anatoxins_final, triplicates_atx_final)

## (b) chlorophyll-a data

# filter out triplicates and calculate summary statistics
triplicates_chl <- chlorophyll %>% 
  filter(triplicate == "y") %>% 
  dplyr::group_by(field_date, site_reach, sample_type) %>% 
  dplyr::summarize(mean_chla = mean(Chla_ug_mg),
                   sd_chla = sd(Chla_ug_mg),
                   rsd_chla = sd_chla * 100 / mean_chla,
                   mean_pheo = mean(Pheo_ug_mg)) %>% 
  ungroup() %>% 
  distinct()

# look at triplicate results
view(triplicates_chl) # RSD max is 78% which is very high, the next is 33%
mean(triplicates_chl$rsd_chla) # average is 12.6%

# some thoughts on high RSD:
# the highest samples are all TM (those with 12.5% and up)
# this is likely due to the sediment amounts in the mats
# so when we take a small amount (~2 mg), which we need to do
# to get plausible values on the fluorometer (i.e. no negative pheophytin)
# it is highly possible that sometimes our sample is mostly sediment
# and sometimes more mat material is included

# take dataset and select for columns we care about before merging
triplicates_chl_final <- triplicates_chl %>% 
  dplyr::rename(Chla_ug_mg = mean_chla,
                Pheo_ug_mg = mean_pheo) %>% 
  select(field_date, site_reach, sample_type, Chla_ug_mg, Pheo_ug_mg)

# remove triplicates from original dataset before joining the two
chla_final <- chlorophyll %>% 
  filter(triplicate == "n") %>% 
  select(field_date, site_reach, sample_type, Chla_ug_mg, Pheo_ug_mg)

# add processed/averaged triplicates back in & do some conversions
chla_final <- rbind(chla_final, triplicates_chl_final) %>% 
  select(field_date, site_reach, sample_type, Chla_ug_mg, Pheo_ug_mg)

## (c) percent organic matter data

# calculating summary statistics for triplicates
triplicates_org_matter <- per_org_matter %>% 
  filter(triplicate == "y") %>% 
  dplyr::group_by(field_date, site_reach, sample_type) %>% 
  dplyr::summarize(mean = mean(percent_organic_matter),
                   sd = sd(percent_organic_matter),
                   rsd = sd * 100 / mean) %>% 
  ungroup() %>% 
  distinct()

# look at triplicate results
view(triplicates_org_matter) # RSD max is 16%; only 5 > 10%
mean(triplicates_org_matter$rsd) # average is 5.6%

# take dataset and select for columns we care about before merging
triplicates_org_matter_final <- triplicates_org_matter %>% 
  dplyr::rename(percent_organic_matter = mean) %>% 
  mutate(percent_organic_matter = round(percent_organic_matter, 3)) %>% # round to nearest 2 decimals
  # (briefly looking at the data that is roughly our sig figs...)
  select(field_date, site_reach, sample_type, percent_organic_matter)

# remove triplicates from original dataset before joining the two
org_matter_final <- per_org_matter %>% 
  filter(triplicate == "n") %>% 
  mutate(percent_organic_matter = round(percent_organic_matter, 2)) %>% # also round to the nearest 2 decimals
  select(field_date, site_reach, sample_type, percent_organic_matter)

# add processed/averaged triplicates back in
org_matter_final <- rbind(org_matter_final, triplicates_org_matter_final)

# checking length of data frame (aka no sample left behind!)
# final dataset should be length of original dataset minus 2/3 the length of the averaged triplicate dataset
eval(nrow(org_matter_final) == (nrow(per_org_matter)-(2*nrow(triplicates_org_matter)))) # yay!

#### (3) Combining datasets and making calculations ####

# merge in chlorophyll and percent organic matter data
anatoxins_final <- left_join(anatoxins_final, chla_final, by = c("site_reach", "field_date", "sample_type"))

# note that SFE-M-3 7.14.22 TM has no chl-a value -- not enough material leftover to calculate chla
# we will just fill in this value with the TM chl-a downstream that day (but it doesn't matter because ATX is 0)
anatoxins_final$Chla_ug_mg[1] <- anatoxins_final$Chla_ug_mg[2]

# merge in percent organic matter data
anatoxins_final <- left_join(anatoxins_final, org_matter_final, by = c("site_reach", "field_date", "sample_type"))

# calculate anatoxins ug / (approximate) g chlorophyll-a AND anatoxins ug / (approximate) g organic matter
anatoxins_final <- anatoxins_final %>% 
  dplyr::mutate(Chla_ug_g = Chla_ug_mg * 1000,
                Pheo_ug_g = Pheo_ug_mg * 1000,
                ATX_all_ug_chla_ug = round((ATX_all_ug_g / (Chla_ug_g)), 7),
                ATX_all_ug_orgmat_g = round((ATX_all_ug_g * (percent_organic_matter/100)), 7))

# create final csv for time series data
anatoxins_final_timeseries <- anatoxins_final %>% 
  filter(sample_type != "riffle_exp") %>% # exclude riffle experiment
  select(field_date, site_reach, sample_type, ATXa_ug_g, dhATXa_ug_g, HTXa_ug_g, ATX_all_ug_g, 
         Chla_ug_g, Pheo_ug_g, percent_organic_matter, ATX_all_ug_chla_ug, ATX_all_ug_orgmat_g) %>% 
  arrange(field_date) # arrange in order of date

# create final csv for riffle experiment
anatoxins_final_riffle <- anatoxins_final %>% 
  filter(sample_type == "riffle_exp") %>% # is only the riffle experiment
  select(field_date, site_reach, sample_type, ATXa_ug_g, dhATXa_ug_g, HTXa_ug_g, ATX_all_ug_g, 
         Chla_ug_g, Pheo_ug_g, percent_organic_matter, ATX_all_ug_chla_ug, ATX_all_ug_orgmat_g) %>% 
  arrange(field_date) # arrange in order of date

# save new csvs
write.csv(anatoxins_final_timeseries, "./data/field_and_lab/cyano_atx.csv", row.names = FALSE)
write.csv(anatoxins_final_riffle, "./data/field_and_lab/riffle_exp_atx.csv", row.names = FALSE)
