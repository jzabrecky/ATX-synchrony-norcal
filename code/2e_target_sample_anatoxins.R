#### processing anatoxin concentrations from benthic mats
### Jordan Zabrecky
## last edited 12.06.2024

# This script takes in anatoxin concentrations, chlorophyll-a concentrations, 
# and percent organic matter from targeted samples (TM- target Microcoleus and
# TAC- target Anabaena and/or Cylindrospermum) and processes them by calculating
# amount of anatoxin per chlorophyll-a and percent organic matter

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

## (a) anatoxin data

## (b) chlorophyll-a data

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

# process triplicates and blanks

## (b) chlorophyll-a data

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