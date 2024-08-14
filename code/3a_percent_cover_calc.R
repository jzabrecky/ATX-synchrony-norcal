#### calculating percent cover from reach surveys for each reach & site
### Jordan Zabrecky
## last edited: 08.06.2024

# This code calculates averages using % cover data for each study reach
# and additionally each river site (designated by nearby sensors & USGS gage)
# It also tallies the number and calculates the percent of transects 
# where Anabaena or Microcoleus was present

#### (1) Loading libraries and reach data ####

# loading libraries
lapply(c("tidyverse", "lubridate"), require, character.only = T)

# loading raw % cover data
percover22_raw <- read.csv("./data/field_and_lab/raw_data/percover_2022.csv")
percover23_raw <- read.csv("./data/field_and_lab/raw_data/percover_2023.csv")

#### (2) Checking field data ####
# externally checked for issues, but double-checking!

# making temporary data frames with a total column
percover22_test <- percover22_raw %>% 
  mutate(total = green_algae + Microcoleus + Anabaena + bare_biofilm + other_N_fixers)

percover23_test <- percover23_raw %>% 
  mutate(total = green_algae + Microcoleus + Anabaena + bare_biofilm + other_N_fixers)

# looking to see if any != 100%
which(percover22_test$total != 100)
which(percover23_test$total != 100)
# all 100- we are all good :)

# remove the temporary dataframes
rm(percover22_test, percover23_test)

# merging them
percover_raw <- rbind(percover22_raw, percover23_raw)

#### (3) Calculating percent cover for each reach on each sampling day ####

# creating new dataframe for manipulations
percover <- percover_raw %>% 
  mutate(field_date = mdy(field_date))

## function to calculate % cover averages for each reach on each sampling day
average_per_reach <- function(data) {
  data %>% 
    group_by(site, field_date, reach) %>% 
    mutate(
      green_algae = mean(green_algae),
      microcoleus = mean(Microcoleus),
      anabaena = mean(Anabaena),
      bare_biofilm = mean(bare_biofilm),
      other_nfixers = mean(other_N_fixers),
      micro_transects = sum(Micro_pres),
      ana_transects = sum(Ana_pres),
      total_transects = length(transect),
      percent_micro_transects = micro_transects / total_transects,
      percent_ana_transects = ana_transects / total_transects
    ) %>% 
    ungroup() %>% 
    select(field_date, site_reach, site, reach, green_algae, microcoleus,
           anabaena, bare_biofilm, other_nfixers, micro_transects, ana_transects, 
           total_transects, percent_micro_transects, percent_ana_transects) %>% 
    distinct() %>% 
    na.omit()
}

# applying function to data
percover_reach <- average_per_reach(percover)

# saving new csv
write.csv(percover_reach, "data/field_and_lab/percover_byreach.csv", row.names = FALSE)

#### (4) Calculating % cover averages for each site* on each sampling day ####
## * site = sensor grouping / nearest USGS discharge station

## function to calculate % cover averages for each site on each sampling day
average_per_site <- function(data) {
  data %>%
    group_by(site, field_date) %>% 
    mutate(
      green_algae = mean(green_algae),
      microcoleus = mean(Microcoleus),
      anabaena = mean(Anabaena),
      bare_biofilm = mean(bare_biofilm),
      other_nfixers = mean(other_N_fixers),
      micro_transects = sum(Micro_pres),
      ana_transects = sum(Ana_pres),
      total_transects = length(transect),
      percent_micro_transects = micro_transects / total_transects,
      percent_ana_transects = ana_transects / total_transects
    ) %>% 
    ungroup() %>% 
    select(field_date, site, green_algae, microcoleus,
           anabaena, bare_biofilm, other_nfixers, micro_transects, ana_transects, 
           total_transects, percent_micro_transects, percent_ana_transects) %>% 
    distinct() %>% 
    na.omit()
}

# Want to manually merge 7.6.2022 RUS-4S & RUS-3UP and 7.7.2022 RUS-2UP to be on the same day 
# as they are only a day apart and were ideally done on the same day
# things do not always go according to plan!
percover$field_date[144:154] <- mdy("07/06/2022")

# applying function to data
percover_site <- average_per_site(percover)

# sampling at SFE-M in 2023 included site 2 whereas SFE-M in 2022 did not
# so want to make a disclaimer
percover_site$site[12:18] <- "SFE-M_excl_site2"
percover_site$site[32:46] <- "SFE-M_all_sites"

# to be able to compare 2022 and 2023, let's recalculate for just those original sites in 2023
SFE_M_exc_site2 <- percover %>% 
  filter(site == "SFE-M",
         reach != "2") %>% 
  filter(field_date >= "2023-01-01 00:00:00")

# applying function to just this data
SFE_M_exc_site2_calc <- average_per_site(SFE_M_exc_site2)

# adding note/modified site
SFE_M_exc_site2_calc$site <- "SFE-M_excl_site2"

# binding above dataframe to full dataframe
percover_site_final <- rbind(percover_site, SFE_M_exc_site2_calc)

# saving new csv
write.csv(percover_site_final, "data/field_and_lab/percover_bysite.csv", row.names = FALSE)