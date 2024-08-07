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
percover22_raw <- read.csv("./data/field_and_lab/raw_data/percover_2022_raw.csv")
percover23_raw <- read.csv("./data/field_and_lab/raw_data/percover_2023_raw.csv")

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

#### (3) Calculating percent cover for each reach on each sampling day ####

# creating new dataframe for manipulations
percover22 <- percover22_raw %>% 
  mutate(field_date = mdy(field_date))
percover23 <- percover23_raw %>% 
  mutate(field_date = mdy(field_date))

## function to calculate % cover averages for each reach on each sampling day
average_per_reach <- function(data) { # data is the full % cover from sampling year
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

# applying function to each year of data
percover22_reach <- average_per_reach(percover22)
percover23_reach <- average_per_reach(percover23)

# saving new csv's
write.csv(percover22_reach, "data/field_and_lab/percover_2022_byreach.csv", row.names = FALSE)
write.csv(percover23_reach, "data/field_and_lab/percover_2023_byreach.csv", row.names = FALSE)

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
percover22$field_date[144:154] <- mdy("07/06/2022")

# applying function to each year of data
percover22_site <- average_per_site(percover22)
percover23_site <- average_per_site(percover23)

## NOTE: when analyzing data that SFE-M for 2023 includes site 2 and SFE-M for 2022 does not!

# saving new csvs
write.csv(percover22_site, "data/field_and_lab/percover_2022_bysite.csv", row.names = FALSE)
write.csv(percover23_site, "data/field_and_lab/percover_2023_bysite.csv", row.names = FALSE)