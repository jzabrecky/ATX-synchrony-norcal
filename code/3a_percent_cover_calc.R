#### calculating percent cover from reach surveys for each reach & site
### Jordan Zabrecky
## last edited: 08.06.2024

# This code calculates averages using % cover data for each study reach
# and additionally each river section (designated by nearby USGS gage).
# It also tallies the number and calculates the percent of transects 
# where Anabaena or Microcoleus was present

#### (1) Loading libraries and reach data ####

# loading libraries
lapply(c("tidyverse", "lubridate"), require, character.only = T)

# loading raw % cover data
percover22_raw <- read.csv("./data/field_data/raw_field_data/percover_2022_raw.csv")
percover23_raw <- read.csv("./data/field_data/raw_field_data/percover_2023_raw.csv")

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
