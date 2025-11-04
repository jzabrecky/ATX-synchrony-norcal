#### Code to look at microcystins & cylindrospermopsins results
### Jordan Zabrecky
## last edited: 11.04.2025

## This code looks at the results of microcystins and cylindrospermopsins
## analyses to see if there is anything interesting!

#### (1) Loading libraries & data ####

# Loading libraries
lapply(c("tidyverse"), require, character.only = T)

# loading data
EDI <- read.csv("./data/EDI_data_package/anatoxin_concentrations.csv") %>% 
  filter(sample_type != "BLANK" & sample_type != "NT")

#### (2) Microcystins ####

# subset for samples with microcystins detected
microcystins <- EDI %>% 
  filter(MCY_ug_g != "ND") %>% 
  select(sample_type, site_reach, field_date, MCY_ug_g)

# save as csv to make table more easily copy & paste-able to supplemental methods

# upper and lower detection limits
max(EDI$MCY_det_limit, na.rm = TRUE)
min(EDI$MCY_det_limit, na.rm = TRUE)

#### (3) Cylindrospermopsins ####

# already know these are all non-detects but what are the upper & lower det. limitss?
max(EDI$CYL_det_limit, na.rm = TRUE)
min(EDI$CYL_det_limit, na.rm = TRUE)
