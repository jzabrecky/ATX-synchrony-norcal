#### EFI abstract code

## initial passes at predictive modeling
library(tidyverse)
library(zoo)

# read in data

# for WHATEVER reason- there is some duplicates of 9.12.24 that need to be looked into
sfkeel23 <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  mutate(field_date = ymd(field_date))
metab_sfkeelmir_23 <- read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2023_metab.csv") %>% 
  rename(field_date = date) %>% 
  mutate(field_date = ymd(field_date))
metab_sfkeelsth_23 <- read.csv("./data/metab_model_outputs_processed/sfkeel_sth_2023_metab.csv") %>% 
  rename(field_date = date) %>% 
  mutate(field_date = ymd(field_date))

## interpolate any missing 
# get sequence of datesd=
dates <- data.frame(seq(ymd('2023-06-25'), ymd('2023-09-26'), by = 'day'))
colnames(dates) <- c("field_date")

# left join
metab_sfkeelmir_23 <- left_join(dates, metab_sfkeelmir_23, by = "field_date")
metab_sfkeelsth_23 <- left_join(dates, metab_sfkeelsth_23, by = "field_date")

# looks like we have all dates for south fork eel 2023 miranda but not standish
metab_sfkeelsth_23$GPP.mean <- na.approx(metab_sfkeelsth_23$GPP.mean)

# get field dates and merge onto metabolism df
visits_mir <- sfkeel23 %>% 
  dplyr::filter(site == "SFE-M") %>% 
  select(field_date)

visits_mir <- data.frame(unique(visits_mir$field_date))
colnames(visits_mir) <- c("field_date")

visits_mir$visit <- seq(1, nrow(visits_mir))

visits_sth <- sfkeel23 %>% 
  dplyr::filter(site == "SFE-SH") %>% 
  select(field_date)

visits_sth <- data.frame(unique(visits_sth$field_date))
colnames(visits_sth) <- c("field_date")

visits_sth$visit <- seq(1, nrow(visits_sth))

## get average and mean GPP five days prior to visit (including day of?)

# left merge back onto metab
metab_sfkeelmir_23 <- left_join(metab_sfkeelmir_23, visits_mir, by = "field_date")
metab_sfkeelsth_23 <- left_join(metab_sfkeelsth_23, visits_sth, by = "field_date")

# trial run!

# not going to get prior metab for first date
mod_metab_sfkeelmir_23 <- metab_sfkeelmir_23[-1,]
index <- which(!is.na(mod_metab_sfkeelmir_23$visit))

# doing 4 days beforehand
mir <- index %>% 
  purrr::map(. , function(x) mod_metab_sfkeelmir_23$GPP.mean[(x-5):(x-1)])

names(mir) <- seq(2:length(mir))
mir_df <- data.frame(mir)

