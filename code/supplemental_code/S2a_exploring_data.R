#### Determining peak dates of GPP & benthic cyanobacteria dynamics
### Jordan Zabrecky
## Last edited: 03.06.2025

## This code pulls the peak date of GPP, taxa-specific benthic cyanobacteria cover,
## and taxa-specific mat anatoxin concentrations for each reach each year

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "plyr", "lubridate"), require, character.only = T)

# loading data and add in year information
cover <- read.csv("./data/field_and_lab/percover_byreach.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date))
atx <- read.csv("./data/field_and_lab/cyano_atx.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date))
metab <- ldply(list.files(path = "./data/metab_model_outputs_processed/", pattern = "_metab.csv"), function(filename) {
  d <- read.csv(paste("data/metab_model_outputs_processed/", filename, sep = ""))
  d$site = d$site_year %>% str_sub(end=-6)
  d$field_date = ymd(d$date)
  d$year = year(d$field_date)
  return(d)
})

# need to separate anatoxin data out by sample type
# (cover already has Microcoleus and Anabaena/Cylindrospermum in separate columns)
atx_tm <- atx %>% 
  filter(sample_type == "TM")
atx_tac <- atx %>% 
  filter(sample_type == "TAC")

#### (3) Getting maximum dates for each reach-year combination ####

## (a) benthic cover

# create summary df for microcoleus with max values at each reach
summary_micro <- cover %>% 
  dplyr::group_by(site_reach, year) %>% 
  dplyr::summarize(microcoleus = max(microcoleus)) %>% 
  filter(microcoleus != 0.00)

# join in date
summary_micro <- left_join(summary_micro, cover, by = c("microcoleus", "site_reach", "year")) %>% 
  select(site_reach, year, field_date, microcoleus)


# create summary df for anabaena/cylindrospermum with max values at each reach
summary_anacyl <- cover %>% 
  dplyr::group_by(site_reach, year) %>% 
  dplyr::summarize(anabaena_cylindrospermum = max(anabaena_cylindrospermum)) %>% 
  filter(anabaena_cylindrospermum != 0.00)

# join in date
summary_anacyl <- left_join(summary_anacyl, cover, by = c("anabaena_cylindrospermum", "site_reach", "year")) %>% 
  select(site_reach, year, field_date, anabaena_cylindrospermum)

## (b) anatoxins

# create summary df for microcoleus atx concentrations with max values at each reach
summary_tm_atx <- atx_tm %>% 
  dplyr::group_by(site_reach, year) %>% 
  dplyr::summarize(ATX_all_ug_orgmat_g = max(ATX_all_ug_orgmat_g)) %>% 
  filter(ATX_all_ug_orgmat_g != 0.00)

# join in date
summary_tm_atx <- left_join(summary_tm_atx, atx, by = c("ATX_all_ug_orgmat_g", "site_reach", "year")) %>% 
  select(site_reach, year, field_date, ATX_all_ug_orgmat_g)

# create summary df for anabaena/cylindrospermum atx concentrations with max values at each reach
summary_tac_atx <- atx_tac %>% 
  dplyr::group_by(site_reach, year) %>% 
  dplyr::summarize(ATX_all_ug_orgmat_g = max(ATX_all_ug_orgmat_g)) %>% 
  filter(ATX_all_ug_orgmat_g != 0.00)

# join in date
summary_tac_atx <- left_join(summary_tac_atx, atx, by = c("ATX_all_ug_orgmat_g", "site_reach", "year")) %>% 
  select(site_reach, year, field_date, ATX_all_ug_orgmat_g)

## (c) GPP

# create summary df for anabaena/cylindrospermum atx concentrations with max values at each reach
metab_summary <- metab %>% 
  dplyr::group_by(site_year) %>% 
  dplyr::summarize(GPP.mean = max(GPP.mean))

# join in date
metab_summary <- left_join(metab_summary, metab, by = c("GPP.mean", "site_year")) %>% 
  select(site_year, date, GPP.mean)
