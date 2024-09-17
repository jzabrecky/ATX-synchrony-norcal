#### Figure X. Large synchrony figure of metabolism, accrual, and anatoxins
### Jordan Zabrecky
## last edited 09.17.2024

# This figure shows the metabolism modelled for all sites
# and the <AVERAGED OR MAX> percent cover and anatoxin concentrations blah blah

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

# loading in metabolism data as one data-frame for now
metabolism <- ldply(list.files(path = "./data/prelim_metab_estimates/", pattern = "daily_est.csv"), function(filename) {
  d <- read.csv(paste("./data/prelim_metab_estimates/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_daily_est.csv")
  d$site = d$site_year %>% str_sub(end=-6)
  return(d)
})

# using discharge already downloaded in our model input csv
discharge <- ldply(list.files(path = "./data/metab_model_inputs/", pattern = "_modelinputs.csv"), function(filename) {
  d <- read.csv(paste("./data/metab_model_inputs/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_modelinputs.csv")
  return(d)
})

# loading in benthic cyanobacteria data
accrual <- read.csv("./data/field_and_lab/percover_bysite.csv")
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")

#### (2) Processing data to make figures

# cutting down metabolism data (this code may get moved to modeling latent biomass code)
metabolism <- metabolism %>% 
  select(site_year, date, GPP_mean, GPP_sd, GPP_2.5pct, GPP_97.5pct, ER_mean, ER_sd, ER_2.5pct, ER_97.5pct) %>% 
  mutate(NEP_mean = GPP_mean + ER_mean, # adding because ER is negative
         NEP_2.5pct = GPP_2.5pct + ER_2.5pct,
         NEP_97.5pct = GPP_97.5pct + ER_97.5pct)
# probably have to convert date to date time in some way
# also have times with positive ER for the Salmon which need to get filtered out for figure

# splitting data frame into a list
metabolism <- split(metabolism, metabolism$site_year)

# cutting down discharge data
discharge <- discharge %>%
  select(site_year, date_time, discharge_m3_s)

# splitting data frame into a list
discharge <- split(discharge, discharge$site_year)

# process anatoxin data (giving myself a couple of options to start with)
# average vers.
atx_summarized <- anatoxins %>% 
  dplyr::group_by(field_date, site, sample_type) %>% 
  dplyr::summarize(mean_ATX_all_ug_chla_g = mean(ATX_all_ug_chla_g),
            mean_ATX_all_ug_afdm_g = mean(ATX_all_ug_afdm_g),
            max_ATX_all_ug_chla_g = max(ATX_all_ug_chla_g),
            max_ATX_all_ug_afdm_g = max(ATX_all_ug_afdm_g))

# metabolism figure w/ discharge

# NEP figure w/ discharge

# accrual & atx

## How do I want to facet wrap this???