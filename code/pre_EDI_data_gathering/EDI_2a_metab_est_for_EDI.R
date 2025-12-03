#### Putting together metabolism estimates for the EDI package (ver. 3)
### Jordan Zabrecky
## last edited: 11.17.2025

## This script puts together all the metabolism estimates for the EDI
## data package

#### (1) Loading libraries & data ####

# loading packages
lapply(c("tidyverse", "plyr"), require, character.only = T)

# loading data
metab <- ldply(list.files(path = "./data/metab_model_outputs_processed", 
                          pattern = "metab"), function(filename) {
                            d <- read.csv(paste("data/metab_model_outputs_processed/", filename, sep = ""))
                            d$date <- ymd(d$date)
                            return(d)
                          })

# getting depth data


#### (2) Putting together data & saving ####

# add column specifying site only and DO source information
metab <- metab %>% 
  # separate out site and source
  mutate(site = case_when(grepl("russian", site_year) ~ "russian",
                          grepl("salmon", site_year) ~ "salmon",
                          grepl("sfkeel_mir", site_year) ~ "sfkeel_mir",
                          grepl("sfkeel_sth", site_year) ~ "sfkeel_sth"),
         DO_source = case_when(grepl("USGS", site_year) ~ "USGS",
                               grepl("karuk", site_year) ~ "Karuk",
                               TRUE ~ "this_project")) %>% 
  # move new columns right after the site_year column
  relocate(site, .after = "site_year") %>% 
  relocate(DO_source, .after = "site") %>% 
  # remove NEP (users can calculate this themselves if they wish) %>% 
  select(!c("NEP.mean", "NEP.2.5.pct", "NEP.97.5.pct")) %>% 
  # remove discharge
  select(!discharge_m3_s)
    
# save csv
write.csv(metab, "./data/EDI_data_package/metabolism_estimates.csv", row.names = FALSE)
