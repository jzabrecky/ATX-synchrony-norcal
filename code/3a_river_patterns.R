#### Whole river patterns of taxa-specific benthic cyanobacteria dynamics and GPP
### Jordan Zabrecky
## last edited: 05.13.2025

## This script calculates the mean taxa-specific cover and anatoxin concentrations
## across reaches within a river and includes the estimated GPP for the reaches
## to create a summary table and saves the data used to make that table

## MAYBE STANDARDIZE BUT JUST IN CASE OF RIVER!?!?

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "cowplot"), 
       require, character.only = T)

# loading in metabolism data and selecting only 2022 data
metabolism <- ldply(list.files(path = "./data/metab_model_outputs_processed/", 
                               pattern = "_metab.csv"), function(filename) {
                                 d <- read.csv(paste("./data/metab_model_outputs_processed/", filename, sep = ""))
                                 d$site_year = filename %>% stringr::str_remove("_metab.csv") # get site & year info
                                 d$site = d$site_year %>% str_sub(end=-6) # get site only info
                                 d$date = ymd(d$date) # convert date to date object
                                 return(d)
                               }) %>% 
  filter(site_year == "sfkeel_mir_2022" | site_year == "russian_2022" | site_year == "salmon_2022")

# split into a list based on site & year (MAYBE GET RID OF THIS)
metabolism_list <- split(metabolism, metabolism$site_year) 

# loading in cover data and selecting only 2022 data
cover <- read.csv("./data/field_and_lab/percover_bysite.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date),
         site_year = case_when(site == "RUS" ~ "russian_2022", # create site_year column
                              (site == "SFE-M_excl_site2" & year == 2022) ~ "sfkeel_mir_2022",
                              (site == "SAL" & year == 2022) ~ "salmon_2022",
                              TRUE ~ "remove")) %>% 
  filter(site_year != "remove") %>% 
  select(site_year, field_date, microcoleus_mean, microcoleus_sd, anabaena_cylindrospermum_mean,
         anabaena_cylindrospermum_sd)

# loading in anatoxins data and selecting only 2022 data
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date),
         site = case_when(grepl("RUS", site_reach) ~ "RUS", # create site codes
                          grepl("SAL", site_reach) ~ "SAL",
                          grepl("SFE-M", site_reach) ~ "SFE-M",
                          grepl("SFE-SH", site_reach) ~ "SFE-SH"),
         site_year = case_when(site == "RUS" ~ "russian_2022", # create site_year column
                               (site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               TRUE ~ "remove")) %>% 
  filter(site_year != "remove")

# treating 9-8-22 sample as if its 9-6-22
anatoxins$field_date[which(anatoxins$field_date == "2022-09-08")] <- ymd("2022-09-06")

# summarize anatoxins by site and pivot wider
anatoxins_final <- anatoxins %>% 
  dplyr::rename(group = sample_type) %>% 
  dplyr::group_by(site_year, field_date, group) %>% 
  dplyr::reframe(mean_ATX_all_ug_orgmat_g = mean(ATX_all_ug_orgmat_g),
                 sd_ATX_all_ug_orgmat_g = sd(ATX_all_ug_orgmat_g)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "group", 
              values_from = c("mean_ATX_all_ug_orgmat_g", "sd_ATX_all_ug_orgmat_g"),
              names_glue = "{group}_{.value}")

# combine cover and anatoxins data
bc_dynamics <- left_join(cover, anatoxins_final, by = c("site_year", "field_date"))

# fill in zero for times w/o anatoxin (due to ND or )

#### (2) Create river summary table ####


#### (3) save csv
# both data and summary figure?
# create figure in another script?
