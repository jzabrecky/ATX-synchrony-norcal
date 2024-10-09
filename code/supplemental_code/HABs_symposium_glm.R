#### script for glms for HABs symposium poster
### Jordan Zabrecky
## last edited: 10.07.2024

# This script makes a generalized linear model (glm) for each site
# and plots the effect size for each variable for poster for 12th
# U.S. Symposium on Harmful Algae

#### Loading libraries and data ####

# loading libraries
lapply(c("lubridate", "plyr", "tidyverse"), require, character.only = T)

# loading metabolism data
metabolism <- ldply(list.files(path = "./data/prelim_metab_estimates/", pattern = "daily_est.csv"), function(filename) {
  d <- read.csv(paste("./data/prelim_metab_estimates/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_daily_est.csv")
  d$site = d$site_year %>% str_sub(end=-6)
  d$field_date = ymd(d$date)
  return(d)
})

# get light data from metabolism model input
light <- ldply(list.files(path = "./data/metab_model_inputs/", pattern = "modelinputs"), function(filename) {
  d <- read.csv(paste("data/metab_model_inputs/", filename, sep = ""))
  return(d)
})

##  use dataRetrieval to get daily discharge (rather than continuous from our miniDOT inputs!)
# USGS site numbers
USGS_gages <- c("11463000", "11522500", "11476500", "11475800")

# mean daily discharge in cfs
param <- "00060"

# use "DataRetrieval" to download data (rather than continuous on our miniDOTs!)
discharge <- lapply(USGS_gages, function(x) readNWISdv(x, param, "2022-06-15","2023-10-01"))

# adding names of each river to list
site_names <- c("russian", "salmon", "sfkeel_mir", "sfkeel_sth")
names(discharge) <- site_names

# loading smaller datasets
nutrients <- read.csv("./data/field_and_lab/water_chemistry.csv")
occurence <- read.csv("./data/field_and_lab/percover_bysite.csv")
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")

# convert dates from character to date object
light$date_time <- ymd_hms(light$date_time)
nutrients$field_date <- ymd(nutrients$field_date)
occurence$field_date <- ymd(occurence$field_date)
anatoxins$field_date <- ymd(anatoxins$field_date)

#### (2) Joining all into a single dataframe ####

# trim down metabolism data
metabolism <- metabolism %>% 
  dplyr::select(field_date, site_year, site, GPP_mean, GPP_sd, GPP_2.5pct, GPP_97.5pct, ER_mean, ER_sd,
         ER_2.5pct, ER_97.5pct, K600_daily_mean, K600_daily_sd, K600_daily_2.5pct, K600_daily_97.5pct)

# split into a list by site year
metabolism_list <- split(metabolism, metabolism$site_year)

# calculate daily light
light <- light %>% 
  mutate(date = date(date_time)) %>% 
  dplyr::group_by(site_year, date) %>% 
  dplyr::summarize(daily_SW_W_m_2 = mean(SW_W_m_2))
  
# function to clean discharge data
clean_discharge <- function(df) {
  new_df <- df %>% 
    rename(field_date = Date) %>% 
    mutate(discharge_cms = X_00060_00003 / 35.31) %>% 
    dplyr::select(field_date, discharge_cms)
  return(new_df)
}

# applying function to dataframe list
discharge <- lapply(discharge, function(x) clean_discharge(x))

# removing 2023 that exludes SFE-M-2 from occurence and making site names continuous
occurence <- occurence %>% 
  mutate(year = year(field_date)) %>%
  dplyr::filter(site != "SFE-M_excl_site2" | year != 2023) %>% # get rid of exclusion of site 2 in 2023
  mutate(site = case_when(site == "SFE-M_all_sites" ~ "SFE-M",
                          site == "RUS" ~ "RUS",
                          site == "SFE-M_excl_site2" ~ "SFE-M",
                          site == "SAL" ~ "SAL",
                          site == "SFE-SH" ~ "SFE-SH"))

# adding site_year info to occurence dataframe
occurence <- occurence %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

# split into a list by site year
occurence_list <- split(occurence, occurence$site_year)

# sample collected on 9/8/2022 was a retake for a sample on 9/6/2022
# as a substitute tech took a very watery sample of just the ends of the mats at SFE-M-1S
anatoxins$field_date[16] <- anatoxins$field_date[15]

# also adding site_year information to anatoxins dataframe
anatoxins <- anatoxins %>% 
  mutate(year = year(field_date)) %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

# calculate average and max per day at each site
atx_summarized <- anatoxins %>% 
  dplyr::rename(group = sample_type) %>% 
  dplyr::group_by(site_year, field_date, site, group) %>% 
  dplyr::summarize(mean_ATX_all_ug_chla_g = mean(ATX_all_ug_chla_g),
                   mean_ATX_all_ug_afdm_g = mean(ATX_all_ug_afdm_g),
                   max_ATX_all_ug_chla_g = max(ATX_all_ug_chla_g), # will probably not use max
                   max_ATX_all_ug_afdm_g = max(ATX_all_ug_afdm_g))

# need to pivot so TM and TAC are there own columns!
atx_summarized_pivot <- pivot_wider(atx_summarized, names_from = c(4), values_from = c(6)) %>% 
  rename(TAC_mean_ATX_all_ug_afdm_g = TAC,
         TM_mean_ATX_all_ug_afdm_g = TM) %>%
  dplyr::select(site, site_year, field_date, TAC_mean_ATX_all_ug_afdm_g, TM_mean_ATX_all_ug_afdm_g)

# split into a list by site year
anatoxins_list <- split(atx_summarized, atx_summarized$site_year)

# change date for Russian from 7/7/2022 to 7/6/2022 on nutrients data
nutrients$field_date[9] <- nutrients$field_date[8]

# need to average nutrients per day
nutrients_averaged <- nutrients %>% 
  dplyr::group_by(site, field_date) %>% 
  summarize(temp_C_mean = mean(temp_C),
            DO_mg_L_mean = mean(DO_mg_L),
            cond_uS_cm_mean = mean(cond_uS_cm),
            oPhos_ug_P_L_mean = mean(oPhos_ug_P_L),
            nitrate_mg_N_L_mean = mean(nitrate_mg_N_L),
            ammonium_mg_N_L_mean = mean(ammonium_mg_N_L),
            TDC_mg_L_mean = mean(TDC_mg_L),
            DOC_mg_L_mean = mean(DOC_mg_L))

# add site_year information to nutrients dataframe
nutrients_averaged <- nutrients_averaged %>% 
  mutate(year = year(field_date)) %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

# split into a list by site year
nutrients_list <- split(nutrients_averaged, nutrients_averaged$site_year)

# modify discharge so it has similar ordering as other lists
discharge[[5]] <- discharge[[3]] # put sfe-mir in position 5
discharge[[3]] <- discharge[[2]] # duplicate salmon in position 3
discharge[[6]] <- discharge [[4]] # put sfe-sth in position 6
discharge[[4]] <- discharge[[5]] # and finally sfe-mir also in posiition 4

# rename discharge list
names(discharge) <- names(metabolism_list)

# create empty vector for all sites
combined <- data.frame()

# combine all into a single dataframe per site
# can use indexing because all lists are in same site order
for(i in 1:length(metabolism_list)) {
  single_site <- (list(metabolism_list[[i]], discharge[[i]], nutrients_list[[i]], occurence_list[[i]],
                       anatoxins_list[[i]])) %>% 
    join_all(by = c("field_date"), type = "left")
  combined <- rbind(combined, single_site)
}

# split into a list for modeling purposes
combined_list <- split(combined, combined$site_year)

#### (3) Making the GLMs ####

#### (4) Effect Size Plots ####
