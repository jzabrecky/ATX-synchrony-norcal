#### putting together field and lab data to answer research questions
### Jordan Zabrecky
## last edited: 07.24.2025

# This script aggregates (1) 2022 benthic cyanobacteria and GPP for all rivers
# to answer Q1, (2) 2023 south fork eel benthic cyanobacteria, GPP, and water
# quality parameters to answer Q2 and Q3

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "zoo"), 
       require, character.only = T)

# read in data
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")
water <- read.csv("./data/field_and_lab/water_chemistry.csv")
survey <- read.csv("./data/field_and_lab/percover_byreach.csv")
metab <- ldply(list.files(path = "./data/metab_model_outputs_processed/", pattern = "_metab.csv"), function(filename) {
  d <- read.csv(paste("data/metab_model_outputs_processed/", filename, sep = ""))
  d$site = d$site_year %>% str_sub(end=-6)
  d$field_date = ymd(d$date)
  d$year = year(d$field_date)
  return(d)
})

# adjust date of anatoxin sample (9/08/2022) that was incorrectly taken two 
# days before while I was gone (on 9/06/2022)
anatoxins$field_date[which(anatoxins$field_date == "2022-09-08")] <- "2022-09-06"

#### (2) Pivot anatoxins longer and join in with other data ####

# separate out TM and TAC
microcoleus <- anatoxins %>% 
  filter(sample_type == "TM")
ana_cyl <- anatoxins %>% 
  filter(sample_type == "TAC")

# rename columns of each to clarify TM or TAC
rename_cols <- function(df, sample_type) {
  old_names <- colnames(df)[4:ncol(df)] # get column names we are changing
  new_names <- rep("", (length(old_names) - 3))# initialize new list of column names
  
  # create new name with sample_type as prefix
  for(i in 1:length(old_names)) {
    new_names[i] <- paste(sample_type, "_", old_names[i], sep = "")
  }
  
  # assign to dataframe and return new dataframe
  colnames(df)[4:ncol(df)] <- new_names
  return(df %>% select(!sample_type))
}

# apply to our microcoleus and ana_cyl dataframes
microcoleus <- rename_cols(microcoleus, "TM")
ana_cyl <- rename_cols(ana_cyl, "TAC")

# joining dataframes
all <- left_join(survey, water, by = c("field_date", "site_reach"))
all <- (list(all, microcoleus, ana_cyl)) %>% 
  join_all(by = c("field_date", "site_reach"), type = "left") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date))

#### (3) Put together 2022 data for summary statistics

# filter out into 2022 data
allrivers22 <- all %>% 
  filter(year == 2022)

# save dataframe
write.csv(allrivers22, "./data/field_and_lab/allrivers22_combined.csv",
          row.names = FALSE)

#### (3) Filter 2023 data and calculate median GPP for each observation ####

# filter out south fork eel 2023 
sfkeel23 <- all %>% 
  filter(site == "SFE-M" | site == "SFE-SH") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date)) %>% 
  filter(year == 2023) %>% 
  select(field_date, site_reach, site, green_algae, microcoleus, anabaena_cylindrospermum,
         bare_biofilm, other_nfixers, proportion_micro_transects, proportion_ana_cyl_transects,
         proportion_riffle_rapid_transects, average_depth_cm_sampled, median_depth_cm_sampled, 
         pH, temp_C, DO_mg_L, cond_uS_cm, oPhos_ug_P_L, nitrate_mg_N_L, ammonium_mg_N_L,
         TDC_mg_L, DOC_mg_L, TM_ATX_all_ug_g, TM_ATX_all_ug_orgmat_g, TM_ATX_all_ug_chla_ug,
         TM_Chla_ug_g, TM_Pheo_ug_g, TM_percent_organic_matter, TAC_ATX_all_ug_g,
         TAC_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_chla_ug, TAC_Chla_ug_g, TAC_Pheo_ug_g,
         TAC_percent_organic_matter)

## (a) getting median of GPP at field date to up to four days prior to match field dates

# filtering out metabolism data for SFE-M and SH 2023
metab_mir <- metab %>% 
  filter(site == "sfkeel_mir" & year == 2023)
metab_sth <- metab %>% 
  filter(site == "sfkeel_sth" & year == 2023)

# need to interpolate missing dates for SFE-SH
# relevant here because model covariate is median GPP of x days prior
dates <- data.frame(seq(ymd('2023-06-25'), ymd('2023-09-26'), by = 'day'))
colnames(dates) <- c("field_date")

# left join (only need to for sth 2023)
metab_sth <- left_join(dates, metab_sth, by = "field_date")

# interpolate missing GPP mean for SFE-SH
metab_sth$GPP.mean <- na.approx(metab_sth$GPP.mean)

# get missing USGS discharge data for SFE-SH
missing_dis <- dataRetrieval::readNWISdv("11475800", "00060", "2023-06-26", "2023-07-30") %>% 
  dplyr::rename(field_date = Date) %>% 
  mutate(discharge_m3_s = X_00060_00003 / 35.31) %>% 
  select(field_date, discharge_m3_s)

# join in missing discharge data
metab_sth <- left_join(metab_sth, missing_dis, by = c("field_date")) %>% 
  mutate(discharge_m3_s = case_when(is.na(discharge_m3_s.x) ~ discharge_m3_s.y,
                                    TRUE ~ discharge_m3_s.x))

# get dates of field visits
visits_mir <- sfkeel23 %>% filter(site == "SFE-M") %>% select(field_date) %>% unique()
visits_sth <- sfkeel23 %>% filter(site == "SFE-SH") %>% select(field_date) %>% unique()

# add column to visit dataframe to indicate visit number
visits_mir$visit <- seq(1, nrow(visits_mir))
visits_sth$visit <- seq(1, nrow(visits_sth))

# merge onto metabolism dataframes
metab_mir <- left_join(metab_mir, visits_mir, by = "field_date")
metab_sth <- left_join(metab_sth, visits_sth, by = "field_date")

# need to get indices of dataframe that have field visits
index_mir <- which(!is.na(metab_mir$visit))[-1] # cannot get more than a day before
index_sth <- which(!is.na(metab_sth$visit))[-1] # of GPP from first field visit

# use map in purr to get the four days of GPP before each visit and GPP of day of visit
map_mir <- index_mir %>% 
  purrr::map(. , function(x) metab_mir$GPP.mean[(x-4):(x)])
map_sth <- index_sth %>% 
  purrr::map(. , function(x) metab_sth$GPP.mean[(x-4):(x)])

# take median of four days and put in dataframe with index
med_mir <- data.frame(unlist(lapply(map_mir, function(x) median(x))))
med_sth <- data.frame(unlist(lapply(map_sth, function(x) median(x))))

# change column name
colnames(med_mir) <- c("GPP_median_tofourdaysprior")
colnames(med_sth) <- c("GPP_median_tofourdaysprior")

# put in visit index to GPP average
med_mir$visit <- visits_mir$visit[-1]
med_sth$visit <- visits_sth$visit[-1]

## (b) joining into final csv and saving

# left merge onto metabolism dataframes & only keep columns that we care about
metab_mir <- left_join(metab_mir, med_mir, by = "visit") %>% 
  select(field_date, GPP_median_tofourdaysprior, discharge_m3_s) %>% 
  mutate(site = "SFE-M")
metab_sth <- left_join(metab_sth, med_sth, by = "visit") %>% 
  select(field_date, GPP_median_tofourdaysprior, discharge_m3_s) %>% 
  mutate(site = "SFE-SH")

# keep only columns we care about and merge into south fork eel field dataframes
# left join by field date and site
sfkeel23 <- left_join(sfkeel23, rbind(metab_mir, metab_sth), by = c("field_date", "site"))

# fill in first day of GPP with observation on that day (exception case here)
sfkeel23$GPP_median_tofourdaysprior[which(is.na(sfkeel23$GPP_median_tofourdaysprior) 
               & (sfkeel23$site == "SFE-M"))] <- 
  metab$GPP.mean[which(metab$site_year == "sfkeel_mir_2023" & metab$field_date == "2023-06-20")]
sfkeel23$GPP_median_tofourdaysprior[which(is.na(sfkeel23$GPP_median_tofourdaysprior) 
                                          & (sfkeel23$site == "SFE-SH"))] <- 
  metab$GPP.mean[which(metab$site_year == "sfkeel_sth_2023" & metab$field_date == "2023-06-25")]


# save csv
write.csv(sfkeel23, "./data/field_and_lab/sfkeel23_combined.csv", row.names = FALSE)
