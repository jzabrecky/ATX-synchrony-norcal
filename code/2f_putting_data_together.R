#### putting all field and lab data together for south fork eel 2023
### Jordan Zabrecky
## last edited: 01.11.2025

# This script aggregates all field and lab information for reaches
# on the South Fork Eel in 2023

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval"), require, character.only = T)

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

#### (2) Processing, joining data-frames and focusing on SFE 2023 ####

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
all <- left_join(survey, water, by = c("field_date", "site_reach", "site", "reach"))
all <- (list(all, microcoleus, ana_cyl)) %>% 
  join_all(by = c("field_date", "site_reach"), type = "left")

# filter out south fork eel 2023 
sfkeel23 <- all %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date)) %>% 
  filter(site == "SFE-M" | site == "SFE-SH") %>% 
  filter(year == 2023) %>% 
  select(field_date, site_reach, site, green_algae, microcoleus, anabaena_cylindrospermum,
         bare_biofilm, other_nfixers, proportion_micro_transects, proportion_ana_cyl_transects,
         proportion_riffle_rapid_transects, average_depth_cm_sampled, median_depth_cm_sampled, 
         pH, temp_C, DO_mg_L, cond_uS_cm, oPhos_ug_P_L, nitrate_mg_N_L, ammonium_mg_N_L,
         TDC_mg_L, DOC_mg_L, TM_ATX_all_ug_g, TM_ATX_all_ug_orgmat_g, TM_ATX_all_ug_chla_ug,
         TM_Chla_ug_g, TM_Pheo_ug_g, TM_percent_organic_matter, TAC_ATX_all_ug_g,
         TAC_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_chla_ug, TAC_Chla_ug_g, TAC_Pheo_ug_g,
         TAC_percent_organic_matter)

# may fill out all NA's (no ATX sample taken) with 0's but may rethink in future
# sfkeel23 <- sfkeel23 %>% replace(is.na(.), 0)

#### (3) Joining in metabolism data ####

# filtering out metabolism data for SFE-M and SH 2023
metab_mir <- metab %>% 
  filter(site == "sfkeel_mir" & year == 2023)
metab_sth <- metab %>% 
  filter(site == "sfkeel_sth" & year == 2023)

# need to interpolate missing dates for SFE-SH
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

# at most can do average of four days before field visit
# will just do that for now- but can play around with interval

# need to get indices of dataframe that have field visits
index_mir <- which(!is.na(metab_mir$visit))[-1] # cannot get more than a day before
index_sth <- which(!is.na(metab_sth$visit))[-1] # of GPP from first field visit

# use map in purr to get the four days of GPP before each visit
map_mir <- index_mir %>% 
  purrr::map(. , function(x) metab_mir$GPP.mean[(x-4):(x-1)])
map_sth <- index_sth %>% 
  purrr::map(. , function(x) metab_sth$GPP.mean[(x-4):(x-1)])

# take average of four days and put in dataframe with index
avg_mir <- data.frame(unlist(lapply(map_mir, function(x) mean(x))))
avg_sth <- data.frame(unlist(lapply(map_sth, function(x) mean(x))))

# change column name and add in visit index
colnames(avg_mir) <- c("GPP_mean_fourdaysprior")
colnames(avg_sth) <- c("GPP_mean_fourdaysprior")

# put in visit index to GPP average
avg_mir$visit <- visits_mir$visit[-1]
avg_sth$visit <- visits_sth$visit[-1]

# calculate change in GPP average
avg_mir$GPP_change[2:nrow(avg_mir)] <- avg_mir$GPP_mean_fourdaysprior[2:nrow(avg_mir)] -
  avg_mir$GPP_mean_fourdaysprior[1:(nrow(avg_mir)-1)]
avg_sth$GPP_change[2:nrow(avg_sth)] <- avg_sth$GPP_mean_fourdaysprior[2:nrow(avg_sth)] -
  avg_sth$GPP_mean_fourdaysprior[1:(nrow(avg_sth)-1)]

# left merge onto metabolism dataframes
metab_mir <- left_join(metab_mir, avg_mir, by = "visit")
metab_sth <- left_join(metab_sth, avg_sth, by = "visit")

# only keep columns from metab that we care about
metab_mir <- metab_mir %>% 
  select(field_date, GPP_mean_fourdaysprior, discharge_m3_s) %>% 
  mutate(site = "SFE-M")
metab_sth <- metab_sth %>% 
  select(field_date, GPP_mean_fourdaysprior, discharge_m3_s) %>% 
  mutate(site = "SFE-SH")

# keep only columns we care about and merge into south fork eel field dataframes
# left join by field date and site
sfkeel23 <- left_join(sfkeel23, rbind(metab_mir, metab_sth), by = c("field_date", "site"))

# save csv
write.csv(sfkeel23, "./data/field_and_lab/sfkeel23_combined.csv", row.names = FALSE)
