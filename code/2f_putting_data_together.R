#### putting together field and lab data to answer research questions
### Jordan Zabrecky
## last edited: 06.16.2025

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
  join_all(by = c("field_date", "site_reach"), type = "left")

#### (3) Filtering out data for each question ####

# make some slight adjustments to full dataframe
all <- all %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date)) %>% 
  mutate(TM_Chla_Pheo_ratio = TM_Chla_ug_g / TM_Pheo_ug_g,
         TAC_Chla_Pheo_ratio = TAC_Chla_ug_g / TAC_Pheo_ug_g)

# filter out into 2022 data
allrivers22 <- all %>% 
  filter(year == 2022)

# filter out south fork eel 2023 
sfkeel23 <- all %>% 
  filter(site == "SFE-M" | site == "SFE-SH") %>% 
  filter(year == 2023) %>% 
  select(field_date, site_reach, site, green_algae, microcoleus, anabaena_cylindrospermum,
         bare_biofilm, other_nfixers, proportion_micro_transects, proportion_ana_cyl_transects,
         proportion_riffle_rapid_transects, average_depth_cm_sampled, median_depth_cm_sampled, 
         pH, temp_C, DO_mg_L, cond_uS_cm, oPhos_ug_P_L, nitrate_mg_N_L, ammonium_mg_N_L,
         TDC_mg_L, DOC_mg_L, TM_ATX_all_ug_g, TM_ATX_all_ug_orgmat_g, TM_ATX_all_ug_chla_ug,
         TM_Chla_ug_g, TM_Pheo_ug_g, TM_Chla_Pheo_ratio, TM_percent_organic_matter, TAC_ATX_all_ug_g,
         TAC_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_chla_ug, TAC_Chla_ug_g, TAC_Pheo_ug_g,
         TAC_Chla_Pheo_ratio, TAC_percent_organic_matter)

# can fill out all NA's with 0's but may do that in the finalizing document
# sfkeel23 <- sfkeel23 %>% replace(is.na(.), 0)

#### (4) Joining in 2022 metabolism data and saving ####

## (a) getting median of GPP four days prior to match field dates

# separating out metabolism for each site
rus_metab_22 <- metab %>% 
  filter(site_year == "russian_USGS_2022")
sal_metab_22 <- metab %>% 
  filter(site_year == "salmon_karuk_2022")
eel_metab_22 <- metab %>% 
  filter(site_year == "sfkeel_mir_2022")

# need to interpolate missing dates for SFE-SH
# relevant here because model covariate is median GPP of x days prior
dates_2022_eel <- data.frame(seq(ymd('2022-06-30'), ymd('2022-09-17'), by = 'day'))
colnames(dates_2022_eel) <- c("field_date")
eel_metab_22 <- left_join(dates_2022_eel, eel_metab_22, by = "field_date")

# interpolate missing GPP mean for South Fork Eel 2022
# don't like that there is a lot of missing data here
# but it is likely this won't be used :)
eel_metab_22$GPP.mean[-nrow(eel_metab_22)] <- na.approx(eel_metab_22$GPP.mean)

# need to add a couple of extra days to salmon dataframe, but won't
# be able to interpolate
dates_2022_sal <- data.frame(seq(ymd('2022-06-27'), ymd('2022-09-22'), by = 'day'))
colnames(dates_2022_sal) <- c("field_date")
sal_metab_22 <- left_join(dates_2022_sal, sal_metab_22, by = "field_date")

# get dates of field visits
visits_eel <- allrivers22 %>% filter(site == "SFE-M") %>% select(field_date) %>% unique()
visits_rus <- allrivers22 %>% filter(site == "RUS") %>% select(field_date) %>% unique()
visits_sal <- allrivers22 %>% filter(site == "SAL") %>% select(field_date) %>% unique()

# remove 2022-07-07 date; as stated many times before, supposed to have sampled
# that reach on 07-06 but ran out of time
visits_rus <- as.data.frame(visits_rus[-which(visits_rus$field_date == ymd("2022-07-07")),])
colnames(visits_rus) <- "field_date"

# add column to visit dataframe to indicate visit number
visits_eel$visit <- seq(1, nrow(visits_eel))
visits_rus$visit <- seq(1, nrow(visits_rus))
visits_sal$visit <- seq(1, nrow(visits_sal))

# merge onto metabolism dataframes
eel_metab_22 <- left_join(eel_metab_22, visits_eel, by = "field_date")
rus_metab_22 <- left_join(rus_metab_22, visits_rus, by = "field_date")
sal_metab_22 <- left_join(sal_metab_22, visits_sal, by = "field_date")

# need to get indices of dataframe that have field visits
# cannot get median of four days prior for first field date, so adjust accordingly
index_eel <- which(!is.na(eel_metab_22$visit)) # cannot get more than a day before
index_rus <- which(!is.na(rus_metab_22$visit))[-1] # of GPP from first field visit
index_sal <- which(!is.na(sal_metab_22$visit))[-1]

# use map in purr to get the four days of GPP before each visit
map_eel <- index_eel %>% 
  purrr::map(. , function(x) eel_metab_22$GPP.mean[(x-4):(x-1)])
map_rus <- index_rus %>% 
  purrr::map(. , function(x) rus_metab_22$GPP.mean[(x-4):(x-1)])
map_sal <- index_sal %>% 
  purrr::map(. , function(x) sal_metab_22$GPP.mean[(x-4):(x-1)])

# take median of four days and put in dataframe with index
med_eel <- data.frame(unlist(lapply(map_eel, function(x) median(x))))
med_rus <- data.frame(unlist(lapply(map_rus, function(x) median(x))))
# have to remove an rm for sal...
med_sal <- data.frame(unlist(lapply(map_sal, function(x) median(x, na.rm = TRUE))))

# change column name
colnames(med_eel) <- c("GPP_median_fourdaysprior")
colnames(med_rus) <- c("GPP_median_fourdaysprior")
colnames(med_sal) <- c("GPP_median_fourdaysprior")

# put in visit index to GPP average
med_eel$visit <- visits_eel$visit[-1]
med_rus$visit <- visits_rus$visit[-1]
med_sal$visit <- visits_sal$visit[-1]

## (b) joining into final csv and saving

# left merge onto metabolism dataframes & only keep columns that we care about
eel_metab_22 <- left_join(eel_metab_22, med_eel, by = "visit") %>% 
  select(field_date, GPP_median_fourdaysprior) %>% 
  mutate(site = "SFE-M")
rus_metab_22 <- left_join(rus_metab_22, med_eel, by = "visit") %>% 
  select(field_date, GPP_median_fourdaysprior) %>% 
  mutate(site = "RUS")
sal_metab_22 <- left_join(sal_metab_22, med_eel, by = "visit") %>% 
  select(field_date, GPP_median_fourdaysprior) %>% 
  mutate(site = "SAL")

# keep only columns we care about and merge into south fork eel field dataframes
# left join by field date and site
allrivers22 <- left_join(allrivers22, rbind(eel_metab_22, rus_metab_22,
                                            sal_metab_22), by = c("field_date", "site"))

# save csv
write.csv(allrivers22, "./data/field_and_lab/allrivers22_combined.csv", row.names = FALSE)

#### Joining in 2023 metabolism data and saving ####

## (a) getting median of GPP four days prior to match field dates

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

# currently just doing average and median of four days before visit

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
med_mir <- data.frame(unlist(lapply(map_mir, function(x) median(x))))
med_sth <- data.frame(unlist(lapply(map_sth, function(x) median(x))))

# change column name
colnames(avg_mir) <- c("GPP_mean_fourdaysprior")
colnames(avg_sth) <- c("GPP_mean_fourdaysprior")
colnames(med_mir) <- c("GPP_median_fourdaysprior")
colnames(med_sth) <- c("GPP_median_fourdaysprior")

# add in median as a column to the dataframes for the average
avg_mir <- cbind(avg_mir, med_mir)
avg_sth <- cbind(avg_sth, med_sth)

# put in visit index to GPP average
avg_mir$visit <- visits_mir$visit[-1]
avg_sth$visit <- visits_sth$visit[-1]

# calculate change in GPP average
#avg_mir$GPP_change[2:nrow(avg_mir)] <- avg_mir$GPP_mean_fourdaysprior[2:nrow(avg_mir)] -
 # avg_mir$GPP_mean_fourdaysprior[1:(nrow(avg_mir)-1)]
#avg_sth$GPP_change[2:nrow(avg_sth)] <- avg_sth$GPP_mean_fourdaysprior[2:nrow(avg_sth)] -
 # avg_sth$GPP_mean_fourdaysprior[1:(nrow(avg_sth)-1)]

## (b) joining into final csv and saving

# left merge onto metabolism dataframes & only keep columns that we care about
metab_mir <- left_join(metab_mir, avg_mir, by = "visit") %>% 
  select(field_date, GPP_mean_fourdaysprior, GPP_median_fourdaysprior, discharge_m3_s) %>% 
  mutate(site = "SFE-M")
metab_sth <- left_join(metab_sth, avg_sth, by = "visit") %>% 
  select(field_date, GPP_mean_fourdaysprior, GPP_median_fourdaysprior, discharge_m3_s) %>% 
  mutate(site = "SFE-SH")

# keep only columns we care about and merge into south fork eel field dataframes
# left join by field date and site
sfkeel23 <- left_join(sfkeel23, rbind(metab_mir, metab_sth), by = c("field_date", "site"))

# save csv
write.csv(sfkeel23, "./data/field_and_lab/sfkeel23_combined.csv", row.names = FALSE)
