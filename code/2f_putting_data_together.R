#### putting all field and lab data together for south fork eel 2023
### Jordan Zabrecky
## last edited: 12.17.2024

# This script aggregates all field and lab information for reaches
# on the South Fork Eel in 2023

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "lubridate"), require, character.only = T)

# read in data
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")
water <- read.csv("./data/field_and_lab/water_chemistry.csv")
survey <- read.csv("./data/field_and_lab/percover_byreach.csv")

#### (2) Processing and Joining Data Frames ####

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

#### (3) Focusing on South Fork Eel 2023 data and save ####

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
         TDC_mg_L, DOC_mg_L, TM_ATX_all_ug_orgmat_g, TM_Chla_ug_g, TM_Pheo_ug_g, 
         TM_percent_organic_matter, TAC_ATX_all_ug_orgmat_g, TAC_Chla_ug_g, TAC_Pheo_ug_g,
         TAC_percent_organic_matter)

# fill anatoxin NAs with 0's when Microcoleus or TAC present??
# instead of just all of those 0's?