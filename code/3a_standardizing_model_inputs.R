#### standardizing and normalizing data for predictive moels
### Jordan Zabrecky
## last edited: 06.17.2025

# This script standardizes (and, in some cases, normalizes) South Fork
# Eel 2023 data before predictive modeling

#### (1) Loading data and libraries ####

# loading libaries
lapply(c("tidyverse", "lubridate", "ggplot2"), require, character.only = T)

# all data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

#### (2) Prepping data for modeling ####

# making column that is dissolved inorganic nitrogen (DIN)
# which is ammonium, nitrate, and nitrite to reduce number of covariates
# we are assuming amount of nitrite is low
data <- data %>% 
  mutate(DIN_mg_N_L = ammonium_mg_N_L + nitrate_mg_N_L) %>% 
  relocate(DIN_mg_N_L, .after = ammonium_mg_N_L)

# new dataframe for standardizing covariates
data_cov <- data %>%  # remove columns that won't be used as covariates
  select(!c(proportion_micro_transects, proportion_ana_cyl_transects,
         proportion_riffle_rapid_transects, TM_ATX_all_ug_g, TM_ATX_all_ug_orgmat_g,
         TM_ATX_all_ug_chla_ug, TM_Chla_ug_g, TM_Pheo_ug_g,
         TM_percent_organic_matter, TAC_ATX_all_ug_g, TAC_ATX_all_ug_orgmat_g, 
         TAC_ATX_all_ug_chla_ug, TAC_Chla_ug_g, TAC_Pheo_ug_g,
         TAC_percent_organic_matter))
         

# standardize all covariates by reach
data_cov[,c(4:ncol(data_cov))] <- apply(data_cov[,c(4:ncol(data_cov))], MARGIN = 2, 
                                        function(x) ave(x, data_cov$site_reach, 
                                                        FUN = scale))

#### (3) Prepping response for modeling ####

## (a) Cover (Quadrat)

# get max for each reach
max_cover <- data %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(max_microcoleus = max(microcoleus),
                   max_anacyl = max(anabaena_cylindrospermum))

# normalizing for max microcoleus and anabaena/cylindrospermum
cover_rsp <- data %>% 
  mutate(max_microcoleus = case_when(site_reach == "SFE-M-1S" ~ max_cover$max_microcoleus[1],
                                     site_reach == "SFE-M-2" ~ max_cover$max_microcoleus[2],
                                     site_reach == "SFE-M-3" ~ max_cover$max_microcoleus[3],
                                     site_reach == "SFE-M-4" ~ max_cover$max_microcoleus[4],
                                     site_reach == "SFE-SH-1S" ~ max_cover$max_microcoleus[5],),
         max_anacyl = case_when(site_reach == "SFE-M-1S" ~ max_cover$max_anacyl[1],
                                site_reach == "SFE-M-2" ~ max_cover$max_anacyl[2],
                                site_reach == "SFE-M-3" ~ max_cover$max_anacyl[3],
                                site_reach == "SFE-M-4" ~ max_cover$max_anacyl[4],
                                site_reach == "SFE-SH-1S" ~ max_cover$max_anacyl[5]),
         resp_M_cover_norm = microcoleus / max_microcoleus * 100,
         resp_AC_cover_norm = anabaena_cylindrospermum / max_anacyl * 100)

# keep response variables only
cover_rsp <- cover_rsp %>% 
  select(field_date, site_reach, site, resp_M_cover_norm, resp_AC_cover_norm)

## (b) Transects Present

# get max proportion of transects for each reach
max_pres <- data %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(max_microcoleus = max(proportion_micro_transects),
                   max_anacyl = max(proportion_ana_cyl_transects))

# normalizing for max microcoleus and anabaena/cylindrospermum
pres_rsp <- data %>% 
  mutate(max_microcoleus = case_when(site_reach == "SFE-M-1S" ~ max_pres$max_microcoleus[1],
                                     site_reach == "SFE-M-2" ~ max_pres$max_microcoleus[2],
                                     site_reach == "SFE-M-3" ~ max_pres$max_microcoleus[3],
                                     site_reach == "SFE-M-4" ~ max_pres$max_microcoleus[4],
                                     site_reach == "SFE-SH-1S" ~ max_pres$max_microcoleus[5],),
         max_anacyl = case_when(site_reach == "SFE-M-1S" ~ max_pres$max_anacyl[1],
                                site_reach == "SFE-M-2" ~ max_pres$max_anacyl[2],
                                site_reach == "SFE-M-3" ~ max_pres$max_anacyl[3],
                                site_reach == "SFE-M-4" ~ max_pres$max_anacyl[4],
                                site_reach == "SFE-SH-1S" ~ max_pres$max_anacyl[5]),
         resp_M_pres_norm = proportion_micro_transects / max_microcoleus * 100,
         resp_AC_pres_norm = proportion_ana_cyl_transects / max_anacyl * 100)

# keep response variables only
pres_rsp <- pres_rsp %>% 
  select(field_date, site_reach, site, resp_M_pres_norm, resp_AC_pres_norm)

## (c) Anatoxins

# modeling with anatoxins normalized to ash-free dry mass % of sample

# fill NAs with 0
data$TM_ATX_all_ug_orgmat_g <- replace_na(data$TM_ATX_all_ug_orgmat_g, 0)
data$TAC_ATX_all_ug_orgmat_g <- replace_na(data$TAC_ATX_all_ug_orgmat_g, 0)

# get max for each reach
max_atx <- data %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(max_TM_atx = max(TM_ATX_all_ug_orgmat_g),
                   max_TAC_atx = max(TAC_ATX_all_ug_orgmat_g))

# normalizing for max microcoleus and anabaena/cylindrospermum anatoxins
atx_rsp <- data %>% 
  mutate(max_TM_atx = case_when(site_reach == "SFE-M-1S" ~ max_atx$max_TM_atx[1],
                                     site_reach == "SFE-M-2" ~ max_atx$max_TM_atx[2],
                                     site_reach == "SFE-M-3" ~ max_atx$max_TM_atx[3],
                                     site_reach == "SFE-M-4" ~ max_atx$max_TM_atx[4],
                                     site_reach == "SFE-SH-1S" ~ max_atx$max_TM_atx[5],),
         max_TAC_atx = case_when(site_reach == "SFE-M-1S" ~ max_atx$max_TAC_atx[1],
                                site_reach == "SFE-M-2" ~ max_atx$max_TAC_atx[2],
                                site_reach == "SFE-M-3" ~ max_atx$max_TAC_atx[3],
                                site_reach == "SFE-M-4" ~ max_atx$max_TAC_atx[4],
                                site_reach == "SFE-SH-1S" ~ max_atx$max_TAC_atx[5]),
         resp_M_atx_norm = TM_ATX_all_ug_orgmat_g / max_TM_atx * 100,
         resp_AC_atx_norm = TAC_ATX_all_ug_orgmat_g / max_TAC_atx * 100)

# keep response variables only
atx_rsp <- atx_rsp %>% 
  # also including raw TM & TAC (per organic matter)
  mutate(resp_M_atx_raw = TM_ATX_all_ug_orgmat_g,
         resp_AC_atx_raw = TAC_ATX_all_ug_orgmat_g) %>%
  select(field_date, site_reach, site, resp_M_atx_raw, resp_AC_atx_raw, 
         resp_M_atx_norm, resp_AC_atx_norm)

#### (4) Join covariates and responses and save ####

# left join in responses to standardized covariates
final <- left_join(data_cov, cover_rsp, by = c("field_date", "site_reach", "site"))
final <- left_join(final, pres_rsp, by = c("field_date", "site_reach", "site"))
final <- left_join(final, atx_rsp, by = c("field_date", "site_reach", "site"))

# save csv
write.csv(final, "./data/predictive_models/inputs.csv", row.names = FALSE)
