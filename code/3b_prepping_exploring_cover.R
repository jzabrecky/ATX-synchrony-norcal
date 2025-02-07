#### pre-modeling data exploration for BC cover based on surveys
### Jordan Zabrecky
## last edited: 02.05.2025

# This script explores the available data in the South Fork Eel 2023
# weekly dataframe before building models to predict presence 
# based on percent cover of given benthic cyanobacteria as obtained
# via quadrats in benthic surveys following the California SWAMP protocol

# need to redo w/ normalization per site
# also consider logging discharge?
# and want median GPP after redoing metab models

#### (1) Loading data and libraries ####

# loading libaries
lapply(c("tidyverse", "lubridate", "ggplot2"), require, character.only = T)

# get plotting functions from supplemental code
source("code/supplemental_code/S3a_exploration_plot_functions.R")

# all data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

#### (2) Prepping data for modeling ####

## (a) normalize response variable (microcoleus and anabaena) to max of reach

# get max for each reach
max <- data %>% 
  group_by(site_reach) %>% 
  summarize(max_microcoleus = max(microcoleus),
            max_anacyl = max(anabaena_cylindrospermum))

# calculate percentage of maximum for each to use as response variable
data <- data %>% 
  mutate(max_microcoleus = case_when(site_reach == "SFE-M-1S" ~ max$max_microcoleus[1],
                                     site_reach == "SFE-M-2" ~ max$max_microcoleus[2],
                                     site_reach == "SFE-M-3" ~ max$max_microcoleus[3],
                                     site_reach == "SFE-M-4" ~ max$max_microcoleus[4],
                                     site_reach == "SFE-SH-1S" ~ max$max_microcoleus[5],),
         max_anacyl = case_when(site_reach == "SFE-M-1S" ~ max$max_anacyl[1],
                                site_reach == "SFE-M-2" ~ max$max_anacyl[2],
                                site_reach == "SFE-M-3" ~ max$max_anacyl[3],
                                site_reach == "SFE-M-4" ~ max$max_anacyl[4],
                                site_reach == "SFE-SH-1S" ~ max$max_anacyl[5]),
         resp_micro = round(microcoleus / max_microcoleus * 100),
         resp_anacyl = round(anabaena_cylindrospermum / max_anacyl * 100))

## (b) normalize covariates

# remove columns we don't need (i.e. toxins won't be used to predict occurrence!)
data_std <- data %>% 
  select(!(TM_ATX_all_ug_g:TAC_percent_organic_matter))

# standardize all covariates by reach
data_std[,c(4:25)] <- apply(data_std[,c(4:25)], MARGIN = 2, function(x) ave(x, data_std$site_reach, FUN = scale))

# save csv
write.csv(data_std, "./data/predictive_model_inputs/cover_inputs.csv", row.names = FALSE)

#### OLD CODE BELOW :) will reanalyze with normalizations

# NA.omits for more limited data
GPP_data <- data[-c(which(is.na(data$GPP_mean_fourdaysprior))),]
GPP_change_data <- data[-c(which(is.na(data$GPP_change))),]
chla_TM_data <- data[-c(which(is.na(data$TM_Chla_ug_g))),]
chla_TAC_data <- data[-c(which(is.na(data$TAC_Chla_ug_g))),]

#### (2) Microcoleus Cover (Benthic Survey Quadrat) ####

## 2a. Geomorphic & Hydraulic predictors ##

# discharge
time_plot(data, data$discharge_m3_s / 5, data$microcoleus / 25,
          "Discharge (cms) vs. Microcoleus Quadrat Cover")
corr_plot(data, data$discharge_m3_s, data$microcoleus,
          "Discharge (cms) vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$discharge_m3_s) # -.429
# negative exponential relationship rather than linear!

# median transect depth sampled (cm)
time_plot(data, data$median_depth_cm_sampled / 60, data$microcoleus / 25,
          "Median Depth Sampled vs. Microcoleus Quadrat Cover")
corr_plot(data, data$median_depth_cm_sampled, data$microcoleus,
          "Median Depth Sampled vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$median_depth_cm_sampled) # -.007
cor(data$microcoleus, data$average_depth_cm_sampled) # -.050
# no relationship

# proportion of transects rapid or riffle
time_plot(data, data$proportion_riffle_rapid_transects, data$microcoleus,
          "Proportion Riffle/Rapid Transects vs. Microcoleus Quadrat Cover")
corr_plot(data, data$proportion_riffle_rapid_transects, data$microcoleus,
          "Proportion Riffle/Rapid Transects vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$proportion_riffle_rapid_transects) # -0.057
# not useful information as riffle/rapids exist prior to accrual

## 2b. Water Quality predictors ##

# temperature
time_plot(data, data$temp_C / 35, data$microcoleus / 25,
          "Temperature vs. Microcoleus Quadrat Cover")
corr_plot(data, data$temp_C, data$microcoleus,
          "Temperature vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$temp_C) # -0.135
# not really a relationship; may also be reflective of time of day
# could also maybe explore an aggregate with continuous data

# pH
time_plot(data, data$pH / 10, data$microcoleus / 25,
          "pH vs. Microcoleus Quadrat Cover")
corr_plot(data, data$pH, data$microcoleus,
          "pH vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$pH) # -0.280
# not seeing any relationship visually; pH also reflects time of day and is 
# a straight line in the time plots

# dissolved oxygen
time_plot(data, data$DO_mg_L / 12, data$microcoleus / 25,
          "Dissolved Oxygen vs. Microcoleus Quadrat Cover")
corr_plot(data, data$DO_mg_L, data$microcoleus,
          "Dissolved Oxygen vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$DO_mg_L) # -0.246
# slight negative relationship; I wouldn't guess is important though

# conductivity
time_plot(data, data$cond_uS_cm / 270, data$microcoleus / 25,
          "Dissolved Oxygen vs. Microcoleus Quadrat Cover")
corr_plot(data, data$cond_uS_cm, data$microcoleus,
          "Dissolved Oxygen vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$cond_uS_cm) # 0.556
# slight increase in conductivity across the field season
# positive linear relationship with presence

# orthophosphate
time_plot(data, data$oPhos_ug_P_L / 7, data$microcoleus / 25,
          "Orthophosphate vs. Microcoleus Quadrat Cover")
corr_plot(data, data$oPhos_ug_P_L, data$microcoleus,
          "Orthophosphate vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$oPhos_ug_P_L) # 0.360
# small positive linear relationship
# seems to be a slight increase in orthophoshate across the season

# nitrate
time_plot(data, data$nitrate_mg_N_L * 8, data$microcoleus / 25,
          "Nitrate vs. Microcoleus Quadrat Cover")
corr_plot(data, data$nitrate_mg_N_L, data$microcoleus,
          "Nitrate vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$nitrate_mg_N_L) # 0.252
# also seems to be a slight increase in nitrate across the season

# ammonium
time_plot(data, data$ammonium_mg_N_L * 13, data$microcoleus / 25,
          "Ammonium vs. Microcoleus Quadrat Cover")
corr_plot(data, data$ammonium_mg_N_L, data$microcoleus,
          "Ammonium vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$ammonium_mg_N_L) # 0.171
# also seems to be a slight increase in ammonium across the season

# dissolved organic carbon
time_plot(data, data$DOC_mg_L / 1.5, data$microcoleus / 25,
          "DOC vs. Microcoleus Quadrat Cover")
corr_plot(data, data$DOC_mg_L, data$microcoleus,
          "DOC vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$DOC_mg_L) # -0.138
# unsurprising- not really a relationship here

# total dissolved carbon
time_plot(data, data$TDC_mg_L / 18, data$microcoleus / 25,
          "TDC vs. Microcoleus Quadrat Cover")
corr_plot(data, data$TDC_mg_L, data$microcoleus,
          "TDC vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$TDC_mg_L) # 0.178
# unsurprising- not really a relationship here

## 2c. Biotic predictors (GPP, Other Species) ##

# GPP (average of four days prior)
time_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior / 10, GPP_data$microcoleus / 25,
          "GPP vs. Microcoleus Quadrat Cover")
corr_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior, GPP_data$microcoleus,
          "GPP vs. Microcoleus Quadrat Cover")
cor(GPP_data$microcoleus, GPP_data$GPP_mean_fourdaysprior) # -0.375
# stronger relationship here than with proportion of transects/presence

# change in GPP
time_plot(GPP_change_data, GPP_change_data$GPP_change, GPP_change_data$microcoleus / 25,
          "GPP change vs. Microcoleus Quadrat Cover")
corr_plot(GPP_change_data, GPP_change_data$GPP_change, GPP_change_data$microcoleus,
          "GPP change vs. Microcoleus Quadrat Cover")
cor(GPP_change_data$microcoleus, GPP_change_data$GPP_change) # -0.174
# still nothing really

# N-fixers
time_plot(data, data$other_nfixers, data$microcoleus / 25,
          "N-fixer cover vs. Microcoleus Quadrat Cover")
corr_plot(data, data$other_nfixers, data$microcoleus,
          "N-fixer cover vs. Microcoleus Quadrat Cover")
cor(data$microcoleus, data$other_nfixers) # 0.149
# not a good predictor

# chlorophyll-a of mats
time_plot(chla_TM_data, chla_TM_data$TM_Chla_ug_g / 18000, chla_TM_data$microcoleus,
          "Mat Chlorophyll-a vs. Microcoleus Quadrat Cover")
corr_plot(chla_TM_data, chla_TM_data$TM_Chla_ug_g, chla_TM_data$microcoleus,
          "TN-fixer cover vs. Microcoleus Quadrat Cover")
cor(chla_TM_data$microcoleus, chla_TM_data$TM_Chla_ug_g) # 0.465
# as mats become more widespread they have more chlorophyll-a per gram??

#### (3) Anabaena/Cylindrospermum Cover (Benthic Survey Quadrat) ####

## 3a. Geomorphic & Hydraulic predictors ##

# discharge
time_plot(data, data$discharge_m3_s / 5, data$anabaena_cylindrospermum / 25,
          "Discharge (cms) vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$discharge_m3_s, data$anabaena_cylindrospermum,
          "Discharge (cms) vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$discharge_m3_s) # -.185
# not a predictor like with Microcoleus

# median transect depth sampled (cm)
time_plot(data, data$median_depth_cm_sampled / 60, data$anabaena_cylindrospermum / 25,
          "Median Depth Sampled vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$median_depth_cm_sampled, data$anabaena_cylindrospermum,
          "Median Depth Sampled vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$median_depth_cm_sampled) # 0.247
cor(data$anabaena_cylindrospermum, data$average_depth_cm_sampled) # 0.093
# no relationship really

# proportion of transects rapid or riffle
time_plot(data, data$proportion_riffle_rapid_transects, data$anabaena_cylindrospermum,
          "Proportion Riffle/Rapid Transects vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$proportion_riffle_rapid_transects, data$anabaena_cylindrospermum,
          "Proportion Riffle/Rapid Transects vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$proportion_riffle_rapid_transects) # -0.322
# maybe more useful here? sites with less riffle/rapid have higher anabaena potential

## 3b. Water Quality predictors ##

# temperature
time_plot(data, data$temp_C / 35, data$anabaena_cylindrospermum / 25,
          "Temperature vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$temp_C, data$anabaena_cylindrospermum,
          "Temperature vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$temp_C) # 0.394
# maybe relationship here as it peaks in the midle of the summer when temps
# are the highest

# pH
time_plot(data, data$pH / 10, data$anabaena_cylindrospermum / 25,
          "pH vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$pH, data$anabaena_cylindrospermum,
          "pH vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$pH) # 0.226
# not seeing any relationship visually; pH also reflects time of day and is 
# a straight line in the time plots

# dissolved oxygen
time_plot(data, data$DO_mg_L / 12, data$anabaena_cylindrospermum / 25,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$DO_mg_L, data$anabaena_cylindrospermum,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$DO_mg_L) # 0.234
# slight negative relationship; I wouldn't guess is important though

# conductivity
time_plot(data, data$cond_uS_cm / 270, data$anabaena_cylindrospermum / 25,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$cond_uS_cm, data$anabaena_cylindrospermum,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum  Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$cond_uS_cm) # 0.079
# not really a relationship here

# orthophosphate
time_plot(data, data$oPhos_ug_P_L / 7, data$anabaena_cylindrospermum / 25,
          "Orthophosphate vs. Anabaena/Cylindrospermum  Quadrat Cover")
corr_plot(data, data$oPhos_ug_P_L, data$anabaena_cylindrospermum,
          "Orthophosphate vs. Anabaena/Cylindrospermum  Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$oPhos_ug_P_L) # -0.085
# not really a relationship

# nitrate
time_plot(data, data$nitrate_mg_N_L * 8, data$anabaena_cylindrospermum / 25,
          "Nitrate vs. Anabaena/Cylindospermum Quadrat Cover")
corr_plot(data, data$nitrate_mg_N_L, data$anabaena_cylindrospermum,
          "Nitrate vs. Anabaena/Cylindospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$nitrate_mg_N_L) # -0.158
# not really a relationship

# ammonium
time_plot(data, data$ammonium_mg_N_L * 13, data$anabaena_cylindrospermum / 25,
          "Ammonium vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$ammonium_mg_N_L, data$anabaena_cylindrospermum,
          "Ammonium vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$ammonium_mg_N_L) # 0.033
# not really a relationship

# dissolved organic carbon
time_plot(data, data$DOC_mg_L / 1.5, data$anabaena_cylindrospermum / 25,
          "DOC vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$DOC_mg_L, data$anabaena_cylindrospermum,
          "DOC vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$DOC_mg_L) # 0.107
# unsurprising- not really a relationship here

# total dissolved carbon
time_plot(data, data$TDC_mg_L / 18, data$anabaena_cylindrospermum / 25,
          "TDC vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$TDC_mg_L, data$anabaena_cylindrospermum,
          "TDC vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$TDC_mg_L) # 0.079
# unsurprising- not really a relationship here

## 3c. Biotic predictors (GPP, Other Species) ##

# GPP (average of four days prior)
time_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior / 10, GPP_data$anabaena_cylindrospermum / 25,
          "GPP vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior, GPP_data$anabaena_cylindrospermum,
          "GPP vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(GPP_data$anabaena_cylindrospermum, GPP_data$GPP_mean_fourdaysprior) # -0.044
# not really a predictor here

# change in GPP
time_plot(GPP_change_data, GPP_change_data$GPP_change, GPP_change_data$anabaena_cylindrospermum / 25,
          "GPP change vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(GPP_change_data, GPP_change_data$GPP_change, GPP_change_data$anabaena_cylindrospermum,
          "GPP change vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(GPP_change_data$anabaena_cylindrospermum, GPP_change_data$GPP_change) # -0.002
# still nothing really

# N-fixers
time_plot(data, data$other_nfixers / 2, data$anabaena_cylindrospermum / 25,
          "N-fixer cover vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(data, data$other_nfixers, data$anabaena_cylindrospermum,
          "N-fixer cover vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(data$anabaena_cylindrospermum, data$other_nfixers) # 0.057
# not a good predictor

# chlorophyll-a of mats
time_plot(chla_TAC_data, chla_TAC_data$TAC_Chla_ug_g / 10000, chla_TAC_data$anabaena_cylindrospermum / 25,
          "Mat Chlorophyll-a vs. Anabaena/Cylindrospermum Quadrat Cover")
corr_plot(chla_TAC_data, chla_TAC_data$TAC_Chla_ug_g, chla_TAC_data$anabaena_cylindrospermum,
          "TN-fixer cover vs. Anabaena/Cylindrospermum Quadrat Cover")
cor(chla_TAC_data$anabaena_cylindrospermum, chla_TAC_data$TAC_Chla_ug_g) # 0.261
# not really anything here
