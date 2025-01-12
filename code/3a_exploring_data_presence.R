#### pre-modeling data exploration for BC presence based on transects
### Jordan Zabrecky
## last edited: 01.12.2025

# This script explores the available data in the South Fork Eel 2023
# weekly dataframe before building models to predict presence 
# based on proportion of transects in which given benthic cyanobacteria
# is present

# need to think about normalized vs. non-normalized covariates

#### (1) Loading data and libraries ####

# loading libaries
lapply(c("tidyverse", "lubridate", "ggplot2"), require, character.only = T)

# get plotting functions from supplemental code
source("code/supplemental_code/S3a_exploration_plot_functions.R")

# all data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

# NA.omits for more limited data
GPP_data <- data[-c(which(is.na(data$GPP_mean_fourdaysprior))),]
GPP_change_data <- data[-c(which(is.na(data$GPP_change))),]
chla_TM_data <- data[-c(which(is.na(data$TM_Chla_ug_g))),]
chla_TAC_data <- data[-c(which(is.na(data$TAC_Chla_ug_g))),]


#### (2) Microcoleus Presence (Percent of Transects Present) ####

## 2a. Geomorphic & Hydraulic predictors ##

# discharge
time_plot(data, data$discharge_m3_s / 5, data$proportion_micro_transects,
          "Discharge (cms) vs. Proportion Microcoleus Transects")
corr_plot(data, data$discharge_m3_s, data$proportion_micro_transects,
          "Discharge (cms) vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$discharge_m3_s) # -.634
# negative exponential relationship rather than linear!

# median transect depth sampled (cm)
time_plot(data, data$median_depth_cm_sampled / 60, data$proportion_micro_transects,
          "Median Depth Sampled vs. Proportion Microcoleus Transects")
corr_plot(data, data$median_depth_cm_sampled, data$proportion_micro_transects,
          "Median Depth Sampled vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$median_depth_cm_sampled) # -.344
cor(data$proportion_micro_transects, data$average_depth_cm_sampled) # -.296
# would use median instead of average here; slight negative relationship

# proportion of transects rapid or riffle
time_plot(data, data$proportion_riffle_rapid_transects, data$proportion_micro_transects,
          "Proportion Riffle/Rapid Transects vs. Proportion Microcoleus Transects")
corr_plot(data, data$proportion_riffle_rapid_transects, data$proportion_micro_transects,
          "Proportion Riffle/Rapid Transects vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$proportion_riffle_rapid_transects) # .185
# not useful information as riffle/rapids exist prior to accrual

## 2b. Water Quality predictors ##

# temperature
time_plot(data, data$temp_C / 35, data$proportion_micro_transects,
          "Temperature vs. Proportion Microcoleus Transects")
corr_plot(data, data$temp_C, data$proportion_micro_transects,
          "Temperature vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$temp_C) # -0.170
# not really a relationship; may also be reflective of time of day
# could also maybe explore an aggregate with continuous data

# pH
time_plot(data, data$pH / 10, data$proportion_micro_transects,
          "pH vs. Proportion Microcoleus Transects")
corr_plot(data, data$pH, data$proportion_micro_transects,
          "pH vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$pH) # -0.383
# not seeing any relationship visually; pH also reflects time of day and is 
# a straight line in the time plots

# dissolved oxygen
time_plot(data, data$DO_mg_L / 12, data$proportion_micro_transects,
          "Dissolved Oxygen vs. Proportion Microcoleus Transects")
corr_plot(data, data$DO_mg_L, data$proportion_micro_transects,
          "Dissolved Oxygen vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$DO_mg_L) # -0.388
# slight negative relationship; I wouldn't guess is important though

# conductivity
time_plot(data, data$cond_uS_cm / 270, data$proportion_micro_transects,
          "Dissolved Oxygen vs. Proportion Microcoleus Transects")
corr_plot(data, data$cond_uS_cm, data$proportion_micro_transects,
          "Dissolved Oxygen vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$cond_uS_cm) # 0.623
# slight increase in conductivity across the field season
# positive linear relationship with presence

# orthophosphate
time_plot(data, data$oPhos_ug_P_L / 7, data$proportion_micro_transects,
          "Orthophosphate vs. Proportion Microcoleus Transects")
corr_plot(data, data$oPhos_ug_P_L, data$proportion_micro_transects,
          "Orthophosphate vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$oPhos_ug_P_L) # 0.401
# positive linear relationship; 
# seems to be a slight increase in orthophoshate across the season

# nitrate
time_plot(data, data$nitrate_mg_N_L * 8, data$proportion_micro_transects,
          "Nitrate vs. Proportion Microcoleus Transects")
corr_plot(data, data$nitrate_mg_N_L, data$proportion_micro_transects,
          "Nitrate vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$nitrate_mg_N_L) # 0.344
# also seems to be a slight increase in nitrate across the season

# ammonium
time_plot(data, data$ammonium_mg_N_L * 13, data$proportion_micro_transects,
          "Ammonium vs. Proportion Microcoleus Transects")
corr_plot(data, data$ammonium_mg_N_L, data$proportion_micro_transects,
          "Ammonium vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$ammonium_mg_N_L) # 0.280
# also seems to be a slight increase in ammonium across the season

# dissolved organic carbon
time_plot(data, data$DOC_mg_L / 1.5, data$proportion_micro_transects,
          "DOC vs. Proportion Microcoleus Transects")
corr_plot(data, data$DOC_mg_L, data$proportion_micro_transects,
          "DOC vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$DOC_mg_L) # -0.132
# unsurprising- not really a relationship here

# total dissolved carbon
time_plot(data, data$TDC_mg_L / 18, data$proportion_micro_transects,
          "TDC vs. Proportion Microcoleus Transects")
corr_plot(data, data$TDC_mg_L, data$proportion_micro_transects,
          "TDC vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$TDC_mg_L) # 0.088
# unsurprising- not really a relationship here

## 2c. Biotic predictors (GPP, Other Species) ##

# GPP (average of four days prior)
time_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior / 10, GPP_data$proportion_micro_transects,
          "GPP vs. Proportion Microcoleus Transects")
corr_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior, GPP_data$proportion_micro_transects,
          "GPP vs. Proportion Microcoleus Transects")
cor(GPP_data$proportion_micro_transects, GPP_data$GPP_mean_fourdaysprior) # -0.106
# increase when decline occurs; otherwise only relationship for SFE-SH
# maybe could calculate a change in GPP as a parameter?

# change in GPP
time_plot(GPP_change_data, GPP_change_data$GPP_change, GPP_change_data$proportion_micro_transects,
          "GPP change vs. Proportion Microcoleus Transects")
corr_plot(GPP_change_data, GPP_change_data$GPP_change, GPP_change_data$proportion_micro_transects,
          "GPP change vs. Proportion Microcoleus Transects")
cor(GPP_change_data$proportion_micro_transects, GPP_change_data$GPP_change) # -0.140
# still nothing really

# N-fixers
time_plot(data, data$other_nfixers, data$proportion_micro_transects,
          "N-fixer cover vs. Proportion Microcoleus Transects")
corr_plot(data, data$other_nfixers, data$proportion_micro_transects,
          "N-fixer cover vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$other_nfixers) # 0.071
# not a good predictor

# chlorophyll-a of mats
time_plot(chla_TM_data, chla_TM_data$TM_Chla_ug_g / 18000, chla_TM_data$proportion_micro_transects,
          "Mat Chlorophyll-a vs. Proportion Microcoleus Transects")
corr_plot(chla_TM_data, chla_TM_data$TM_Chla_ug_g, chla_TM_data$proportion_micro_transects,
          "TN-fixer cover vs. Proportion Microcoleus Transects")
cor(chla_TM_data$proportion_micro_transects, chla_TM_data$TM_Chla_ug_g) # 0.691
# as mats become more widespread they have more chlorophyll-a per gram??

#### (3) Anabaena/Cylindrospermum Presence (Percent of Transects Present) ####

# do later