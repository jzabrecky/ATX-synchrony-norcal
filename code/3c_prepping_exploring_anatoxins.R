#### pre-modeling TM_data exploration for mat anatoxin concentrations
### Jordan Zabrecky
## last edited: 01.12.2025

# This script explores the available TM_data in the South Fork Eel 2023
# weekly TM_dataframe before building models to predict mat anatoxin concentrations
# of mats (total anatoxins normalized by percent organic matter of sample)

#### (1) Microcoleus Anatoxins ####

# loading libaries
lapply(c("tidyverse", "lubridate", "ggplot2"), require, character.only = T)

# get plotting functions from supplemental code
source("code/supplemental_code/S3a_exploration_plot_functions.R")

# all data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

# NA.omits for anatoxin data
# (toxin concentration not relevant if there was no sample to begin with)
TM_data <- data[-c(which(is.na(data$TM_ATX_all_ug_orgmat_g))),]
TAC_data <- data[-c(which(is.na(data$TAC_ATX_all_ug_orgmat_g))),]

# NA.omits for more limited data

#### (2) Microcoleus Cover (Benthic Survey Quadrat) ####

## 2a. Water Quality predictors ##

# temperature
time_plot(TM_data, TM_data$temp_C / 30, TM_data$TM_ATX_all_ug_orgmat_g / 40,
          "Temperature vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$temp_C, TM_data$TM_ATX_all_ug_orgmat_g,
          "Temperature vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$temp_C) # 0.109
# not really anything here

# pH
time_plot(TM_data, TM_data$pH / 10, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "pH vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$pH, TM_data$TM_ATX_all_ug_orgmat_g,
          "pH vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$pH) # -0.021
# not seeing any relationship visually; pH also reflects time of day and is 
# a straight line in the time plots

# dissolved oxygen
time_plot(TM_data, TM_data$DO_mg_L / 12, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "Dissolved Oxygen vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$DO_mg_L, TM_data$TM_ATX_all_ug_orgmat_g,
          "Dissolved Oxygen vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$DO_mg_L) # 0.141
# no relationship

# conductivity
time_plot(TM_data, TM_data$cond_uS_cm / 270, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "Dissolved Oxygen vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$cond_uS_cm, TM_data$TM_ATX_all_ug_orgmat_g,
          "Dissolved Oxygen vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$cond_uS_cm) # 0.348
# slight positive correlation

# orthophosphate
time_plot(TM_data, TM_data$oPhos_ug_P_L / 7, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "Orthophosphate vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$oPhos_ug_P_L, TM_data$TM_ATX_all_ug_orgmat_g,
          "Orthophosphate vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$oPhos_ug_P_L) # 0.164
# small positive linear relationship
# seems to be a slight increase in orthophoshate across the season

# nitrate
time_plot(TM_data, TM_data$nitrate_mg_N_L * 8, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "Nitrate vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$nitrate_mg_N_L, TM_data$TM_ATX_all_ug_orgmat_g,
          "Nitrate vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$nitrate_mg_N_L) # 0.216
# slight positive correlation

# ammonium
time_plot(TM_data, TM_data$ammonium_mg_N_L * 13, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "Ammonium vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$ammonium_mg_N_L, TM_data$TM_ATX_all_ug_orgmat_g,
          "Ammonium vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$ammonium_mg_N_L) # 0.245
# slight positive correlation here also

# dissolved organic carbon
time_plot(TM_data, TM_data$DOC_mg_L / 1.5, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "DOC vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$DOC_mg_L, TM_data$TM_ATX_all_ug_orgmat_g,
          "DOC vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$DOC_mg_L) # -0.046
# unsurprising- not really a relationship here

# total dissolved carbon
time_plot(TM_data, TM_data$TDC_mg_L / 18, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "TDC vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$TDC_mg_L, TM_data$TM_ATX_all_ug_orgmat_g,
          "TDC vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$TDC_mg_L) # -0.028
# unsurprising- not really a relationship here

## 2b. Biotic predictors (GPP, Other Species) ##

# Microcoleus cover
time_plot(TM_data, TM_data$microcoleus / 10, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "Cover vs. Anabaena/Cylindrospermum ATX")
corr_plot(TM_data, TM_data$microcoleus, TM_data$TM_ATX_all_ug_orgmat_g,
          "Cover vs. Anabaena/Cylindrospermum ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$microcoleus) # 0.112
# not really a predictor

# GPP (average of four days prior)
time_plot(TM_data, TM_data$GPP_mean_fourdaysprior / 10, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "GPP vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$GPP_mean_fourdaysprior, TM_data$TM_ATX_all_ug_orgmat_g,
          "GPP vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$GPP_mean_fourdaysprior) # -0.168
# stronger relationship here than with proportion of transects/presence

# change in GPP
time_plot(TM_data, TM_data$GPP_change, TM_data$TM_ATX_all_ug_orgmat_g / 35,
          "GPP change vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$GPP_change, TM_data$TM_ATX_all_ug_orgmat_g,
          "GPP change vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$GPP_change) # -0.149
# still nothing really

# N-fixers
time_plot(TM_data, TM_data$other_nfixers, TM_data$TM_ATX_all_ug_orgmat_g / 25,
          "N-fixer cover vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$other_nfixers, TM_data$TM_ATX_all_ug_orgmat_g,
          "N-fixer cover vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$other_nfixers) # 0.192
# not a good predictor

# chlorophyll-a of mats
time_plot(TM_data, TM_data$TM_Chla_ug_g / 18000, TM_data$TM_ATX_all_ug_orgmat_g,
          "Mat Chlorophyll-a vs. Microcoleus ATX")
corr_plot(TM_data, TM_data$TM_Chla_ug_g, TM_data$TM_ATX_all_ug_orgmat_g,
          "TN-fixer cover vs. Microcoleus ATX")
cor(TM_data$TM_ATX_all_ug_orgmat_g, TM_data$TM_Chla_ug_g) # 0.060
# no correlation here

#### (3) Anabaena/Cylindrospermum Anatoxins ####

## 3a. Water Quality predictors ##

# temperature
time_plot(TAC_data, TAC_data$temp_C / 30, TAC_data$TAC_ATX_all_ug_orgmat_g / 40,
          "Temperature vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$temp_C, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Temperature vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$temp_C) # 0.505
# really high correlation here which is interesting

# pH
time_plot(TAC_data, TAC_data$pH / 10, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "pH vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$pH, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "pH vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$pH) # -0.010
# not seeing any relationship visually; pH also reflects time of day and is 
# a straight line in the time plots

# dissolved oxygen
time_plot(TAC_data, TAC_data$DO_mg_L / 12, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$DO_mg_L, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$DO_mg_L) # 0.049
# no relationship

# conductivity
time_plot(TAC_data, TAC_data$cond_uS_cm / 270, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$cond_uS_cm, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Dissolved Oxygen vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$cond_uS_cm) # 0.304
# slight positive correlation

# orthophosphate
time_plot(TAC_data, TAC_data$oPhos_ug_P_L / 7, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "Orthophosphate vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$oPhos_ug_P_L, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Orthophosphate vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$oPhos_ug_P_L) # -0.031
# no relationship

# nitrate
time_plot(TAC_data, TAC_data$nitrate_mg_N_L * 8, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "Nitrate vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$nitrate_mg_N_L, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Nitrate vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$nitrate_mg_N_L) # -0.009
# no relationship

# ammonium
time_plot(TAC_data, TAC_data$ammonium_mg_N_L * 13, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "Ammonium vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$ammonium_mg_N_L, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Ammonium vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$ammonium_mg_N_L) # 0.054
# no relationship

# dissolved organic carbon
time_plot(TAC_data, TAC_data$DOC_mg_L / 1.5, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "DOC vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$DOC_mg_L, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "DOC vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$DOC_mg_L) # 0.0922
# unsurprising- not really a relationship here

# total dissolved carbon
time_plot(TAC_data, TAC_data$TDC_mg_L / 18, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "TDC vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$TDC_mg_L, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "TDC vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$TDC_mg_L) # -0.032
# unsurprising- not really a relationship here

## 3b. Biotic predictors (GPP, Other Species) ##

# Anabaena/Cylindrospermum cover
time_plot(TAC_data, TAC_data$anabaena_cylindrospermum / 10, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "Cover vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$anabaena_cylindrospermum, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Cover vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$anabaena_cylindrospermum) # 0.375
# positive relationship here

# GPP (average of four days prior)
time_plot(TAC_data, TAC_data$GPP_mean_fourdaysprior / 10, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "GPP vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$GPP_mean_fourdaysprior, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "GPP vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$GPP_mean_fourdaysprior) # -0.0553
# no relationship here

# change in GPP
time_plot(TAC_data, TAC_data$GPP_change, TAC_data$TAC_ATX_all_ug_orgmat_g / 35,
          "GPP change vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$GPP_change, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "GPP change vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$GPP_change) # 0.016
# still nothing really

# N-fixers
time_plot(TAC_data, TAC_data$other_nfixers, TAC_data$TAC_ATX_all_ug_orgmat_g / 25,
          "N-fixer cover vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$other_nfixers, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "N-fixer cover vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$other_nfixers) # -0.112
# not a good predictor-- wonder if we normalized this???

# chlorophyll-a of mats
time_plot(TAC_data, TAC_data$TAC_Chla_ug_g / 18000, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "Mat Chlorophyll-a vs. Anabaena/Cylindrospermum ATX")
corr_plot(TAC_data, TAC_data$TAC_Chla_ug_g, TAC_data$TAC_ATX_all_ug_orgmat_g,
          "TN-fixer cover vs. Anabaena/Cylindrospermum ATX")
cor(TAC_data$TAC_ATX_all_ug_orgmat_g, TAC_data$TAC_Chla_ug_g) # 0.072
# no correlation here
