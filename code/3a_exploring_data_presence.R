#### pre-modeling data exploration
### Jordan Zabrecky
## last edited: 01.11.2025

# This script explores the available data in the South Fork Eel 2023
# weekly dataframe before building models to predict presence, cover, 
# and anatoxin concentrations

# need to think about normalized vs. non-normalized covariates

#### (1) Loading data and libraries ####

# loading libaries
lapply(c("tidyverse", "lubridate", "ggplot2"), require, character.only = T)

# all data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

# NA.omits for more limited data
which(is.na(data$GPP_mean_fourdaysprior))
GPP_data <- data[-c(1, 15, 30, 45, 60),]
# chlorophyll-a of mat

#### (2) Functions for plotting ####

# plots for time series
# five panel with each site reach separated out
time_plot <- function(data, predictor, response, title) {
  ggplot(data = data, aes(x = field_date)) +
    geom_point(y = predictor, color = "gray") +
    geom_point(y = response, color = "purple") +
    labs(title = title) +
    facet_wrap(~site_reach) +
    theme_bw()
}

# plots for correlation
corr_plot <- function(data, predictor, response, title) {
  
  # plot with all site reaches
  all <- ggplot(data = data, aes(x = predictor, y = response, color = site_reach, 
                                 shape = site_reach)) +
                geom_point() +
                labs(title = title) +
                theme_bw()
  print(all)
  
  # plots with site reaches separated out
  separate <- ggplot(data = data, aes(x = predictor, y = response, color = site_reach, 
                                      shape = site_reach)) +
                      geom_point() +
                      labs(title = title) +
                      theme_bw() +
                      facet_wrap(~site_reach)
  print(separate)
}

#### (3) Microcoleus Presence (Percent of Transects Present) ####

## 3a. Geomorphic & Hydraulic predictors ##

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

## 3b. Water Quality predictors ##

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

## 3c. Biotic predictors (GPP, Other Species) ##

# GPP (average of four days prior)
time_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior / 10, GPP_data$proportion_micro_transects,
          "GPP vs. Proportion Microcoleus Transects")
corr_plot(GPP_data, GPP_data$GPP_mean_fourdaysprior, GPP_data$proportion_micro_transects,
          "GPP vs. Proportion Microcoleus Transects")
cor(GPP_data$proportion_micro_transects, GPP_data$GPP_mean_fourdaysprior) # -0.106
# increase when decline occurs; otherwise only relationship for SFE-SH
# maybe could calculate a change in GPP as a parameter?

# change in GPP

# N-fixers
time_plot(data, data$other_nfixers, data$proportion_micro_transects,
          "N-fixer cover vs. Proportion Microcoleus Transects")
corr_plot(data, data$other_nfixers, data$proportion_micro_transects,
          "N-fixer cover vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$other_nfixers) # 0.071
# not a good predictor

# chlorophyll-a of mats
time_plot(data, data$TM_Chla_ug_g, data$proportion_micro_transects,
          "Mat Chlorophyll-a vs. Proportion Microcoleus Transects")
corr_plot(data, data$TM_Chla_ug_g, data$proportion_micro_transects,
          "TN-fixer cover vs. Proportion Microcoleus Transects")
cor(data$proportion_micro_transects, data$TM_Chla_ug_g) # 0.691

# curious about chlorophyll-a pheo ratio

#### (4) Anabaena/Cylindrospermum Presence (Percent of Transects Present) ####

#### (4) Microcoleus Extent (Percent Cover from Benthic Surveys) ####

## 4a. Geomorphic & Hydraulic predictors ##

# curious though how proportion rapid/riffle relates to max cover

## 4b. Water Quality predictors ##

## 4c. Biotic predictors (GPP, Other Species) ##

#### (5) Microcoleus Anatoxin Concentrations ####

## 4a. Cyano cover only ##

## 4b. Water Quality predictors ##

## 4c. Biotic predicotrs (GPP, Other Species) ##