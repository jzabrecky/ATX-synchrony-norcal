#### predicting to determine process uncertainty in models predicting Anabaena/Cylindrospermum cover
### Jordan Zabrecky
## last edited: 01.13.2026

# This script uses previously built models (see script 3c) to predict 
# Anabaena/Cylindrospermum cover WITHOUT incorporating parameter uncertainty (by holding
# parameter estimates consistent) to make predictions to 
# determine process uncertainty

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "truncnorm", "Metrics"), 
       require, character.only = T)

# loading in data and select only columns we care about
raw_data <- read.csv("./data/predictive_models/inputs.csv") %>% 
  select(field_date, site_reach, resp_AC_cover_norm, future_AC_cover_norm, temp_C,
         discharge_m3_s, DIN_mg_N_L, oPhos_ug_P_L, cond_uS_cm,
         GPP_median_tofourdaysprior)

# make another data frame for edited data
data <- raw_data

# when cover = 0, add 0.05 (so model can increase as we multiply by this value)
data <- data %>%
  mutate(resp_AC_cover_norm = case_when(resp_AC_cover_norm == 0 ~ 0.05,
                                        TRUE ~ resp_AC_cover_norm),
         future_AC_cover_norm = case_when(future_AC_cover_norm == 0 ~ 0.05,
                                          TRUE ~ future_AC_cover_norm))

# lastly, double check there is no NA for STAN purposes
any(is.na(data)) # nope!

# set seed for consistent predictions!
# (NOTE: setting this up post-model creation...)
set.seed(2026)

#### (2) Separating out data for modeling ####

# model evaluating/testing set list
test_sites <- split(data, data$site_reach)

# model building/training set list
training_sites <- list()
for(i in 1:length(test_sites)) {
  training_sites[[i]] <- data %>%
    filter(site_reach != test_sites[[i]]$site_reach[i])
}

# name of training site- means it excludes that (test) site
names(training_sites) <- names(test_sites)

# lastly, just want to get dates for predictions including initial
field_dates <- raw_data %>% select(site_reach, field_date)
field_dates <- rbind(field_dates, data.frame(site_reach = unique(field_dates$site_reach),
                                             field_date = rep("2023-09-24", 5)))
field_dates <- split(field_dates, field_dates$site_reach)

#### (3) Create empty tables for predictions ####

# empty dataframes for predictions (list of model types and then list within per reach)
model_names <- c("null", "physical", "chemical", "biological", "physicochemical",
                 "ecohydrological", "biochemical", "all")
predictions <- list() # empty list, excludes first day as we are not making predictions for that day
for(j in 1:length(model_names)) {
  # for each model (j) create a list for each reach (i)
  predictions[[j]] <- list(rep(NA, length(test_sites)))
  # create empty dataframe for predictions for each reach (i) for each model (j)
  # we are including initial day which will just be 0
  for(i in 1:length(test_sites)) {
    predictions[[j]][[i]] <- data.frame(field_date = field_dates[[i]]$field_date,
                                        mean = rep(NA, length(field_dates[[i]]$field_date)),
                                        ci_lower = rep(NA, length(field_dates[[i]]$field_date)), # 2.5%; lower bound of 95% interval
                                        ci_upper = rep(NA, length(field_dates[[i]]$field_date))) # 97.5%; upper bound of 95% interval
  }
  names(predictions[[j]]) <- names(test_sites)
}
names(predictions) <- model_names

# model names cheat sheet:
# null = average of all across time
# physical = autoregressive w/ temperature and flow
# chemical = autoregressive w/ nutrients (DIN & oPhos) and conductivity
# biological = autoregressive w/ GPP
# physicochemical = autoregressive w/ temperature, flow, nutrients, and conductivity
# ecohydrological = autoregressive w/ temperature, flow, and GPP
# biochemical = autoregressive w/ GPP, nutrients, and conductivity
# all = autoregressive w/ all covariates

#### (4) Predicting Microcoleus Cover ####

# get prediction functions
source("./code/supplemental_code/S3b_pred_functions.R")

## (a) null - mean of all cover data

# calculate mean to use for null model (this ignores first day which we are not predicting)
mean_cover <- mean(data$future_AC_cover_norm)

# add to predictions for each site and calculate NRMSE
for(i in 1:length(test_sites)) {
  predictions$null[[i]]$mean <- rep(mean_cover, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_lower <- rep(mean_cover, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_upper <- rep(mean_cover, nrow(predictions$null[[i]]))
}

# empty vector for null NMRSE
NRMSE <- c(rep(NA, length(test_sites)))

# calculate NRMSE 
for(i in 1:length(test_sites)) {
  # (removing first row of prediction which is first day that we are not predicting!)
  NRMSE[i] <- calc_NRMSE(predictions$null[[i]]$mean[-1], test_sites[[i]]$future_AC_cover_norm,
                         max(test_sites[[i]]$future_AC_cover_norm), min(test_sites[[i]]$future_AC_cover_norm))
}

# save null NMRSE
write.csv(NRMSE, "./data/predictive_models/process_uncertainty/AC_cover_models/NRMSE_vectors/null.csv", row.names = FALSE)

## (b) all others (putting data together and then run through big for loop)

# indexes cheat code:
# i = training/test sites
# j = model type according to predictions list (i.e. 1 = null, 2 = physical, 3 = chemical)

# empty list for covariates
covariates <- list()
covariates[[1]] <- NA # holder for null model

# make list of covariates for physical (temp + discharge)
covariates[[2]] <- make_covariates(c("temp_C", "discharge_m3_s"))

# make list of covariates for chemical (din + ophos + conductivity)
covariates[[3]] <- make_covariates(c("DIN_mg_N_L", "oPhos_ug_P_L", "cond_uS_cm"))

# make list of covariates for biological (GPP)
covariates[[4]] <- make_covariates(c("GPP_median_tofourdaysprior"))

# make list of covariates for physicochemical (temp + flow + din + ophos + cond)
covariates[[5]] <- make_covariates(c("temp_C", "discharge_m3_s",
                                     "DIN_mg_N_L", "oPhos_ug_P_L",
                                     "cond_uS_cm"))

# make list of covariates for ecohydrological (temp + disc + gpp)
covariates[[6]] <- make_covariates(c("temp_C", "discharge_m3_s",
                                     "GPP_median_tofourdaysprior"))

# make list of covariates for biochemical (din + ophos + cond + GPP)
covariates[[7]] <- make_covariates(c("DIN_mg_N_L", "oPhos_ug_P_L",
                                     "cond_uS_cm", "GPP_median_tofourdaysprior"))

# make list of covariates for all (temp + dis + din + ophos + cond + GPP)
covariates[[8]] <- make_covariates(c("temp_C", "discharge_m3_s", "DIN_mg_N_L", 
                                     "oPhos_ug_P_L", "cond_uS_cm", "GPP_median_tofourdaysprior"))

# giving list names for clarification
names(covariates) <- names(predictions)

# run through for loop to run all models; start with j for models
# start at 2 because we did null model separately
for(j in 2:length(predictions)) {
  
  # get model name string (for saving files)
  model_name <- names(covariates)[j]
  
  # build models and make predictions for each reach
  for(i in 1:length(training_sites)) {
    
    # NOTE: for this script, we are missing models that did not converge
    # to allow script to run uninterrupted, we will have to skip trying to load these files
    # (it's always SFE-M-1S which is i = 1)
    if((model_name == "biological" | model_name == "biochemical" | 
        model_name == "all" | model_name == "ecohydrological") & i == 1) {
      i = 2 # skip to next
    }
    
    # gather data
    mod_data = list(N = nrow(training_sites[[i]]),
                    c = ncol(covariates[[j]]$training[[i]]),
                    future = training_sites[[i]]$future_AC_cover_norm,
                    present = training_sites[[i]]$resp_AC_cover_norm,
                    covar = as.matrix(covariates[[j]]$training[[i]]))
    # read in model previously built (script 3c)
    model <- readRDS(paste("./data/predictive_models/AC_cover_models/", model_name, 
                                                "_", names(test_sites)[i], sep = ""))
    # extract parameters
    params <- rstan::extract(model, c("sigma", "b0", "b"))
    # make predictions matrix
    preds_matrix <- preds_cover_processuncertainty(params = params,
                                                   y = predictions[[j]][[i]],
                                                   covar = as.matrix(covariates[[j]]$testing[[i]]))
    # save summary of prediction; make sure to assign globally
    predictions[[j]][[i]][,2:4] <- preds_summary(preds_matrix)
    # save matrix (to compare standard deviations for uncertainty)
    write.csv(preds_matrix, paste("./data/predictive_models/process_uncertainty/AC_cover_models/pred_matrices/",
                                  model_name, "_", names(test_sites)[i], "_predsmatrix.csv", sep = ""))
    # calculate NRMSE of model
    NRMSE <- NRMSE_summary(preds_matrix, observed = test_sites[[i]]$future_AC_cover_norm)
    # save NRMSE vector
    write.csv(NRMSE, paste("./data/predictive_models/process_uncertainty/AC_cover_models/NRMSE_vectors/",
                           model_name, "_", names(test_sites)[i], "_NRMSE.csv", sep = ""), 
              row.names = FALSE)
  }
}

#### (5) Saving Prediction Summary ####

# adding site_reach and model name information to dataframe
for(j in 1:length(test_sites)) {
  for(i in 1:length(model_names)) {
    predictions[[i]][[j]]$model <- names(predictions)[i]
    predictions[[i]][[j]]$site_reach <- names(predictions[[i]])[j]
  }
}

# create empty vector for all sites
final_predictions <- data.frame()

# creating final predictions list
for(j in 1:length(test_sites)) {
  for(i in 1:length(model_names)) {
    final_predictions <- rbind(final_predictions, predictions[[i]][[j]])
  }
}

# saving final predictions
write.csv(final_predictions, "./data/predictive_models/process_uncertainty/predictions_AC_cover.csv",
          row.names = FALSE)
