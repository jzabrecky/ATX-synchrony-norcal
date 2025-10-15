#### models to predict anatoxins for Microcoleus
### Jordan Zabrecky
## last edited: 10.14.2025

# This script builds models to predict benthic Microcoleus anatoxins
# as determined by composite Microcoleus sample anatoxin concentrations. 
# Each model is built using data from 4 of 5 reaches, then the model is tested 
# using the withheld fifth reach

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "truncnorm", "Metrics"), 
       require, character.only = T)

# loading in data and select only columns we care about
raw_data <- read.csv("./data/predictive_models/inputs.csv") %>% 
  select(field_date, site_reach, resp_M_atx_norm, future_M_atx_norm, temp_C,
         discharge_m3_s, DIN_mg_N_L, oPhos_ug_P_L, cond_uS_cm,
         GPP_median_tofourdaysprior, microcoleus)

# make another data frame for edited data
data <- raw_data

# lastly, double check there is no NA for STAN purposes
any(is.na(data)) # nope!

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
model_names <- c("null", "physical", "physical_w_cover", "chemical", "chemical_w_cover", 
                 "biological", "biological_w_cover", "physicochemical", "physicochemical_w_cover",
                 "ecohydrological", "ecohydrological_w_cover", "biochemical", "biochemical_w_cover", 
                 "all", "all_w_cover")
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
# w_cover includes the addition of cover at the end!!

#### (4) Predicting Microcoleus Mat Anatoxins ####

# get prediction functions
source("./code/supplemental_code/S3b_pred_functions.R")

## (a) null - mean of all Microcoleus atx data

# calculate mean to use for null model (this ignores first day which we are not predicting)
mean_atx <- mean(data$future_M_atx_norm)

# add to predictions for each site and calculate NRMSE
for(i in 1:length(test_sites)) {
  predictions$null[[i]]$mean <- rep(mean_atx, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_lower <- rep(mean_atx, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_upper <- rep(mean_atx, nrow(predictions$null[[i]]))
}

# empty vector for null NMRSE
NRMSE <- c(rep(NA, length(test_sites)))

# calculate NRMSE 
for(i in 1:length(test_sites)) {
  # (removing first row of prediction which is first day that we are not predicting!)
  NRMSE[i] <- calc_NRMSE(predictions$null[[i]]$mean[-1], test_sites[[i]]$future_M_atx_norm,
                         max(test_sites[[i]]$future_M_atx_norm), min(test_sites[[i]]$future_M_atx_norm))
}

# save null NMRSE
write.csv(NRMSE, "./data/predictive_models/M_atx_models/NRMSE_vectors/null.csv", row.names = FALSE)

## (b) all others (putting data together and then run through big for loop)

# indexes cheat code:
# i = training/test sites
# j = model type according to predictions list (i.e. 1 = null, 2 = physical, 3 = chemical)

# empty list for covariates
covariates <- list()
covariates[[1]] <- NA # holder for null model

# make list of covariates for physical (temp + discharge)
covariates[[2]] <- make_covariates(c("temp_C", "discharge_m3_s"))

# make list of covariates for physical w/cover (temp + discharge + cover)
covariates[[3]] <- make_covariates(c("temp_C", "discharge_m3_s", "microcoleus"))

# make list of covariates for chemical (din + ophos + conductivity)
covariates[[4]] <- make_covariates(c("DIN_mg_N_L", "oPhos_ug_P_L", "cond_uS_cm"))

# make list of covariates for chemical w/ cover (din + ophos + conductivity + cover)
covariates[[5]] <- make_covariates(c("DIN_mg_N_L", "oPhos_ug_P_L", "cond_uS_cm", "microcoleus"))

# make list of covariates for biological (GPP)
covariates[[6]] <- make_covariates(c("GPP_median_tofourdaysprior"))

# make list of covariates for biological (GPP) w/ cover
covariates[[7]] <- make_covariates(c("GPP_median_tofourdaysprior", "microcoleus"))

# make list of covariates for physicochemical (temp + flow + din + ophos + cond)
covariates[[8]] <- make_covariates(c("temp_C", "discharge_m3_s",
                                                "DIN_mg_N_L", "oPhos_ug_P_L",
                                                "cond_uS_cm"))

# make list of covariates for physicochemical w/ cover (temp + flow + din + ophos + cond + cover)
covariates[[9]] <- make_covariates(c("temp_C", "discharge_m3_s",
                                     "DIN_mg_N_L", "oPhos_ug_P_L",
                                     "cond_uS_cm", "microcoleus"))

# make list of covariates for ecohydrological (temp + disc + gpp)
covariates[[10]] <- make_covariates(c("temp_C", "discharge_m3_s",
                                                "GPP_median_tofourdaysprior"))

# make list of covariates for ecohydrological w/ cover (temp + disc + gpp + cover)
covariates[[11]] <- make_covariates(c("temp_C", "discharge_m3_s",
                                     "GPP_median_tofourdaysprior", "microcoleus"))

# make list of covariates for biochemical (din + ophos + cond + GPP)
covariates[[12]] <- make_covariates(c("DIN_mg_N_L", "oPhos_ug_P_L",
                                            "cond_uS_cm", "GPP_median_tofourdaysprior"))

# make list of covariates for biochemical w/ cover (din + ophos + cond + GPP + cover)
covariates[[13]] <- make_covariates(c("DIN_mg_N_L", "oPhos_ug_P_L",
                                     "cond_uS_cm", "GPP_median_tofourdaysprior",
                                     "microcoleus"))

# make list of covariates for all (temp + dis + din + ophos + cond + GPP)
covariates[[14]] <- make_covariates(c("temp_C", "discharge_m3_s", "DIN_mg_N_L", 
                                    "oPhos_ug_P_L", "cond_uS_cm", "GPP_median_tofourdaysprior"))


# make list of covariates for all w/ cover (temp + dis + din + ophos + cond + GPP + cover)
covariates[[15]] <- make_covariates(c("temp_C", "discharge_m3_s", "DIN_mg_N_L", 
                                     "oPhos_ug_P_L", "cond_uS_cm", "GPP_median_tofourdaysprior",
                                     "microcoleus"))

# giving list names for clarification
names(covariates) <- names(predictions)

# run through for loop to run all models; start with j for models
# start at 2 because we did null model separately
for(j in 2:length(predictions)) {
  
  # empty list to save parameter estimates for each model
  # and parameter r-hats for each model
  # number of covariates plus 3 (for b0, sigma, and lp)
  # ncol = 5 (hard-coded; number of reaches)
  param_est <- data.frame(matrix(NA, nrow = ncol(covariates[[j]]$training[[1]]) 
                                 # just using reach 1, all will have same number of columns 
                                 + 3, ncol = 5)) # plus 3 includes b0, sigma, & lp
  rhats <- data.frame(matrix(NA, nrow = ncol(covariates[[j]]$training[[1]]) 
                             + 3, ncol = 5))
  colnames(param_est) = names(test_sites)
  colnames(rhats) = names(test_sites)
  
  # get model name string (for saving files)
  model_name <- names(covariates)[j]
  
  # build models and make predictions for each reach
  for(i in 1:length(training_sites)) {
    # gather data
    mod_data = list(N = nrow(training_sites[[i]]),
                    c = ncol(covariates[[j]]$training[[i]]),
                    future = training_sites[[i]]$future_M_atx_norm,
                    covar = as.matrix(covariates[[j]]$training[[i]]))
    # run STAN model
    # if there are warning issues, code may stop if running through whole script 
    # (if using cntl+shift+enter)
    # model <- stan(file = "./code/model_STAN_files/predicting_NOT_autoregressive.stan", 
    #              data = mod_data,
    #              chains = 3, iter = 10000, warmup = 5000, 
    #              control = list(adapt_delta = 0.95, max_treedepth = 13))
    # # save STAN model
    # saveRDS(model, paste("./data/predictive_models/M_atx_models/", model_name, 
    #                      "_", names(test_sites)[i], sep = ""))
    # ALTERNATIVELY, option instead to read RDS object if model already built
    model <- readRDS(paste("./data/predictive_models/M_atx_models/", model_name, 
                           "_", names(test_sites)[i], sep = ""))
    # extract parameters
    params <- rstan::extract(model, c("sigma", "b0", "b"))
    # add mean parameter estimates to dataframe
    rownames(param_est) <- rownames(get_posterior_mean(model))
    param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
    # add r-hats to dataframe
    rownames(rhats) <- names(summary(model)$summary[,"Rhat"])
    rhats[i] <- summary(model)$summary[,"Rhat"]
    # make predictions matrix
    preds_matrix <- preds_anatoxins(params = params,
                               y = predictions[[j]][[i]],
                               covar = as.matrix(covariates[[j]]$testing[[i]]))
    # save summary of prediction; make sure to assign globally
    predictions[[j]][[i]][,2:4] <- preds_summary(preds_matrix)
    # calculate NRMSE of model
    NRMSE <- NRMSE_summary(preds_matrix, observed = test_sites[[i]]$future_M_atx_norm)
    # save NRMSE vector
    write.csv(NRMSE, paste("./data/predictive_models/M_atx_models/NRMSE_vectors/",
                           model_name, "_", names(test_sites)[i], "_NRMSE.csv", sep = ""), 
              row.names = FALSE)
  }
  
  # save rhats and mean parameter estimates
  write.csv(rhats, paste("./data/predictive_models/M_atx_models/model_attributes/",
                         model_name, "_rhats.csv", sep = ""), row.names = TRUE)
  write.csv(param_est,  paste("./data/predictive_models/M_atx_models/model_attributes/",
                             model_name, "_param_est.csv", sep = ""), row.names = TRUE)
  
  # lastly, print if all models converged <1.05 or not!
  if(any(rhats > 1.05)) {
    print(paste(model_name, " models did not converge :(", sep = ""))} else {
      print(paste(model_name, " models did converge :)", sep = ""))
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
write.csv(final_predictions, "./data/predictive_models/predictions_M_atx.csv",
          row.names = FALSE)
