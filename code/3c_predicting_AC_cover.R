#### models to predict cover (truncated norm version)
### Jordan Zabrecky
## last edited: 04.29.2025

# This script builds models to predict cover of benthic Anabaena/Cylindrospermum cover
# as determined by benthic cover surveys

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "truncnorm", "Metrics"), 
       require, character.only = T)

# loading in data- see note above
data <- read.csv("./data/predictive_models/inputs.csv")

# first day of GPP is always NA because we put out sensors approximately that day
# we won't be using that day because it's only used for autoregressive cover term
# but STAN will freak out if we have NA even if it doesn't call that specific cell
# so, let's just fill it with the mean standardized GPP
data$GPP_median_fourdaysprior <- replace_na(data$GPP_median_fourdaysprior,
                                            mean(data$GPP_median_fourdaysprior, 
                                                 na.rm = TRUE))

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

#### (3) Empty tables  and functions for predictions and nRMSE ####

# empty RMSE data frame
nrmse <- data.frame(site_reach = NA,
                    model = NA,
                    mean = NA,
                    ci_lower = NA,
                    ci_upper = NA)

# empty dataframes for predictions (list of model types and then list within per reach)
model_names <- c("null", "physical", "chemical", "biological", "physicochemical",
                 "ecohydrological", "biochemical", "all")
predictions <- list() # empty list
for(j in 1:length(model_names)) {
  # for each model (j) create a list for each reach (i)
  predictions[[j]] <- list(rep(NA, length(test_sites)))
  # create empty dataframe for predictions for each reach (i) for each model (j)
  for(i in 1:length(test_sites)) {
    predictions[[j]][[i]] <- data.frame(field_date = test_sites[[i]]$field_date,
                                        mean = rep(NA, length(test_sites[[i]]$field_date)),
                                        ci_lower = rep(NA, length(test_sites[[i]]$field_date)), # 2.5%; lower bound of 95% interval
                                        ci_upper = rep(NA, length(test_sites[[i]]$field_date))) # 97.5%; upper bound of 95% interval
    # initialize first spot with 0's
    predictions[[j]][[i]]$mean[1] = 0
    predictions[[j]][[i]]$ci_lower[1] = 0
    predictions[[j]][[i]]$ci_upper[1] = 0
  }
  names(predictions[[j]]) <- names(test_sites)
}
names(predictions) <- c("null", "physical", "chemical", "biological", "physicochemical",
                        "ecohydrological", "biochemical", "all")

# model names cheat sheet:
# null = average of all across time
# physical = autoregressive w/ temperature and flow
# chemical = autoregressive w/ nutrients and conductivity
# biological = autoregressive w/ GPP
# physicochemical = autoregressive w/ temperature, flow, nutrients, and conductivity
# ecohydrological = autoregressive w/ temperature, flow, and GPP
# biochemical = autoregressive w/ GPP, nutrients, and conductivity
# all = autoregressive w/ all covariates

# function to calculate nRMSE for all sites
# takes in model name and predictions dataframe (which includes, in order mean, ci_lower, ci_upper)
calcnRMSE <- function(model, predictions) {
  
  # make new dataframe
  data <- data.frame(site_reach = names(predictions), # list of test sites
                     model = rep(model, length(names(predictions))), # model name
                     mean = rep(NA, length(names(predictions))),
                     ci_lower = rep(NA, length(names(predictions))),
                     ci_upper = rep(NA, length(names(predictions)))) # nRMSE mean
  
  # calculate nRMSE for each reach
  for(i in 1:length(names(predictions))) {
    # calculate nRMSE for each type of value
    for(j in 3:5) {
      
      # temporary values for nRMSE calculation
      observed <- test_sites[[i]]$resp_AC_cover_norm[-1]
      predicted <- predictions[[i]][[j-1]][-1] # minus one because no model name column
      max <- max(test_sites[[i]]$resp_AC_cover_norm[-1])
      min <- min(test_sites[[i]]$resp_AC_cover_norm[-1])
      
      # calculate nRMSE (we ignore 1st value as that was not calculated)
      data[i,j] <- rmse(observed, predicted) / (max - min)
    }
  }
  
  return(data)
}

#### (4) Predicting Anabaena/Cylindrospermum Cover ####

# get prediction functions
source("./code/supplemental_code/S3b_pred_functions.R")

## (a) null - mean of all cover data

# calculate mean (ignoring first day which are all zeros)
mean_cover <- sum(data$resp_AC_cover_norm) / (nrow(data) - length(test_sites))

# add to predictions for each site and calculate nRMSE
for(i in 1:length(test_sites)) {
  predictions$null[[i]]$mean <- rep(mean_cover, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_lower <- rep(mean_cover, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_upper <- rep(mean_cover, nrow(predictions$null[[i]]))
}

# calculate nRMSE using function
nrmse <- rbind(nrmse, calcnRMSE("null", predictions$null))

## (b) physical (discharge + temp)

# empty list to save parameter estimates for each model
physical_param_est <- data.frame(matrix(NA, nrow = 6, ncol = 5))
colnames(physical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C)
  model <- stan(file = "./code/model_STAN_files/normalized_physical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_cover_models/physical_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3"))
  rownames(physical_param_est) <- rownames(get_posterior_mean(model))
  physical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  predictions$physical[[i]] <- preds_physical(params, y = predictions$physical[[i]],
                                              dis = test_sites[[i]]$discharge_m3_s,
                                              temp = test_sites[[i]]$temp_C)
}

# add nrmse for model predictions to table
nrmse <- rbind(nrmse, calcnRMSE("physical", predictions$physical))

# looking at how parameter estimates change across all models
view(physical_param_est)
# ranges:
# b0 / intercept: 7.7, 9.6
# b1 / autoregressive: 0.35, 0.46
# b2 / discharge: -7.0, -5.1
# b3 / temperature: 7.1, 8.1
# sigma: 23.4, 25

## (c) chemical (DIN + ophos + cond)

# empty list to save parameter estimates for each model
chemical_param_est <- data.frame(matrix(NA, nrow = 7, ncol = 5))
colnames(chemical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "./code/model_STAN_files/normalized_chemical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_cover_models/chemical_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4"))
  rownames(chemical_param_est) <- rownames(get_posterior_mean(model))
  chemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  predictions$chemical[[i]] <- preds_chemical(params, y = predictions$chemical[[i]],
                                              din = test_sites[[i]]$DIN_mg_N_L,
                                              ophos = test_sites[[i]]$oPhos_ug_P_L,
                                              cond = test_sites[[i]]$cond_uS_cm)
}

# add nrmse for model predictions to table
nrmse <- rbind(nrmse, calcnRMSE("chemical", predictions$chemical))

# looking at how parameter estimates change across all models
view(chemical_param_est)
# ranges:
# b0 / intercept: 7.3, 9.4
# b1 / autoregressive: 0.37, 0.61
# b2 / DIN: -4.5, -1.2
# b3 / ophos: -7.9, -1.1
# b4 / cond: 0.2, 10.1
# sigma: 21.1, 23.8

## (c) biological (GPP)

# empty list to save parameter estimates for each model
biological_param_est <- data.frame(matrix(NA, nrow = 5, ncol = 5))
colnames(biological_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_biological.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_cover_models/biological_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2"))
  rownames(biological_param_est) <- rownames(get_posterior_mean(model))
  biological_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  predictions$biological[[i]] <- preds_biological(params, y = predictions$biological[[i]],
                                                  GPP = test_sites[[i]]$GPP_median_fourdaysprior)
}

# add nrmse for model predictions to table
nrmse <- rbind(nrmse, calcnRMSE("biological", predictions$biological))

# looking at how parameter estimates change across all models
view(biological_param_est)
# ranges:
# b0 / intercept: 7.5, 8.8
# b1 / autoregressive: 0.43, 0.55
# b2 / GPP: 3.1, 5.2
# sigma: 24.4, 25.9

## (d) physicochemical (discharge + temp + DIN + ophos + cond)

# empty list to save parameter estimates for each model
physicochemical_param_est <- data.frame(matrix(NA, nrow = 9, ncol = 5))
colnames(physicochemical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "./code/model_STAN_files/normalized_physicochemical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_cover_models/physicochemical_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5", "b6"))
  rownames(physicochemical_param_est) <- rownames(get_posterior_mean(model))
  physicochemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  predictions$physicochemical[[i]] <- preds_physicochemical(params, 
                                                            y = predictions$physicochemical[[i]],
                                                            dis = test_sites[[i]]$discharge_m3_s,
                                                            temp = test_sites[[i]]$temp_C,
                                                            din = test_sites[[i]]$DIN_mg_N_L,
                                                            ophos = test_sites[[i]]$oPhos_ug_P_L,
                                                            cond = test_sites[[i]]$cond_uS_cm)
}

# add nrmse for model predictions to table
nrmse <- rbind(nrmse, calcnRMSE("physicochemical", predictions$physicochemical))

# looking at how parameter estimates change across all models
view(physicochemical_param_est)
# ranges:
# b0 / intercept: 7.6, 10.1
# b1 / autoregressive: 0.30, 0.53
# b2 / discharge: -11.7, -4.8
# b3 / temperature: 5.4, 9.2
# b4 / DIN: 0.02, 6.1
# b5 / ophos: -7.3, 0.9
# b6 / cond: -8.9, 5.7
# sigma: 22.9, 25.0

## (e) ecohydrological (discharge + temp + GPP)

# empty list to save parameter estimates for each model
ecohydrological_param_est <- data.frame(matrix(NA, nrow = 7, ncol = 5))
colnames(ecohydrological_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_ecohydrological.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_cover_models/ecohydrological_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4"))
  rownames(ecohydrological_param_est) <- rownames(get_posterior_mean(model))
  ecohydrological_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  predictions$ecohydrological[[i]] <- preds_ecohydrological(params, 
                                                            y = predictions$ecohydrological[[i]],
                                                            dis = test_sites[[i]]$discharge_m3_s,
                                                            temp = test_sites[[i]]$temp_C,
                                                            GPP = test_sites[[i]]$GPP_median_fourdaysprior)
}

# add nrmse for model predictions to table
nrmse <- rbind(nrmse, calcnRMSE("ecohydrological", predictions$ecohydrological))

# looking at how parameter estimates change across all models
view(ecohydrological_param_est)
# ranges:
# b0 / intercept: 7.8, 9.5
# b1 / autoregressive: 0.35, 0.44
# b2 / discharge: -7.3, -5.1
# b3 / temperature: 6.3, 9.5
# b4 / GPP: -1.6, 3.7
# sigma: 23.5, 25.1

## (f) biochemical (DIN + ophos + cond + GPP)

# empty list to save parameter estimates for each model
biochemical_param_est <- data.frame(matrix(NA, nrow = 8, ncol = 5))
colnames(biochemical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_biochemical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_cover_models/biochemical_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5"))
  rownames(biochemical_param_est) <- rownames(get_posterior_mean(model))
  biochemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  predictions$biochemical[[i]] <- preds_biochemical(params, y = predictions$biochemical[[i]],
                                                    din = test_sites[[i]]$DIN_mg_N_L,
                                                    ophos = test_sites[[i]]$oPhos_ug_P_L,
                                                    cond = test_sites[[i]]$cond_uS_cm,
                                                    GPP = test_sites[[i]]$GPP_median_fourdaysprior)
}

# add nrmse for model predictions to table
nrmse <- rbind(nrmse, calcnRMSE("biochemical", predictions$biochemical))

# looking at how parameter estimates change across all models
view(biochemical_param_est)
# ranges:
# b0 / intercept: 7.1, 9.7
# b1 / autoregressive: 0.3, 0.6
# b2 / din: -4.6, -0.5
# b3 / ophos: -7.2, -0.8 
# b4 / cond: 1.06, 10.3
# b5 / GPP: -0.5, 4
# sigma: 24, 26

## (g) all (discharge + temp + DIN + ophos + cond + GPP)

# empty list to save parameter estimates for each model
all_param_est <- data.frame(matrix(NA, nrow = 10, ncol = 5))
colnames(all_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_all.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_cover_models/all_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7"))
  rownames(all_param_est) <- rownames(get_posterior_mean(model))
  all_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  predictions$all[[i]] <- preds_all(params, y = predictions$all[[i]],
                                    dis = test_sites[[i]]$discharge_m3_s,
                                    temp = test_sites[[i]]$temp_C,
                                    din = test_sites[[i]]$DIN_mg_N_L,
                                    ophos = test_sites[[i]]$oPhos_ug_P_L,
                                    cond = test_sites[[i]]$cond_uS_cm,
                                    GPP = test_sites[[i]]$GPP_median_fourdaysprior)
}

# add nrmse for model predictions to table
nrmse <- rbind(nrmse, calcnRMSE("all", predictions$all))

# looking at how parameter estimates change across all models
view(all_param_est)
# ranges:
# b0 / intercept: 7.6, 9.9
# b1 / autoregressive: 0.31, 0.56
# b2 / discharge: -11, -4.4
# b3 / temperature: 6.3, 11.6
# b4 / din: -0.45, 6.1
# b5 / ophos: -8.1, 0.6
# b6 / cond: -3.9, 5.4
# b7 / GPP: -3.7, -0.3
# sigma: 23.0, 25.0

#### (5) Saving outputs ####

# saving nRMSE table
write.csv(nrmse %>% na.omit(), "./data/predictive_models/nrmse_AC_cover.csv",
          row.names = FALSE)

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
write.csv(final_predictions, "./data/predictive_models/predictions_AC_cover.csv",
          row.names = FALSE)
