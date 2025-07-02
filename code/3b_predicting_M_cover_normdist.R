#### models to predict cover (truncated norm version)
### Jordan Zabrecky
## last edited: 06.19.2025

# This script builds models to predict cover of benthic Microcoleus cover
# as determined by benthic cover surveys. Each model is built using 4 of 
# 5 reaches, then the model is tested using the withheld fifth reach

# looked at predictions 06.20.25-
# test to see how normal distribution compares to the truncated ver

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "truncnorm", "Metrics"), 
       require, character.only = T)

# loading in data- see note above
raw_data <- read.csv("./data/predictive_models/inputs.csv")

# because we are training one model with data from multiple sites,
# if we do y-1, when we iterate to the next, it will cross sites
# instead lets create autoregressive terms in the same dataframe
# and remove the first day
raw_data$prior_M_cover_norm <- c(NA, raw_data$resp_M_cover_norm[-nrow(raw_data)])
raw_data$prior_AC_cover_norm <- c(NA, raw_data$resp_AC_cover_norm[-nrow(raw_data)])
# may not do autoregressive for ATX, but just in case
raw_data$prior_M_atx_norm <- c(NA, raw_data$resp_M_atx_norm[-nrow(raw_data)])
raw_data$prior_AC_atx_norm <- c(NA, raw_data$resp_AC_atx_norm[-nrow(raw_data)])

# double-check that lines up as if prior is the previous value from resp on same row
check <- raw_data %>% 
  select(resp_M_cover_norm, prior_M_cover_norm)
view(check)

# remove first day at each reach (6/20/2023 and 6/25/2023 for STH only)
data <- raw_data[-1,] # removes first day at Standish Hickey
data <- data[-which(data$field_date == "2023-06-20"),] # removes all other first days

# double-check again that lines up
check <- data %>% 
  select(field_date, site_reach, resp_M_cover_norm, prior_M_cover_norm)
view(check)
# looks good, first day of sites removed

# double check there is no NA for STAN purposes
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

# lastly, just want to get dates including initial
field_dates <- raw_data %>% select(site_reach, field_date) 
field_dates <- split(field_dates, field_dates$site_reach)

#### (3) Create empty tables for predictions and nRMSE ####

# empty RMSE data frame
nrmse <- data.frame(site_reach = NA,
                    model = NA,
                    mean = NA,
                    ci_lower = NA,
                    ci_upper = NA)

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

#### (3.5) Helper function for predicting cover under normal distribution ####
# for predictions involving cover (i.e. includes an autoregressive term)
preds_cover_norm <- function(params, y, covar) {
  n.pred <- nrow(y) # includes initial day where we used 0
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rnorm(n = 1,
                               mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                 # covariance matrix is missing the first day
                                 # but predictions matrix isn't (hence j-1)
                                 covar[j-1,]%*%params$b[i,],
                               sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
  
}

#### (4) Predicting Microcoleus Cover ####

# get prediction functions
source("./code/supplemental_code/S3b_pred_functions.R")

## (a) null - mean of all cover data

# calculate mean (ignoring first day which are all zeros)
mean_cover <- sum(data$resp_M_cover_norm) / (nrow(data) - length(test_sites))

# add to predictions for each site and calculate nRMSE
for(i in 1:length(test_sites)) {
  predictions$null[[i]]$mean <- rep(mean_cover, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_lower <- rep(mean_cover, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_upper <- rep(mean_cover, nrow(predictions$null[[i]]))
}

# calculate nRMSE 
for(i in 1:length(test_sites)) {
  new <- data.frame(site_reach = names(test_sites)[i],
                    model = "null")
  new$mean <- calc_nRMSE(predictions$null[[i]]$mean[-1], test_sites[[i]]$resp_M_cover_norm,
                         max(test_sites[[i]]$resp_M_cover_norm), min(test_sites[[i]]$resp_M_cover_norm))
  new$ci_lower <- calc_nRMSE(predictions$null[[i]]$ci_lower[-1], test_sites[[i]]$resp_M_cover_norm,
                             max(test_sites[[i]]$resp_M_cover_norm), min(test_sites[[i]]$resp_M_cover_norm))
  new$ci_upper <- calc_nRMSE(predictions$null[[i]]$ci_upper[-1], test_sites[[i]]$resp_M_cover_norm,
                             max(test_sites[[i]]$resp_M_cover_norm), min(test_sites[[i]]$resp_M_cover_norm))
  nrmse <- rbind(nrmse, new)
}

## (b) physical (temp + discharge)

# empty list to save parameter estimates for each model
# and parameter r-hats for each model
physical_param_est <- data.frame(matrix(NA, nrow = 6, ncol = 5))
physical_rhats <- data.frame(matrix(NA, nrow = 6, ncol = 5))
colnames(physical_param_est) <- names(test_sites)
colnames(physical_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  covariates <- as.matrix(training_sites[[i]] %>% 
                            select(temp_C, discharge_m3_s))
  mod_data <- list(N = nrow(training_sites[[i]]),
                   c = ncol(covariates),
                   y = training_sites[[i]]$resp_M_cover_norm,
                   autoreg = training_sites[[i]]$prior_M_cover_norm,
                   covar = covariates)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/predicting_cover_normalnottruncated.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/normal_test/M_cover_models/physical_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
  # add mean parameter estimates to dataframe
  rownames(physical_param_est) <- rownames(get_posterior_mean(model))
  physical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(physical_rhats) <- names(summary(model)$summary[,"Rhat"])
  physical_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_cover_norm(params = params,
                              y = predictions$physical[[i]],
                              covar = as.matrix(test_sites[[i]] %>% 
                                                  select(temp_C, discharge_m3_s)))
  # save summary of predictions
  predictions$physical[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_cover_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "physical"))
}

# check all r-hats < 1.05 & save r-hats
any(physical_rhats > 1.05) # all above 1.05!
write.csv(physical_rhats,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/physical_rhats.csv",
          row.names = FALSE)

# looking at how parameter estimates change across all models
view(physical_param_est)
write.csv(physical_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/physical_param_est.csv",
          row.names = FALSE)

## (c) chemical (DIN + ophos + cond)

# empty list to save parameter estimates for each model
# and parameter r-hats for each model
chemical_param_est <- data.frame(matrix(NA, nrow = 7, ncol = 5))
chemical_rhats <- data.frame(matrix(NA, nrow = 7, ncol = 5))
colnames(chemical_param_est) <- names(test_sites)
colnames(chemical_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  covariates <- as.matrix(training_sites[[i]] %>% 
                            select(DIN_mg_N_L, oPhos_ug_P_L, cond_uS_cm))
  mod_data <- list(N = nrow(training_sites[[i]]),
                   c = ncol(covariates),
                   y = training_sites[[i]]$resp_M_cover_norm,
                   autoreg = training_sites[[i]]$prior_M_cover_norm,
                   covar = covariates)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/predicting_cover_normalnottruncated.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/normal_test/M_cover_models/chemical_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
  # add mean parameter estimates to dataframe
  rownames(chemical_param_est) <- rownames(get_posterior_mean(model))
  chemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(chemical_rhats) <- names(summary(model)$summary[,"Rhat"])
  chemical_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_cover_norm(params = params,
                              y = predictions$chemical[[i]],
                              covar = as.matrix(test_sites[[i]] %>% 
                                          select(DIN_mg_N_L, oPhos_ug_P_L, cond_uS_cm)))
  # save summary of predictions
  predictions$chemical[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_cover_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "chemical"))
}

# check all r-hats < 1.05 & save r-hats
any(chemical_rhats > 1.05) # all above 1.05!
write.csv(chemical_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/chemical_rhats.csv",
          row.names = FALSE)

# looking at how parameter estimates change across all models
view(chemical_param_est)
write.csv(chemical_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/chemical_param_est.csv",
          row.names = FALSE)

## (d) biological (GPP)

# empty list to save parameter estimates for each model
# and parameter r-hats for each model
biological_param_est <- data.frame(matrix(NA, nrow = 5, ncol = 5))
biological_rhats <- data.frame(matrix(NA, nrow = 5, ncol = 5))
colnames(biological_param_est) <- names(test_sites)
colnames(biological_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  covariates <- as.matrix(training_sites[[i]] %>% 
                            select(GPP_median_fourdaysprior))
  mod_data <- list(N = nrow(training_sites[[i]]),
                   c = ncol(covariates),
                   y = training_sites[[i]]$resp_M_cover_norm,
                   autoreg = training_sites[[i]]$prior_M_cover_norm,
                   covar = covariates)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/predicting_cover_normalnottruncated.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/normal_test/M_cover_models/biological_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
  # add mean parameter estimates to dataframe
  rownames(biological_param_est) <- rownames(get_posterior_mean(model))
  biological_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(biological_rhats) <- names(summary(model)$summary[,"Rhat"])
  biological_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_cover_norm(params = params,
                              y = predictions$biological[[i]],
                              covar = as.matrix(test_sites[[i]] %>% 
                                                  select(GPP_median_fourdaysprior)))
  # save summary of predictions
  predictions$biological[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_cover_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "biological"))
}

# check all r-hats < 1.05 & save r-hats
any(biological_rhats > 1.05) # all above 1.05!
write.csv(biological_rhats,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/biological_rhats.csv",
          row.names = FALSE)

# looking at how parameter estimates change across all models
view(biological_param_est)
write.csv(biological_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/biological_param_est.csv",
          row.names = FALSE)

## (e) physicochemical (temp + discharge + DIN + ophos + cond)

# empty list to save parameter estimates for each model
# and parameter r-hats for each model
physicochemical_param_est <- data.frame(matrix(NA, nrow = 9, ncol = 5))
physicochemical_rhats <- data.frame(matrix(NA, nrow = 9, ncol = 5))
colnames(physicochemical_param_est) <- names(test_sites)
colnames(physicochemical_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  covariates <- as.matrix(training_sites[[i]] %>% 
                            select(temp_C, discharge_m3_s, DIN_mg_N_L,
                                   oPhos_ug_P_L, cond_uS_cm))
  mod_data <- list(N = nrow(training_sites[[i]]),
                   c = ncol(covariates),
                   y = training_sites[[i]]$resp_M_cover_norm,
                   autoreg = training_sites[[i]]$prior_M_cover_norm,
                   covar = covariates)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/predicting_cover_normalnottruncated.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/normal_test/M_cover_models/physicochemical_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
  # add mean parameter estimates to dataframe
  rownames(physicochemical_param_est) <- rownames(get_posterior_mean(model))
  physicochemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(physicochemical_rhats) <- names(summary(model)$summary[,"Rhat"])
  physicochemical_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_cover_norm(params = params,
                              y = predictions$physicochemical[[i]],
                              covar = as.matrix(test_sites[[i]] %>% 
                                                  select(temp_C, discharge_m3_s,
                                                         DIN_mg_N_L, oPhos_ug_P_L,
                                                         cond_uS_cm)))
  # save summary of predictions
  predictions$physicochemical[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_cover_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "physicochemical"))
}

# check all r-hats < 1.05 & save r-hats
any(physicochemical_rhats > 1.05) # all above 1.05!
write.csv(physicochemical_rhats,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/physicochemical_rhats.csv",
          row.names = FALSE)

# looking at how parameter estimates change across all models
view(physicochemical_param_est)
write.csv(physicochemical_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/physicochemical_param_est.csv",
          row.names = FALSE)

## (f) ecohydrological (temp + discharge + GPP)

# empty list to save parameter estimates for each model
# and parameter r-hats for each model
ecohydrological_param_est <- data.frame(matrix(NA, nrow = 7, ncol = 5))
ecohydrological_rhats <- data.frame(matrix(NA, nrow = 7, ncol = 5))
colnames(ecohydrological_param_est) <- names(test_sites)
colnames(ecohydrological_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  covariates <- as.matrix(training_sites[[i]] %>% 
                            select(temp_C, discharge_m3_s,
                                   GPP_median_fourdaysprior))
  mod_data <- list(N = nrow(training_sites[[i]]),
                   c = ncol(covariates),
                   y = training_sites[[i]]$resp_M_cover_norm,
                   autoreg = training_sites[[i]]$prior_M_cover_norm,
                   covar = covariates)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/predicting_cover_normalnottruncated.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/normal_test/M_cover_models/ecohydrological_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
  # add mean parameter estimates to dataframe
  rownames(ecohydrological_param_est) <- rownames(get_posterior_mean(model))
  ecohydrological_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(ecohydrological_rhats) <- names(summary(model)$summary[,"Rhat"])
  ecohydrological_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_cover_norm(params = params,
                              y = predictions$ecohydrological[[i]],
                              covar = as.matrix(test_sites[[i]] %>% 
                                                  select(temp_C, discharge_m3_s,
                                                         GPP_median_fourdaysprior)))
  # save summary of predictions
  predictions$ecohydrological[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_cover_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "ecohydrological"))
}

# check all r-hats < 1.05 & save r-hats
any(ecohydrological_rhats > 1.05) # all above 1.05!
write.csv(ecohydrological_rhats,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/ecohydrological_rhats.csv",
          row.names = FALSE)

# looking at how parameter estimates change across all models
view(ecohydrological_param_est)
write.csv(ecohydrological_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/ecohydrological_param_est.csv",
          row.names = FALSE)

## (g) biochemical (DIN + ophos + cond + GPP)

# empty list to save parameter estimates for each model
# and parameter r-hats for each model
biochemical_param_est <- data.frame(matrix(NA, nrow = 8, ncol = 5))
biochemical_rhats <- data.frame(matrix(NA, nrow = 8, ncol = 5))
colnames(biochemical_param_est) <- names(test_sites)
colnames(biochemical_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  covariates <- as.matrix(training_sites[[i]] %>% 
                            select(DIN_mg_N_L, oPhos_ug_P_L, cond_uS_cm,
                                   GPP_median_fourdaysprior))
  mod_data <- list(N = nrow(training_sites[[i]]),
                   c = ncol(covariates),
                   y = training_sites[[i]]$resp_M_cover_norm,
                   autoreg = training_sites[[i]]$prior_M_cover_norm,
                   covar = covariates)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/predicting_cover_normalnottruncated.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/normal_test/M_cover_models/biochemical_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
  # add mean parameter estimates to dataframe
  rownames(biochemical_param_est) <- rownames(get_posterior_mean(model))
  biochemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(biochemical_rhats) <- names(summary(model)$summary[,"Rhat"])
  biochemical_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_cover_norm(params = params,
                              y = predictions$biochemical[[i]],
                              covar = as.matrix(test_sites[[i]] %>% 
                                                  select(DIN_mg_N_L, oPhos_ug_P_L,
                                                         cond_uS_cm,
                                                         GPP_median_fourdaysprior)))
  # save summary of predictions
  predictions$biochemical[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_cover_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "biochemical"))
}

# check all r-hats < 1.05 & save r-hats
any(biochemical_rhats > 1.05) # all above 1.05!
write.csv(biochemical_rhats,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/biochemical_rhats.csv",
          row.names = FALSE)

# looking at how parameter estimates change across all models
view(biochemical_param_est)
write.csv(biochemical_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/biochemical_rhats.csv",
          row.names = FALSE)

## (h) all (temp + discharge + DIN + ophos + cond + GPP)

# empty list to save parameter estimates for each model
# and parameter r-hats for each model
all_param_est <- data.frame(matrix(NA, nrow = 10, ncol = 5))
all_rhats <- data.frame(matrix(NA, nrow = 10, ncol = 5))
colnames(all_param_est) <- names(test_sites)
colnames(all_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  covariates <- as.matrix(training_sites[[i]] %>% 
                            select(temp_C, discharge_m3_s, DIN_mg_N_L, 
                                   oPhos_ug_P_L, cond_uS_cm,
                                   GPP_median_fourdaysprior))
  mod_data <- list(N = nrow(training_sites[[i]]),
                   c = ncol(covariates),
                   y = training_sites[[i]]$resp_M_cover_norm,
                   autoreg = training_sites[[i]]$prior_M_cover_norm,
                   covar = covariates)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/predicting_cover_normalnottruncated.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/normal_test/M_cover_models/all_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
  # add mean parameter estimates to dataframe
  rownames(all_param_est) <- rownames(get_posterior_mean(model))
  all_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(all_rhats) <- names(summary(model)$summary[,"Rhat"])
  all_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_cover_norm(params = params,
                              y = predictions$biochemical[[i]],
                              covar = as.matrix(test_sites[[i]] %>% 
                                                  select(temp_C, discharge_m3_s, 
                                                         DIN_mg_N_L, oPhos_ug_P_L,
                                                         cond_uS_cm,
                                                         GPP_median_fourdaysprior)))
  # save summary of predictions
  predictions$all[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_cover_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "all"))
}

# check all r-hats < 1.05 & save r-hats
any(all_rhats > 1.05) # all above 1.05!
write.csv(all_rhats,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/all_rhats.csv",
          row.names = FALSE)

# looking at how parameter estimates change across all models
view(all_param_est)
write.csv(all_param_est,
          "./data/predictive_models/normal_test/M_cover_models/model_attributes/all_param_est.csv",
          row.names = FALSE)

#### (5) Saving outputs ####

# saving nRMSE table
write.csv(nrmse %>% na.omit(), "./data/predictive_models/normal_test/nrmse_M_cover.csv",
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
write.csv(final_predictions, "./data/predictive_models/normal_test/predictions_M_cover.csv",
          row.names = FALSE)
