#### models to predict cover (truncated norm version)
### Jordan Zabrecky
## last edited: 03.15.2025

# This script builds models to predict cover of benthic Microcoleus
# and Anabaena (separately) as determined by benthic cover surveys
# really failing on certain sites (all but the ideal ones)
# - wonder if we should include depth? see how those compare across sites...
# curious about reach-specific depth
# also curious if anabaena and GPP alone is the way because it peaks more synchronously
# I think we need to revisit hypotheses

### TO DO- list within a list for 95 percentiles
# eg predictions$chemical$site with 3 columns

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "truncnorm"), 
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

#### (3) Functions for predictions and starting RMSE table ####

# cover RMSE table: one for Microcoleus (M) and one for Anabaena (A)
# also will need one for standardized and one for normalized
nrmse_M <- data.frame(model = c("null", "physical", "chemical", "biological"),
                      SFE_M_1S = rep(NA, 4),
                      SFE_M_2 = rep(NA, 4),
                      SFE_M_3 = rep(NA, 4),
                      SFE_M_4 = rep(NA, 4),
                      SFE_SH_1S = rep(NA, 4))
nrmse_A <- data.frame(model = c("null", "physical", "chemical", "biological"),
                      SFE_M_1S = rep(NA, 4),
                      SFE_M_2 = rep(NA, 4),
                      SFE_M_3 = rep(NA, 4),
                      SFE_M_4 = rep(NA, 4),
                      SFE_SH_1S = rep(NA, 4))

# empty dataframes for predictions with initial condition set
# NEED TO ADD IN 95% CONFIDENCE INTERVALS -- add additional columns?
predictions <- list()
models <- c("physical", "chemical", "biological") # list within a list?
for(i in 1:length(test_sites)) {
  n.row <- nrow(test_sites[[i]])
  predictions[[i]] <- data.frame(field_date = test_sites[[i]]$field_date,
                                 null = rep(NA, n.row),
                                 physical = rep(NA, n.row),
                                 chemical = rep(NA, n.row),
                                 biological = rep(NA, n.row))
  # set initial condition for each prediction as 0 (on z-score scale)
  # aka we are assuming no cover on first day which is the actual case here
  # for standardized data, this is the minimum on each site-reach scale
  predictions[[i]][1,2:ncol(predictions[[i]])] <- rep(0, ncol(predictions[[i]])-1)
}
names(predictions) <- names(test_sites)

# model names cheat sheet:
# null = simple autoregressive
# physical = autoregressive w/ temperature and flow
# chemical = autoregressive w/ physical + nutrients and conductivity
# biological = autoregressive w/ physical and chemical + GPP

## insert functions ##

# make function to calculate RMSE?
# function arguments: type (micro/ana), i, model name

# prediction function for physical model
pred_out_physical <- function(params, y, dis, temp) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y[j-1] +
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # return mean and 95% confidence intervals
  final <- data.frame(mean = y[-1], # remove first prediction which preset 0
                      ci_lower = apply(preds, 2, quantile, prob = 0.025),
                      ci_upper = apply(preds, 2, quantile, prob = 0.975))
  return(final)
}

# prediction function for chemical model
pred_out_chemical <- function(params, y, dis, temp, 
                              nit, amm, ophos, cond) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y[j-1] + 
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j] +
                                   params$b4[i] * nit[j] + params$b5[i] * amm[j] +
                                   params$b6[i] * ophos[j] + params$b7[i] * cond[j], 
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # return mean and 95% confidence intervals
  final <- data.frame(mean = y[-1], # remove first prediction which preset 0
                      ci_lower = apply(preds, 2, quantile, prob = 0.025),
                      ci_upper = apply(preds, 2, quantile, prob = 0.975))
  return(final)
}

# prediction function for biological model
pred_out_biological <- function(params, y, dis, temp, 
                                nit, amm, ophos, cond, GPP) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y[j-1] + 
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j] +
                                   params$b4[i] * nit[j] + params$b5[i] * amm[j] +
                                   params$b6[i] * ophos[j] + params$b7[i] * cond[j] +
                                   params$b8[i] * GPP[j], 
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # return mean and 95% confidence intervals
  final <- data.frame(mean = y[-1], # remove first prediction which preset 0
                      ci_lower = apply(preds, 2, quantile, prob = 0.025),
                      ci_upper = apply(preds, 2, quantile, prob = 0.975))
  return(final)
}

### IF WE USE PRIORS NEED TO USE SEPARATE MODELS

# set working directory to where STAN files are located
setwd("./code/model_STAN_files")

#### (4) Predicting Microcoleus Cover (Standardized) ####

## (a) null - mean of all cover data

# calculate mean
mean_m_cover <- mean(data$resp_M_cover_norm)

# add to predictions for each site and calculate nRMSE
for(i in 1:length(predictions)) {
  predictions[[i]]$null <- rep(mean_m_cover, nrow(predictions[[i]]))
  nrmse_M[1,i+1] <- sqrt(sum((predictions[[i]]$null[-1] - test_sites[[i]]$resp_M_cover_norm[-1])^2)
                         / length(predictions[[i]]$null[-1])) /
    (max(test_sites[[i]]$resp_M_cover_norm) - min(test_sites[[i]]$resp_M_cover_norm))
}

# calculate RMSE with mean for each site

## (b) physical - want to save effect size also; maybe df at beginning of each run?
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C)
  model <- stan(file = "normalized_cover_physical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3"))
  preds <- pred_out_physical(params, y = predictions[[i]]$physical,
                             dis = test_sites[[i]]$discharge_m3_s,
                             temp = test_sites[[i]]$temp_C)
  # add predictions to final predictions dataframe
  predictions[[i]]$physical[-1] <- preds$mean
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[2,i+1] <- sqrt(sum((predictions[[i]]$physical[-1] - test_sites[[i]]$resp_M_cover_norm[-1])^2)
                         / length(predictions[[i]]$physical[-1])) /
    (max(test_sites[[i]]$resp_M_cover_norm) - min(test_sites[[i]]$resp_M_cover_norm))
}

## (c) chemical
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   nit = training_sites[[i]]$nitrate_mg_N_L,
                   amm = training_sites[[i]]$ammonium_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "normalized_cover_chemical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3",
                                    "b4", "b5", "b6", "b7"))
  preds <- pred_out_chemical(params, predictions[[i]]$chemical,
                             dis = test_sites[[i]]$discharge_m3_s,
                             temp = test_sites[[i]]$temp_C,
                             nit = test_sites[[i]]$nitrate_mg_N_L,
                             amm = test_sites[[i]]$ammonium_mg_N_L,
                             ophos = test_sites[[i]]$oPhos_ug_P_L,
                             cond = test_sites[[i]]$cond_uS_cm)
  # add predictions to final predictions dataframe
  predictions[[i]]$chemical[-1] <- preds$mean
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[3,i+1] <- sqrt(sum((predictions[[i]]$chemical[-1] - test_sites[[i]]$resp_M_cover_norm[-1])^2)
                         / length(predictions[[i]]$chemical[-1])) /
    (max(test_sites[[i]]$resp_M_cover_norm) - min(test_sites[[i]]$resp_M_cover_norm))
}

## (d) biological
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   nit = training_sites[[i]]$nitrate_mg_N_L,
                   amm = training_sites[[i]]$ammonium_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "normalized_cover_biological.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3",
                                    "b4", "b5", "b6", "b7", "b8"))
  preds <- pred_out_biological(params, predictions[[i]]$biological,
                               dis = test_sites[[i]]$discharge_m3_s,
                               temp = test_sites[[i]]$temp_C,
                               nit = test_sites[[i]]$nitrate_mg_N_L,
                               amm = test_sites[[i]]$ammonium_mg_N_L,
                               ophos = test_sites[[i]]$oPhos_ug_P_L,
                               cond = test_sites[[i]]$cond_uS_cm,
                               GPP = test_sites[[i]]$GPP_median_fourdaysprior)
  # add predictions to predictions dataframe
  predictions[[i]]$biological[-1] <- preds$mean
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[4,i+1] <- sqrt(sum((predictions[[i]]$biological[-1] - test_sites[[i]]$resp_M_cover_norm[-1])^2)
                         / length(predictions[[i]]$biological[-1])) /
    (max(test_sites[[i]]$resp_M_cover_norm) - min(test_sites[[i]]$resp_M_cover_norm))
}

#### (4b) Anabaena ####

## (a) null - mean of all cover data

# calculate mean
mean_a_cover <- mean(data$resp_AC_cover_norm)

# add to predictions for each site and calculate nRMSE
for(i in 1:length(predictions)) {
  predictions[[i]]$null <- rep(mean_ac_cover, nrow(predictions[[i]]))
  nrmse_A[1,i+1] <- sqrt(sum((predictions[[i]]$null[-1] - test_sites[[i]]$resp_AC_cover_norm[-1])^2)
                         / length(predictions[[i]]$null[-1])) /
    (max(test_sites[[i]]$resp_AC_cover_norm) - min(test_sites[[i]]$resp_AC_cover_norm))
}

# calculate RMSE with mean for each site

## (b) physical - want to save effect size also; maybe df at beginning of each run?
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C)
  model <- stan(file = "normalized_cover_physical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3"))
  preds <- pred_out_physical(params, y = predictions[[i]]$physical,
                             dis = test_sites[[i]]$discharge_m3_s,
                             temp = test_sites[[i]]$temp_C)
  # add predictions to final predictions dataframe
  predictions[[i]]$physical[-1] <- preds$mean
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_A[2,i+1] <- sqrt(sum((predictions[[i]]$physical[-1] - test_sites[[i]]$resp_AC_cover_norm[-1])^2)
                         / length(predictions[[i]]$physical[-1])) /
    (max(test_sites[[i]]$resp_AC_cover_norm) - min(test_sites[[i]]$resp_AC_cover_norm))
}

## (c) chemical
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   nit = training_sites[[i]]$nitrate_mg_N_L,
                   amm = training_sites[[i]]$ammonium_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "normalized_cover_chemical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3",
                                    "b4", "b5", "b6", "b7"))
  preds <- pred_out_chemical(params, predictions[[i]]$chemical,
                             dis = test_sites[[i]]$discharge_m3_s,
                             temp = test_sites[[i]]$temp_C,
                             nit = test_sites[[i]]$nitrate_mg_N_L,
                             amm = test_sites[[i]]$ammonium_mg_N_L,
                             ophos = test_sites[[i]]$oPhos_ug_P_L,
                             cond = test_sites[[i]]$cond_uS_cm)
  # add predictions to final predictions dataframe
  predictions[[i]]$chemical[-1] <- preds$mean
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_A[3,i+1] <- sqrt(sum((predictions[[i]]$chemical[-1] - test_sites[[i]]$resp_AC_cover_norm[-1])^2)
                         / length(predictions[[i]]$chemical[-1])) /
    (max(test_sites[[i]]$resp_AC_cover_norm) - min(test_sites[[i]]$resp_AC_cover_norm))
}

## (d) biological
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_cover_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   nit = training_sites[[i]]$nitrate_mg_N_L,
                   amm = training_sites[[i]]$ammonium_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "normalized_cover_biological.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3",
                                    "b4", "b5", "b6", "b7", "b8"))
  preds <- pred_out_biological(params, predictions[[i]]$biological,
                               dis = test_sites[[i]]$discharge_m3_s,
                               temp = test_sites[[i]]$temp_C,
                               nit = test_sites[[i]]$nitrate_mg_N_L,
                               amm = test_sites[[i]]$ammonium_mg_N_L,
                               ophos = test_sites[[i]]$oPhos_ug_P_L,
                               cond = test_sites[[i]]$cond_uS_cm,
                               GPP = test_sites[[i]]$GPP_median_fourdaysprior)
  # add predictions to predictions dataframe
  predictions[[i]]$biological[-1] <- preds$mean
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_A[4,i+1] <- sqrt(sum((predictions[[i]]$biological[-1] - test_sites[[i]]$resp_AC_cover_norm[-1])^2)
                         / length(predictions[[i]]$biological[-1])) /
    (max(test_sites[[i]]$resp_AC_cover_norm) - min(test_sites[[i]]$resp_AC_cover_norm))
}

#### (5) Saving Outputs ####
setwd("../..")
write.csv(nrmse_A, "./data/predictive_models/20250311_nrmse_A_cover_truncated.csv",
          row.names = FALSE)
