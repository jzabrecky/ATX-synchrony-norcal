#### models to predict cover
### Jordan Zabrecky
## last edited: 03.11.2025

# This script builds models to predict cover of benthic Microcoleus
# and Anabaena (separately) as determined by benthic cover surveys

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders"), require, character.only = T)

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
nrmse_M <- data.frame(model = c("null", "cover", "physical", "chemical", 
                                "biological"),
                      SFE_M_1S = rep(NA, 5),
                      SFE_M_2 = rep(NA, 5),
                      SFE_M_3 = rep(NA, 5),
                      SFE_M_4 = rep(NA, 5),
                      SFE_SH_1S = rep(NA, 5))
nrmse_A <- data.frame(model = c("null", "cover", "physical", "chemical", 
                                "biological"),
                      SFE_M_1S = rep(NA, 5),
                      SFE_M_2 = rep(NA, 5),
                      SFE_M_3 = rep(NA, 5),
                      SFE_M_4 = rep(NA, 5),
                      SFE_SH_1S = rep(NA, 5))

# model names cheat sheet:
# null = simple autoregressive
# cover = autoregressive w/ BC cover
# physical = autoregressive w/ BC cover + temperature and flow
# chemical = autoregressive w/ BC cover + physical + nutrients and conductivity
# biological = autoregressive w/ BC cover + physical and chemical + GPP

# empty dataframes for predictions with initial condition set
# NEED TO ADD IN 95% CONFIDENCE INTERVALS
predictions <- list()
for(i in 1:length(test_sites)) {
  n.row <- nrow(test_sites[[i]])
  predictions[[i]] <- data.frame(field_date = test_sites[[i]]$field_date,
                                 null = rep(NA, n.row),
                                 cover = rep(NA, n.row),
                                 physical = rep(NA, n.row),
                                 chemical = rep(NA, n.row),
                                 biological = rep(NA, n.row))
  # set initial condition for each prediction as 0 (on z-score scale)
  # aka we are assuming no cover on first day which is the actual case here
  # for standardized data, this is the minimum on each site-reach scale
  predictions[[i]][1,2:ncol(predictions[[i]])] <- rep(min(test_sites[[i]]$resp_M_cover_stnd,
                                                          ncol(predictions[[i]])))
}
names(predictions) <- names(test_sites)

# prediction function for null autoregressive model
# want to annotate this more
pred_out_null <- function(params, y) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rnorm(n = 1, mean = y[j-1], sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # need to include 95% confidence intervals
  return(y) # only returning mean prediction at the moment
}

# prediction function for cover model
# want to annotate this more
pred_out_cover <- function(params, cover, y) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rnorm(n = 1, mean = params$b0[i] + params$b1[i] * y[j-1] + 
                            params$b2[i] * cover[j], 
                            sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # need to include 95% confidence intervals
  return(y) # only returning mean prediction at the moment
}

# prediction function for physical model
pred_out_physical <- function(params, y, cover, dis, temp) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rnorm(n = 1, mean = params$b0[i] + params$b1[i] * y[j-1] +
                            params$b2[i] * cover[j] + params$b3[i] * dis[j] + 
                            params$b4 * temp[j], 
                            sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # need to include 95% confidence intervals
  return(y) # only returning mean prediction at the moment
}

# prediction function for chemical model
pred_out_chemical <- function(params, y, cover, dis, temp, 
                              nit, amm, ophos, cond) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rnorm(n = 1, mean = params$b0[i] + params$b1[i] * y[j-1] +
                              params$b2[i] * cover[j] + params$b3[i] * dis[j] + 
                              params$b4[i] * temp[j] + params$b5[i] * nit[j] + 
                              params$b6[i] * amm[j] + params$b7[i] * ophos[j] + 
                              params$b8[i] * cond[j], 
                            sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # need to include 95% confidence intervals
  return(y) # only returning mean prediction at the moment
}

# prediction function for biological model
pred_out_biological <- function(params, y, cover, dis, temp, 
                                nit, amm, ophos, cond, GPP) {
  n.pred <- length(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rnorm(n = 1, mean = params$b0[i] + params$b1[i] * y[j-1] + 
                              params$b2[i] * cover[j] + params$b3[i] * dis[j] + 
                              params$b4[i] * temp[j] + params$b5[i] * nit[j] + 
                              params$b6[i] * amm[j] + params$b7[i] * ophos[j] + 
                              params$b8[i] * cond[j] + params$b9[i] * GPP[j], 
                            sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y[j] <- mean(preds[,j-1])
  }
  
  # need to include 95% confidence intervals
  return(y)
}

# set working directory to where STAN files are located
setwd("./code/model_STAN_files")

#### (4) Predicting Microcoleus ATX (Standardized) ####

## (a) null autoregressive
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_atx_stnd)
  model <- stan(file = "standardized_null.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma"))
  preds <- pred_out_null(params, predictions[[i]]$null)
  # add predictions to final predictions dataframe
  predictions[[i]]$null <- preds
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[1,i+1] <- (sqrt(sum((preds[-1] - test_sites[[i]]$resp_M_cover_stnd[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$resp_M_cover_stnd - min(test_sites[[i]]$resp_M_cover_stnd)))
}

## (b) cover
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_atx_stnd,
                   cover = training_sites[[i]]$microcoleus)
  model <- stan(file = "standardized_atx_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2"))
  preds <- pred_out_cover(params, predictions[[i]]$cover,
                          cover = test_sites[[i]]$microcoleus)
  # add predictions to final predictions dataframe
  predictions[[i]]$cover <- preds
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[2,i+1] <- (sqrt(sum((preds[-1] - test_sites[[i]]$resp_M_cover_stnd[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$resp_M_cover_stnd - min(test_sites[[i]]$resp_M_cover_stnd)))
}

## (c) physical - want to save effect size also; maybe df at beginning of each run?
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_atx_stnd,
                   cover = training_sites[[i]]$microcoleus,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C)
  model <- stan(file = "standardized_atx_physical.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4"))
  preds <- pred_out_physical(params, predictions[[i]]$physical,
                             cover = test_sites[[i]]$microcoleus,
                             dis = test_sites[[i]]$discharge_m3_s,
                             temp = test_sites[[i]]$temp_C)
  # add predictions to final predictions dataframe
  predictions[[i]]$physical <- preds
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[3,i+1] <- (sqrt(sum((preds[-1] - test_sites[[i]]$resp_M_cover_stnd[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$resp_M_cover_stnd - min(test_sites[[i]]$resp_M_cover_stnd)))
}

## (d) chemical -- note: not converging atm
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_cover_stnd,
                   cover = training_sites[[i]]$microcoleus,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   nit = training_sites[[i]]$nitrate_mg_N_L,
                   amm = training_sites[[i]]$ammonium_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "standardized_atx_chemical.stan", 
                data = mod_data,
                chains = 3, iter = 4000, warmup = 3000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3",
                                    "b4", "b5", "b6", "b7", "b8"))
  preds <- pred_out_chemical(params, predictions[[i]]$chemical,
                             cover = test_sites[[i]]$microcoleus,
                             dis = test_sites[[i]]$discharge_m3_s,
                             temp = test_sites[[i]]$temp_C,
                             nit = test_sites[[i]]$nitrate_mg_N_L,
                             amm = test_sites[[i]]$ammonium_mg_N_L,
                             ophos = test_sites[[i]]$oPhos_ug_P_L,
                             cond = test_sites[[i]]$cond_uS_cm)
  # add predictions to final predictions dataframe
  predictions[[i]]$chemical <- preds
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[4,i+1] <- (sqrt(sum((preds[-1] - test_sites[[i]]$resp_M_cover_stnd[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$resp_M_cover_stnd - min(test_sites[[i]]$resp_M_cover_stnd)))
}

## (e) biological
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_M_cover_stnd,
                   cover = training_sites[[i]]$microcoleus,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   nit = training_sites[[i]]$nitrate_mg_N_L,
                   amm = training_sites[[i]]$ammonium_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "standardized_atx_biological.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3",
                                    "b4", "b5", "b6", "b7", "b8", "b9"))
  preds <- pred_out_biological(params, predictions[[i]]$biological,
                               cover = test_sites[[i]]$microcoleus,
                               dis = test_sites[[i]]$discharge_m3_s,
                               temp = test_sites[[i]]$temp_C,
                               nit = test_sites[[i]]$nitrate_mg_N_L,
                               amm = test_sites[[i]]$ammonium_mg_N_L,
                               ophos = test_sites[[i]]$oPhos_ug_P_L,
                               cond = test_sites[[i]]$cond_uS_cm,
                               GPP = test_sites[[i]]$GPP_median_fourdaysprior)
  # add predictions to predictions dataframe
  predictions[[i]]$biological <- preds
  # add in nrmse for model predictions on each test site (not including initial value)
  nrmse_M[5,i+1] <- (sqrt(sum((preds[-1] - test_sites[[i]]$resp_M_cover_stnd[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$resp_M_cover_stnd - min(test_sites[[i]]$resp_M_cover_stnd)))
}

## need to do other taxa obviously

#### (5) Saving Outputs ####
setwd("../..")
write.csv(nrmse_M, "./data/predictive_models/20250311_nrmse_M_atx.csv",
          row.names = FALSE)
