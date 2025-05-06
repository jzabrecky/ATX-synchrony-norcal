#### models to predict anatoxins (truncated norm version)
### Jordan Zabrecky
## last edited: 05.05.2025

# This script builds models to predict anatoxin concentrations of Anabaena/Cylindrospermum 
# mats using anabaena_cylindrospermum cover as a covariate

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
model_names <- c("null", "w_cover", "physical_w_cover", "chemical_w_cover", "biological_w_cover", 
                 "physicochemical_w_cover", "ecohydrological_w_cover", "biochemical_w_cover",
                 "all_w_cover")
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
  }
  names(predictions[[j]]) <- names(test_sites)
}
names(predictions) <- model_names

# model names cheat sheet:
# null = average of all across time
# w/ cover = autoregressive w/ cover
# physical w/ cover = autoregressive w/ cover + temperature and flow
# chemical w/ cover = autoregressive w/ cover + nutrients and conductivity
# biological w/ cover = autoregressive w/ cover + GPP
# physicochemical w/ cover = autoregressive w/ cover + temperature, flow, nutrients, and conductivity
# ecohydrological w/ cover = autoregressive w/ cover + temperature, flow, and GPP
# biochemical w/ cover = autoregressive w/ cover + GPP, nutrients, and conductivity
# all w/ cover = autoregressive w/ cover + all covariates

# function to calculate mean & 95% confidence interval of predictions from prediction matrix
preds_summary <- function(preds_matrix) {
  
  # creating dataframe of mean and 95% confidence interval
  y <- data.frame(mean = rep(NA, ncol(preds_matrix)))
  y$mean <- apply(preds_matrix, 2, mean)
  y$ci_lower <- apply(preds_matrix, 2, function(x) quantile(x, prob = 0.025))
  y$ci_upper <- apply(preds_matrix, 2, function(x) quantile(x, prob = 0.975))
  
  return(y)
}

# function to calculate nRMSE with predicted and observed vectors
calc_nRMSE <- function(predicted, observed, max, min) {
  nRMSE <- rmse(observed, predicted) / (max - min)
}

# calculate mean and 95% confidence interval nRMSE's
nRMSE_summary <- function(preds_matrix, observed, site_reach_name, model_name) {
  
  # get number of predictions and max and min of observed values (should be 0 & 100)
  n.pred <- length(observed)
  max <- max(observed[-1])
  min <- min(observed[-1])
  
  # make matrix for RMSE values
  nRMSE_matrix <- matrix(data = NA, nrow = nrow(preds_matrix), ncol = ncol(preds_matrix))
  
  # fill in nRMSE for each predicted value in the matrix
  for(j in 2:n.pred){
    for(i in 1:length(params$sigma)) {
      nRMSE_matrix[i,j-1] <- calc_nRMSE(observed[j], preds_matrix[i,j-1], max, min)
    }
  }
  
  # creating dataframe for nRMSE 
  nRMSE <- data.frame(site_reach = site_reach_name,
                      model = model_name)
  nRMSE$mean <- mean(nRMSE_matrix)
  nRMSE$ci_lower <- quantile(nRMSE_matrix, prob = 0.025)
  nRMSE$ci_upper <- quantile(nRMSE_matrix, prob = 0.975)
  
  return(nRMSE)
}

#### (4) Predicting anabaena_cylindrospermum anatoxins ####

# get prediction functions
source("./code/supplemental_code/S3b_pred_functions.R")

## (a) null - mean of all cover data (repeat of what we did before but for plotting)

# calculate mean (ignoring first day which are all zeros)
mean_atx <- sum(data$resp_AC_atx_norm) / (nrow(data) - length(test_sites))

# add to predictions for each site and calculate nRMSE
for(i in 1:length(test_sites)) {
  predictions$null[[i]]$mean <- rep(mean_atx, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_lower <- rep(mean_atx, nrow(predictions$null[[i]]))
  predictions$null[[i]]$ci_upper <- rep(mean_atx, nrow(predictions$null[[i]]))
}

# calculate nRMSE 
for(i in 1:length(test_sites)) {
  new <- data.frame(site_reach = names(test_sites)[i],
                    model = "null")
  new$mean <- calc_nRMSE(predictions$null[[i]]$mean[-1], test_sites[[i]]$resp_AC_atx_norm[-1],
                         max(test_sites[[i]]$resp_AC_atx_norm[-1]), min(test_sites[[i]]$resp_AC_atx_norm[-1]))
  new$ci_lower <- calc_nRMSE(predictions$null[[i]]$ci_lower[-1], test_sites[[i]]$resp_AC_atx_norm[-1],
                             max(test_sites[[i]]$resp_AC_atx_norm[-1]), min(test_sites[[i]]$resp_AC_atx_norm[-1]))
  new$ci_upper <- calc_nRMSE(predictions$null[[i]]$ci_upper[-1], test_sites[[i]]$resp_AC_atx_norm[-1],
                             max(test_sites[[i]]$resp_AC_atx_norm[-1]), min(test_sites[[i]]$resp_AC_atx_norm[-1]))
  nrmse <- rbind(nrmse, new)
}

## (b) cover only (cover)

# empty list to save parameter estimates for each model
cover_param_est <- data.frame(matrix(NA, nrow = 5, ncol = 5))
colnames(cover_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum)
  model <- stan(file = "./code/model_STAN_files/normalized_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2"))
  rownames(cover_param_est) <- rownames(get_posterior_mean(model))
  cover_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_w_cover(params, y = predictions$physical[[i]],
                                cover = test_sites[[i]]$resp_M_cover_stnd)
  predictions$w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$anabaena_cylindrospermum,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "w_cover")) # calculate nRMSE
}

# looking at how parameter estimates change across all models
view(cover_param_est)
# ranges:
# b0 / intercept: 7.9, 9.1
# b1 / autoregressive: 0.38, 0.44
# b2 / cover: 14.1, 20.3 (yay!)
# sigma: 18.8, 23.1

## (c) physical (discharge + temp)

# empty list to save parameter estimates for each model
physical_param_est <- data.frame(matrix(NA, nrow = 7, ncol = 5))
colnames(physical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C)
  model <- stan(file = "./code/model_STAN_files/normalized_physical_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/physical_w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4"))
  rownames(physical_param_est) <- rownames(get_posterior_mean(model))
  physical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_physical_w_cover(params, y = predictions$physical[[i]],
                                         cover = test_sites[[i]]$resp_M_cover_stnd,
                                         dis = test_sites[[i]]$discharge_m3_s,
                                         temp = test_sites[[i]]$temp_C)
  predictions$physical_w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$anabaena_cylindrospermum,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "physical_w_cover")) # calculate nRMSE
}

# looking at how parameter estimates change across all models
view(physical_param_est)
# ranges:
# b0 / intercept: 8.3, 9.6
# b1 / autoregressive: 0.34, 0.41
# b2 / cover: 11.1, 18.9
# b3 / discharge: -2.5, 0.5
# b4 / temperature: 3.7, 9.6
# sigma: 18.8, 22.3

## (d) chemical (DIN + ophos + cond)

# empty list to save parameter estimates for each model
chemical_param_est <- data.frame(matrix(NA, nrow = 8, ncol = 5))
colnames(chemical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "./code/model_STAN_files/normalized_chemical_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/chemical_w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5"))
  rownames(chemical_param_est) <- rownames(get_posterior_mean(model))
  chemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_chemical_w_cover(params, y = predictions$chemical[[i]],
                                         cover = test_sites[[i]]$anabaena_cylindrospermum,
                                         din = test_sites[[i]]$DIN_mg_N_L,
                                         ophos = test_sites[[i]]$oPhos_ug_P_L,
                                         cond = test_sites[[i]]$cond_uS_cm)
  predictions$chemical_w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$resp_AC_atx_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "chemical_w_cover")) # calculate nRMSE
}

# looking at how parameter estimates change across all models
view(chemical_param_est)
# ranges:
# b0 / intercept: 8.3, 10.1
# b1 / autoregressive: 0.36, 0.40
# b2 / cover: 11.6, 19.4
# b3 / DIN: -4.8, 2.5
# b4 / ophos: -12.1, -3.7
# b5 / cond: 2.5, 10.9
# sigma: 18.9, 22.5

## (e) biological (GPP)

# empty list to save parameter estimates for each model
biological_param_est <- data.frame(matrix(NA, nrow = 6, ncol = 5))
colnames(biological_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_biological_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/biological_w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3"))
  rownames(biological_param_est) <- rownames(get_posterior_mean(model))
  biological_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_biological_w_cover(params, y = predictions$biological[[i]],
                                           cover = test_sites[[i]]$anabaena_cylindrospermum,
                                           GPP = test_sites[[i]]$GPP_median_fourdaysprior)
  predictions$biological_w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$resp_AC_atx_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "biological_w_cover")) # calculate nRMSE
}

# looking at how parameter estimates change across all models
view(biological_param_est)
# ranges:
# b0 / intercept: 8.9, 9.9
# b1 / autoregressive: 0.35, 0.40
# b2 / cover: 13.6, 19.6
# b2 / GPP: 2.1, 7.2
# sigma: 18.9, 22.5

## (f) physicochemical (discharge + temp + DIN + ophos + cond)

# empty list to save parameter estimates for each model
physicochemical_param_est <- data.frame(matrix(NA, nrow = 10, ncol = 5))
colnames(physicochemical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "./code/model_STAN_files/normalized_physicochemical_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/physicochemical_w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7"))
  rownames(physicochemical_param_est) <- rownames(get_posterior_mean(model))
  physicochemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_physicochemical_w_cover(params, y = predictions$physicochemical[[i]],
                                                cover = test_sites[[i]]$anabaena_cylindrospermum,
                                                dis = test_sites[[i]]$discharge_m3_s,
                                                temp = test_sites[[i]]$temp_C,
                                                din = test_sites[[i]]$DIN_mg_N_L,
                                                ophos = test_sites[[i]]$oPhos_ug_P_L,
                                                cond = test_sites[[i]]$cond_uS_cm)
  predictions$physicochemical_w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$resp_AC_atx_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "physicochemical_w_cover")) # calculate nRMSE
}

# looking at how parameter estimates change across all models
view(physicochemical_param_est)
# ranges:
# b0 / intercept: 9.6, 11
# b1 / autoregressive: 0.24, 0.31
# b2 / cover: 8.1, 17.4
# b3 / discharge: -0.5, 6.3
# b4 / temperature: 6.7, 12.8
# b5 / DIN: -2.78, 7.9
# b6 / ophos: -14.2, -4.2
# b7 / cond: 8.1, 20.4
# sigma: 18.5, 21.3

## (g) ecohydrological (discharge + temp + GPP)

# empty list to save parameter estimates for each model
ecohydrological_param_est <- data.frame(matrix(NA, nrow = 8, ncol = 5))
colnames(ecohydrological_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_ecohydrological_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/ecohydrological_w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5"))
  rownames(ecohydrological_param_est) <- rownames(get_posterior_mean(model))
  ecohydrological_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_ecohydrological_w_cover(params, y = predictions$ecohydrological[[i]],
                                                cover = test_sites[[i]]$anabaena_cylindrospermum,
                                                dis = test_sites[[i]]$discharge_m3_s,
                                                temp = test_sites[[i]]$temp_C,
                                                GPP = test_sites[[i]]$GPP_median_fourdaysprior)
  predictions$ecohydrological_w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$resp_AC_atx_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "ecohydrological_w_cover")) # calculate nRMSE
}

# looking at how parameter estimates change across all models
view(ecohydrological_param_est)
# ranges:
# b0 / intercept: 8.6, 9.6
# b1 / autoregressive: 0.33, 0.42
# b2 / cover: 11.2, 19.1
# b3 / discharge: -3.2, 0.98
# b4 / temperature: 4.4, 9.5
# b5 / GPP: -1.2, 4.9
# sigma: 19, 22.5

## (h) biochemical (DIN + ophos + cond + GPP)

# empty list to save parameter estimates for each model
biochemical_param_est <- data.frame(matrix(NA, nrow = 9, ncol = 5))
colnames(biochemical_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_biochemical_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/biochemical_w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5", "b6"))
  rownames(biochemical_param_est) <- rownames(get_posterior_mean(model))
  biochemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_biochemical_w_cover(params, y = predictions$biochemical[[i]],
                                            cover = test_sites[[i]]$anabaena_cylindrospermum,
                                            din = test_sites[[i]]$DIN_mg_N_L,
                                            ophos = test_sites[[i]]$oPhos_ug_P_L,
                                            cond = test_sites[[i]]$cond_uS_cm,
                                            GPP = test_sites[[i]]$GPP_median_fourdaysprior)
  predictions$biochemical_w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$resp_AC_atx_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "biochemical_w_cover")) # calculate nRMSE
}

# looking at how parameter estimates change across all models
view(biochemical_param_est)
# ranges:
# b0 / intercept: 11.1, 13.1
# b1 / autoregressive: 0.06, 0.28
# b2 / cover: 10.9, 18.1
# b3 / din: -1.8, 3.9
# b4 / ophos: -10.2, -1.4
# b5 / cond: 5.1, 19.5
# b6 / GPP: 6.3, 14.0
# sigma: 18.8, 22.3

## (i) all (discharge + temp + DIN + ophos + cond + GPP)

# empty list to save parameter estimates for each model
all_param_est <- data.frame(matrix(NA, nrow = 11, ncol = 5))
colnames(all_param_est) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   y = training_sites[[i]]$resp_AC_atx_norm,
                   cover = training_sites[[i]]$anabaena_cylindrospermum,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm,
                   GPP = training_sites[[i]]$GPP_median_fourdaysprior)
  model <- stan(file = "./code/model_STAN_files/normalized_all_w_cover.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  saveRDS(model, paste("./data/predictive_models/AC_atx_models/all_w_cover_", 
                       names(test_sites)[i], sep = ""))
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8"))
  rownames(all_param_est) <- rownames(get_posterior_mean(model))
  all_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  preds_matrix <- preds_all_w_cover(params, y = predictions$all[[i]],
                                    cover = test_sites[[i]]$anabaena_cylindrospermum,
                                    dis = test_sites[[i]]$discharge_m3_s,
                                    temp = test_sites[[i]]$temp_C,
                                    din = test_sites[[i]]$DIN_mg_N_L,
                                    ophos = test_sites[[i]]$oPhos_ug_P_L,
                                    cond = test_sites[[i]]$cond_uS_cm,
                                    GPP = test_sites[[i]]$GPP_median_fourdaysprior)
  predictions$all_w_cover[[i]][,2:4] <- preds_summary(preds_matrix) # calculate predictions
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix[,-1], test_sites[[i]]$resp_AC_atx_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "all_w_cover")) # calculate nRMSE 
}

# looking at how parameter estimates change across all models
view(all_param_est)
# ranges:
# b0 / intercept: 9.8, 11.8
# b1 / autoregressive: 0.03, 0.29
# b2 / cover: 8.1, 17.4
# b3 / discharge: -0.3, 5.6
# b4 / temperature: 6.5, 12.9
# b5 / din: -2.3, 7.8
# b6 / ophos: -13.8, -3.8
# b7 / cond: 7.9, 22.2
# b8 / GPP: -0.25, 5.4
# sigma: 18.6, 21.2

#### (5) Saving outputs ####

# saving nRMSE table
write.csv(nrmse %>% na.omit(), "./data/predictive_models/nrmse_AC_atx_w_cover.csv",
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
write.csv(final_predictions, "./data/predictive_models/predictions_AC_atx_w_cover.csv",
          row.names = FALSE)
