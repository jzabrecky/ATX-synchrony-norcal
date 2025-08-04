#### models to predict cover
### Jordan Zabrecky
## last edited: 07.29.2025

# This script builds models to predict cover of benthic Anabaena/Cylindrospermum cover
# as determined by benthic cover surveys. Each model is built using 4 of 
# 5 reaches, then the model is tested using the withheld fifth reach

# LOGGED VERSION!!!!

# temporary predictions function
preds_cover_trunc_for_log <- function(params, y, covar) {
  n.pred <- nrow(y) # includes initial day where we used 0
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0.01 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      # exponetiated to convert back to 0,100 scale!
      preds[i,j] <- exp(rtruncnorm(n = 1, b = log(100),mean = params$b0[i] + 
                                     params$b1[i] * preds[i,j-1] +
                                 # using previous prediction and covariates (j-1)
                                 # to predict next time step
                                 covar[j-1,]%*%params$b[i,],
                               sd = params$sigma[i])) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
  
}

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
  mutate(resp_AC_cover_norm = case_when(resp_AC_cover_norm == 0 ~ 0.01,
                                       TRUE ~ resp_AC_cover_norm),
         future_AC_cover_norm = case_when(future_AC_cover_norm == 0 ~ 0.01,
                                         TRUE ~ future_AC_cover_norm)) %>% 
  # log values
  mutate(resp_AC_cover_norm = log(resp_AC_cover_norm),
         future_AC_cover_norm = log(future_AC_cover_norm))

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

# function to make list of covariates training and testing for each model
make_covariates <- function(covariates) {
  
  # create empty lists
  training_list = list()
  testing_list = list()
  
  # assign covariates for each reach grouping
  for(i in 1:length(test_sites)) {
    training_list[[i]] = as.matrix(training_sites[[i]] %>% 
                                     select(all_of(covariates)))
    testing_list[[i]] = as.matrix(test_sites[[i]] %>% 
                                    select(all_of(covariates)))
  }
  
  # create and return final list of the two combined
  final_list = list(training_list, testing_list)
  names(final_list) = c("training", "testing")
  return(final_list)
}

#### (4) Predicting Microcoleus Cover ####

# get prediction functions
source("./code/supplemental_code/S3b_pred_functions.R")

## (a) null - mean of all cover data

# calculate mean to use for null model (this ignores first day which we are not predicting)
mean_cover <- mean(exp(data$future_AC_cover_norm)) # need to exponential for % on 0,100 scale

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
  # (removing first row of prediction which is first day that we are not predicting!)
  new$mean <- calc_nRMSE(predictions$null[[i]]$mean[-1], test_sites[[i]]$future_AC_cover_norm,
                         # predictions values are on 0-100% scale so also need to exponentiate normalization
                         max(exp(test_sites[[i]]$future_AC_cover_norm)), min(exp(test_sites[[i]]$future_AC_cover_norm)))
  new$ci_lower <- calc_nRMSE(predictions$null[[i]]$ci_lower[-1], test_sites[[i]]$future_AC_cover_norm,
                             max(exp(test_sites[[i]]$future_AC_cover_norm)), min(exp(test_sites[[i]]$future_AC_cover_norm)))
  new$ci_upper <- calc_nRMSE(predictions$null[[i]]$ci_upper[-1], test_sites[[i]]$future_AC_cover_norm,
                             max(exp(test_sites[[i]]$future_AC_cover_norm)), min(exp(test_sites[[i]]$future_AC_cover_norm)))
  nrmse <- rbind(nrmse, new)
}

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
  
  # empty list to save parameter estimates for each model
  # and parameter r-hats for each model
  # number of covariates plus 4 (for b0, b1, sigma, and lp)
  # ncol = 5 (hard-coded; number of reaches)
  param_est <- data.frame(matrix(NA, nrow = ncol(covariates[[j]]$training[[1]]) 
                                 # just using reach 1, all will have same number of columns 
                                 + 4, ncol = 5)) # plus 4 includes b0, b1, sigma, & lp
  rhats <- data.frame(matrix(NA, nrow = ncol(covariates[[j]]$training[[1]]) 
                             + 4, ncol = 5))
  colnames(param_est) = names(test_sites)
  colnames(rhats) = names(test_sites)
  
  # get model name string (for saving files)
  model_name <- names(covariates)[j]
  
  # build models and make predictions for each reach
  for(i in 1:length(training_sites)) {
    # gather data
    mod_data = list(N = nrow(training_sites[[i]]),
                    c = ncol(covariates[[j]]$training[[i]]),
                    future = training_sites[[i]]$future_AC_cover_norm,
                    present = training_sites[[i]]$resp_AC_cover_norm,
                    covar = as.matrix(covariates[[j]]$training[[i]]))
    # run STAN model
    # if there are warning issues, code may stop if running through whole script 
    # (if using cntl+shift+enter)
    model <- stan(file = "./code/model_STAN_files/predicting_autoregressive_logver.stan", 
                 data = mod_data,
                 chains = 3, iter = 10000, warmup = 5000, 
                 control = list(adapt_delta = 0.95, max_treedepth = 12))
    # save STAN model
    saveRDS(model, paste("./data/predictive_models/log_ver/AC_cover_models/", model_name, 
                         "_", names(test_sites)[i], sep = ""))
    # extract parameters
    params <- rstan::extract(model, c("sigma", "b0", "b1", "b"))
    # add mean parameter estimates to dataframe
    rownames(param_est) <- rownames(get_posterior_mean(model))
    param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
    # add r-hats to dataframe
    rownames(rhats) <- names(summary(model)$summary[,"Rhat"])
    rhats[i] <- summary(model)$summary[,"Rhat"]
    # make predictions matrix
    preds_matrix <- preds_cover_trunc_for_log(params = params,
                               y = predictions[[j]][[i]],
                               covar = as.matrix(covariates[[j]]$testing[[i]]))
    # save summary of prediction; make sure to assign globally
    predictions[[j]][[i]][,2:4] <- preds_summary(preds_matrix)
    # calculate nRMSE of model
    nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, exp(test_sites[[i]]$future_AC_cover_norm),
                                        site_reach_name = names(test_sites)[i],
                                        model_name = model_name))
  }
  
  # save rhats and mean parameter estimates
  write.csv(rhats, paste("./data/predictive_models/log_ver/AC_cover_models/model_attributes/",
                         model_name, "_rhats.csv", sep = ""), row.names = TRUE)
  write.csv(param_est,  paste("./data/predictive_models/log_ver/AC_cover_models/model_attributes/",
                             model_name, "_param_est.csv", sep = ""), row.names = TRUE)
  
  # lastly, print if all models converged <1.05 or not!
  if(any(rhats > 1.05)) {
    print(paste(model_name, " models did not converge :(", sep = ""))} else {
      print(paste(model_name, " models did converge :)", sep = ""))
    }
  
}

#### (5) Saving Outputs

# saving nRMSE table
write.csv(nrmse %>% na.omit(), "./data/predictive_models/log_ver/nrmse_AC_cover.csv",
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

# exponentiate predictions before saving

# saving final predictions
write.csv(final_predictions, "./data/predictive_models/log_ver/predictions_AC_cover.csv",
          row.names = FALSE)
