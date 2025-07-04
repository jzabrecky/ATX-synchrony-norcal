#### models to predict cover (truncated norm version)
### Jordan Zabrecky
## last edited: 07.02.2025

# This script builds models to predict cover of benthic Microcoleus cover
# as determined by benthic cover surveys. Each model is built using 4 of 
# 5 reaches, then the model is tested using the withheld fifth reach

# code redo/todo:
# -use prior covariates
# -check matrix math & fix matrix stuff
# -set b1 prior to unif
# -function for all; modularize code

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "truncnorm", "Metrics"), 
       require, character.only = T)

# loading in data and select only columns we care about
raw_data <- read.csv("./data/predictive_models/inputs.csv") %>% 
  select(field_date, site_reach, resp_M_cover_norm, temp_C,
         discharge_m3_s, DIN_mg_N_L, oPhos_ug_P_L, cond_uS_cm,
         GPP_median_tofourdaysprior)

# make another data frame for edited data
data <- raw_data

# because we are training one model with data from multiple sites,
# if we do t-1, when we iterate to the next, it will cross sites
# instead lets create a future response column and delete the last days
data$future_M_cover_norm <- c(data$resp_M_cover_norm[-1], NA)

# remove final day (have no future values after that date)
data <- data[-which(data$field_date == "2023-09-24"),]

# double-check that lines up as if prior is the previous value from resp on same row
# including DIN to show a covariate observed on that day
check <- data %>% 
  select(field_date, site_reach, resp_M_cover_norm, DIN_mg_N_L, future_M_cover_norm)
view(check)

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

#### (4) Predicting Microcoleus Cover ####

# get prediction functions
source("./code/supplemental_code/S3b_pred_functions.R")

model_name <- "physical"
covariates <- list()
for(i in 1:length(training_sites)) {
  covariates[[i]] <- as.matrix(training_sites[[i]] %>% 
                                 select(temp_C, discharge_m3_s))
}
i <- 1

#### mini test
i <- 2
covariates <- as.matrix(training_sites[[i]] %>% 
                          select(temp_C, discharge_m3_s))
mod_data <- list(N = nrow(training_sites[[i]]),
                 c = ncol(covariates),
                 future = training_sites[[i]]$future_M_cover_norm,
                 present = training_sites[[i]]$resp_M_cover_norm,
                 covar = covariates)
# run STAN model
model <- stan(file = "./code/model_STAN_files/predicting_cover.stan", 
              data = mod_data,
              chains = 3, iter = 2000, warmup = 1000, 
              control = list(adapt_delta = 0.95))


# function to build all five predictive models for each model
# model_name = string of model name
# covariates = list of matrices for each reach test

## LEFT OFF HERE- go through it and make sure it makes sense for a test case!!!

predict_all(model_name, covariates) {
  
  # empty list to save parameter estimates for each model
  # and parameter r-hats for each model
  # number of covariates plus 4 (for b0, b1, sigma, and lp)
  # ncol = 5 (hard-coded; number of reaches)
  param_est <- data.frame(matrix(NA, nrow = ncol(covariates[[1]]) + 4, 
                                 ncol = 5))
  rhats <- data.frame(matrix(NA, nrow = ncol(covariates[[1]]) + 4, 
                            ncol = 5))
  colnames(param_est) <- names(test_sites)
  colnames(rhats) <- names(test_sites)
  
  # get index of list of predictions dataframe that corresponds to model name
  j <- which(names(predictions) == model_name) 
  
  for(i in 1:length(training_sites)) {
    # gather data
    mod_data <- list(N = nrow(training_sites[[i]]),
                     c = ncol(covariates[[i]]),
                     future = training_sites[[i]]$future_M_cover_norm,
                     present = training_sites[[i]]$resp_M_cover_norm,
                     covar = as.matrix(covariates[[i]]))
    # run STAN model
    model <- stan(file = "./code/model_STAN_files/predicting_cover.stan", 
                  data = mod_data,
                  chains = 3, iter = 2000, warmup = 1000, 
                  control = list(adapt_delta = 0.95))
    # save STAN model
    saveRDS(model, paste("./data/predictive_models/M_cover_models/", model_name, 
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
    preds_matrix <- preds_cover(params = params,
                                y = predictions$physical[[i]],
                                covar = as.matrix(test_sites[[i]] %>% 
                                                    select(temp_C, discharge_m3_s)))
    # save summary of predictions
    predictions[j][[i]][,2:4] <- preds_summary(preds_matrix)
    # calculate nRMSE of model
    nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$future_M_cover_norm,
                                        site_reach_name = names(test_sites)[i],
                                        model_name = model_name))
  }
  
  # save rhats and mean parameter estimates
  write.csv(rhats, paste("./data/predictive_models/M_cover_models/model_attributes/",
                         model_name, "_rhats.csv", sep = ""), row.names = TRUE)
  write.csv(param_est, paste("./data/predictive_models/M_cover_models/model_attributes/",
                         model_name, "_param_est.csv", sep = ""), row.names = TRUE)
  
  # lastly, print if all models converged <1.05 or not!
  if(any(rhats > 1.05)) {
    print("models did not converge :(")} else {
      print("models did converge!! :)")
    }
}

## (a) null - mean of all cover data

# calculate mean (ignoring first day which we are not predicting)
mean_cover <- mean(data$future_M_cover_norm)

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

#### (5) Saving Outputs

# need to save nRMSE table and predictions