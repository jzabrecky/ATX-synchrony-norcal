#### models to predict atx (truncated norm version)
### Jordan Zabrecky
## last edited: 07.01.2025

# just a test of one case without matrix declaration

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
raw_data$prior_M_atx_norm <- c(NA, raw_data$resp_M_atx_norm[-nrow(raw_data)])

# double-check that lines up as if prior is the previous value from resp on same row
check <- raw_data %>% 
  select(resp_M_atx_norm, prior_M_atx_norm)
view(check)

# remove first day at each reach (6/20/2023 and 6/25/2023 for STH only)
data <- raw_data[-1,] # removes first day at Standish Hickey
data <- data[-which(data$field_date == "2023-06-20"),] # removes all other first days

# double-check again that lines up
check <- data %>% 
  select(field_date, site_reach, resp_M_atx_norm, prior_M_atx_norm)
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
# physical = temperature and flow
# chemical = nutrients (DIN & oPhos) and conductivity
# biological = GPP
# physicochemical = temperature, flow, nutrients, and conductivity
# ecohydrological = temperature, flow, and GPP
# biochemical = GPP, nutrients, and conductivity
# all = autoregressive w/ all covariates

#### (4) Predicting Microcoleus anatoxins ####

# just doing one test for physicochemical model

# prediction function


## (e) physicochemical (temp + discharge + DIN + ophos + cond)

# for predictions involving cover (i.e. includes an autoregressive term)
preds_physicochemical <- function(params, y, temp, dis, din, ophos, cond) {
  n.pred <- nrow(y) # includes initial day where we used 0
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                               mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                 params$b2[i] * temp[j-1] + params$b3[i] * dis[j-1] +
                                 params$b4[i] * din[j-1] + params$b5[i] * ophos[j-1] +
                                 params$b5[i] * cond[j-1],
                               sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
  
}

source("./code/supplemental_code/S3b_pred_functions.R")


# empty list to save parameter estimates for each model
# and parameter r-hats for each model
physicochemical_param_est <- data.frame(matrix(NA, nrow = 9, ncol = 5))
physicochemical_rhats <- data.frame(matrix(NA, nrow = 9, ncol = 5))
colnames(physicochemical_param_est) <- names(test_sites)
colnames(physicochemical_rhats) <- names(test_sites)

# generate predictions for each site
for(i in 1:length(training_sites)) {
  # gather data
  mod_data <- list(N = nrow(training_sites[[i]]),
                   y = training_sites[[i]]$resp_M_atx_norm,
                   autoreg = training_sites[[i]]$prior_M_atx_norm,
                   dis = training_sites[[i]]$discharge_m3_s,
                   temp = training_sites[[i]]$temp_C,
                   din = training_sites[[i]]$DIN_mg_N_L,
                   ophos = training_sites[[i]]$oPhos_ug_P_L,
                   cond = training_sites[[i]]$cond_uS_cm)
  # run STAN model
  model <- stan(file = "./code/model_STAN_files/old_model_specific_files/normalized_physicochemical_fixed_autoreg.stan", 
                data = mod_data,
                chains = 3, iter = 2000, warmup = 1000, 
                control = list(adapt_delta = 0.95))
  # save STAN model
  saveRDS(model, paste("./data/predictive_models/no_matrix_test/M_atx_models/physicochemical_", 
                       names(test_sites)[i], sep = ""))
  # extract parameters
  params <- rstan::extract(model, c("sigma", "b0", "b1", "b2", "b3", "b4", "b5", "b6"))
  # add mean parameter estimates to dataframe
  rownames(physicochemical_param_est) <- rownames(get_posterior_mean(model))
  physicochemical_param_est[i] <- get_posterior_mean(model)[,"mean-all chains"]
  # add r-hats to dataframe
  rownames(physicochemical_rhats) <- names(summary(model)$summary[,"Rhat"])
  physicochemical_rhats[i] <- summary(model)$summary[,"Rhat"]
  # make predictions matrix
  preds_matrix <- preds_physicochemical(params = params,
                            y = predictions$physicochemical[[i]],
                            temp = test_sites[[i]]$temp_C,
                            dis = test_sites[[i]]$discharge_m3_s,
                            din = test_sites[[i]]$DIN_mg_N_L,
                            ophos = test_sites[[i]]$oPhos_ug_P_L,
                            cond = test_sites[[i]]$cond_uS_cm)
  # save summary of predictions
  predictions$physicochemical[[i]][,2:4] <- preds_summary(preds_matrix)
  # calculate nRMSE of model
  nrmse <- rbind(nrmse, nRMSE_summary(preds_matrix, test_sites[[i]]$resp_M_atx_norm,
                                      site_reach_name = names(test_sites)[i],
                                      model_name = "physicochemical"))
}

# check all r-hats < 1.05 & save r-hats
any(physicochemical_rhats > 1.05) # all above 1.05!
write.csv(physicochemical_rhats,
          "./data/predictive_models/no_matrix_test/M_atx_models/model_attributes/physicochemical_rhats.csv",
          row.names = TRUE)

# looking at how parameter estimates change across all models
view(physicochemical_param_est)
write.csv(physicochemical_param_est,
          "./data/predictive_models/no_matrix_test/M_atx_models/model_attributes/physicochemical_param_est.csv",
          row.names = TRUE)

#### (5) Saving outputs ####

# saving nRMSE table
write.csv(nrmse %>% na.omit(), "./data/predictive_models/no_matrix_test/nrmse_M_atx.csv",
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
write.csv(final_predictions, "./data/predictive_models/no_matrix_test/predictions_M_atx.csv",
          row.names = FALSE)
