#### Simple model testing
### Jordan Zabrecky
## last edited: 07.03.2025

# This code tests models comparing a simple STAN file and one using a 
# covariance matrix to make sure parameter estimates are roughly the 
# same (both using truncated normal distribution)

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

#### (3) Create empty tables for predictions and parameter estimates ####

# we will predict site three for now, so assign i 
# (using 'z' in case we want to easily change it later)
z <- 3

## predictions

# make empty list for predictions
predictions <- list()

# add in two dataframes
predictions[[1]] <- data.frame(field_date = field_dates[[z]]$field_date,
                          mean = rep(NA, length(field_dates[[z]]$field_date)),
                          ci_lower = rep(NA, length(field_dates[[z]]$field_date)), # 2.5%; lower bound of 95% interval
                          ci_upper = rep(NA, length(field_dates[[z]]$field_date))) # 97.5%; upper bound of 95% interval
predictions[[2]] <- data.frame(field_date = field_dates[[z]]$field_date,
                               mean = rep(NA, length(field_dates[[z]]$field_date)),
                               ci_lower = rep(NA, length(field_dates[[z]]$field_date)), # 2.5%; lower bound of 95% interval
                               ci_upper = rep(NA, length(field_dates[[z]]$field_date))) # 97.5%; upper bound of 95% interval

# change dataframe names
names(predictions) <- c("no_matrix", "matrix")

## make dataframe for parameter estimates
param_est <- data.frame(matrix(NA, nrow = 6, ncol = 2))

# change column names for each test
colnames(param_est) <- names(predictions)

#### (4) Making Models & Comparing Parameter Estimates ####

## Model 1 -- using simpler .STAN file (no matrix)

# model data for model 1
mod_data1 <- list(N = nrow(training_sites[[z]]),
                  future = training_sites[[z]]$future_M_cover_norm,
                  present = training_sites[[z]]$resp_M_cover_norm,
                  temp = training_sites[[z]]$temp_C,
                  disc = training_sites[[z]]$discharge_m3_s)

# run model 1
model1 <- stan(file = "./code/model_STAN_files/predicting_cover_simplephysical.stan", 
               data = mod_data1,
               chains = 3, iter = 2000, warmup = 1000, 
               control = list(adapt_delta = 0.95))


# get parameter estimates
params1 <- rstan::extract(model1, c("sigma", "b0", "b1", "b2", "b3"))
rownames(param_est) <- rownames(get_posterior_mean(model1))
param_est[,1] <- get_posterior_mean(model1)[,"mean-all chains"]

## Model 2 -- using matrix .STAN file

# get covariate matrix (temperature & discharge)
training_covariates <- as.matrix(training_sites[[z]] %>% 
                                   select(temp_C, discharge_m3_s))

# model data for model 2
mod_data2 <- list(N = nrow(training_sites[[z]]),
                 c = ncol(training_covariates),
                 future = training_sites[[z]]$future_M_cover_norm,
                 present = training_sites[[z]]$resp_M_cover_norm,
                 covar = training_covariates)

# run model 2
model2 <- stan(file = "./code/model_STAN_files/predicting_cover.stan", 
              data = mod_data2,
              chains = 3, iter = 2000, warmup = 1000, 
              control = list(adapt_delta = 0.95))

# get parameter estimates
params2 <- rstan::extract(model2, c("sigma", "b0", "b1", "b"))
param_est[,2] <- get_posterior_mean(model2)[,"mean-all chains"]

# can launch shinystan
library(shinystan)
shinystan::launch_shinystan(model2)

#### (4) Making Predictions and Comparing ####

# pull prediction function from other script for matrix
source("./code/supplemental_code/S3b_pred_functions.R")

# prediction function for no matrix
preds_cover_nomatrix <- function(params, y, temp, dis) {
  n.pred <- nrow(y) # includes initial day where we used 0
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                               mean = params$b0[i] + params$b1[i] * preds[i,j-1] 
                               + params$b2[i] * temp[j-1] + params$b3[i] * dis[j-1],
                               sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## Model 1 (no matrix) predictions

# making predictions for no matrix model
no_matrix_predictions <- preds_cover_nomatrix(params = params1,
                                              y = predictions$no_matrix,
                                              temp = test_sites[[z]]$temp_C,
                                              dis = test_sites[[z]]$discharge_m3_s)

# summarize predictions
predictions$no_matrix <- preds_summary(no_matrix_predictions)

## Model 2 (matrix) predictions

# get covariates for test site
testing_covariates <- as.matrix(test_sites[[z]] %>% 
                                  select(temp_C, discharge_m3_s))

# making predictions for matrix model
matrix_predictions <- preds_cover(params = params2, y = predictions$matrix,
                                  covar = testing_covariates)

# summarize predictions
predictions$matrix <- preds_summary(matrix_predictions)

# compare the two
comparison <- data.frame(no_matrix = predictions$no_matrix$mean,
                         matrix = predictions$matrix$mean)

view(comparison)
# very similar- yay!

#### (5) Test without prior for b1 ####

## Model 3 -- using matrix .STAN file

# using same model data (matrix ver.) as model 2

# run model 3
model3 <- stan(file = "./code/model_STAN_files/predicting_cover_nob1prior.stan", 
               data = mod_data2,
               chains = 3, iter = 2000, warmup = 1000, 
               control = list(adapt_delta = 0.95))

# get parameter estimates
params3 <- rstan::extract(model3, c("sigma", "b0", "b1", "b"))
param_est[,3] <- get_posterior_mean(model3)[,"mean-all chains"]
names(param_est)[3] <- "matrix, no b1 prior"

# can launch shinystan
library(shinystan)
shinystan::launch_shinystan(model1)

#### (6) Test more iterations ####

# doing matrix with b1 prior
# run model 4
model4 <- stan(file = "./code/model_STAN_files/predicting_cover.stan", 
               data = mod_data2,
               chains = 3, iter = 10000, warmup = 5000, 
               control = list(adapt_delta = 0.95))

# shiny stan
launch_shinystan(model4)

# doing matrix w/o b1 prior
# run model 5
model5 <- stan(file = "./code/model_STAN_files/predicting_cover_nob1prior.stan", 
               data = mod_data2,
               chains = 3, iter = 10000, warmup = 5000, 
               control = list(adapt_delta = 0.95))

# shiny stan
launch_shinystan(model4)

#### (7) Parameter Estimates under Normal w/o truncation ####

# doing matrix under normal dist
# run model 6
model6 <- stan(file = "./code/model_STAN_files/predicting_cover_normaldist.stan", 
               data = mod_data2,
               chains = 3, iter = 2000, warmup = 1000, 
               control = list(adapt_delta = 0.95))

# shiny stan
launch_shinystan(model6)

# get parameter estimates
params6 <- rstan::extract(model6, c("sigma", "b0", "b1", "b"))
param_est[,4] <- get_posterior_mean(model6)[,"mean-all chains"]
names(param_est)[4] <- "matrix, normal dist"
