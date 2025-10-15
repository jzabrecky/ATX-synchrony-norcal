#### functions to make predictions with STAN models
### Jordan Zabrecky
## last edited: 10.14.2025

# This script hosts function to make covariates for each model, 
# make predictions using STAN models, and returns predictions 
# (mean, lower bound of 95% confidence interval, and upper bound of 95% confidence interval)

#### (1) Function to make covariates for each model ####

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


#### (2) Functions to generate predictions matrix ####

# for predictions functions...
# params = parameters extracted from STAN model
# y = empty predictions dataframe
# covar = matrix of covariates for test site

# for predictions of cover (autoregressive, starting value of 0.05)
preds_cover <- function(params, y, covar) {
  n.pred <- nrow(y) # includes initial day where we used 0
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0.05 # assign first values to our version of 0; hard-coded bc all predictions start with 0
  # again, our zero here is >0 as when making the model we need a number >0 to make it increase
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100, mean = (params$b0[i] + covar[j-1,]%*%params$b[i,]) 
                               * preds[i,j-1],
                               # using previous prediction and covariates (j-1)
                               # to predict next time step
                               sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

# for predictions of anatoxins (i.e. does not include autoregressive term)
preds_anatoxins <- function(params, y, covar) {
  n.pred <- nrow(y) # includes initial day where we used 0
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100, mean = (params$b0[i] + covar[j-1,]%*%params$b[i,]),
                               # using previous prediction and covariates (j-1)
                               # to predict next time step
                               sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

#### (3) Functions to get predictions and NRMSEs ####

# function to calculate mean & 95% confidence interval of predictions from prediction matrix
preds_summary <- function(preds_matrix) {
  
  # creating dataframe of mean and 95% confidence interval
  y <- data.frame(mean = rep(NA, ncol(preds_matrix)))
  y$mean <- apply(preds_matrix, 2, mean)
  y$ci_lower <- apply(preds_matrix, 2, function(x) quantile(x, prob = 0.025))
  y$ci_upper <- apply(preds_matrix, 2, function(x) quantile(x, prob = 0.975))
  
  
  # return predictions data frame
  return(y)
}

# function to calculate NRMSE with predicted and observed vectors
calc_NRMSE <- function(predicted, observed, max, min) {
  NRMSE <- rmse(observed, predicted) / (max - min)
}

# calculate mean and 95% confidence interval NRMSE's
NRMSE_summary <- function(preds_matrix, observed) {
  # calculation of NRMSE of predictions should not include initial value as that was pre-set
  # so remove first column from preds_matrix
  preds_matrix <- preds_matrix[,-1]
  
  # get number of predictions and max and min of observed values (should be 0 or 0.05 & 100)
  n.pred <- length(observed)
  max <- max(observed)
  min <- min(observed)
  
  # make VECTOR for RMSE values (one value for each model!)
  NRMSE_vector <- c(rep(NA, nrow(preds_matrix)))
  
  # fill in NRMSE for each model predictions
  for(j in 1:n.pred){
    for(i in 1:length(params$sigma)) {
      NRMSE_vector[i] <- calc_NRMSE(observed, preds_matrix[i,], max, min)
    }
  }
  
  # return vector to be saved in main script
  return(NRMSE_vector)
}
