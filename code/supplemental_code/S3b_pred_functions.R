#### functions to make predictions with STAN models
### Jordan Zabrecky
## last edited: 07.03.2025

# This script hosts function to make predictions using STAN
# models and returns predictions (mean, lower bound of 95% confidence 
# interval, and upper bound of 95% confidence interval)

#### (1) Functions to generate predictions matrix ####

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

#### (2) Functions to get predictions and nRMSE summaries ####

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

# function to calculate nRMSE with predicted and observed vectors
calc_nRMSE <- function(predicted, observed, max, min) {
  nRMSE <- rmse(observed, predicted) / (max - min)
}

# calculate mean and 95% confidence interval nRMSE's
nRMSE_summary <- function(preds_matrix, observed, site_reach_name, model_name) {
  # calculation of nRMSE of predictions should not include initial value as that was pre-set
  # so remove first column from preds_matrix
  preds_matrix <- preds_matrix[,-1]
  
  # get number of predictions and max and min of observed values (should be 0 & 100)
  n.pred <- length(observed)
  max <- max(observed)
  min <- min(observed)
  
  # make matrix for RMSE values
  nRMSE_matrix <- matrix(data = NA, nrow = nrow(preds_matrix), 
                         ncol = ncol(preds_matrix))
  
  # fill in nRMSE for each predicted value in the matrix
  for(j in 1:n.pred){
    for(i in 1:length(params$sigma)) {
      nRMSE_matrix[i,j] <- calc_nRMSE(observed[j], preds_matrix[i,j], max, min)
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
