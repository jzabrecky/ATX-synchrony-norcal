#### functions to make predictions with STAN models
### Jordan Zabrecky
## last edited: 04.29.2025

# This script hosts function to make predictions using STAN
# models and returns predictions (mean, lower bound of 95% confidence 
# interval, and upper bound of 95% confidence interval)

#### (1) Functions (autoregressive w/o cover covariate) ####

## (a) for normalized_physical.STAN 
# (autoregressive term + discharge + temperature)
preds_physical <- function(params, y, dis, temp) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y$mean[j-1] +
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
    y$ci_lower[j] <- quantile(preds[,j-1], prob = 0.025)
    y$ci_upper[j] <- quantile(preds[,j-1], prob = 0.975)
  }
  
  # return filled in dataframe
  return(y)
}

## (b) for normalized_chemical.STAN 
# (autoregressive term + DIN + orthophosphate + conductivity)
preds_chemical <- function(params, y, din, ophos, cond) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y$mean[j-1] +
                                   params$b2[i] * din[j] + params$b3[i] * ophos[j] +
                                   params$b4[i] * cond[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
    y$ci_lower[j] <- quantile(preds[,j-1], prob = 0.025)
    y$ci_upper[j] <- quantile(preds[,j-1], prob = 0.975)
  }
  
  # return filled in dataframe
  return(y)
}

## (c) for normalized_biological.STAN 
# (autoregressive term + GPP)
preds_biological <- function(params, y, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y$mean[j-1] +
                                   params$b2[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
    y$ci_lower[j] <- quantile(preds[,j-1], prob = 0.025)
    y$ci_upper[j] <- quantile(preds[,j-1], prob = 0.975)
  }
  
  # return filled in dataframe
  return(y)
}

## (d) for normalized_physicochemical.STAN 
# (autoregressive term + discharge + temperature + din + ophos + cond)
preds_physicochemical <- function(params, y, dis, temp, din, ophos, cond) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y$mean[j-1] +
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j] +
                                   params$b4[i] * din[j] + params$b5[i] * ophos[j] +
                                   params$b6[i] * cond[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
    y$ci_lower[j] <- quantile(preds[,j-1], prob = 0.025)
    y$ci_upper[j] <- quantile(preds[,j-1], prob = 0.975)
  }
  
  # return filled in dataframe
  return(y)
}

## (e) for normalized_ecohydrological.STAN 
# (autoregressive term + discharge + temperature + GPP)
preds_ecohydrological <- function(params, y, dis, temp, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y$mean[j-1] +
                                   params$b2[i] * temp[j] + params$b3[i] * dis[j] +
                                   params$b4[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
    y$ci_lower[j] <- quantile(preds[,j-1], prob = 0.025)
    y$ci_upper[j] <- quantile(preds[,j-1], prob = 0.975)
  }
  
  # return filled in dataframe
  return(y)
}

## (f) for normalized_biochemical.STAN 
# (autoregressive term + din + ophos + cond + GPP)
preds_biochemical <- function(params, y, din, ophos, cond, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y$mean[j-1] +
                                   params$b2[i] * din[j] + params$b3[i] * ophos[j] +
                                   params$b4[i] * cond[j] + params$b5[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
    y$ci_lower[j] <- quantile(preds[,j-1], prob = 0.025)
    y$ci_upper[j] <- quantile(preds[,j-1], prob = 0.975)
  }
  
  # return filled in dataframe
  return(y)
}

## (f) for normalized_all.STAN 
# (autoregressive term + discharge + temp din + ophos + cond + GPP)
preds_all <- function(params, y, dis, temp, din, ophos, cond, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred - 1) # empty prediction matrix
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j-1] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * y$mean[j-1] +
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j] +
                                   params$b4[i] * ophos[j] + params$b5[i] * din[j] +
                                   params$b6[i] * cond[j] + params$b7[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
    y$ci_lower[j] <- quantile(preds[,j-1], prob = 0.025)
    y$ci_upper[j] <- quantile(preds[,j-1], prob = 0.975)
  }
  
  # return filled in dataframe
  return(y)
}
