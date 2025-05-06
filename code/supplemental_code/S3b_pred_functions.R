#### functions to make predictions with STAN models
### Jordan Zabrecky
## last edited: 05.05.2025

# This script hosts function to make predictions using STAN
# models and returns predictions (mean, lower bound of 95% confidence 
# interval, and upper bound of 95% confidence interval)

#### (1) Functions (autoregressive w/o cover covariate) ####

## (a) for normalized_physical.STAN 
# (autoregressive term + discharge + temperature)
preds_physical <- function(params, y, dis, temp) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (b) for normalized_chemical.STAN 
# (autoregressive term + DIN + orthophosphate + conductivity)
preds_chemical <- function(params, y, din, ophos, cond) {
  n.pred <- nrow(y) # days including initial
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * din[j] + params$b3[i] * ophos[j] +
                                   params$b4[i] * cond[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (c) for normalized_biological.STAN 
# (autoregressive term + GPP)
preds_biological <- function(params, y, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      # calculate predictions
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
    # save mean of all predictions to use as autoregressive term in next time stamp
    y$mean[j] <- mean(preds[,j-1])
  }
  
  # return filled predictions matrix
  return(preds)
}

## (d) for normalized_physicochemical.STAN 
# (autoregressive term + discharge + temperature + din + ophos + cond)
preds_physicochemical <- function(params, y, dis, temp, din, ophos, cond) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j] +
                                   params$b4[i] * din[j] + params$b5[i] * ophos[j] +
                                   params$b6[i] * cond[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (e) for normalized_ecohydrological.STAN 
# (autoregressive term + discharge + temperature + GPP)
preds_ecohydrological <- function(params, y, dis, temp, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * temp[j] + params$b3[i] * dis[j] +
                                   params$b4[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (f) for normalized_biochemical.STAN 
# (autoregressive term + din + ophos + cond + GPP)
preds_biochemical <- function(params, y, din, ophos, cond, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * din[j] + params$b3[i] * ophos[j] +
                                   params$b4[i] * cond[j] + params$b5[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (f) for normalized_all.STAN 
# (autoregressive term + discharge + temp din + ophos + cond + GPP)
preds_all <- function(params, y, dis, temp, din, ophos, cond, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * dis[j] + params$b3[i] * temp[j] +
                                   params$b4[i] * ophos[j] + params$b5[i] * din[j] +
                                   params$b6[i] * cond[j] + params$b7[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

#### (2) Functions (autoregressive w/ cover covariate) ####

## (b) for normalized_w_cover.STAN 
# (autoregressive term + cover)
preds_w_cover <- function(params, y, cover) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      # calculate predictions
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (b) for normalized_physical_w_cover.STAN 
# (autoregressive term + cover + discharge + temperature)
preds_physical_w_cover <- function(params, y, cover, dis, temp) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j] + params$b3[i] * dis[j] +
                                   params$b4[i] * temp[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (c) for normalized_chemical_w_cover.STAN 
# (autoregressive term + cover + DIN + orthophosphate + conductivity)
preds_chemical_w_cover <- function(params, y, cover, din, ophos, cond) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j] + params$b3[i] * din[j] +
                                   params$b4[i] * ophos[j] + params$b5[i] * cond[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (d) for normalized_biological_w_cover.STAN 
# (autoregressive term + cover + GPP)
preds_biological_w_cover <- function(params, y, cover, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      # calculate predictions
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j] + params$b3[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (e) for normalized_physicochemical_w_cover.STAN 
# (autoregressive term + discharge + temperature + din + ophos + cond)
preds_physicochemical_w_cover <- function(params, y, cover, dis, temp, din, ophos, cond) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j] + params$b3[i] * dis[j] +
                                   params$b4[i] * temp[j] + params$b5[i] * din[j] +
                                   params$b6[i] * ophos[j] + params$b7[i] * cond[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (f) for normalized_ecohydrological_w_cover.STAN 
# (autoregressive term + discharge + temperature + GPP)
preds_ecohydrological_w_cover <- function(params, y, cover, dis, temp, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j] + params$b3[i] * temp[j] +
                                   params$b4[i] * dis[j] + params$b5[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (g) for normalized_biochemical_w_cover.STAN 
# (autoregressive term + din + ophos + cond + GPP)
preds_biochemical_w_cover <- function(params, y, cover, din, ophos, cond, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j] + params$b3[i] * din[j] +
                                   params$b4[i] * ophos[j] + params$b5[i] * cond[j] +
                                   params$b6[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}

## (h) for normalized_all_w_cover.STAN 
# (autoregressive term + discharge + temp din + ophos + cond + GPP)
preds_all_w_cover <- function(params, y, cover, dis, temp, din, ophos, cond, GPP) {
  n.pred <- nrow(y) # days including initial
  preds <- matrix(NA, length(params$sigma), n.pred) # empty prediction matrix
  preds[,1] <- 0 # assign first values to zero (hard-coded because we start all w/ zero)
  
  # make predictions
  for(j in 2:n.pred) {
    for(i in 1:length(params$sigma)) {
      preds[i,j] <- rtruncnorm(n = 1, a = 0, b = 100,
                                 mean = params$b0[i] + params$b1[i] * preds[i,j-1] +
                                   params$b2[i] * cover[j] + params$b3[i] * dis[j] +
                                   params$b4[i] * temp[j] + params$b5[i] * ophos[j] +
                                   params$b6[i] * din[j] + params$b7[i] * cond[j] +
                                   params$b8[i] * GPP[j],
                                 sd = params$sigma[i]) # process error
    }
  }
  
  # return filled predictions matrix
  return(preds)
}
