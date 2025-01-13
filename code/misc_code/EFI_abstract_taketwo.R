## initial passes at predictive modeling
library(tidyverse)
library(zoo)
library(randomForest)
library(factoextra)
library(rstan)
library(StanHeaders)

# read in data

# all data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

# want to normalize GPP and discharge by site?
data$discharge_m3_s_normalized <- ave(data$discharge_m3_s, data$site, FUN = scale)
data$GPP_mean_fourdaysprior_normalized <- ave(data$GPP_mean_fourdaysprior, data$site, FUN = scale)

# fuck it, normalizing occurence per site
data$microcoleus_normalized <- ave(data$microcoleus, data$site_reach, FUN = scale)
data$anabaena_cylindrospermum_normalized <- ave(data$anabaena_cylindrospermum, data$site_reach, FUN = scale)

# split into sites for testing and training
test_sites <- split(data, data$site_reach)

# training sites
training_sites <- list()
for(i in 1:length(test_sites)) {
  training_sites[[i]] <- data %>%
    filter(site_reach != test_sites[[i]]$site_reach[i])
}

# remove rows with NA for GPP
data_GPP <- data[-which(is.na(data$GPP_mean_fourdaysprior)),]
test_sites_GPP <- split(data_GPP, data_GPP$site_reach)
training_sites_GPP <- list()
for(i in 1:length(test_sites_GPP)) {
  training_sites_GPP[[i]] <- data_GPP %>%
    filter(site_reach != test_sites_GPP[[i]]$site_reach[i])
}

# remove rows with NA for ATX (note this has issues w/ autoregressive component)
data_TM <- data[-which(is.na(data$TM_ATX_all_ug_orgmat_g)),]
test_sites_TM <- split(data_TM, data_TM$site_reach)
training_sites_TM <- list()
for(i in 1:length(test_sites_TM)) {
  training_sites_TM[[i]] <- data_TM %>%
    filter(site_reach != test_sites_TM[[i]]$site_reach[i])
}

# remove rows with NA for ATX (note this has issues w/ autoregressive component)
data_TAC <- data[-which(is.na(data$TAC_ATX_all_ug_orgmat_g)),]
test_sites_TAC <- split(data_TAC, data_TAC$site_reach)
training_sites_TAC <- list()
for(i in 1:length(test_sites_TAC)) {
  training_sites_TAC[[i]] <- data_TAC %>%
    filter(site_reach != test_sites_TAC[[i]]$site_reach[i])
}


#### predictions #### -- can better merge these functions in the future
pred_out_autoregressive <- function(params, cover) {
  n.pred <- (length(cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 2:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + cover[j-1] * params$b1[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP[-1]) 
}

pred_out_singlevar <- function(params, cover, var1) {
  n.pred <- (length(cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 2:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + cover[j-1] * params$b1[i] +
                 var1[j] * params$b2[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP[-1]) 
}

pred_out_doublevar <- function(params, cover, var1, var2) {
  n.pred <- (length(cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 2:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + cover[j-1] * params$b1[i] +
                   var1[j] * params$b2[i] + var2[j] * params$b3[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP[-1]) 
}

#### occurrence models ####

setwd("./code/misc_code/EFI_prelim_STAN/")

## microcoleus

# could index predictions to look at predictions!

# plain autoregressive
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$microcoleus)
  model <- stan(file = "autoregressive_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "sigma"))
  preds <- pred_out_autoregressive(params, test_sites[[i]]$microcoleus)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$microcoleus[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$microcoleus - min(test_sites[[i]]$microcoleus)))
}
mean(nrmse) # 0.247; worst site is SFE-M 4 best is SFE-M-1S and SFE-SH-1S

# normalized autoregressive
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$microcoleus_normalized)
  model <- stan(file = "autoregressive_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "sigma"))
  preds <- pred_out_autoregressive(params, test_sites[[i]]$microcoleus_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$microcoleus_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$microcoleus_normalized - min(test_sites[[i]]$microcoleus_normalized)))
}
mean(nrmse) # 0.245; worst site is SFE-M 4 best is SFE-M-1S and SFE-SH-1S

# discharge
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$microcoleus,
                   var1 = training_sites[[i]]$discharge_m3_s_normalized)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$microcoleus, 
                                   test_sites[[i]]$discharge_m3_s_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$microcoleus[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$microcoleus - min(test_sites[[i]]$microcoleus)))
}
mean(nrmse) # 0.247; marginally better; worst for SFE-M 4 and best for SFE-M 1 & 2

# discharge; cover normalized per site_reach
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$microcoleus_normalized,
                   var1 = training_sites[[i]]$discharge_m3_s_normalized)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$microcoleus_normalized, 
                              test_sites[[i]]$discharge_m3_s_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$microcoleus_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$microcoleus_normalized - min(test_sites[[i]]$microcoleus_normalized)))
}
mean(nrmse) # 0.207; getter! worse for SFE-M 4 but better for every other site I think

# GPP
nrmse <- rep(NA, length(training_sites_GPP))
for(i in 1:length(training_sites_GPP)) {
  mod_data <- list(N = nrow(training_sites_GPP[[i]]), 
                   cover = training_sites_GPP[[i]]$microcoleus,
                   var1 = training_sites_GPP[[i]]$GPP_mean_fourdaysprior_normalized)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites_GPP[[i]]$microcoleus, 
                              test_sites_GPP[[i]]$GPP_mean_fourdaysprior_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites_GPP[[i]]$microcoleus[-1])^2)/length(preds))) /
    (max(test_sites_GPP[[i]]$microcoleus - min(test_sites_GPP[[i]]$microcoleus)))
}
mean(nrmse) # 0.254; worse worst for SFE-M-4 again

# GPP; cover normalized per reach
nrmse <- rep(NA, length(training_sites_GPP))
for(i in 1:length(training_sites_GPP)) {
  mod_data <- list(N = nrow(training_sites_GPP[[i]]), 
                   cover = training_sites_GPP[[i]]$microcoleus_normalized,
                   var1 = training_sites_GPP[[i]]$GPP_mean_fourdaysprior_normalized)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites_GPP[[i]]$microcoleus_normalized, 
                              test_sites_GPP[[i]]$GPP_mean_fourdaysprior_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites_GPP[[i]]$microcoleus_normalized[-1])^2)/length(preds))) /
    (max(test_sites_GPP[[i]]$microcoleus_normalized - min(test_sites_GPP[[i]]$microcoleus_normalized)))
}
mean(nrmse) # 0.227; better than just autoregressive

# potential variables: discharge, conductivity, orthophosphate, nitrate, GPP

# conductivity normalized
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$microcoleus_normalized,
                   var1 = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$microcoleus_normalized, 
                              test_sites[[i]]$cond_uS_cm)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$microcoleus_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$microcoleus_normalized - min(test_sites[[i]]$microcoleus_normalized)))
}
mean(nrmse) # 0.228 better than just autoregressive

# orthophosphate; cover normalized
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$microcoleus_normalized,
                   var1 = training_sites[[i]]$oPhos_ug_P_L)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$microcoleus_normalized, 
                              test_sites[[i]]$oPhos_ug_P_L)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$microcoleus_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$microcoleus_normalized - min(test_sites[[i]]$microcoleus_normalized)))
}
mean(nrmse) # 0.233 better than just autoregressive

nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$microcoleus_normalized,
                   var1 = training_sites[[i]]$nitrate_mg_N_L)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$microcoleus_normalized, 
                              test_sites[[i]]$nitrate_mg_N_L)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$microcoleus_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$microcoleus_normalized - min(test_sites[[i]]$microcoleus_normalized)))
}
mean(nrmse) # 0.246 better than just autoregressive

## anabaena

# normalized autoregressive
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$anabaena_cylindrospermum_normalized)
  model <- stan(file = "autoregressive_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "sigma"))
  preds <- pred_out_autoregressive(params, test_sites[[i]]$anabaena_cylindrospermum_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$anabaena_cylindrospermum_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$anabaena_cylindrospermum_normalized - min(test_sites[[i]]$anabaena_cylindrospermum_normalized)))
}
mean(nrmse) # 0.269; worst site is SFE-M 2

# discharge; cover normalized per site_reach
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$anabaena_cylindrospermum_normalized,
                   var1 = training_sites[[i]]$discharge_m3_s_normalized)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$anabaena_cylindrospermum_normalized, 
                              test_sites[[i]]$discharge_m3_s_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$anabaena_cylindrospermum_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$anabaena_cylindrospermum_normalized - min(test_sites[[i]]$anabaena_cylindrospermum_normalized)))
}
mean(nrmse) # 0.267; getter! worse for SFE-M 4 but better for every other site I think

nrmse <- rep(NA, length(training_sites_GPP))
for(i in 1:length(training_sites_GPP)) {
  mod_data <- list(N = nrow(training_sites_GPP[[i]]), 
                   cover = training_sites_GPP[[i]]$anabaena_cylindrospermum_normalized,
                   var1 = training_sites_GPP[[i]]$GPP_mean_fourdaysprior_normalized)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites_GPP[[i]]$anabaena_cylindrospermum_normalized, 
                              test_sites_GPP[[i]]$GPP_mean_fourdaysprior_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites_GPP[[i]]$anabaena_cylindrospermum_normalized[-1])^2)/length(preds))) /
    (max(test_sites_GPP[[i]]$anabaena_cylindrospermum_normalized - min(test_sites_GPP[[i]]$anabaena_cylindrospermum_normalized)))
}
mean(nrmse) # 0.272; worse

# conductivity
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$anabaena_cylindrospermum_normalized,
                   var1 = training_sites[[i]]$cond_uS_cm)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$anabaena_cylindrospermum_normalized, 
                              test_sites[[i]]$cond_uS_cm)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$anabaena_cylindrospermum_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$anabaena_cylindrospermum_normalized - min(test_sites[[i]]$anabaena_cylindrospermum_normalized)))
}
mean(nrmse) # 0.277; worse

# ophos
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$anabaena_cylindrospermum_normalized,
                   var1 = training_sites[[i]]$oPhos_ug_P_L)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$anabaena_cylindrospermum_normalized, 
                              test_sites[[i]]$oPhos_ug_P_L)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$anabaena_cylindrospermum_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$anabaena_cylindrospermum_normalized - min(test_sites[[i]]$anabaena_cylindrospermum_normalized)))
}
mean(nrmse) # 0.272; worse

# nitrate
nrmse <- rep(NA, length(training_sites))
for(i in 1:length(training_sites)) {
  mod_data <- list(N = nrow(training_sites[[i]]), 
                   cover = training_sites[[i]]$anabaena_cylindrospermum_normalized,
                   var1 = training_sites[[i]]$nitrate_mg_N_L)
  model <- stan(file = "singlevar_cover.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites[[i]]$anabaena_cylindrospermum_normalized, 
                              test_sites[[i]]$nitrate_mg_N_L)
  nrmse[i] <- (sqrt(sum((preds - test_sites[[i]]$anabaena_cylindrospermum_normalized[-1])^2)/length(preds))) /
    (max(test_sites[[i]]$anabaena_cylindrospermum_normalized - min(test_sites[[i]]$anabaena_cylindrospermum_normalized)))
}
mean(nrmse) # 0.269; worse
  
#### atx models


## microcoleus
# just cover
nrmse <- rep(NA, length(training_sites_TM))
for(i in 1:length(training_sites_TM)) {
  mod_data <- list(N = nrow(training_sites_TM[[i]]), 
                   atx = training_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g,
                   cover = training_sites_TM[[i]]$microcoleus_normalized)
  model <- stan(file = "autoregressive_atx.stan", data = mod_data,
                chains = 3, iter = 3000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g, 
                              test_sites_TM[[i]]$microcoleus_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g[-1])^2)/length(preds))) /
    (max(test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g - min(test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g)))
}
mean(nrmse) # 0.461

# cover + conductivity
nrmse <- rep(NA, length(training_sites_TM))
for(i in 1:length(training_sites_TM)) {
  mod_data <- list(N = nrow(training_sites_TM[[i]]), 
                   atx = training_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g,
                   cover = training_sites_TM[[i]]$microcoleus_normalized, 
                   var1 = training_sites_TM[[i]]$cond_uS_cm)
  model <- stan(file = "singlevar_atx.stan", data = mod_data,
                chains = 3, iter = 2000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "b3", "sigma"))
  preds <- pred_out_doublevar(params, test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g, 
                              test_sites_TM[[i]]$microcoleus_normalized,
                              test_sites_TM[[i]]$cond_uS_cm)
  nrmse[i] <- (sqrt(sum((preds - test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g[-1])^2)/length(preds))) /
    (max(test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g - min(test_sites_TM[[i]]$TM_ATX_all_ug_orgmat_g)))
}
mean(nrmse) #0.505

## anabaena
nrmse <- rep(NA, length(training_sites_TAC))
for(i in 1:length(training_sites_TAC)) {
  mod_data <- list(N = nrow(training_sites_TAC[[i]]), 
                   atx = training_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g,
                   cover = training_sites_TAC[[i]]$anabaena_cylindrospermum_normalized)
  model <- stan(file = "autoregressive_atx.stan", data = mod_data,
                chains = 3, iter = 3000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "sigma"))
  preds <- pred_out_singlevar(params, test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g, 
                              test_sites_TAC[[i]]$anabaena_cylindrospermum_normalized)
  nrmse[i] <- (sqrt(sum((preds - test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g[-1])^2)/length(preds))) /
    (max(test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g - min(test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g)))
}
mean(nrmse) #0.371


nrmse <- rep(NA, length(training_sites_TAC))
for(i in 1:length(training_sites_TAC)) {
  mod_data <- list(N = nrow(training_sites_TAC[[i]]), 
                   atx = training_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g,
                   cover = training_sites_TAC[[i]]$anabaena_cylindrospermum_normalized,
                   var1 = training_sites_TAC[[i]]$cond_uS_cm)
  model <- stan(file = "singlevar_atx.stan", data = mod_data,
                chains = 3, iter = 3000, warmup = 1000)
  params <- rstan::extract(model, c("b0", "b1", "b2", "b3", "sigma"))
  preds <- pred_out_doublevar(params, test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g, 
                              test_sites_TAC[[i]]$anabaena_cylindrospermum_normalized,
                              test_sites_TAC[[i]]$cond_uS_cm)
  nrmse[i] <- (sqrt(sum((preds - test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g[-1])^2)/length(preds))) /
    (max(test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g - min(test_sites_TAC[[i]]$TAC_ATX_all_ug_orgmat_g)))
}
mean(nrmse) #0.414
