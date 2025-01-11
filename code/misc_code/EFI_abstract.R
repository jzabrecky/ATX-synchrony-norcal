#### EFI abstract code

# REDO METAB PROCESSING TO INCLUDE DISCHARGE AND DEPTH :)

## initial passes at predictive modeling
library(tidyverse)
library(zoo)
library(randomForest)
library(factoextra)
library(rstan)
library(StanHeaders)

# read in data

# for WHATEVER reason- there is some duplicates of 9.12.24 that need to be looked into
sfkeel23 <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  mutate(field_date = ymd(field_date))
metab_sfkeelmir_23 <- read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2023_metab.csv") %>% 
  rename(field_date = date) %>% 
  mutate(field_date = ymd(field_date))
metab_sfkeelsth_23 <- read.csv("./data/metab_model_outputs_processed/sfkeel_sth_2023_metab.csv") %>% 
  rename(field_date = date) %>% 
  mutate(field_date = ymd(field_date))

## interpolate any missing 
# get sequence of dates
dates <- data.frame(seq(ymd('2023-06-25'), ymd('2023-09-26'), by = 'day'))
colnames(dates) <- c("field_date")

# left join (only need to for sth 2023)
metab_sfkeelsth_23 <- left_join(dates, metab_sfkeelsth_23, by = "field_date")


# looks like we have all dates for south fork eel 2023 miranda but not standish
metab_sfkeelsth_23$GPP.mean <- na.approx(metab_sfkeelsth_23$GPP.mean)
metab_sfkeelsth_23$discharge_m3_s <- na.approx(metab_sfkeelsth_23$discharge_m3_s)

# probably will want to normalize GPP per site

# get field dates and merge onto metabolism df
visits_mir <- sfkeel23 %>% 
  dplyr::filter(site == "SFE-M") %>% 
  select(field_date) %>% 
  unique() # we have multiple reaches

visits_mir <- data.frame(visits_mir$field_date)
colnames(visits_mir) <- c("field_date")

visits_mir$visit <- seq(1, nrow(visits_mir))

visits_sth <- sfkeel23 %>% 
  dplyr::filter(site == "SFE-SH") %>% 
  select(field_date)

visits_sth <- data.frame(unique(visits_sth$field_date))
colnames(visits_sth) <- c("field_date")

visits_sth$visit <- seq(1, nrow(visits_sth))

## get average and mean GPP five days prior to visit (including day of?)

# left merge back onto metab
metab_sfkeelmir_23 <- left_join(metab_sfkeelmir_23, visits_mir, by = "field_date")
metab_sfkeelsth_23 <- left_join(metab_sfkeelsth_23, visits_sth, by = "field_date")

# trial run!

# not going to get prior metab for first date
mod_metab_sfkeelmir_23 <- metab_sfkeelmir_23[-1:-2,]
index <- which(!is.na(mod_metab_sfkeelmir_23$visit))

# doing 4 days beforehand
mir <- index %>% 
  purrr::map(. , function(x) mod_metab_sfkeelmir_23$GPP.mean[(x-4):(x-1)])

names(mir) <- seq(1:length(mir))

final_mir <- data.frame()
for(i in 1:length(mir)) {
  temp <- data.frame(mir[[i]])
  colnames(temp) <- c("metab")
  temp$index <- (i+1)
  final_mir <- rbind(final_mir, temp)
}

final_mir <- final_mir %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(mean_GPP_fourdaysprior = mean(metab)) %>% 
  rename(visit = index)

# repeat for sth

# not going to get prior metab for first date
mod_metab_sfkeelsth_23 <- metab_sfkeelsth_23[-1,]
index_sth <- which(!is.na(mod_metab_sfkeelsth_23$visit))

# doing 4 days beforehand
sth <- index_sth %>% 
  purrr::map(. , function(x) mod_metab_sfkeelsth_23$GPP.mean[(x-4):(x-1)])

names(sth) <- seq(1:length(sth))

final_sth <- data.frame()
for(i in 1:length(sth)) {
  temp <- data.frame(sth[[i]])
  colnames(temp) <- c("metab")
  temp$index <- (i+1)
  final_sth <- rbind(final_sth, temp)
}

final_sth <- final_sth %>% 
  dplyr::group_by(index) %>% 
  dplyr::summarize(mean_GPP_fourdaysprior = mean(metab)) %>% 
  rename(visit = index)

# left join back onto visits dataframe
metab_visits <- left_join(visits_mir, final_mir, by = "visit")
metab_visits_sth <- left_join(visits_sth, final_sth, by = "visit")

# merge back onto dataframe for site
sfkeel23_mir <- sfkeel23 %>% 
  filter(site == "SFE-M") %>% 
  unique()

sfkeel23_sth <- sfkeel23 %>% 
  filter(site == "SFE-SH") %>% 
  unique()

sfkeel23_mir <- left_join(sfkeel23_mir, metab_visits, by = c("field_date"))
sfkeel23_sth <- left_join(sfkeel23_sth, metab_visits_sth, by = c("field_date"))
# add in discharge
sfkeel23_mir <- left_join(sfkeel23_mir, metab_sfkeelmir_23 %>% select(field_date, discharge_m3_s),
                          by = c("field_date"))
sfkeel23_sth <- left_join(sfkeel23_sth, metab_sfkeelsth_23 %>% select(field_date, discharge_m3_s),
                          by = c("field_date"))

#### getting prior microcoleus cover and anabaena/cyl cover

sfkeel23_mir_list <- split(sfkeel23_mir, sfkeel23_mir$site_reach)

# would probably just join into a single list in final
# iterate through list of miranda
for(i in 1:length(sfkeel23_mir_list)) {
  sfkeel23_mir_list[[i]]$prior_microcoleus <- rep(NA, nrow(sfkeel23_mir_list[[i]]))
  sfkeel23_mir_list[[i]]$prior_anacyl <- rep(NA, nrow(sfkeel23_mir_list[[i]]))
  for(j in 2:nrow(sfkeel23_mir_list[[i]])) {
    sfkeel23_mir_list[[i]]$prior_microcoleus[j] <- sfkeel23_mir_list[[i]]$microcoleus[j-1]
    sfkeel23_mir_list[[i]]$prior_anacyl[j] <- sfkeel23_mir_list[[i]]$anabaena_cylindrospermum[j-1]
  }
  sfkeel23_mir_list[[i]] <- sfkeel23_mir_list[[i]][-1,]
}

sfkeel23_sth$prior_microcoleus <- rep(NA, nrow(sfkeel23_sth))
sfkeel23_sth$prior_anacyl <- rep(NA, nrow(sfkeel23_sth))
for(i in 2:nrow(sfkeel23_sth)) {
  sfkeel23_sth$prior_microcoleus[i] <- sfkeel23_sth$microcoleus[i]
  sfkeel23_sth$prior_anacyl[i] <- sfkeel23_sth$anabaena_cylindrospermum[i]
}

# remove first row (already done for miranda above)
sfkeel23_sth <- sfkeel23_sth[-1,]

## list and then reduce like w/ miniDOT
sfkeel23_all_final <- rbind(sfkeel23_mir_list$`SFE-M-1S`, sfkeel23_mir_list$`SFE-M-2`,
                          sfkeel23_mir_list$`SFE-M-3`, sfkeel23_mir_list$`SFE-M-4`,
                          sfkeel23_sth) %>% 
  select(!visit)

#### PRELIM DATA ANALYSES ####

## normalize covariates? could do a function and run it through;
## NORMALIZE FOR LARGER SITE?

# hist-- all log-normal
hist(sfkeel23_all_final$microcoleus, breaks = 8) # log normal
hist(sfkeel23_all_final$anabaena_cylindrospermum, breaks = 8)
hist(sfkeel23_all_final$TAC_ATX_all_ug_orgmat_g, breaks = 8)
hist(sfkeel23_all_final$TM_ATX_all_ug_orgmat_g, breaks = 8)

# look at project for ecoforecasting for this?

# separate covars from site_reach / field_date
covars <- sfkeel23_all_final[4:ncol(sfkeel23_all_final)]

# run random forest- recheck with discharge data
micro_pred <- randomForest(microcoleus ~ ., data = covars, importance=TRUE)
anacyl_pred <- randomForest(anabaena_cylindrospermum ~ ., data = covars, importance = TRUE) 
tm_atx_pred <- randomForest(TM_ATX_all_ug_orgmat_g ~ ., data = covars, importance = TRUE) 
tac_atx_pred <- randomForest(TAC_ATX_all_ug_orgmat_g ~ ., data = covars, importance = TRUE)

# display variable importance data
plot(micro_pred)
varImpPlot(micro_pred) # prior microcoleus, depth, green algae, conductivity
# note probably need to normalize everything-- reference depth code
plot(anacyl_pred)
varImpPlot(anacyl_pred) # prior ancyl, bare_biofilm, temp_C, median_depth_cm, nitrate, pH

plot(tm_atx_pred) # cover- but if cover is 0 can we even predict mat concentrations?
varImpPlot(tm_atx_pred) # cond, microcoleus, ophos, nfixers, chla of mat, nitrate, tdc

plot(tac_atx_pred)
varImpPlot(tac_atx_pred) # anacyl, temp, cond 

covars_PCA <- covars[,]
atx_PCA <- prcomp(covars_PCA, scale = TRUE)

#Show the percentage of variances explained by each principal component.
fviz_eig(atx_PCA) # elbow at like 3-5

#Positive correlated variables point to the same side of the plot.
#Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(atx_PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

## normalize/z-score
sfkeel23_all_final <- sfkeel23_all_final %>% 
  relocate(TAC_ATX_all_ug_orgmat_g, .after = site) %>% 
  relocate(TM_ATX_all_ug_orgmat_g, .after = site) %>% 
  relocate(anabaena_cylindrospermum, .after = site) %>% 
  relocate(microcoleus, .after = site) %>% 
  select(!TM_ATX_all_ug_g) %>% 
  select(!TAC_ATX_all_ug_g) %>% 
  select(!TM_ATX_all_ug_chla_ug) %>% 
  select(!TAC_ATX_all_ug_chla_ug)

# meh may just not for now
# McAllister et al. 2018 log-transforms the nutrients and root-squares the discharge

#### FINAL: MODELING ITERATION STEPS ####

# separate out df for site
pred_M_1s <- sfkeel23_all_final %>% 
  filter(site_reach != "SFE-M-1S")
pred_M_2 <- sfkeel23_all_final %>% 
  filter(site_reach != "SFE-M-2")
pred_M_3 <- sfkeel23_all_final %>% 
  filter(site_reach != "SFE-M-3")
pred_M_4 <- sfkeel23_all_final %>% 
  filter(site_reach != "SFE-M-4")
pred_SH_1s <- sfkeel23_all_final %>% 
  filter(site_reach != "SFE-SH-1S")

M_1s <- sfkeel23_all_final %>% 
  filter(site_reach == "SFE-M-1S")
M_2 <- sfkeel23_all_final %>% 
  filter(site_reach == "SFE-M-2")
M_3 <- sfkeel23_all_final %>% 
  filter(site_reach == "SFE-M-3")
M_4 <- sfkeel23_all_final %>% 
  filter(site_reach == "SFE-M-4")
SH_1s <- sfkeel23_all_final %>% 
  filter(site_reach == "SFE-SH-1S")

# separate out later using unique?

## (1) Microcoleus occurence

setwd("./code/misc_code/EFI_prelim_STAN")

# look at joanna's notes

## micro occurrence
# just with prior occupancy (will list this out later); just for practice for now
# function to make predictions
PredOut_model1 <- function(params, prior_cover){
  n.pred <- (length(prior_cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 1:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + prior_cover[j] * params$b1[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP) 
}

PredOut_model7 <- function(params, prior_cover, GPP){
  n.pred <- (length(prior_cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 1:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + prior_cover[j] * params$b1[i] 
                 + GPP[j] * params$b2[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP) 
}

PredOut_model2 <- function(params, prior_cover, riffle_rapid, avg_depth){
  n.pred <- (length(prior_cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 1:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + prior_cover[j] * params$b1[i]
                 + riffle_rapid[j] * params$b2[i] + avg_depth[j] * params$b3[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP) 
}

PredOut_model3 <- function(params, prior_cover, ophos, amm, nitrate){
  n.pred <- (length(prior_cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 1:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + prior_cover[j] * params$b1[i]
                 + ophos[j] * params$b2[i] + amm[j] * params$b3[i]
                 + nitrate * params$b4[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP) 
}

# note that it will go through 13 rather than 14 whatever
PredOut_model4 <- function(params, cover, atx){
  n.pred <- (length(cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 2:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + atx[j-1] * params$b1[i]
                 + cover[j] * params$b2[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP) 
}

PredOut_model5 <- function(params, cover, atx, ophos, amm, nitrate){
  n.pred <- (length(cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 2:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + atx[j-1] * params$b1[i]
                 + cover[j] * params$b2[i] + ophos[j] * params$b3[i] +
                   amm[j] * params$b4[i] + nitrate[j] * params$b5[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP) 
}

PredOut_model6 <- function(params, cover, atx, GPP){
  n.pred <- (length(cover))
  Preds <- matrix(NA, length(params$b0), n.pred)
  
  for(i in 1:length(params$b0)){
    for(j in 2:n.pred){ 
      y <- rnorm(n = 1, mean = params$b0[i] + atx[j-1] * params$b1[i]
                 + cover[j] * params$b2[i] + GPP[j] * params$b3[i],
                 sd = params$sigma[i]) #Process error
      Preds[i,j] <- y
    }
  }
  
  MeanP <- apply(Preds,2, mean)
  return(MeanP) 
}

model1_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         prior_cover = pred_M_1s$prior_microcoleus)
model1_M_1S <- stan(file = "cover_model1.stan", data = model1_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model1_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$microcoleus,
                         prior_cover = pred_M_2$prior_microcoleus)
model1_M_2 <- stan(file = "cover_model1.stan", data = model1_M_2_data,
                    chains = 3, iter = 2000, warmup = 1000)
model1_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$microcoleus,
                         prior_cover = pred_M_3$prior_microcoleus)
model1_M_3 <- stan(file = "cover_model1.stan", data = model1_M_3_data,
                    chains = 3, iter = 2000, warmup = 1000)
model1_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$microcoleus,
                         prior_cover = pred_M_4$prior_microcoleus)
model1_M_4 <- stan(file = "cover_model1.stan", data = model1_M_4_data,
                    chains = 3, iter = 2000, warmup = 1000)
model1_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$microcoleus,
                         prior_cover = pred_SH_1s$prior_microcoleus)
model1_SH_1S <- stan(file = "cover_model1.stan", data = model1_SH_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model1_M_1S, c("b0", "b1", "sigma"))
params2 <- rstan::extract(model1_M_2, c("b0", "b1", "sigma"))
params3 <- rstan::extract(model1_M_3, c("b0", "b1", "sigma"))
params4 <- rstan::extract(model1_M_4, c("b0", "b1", "sigma"))
params5 <- rstan::extract(model1_SH_1S, c("b0", "b1", "sigma"))

preds1 <- PredOut_model1(params1, M_1s$prior_microcoleus)
preds2 <- PredOut_model1(params2, M_2$prior_microcoleus)
preds3 <- PredOut_model1(params3, M_3$prior_microcoleus)
preds4 <- PredOut_model1(params4, M_4$prior_microcoleus)
preds5 <- PredOut_model1(params5, SH_1s$prior_microcoleus)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$microcoleus)^2)/length(preds1))) / (max(M_1s$microcoleus - min(M_1s$microcoleus)))
nrmse2 <- (sqrt(sum((preds2 - M_2$microcoleus)^2)/length(preds2))) / (max(M_2$microcoleus - min(M_2$microcoleus)))
nrmse3 <- (sqrt(sum((preds3 - M_3$microcoleus)^2)/length(preds3))) / (max(M_3$microcoleus - min(M_3$microcoleus)))
nrmse4 <- (sqrt(sum((preds4 - M_4$microcoleus)^2)/length(preds4))) / (max(M_4$microcoleus - min(M_4$microcoleus)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$microcoleus)^2)/length(preds5))) / (max(SH_1s$microcoleus - min(SH_1s$microcoleus)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.197
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.114
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.357
# worst at predicting site 4

## micro occurence w/ GPP
model7_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         prior_cover = pred_M_1s$prior_microcoleus, GPP = pred_M_1s$mean_GPP_fourdaysprior)
model7_M_1S <- stan(file = "cover_model7.stan", data = model7_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model7_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$microcoleus,
                        prior_cover = pred_M_2$prior_microcoleus, GPP = pred_M_2$mean_GPP_fourdaysprior)
model7_M_2 <- stan(file = "cover_model7.stan", data = model7_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model7_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$microcoleus,
                        prior_cover = pred_M_3$prior_microcoleus, GPP = pred_M_3$mean_GPP_fourdaysprior)
model7_M_3 <- stan(file = "cover_model7.stan", data = model7_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model7_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$microcoleus,
                        prior_cover = pred_M_4$prior_microcoleus, GPP = pred_M_4$mean_GPP_fourdaysprior)
model7_M_4 <- stan(file = "cover_model7.stan", data = model7_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model7_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$microcoleus,
                          prior_cover = pred_SH_1s$prior_microcoleus, GPP = pred_SH_1s$mean_GPP_fourdaysprior)
model7_SH_1S <- stan(file = "cover_model7.stan", data = model7_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model7_M_1S, c("b0", "b1", "b2", "sigma"))
params2 <- rstan::extract(model7_M_2, c("b0", "b1", "b2", "sigma"))
params3 <- rstan::extract(model7_M_3, c("b0", "b1", "b2", "sigma"))
params4 <- rstan::extract(model7_M_4, c("b0", "b1", "b2", "sigma"))
params5 <- rstan::extract(model7_SH_1S, c("b0", "b1", "b2", "sigma"))

preds1 <- PredOut_model7(params1, M_1s$prior_microcoleus, M_1s$mean_GPP_fourdaysprior)
preds2 <- PredOut_model7(params2, M_2$prior_microcoleus, M_2$mean_GPP_fourdaysprior)
preds3 <- PredOut_model7(params3, M_3$prior_microcoleus, M_3$mean_GPP_fourdaysprior)
preds4 <- PredOut_model7(params4, M_4$prior_microcoleus, M_4$mean_GPP_fourdaysprior)
preds5 <- PredOut_model7(params5, SH_1s$prior_microcoleus, SH_1s$mean_GPP_fourdaysprior)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$microcoleus)^2)/length(preds1))) / (max(M_1s$microcoleus - min(M_1s$microcoleus)))
nrmse2 <- (sqrt(sum((preds2 - M_2$microcoleus)^2)/length(preds2))) / (max(M_2$microcoleus - min(M_2$microcoleus)))
nrmse3 <- (sqrt(sum((preds3 - M_3$microcoleus)^2)/length(preds3))) / (max(M_3$microcoleus - min(M_3$microcoleus)))
nrmse4 <- (sqrt(sum((preds4 - M_4$microcoleus)^2)/length(preds4))) / (max(M_4$microcoleus - min(M_4$microcoleus)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$microcoleus)^2)/length(preds5))) / (max(SH_1s$microcoleus - min(SH_1s$microcoleus)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.216
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.161
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.359
# again bad at site 4

## micro occurence w/ geomorphic
model2_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         prior_cover = pred_M_1s$prior_microcoleus, 
                         rapid_riffle = pred_M_1s$proportion_riffle_rapid_transects,
                         avg_depth = pred_M_1s$average_depth_cm_sampled)
model2_M_1S <- stan(file = "cover_model2.stan", data = model2_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model2_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$microcoleus,
                        prior_cover = pred_M_2$prior_microcoleus,
                        rapid_riffle = pred_M_2$proportion_riffle_rapid_transects,
                        avg_depth = pred_M_2$average_depth_cm_sampled)
model2_M_2 <- stan(file = "cover_model2.stan", data = model2_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model2_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$microcoleus,
                        prior_cover = pred_M_3$prior_microcoleus,
                        rapid_riffle = pred_M_3$proportion_riffle_rapid_transects,
                        avg_depth = pred_M_3$average_depth_cm_sampled)
model2_M_3 <- stan(file = "cover_model2.stan", data = model2_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model2_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$microcoleus,
                        prior_cover = pred_M_4$prior_microcoleus,
                        rapid_riffle = pred_M_4$proportion_riffle_rapid_transects,
                        avg_depth = pred_M_4$average_depth_cm_sampled)
model2_M_4 <- stan(file = "cover_model2.stan", data = model2_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model2_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$microcoleus,
                          prior_cover = pred_SH_1s$prior_microcoleus,
                          rapid_riffle = pred_SH_1s$proportion_riffle_rapid_transects,
                          avg_depth = pred_SH_1s$average_depth_cm_sampled)
model2_SH_1S <- stan(file = "cover_model2.stan", data = model2_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model2_M_1S, c("b0", "b1", "b2", "b3", "sigma"))
params2 <- rstan::extract(model2_M_2, c("b0", "b1", "b2", "b3", "sigma"))
params3 <- rstan::extract(model2_M_3, c("b0", "b1", "b2", "b3", "sigma"))
params4 <- rstan::extract(model2_M_4, c("b0", "b1", "b2", "b3", "sigma"))
params5 <- rstan::extract(model2_SH_1S, c("b0", "b1", "b2", "b3", "sigma"))

preds1 <- PredOut_model2(params1, M_1s$prior_microcoleus, M_1s$proportion_riffle_rapid_transects, M_1s$average_depth_cm_sampled)
preds2 <- PredOut_model2(params2, M_2$prior_microcoleus, M_2$proportion_riffle_rapid_transects, M_2$average_depth_cm_sampled)
preds3 <- PredOut_model2(params3, M_3$prior_microcoleus, M_3$proportion_riffle_rapid_transects, M_3$average_depth_cm_sampled)
preds4 <- PredOut_model2(params4, M_4$prior_microcoleus, M_4$proportion_riffle_rapid_transects, M_4$average_depth_cm_sampled)
preds5 <- PredOut_model2(params5, SH_1s$prior_microcoleus, SH_1s$proportion_riffle_rapid_transects, SH_1s$average_depth_cm_sampled)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$microcoleus)^2)/length(preds1))) / (max(M_1s$microcoleus - min(M_1s$microcoleus)))
nrmse2 <- (sqrt(sum((preds2 - M_2$microcoleus)^2)/length(preds2))) / (max(M_2$microcoleus - min(M_2$microcoleus)))
nrmse3 <- (sqrt(sum((preds3 - M_3$microcoleus)^2)/length(preds3))) / (max(M_3$microcoleus - min(M_3$microcoleus)))
nrmse4 <- (sqrt(sum((preds4 - M_4$microcoleus)^2)/length(preds4))) / (max(M_4$microcoleus - min(M_4$microcoleus)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$microcoleus)^2)/length(preds5))) / (max(SH_1s$microcoleus - min(SH_1s$microcoleus)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.205
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.103
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.358

## micro occurence w/ water quality
model3_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         prior_cover = pred_M_1s$prior_microcoleus, 
                         ophos = pred_M_1s$oPhos_ug_P_L,
                         amm = pred_M_1s$ammonium_mg_N_L,
                         nitrate = pred_M_1s$nitrate_mg_N_L)
model3_M_1S <- stan(file = "cover_model3.stan", data = model3_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model3_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$microcoleus,
                        prior_cover = pred_M_2$prior_microcoleus,
                        ophos = pred_M_2$oPhos_ug_P_L,
                        amm = pred_M_2$ammonium_mg_N_L,
                        nitrate = pred_M_2$nitrate_mg_N_L)
model3_M_2 <- stan(file = "cover_model3.stan", data = model3_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model3_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$microcoleus,
                        prior_cover = pred_M_3$prior_microcoleus,
                        ophos = pred_M_3$oPhos_ug_P_L,
                        amm = pred_M_3$ammonium_mg_N_L,
                        nitrate = pred_M_3$nitrate_mg_N_L)
model3_M_3 <- stan(file = "cover_model3.stan", data = model3_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model3_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$microcoleus,
                        prior_cover = pred_M_4$prior_microcoleus,
                        ophos = pred_M_4$oPhos_ug_P_L,
                        amm = pred_M_4$ammonium_mg_N_L,
                        nitrate = pred_M_4$nitrate_mg_N_L)
model3_M_4 <- stan(file = "cover_model3.stan", data = model3_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model3_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$microcoleus,
                          prior_cover = pred_SH_1s$prior_microcoleus,
                          ophos = pred_SH_1s$oPhos_ug_P_L,
                          amm = pred_SH_1s$ammonium_mg_N_L,
                          nitrate = pred_SH_1s$nitrate_mg_N_L)
model3_SH_1S <- stan(file = "cover_model3.stan", data = model3_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model3_M_1S, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params2 <- rstan::extract(model3_M_2, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params3 <- rstan::extract(model3_M_3, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params4 <- rstan::extract(model3_M_4, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params5 <- rstan::extract(model3_SH_1S, c("b0", "b1", "b2", "b3", "b4", "sigma"))

preds1 <- PredOut_model3(params1, M_1s$prior_microcoleus, M_1s$oPhos_ug_P_L, M_1s$ammonium_mg_N_L, M_1s$nitrate_mg_N_L)
preds2 <- PredOut_model3(params2, M_2$prior_microcoleus, M_2$oPhos_ug_P_L, M_2$ammonium_mg_N_L, M_2$nitrate_mg_N_L)
preds3 <- PredOut_model3(params3, M_3$prior_microcoleus, M_3$oPhos_ug_P_L, M_3$ammonium_mg_N_L, M_3$nitrate_mg_N_L)
preds4 <- PredOut_model3(params4, M_4$prior_microcoleus, M_4$oPhos_ug_P_L, M_4$ammonium_mg_N_L, M_4$nitrate_mg_N_L)
preds5 <- PredOut_model3(params5, SH_1s$prior_microcoleus, SH_1s$oPhos_ug_P_L, SH_1s$ammonium_mg_N_L, SH_1s$nitrate_mg_N_L)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$microcoleus)^2)/length(preds1))) / (max(M_1s$microcoleus - min(M_1s$microcoleus)))
nrmse2 <- (sqrt(sum((preds2 - M_2$microcoleus)^2)/length(preds2))) / (max(M_2$microcoleus - min(M_2$microcoleus)))
nrmse3 <- (sqrt(sum((preds3 - M_3$microcoleus)^2)/length(preds3))) / (max(M_3$microcoleus - min(M_3$microcoleus)))
nrmse4 <- (sqrt(sum((preds4 - M_4$microcoleus)^2)/length(preds4))) / (max(M_4$microcoleus - min(M_4$microcoleus)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$microcoleus)^2)/length(preds5))) / (max(SH_1s$microcoleus - min(SH_1s$microcoleus)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.194
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.101
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.356

## anabaena occurence w/ prior only

model1_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$anabaena_cylindrospermum,
                         prior_cover = pred_M_1s$prior_anacyl)
model1_M_1S <- stan(file = "cover_model1.stan", data = model1_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model1_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$anabaena_cylindrospermum,
                        prior_cover = pred_M_2$prior_anacyl)
model1_M_2 <- stan(file = "cover_model1.stan", data = model1_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model1_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$anabaena_cylindrospermum,
                        prior_cover = pred_M_3$prior_anacyl)
model1_M_3 <- stan(file = "cover_model1.stan", data = model1_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model1_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$anabaena_cylindrospermum,
                        prior_cover = pred_M_4$prior_anacyl)
model1_M_4 <- stan(file = "cover_model1.stan", data = model1_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model1_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$anabaena_cylindrospermum,
                          prior_cover = pred_SH_1s$prior_anacyl)
model1_SH_1S <- stan(file = "cover_model1.stan", data = model1_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model1_M_1S, c("b0", "b1", "sigma"))
params2 <- rstan::extract(model1_M_2, c("b0", "b1", "sigma"))
params3 <- rstan::extract(model1_M_3, c("b0", "b1", "sigma"))
params4 <- rstan::extract(model1_M_4, c("b0", "b1", "sigma"))
params5 <- rstan::extract(model1_SH_1S, c("b0", "b1", "sigma"))

preds1 <- PredOut_model1(params1, M_1s$prior_anacyl)
preds2 <- PredOut_model1(params2, M_2$prior_anacyl)
preds3 <- PredOut_model1(params3, M_3$prior_anacyl)
preds4 <- PredOut_model1(params4, M_4$prior_anacyl)
preds5 <- PredOut_model1(params5, SH_1s$prior_anacyl)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$anabaena_cylindrospermum)^2)/length(preds1))) / (max(M_1s$anabaena_cylindrospermum - min(M_1s$anabaena_cylindrospermum)))
nrmse2 <- (sqrt(sum((preds2 - M_2$anabaena_cylindrospermum)^2)/length(preds2))) / (max(M_2$anabaena_cylindrospermum - min(M_2$anabaena_cylindrospermum)))
nrmse3 <- (sqrt(sum((preds3 - M_3$anabaena_cylindrospermum)^2)/length(preds3))) / (max(M_3$anabaena_cylindrospermum - min(M_3$anabaena_cylindrospermum)))
nrmse4 <- (sqrt(sum((preds4 - M_4$anabaena_cylindrospermum)^2)/length(preds4))) / (max(M_4$anabaena_cylindrospermum - min(M_4$anabaena_cylindrospermum)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$anabaena_cylindrospermum)^2)/length(preds5))) / (max(SH_1s$anabaena_cylindrospermum - min(SH_1s$anabaena_cylindrospermum)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.590
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.244
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 1.784

## anabaena occurence w/ GPP
model7_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$anabaena_cylindrospermum,
                         prior_cover = pred_M_1s$prior_anacyl, GPP = pred_M_1s$mean_GPP_fourdaysprior)
model7_M_1S <- stan(file = "cover_model7.stan", data = model7_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model7_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$anabaena_cylindrospermum,
                        prior_cover = pred_M_2$prior_anacyl, GPP = pred_M_2$mean_GPP_fourdaysprior)
model7_M_2 <- stan(file = "cover_model7.stan", data = model7_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model7_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$anabaena_cylindrospermum,
                        prior_cover = pred_M_3$prior_anacyl, GPP = pred_M_3$mean_GPP_fourdaysprior)
model7_M_3 <- stan(file = "cover_model7.stan", data = model7_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model7_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$anabaena_cylindrospermum,
                        prior_cover = pred_M_4$prior_anacyl, GPP = pred_M_4$mean_GPP_fourdaysprior)
model7_M_4 <- stan(file = "cover_model7.stan", data = model7_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model7_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$anabaena_cylindrospermum,
                          prior_cover = pred_SH_1s$prior_anacyl, GPP = pred_SH_1s$mean_GPP_fourdaysprior)
model7_SH_1S <- stan(file = "cover_model7.stan", data = model7_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model7_M_1S, c("b0", "b1", "b2", "sigma"))
params2 <- rstan::extract(model7_M_2, c("b0", "b1", "b2", "sigma"))
params3 <- rstan::extract(model7_M_3, c("b0", "b1", "b2", "sigma"))
params4 <- rstan::extract(model7_M_4, c("b0", "b1", "b2", "sigma"))
params5 <- rstan::extract(model7_SH_1S, c("b0", "b1", "b2", "sigma"))

preds1 <- PredOut_model7(params1, M_1s$prior_anacyl, M_1s$mean_GPP_fourdaysprior)
preds2 <- PredOut_model7(params2, M_2$prior_anacyl, M_2$mean_GPP_fourdaysprior)
preds3 <- PredOut_model7(params3, M_3$prior_anacyl, M_3$mean_GPP_fourdaysprior)
preds4 <- PredOut_model7(params4, M_4$prior_anacyl, M_4$mean_GPP_fourdaysprior)
preds5 <- PredOut_model7(params5, SH_1s$prior_anacyl, SH_1s$mean_GPP_fourdaysprior)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$anabaena_cylindrospermum)^2)/length(preds1))) / (max(M_1s$anabaena_cylindrospermum - min(M_1s$anabaena_cylindrospermum)))
nrmse2 <- (sqrt(sum((preds2 - M_2$anabaena_cylindrospermum)^2)/length(preds2))) / (max(M_2$anabaena_cylindrospermum - min(M_2$anabaena_cylindrospermum)))
nrmse3 <- (sqrt(sum((preds3 - M_3$anabaena_cylindrospermum)^2)/length(preds3))) / (max(M_3$anabaena_cylindrospermum - min(M_3$anabaena_cylindrospermum)))
nrmse4 <- (sqrt(sum((preds4 - M_4$anabaena_cylindrospermum)^2)/length(preds4))) / (max(M_4$anabaena_cylindrospermum - min(M_4$anabaena_cylindrospermum)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$anabaena_cylindrospermum)^2)/length(preds5))) / (max(SH_1s$anabaena_cylindrospermum - min(SH_1s$anabaena_cylindrospermum)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.967
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.243
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 2.366

## anabaen occurence w/ geomorphic
model2_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$anabaena_cylindrospermum,
                         prior_cover = pred_M_1s$prior_anacyl, 
                         rapid_riffle = pred_M_1s$proportion_riffle_rapid_transects,
                         avg_depth = pred_M_1s$average_depth_cm_sampled)
model2_M_1S <- stan(file = "cover_model2.stan", data = model2_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model2_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$anabaena_cylindrospermum,
                        prior_cover = pred_M_2$prior_anacyl,
                        rapid_riffle = pred_M_2$proportion_riffle_rapid_transects,
                        avg_depth = pred_M_2$average_depth_cm_sampled)
model2_M_2 <- stan(file = "cover_model2.stan", data = model2_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model2_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$anabaena_cylindrospermum,
                        prior_cover = pred_M_3$prior_anacyl,
                        rapid_riffle = pred_M_3$proportion_riffle_rapid_transects,
                        avg_depth = pred_M_3$average_depth_cm_sampled)
model2_M_3 <- stan(file = "cover_model2.stan", data = model2_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model2_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$anabaena_cylindrospermum,
                        prior_cover = pred_M_4$prior_anacyl,
                        rapid_riffle = pred_M_4$proportion_riffle_rapid_transects,
                        avg_depth = pred_M_4$average_depth_cm_sampled)
model2_M_4 <- stan(file = "cover_model2.stan", data = model2_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model2_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$anabaena_cylindrospermum,
                          prior_cover = pred_SH_1s$prior_anacyl,
                          rapid_riffle = pred_SH_1s$proportion_riffle_rapid_transects,
                          avg_depth = pred_SH_1s$average_depth_cm_sampled)
model2_SH_1S <- stan(file = "cover_model2.stan", data = model2_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model2_M_1S, c("b0", "b1", "b2", "b3", "sigma"))
params2 <- rstan::extract(model2_M_2, c("b0", "b1", "b2", "b3", "sigma"))
params3 <- rstan::extract(model2_M_3, c("b0", "b1", "b2", "b3", "sigma"))
params4 <- rstan::extract(model2_M_4, c("b0", "b1", "b2", "b3", "sigma"))
params5 <- rstan::extract(model2_SH_1S, c("b0", "b1", "b2", "b3", "sigma"))

preds1 <- PredOut_model2(params1, M_1s$prior_anacyl, M_1s$proportion_riffle_rapid_transects, M_1s$average_depth_cm_sampled)
preds2 <- PredOut_model2(params2, M_2$prior_anacyl, M_2$proportion_riffle_rapid_transects, M_2$average_depth_cm_sampled)
preds3 <- PredOut_model2(params3, M_3$prior_anacyl, M_3$proportion_riffle_rapid_transects, M_3$average_depth_cm_sampled)
preds4 <- PredOut_model2(params4, M_4$prior_anacyl, M_4$proportion_riffle_rapid_transects, M_4$average_depth_cm_sampled)
preds5 <- PredOut_model2(params5, SH_1s$prior_anacyl, SH_1s$proportion_riffle_rapid_transects, SH_1s$average_depth_cm_sampled)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$anabaena_cylindrospermum)^2)/length(preds1))) / (max(M_1s$anabaena_cylindrospermum - min(M_1s$anabaena_cylindrospermum)))
nrmse2 <- (sqrt(sum((preds2 - M_2$anabaena_cylindrospermum)^2)/length(preds2))) / (max(M_2$anabaena_cylindrospermum - min(M_2$anabaena_cylindrospermum)))
nrmse3 <- (sqrt(sum((preds3 - M_3$anabaena_cylindrospermum)^2)/length(preds3))) / (max(M_3$anabaena_cylindrospermum - min(M_3$anabaena_cylindrospermum)))
nrmse4 <- (sqrt(sum((preds4 - M_4$anabaena_cylindrospermum)^2)/length(preds4))) / (max(M_4$anabaena_cylindrospermum - min(M_4$anabaena_cylindrospermum)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$anabaena_cylindrospermum)^2)/length(preds5))) / (max(SH_1s$anabaena_cylindrospermum - min(SH_1s$anabaena_cylindrospermum)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.426
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.244
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.798

## anabaena occurence w/ water quality
model3_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$anabaena_cylindrospermum,
                         prior_cover = pred_M_1s$prior_anacyl, 
                         ophos = pred_M_1s$oPhos_ug_P_L,
                         amm = pred_M_1s$ammonium_mg_N_L,
                         nitrate = pred_M_1s$nitrate_mg_N_L)
model3_M_1S <- stan(file = "cover_model3.stan", data = model3_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model3_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$anabaena_cylindrospermum,
                        prior_cover = pred_M_2$prior_anacyl,
                        ophos = pred_M_2$oPhos_ug_P_L,
                        amm = pred_M_2$ammonium_mg_N_L,
                        nitrate = pred_M_2$nitrate_mg_N_L)
model3_M_2 <- stan(file = "cover_model3.stan", data = model3_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model3_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$anabaena_cylindrospermum,
                        prior_cover = pred_M_3$prior_anacyl,
                        ophos = pred_M_3$oPhos_ug_P_L,
                        amm = pred_M_3$ammonium_mg_N_L,
                        nitrate = pred_M_3$nitrate_mg_N_L)
model3_M_3 <- stan(file = "cover_model3.stan", data = model3_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model3_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$anabaena_cylindrospermum,
                        prior_cover = pred_M_4$prior_anacyl,
                        ophos = pred_M_4$oPhos_ug_P_L,
                        amm = pred_M_4$ammonium_mg_N_L,
                        nitrate = pred_M_4$nitrate_mg_N_L)
model3_M_4 <- stan(file = "cover_model3.stan", data = model3_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model3_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$anabaena_cylindrospermum,
                          prior_cover = pred_SH_1s$prior_anacyl,
                          ophos = pred_SH_1s$oPhos_ug_P_L,
                          amm = pred_SH_1s$ammonium_mg_N_L,
                          nitrate = pred_SH_1s$nitrate_mg_N_L)
model3_SH_1S <- stan(file = "cover_model3.stan", data = model3_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model3_M_1S, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params2 <- rstan::extract(model3_M_2, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params3 <- rstan::extract(model3_M_3, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params4 <- rstan::extract(model3_M_4, c("b0", "b1", "b2", "b3", "b4", "sigma"))
params5 <- rstan::extract(model3_SH_1S, c("b0", "b1", "b2", "b3", "b4", "sigma"))

preds1 <- PredOut_model3(params1, M_1s$prior_anacyl, M_1s$oPhos_ug_P_L, M_1s$ammonium_mg_N_L, M_1s$nitrate_mg_N_L)
preds2 <- PredOut_model3(params2, M_2$prior_anacyl, M_2$oPhos_ug_P_L, M_2$ammonium_mg_N_L, M_2$nitrate_mg_N_L)
preds3 <- PredOut_model3(params3, M_3$prior_anacyl, M_3$oPhos_ug_P_L, M_3$ammonium_mg_N_L, M_3$nitrate_mg_N_L)
preds4 <- PredOut_model3(params4, M_4$prior_anacyl, M_4$oPhos_ug_P_L, M_4$ammonium_mg_N_L, M_4$nitrate_mg_N_L)
preds5 <- PredOut_model3(params5, SH_1s$prior_anacyl, SH_1s$oPhos_ug_P_L, SH_1s$ammonium_mg_N_L, SH_1s$nitrate_mg_N_L)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1 - M_1s$anabaena_cylindrospermum)^2)/length(preds1))) / (max(M_1s$anabaena_cylindrospermum - min(M_1s$anabaena_cylindrospermum)))
nrmse2 <- (sqrt(sum((preds2 - M_2$anabaena_cylindrospermum)^2)/length(preds2))) / (max(M_2$anabaena_cylindrospermum - min(M_2$anabaena_cylindrospermum)))
nrmse3 <- (sqrt(sum((preds3 - M_3$anabaena_cylindrospermum)^2)/length(preds3))) / (max(M_3$anabaena_cylindrospermum - min(M_3$anabaena_cylindrospermum)))
nrmse4 <- (sqrt(sum((preds4 - M_4$anabaena_cylindrospermum)^2)/length(preds4))) / (max(M_4$anabaena_cylindrospermum - min(M_4$anabaena_cylindrospermum)))
nrmse5 <- (sqrt(sum((preds5 - SH_1s$anabaena_cylindrospermum)^2)/length(preds5))) / (max(SH_1s$anabaena_cylindrospermum - min(SH_1s$anabaena_cylindrospermum)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.699
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.240
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 2.191

#### ATX predictions (will just do t-1 for prior instead of separate variable)

## micro anatoxin with percent cover
model4_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         atx = pred_M_1s$TM_ATX_all_ug_orgmat_g)
model4_M_1S <- stan(file = "cover_model4.stan", data = model4_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model4_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$microcoleus,
                        atx = pred_M_2$TM_ATX_all_ug_orgmat_g)
model4_M_2 <- stan(file = "cover_model4.stan", data = model4_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model4_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$microcoleus,
                        atx = pred_M_3$TM_ATX_all_ug_orgmat_g)
model4_M_3 <- stan(file = "cover_model4.stan", data = model4_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model4_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$microcoleus,
                        atx = pred_M_4$TM_ATX_all_ug_orgmat_g)
model4_M_4 <- stan(file = "cover_model4.stan", data = model4_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model4_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$microcoleus,
                          atx = pred_SH_1s$TM_ATX_all_ug_orgmat_g)
model4_SH_1S <- stan(file = "cover_model4.stan", data = model4_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model4_M_1S, c("b0", "b1", "b2", "sigma"))
params2 <- rstan::extract(model4_M_2, c("b0", "b1", "b2", "sigma"))
params3 <- rstan::extract(model4_M_3, c("b0", "b1", "b2", "sigma"))
params4 <- rstan::extract(model4_M_4, c("b0", "b1", "b2", "sigma"))
params5 <- rstan::extract(model4_SH_1S, c("b0", "b1", "b2", "sigma"))

preds1 <- PredOut_model4(params1, M_1s$microcoleus, M_1s$TM_ATX_all_ug_orgmat_g)
preds2 <- PredOut_model4(params2, M_2$microcoleus, M_2$TM_ATX_all_ug_orgmat_g)
preds3 <- PredOut_model4(params3, M_3$microcoleus, M_3$TM_ATX_all_ug_orgmat_g)
preds4 <- PredOut_model4(params4, M_4$microcoleus, M_4$TM_ATX_all_ug_orgmat_g)
preds5 <- PredOut_model4(params5, SH_1s$microcoleus, SH_1s$TM_ATX_all_ug_orgmat_g)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1[-1] - M_1s$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds1)-1))) / (max(M_1s$TM_ATX_all_ug_orgmat_g - min(M_1s$TM_ATX_all_ug_orgmat_g)))
nrmse2 <- (sqrt(sum((preds2[-1] - M_2$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds2)-1))) / (max(M_2$TM_ATX_all_ug_orgmat_g - min(M_2$TM_ATX_all_ug_orgmat_g)))
nrmse3 <- (sqrt(sum((preds3[-1] - M_3$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds3)-1))) / (max(M_3$TM_ATX_all_ug_orgmat_g - min(M_3$TM_ATX_all_ug_orgmat_g)))
nrmse4 <- (sqrt(sum((preds4[-1] - M_4$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds4)-1))) / (max(M_4$TM_ATX_all_ug_orgmat_g - min(M_4$TM_ATX_all_ug_orgmat_g)))
nrmse5 <- (sqrt(sum((preds5[-1] - SH_1s$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds5)-1))) / (max(SH_1s$TM_ATX_all_ug_orgmat_g - min(SH_1s$TM_ATX_all_ug_orgmat_g)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.365
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.251
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.516

## micro anatoxin with water quality
model5_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         atx = pred_M_1s$TM_ATX_all_ug_orgmat_g, ophos = pred_M_1s$oPhos_ug_P_L,
                         amm = pred_M_1s$ammonium_mg_N_L, nitrate = pred_M_1s$nitrate_mg_N_L)
model5_M_1S <- stan(file = "cover_model5.stan", data = model5_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model5_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$microcoleus,
                        atx = pred_M_2$TM_ATX_all_ug_orgmat_g, ophos = pred_M_2$oPhos_ug_P_L,
                        amm = pred_M_2$ammonium_mg_N_L, nitrate = pred_M_2$nitrate_mg_N_L)
model5_M_2 <- stan(file = "cover_model5.stan", data = model5_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model5_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$microcoleus,
                        atx = pred_M_3$TM_ATX_all_ug_orgmat_g, ophos = pred_M_3$oPhos_ug_P_L,
                        amm = pred_M_3$ammonium_mg_N_L, nitrate = pred_M_3$nitrate_mg_N_L)
model5_M_3 <- stan(file = "cover_model5.stan", data = model5_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model5_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$microcoleus,
                        atx = pred_M_4$TM_ATX_all_ug_orgmat_g, ophos = pred_M_4$oPhos_ug_P_L,
                        amm = pred_M_4$ammonium_mg_N_L, nitrate = pred_M_4$nitrate_mg_N_L)
model5_M_4 <- stan(file = "cover_model5.stan", data = model5_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model5_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$microcoleus,
                          atx = pred_SH_1s$TM_ATX_all_ug_orgmat_g, ophos = pred_SH_1s$oPhos_ug_P_L,
                          amm = pred_SH_1s$ammonium_mg_N_L, nitrate = pred_SH_1s$nitrate_mg_N_L)
model5_SH_1S <- stan(file = "cover_model5.stan", data = model5_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model5_M_1S, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params2 <- rstan::extract(model5_M_2, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params3 <- rstan::extract(model5_M_3, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params4 <- rstan::extract(model5_M_4, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params5 <- rstan::extract(model5_SH_1S, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))

preds1 <- PredOut_model5(params1, M_1s$microcoleus, M_1s$TM_ATX_all_ug_orgmat_g, M_1s$oPhos_ug_P_L,
                         M_1s$ammonium_mg_N_L, M_1s$nitrate_mg_N_L)
preds2 <- PredOut_model5(params2, M_2$microcoleus, M_2$TM_ATX_all_ug_orgmat_g, M_2$oPhos_ug_P_L,
                         M_2$ammonium_mg_N_L, M_2$nitrate_mg_N_L)
preds3 <- PredOut_model5(params3, M_3$microcoleus, M_3$TM_ATX_all_ug_orgmat_g, M_3$oPhos_ug_P_L,
                         M_3$ammonium_mg_N_L, M_3$nitrate_mg_N_L)
preds4 <- PredOut_model5(params4, M_4$microcoleus, M_4$TM_ATX_all_ug_orgmat_g, M_4$oPhos_ug_P_L,
                         M_4$ammonium_mg_N_L, M_4$nitrate_mg_N_L)
preds5 <- PredOut_model5(params5, SH_1s$microcoleus, SH_1s$TM_ATX_all_ug_orgmat_g, SH_1s$oPhos_ug_P_L,
                         SH_1s$ammonium_mg_N_L, SH_1s$nitrate_mg_N_L)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1[-1] - M_1s$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds1)-1))) / (max(M_1s$TM_ATX_all_ug_orgmat_g - min(M_1s$TM_ATX_all_ug_orgmat_g)))
nrmse2 <- (sqrt(sum((preds2[-1] - M_2$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds2)-1))) / (max(M_2$TM_ATX_all_ug_orgmat_g - min(M_2$TM_ATX_all_ug_orgmat_g)))
nrmse3 <- (sqrt(sum((preds3[-1] - M_3$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds3)-1))) / (max(M_3$TM_ATX_all_ug_orgmat_g - min(M_3$TM_ATX_all_ug_orgmat_g)))
nrmse4 <- (sqrt(sum((preds4[-1] - M_4$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds4)-1))) / (max(M_4$TM_ATX_all_ug_orgmat_g - min(M_4$TM_ATX_all_ug_orgmat_g)))
nrmse5 <- (sqrt(sum((preds5[-1] - SH_1s$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds5)-1))) / (max(SH_1s$TM_ATX_all_ug_orgmat_g - min(SH_1s$TM_ATX_all_ug_orgmat_g)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.356
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.238
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.580

## micro anatoxin with metabolism
model6_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         atx = pred_M_1s$TM_ATX_all_ug_orgmat_g, GPP = pred_M_1s$mean_GPP_fourdaysprior)
model6_M_1S <- stan(file = "cover_model6.stan", data = model6_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model6_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$microcoleus,
                        atx = pred_M_2$TM_ATX_all_ug_orgmat_g, GPP = pred_M_2$mean_GPP_fourdaysprior)
model6_M_2 <- stan(file = "cover_model6.stan", data = model6_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model6_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$microcoleus,
                        atx = pred_M_3$TM_ATX_all_ug_orgmat_g, GPP = pred_M_3$mean_GPP_fourdaysprior)
model6_M_3 <- stan(file = "cover_model6.stan", data = model6_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model6_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$microcoleus,
                        atx = pred_M_4$TM_ATX_all_ug_orgmat_g, GPP = pred_M_4$mean_GPP_fourdaysprior)
model6_M_4 <- stan(file = "cover_model6.stan", data = model6_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model6_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$microcoleus,
                          atx = pred_SH_1s$TM_ATX_all_ug_orgmat_g, GPP = pred_SH_1s$mean_GPP_fourdaysprior)
model6_SH_1S <- stan(file = "cover_model6.stan", data = model6_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model5_M_1S, c("b0", "b1", "b2", "b3", "sigma"))
params2 <- rstan::extract(model5_M_2, c("b0", "b1", "b2", "b3", "sigma"))
params3 <- rstan::extract(model5_M_3, c("b0", "b1", "b2", "b3", "sigma"))
params4 <- rstan::extract(model5_M_4, c("b0", "b1", "b2", "b3", "sigma"))
params5 <- rstan::extract(model5_SH_1S, c("b0", "b1", "b2", "b3", "sigma"))

preds1 <- PredOut_model6(params1, M_1s$microcoleus, M_1s$TM_ATX_all_ug_orgmat_g, M_1s$mean_GPP_fourdaysprior)
preds2 <- PredOut_model6(params2, M_2$microcoleus, M_2$TM_ATX_all_ug_orgmat_g, M_2$mean_GPP_fourdaysprior)
preds3 <- PredOut_model6(params3, M_3$microcoleus, M_3$TM_ATX_all_ug_orgmat_g, M_3$mean_GPP_fourdaysprior)
preds4 <- PredOut_model6(params4, M_4$microcoleus, M_4$TM_ATX_all_ug_orgmat_g, M_4$mean_GPP_fourdaysprior)
preds5 <- PredOut_model6(params5, SH_1s$microcoleus, SH_1s$TM_ATX_all_ug_orgmat_g, SH_1s$mean_GPP_fourdaysprior)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1[-1] - M_1s$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds1)-1))) / (max(M_1s$TM_ATX_all_ug_orgmat_g - min(M_1s$TM_ATX_all_ug_orgmat_g)))
nrmse2 <- (sqrt(sum((preds2[-1] - M_2$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds2)-1))) / (max(M_2$TM_ATX_all_ug_orgmat_g - min(M_2$TM_ATX_all_ug_orgmat_g)))
nrmse3 <- (sqrt(sum((preds3[-1] - M_3$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds3)-1))) / (max(M_3$TM_ATX_all_ug_orgmat_g - min(M_3$TM_ATX_all_ug_orgmat_g)))
nrmse4 <- (sqrt(sum((preds4[-1] - M_4$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds4)-1))) / (max(M_4$TM_ATX_all_ug_orgmat_g - min(M_4$TM_ATX_all_ug_orgmat_g)))
nrmse5 <- (sqrt(sum((preds5[-1] - SH_1s$TM_ATX_all_ug_orgmat_g[-1])^2)/(length(preds5)-1))) / (max(SH_1s$TM_ATX_all_ug_orgmat_g - min(SH_1s$TM_ATX_all_ug_orgmat_g)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.463
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.274
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 1.07

## ana anatoxin with percent cover
model4_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$anabaena_cylindrospermum,
                         atx = pred_M_1s$TAC_ATX_all_ug_orgmat_g)
model4_M_1S <- stan(file = "cover_model4.stan", data = model4_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model4_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$anabaena_cylindrospermum,
                        atx = pred_M_2$TAC_ATX_all_ug_orgmat_g)
model4_M_2 <- stan(file = "cover_model4.stan", data = model4_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model4_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$anabaena_cylindrospermum,
                        atx = pred_M_3$TAC_ATX_all_ug_orgmat_g)
model4_M_3 <- stan(file = "cover_model4.stan", data = model4_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model4_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$anabaena_cylindrospermum,
                        atx = pred_M_4$TAC_ATX_all_ug_orgmat_g)
model4_M_4 <- stan(file = "cover_model4.stan", data = model4_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model4_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$anabaena_cylindrospermum,
                          atx = pred_SH_1s$TAC_ATX_all_ug_orgmat_g)
model4_SH_1S <- stan(file = "cover_model4.stan", data = model4_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

params1 <- rstan::extract(model4_M_1S, c("b0", "b1", "b2", "sigma"))
params2 <- rstan::extract(model4_M_2, c("b0", "b1", "b2", "sigma"))
params3 <- rstan::extract(model4_M_3, c("b0", "b1", "b2", "sigma"))
params4 <- rstan::extract(model4_M_4, c("b0", "b1", "b2", "sigma"))
params5 <- rstan::extract(model4_SH_1S, c("b0", "b1", "b2", "sigma"))

preds1 <- PredOut_model4(params1, M_1s$anabaena_cylindrospermum, M_1s$TAC_ATX_all_ug_orgmat_g)
preds2 <- PredOut_model4(params2, M_2$anabaena_cylindrospermum, M_2$TAC_ATX_all_ug_orgmat_g)
preds3 <- PredOut_model4(params3, M_3$anabaena_cylindrospermum, M_3$TAC_ATX_all_ug_orgmat_g)
preds4 <- PredOut_model4(params4, M_4$anabaena_cylindrospermum, M_4$TAC_ATX_all_ug_orgmat_g)
preds5 <- PredOut_model4(params5, SH_1s$anabaena_cylindrospermum, SH_1s$TAC_ATX_all_ug_orgmat_g)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1[-1] - M_1s$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds1)-1))) / (max(M_1s$TAC_ATX_all_ug_orgmat_g - min(M_1s$TAC_ATX_all_ug_orgmat_g)))
nrmse2 <- (sqrt(sum((preds2[-1] - M_2$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds2)-1))) / (max(M_2$TAC_ATX_all_ug_orgmat_g - min(M_2$TAC_ATX_all_ug_orgmat_g)))
nrmse3 <- (sqrt(sum((preds3[-1] - M_3$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds3)-1))) / (max(M_3$TAC_ATX_all_ug_orgmat_g - min(M_3$TAC_ATX_all_ug_orgmat_g)))
nrmse4 <- (sqrt(sum((preds4[-1] - M_4$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds4)-1))) / (max(M_4$TAC_ATX_all_ug_orgmat_g - min(M_4$TAC_ATX_all_ug_orgmat_g)))
nrmse5 <- (sqrt(sum((preds5[-1] - SH_1s$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds5)-1))) / (max(SH_1s$TAC_ATX_all_ug_orgmat_g - min(SH_1s$TAC_ATX_all_ug_orgmat_g)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.239
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.209
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.284

## ana anatoxin with water quality
model5_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$anabaena_cylindrospermum,
                         atx = pred_M_1s$TAC_ATX_all_ug_orgmat_g, ophos = pred_M_1s$oPhos_ug_P_L,
                         amm = pred_M_1s$ammonium_mg_N_L, nitrate = pred_M_1s$nitrate_mg_N_L)
model5_M_1S <- stan(file = "cover_model5.stan", data = model5_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model5_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$anabaena_cylindrospermum,
                        atx = pred_M_2$TAC_ATX_all_ug_orgmat_g, ophos = pred_M_2$oPhos_ug_P_L,
                        amm = pred_M_2$ammonium_mg_N_L, nitrate = pred_M_2$nitrate_mg_N_L)
model5_M_2 <- stan(file = "cover_model5.stan", data = model5_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model5_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$anabaena_cylindrospermum,
                        atx = pred_M_3$TAC_ATX_all_ug_orgmat_g, ophos = pred_M_3$oPhos_ug_P_L,
                        amm = pred_M_3$ammonium_mg_N_L, nitrate = pred_M_3$nitrate_mg_N_L)
model5_M_3 <- stan(file = "cover_model5.stan", data = model5_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model5_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$anabaena_cylindrospermum,
                        atx = pred_M_4$TAC_ATX_all_ug_orgmat_g, ophos = pred_M_4$oPhos_ug_P_L,
                        amm = pred_M_4$ammonium_mg_N_L, nitrate = pred_M_4$nitrate_mg_N_L)
model5_M_4 <- stan(file = "cover_model5.stan", data = model5_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model5_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$anabaena_cylindrospermum,
                          atx = pred_SH_1s$TAC_ATX_all_ug_orgmat_g, ophos = pred_SH_1s$oPhos_ug_P_L,
                          amm = pred_SH_1s$ammonium_mg_N_L, nitrate = pred_SH_1s$nitrate_mg_N_L)
model5_SH_1S <- stan(file = "cover_model5.stan", data = model5_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model5_M_1S, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params2 <- rstan::extract(model5_M_2, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params3 <- rstan::extract(model5_M_3, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params4 <- rstan::extract(model5_M_4, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))
params5 <- rstan::extract(model5_SH_1S, c("b0", "b1", "b2", "b3", "b4", "b5", "sigma"))

preds1 <- PredOut_model5(params1, M_1s$anabaena_cylindrospermum, M_1s$TAC_ATX_all_ug_orgmat_g, M_1s$oPhos_ug_P_L,
                         M_1s$ammonium_mg_N_L, M_1s$nitrate_mg_N_L)
preds2 <- PredOut_model5(params2, M_2$anabaena_cylindrospermum, M_2$TAC_ATX_all_ug_orgmat_g, M_2$oPhos_ug_P_L,
                         M_2$ammonium_mg_N_L, M_2$nitrate_mg_N_L)
preds3 <- PredOut_model5(params3, M_3$anabaena_cylindrospermum, M_3$TAC_ATX_all_ug_orgmat_g, M_3$oPhos_ug_P_L,
                         M_3$ammonium_mg_N_L, M_3$nitrate_mg_N_L)
preds4 <- PredOut_model5(params4, M_4$anabaena_cylindrospermum, M_4$TAC_ATX_all_ug_orgmat_g, M_4$oPhos_ug_P_L,
                         M_4$ammonium_mg_N_L, M_4$nitrate_mg_N_L)
preds5 <- PredOut_model5(params5, SH_1s$anabaena_cylindrospermum, SH_1s$TAC_ATX_all_ug_orgmat_g, SH_1s$oPhos_ug_P_L,
                         SH_1s$ammonium_mg_N_L, SH_1s$nitrate_mg_N_L)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1[-1] - M_1s$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds1)-1))) / (max(M_1s$TAC_ATX_all_ug_orgmat_g - min(M_1s$TAC_ATX_all_ug_orgmat_g)))
nrmse2 <- (sqrt(sum((preds2[-1] - M_2$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds2)-1))) / (max(M_2$TAC_ATX_all_ug_orgmat_g - min(M_2$TAC_ATX_all_ug_orgmat_g)))
nrmse3 <- (sqrt(sum((preds3[-1] - M_3$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds3)-1))) / (max(M_3$TAC_ATX_all_ug_orgmat_g - min(M_3$TAC_ATX_all_ug_orgmat_g)))
nrmse4 <- (sqrt(sum((preds4[-1] - M_4$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds4)-1))) / (max(M_4$TAC_ATX_all_ug_orgmat_g - min(M_4$TAC_ATX_all_ug_orgmat_g)))
nrmse5 <- (sqrt(sum((preds5[-1] - SH_1s$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds5)-1))) / (max(SH_1s$TAC_ATX_all_ug_orgmat_g - min(SH_1s$TAC_ATX_all_ug_orgmat_g)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.240
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.209
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.282

## ana anatoxin with metabolism
model6_M_1S_data <- list(N_days = nrow(pred_M_1s), cover = pred_M_1s$anabaena_cylindrospermum,
                         atx = pred_M_1s$TAC_ATX_all_ug_orgmat_g, GPP = pred_M_1s$mean_GPP_fourdaysprior)
model6_M_1S <- stan(file = "cover_model6.stan", data = model6_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)
model6_M_2_data <- list(N_days = nrow(pred_M_2), cover = pred_M_2$anabaena_cylindrospermum,
                        atx = pred_M_2$TAC_ATX_all_ug_orgmat_g, GPP = pred_M_2$mean_GPP_fourdaysprior)
model6_M_2 <- stan(file = "cover_model6.stan", data = model6_M_2_data,
                   chains = 3, iter = 2000, warmup = 1000)
model6_M_3_data <- list(N_days = nrow(pred_M_3), cover = pred_M_3$anabaena_cylindrospermum,
                        atx = pred_M_3$TAC_ATX_all_ug_orgmat_g, GPP = pred_M_3$mean_GPP_fourdaysprior)
model6_M_3 <- stan(file = "cover_model6.stan", data = model6_M_3_data,
                   chains = 3, iter = 2000, warmup = 1000)
model6_M_4_data <- list(N_days = nrow(pred_M_4), cover = pred_M_4$anabaena_cylindrospermum,
                        atx = pred_M_4$TAC_ATX_all_ug_orgmat_g, GPP = pred_M_4$mean_GPP_fourdaysprior)
model6_M_4 <- stan(file = "cover_model6.stan", data = model6_M_4_data,
                   chains = 3, iter = 2000, warmup = 1000)
model6_SH_1S_data <- list(N_days = nrow(pred_SH_1s), cover = pred_SH_1s$anabaena_cylindrospermum,
                          atx = pred_SH_1s$TAC_ATX_all_ug_orgmat_g, GPP = pred_SH_1s$mean_GPP_fourdaysprior)
model6_SH_1S <- stan(file = "cover_model6.stan", data = model6_SH_1S_data,
                     chains = 3, iter = 2000, warmup = 1000)

# need to get predictions
params1 <- rstan::extract(model6_M_1S, c("b0", "b1", "b2", "b3", "sigma"))
params2 <- rstan::extract(model6_M_2, c("b0", "b1", "b2", "b3", "sigma"))
params3 <- rstan::extract(model6_M_3, c("b0", "b1", "b2", "b3", "sigma"))
params4 <- rstan::extract(model6_M_4, c("b0", "b1", "b2", "b3", "sigma"))
params5 <- rstan::extract(model6_SH_1S, c("b0", "b1", "b2", "b3", "sigma"))

preds1 <- PredOut_model6(params1, M_1s$anabaena_cylindrospermum, M_1s$TAC_ATX_all_ug_orgmat_g, M_1s$mean_GPP_fourdaysprior)
preds2 <- PredOut_model6(params2, M_2$anabaena_cylindrospermum, M_2$TAC_ATX_all_ug_orgmat_g, M_2$mean_GPP_fourdaysprior)
preds3 <- PredOut_model6(params3, M_3$anabaena_cylindrospermum, M_3$TAC_ATX_all_ug_orgmat_g, M_3$mean_GPP_fourdaysprior)
preds4 <- PredOut_model6(params4, M_4$anabaena_cylindrospermum, M_4$TAC_ATX_all_ug_orgmat_g, M_4$mean_GPP_fourdaysprior)
preds5 <- PredOut_model6(params5, SH_1s$anabaena_cylindrospermum, SH_1s$TAC_ATX_all_ug_orgmat_g, SH_1s$mean_GPP_fourdaysprior)

# average RMSE and get range (write it in code comment)
nrmse1 <- (sqrt(sum((preds1[-1] - M_1s$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds1)-1))) / (max(M_1s$TAC_ATX_all_ug_orgmat_g - min(M_1s$TAC_ATX_all_ug_orgmat_g)))
nrmse2 <- (sqrt(sum((preds2[-1] - M_2$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds2)-1))) / (max(M_2$TAC_ATX_all_ug_orgmat_g - min(M_2$TAC_ATX_all_ug_orgmat_g)))
nrmse3 <- (sqrt(sum((preds3[-1] - M_3$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds3)-1))) / (max(M_3$TAC_ATX_all_ug_orgmat_g - min(M_3$TAC_ATX_all_ug_orgmat_g)))
nrmse4 <- (sqrt(sum((preds4[-1] - M_4$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds4)-1))) / (max(M_4$TAC_ATX_all_ug_orgmat_g - min(M_4$TAC_ATX_all_ug_orgmat_g)))
nrmse5 <- (sqrt(sum((preds5[-1] - SH_1s$TAC_ATX_all_ug_orgmat_g[-1])^2)/(length(preds5)-1))) / (max(SH_1s$TAC_ATX_all_ug_orgmat_g - min(SH_1s$TAC_ATX_all_ug_orgmat_g)))
mean(c(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5)) # 0.296
min(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.209
max(nrmse1, nrmse2, nrmse3, nrmse4, nrmse5) # 0.559

