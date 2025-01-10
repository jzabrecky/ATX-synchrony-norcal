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

## (1) Microcoleus occurence

setwd("./code/misc_code/EFI_prelim_STAN")

# look at joanna's notes

# just with prior occupancy
model1_M_1S_data <- list(N = nrow(pred_M_1s), cover = pred_M_1s$microcoleus,
                         prior_cover = pred_M_1s$prior_cover)
model1_M_1S <- stan(file = "cover_model1.stan", data = model1_M_1S_data,
                    chains = 3, iter = 2000, warmup = 1000)

# rows of data
N <- nrow(training)

model1_data <- list(N = N, cover = pred_M_1s$microcoleus, covar1 = pred_M_1s$prior_microcoleus)

model1 <- stan(file = "20230331_ForecastingChallenge2.stan", data = model1_data, 
               chains = 3, iter = 2000, warmup = 1000)

launch_shinystan(model1)

params1 <- rstan::extract(model1, c("b0", "b1", "b2", "b3", "sigma"))
par(mfrow=c(2,3))
hist(params1$b0, ylim = c(0,800))
hist(params1$b1, ylim = c(0,800))
hist(params1$b2, ylim = c(0,800))
hist(params1$b3, ylim = c(0,800))
hist(params1$sigma, ylim = c(0,800))

# including water quality parameters

# predicting both occurance (as cover for now) and atx separately