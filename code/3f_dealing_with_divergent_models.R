#### Dealing with divergent transitions
### Jordan Zabrecky
## last edited: 10.14.2025

## Had a few models (4) with divergent transitions for models predicting 
## Anabaena/Cylindrospermum cover as identified in script 
## "S3c_model_checks.R" in the "supplemental_code" folder

#### (1) Create dataframe of these models ###

# create dataframe of these models
divergent_models <- data.frame(model_name = c("all_SFE-M-1S", "biochemical_SFE-M-1S", "biological_SFE-M-1S",
                                              "ecohydrological_SFE-M-1S")) %>% 
  mutate(model = sub(paste0("_SFE", ".*"), "", model_name),
         site_reach = sub(paste0(".*", "_"), "", model_name),
         # to match csv's when we read them in
         modified_site_reach = gsub("-", ".", site_reach))

#### (2) Deal with model files ####

# move & remove model files
for(i in 1:nrow(divergent_models)) {
  
  # copy file to new location
  file.copy(from = paste("./data/predictive_models/AC_cover_models/", divergent_models$model_name[i], sep = ""),
            to = paste("./data/predictive_models/omitted_AC_models/", divergent_models$model_name[i], sep = ""))
  
  # remove file in old location
  file.remove(paste("./data/predictive_models/AC_cover_models/", divergent_models$model_name[i], sep = ""))
}

#### (3) Deal with prediction summary ####

# save a copy of original
file.copy(from = "./data/predictive_models/predictions_AC_cover.csv",
          to = "./data/predictive_models/omitted_AC_models/predictions_AC_cover.csv")

# read in original
predictions <- read.csv("./data/predictive_models/predictions_AC_cover.csv")

# remove divergent models
final_predictions <- predictions %>% 
  filter(!(model %in% divergent_models$model & site_reach %in% divergent_models$site_reach))

# overwrite csv
write.csv(final_predictions, "./data/predictive_models/predictions_AC_cover.csv",
          row.names = FALSE)

#### (4) Remove NRMSEs ####

# move & remove NRMSE files
for(i in 1:nrow(divergent_models)) {
  
  # copy file to new location
  file.copy(from = paste("./data/predictive_models/AC_cover_models/NRMSE_vectors/", 
                         divergent_models$model_name[i], "_NRMSE.csv", sep = ""),
            to = paste("./data/predictive_models/omitted_AC_models/", 
                       divergent_models$model_name[i], "_NRMSE.csv", sep = ""))
  
  # remove file in old location
  file.remove(paste("./data/predictive_models/AC_cover_models/NRMSE_vectors/", 
                    divergent_models$model_name[i], "_NRMSE.csv", sep = ""))
  
}

#### (5) Remove Parameter Estimate Summaries ####

# move & edit parameter estimate summary files
for(i in 1:nrow(divergent_models)) {
  
  # read in csv with divergent model data
  param_est <- read.csv(paste("./data/predictive_models/AC_cover_models/model_attributes/",
                        divergent_models$model[i], "_param_est.csv", sep = ""))
  
  # save original copy in other folder
  write.csv(param_est, paste("./data/predictive_models/omitted_AC_models/",
                             divergent_models$model[i], "_param_est.csv", sep = ""),
            row.names = FALSE)
  
  # remove column associated with problematic site
  index <- which(colnames(param_est) == divergent_models$modified_site_reach[i])
  param_est[,index] <- NA
  
  # rewrite old file
  write.csv(param_est, paste("./data/predictive_models/AC_cover_models/model_attributes/",
                            divergent_models$model[i], "_param_est.csv", sep = ""),
            row.names = FALSE)
}

#### (6) Remove R-hats files ####

# move & edit r-hat summary files
for(i in 1:nrow(divergent_models)) {
  
  # read in csv with divergent model data
  rhats <- read.csv(paste("./data/predictive_models/AC_cover_models/model_attributes/",
                              divergent_models$model[i], "_rhats.csv", sep = ""))
  
  # save original copy in other folder
  write.csv(rhats, paste("./data/predictive_models/omitted_AC_models/",
                             divergent_models$model[i], "_rhats.csv", sep = ""),
            row.names = FALSE)
  
  # remove column associated with problematic site
  index <- which(colnames(rhats) == divergent_models$modified_site_reach[i])
  rhats[,index] <- NA
  
  # rewrite old file
  write.csv(rhats, paste("./data/predictive_models/AC_cover_models/model_attributes/",
                             divergent_models$model[i], "_rhats.csv", sep = ""),
            row.names = FALSE)
}
