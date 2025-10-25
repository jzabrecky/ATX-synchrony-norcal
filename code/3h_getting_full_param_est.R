#### Extracting mean and 95% CI for parameter estimates from predictive models
### Jordan Zabrecky
## last edited: 10.25.2025

## This code goes back to open previously ran models and saves their parameter estimates
## (mean and 95% credible interval) into one csv for each prediction type for
## each RDS object and also grouping multiple parameter estimates for one model
## across the five RDS objects (e.g., all RDS objects for "physical" models predicting M cover)

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "plyr"), 
       require, character.only = T)

#### (2) Open model and save parameter estimates for cover models ####

# create lists for folders we want to iterate through and different model names
folders <- c("M_cover_models", "AC_cover_models", "M_atx_models", "AC_atx_models")

# empty dataframe to add parameter estimates to (one that separates out site-reach & one that does not)
param_list <- data.frame(parameters = NA,
                         model = NA,
                         predicting = NA,
                         mean = NA,
                         ci_lower = NA,
                         ci_upper = NA)

param_list_site_reach <- data.frame(parameters = NA,
                                    site_reach = NA,
                                    model = NA,
                                    predicting = NA,
                                    mean = NA,
                                    ci_lower = NA,
                                    ci_upper = NA)

# iterate through for loop of all folders (i; what we are predicting) and all model names (j; different covariates)
for(i in 1:length(folders)) {
  # list unique model names (by listing all files in folder & removing site_reach tag minus S)
  # will still have a duplicate of biochemical & physicochemical from the chemical model pattern
  # but as it gets grouped by model name to summarize the matrix, the results are unaffected
  model_names <- c(unique(str_extract(list.files(path = paste("./data/predictive_models/", folders[i], "/", sep = "")),
                               ".+?(?=FE)")) %>% na.omit())
  for(j in 1:(length(model_names))) {
    full_matrix <- ldply(list.files(path = paste("./data/predictive_models/", folders[i],"/", sep = ""), 
                                     pattern = model_names[j]), function(filename) {
                                       # read in model
                                       model <- readRDS(paste("./data/predictive_models/", folders[i], "/", filename, sep = ""))
                                       # add in site_reach information
                                       sitereach <- sub(paste0(".*", "_"), "", filename)
                                       # get model name information
                                       modelname <- sub(paste0("_SFE", ".*"), "", filename)
                                       # extract parameters
                                       params <- as.data.frame((rstan::extract(model, c("b")))$b) 
                                       # pivot longer
                                       params <- params %>% 
                                        pivot_longer(cols = c(1:ncol(params)), names_to = "beta", values_to = "value")
                                      
                                       # assign parameter names based on model name
                                       if(modelname == "physical") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge"))
                                       } else if(modelname == "physical_w_cover") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge",
                                                                         beta == "V3" ~ "Cover"))
                                       } else if(modelname == "chemical") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "DIN",
                                                                         beta == "V2" ~ "OPhos",
                                                                         beta == "V3" ~ "Conductivity"))
                                       } else if(modelname == "chemical_w_cover") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "DIN",
                                                                         beta == "V2" ~ "OPhos",
                                                                         beta == "V3" ~ "Conductivity",
                                                                         beta == "V4" ~ "Cover"))
                                       } else if(modelname == "biological") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "GPP"))
                                       } else if(modelname == "biological_w_cover") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "GPP",
                                                                         beta == "V2" ~ "Cover"))
                                       } else if(modelname == "physicochemical") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge",
                                                                         beta == "V3" ~  "DIN",
                                                                         beta == "V4" ~ "OPhos",
                                                                         beta == "V5" ~ "Conductivity"))
                                       } else if(modelname == "physicochemical_w_cover") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge",
                                                                         beta == "V3" ~  "DIN",
                                                                         beta == "V4" ~ "OPhos",
                                                                         beta == "V5" ~ "Conductivity",
                                                                         beta == "V6" ~ "Cover"))
                                       } else if(modelname == "ecohydrological") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge",
                                                                         beta == "V3" ~ "GPP"))
                                       } else if (modelname == "ecohydrological_w_cover") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge",
                                                                         beta == "V3" ~ "GPP",
                                                                         beta == "V4" ~ "Cover"))
                                       } else if (modelname == "biochemical") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "DIN",
                                                                         beta == "V2" ~ "OPhos",
                                                                         beta == "V3" ~ "Conductivity",
                                                                         beta == "V4" ~ "GPP"))
                                       } else if (modelname == "biochemical_w_cover") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "DIN",
                                                                         beta == "V2" ~ "OPhos",
                                                                         beta == "V3" ~ "Conductivity",
                                                                         beta == "V4" ~ "GPP",
                                                                         beta == "V5" ~ "Cover"))
                                       } else if(modelname == "all") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge",
                                                                         beta == "V3" ~  "DIN",
                                                                         beta == "V4" ~ "OPhos",
                                                                         beta == "V5" ~ "Conductivity",
                                                                         beta == "V6" ~ "GPP"))
                                       } else if(modelname == "all_w_cover") {
                                         params <- params %>% 
                                           mutate(parameters = case_when(beta == "V1" ~ "Temperature",
                                                                         beta == "V2" ~ "Discharge",
                                                                         beta == "V3" ~  "DIN",
                                                                         beta == "V4" ~ "OPhos",
                                                                         beta == "V5" ~ "Conductivity",
                                                                         beta == "V6" ~ "GPP",
                                                                         beta == "V7" ~ "Cover"))
                                       }
                                       
                                       # add in reach the model is predicting and model name
                                       params$site_reach <- sitereach
                                       params$model <- modelname
                                       params$predicting <- sub(paste0("_models", ".*"), "", folders[i])
                                       
                                       return(params)
                                       })
    
    # summarize on a model basis
    by_model <- full_matrix %>% 
      dplyr::group_by(parameters, model, predicting) %>% 
      dplyr::summarize(mean = mean(value),
                       ci_lower = quantile(value, 0.025),
                       ci_upper = quantile(value, 0.975))
    
    # add to list
    param_list <- rbind(param_list, by_model)
    
    # summarize on a site_reach basis
    by_site_reach <- full_matrix %>% 
      dplyr::group_by(parameters, site_reach, model, predicting) %>% 
      dplyr::summarize(mean = mean(value),
                       ci_lower = quantile(value, 0.025),
                       ci_upper = quantile(value, 0.975))
    
    # add to list
    param_list_site_reach <- rbind(param_list_site_reach, by_site_reach)
  }
}
  
#### (3) Saving  ####

# remove first row of dataframe (NAs)
param_list <- param_list[-1,]
param_list_site_reach <- param_list_site_reach[-1,]

# remove duplicate of biochemical and physicochemical (gets added twice because of chemical!)
param_list <- unique(param_list)
param_list_site_reach <- unique(param_list_site_reach)

# save
write.csv(param_list, "./data/predictive_models/parameter_est_allmodels.csv",
          row.names = FALSE)
write.csv(param_list_site_reach, "./data/predictive_models/parameter_est_allmodels_by_site_reach.csv",
          row.names = FALSE)

