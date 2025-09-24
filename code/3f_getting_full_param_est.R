#### Extracting mean and 95% CI for parameter estimates from predictive models
### Jordan Zabrecky
## last edited: 09.25.2025

## This code goes back to open previously ran models and saves their parameter estimates
## (mean and 95% credible interval) into one csv for each prediction type

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "plyr"), 
       require, character.only = T)

#### (2) Open model and save parameter estimates for cover models ####

# create lists for folders we want to iterate through and different model names
folders <- c("M_cover_models", "AC_cover_models", "M_atx_models", "AC_atx_models")
model_names <- c("null", "physical", "chemical", "biological", "ecohydrological", "all")
# missing some, but note that "chemical" will catch three models (chemical, biochemical, physicochemical!)
# and the original phrase will catch all of the _w_cover models!

# empty dataframe to add parameter estimates to
param_list <- data.frame(beta = NA,
                         mean = NA,
                         ci_lower = NA,
                         ci_upper = NA,
                         parameters = NA,
                         site_reach = NA,
                         model = NA,
                         predicting = NA)

# iterate through for loop of all folders (i; what we are predicting) and all model names (j; different covariates)
for(i in 1:length(folders)) {
  for(j in 1:length(model_names)) {
    new <- ldply(list.files(path = paste("./data/predictive_models/", folders[i],"/", sep = ""), pattern = model_names[j]),
          function(filename) {
            model <- readRDS(paste("./data/predictive_models/", folders[i], "/", filename, sep = ""))
            modelname <- sub(paste0("_SFE", ".*"), "", filename)
            sitereach <- sub(paste0(".*", "_"), "", filename)
            params <- as.data.frame(summary(model)$summary[,c("mean", "2.5%", "97.5%")])
            params <- tibble::rownames_to_column(params, "beta") %>% 
              filter(!beta %in% c("b0", "sigma", "lp__"))
            
            # assign parameter names based on model name
            if(modelname == "physical") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge"))
            } else if(modelname == "physical_w_cover") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge",
                                              beta == "b[3]" ~ "Cover"))
            } else if(modelname == "chemical") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "DIN",
                                              beta == "b[2]" ~ "OPhos",
                                              beta == "b[3]" ~ "Conductivity"))
            } else if(modelname == "chemical_w_cover") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "DIN",
                                              beta == "b[2]" ~ "OPhos",
                                              beta == "b[3]" ~ "Conductivity",
                                              beta == "b[4]" ~ "Cover"))
            } else if(modelname == "biological") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "GPP"))
            } else if(modelname == "biological_w_cover") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "GPP",
                                              beta == "b[2]" ~ "Cover"))
            } else if(modelname == "physicochemical") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge",
                                              beta == "b[3]" ~  "DIN",
                                              beta == "b[4]" ~ "OPhos",
                                              beta == "b[5]" ~ "Conductivity"))
            } else if(modelname == "physicochemical_w_cover") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge",
                                              beta == "b[3]" ~  "DIN",
                                              beta == "b[4]" ~ "OPhos",
                                              beta == "b[5]" ~ "Conductivity",
                                              beta == "b[6]" ~ "Cover"))
            } else if(modelname == "ecohydrological") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge",
                                              beta == "b[3]" ~ "GPP"))
            } else if (modelname == "ecohydrological_w_cover") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge",
                                              beta == "b[3]" ~ "GPP",
                                              beta == "b[4]" ~ "Cover"))
            } else if (modelname == "biochemical") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "DIN",
                                              beta == "b[2]" ~ "OPhos",
                                              beta == "b[3]" ~ "Conductivity",
                                              beta == "b[4]" ~ "GPP"))
            } else if (modelname == "biochemical_w_cover") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "DIN",
                                              beta == "b[2]" ~ "OPhos",
                                              beta == "b[3]" ~ "Conductivity",
                                              beta == "b[4]" ~ "GPP",
                                              beta == "b[5]" ~ "Cover"))
            } else if(modelname == "all") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge",
                                              beta == "b[3]" ~  "DIN",
                                              beta == "b[4]" ~ "OPhos",
                                              beta == "b[5]" ~ "Conductivity",
                                              beta == "b[6]" ~ "GPP"))
            } else if(modelname == "all_w_cover") {
              params <- params %>% 
                mutate(parameters = case_when(beta == "b[1]" ~ "Temperature",
                                              beta == "b[2]" ~ "Discharge",
                                              beta == "b[3]" ~  "DIN",
                                              beta == "b[4]" ~ "OPhos",
                                              beta == "b[5]" ~ "Conductivity",
                                              beta == "b[6]" ~ "GPP",
                                              beta == "b[7]" ~ "Cover"))
            }
            
            # add in reach the model is predicting and model name
            params$site_reach <- sitereach
            params$model <- modelname
            params$predicting <- sub(paste0("_models", ".*"), "", folders[i])
            
            # change names of 2.5% and 97.5% end points
            params <- params %>% 
              dplyr::rename(ci_lower = `2.5%`,
                     ci_upper = `97.5%`)
            
            # return parameters dataframe
            return(params)
            
          })
          
          # bind new (what we just read in) to full dataframe
          param_list <- rbind(param_list, new)
    }
}
 
# remove first row of dataframe (NAs)
param_list <- param_list[-1,]

#### (3) Removing divergent models ####

# create a dataframe for divergent models
divergent_models <- data.frame(predicting = rep("AC_cover", 4),
                               model_name = c("all_SFE-M-1S", "biochemical_SFE-M-1S", "biological_SFE-M-1S",
                                              "ecohydrological_SFE-M-1S")) %>% 
  mutate(model = sub(paste0("_SFE", ".*"), "", model_name),
         site_reach = sub(paste0(".*", "_"), "", model_name))

# remove those diverging models from the parameter estimates
param_list_final <- param_list %>% 
  filter(!(site_reach %in% divergent_models$site_reach & predicting %in% divergent_models$predicting &
             model %in% divergent_models$model))

# save
write.csv(param_list_final, "./data/predictive_models/parameter_est_allmodels.csv",
          row.names = FALSE)
