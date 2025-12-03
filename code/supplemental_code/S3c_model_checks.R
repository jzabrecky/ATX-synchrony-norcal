#### Ensuring model convergence
### Jordan Zabrecky
## last edited: 09.23.2025

# This code pulls the number of divergent transition and r-hats for all parameters
# in all models to ensure convergence. There is also a section at the end to load in
# a model RDS object and troubleshoot using shinystan

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders", "plyr"), 
       require, character.only = T)

#### (2) Open model check convergence for models ####

# create lists for folders we want to iterate through and different model names
folders <- c("M_cover_models", "AC_cover_models", "M_atx_models", "AC_atx_models")
model_names <- c("physical", "chemical", "biological", "ecohydrological", "all")

# empty data frame for convergence
convergence_summary <- data.frame(predicting = NA,
                                 model_name = NA,
                                 site_reach = NA,
                                 divergent_transitions = NA,
                                 num_divergent = NA)

# iterate through for loop of all folders (i; what we are predicting) and all model names (j; different covariates)
for(i in 1:length(folders)) {
  for(j in 1:length(model_names)) {
    new <- ldply(list.files(path = paste("./data/predictive_models/", folders[i],"/", sep = ""), pattern = model_names[j]),
         function(filename) {
           # load model
           model <- readRDS(paste("./data/predictive_models/", folders[i], "/", filename, sep = ""))
           
           # add information into a matching dataframe
           assessment <- data.frame(predicting = sub(paste0("_models", ".*"), "", folders[i]),
                                   model_name = sub(paste0("_SFE", ".*"), "", filename),
                                   site_reach = sub(paste0(".*", "_"), "", filename),
                                   divergent_transitions = eval(rstan::get_num_divergent(model) > 0),
                                   num_divergent = rstan::get_num_divergent(model))
           return(assessment)
         })
    
    # bind new (what we just read in) to full dataframe
    convergence_summary <- rbind(convergence_summary, new)
  }
}
# going to have to also manually check console output while running models 
# because cannot seem to easily get warning to save

#### (3) Open r-hat summaries and check to see if any are above 1.05 ####

# create empty dataframe to store information
rhat_summary <- data.frame(predicting = NA,
                           model_name = NA,
                           site_reach = NA,
                           rhat_above_1.05 = NA)

# iterate through for loop of all folders (i; what we are predicting) and model names will come up with r-hat pattern
for(i in 1:length(folders)) {
    new <- ldply(list.files(path = paste("./data/predictive_models/", folders[i],"/model_attributes", sep = ""), 
                            pattern = "rhats"),
                 function(filename) {
                   rhats <- read.csv(paste("./data/predictive_models/", folders[i], 
                                           "/model_attributes/", filename, sep = ""))
                   assessment <- data.frame(predicting = rep(sub(paste0("_models", ".*"), "", folders[i]), 5),
                                            model_name = rep(sub(paste0("_rhats", ".*"), "", filename), 5),
                                            site_reach = colnames(rhats)[-1],
                                            rhat_above_1.05 = rep(NA, 5))
                   
                   # iterate through each reach and check if there are any parameter r-hats > 1.05
                   for(k in 2:ncol(rhats)){
                     assessment[k-1,"rhat_above_1.05"] <- any(rhats[,k] > 1.05)
                   }
                   
                  return(assessment)
                 })
    
    # bind new (what we just read in) to full dataframe
    rhat_summary <- rbind(rhat_summary, new)
}

#### (4) Joining two assessments & saving ####

# need to change name of site_reach in rhats
rhat_summary <- rhat_summary %>% 
  mutate(site_reach = case_when(site_reach == "SFE.M.1S" ~ "SFE-M-1S",
                                site_reach == "SFE.M.2" ~ "SFE-M-2",
                                site_reach == "SFE.M.3" ~ "SFE-M-3",
                                site_reach == "SFE.M.4" ~ "SFE-M-4",
                                site_reach == "SFE.SH.1S" ~ "SFE-SH-1S"))

# join two dataframes together
final <- left_join(rhat_summary, convergence_summary, by = c("predicting", "site_reach", "model_name"))

# save as csv
write.csv(final[-1,], "./data/predictive_models/convergence_summary.csv", row.names = FALSE)

#### (5) Model trouble shooting ####

# load a model with issues
#model <- readRDS("./data/predictive_models/AC_cover_models/physicochemical_SFE-M-1S")

# launch shinystan
#shinystan::launch_shinystan(model)
