#### Getting r-hats for all models to one csv
### Jordan Zabrecky
## last edited: 10.15.2025

# This code pulls r-hats from all models and saves them into a single csv file
# to easily make a supplemental figure

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "plyr"), 
       require, character.only = T)

#### (2) Going through folders and saving rhats to a larger csv ####

# create empty dataframe to hold all rhats from csvs
rhats_all <- data.frame(parameters = NA,
                        `SFE.M.1S` = NA,
                        `SFE.M.2` = NA,
                        `SFE.M.3` = NA,
                        `SFE.M.4` = NA,
                        `SFE.SH.1S` = NA,
                        model = NA,
                        predicting = NA)

# list of folder and models to iterate through0
folders <- c("M_cover_models", "AC_cover_models", "M_atx_models", "AC_atx_models")

# iterate through for loop of all folders (i; what we are predicting)
for(i in 1:length(folders)) {
  new <- ldply(list.files(path = paste("./data/predictive_models/", folders[i],"/model_attributes/", sep = ""), 
                          pattern = "rhats"), function(filename) {
                            # open csv and add model information
                            rhats <- read.csv(paste("./data/predictive_models/", folders[i], "/model_attributes/",
                                                    filename, sep = ""))
                            modelname <- sub(paste0("_rhats.csv", ".*"), "", filename)
                            rhats$model <- modelname
                            rhats$predicting <- sub("_models", "", folders[i])
                            
                            # change parameter names based on model name
                            # assign parameter names based on model name
                            if(modelname == "physical") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              TRUE ~ X))
                            } else if(modelname == "physical_w_cover") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              X == "b[3]" ~ "Cover",
                                                              TRUE ~ X))
                            } else if(modelname == "chemical") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "DIN",
                                                              X == "b[2]" ~ "OPhos",
                                                              X == "b[3]" ~ "Conductivity",
                                                              TRUE ~ X))
                            } else if(modelname == "chemical_w_cover") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "DIN",
                                                              X == "b[2]" ~ "OPhos",
                                                              X == "b[3]" ~ "Conductivity",
                                                              X == "b[4]" ~ "Cover",
                                                              TRUE ~ X))
                            } else if(modelname == "biological") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "GPP",
                                                              TRUE ~ X))
                            } else if(modelname == "biological_w_cover") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "GPP",
                                                              X == "b[2]" ~ "Cover",
                                                              TRUE ~ X))
                            } else if(modelname == "physicochemical") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              X == "b[3]" ~  "DIN",
                                                              X == "b[4]" ~ "OPhos",
                                                              X == "b[5]" ~ "Conductivity",
                                                              TRUE ~ X))
                            } else if(modelname == "physicochemical_w_cover") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              X == "b[3]" ~  "DIN",
                                                              X == "b[4]" ~ "OPhos",
                                                              X == "b[5]" ~ "Conductivity",
                                                              X == "b[6]" ~ "Cover",
                                                              TRUE ~ X))
                            } else if(modelname == "ecohydrological") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              X == "b[3]" ~ "GPP",
                                                              TRUE ~ X))
                            } else if (modelname == "ecohydrological_w_cover") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              X == "b[3]" ~ "GPP",
                                                              X == "b[4]" ~ "Cover",
                                                              TRUE ~ X))
                            } else if (modelname == "biochemical") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "DIN",
                                                              X == "b[2]" ~ "OPhos",
                                                              X == "b[3]" ~ "Conductivity",
                                                              X == "b[4]" ~ "GPP",
                                                              TRUE ~ X))
                            } else if (modelname == "biochemical_w_cover") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "DIN",
                                                              X == "b[2]" ~ "OPhos",
                                                              X == "b[3]" ~ "Conductivity",
                                                              X == "b[4]" ~ "GPP",
                                                              X == "b[5]" ~ "Cover",
                                                              TRUE ~ X))
                            } else if(modelname == "all") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              X == "b[3]" ~  "DIN",
                                                              X == "b[4]" ~ "OPhos",
                                                              X == "b[5]" ~ "Conductivity",
                                                              X == "b[6]" ~ "GPP",
                                                              TRUE ~ X))
                            } else if(modelname == "all_w_cover") {
                              rhats <- rhats %>% 
                                mutate(parameters = case_when(X == "b[1]" ~ "Temperature",
                                                              X == "b[2]" ~ "Discharge",
                                                              X == "b[3]" ~  "DIN",
                                                              X == "b[4]" ~ "OPhos",
                                                              X == "b[5]" ~ "Conductivity",
                                                              X == "b[6]" ~ "GPP",
                                                              X == "b[7]" ~ "Cover",
                                                              TRUE ~ X))
                            }
                            
                            # remove and relocate a column to match rhats
                            rhats <- rhats %>% 
                              select(!X) %>% 
                              relocate(parameters, .before = "SFE.M.1S")
                            
                            # add to rhat dataframe
                            return(rhats)
                 })
  
  # add to dataframe of all rhats
  rhats_all <- rbind(rhats_all, new)
}

#### (3) Final processing & save ####

# remove empty first row
rhats_all <- rhats_all[-1,]

# save
write.csv(rhats_all, "./data/predictive_models/rhats_allmodels.csv", row.names = FALSE)
