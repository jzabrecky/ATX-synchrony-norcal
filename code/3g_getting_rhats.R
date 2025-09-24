#### Getting r-hats for all models to one csv
### Jordan Zabrecky
## last edited: 09.24.2025

## This code pulls r-hats from all models and saves them into a single csv file
## to easily make a supplemental figure

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "plyr"), 
       require, character.only = T)

#### (2) Going through folders and saving rhats to a larger csv ####

# create empty dataframe to hold all rhats from csvs
rhats_all <- data.frame(X = NA,
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
                            # open csv and add information
                            rhats <- read.csv(paste("./data/predictive_models/", folders[i], "/model_attributes/",
                                                    filename, sep = ""))
                            rhats$model <- sub(paste0("_rhats.csv", ".*"), "", filename)
                            rhats$predicting <- sub("_models", "", folders[i])
                            
                            # add to rhat dataframe
                            return(rhats)
                 })
  rhats_all <- rbind(rhats_all, new)
}

#### (3) Final processing & save ####

# change column names
rhats_all <- rhats_all %>% 
  dplyr::rename(parameter = X)

# remove empty first row
rhats_all <- rhats_all[-1,]

# save
write.csv(rhats_all, "./data/predictive_models/rhats_allmodels.csv", row.names = FALSE)
