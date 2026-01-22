#### Calculating standard deviations of predictions to determine uncertainty contributions
### Jordan Zabrecky
## last edited: 01.22.2026

# This script pulls in the prediction matrices and calculates the standard 
# deviation for each model

#### (1) Loading in libraries ####

# loading libraries
lapply(c("tidyverse","plyr"),  require, character.only = T)

# folders to iterate through
predicting <- c("M_cover", "AC_cover", "M_atx", "AC_atx")

#### (2) Calculating Standard Deviations ####

# original meaning incorporating BOTH process & parameter uncertainty
# (but not initial condition!)

# function to load prediction matrices, calculate sd & NRMSE, and add to dataframe
# uncertainty is a string (e.g. "process_error_only")
# uncertainty_path is the file path to the folder holding each model subfolder (also string)
calc_pred_sd <- function(uncertainty, uncertainty_path) {
  
  # create empty dataframe to hold one for standard deviation each submodel
  # will take the mean of all five to calculate a single one for each model
  sds_site_reach <- data.frame(uncertainty = NA,
                               predicting = NA,
                               model = NA,
                               site_reach = NA,
                               standard_dev = NA,
                               nrmse = NA)
  
  for(i in 1:length(predicting)) {
    files = list.files(path = paste(uncertainty_path, predicting[i], 
                                    "_models/pred_matrices/", sep = ""), pattern = "predsmatrix")
    
    # get model names
    modelnames = unique(unlist(lapply(str_split(files, "_"), function(x) return (x[1]))))
    
    # go through each model & load in each prediction matrix
    for(j in 1:length(modelnames)) {
      sd_model <- ldply (list.files(path = paste(uncertainty_path, predicting[i], 
                                    "_models/pred_matrices/", sep = ""), 
                       pattern = paste(modelnames[j], "_", sep = "")),
            function(filename) {
              # read in prediction matrix for submodel
              preds = read.csv(paste(uncertainty_path, predicting[i], 
                                     "_models/pred_matrices/", filename, sep = ""))
              # calculate sd for submodel (remove first column which is intial 0.05)
              sd = sd(as.matrix(preds)[,-1])
              # read in NRMSE vector
              nrmse_vector = read.csv(paste(uncertainty_path, predicting[i], 
                                            "_models/NRMSE_vectors/", 
                                            str_replace(filename, "predsmatrix", "NRMSE"), sep = ""))
              nrmse = mean(nrmse_vector$x) # gets read in as a dataframe, not a vector ('x' is only column)
              # add submodel sd to site_reach dataframe
              sd_submodel <- data.frame(uncertainty = uncertainty,
                                        predicting = predicting[i],
                                        model = modelnames[j],
                                        site_reach = str_split(filename, "_")[[1]][2],
                                        standard_dev = sd,
                                        nrmse = nrmse)
            })
      sds_site_reach <- rbind(sds_site_reach, sd_model)
      }
  }
  
  # return dataframe (minus first row of NA)
  return(sds_site_reach[-1,])
}

# run for each uncertainty type
param_and_process <- calc_pred_sd("param_and_process", "./data/predictive_models/")
process_only <- calc_pred_sd("process_only", "./data/predictive_models/process_uncertainty/")
param_process_initial <- calc_pred_sd("param_process_and_initialcon", "./data/predictive_models/initialcondition_uncertainty/")

# join all together!
std_devs_all <- rbind(param_and_process, process_only)
std_devs_all <- rbind(std_devs_all, param_process_initial)

# save file
write.csv(std_devs_all, "./data/predictive_models/predictive_uncertainty.csv",
          row.names = FALSE)

#### (3) Quickly, Pre-Visualize Uncertainty ####

# predictive uncertainty
ggplot(data = std_devs_all, aes(x = model, y = standard_dev, fill = uncertainty)) +
  geom_boxplot() +
  facet_wrap(~predicting)

# nrmse
ggplot(data = std_devs_all, aes(x = model, y = nrmse, fill = uncertainty)) +
  geom_boxplot() +
  facet_wrap(~predicting)
