#### Getting NRMSE summaries for each model
### Jordan Zabrecky
## last edited: 10.14.2025

## This code reads in NRMSEs for all predictions for a type of model and 
## summarizes them via mean & 95% confidence interval

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "plyr"), require, character.only = T)

# different categories of what we are predicting
predicting <- c("M_cover", "AC_cover", "M_atx", "AC_atx")

#### (2) Reading in NRMSE vectors ####

# empty list for all NRMSEs
NRMSE_list <- list()

# pull all saved vectors for each "predicting"
for(i in 1:length(predicting)) {
  NRMSE_list[[i]] <- ldply(list.files(path = paste("./data/predictive_models/", predicting[i], 
                                         "_models/NRMSE_vectors/", sep = ""), 
                            pattern = "NRMSE"),
                 function(filename) {
                   # load csv
                   file <- read.csv(paste("./data/predictive_models/", predicting[i], 
                                            "_models/NRMSE_vectors/", filename, sep = ""))
                   
                   # add information
                   file$model <- str_extract(filename, ".+?(?=_)")
                   file$site_reach <- strsplit(filename, "_")[[1]][2]

                   return(file)
                 })
  
}

# add names to list
names(NRMSE_list) <- predicting

#### (3) Summarizing NRMSEs for each model ####

# summarize NRMSEs
NRMSE_list_summary <- lapply(NRMSE_list, function(x) x <- x %>%
                       dplyr::group_by(model) %>% 
                       dplyr::summarize(mean = mean(x),
                                        lower_ci = quantile(x, 0.025),
                                        upper_ci = quantile(x, 0.975)))

#### (4) Add in the null NRMSE for each model and save ####

# read in null csv and add it into list
for(i in 1:length(predicting)) {
  null <- read.csv(paste("./data/predictive_models/", predicting[i], "_models/NRMSE_vectors/null.csv", sep = "")) %>% 
    dplyr::summarize(model = "null",
                     mean = mean(x),
                     # no point in 95% CI as there are only five points!
                     lower_ci = mean(x),
                     upper_ci = mean(x))
  NRMSE_list_summary[[i]] <- rbind(NRMSE_list_summary[[i]], null)
}

# save csv's
lapply(names(NRMSE_list_summary), function(x) write.csv(NRMSE_list_summary[[x]], paste("./data/predictive_models/NRMSE_", 
                                                                                       x, ".csv", sep = ""),
                                                        row.names = FALSE))

#### (5) For supplemental figure, summarize by site_reach
NRMSE_list_site_reach<- lapply(NRMSE_list, function(x) x <- x %>%
                               dplyr::group_by(model, site_reach) %>% 
                               dplyr::summarize(mean = mean(x),
                                                lower_ci = quantile(x, 0.025),
                                                upper_ci = quantile(x, 0.975)))

# save csv's
lapply(names(NRMSE_list_site_reach), function(x) write.csv(NRMSE_list_summary[[x]], paste("./data/predictive_models/NRMSE_", 
                                                                                       x, "_by_site_reach.csv", sep = ""),
                                                        row.names = FALSE))

#### (5) Posterior Parameter Comparisons ####

## (a) comparing M cover vs. AC cover
# (need to remove sites and reaches omitted from AC cover)

divergent_models <- 

which(NRMSE_list$M_cover %>%  > NRMSE_list$AC_cover)
