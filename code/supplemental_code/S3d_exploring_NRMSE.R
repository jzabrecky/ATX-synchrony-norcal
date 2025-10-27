#### Exploring NRMSEs
### Jordan Zabrecky
## last edited: 10.27.2025

## This code explores the NRMSEs of predictive models

#### (1) Loading libraries & data

# load NRMSEs
NRMSEs <- ldply(list.files(path = "./data/predictive_models/", pattern = "NRMSE"), 
                function(filename) {
                  d <- read.csv(paste("./data/predictive_models/", filename, sep = "")) %>% 
                    # add in what we are predicting from file name
                    mutate(predicting = str_remove(filename, "NRMSE_" )) %>% 
                    mutate(predicting = str_remove(predicting, ".csv")) %>% 
                    # remove null models
                    filter(model != "null") %>% 
                    # remove NMRSEs where site_reach predicted are separated out
                    filter(predicting %in% c("AC_atx", "AC_cover", "M_atx", "M_cover")) %>% 
                    # model base (excluding with cover argument) 
                    mutate(model_base = case_when(grepl("_w_cover", model) ~ str_remove(model, "_w_cover"),
                                                  TRUE ~ model)) %>% 
                    # add column (T/F) if cover is included as covariate
                    mutate(cover_covariate = case_when(grepl("w_cover", model) ~ TRUE,
                                                       TRUE ~ FALSE),
                           # make a final column that tells both what we are predicting and if cover
                           # is a covariate
                           predicting_w_cover = case_when(cover_covariate == TRUE ~ paste(predicting, "_w_cover", sep = ""),
                                                          TRUE ~ predicting))
                })

# calculate mean of mean and ci's (without cover models separated out)
mean_NRMSEs <- NRMSEs %>% 
  dplyr::group_by(predicting) %>% 
  dplyr::summarize(mean_mean = mean(mean),
                   mean_lower_ci = mean(lower_ci),
                   mean_upper_ci = mean(upper_ci))

# calculate mean of mean and ci's (with cover models separated out)
mean_NRMSEs_w_cover <- NRMSEs %>% 
  dplyr::group_by(predicting_w_cover) %>% 
  dplyr::summarize(mean_mean = mean(mean),
                   mean_lower_ci = mean(lower_ci),
                   mean_upper_ci = mean(upper_ci))

# get null models only
null_models <- ldply(list.files(path = "./data/predictive_models/", pattern = "NRMSE"), 
                     function(filename) {
                       d <- read.csv(paste("./data/predictive_models/", filename, sep = "")) %>% 
                         # add in what we are predicting from file name
                         mutate(predicting = str_remove(filename, "NRMSE_" )) %>% 
                         mutate(predicting = str_remove(predicting, ".csv")) %>% 
                         # remove null models
                         filter(model == "null")
                     })

#### (2) Miscellaneous Questions ####

## What is the mean NRMSE for each thing we are predicting?
view(mean_NRMSEs)
view(mean_NRMSEs_w_cover)

## Which/how many models outperform the null?
NRMSEs_null <- left_join(NRMSEs, null_models, by = c("predicting")) %>% 
  mutate(outperform = case_when(mean.x < mean.y ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::group_by(predicting) %>% 
  dplyr::summarize(outperforming = sum(outperform),
                   total = length(outperform))
NRMSEs_null <- left_join(NRMSEs, null_models, by = c("predicting")) %>% 
  mutate(outperform = case_when(mean.x < mean.y ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::group_by(predicting_w_cover) %>% 
  dplyr::summarize(outperforming = sum(outperform),
                   total = length(outperform))

## What models performed best for each?
performance <- split(NRMSEs, NRMSEs$predicting)
view(performance$AC_cover)
view(performance$M_cover)
view(performance$M_atx)
view(performance$AC_atx)

## What are the range of NRMSEs
performance_w_c <- split(NRMSEs, NRMSEs$predicting_w_cover)
max(performance_w_c$M_cover$mean) - min(performance_w_c$M_cover$mean) # 0.13
max(performance_w_c$AC_cover$mean) - min(performance_w_c$AC_cover$mean) # 0.22

max(performance_w_c$M_atx$mean) - min(performance_w_c$M_atx$mean) # 0.08
max(performance_w_c$M_atx_w_cover$mean) - min(performance_w_c$M_atx_w_cover$mean) # 0.07
max(performance_w_c$AC_atx$mean) - min(performance_w_c$AC_atx$mean) # 0.05
max(performance_w_c$AC_atx_w_cover$mean) - min(performance_w_c$AC_atx_w_cover$mean) # 0.05
max(performance$M_atx$mean) - min(performance$M_atx$mean) # 0.08
