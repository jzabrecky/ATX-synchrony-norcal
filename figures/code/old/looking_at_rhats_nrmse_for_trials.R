### will delete this :)

library(tidyverse)
library(plyr)

nrmse_0.01 <- read.csv("./data/predictive_models/add_0.01_withtruncation/nrmse_M_cover.csv")
nrmse_0.05 <- read.csv("./data/predictive_models/add_0.05_withtruncation/nrmse_M_cover.csv")
nrmse_0.1 <- read.csv("./data/predictive_models/add_0.1_withtruncation/nrmse_M_cover.csv")

nrmse_0.01_grouped <- nrmse_0.01 %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

nrmse_0.05_grouped <- nrmse_0.05 %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

nrmse_0.1_grouped <- nrmse_0.1 %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

rhats_0.01 <- ldply(list.files(path = "./data/predictive_models/add_0.01_withtruncation/M_cover_models/model_attributes",
                                 pattern = "_rhats.csv"), function(filename) {
  d <- read.csv(paste("./data/predictive_models/add_0.01_withtruncation/M_cover_models/model_attributes/",
                      filename, sep = ""))
  d$model = filename %>% stringr::str_remove("_rhats.csv")
  return(d)
})

rhats_anacyl <- ldply(list.files(path = "./data/predictive_models/add_0.1_withtruncation/AC_cover_models/model_attributes",
                               pattern = "_rhats.csv"), function(filename) {
                                 d <- read.csv(paste("./data/predictive_models/add_0.1_withtruncation/AC_cover_models/model_attributes/",
                                                     filename, sep = ""))
                                 d$model = filename %>% stringr::str_remove("_rhats.csv")
                                 return(d)
                               })
