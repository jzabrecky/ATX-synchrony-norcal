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

nrmse_opt2_m <- read.csv("./data/predictive_models/option2/nrmse_M_cover.csv")

nrmse_opt2_m_grouped <- nrmse_opt2_m %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

rhats_M_opt2 <- ldply(list.files(path = "./data/predictive_models/option2/M_cover_models/model_attributes",
                                 pattern = "_rhats.csv"), function(filename) {
                                   d <- read.csv(paste("./data/predictive_models/option2/M_cover_models/model_attributes/",
                                                       filename, sep = ""))
                                   d$model = filename %>% stringr::str_remove("_rhats.csv")
                                   return(d)
                                 })

# 7/25/2025
nrmse_old_linear <- read.csv("./data/predictive_models/july_stuff/nrmse_M_cover.csv") %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

nrmse_new_model <- read.csv("./data/predictive_models/nrmse_M_cover.csv") %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

## sigma test

nrmse_0_5 <- read.csv("./data/predictive_models/compare_sigma_prior/0_5/nrmse_M_cover.csv")
nrmse_0_10 <- read.csv("./data/predictive_models/compare_sigma_prior/0_10/M_cover/nrmse_M_cover.csv")
nrmse_0_15 <- read.csv("./data/predictive_models/compare_sigma_prior/0_15/nrmse_M_cover.csv")


nrmse_0_5_grouped <- nrmse_0_5 %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

nrmse_0_10_grouped <- nrmse_0_10 %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

nrmse_0_15_grouped <- nrmse_0_15 %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

## 7/29/2025 
# model 1 vs. logged model microcoleus
not_logged_m <- read.csv("./data/predictive_models/nrmse_M_cover.csv")
logged_m <- read.csv("./data/predictive_models/log_ver/nrmse_M_cover.csv") %>% 
  mutate(nrmse_recalc = mean * (log(100) - 0.6))

not_logged_m_grouped <- not_logged_m %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))
logged_m_grouped <- logged_m %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

# model 1 vs. logged model anabaena
not_logged_ac <- read.csv("./data/predictive_models/nrmse_AC_cover.csv")
logged_ac <- read.csv("./data/predictive_models/log_ver/nrmse_AC_cover.csv") %>% 
  mutate(nrmse_recalc = mean * (log(100) - 0.6))

not_logged_ac_grouped <- not_logged_ac %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))
logged_ac_grouped <- logged_ac %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

## 7/31/2025

# anatoxin
atx_m <- read.csv("./data/predictive_models/nrmse_M_atx.csv")
atx_ac <- read.csv("./data/predictive_models/nrmse_AC_atx.csv")

m_atx_grouped <- atx_m %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

ac_atx_grouped <- atx_ac %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

## 7/31/2025

# log cover tests
m_log_0.6 <- read.csv("./data/predictive_models/log_tests/starting_value_0.6/nrmse_M_cover.csv")
m_log_0.05 <- read.csv("./data/predictive_models/log_tests/starting_value_0.05/nrmse_M_cover.csv")
m_log_0.01 <- read.csv("./data/predictive_models/log_tests/starting_value_0.01/nrmse_M_cover.csv")
m_log_0.005 <- read.csv("./data/predictive_models/log_tests/starting_value_0.005/nrmse_M_cover.csv")

mean(m_log_0.6$mean, na.omit = TRUE)
mean(m_log_0.05$mean, na.omit = TRUE)
mean(m_log_0.01$mean, na.omit = TRUE)
mean(m_log_0.005$mean, na.omit = TRUE)

ac_log_0.6 <- read.csv("./data/predictive_models/log_tests/starting_value_0.6/nrmse_AC_cover.csv")
ac_log_0.05 <- read.csv("./data/predictive_models/log_tests/starting_value_0.05/nrmse_AC_cover.csv")
ac_log_0.01 <- read.csv("./data/predictive_models/log_tests/starting_value_0.01/nrmse_AC_cover.csv")
ac_log_0.005 <- read.csv("./data/predictive_models/log_tests/starting_value_0.005/nrmse_AC_cover.csv")

mean(ac_log_0.6$mean, na.omit = TRUE)
mean(ac_log_0.05$mean, na.omit = TRUE)
mean(ac_log_0.01$mean, na.omit = TRUE)
mean(ac_log_0.005$mean, na.omit = TRUE)

## full log cover r-hats
m_cover_log <- read.csv("./data/predictive_models/log_ver/nrmse_M_cover.csv")
ac_cover_log <- read.csv("./data/predictive_models/log_ver/nrmse_AC_cover.csv")

m_cover_log_grouped <- m_cover_log %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

ac_cover_log_grouped <- ac_cover_log %>% 
  dplyr::group_by(model) %>% 
  dplyr::summarize(mean_nrmse = mean(mean))

### rhat investigation

# microcoleus cover
m_cover_rhats <- ldply(list.files(path = "./data/predictive_models/M_cover_models/model_attributes",
                                  pattern = "_rhats.csv"), function(filename) {
                                    d <- read.csv(paste("./data/predictive_models/M_cover_models/model_attributes/",
                                                        filename, sep = ""))
                                    d$model = filename %>% stringr::str_remove("_rhats.csv")
                                    return(d)
                                  })

bad_model_m_cover <- readRDS("./data/predictive_models/M_cover_models/all_SFE-M-2")
shinystan::launch_shinystan(bad_model_m_cover)

m_cover_rhats_logged <- ldply(list.files(path = "./data/predictive_models/log_ver/M_cover_models/model_attributes",
                                         pattern = "_rhats.csv"), function(filename) {
                                           d <- read.csv(paste("./data/predictive_models/log_ver/M_cover_models/model_attributes/",
                                                               filename, sep = ""))
                                           d$model = filename %>% stringr::str_remove("_rhats.csv")
                                           return(d)
                                         })

test_m_cover_log <- readRDS("./data/predictive_models/log_ver/M_cover_models/ecohydrological_SFE-M-3")
shinystan::launch_shinystan(test_m_cover_log)

ac_cover_rhats <- ldply(list.files(path = "./data/predictive_models/AC_cover_models/model_attributes",
                                   pattern = "_rhats.csv"), function(filename) {
                                     d <- read.csv(paste("./data/predictive_models/AC_cover_models/model_attributes/",
                                                         filename, sep = ""))
                                     d$model = filename %>% stringr::str_remove("_rhats.csv")
                                     return(d)
                                   })
bad_model_ac_cover <- readRDS("./data/predictive_models/AC_cover_models/physicochemical_SFE-M-1S")
shinystan::launch_shinystan(bad_model_ac_cover)

ac_cover_rhats_logged <- ldply(list.files(path = "./data/predictive_models/log_ver/AC_cover_models/model_attributes",
                                         pattern = "_rhats.csv"), function(filename) {
                                           d <- read.csv(paste("./data/predictive_models/log_ver/AC_cover_models/model_attributes/",
                                                               filename, sep = ""))
                                           d$model = filename %>% stringr::str_remove("_rhats.csv")
                                           return(d)
                                         })

test_ac_cover_log <- readRDS("./data/predictive_models/log_ver/M_cover_models/ecohydrological_SFE-M-3")
shinystan::launch_shinystan(test_ac_cover_log)

# microcoleus atx
m_atx_rhats <- ldply(list.files(path = "./data/predictive_models/M_atx_models/model_attributes",
                                  pattern = "_rhats.csv"), function(filename) {
                                    d <- read.csv(paste("./data/predictive_models/M_atx_models/model_attributes/",
                                                        filename, sep = ""))
                                    d$model = filename %>% stringr::str_remove("_rhats.csv")
                                    return(d)
                                  })

bad_model_m_atx <- readRDS("./data/predictive_models/M_atx_models/ecohydrological_SFE-M-4")
shinystan::launch_shinystan(bad_model_m_atx)

# anabaena atx
ac_atx_rhats <- ldply(list.files(path = "./data/predictive_models/AC_atx_models/model_attributes",
                               pattern = "_rhats.csv"), function(filename) {
                                 d <- read.csv(paste("./data/predictive_models/AC_atx_models/model_attributes/",
                                                     filename, sep = ""))
                                 d$model = filename %>% stringr::str_remove("_rhats.csv")
                                 return(d)
                               })

bad_model_ac_atx <- readRDS("./data/predictive_models/M_atx_models/all_SFE-M-3")
shinystan::launch_shinystan(bad_model_ac_atx)
