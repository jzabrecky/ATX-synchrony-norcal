#### models to anatoxin concentrations
### Jordan Zabrecky
## last edited: 02.11.2025

# This script builds models to predict anatoxin concentrations (sum of all 
# congeners) of benthic Microcoleus and Anabaena mats (separately)

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders"), require, character.only = T)

# loading in data- see note above
data <- read.csv("./data/predictive_model_inputs/atx_inputs.csv")

#### (2) Separating out data for modeling ####

# model evaluating/testing set list
test_sites <- split(data, data$site_reach)

# model building/training set list
training_sites <- list()
for(i in 1:length(test_sites)) {
  training_sites[[i]] <- data %>%
    filter(site_reach != test_sites[[i]]$site_reach[i]) # CHECK THIS
}
names(training_sites) <- names(test_sites) # name of training site, means it excludes that site

#### (3) Functions for predictions and starting RMSE table ####

## to be done after figuring out RSTAN stuff for truncated normal

#### (4) Running predictive models ####

# set working directory to where STAN files are located
setwd("./code/model_STAN_files")

## Microcoleus

# null model

# initial test run
i <- 1
mod_data <- list(N = nrow(training_sites[[i]]), 
                 L = 0,
                 y = training_sites[[i]]$resp_TM_ATX)
model <- stan(file = "atx_null.stan", data = mod_data,
              chains = 3, iter = 2000, warmup = 1000)
# yay success with truncated normal!