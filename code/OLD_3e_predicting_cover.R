#### models to predict cover
### Jordan Zabrecky
## last edited: 02.04.2025

# This script builds models to predict cover of benthic Microcoleus
# and Anabaena (separately) as determined by benthic cover surveys

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders"), require, character.only = T)

# loading in data- see note above
data <- read.csv("./data/predictive_model_inputs/cover_inputs.csv")

# mutating data to avoid 0's (fill with 0.5)
data$resp_micro[which(data$resp_micro == 0)] <- 1
data$resp_anacyl[which(data$resp_anacyl == 0)] <- 1

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

# need a way to easily add RMSE; include vector and name in function

# for prediction function use vars as a vector/matrix?
# would compare against one where it is written out
# but doing null model only first!

#### (4) Running predictive models ####

# set working directory to where STAN files are located
setwd("./code/model_STAN_files")

## Microcoleus

# null model

# initial test run
i <- 1
mod_data <- list(N = nrow(training_sites[[i]]), 
                 y = as.integer(training_sites[[i]]$resp_micro))
model <- stan(file = "cover_null.stan", data = mod_data,
              chains = 3, iter = 5000, warmup = 2000)

# maybe can just do truncated normal?
