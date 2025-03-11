#### models to predict cover
### Jordan Zabrecky
## last edited: 03.10.2025

# This script builds models to predict cover of benthic Microcoleus
# and Anabaena (separately) as determined by benthic cover surveys

#### (1) Loading data and libraries ####

# loading libraries
lapply(c("tidyverse", "rstan", "StanHeaders"), require, character.only = T)

# loading in data- see note above
data <- read.csv("./data/predictive_model_inputs/cover_inputs.csv")