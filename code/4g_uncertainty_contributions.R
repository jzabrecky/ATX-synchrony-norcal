#### Calculating standard deviations of predictions to determine uncertainty contributions
### Jordan Zabrecky
## last edited: 01.16.2026

# This script pulls in the prediction matrices and calculates the standard 
# deviation for each model

#### (1) Loading in libraries ####

# loading libraries
lapply(c("tidyverse","plyr"),  require, character.only = T)

# create lists for folders we want to iterate through and different model names
sds <- list(rep(NA, 4))
names(sds) <- c("M_cover_models", "AC_cover_models", "M_atx_models", "AC_atx_models")

#### (2) Calculating Standard deviations for Original Models ####

# original meaning incorporating BOTH process & parameter uncertainty
# (but not intial condition!)

# to-do: load in prediction matrix, calculate sd, save entry in a long format
# reference 
for(i in 1:length(folders)) {
  
  
}
