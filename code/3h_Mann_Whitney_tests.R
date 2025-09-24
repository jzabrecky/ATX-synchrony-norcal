#### Mann Whitney tests to compare models
### Jordan Zabrecky
## last edited: 09.24.2025

## This code completes Mann Whitney tests to compare performance (via mean NRMSE) of:
## (A) Microcoleus vs. Anabaena/Cylindrospermum cover predictions
## (B) Microcoleus vs. Anabaena/Cylindrospermum mat anatoxin predictions
## (C) Microcoleus mat anatoxins predictions with vs. without cover as a covariate
## (D) Anabaena/Cylindrospermum mat anatoxins predictions with vs. without cover as a covariate

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", require, character.only = T))

# loading data
# loading data
NRMSEs <- ldply(list.files(path = "./data/predictive_models/", pattern = "nrmse"), 
                function(filename) {
                  d <- read.csv(paste("./data/predictive_models/", filename, sep = ""))
                  # add column for what we are predicting
                  d$predicting <- str_remove(filename, "nrmse_")
                  d$predicting <- str_remove(d$predicting, ".csv") # remove .csv
                  # some final manipulating
                  d <- d %>% 
                    # add column (T/F) if cover is included as covariate
                    mutate(cover_covariate = case_when(grepl("w_cover", model) ~ TRUE,
                                                       TRUE ~ FALSE),
                           # make a final column that tells both what we are predicting and if cover
                           # is a covariate (to separate with vs. without cover easily)
                           predicting_w_cover = case_when(cover_covariate == TRUE ~ paste(predicting, "_w_cover", sep = ""),
                                                          TRUE ~ predicting)) %>% 
                    # filter to exclude null models
                    filter(!model == "null")
                  return(d)
                })

# check for normality
shapiro.test(NRMSEs$mean) # not normal, non-parametric test required

#### (2) Test A: M vs. AC cover ####

# exclude ATX predictions from data
test_A <- NRMSEs %>% 
  filter(predicting == "M_cover" | predicting == "AC_cover")

# Wilcox test
wilcox.test(mean ~ predicting, data = test_A)
# W = 933, p-value = 9.066e-08; aka significant ***

#### (3) Test B: M vs. AC mat anatoxins ####

# exclude cover predictions from data
test_B <- NRMSEs %>% 
  filter(predicting == "M_atx" | predicting == "AC_atx")

# Wilcox test
wilcox.test(mean ~ predicting, data = test_B)
# W = 1868, p-value = 0.01537; aka significant *

#### (4) Test C: M mat anatoxins w/ vs. w/o cover covariate ####

# exclude A/C predictions from data
test_C <- NRMSEs %>% 
  filter(predicting == "M_atx")

# Wilcox test
wilcox.test(mean ~ predicting_w_cover, data = test_C)
# W = 1868, p-value = 0.1976; aka not significant

#### (5) Test D: AC mat anatoxins w/ vs. w/o cover covariate ####

# exclude M predictions from data
test_D <- NRMSEs %>% 
  filter(predicting == "AC_atx")

# Wilcox test
wilcox.test(mean ~ predicting_w_cover, data = test_D)
# W = 1138, p-value = 5.28e-12; aka significant ***
