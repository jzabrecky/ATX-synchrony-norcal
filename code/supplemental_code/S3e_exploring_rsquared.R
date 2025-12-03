#### Exploring r-squared 
### Jordan Zabrecky
## last edited: 10.25.2025

# This code explores the r-squared of each model

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse"), require, character.only = T)

# loading r-squared values
rsquared <- read.csv("./data/predictive_models/rsquared.csv")

#### (2) Misc. questions ####

## Overall, what is the average explained variability for each?
mean <- rsquared %>% 
  dplyr::group_by(predicting) %>% 
  dplyr::summarize(mean = mean(coef_of_deter_lm),
                   max = max(coef_of_deter_lm),
                   min = min(coef_of_deter_lm))

## What about when separating out cover as a covariate?
mean_cover_separate <- rsquared %>% 
  mutate(predicting_w_cover = case_when(grepl("cover", model) ~ paste(predicting, "_w_cover", sep = ""),
                                        TRUE ~ predicting)) %>% 
  dplyr::group_by(predicting_w_cover) %>% 
  dplyr::summarize(mean = mean(coef_of_deter_lm),
                   max = max(coef_of_deter_lm),
                   min = min(coef_of_deter_lm))
