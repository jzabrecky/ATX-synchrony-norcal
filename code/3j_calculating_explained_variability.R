#### Calculating r^2 for each prediction
### Jordan Zabrecky
## last edited: 10.25.2025

## This script calculates the r^2 statistic (i.e.,  proportion of the variance 
## in the dependent variable that is predictable from the independent variables)
## aka "coefficient of determination" 
## for each prediction by both (1) using the cor() function and (2) using lm() function

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "plyr"), 
       require, character.only = T)

# loading observed
# observed/true values
observed <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  # change site_reach to new names for publication
  mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S")) %>% 
  select(field_date, site_reach, resp_M_cover_norm, resp_AC_cover_norm, resp_M_atx_norm,
         resp_AC_atx_norm)

# get last observed value as our predictive models omit that date
# and instead use the future column to designate the next sampling value
last_day_observed <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  # change site_reach to new names for publication
  mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S")) %>% 
  select(field_date, site_reach, future_M_cover_norm, future_AC_cover_norm, future_M_atx_norm,
         future_AC_atx_norm) %>% 
  filter(field_date == ymd("2023-09-18")) %>% 
  mutate(field_date = ymd("2023-09-24")) %>% 
  dplyr::rename(resp_M_cover_norm = future_M_cover_norm,
                resp_AC_cover_norm = future_AC_cover_norm,
                resp_M_atx_norm = future_M_atx_norm,
                resp_AC_atx_norm = future_AC_atx_norm)

# join in final observations
observed <- rbind(observed, last_day_observed)

# pivot longer 
observed <- pivot_longer(observed, cols = c("resp_M_cover_norm":"resp_AC_atx_norm"), names_to = "predicting",
                         values_to = "observed") %>% 
  # remove "_norm" and "resp"
  mutate(predicting = gsub("_norm", "", predicting)) %>% 
  mutate(predicting = gsub("resp_", "", predicting))

## load predictions
predictions <- ldply(list.files(path = "./data/predictive_models/", pattern = "predictions"), 
                     function(filename) {
                       d <- read.csv(paste("./data/predictive_models/", filename, sep = ""))
                       # add column for what we are predicting
                       d$predicting <- str_remove(filename, "predictions_")
                       d$predicting <- str_remove(d$predicting, ".csv") # remove .csv
                       # some final manipulating
                       d <- d %>% 
                         # convert date from string to date object
                         mutate(field_date = ymd(field_date)) %>% 
                         # change site_reach to new names for publication
                         mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                                       site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                                       site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                                       site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                                       site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S")) %>%
                         filter(model != "null")
                       
                       return(d)
                     })

# remove first days of observations because those values were given
observed <- observed %>% 
  filter(field_date != ymd("2023-06-20")) %>% 
  filter(!(site_reach == "SFE-Upper-1S" & field_date == ymd("2023-06-25")))
predictions <- predictions %>% 
  filter(field_date != ymd("2023-06-20")) %>% 
  filter(!(site_reach == "SFE-Upper-1S" & field_date == ymd("2023-06-25")))

#### (2) Calculate r-squared ####

# join predictions and observed dataframes
together <- left_join(predictions, observed, by = c("field_date", "site_reach", "predicting"))

## test on a single model
# get data
test <- together %>% 
  filter(predicting == "M_cover" & model == "physical" & site_reach == "SFE-Lower-1S")
# lm version
summary(lm(test$mean ~ test$observed))$r.squared # 0.628
cor(x = test$observed, y = test$mean) ^ 2 # 0.628

# calculate for each submodel
variability_1 <- together %>% 
  dplyr::group_by(predicting, model, site_reach) %>% 
  dplyr::summarize(coef_of_deter_lm = summary(lm(mean ~ observed))$r.squared,
                   coef_of_deter_cor = cor(x = observed, y = mean) ^ 2)
# our test value is the same in this dataframe!

# average across those values for a single value for each model
variability_2 <- variability_1 %>% 
  dplyr::group_by(model, predicting) %>% 
  dplyr::summarize(coef_of_deter_lm = mean(coef_of_deter_lm),
                   coef_of_deter_cor = mean(coef_of_deter_cor))

# save csvs
write.csv(variability_1, "./data/predictive_models/rsquared_by_site_reach.csv", row.names = FALSE)
write.csv(variability_2, "./data/predictive_models/rsquared.csv", row.names = FALSE)
