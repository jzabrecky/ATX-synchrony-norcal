#### Calculation of 95% dissolved oxygen turnover length (3v/K)
### Jordan Zabrecky
## last edited: 02.05.2026

# This script calculates the single-station metabolism dissolved turnover
# length in which 95% of the dissolved oxygen has turned over 
# calculated as 3v/K (Hall & Hotchkiss 2017)

#### (1) Loading libraries & data ####

# build off of previous script where average velocity at median discharge was already calculated
source("./code/supplemental_code/S1d_K600_prior_estimates.R")

# load our gas-exchange rates (K600) from metabolism modeling
k600 <- ldply(list.files(path = "./data/metab_model_outputs_processed/",
                         pattern = "metab"), function(filename) {
                           d = read.csv(paste("./data/metab_model_outputs_processed/", filename, sep = "")) 
                         }) %>% 
  # remove estimates with biofouling
  filter(!site_year %in% c("russian_2022", "salmon_2022", "salmon_2023"))

# get median K600 from models
median_k600 <- k600 %>% 
  mutate(river = case_when(grepl("sfkeel_mir", site_year) ~ "sfkeel_mir",
                           grepl("sfkeel_sth", site_year) ~ "sfkeel_sth",
                           grepl("salmon", site_year) ~ "salmon",
                           grepl("russian", site_year) ~ "russian")) %>% 
  dplyr::group_by(river) %>% 
  dplyr::summarize(median_k600 = median(K600_daily_mean))

# join dataframes together
medians <- left_join(median_k600, k600_estimates %>% 
                       dplyr::rename(river = site,
                              velocity_m_s = velocity) %>% 
                       select(river, velocity_m_s), by = "river")

#### (2) Calculate DO Residence Distance & Save ####

# calculate 3v/K
footprint_distance <- medians %>% 
  # convert velocity in m/s to m /d
  mutate(velocity_m_d = as.numeric(velocity_m_s) * 86400) %>% 
  # note that K600 is 1/d units so * (m / d) * (d / 1), then divide by 1000 for km
  mutate(turnover_distance_km = (3 * velocity_m_d) / median_k600 / 1000) %>% 
  select(river, velocity_m_s, velocity_m_d, median_k600, turnover_distance_km)

# save dataframe!
write.csv(footprint_distance, "./data/metab_model_inputs/DO_residence_distance.csv", row.names = FALSE)
