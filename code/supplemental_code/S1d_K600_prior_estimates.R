#### Code for calculating K600 estimates to use as a prior
### Jordan Zabrecky
## last edited: 02.27.2025

# This code calculates a K600 estimate using slope, median velocity, median depth,
# and equation 1 from Raymond et al. (2012) to use as a prior in metabolism models

#### (1) Loading libraries and data ####

# use work from script that builds depth discharge relationships
# will also load in libraries from this script
source("code/1f_processing_metabolism_outputs.R")

# need to also get geomorphology data for South Fork Eel at Miranda and Standish Hickey
geomorph_sfkeel_mir <- geomorph_data("11476500")
  
geomorph_sfkeel_sth <- geomorph_data("11475800")

#### (2) Getting slope information ####

## we have slope estimates using external data sources
## slope must be unitless (i.e. m/m)

# Russian River (Stillwater Sciences 2016):
# slopes are from ~10 river miles downstream but are likely close to that of our site/reaches
# figure 8 for Alexander Valley which is the reach closest to our sites: 43 feet per 8 river mile
# so 13.11 m / 12,874.8 m = 0.001018 or ~0.1%
rus_slope <- 0.001018

# Salmon River (Ayers Associates 1999):
# figure 8.8: slope is 24.3 feet/mile above Somes Bar
# so 7.41 m / 1609.34 m = 0.004604 or ~0.5% (a bit lower than I would expect but ok)
sal_slope <- 0.004604

# South Fork Eel River (Foster and Kelsey 2012):
# figure 6 at Standish Hickey (~20 river km upstream of Standley Creek)
# looks like ~ 50 m for 20 km so 0.0025 or 0.25%
# figure 6 at Miranda reaches (~45 river km downstream of Standley Creek)
# looks like ~ 25 m for 20 km so 0.00125 or 0.125%
sth_slope <- 0.0025
mir_slope <- 0.00125

#### (2) Determine median depths & velocity for study period ####

# create models for discharge vs. velocity
russian_velocity_model <- lm(log(velocity_m_s) ~ log(discharge_m3_s), data = na.omit(geomorph_rus))
salmon_velocity_model <- lm(log(velocity_m_s) ~ log(discharge_m3_s), data = na.omit(geomorph_sal))
mir_velocity_model <- lm(log(velocity_m_s) ~ log(discharge_m3_s), data = na.omit(geomorph_sfkeel_mir))
sth_velocity_model <- lm(log(velocity_m_s) ~ log(discharge_m3_s), data = na.omit(geomorph_sfkeel_sth))

# russian river
rus_dis_all <- USGS_daily_discharge$russian %>% 
  filter(date >= "2022-06-24" & date <= "2022-09-15")
rus_dis <- median(rus_dis_all$discharge_m3_s)
rus_depth <- median(rus_dis_all$depth_m)
rus_vel <- as.numeric(exp(russian_velocity_model$coefficients[1] + log(rus_dis) * russian_velocity_model$coefficients[2]))

# salmon river
sal_dis_all <- USGS_daily_discharge$salmon %>% 
  filter(date >= "2022-06-26" & date <= "2023-09-27") %>% 
  filter(date <= "2022-09-22" | date >= "2023-06-27")
sal_dis <- median(sal_dis_all$discharge_m3_s)
sal_depth <- median(sal_dis_all$depth_m)
sal_vel <- as.numeric(exp(salmon_velocity_model$coefficients[1] + log(sal_dis) * salmon_velocity_model$coefficients[2]))
# lower than expected compared to russian, but it's the best we've got...

# south fork eel river @ miranda
mir_dis_all <- USGS_daily_discharge$sfkeel_mir %>% 
  filter(date >= "2022-06-29" & date <= "2023-09-27") %>% 
  filter(date <= "2022-09-17" | date >= "2023-06-18")
mir_dis <- median(mir_dis_all$discharge_m3_s)
mir_depth <- median(mir_dis_all$depth_m)
mir_vel <- as.numeric(exp(mir_velocity_model$coefficients[1] + log(mir_dis) * mir_velocity_model$coefficients[2]))
# value here makes sense!

# south fork eel river @ standish hickey
sth_dis_all <- USGS_daily_discharge$sfkeel_sth %>% 
  filter(date >= "2023-06-24" & date <= "2023-09-27")
sth_dis <- median(sth_dis_all$discharge_m3_s)
sth_depth <- median(sth_dis_all$depth_m)
sth_vel <- as.numeric(exp(sth_velocity_model$coefficients[1] + log(sth_dis) * sth_velocity_model$coefficients[2]))

#### (3) Use equation 1 from Raymond et al. (2012) for k600 estimate ####

# function for equation: velocity in m/s, slope is unitless, depth in m
raymond_eq <- function(velocity, slope, depth) {
  # equation 1 (not bothering with upper or lower bounds)
  k600_est <- (velocity * slope)^0.89 * (depth)^0.54 * 5037
  return(c(k600_est, velocity, slope, depth))
}

# make empty dataframe
k600_estimates <- data.frame(site = character(), 
                             k600_prior = numeric(),
                             velocity = numeric(),
                             slope = numeric(),
                             depth = numeric())

# add calculations to dataframe
k600_estimates[1,] <- c("russian", raymond_eq(rus_vel, rus_slope, rus_depth))
k600_estimates[2,] <- c("salmon", raymond_eq(sal_vel, sal_slope, sal_depth))
k600_estimates[3,] <- c("sfkeel_mir", raymond_eq(mir_vel, mir_slope, mir_depth))
k600_estimates[4,] <- c("sfkeel_sth", raymond_eq(sth_vel, sth_slope, sth_depth))

# saving dataframe
write.csv(k600_estimates, "./data/metab_model_inputs/k600_estimates.csv", row.names = FALSE)
