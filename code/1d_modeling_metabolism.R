#### modeling metabolism and functions to assess model outputs
### Jordan Zabrecky
## last edited 07.04.2024

# This code models metabolism using the "streamMetabolizer" package

#### (1) Loading packages and reading in data #### 
lapply(c("rstan", "StanHeaders", "streamMetabolizer", "tidyverse", "plyr"), 
       require, character.only = T)

## Loading libraries
# if "rstan" an "StanHeaders" is not downloaded, see documentation here for downloading:
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

# if "streamMetabolizer" is not downloaded, see documentation here for downloading:
# https://github.com/DOI-USGS/streamMetabolizer

## Reading in model input data (from starting working directory)
model_inputs <- ldply(list.files(path = "./data/metab_model_inputs/", pattern = "modelinputs"), function(filename) {
  d <- read.csv(paste("data/metab_model_inputs/", filename, sep = ""))
  return(d)
})

# convert date_time to POSIXct class
model_inputs$date_time <- as.POSIXct(model_inputs$date_time, format = "%Y-%m-%d %H:%M:%S", 
                                     tz= "America/Los_Angeles")

# need to add longitude for each site to later calculate solar time
model_inputs <- model_inputs %>% 
  mutate(
    longitude = case_when(site == "russian" ~ -123.007017,
                          site == "salmon" ~ -123.4770326,
                          site == "sfkeel_mir" ~ -123.775930,
                          site == "sfkeel_sth" ~ -123.727924)
  )

# separating into a list based on site/year
inputs_list <- split(model_inputs, model_inputs$site_year)

#### (2) Final prepping of data inputs for use in streamMetabolizer ####

# check specific input requirements from streamMetabolizer
metab_inputs('bayes', 'data')

# function to modify data frames to match the above requirements
metab_prep <- function(df) {
  new_df <- df %>% 
    # calculate solar time function from streamMetabolizer
    # will account for our data being in PST
    mutate(solar.time = calc_solar_time(date_time, longitude),
           DO.obs = DO_mgL,
           # calculate DO saturation using function from streamMetabolizer
           DO.sat = calc_DO_sat(Temp_C, pressure_mbar, salinity.water = 0, 
                                model = "garcia-benson"),
           depth = depth_m, 
           temp.water = Temp_C,
           # StreamLight gives us PAR in the appropriate units
           light = PAR_surface,
           discharge = discharge_m3_s) %>% 
    dplyr::select(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge)
  return(new_df)
}

# apply function to dataframes
inputs_prepped <- lapply(inputs_list, function(x) metab_prep(x))

#### (3) Modelling metabolism and saving & viewing outputs ####

# function to visualize inputs 
visualize_inputs <- function(df){
  
  # plot for dissolved oxygen data inputs
  oxygen_plots <- df %>% 
    mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%  # calulating percent saturation
    dplyr::select(solar.time, starts_with('DO')) %>% 
    # putting all DO in one column to be able to facet wrap
    gather(type, DO.value, starts_with("DO")) %>% 
    # assigns correct labels to graphs
    mutate(units = ifelse(type == "DO.pctsat", "DO\n(% sat)", "DO\n(mg/L)")) %>% 
    ggplot(aes(x = solar.time, y = DO.value, color = type)) + geom_line() +
    # facet wrap based on units
    facet_grid(units ~ ., scale = "free_y") + theme_bw() + scale_color_discrete("variable")
  
  # creating labels for next plot
  labels <- c(depth = "depth\n(m)", temp.water = "water temp\n(deg C)", light = "Par\n(umol m^-2 s^-1)")
  # plot for other variable inputs
  othervar_plots <- df %>% 
    dplyr::select(solar.time, depth, temp.water, light) %>% 
    # putting all other variables in one column to be able to facet wrap
    gather(type, value, depth, temp.water, light) %>% 
    mutate(
      # order based on type with given list of the types
      type = ordered(type, levels = c("depth", "temp.water", "light")),
      units = ordered(labels[type], unname(labels))) %>% 
    ggplot(aes(x = solar.time, y = value, color = type)) + geom_line() +
    # facet wrap based on units
    facet_grid(units ~ ., scale = "free_y") + theme_bw() + 
    scale_color_discrete("variable")
  
  # print plots
  print(oxygen_plots)
  print(othervar_plots)
}

# function to easily save output data after model run
write_files <- function(data_fit, data_metab, subfolder, SiteID){
  for (i in seq_along(data_fit)) {
    filename = paste(".", subfolder, SiteID, names(data_fit)[i], ".csv", sep="")
    write.csv(data_fit[[i]], filename)
  }
  write.csv(unlist(get_specs(data_metab)), paste(".", subfolder, SiteID, "_specs.csv", sep=""),
            row.names = FALSE)
  write.csv(get_data_daily(data_metab), paste(".", subfolder, SiteID, "_datadaily.csv", sep=""),
            row.names = FALSE)
  write.csv(get_data(data_metab), paste(".", subfolder, SiteID, "_mod_and_obs_DO.csv", sep=""),
            row.names = FALSE)
  saveRDS(data_metab, paste(".", subfolder, SiteID, "metab_obj.rds", sep=""))
}

# function to create plot of binning
plot_binning <- function(site_fit, site_metab, title = NULL) {
  SM_output <- site_fit$daily
  SM_day <- get_data_daily(site_metab)
  SM_KQbin <- site_fit$KQ_binned # date and time index NA?
  SM_specs <- get_specs(site_metab)
  
  day <- data.frame(SM_day$discharge.daily, SM_output$K600_daily_50pct, rep('daily', dim(SM_output)[1]))
  colnames(day)<-c('Q', 'K600', 'Group')
  
  nodes<-data.frame(exp(as.numeric(as.character(SM_specs$K600_lnQ_nodes_centers))), exp(SM_KQbin$lnK600_lnQ_nodes_50pct), rep('node', dim(SM_KQbin)[1]))
  colnames(nodes)<-c('Q', 'K600', 'Group')
  KQ_plot<-rbind(day,nodes)
  
  ggplot(data=KQ_plot, aes(x=log(Q), y=K600, group=Group, colour=Group)) + 
    geom_point(size=3) +
    #geom_line() + 
    scale_color_manual(name="K-Q",
                       breaks = c("daily", "node"),
                       values=c("grey", "purple"),
                       labels=c("Daily","Bin")) +
    ylab("K600") +
    xlab("logQ") +
    ggtitle(title) +
    theme_bw() +
    theme(legend.position = "top")
}

# function to create plot of ER vs. K600
plot_ER_K600 <- function(metab_fit, title) {
  # get data from fit list
  data <- metab_fit$daily

  # make plot
  ggplot(data = data, aes(x = ER_daily_mean, y = K600_daily_mean)) +
    geom_point(color = "dodgerblue3", size = 3) +
    ylab("K600 daily mean") +
    xlab("ER daily mean") +
    ggtitle(title) + 
    theme_bw()
}

# function to create plot of K600 over time
plot_K600 <- function(metab_fit, title) {
  # get data from fit list
  data <- metab_fit$daily
  
  # make plot
  ggplot(data = data, aes(x = date, y = K600_daily_mean)) +
    geom_point(color = "dodgerblue", size = 3) +
    ylab("K600 daily mean") +
    xlab("Date") +
    ggtitle(title) +
    theme_bw()
}

# set working directory
setwd("data/metab_model_outputs")

## south fork eel @ miranda 2022

# USING A SUBSET FOR NOW
test <- inputs_prepped$sfkeel_mir_2022_barochange %>% 
  dplyr::filter(solar.time >= "2022-08-01 00:00:00" & solar.time <= "2022-08-05 00:00:00")

# visualize_inputs
visualize_inputs(inputs_prepped$sfkeel_mir_2022)
visualize_inputs(test)

# setting model specs
# setting bayesian model specifications
bayes_name_sfkeel_mir_2022 <- mm_name(type='bayes', pool_K600="binned",
                                      err_obs_iid=TRUE, err_proc_iid = TRUE,
                                      ode_method = "trapezoid", deficit_src='DO_mod', engine='stan')
bayes_specs_sfkeel_mir_2022 <- specs(bayes_name_sfkeel_mir_2022, burnin_steps = 1000, saved_steps = 5000,
                                     thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10) # trying to go for more iterations?

bayes_specs_sfkeel_mir_2022

# changing to fit range of log(Q) for site
bayes_specs_sfkeel_mir_2022$K600_lnQ_nodes_centers <- c(-1.5, -1, -.5, 0, .5, 1, 1.5)
bayes_specs_sfkeel_mir_2022$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5 apart rather than 1

# TEST MODEL
test_week_bayes_barochange <- metab(bayes_specs_sfkeel_mir_2022, data = test)
test_week_bayes_barochange_fit <- get_fit(test_week_bayes_barochange)

# run model
sfkeel_mir_2022 <- metab(bayes_specs_sfkeel_mir_2022, data = inputs_prepped$sfkeel_mir_2022)
sfkeel_mir_2022_fit <- get_fit(sfkeel_mir_2022)

# check warnings
sfkeel_mir_2022_fit[["warnings"]]
test_week_bayes_fit[["warnings"]]
test_week_bayes_barochange_fit[["warnings"]]

# inspect MCMC chain
rstan::traceplot(get_mcmc(test_week_bayes), pars='GPP_daily', nrow=3)
rstan::traceplot(get_mcmc(test_week_bayes), pars='ER_daily', nrow=3)
rstan::traceplot(get_mcmc(test_week_bayes), pars='K600_daily', nrow=3)

# check rhat for convergence
sfkeel_mir_2022_fit$overall %>%
  dplyr::select(ends_with('Rhat'))

# save files
write_files(sfkeel_mir_2022_fit, sfkeel_mir_2022, "/sfkeel_mir_2022/20240709/", 
            "sfkeel_mir_2022")

write_files(test_week_bayes_fit, test_week_bayes, "/sfkeel_mir_2022/week_test/20240709/",
            "sfkeel_mir_2022")

# visualizing outputs
plot_binning(sfkeel_mir_2022_fit, sfkeel_mir_2022, 
             title = "South Fork Eel @ Miranda, 2022")
plot_metab_preds(predict_metab(sfkeel_mir_2022))
plot_DO_preds(predict_DO(sfkeel_mir_2022, date_start = "2022-08-01", date_end = "2022-08-03"))
plot_ER_K600(sfkeel_mir_2022_fit, title = "South Fork Eel @ Miranda, 2022")
plot_K600(sfkeel_mir_2022_fit, title = "South Fork Eel @ Miranda, 2022")

plot_DO_preds(predict_DO(test_week_bayes))
plot_ER_K600(test_week_bayes_fit, title = "South Fork Eel @ Miranda, 2022")

plot_DO_preds(predict_DO(test_week_bayes_barochange))

# K600 and ER correlation test
cor.test(sfkeel_mir_2022_fit$daily$K600_daily_mean, sfkeel_mir_2022_fit$daily$ER_daily_mean)

# remove large .rmd before starting next model (to avoid R crashing)
rm(sfkeel_mir_2022)
rm(test_week_bayes)
test_week_bayes <- readRDS("sfkeel_mir_2022/week_test/20240709/sfkeel_mir_2022metab_obj.rds")

### misc. plots to test

# DO data
lims <- as.POSIXct(strptime(c("2022-08-01 00:00", "2022-08-03 00:00"), 
                            format = "%Y-%m-%d %H:%M"))
# metab prepped
ggplot(data = inputs_prepped$sfkeel_mir_2022, aes(x = solar.time, y = temp.water)) +
  scale_x_datetime(limits = lims) +
  geom_point(color = "blue")
ggplot(data = inputs_prepped$sfkeel_mir_2022, aes(x = solar.time, y = DO.obs)) +
  scale_x_datetime(limits = lims) +
  geom_point(color = "green")
ggplot(data = inputs_prepped$sfkeel_mir_2022, aes(x = solar.time, y = light)) +
  scale_x_datetime(limits = lims) +
  geom_point(color = "red")
ggplot(data = inputs_list$sfkeel_mir_2022, aes(x = date_time, y = Temp_C)) +
  scale_x_datetime(limits = lims) +
  geom_point()

# same plot
stupid <- inputs_prepped$sfkeel_mir_2022 %>% 
  mutate(streamLight = light / 50) %>% 
  dplyr::select(solar.time, temp.water, DO.obs, streamLight) %>% 
  dplyr::filter(solar.time >= "2022-08-01 00:00" & solar.time <= "2022-08-03 00:00")

stupid <- stupid %>% 
  pivot_longer(cols = 2:4, names_to = "type", values_to = "value")

ggplot(data = stupid, aes(x = solar.time, y = value, color = type)) +
  geom_point() +
  scale_color_manual(values = c("green", "red", "blue")) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 50, name = "streamLight axis"))


# looking at old with just NLDAS
old_sfkeel <- read.csv("R:/Blaszczak_Lab/Ongoing Projects/JMZ/metabolism-norcal-2022-23/data/model_inputs/sfkeel_mir_2022_05232024.csv")
old_sfkeel$date_time <- as.POSIXct(old_sfkeel$date_time, format = "%Y-%m-%d %H:%M:%S", 
                                   tz= "America/Los_Angeles")

old_prep <- function(df, longitude) {
  # function from "streamMetabolizer" to calculate solar time
  df$solar.time <- calc_solar_time(df$date_time, longitude = longitude) # CHECK THAT THIS IS PST AND NOT UTC
  # need to calculate mbar for "calc_DO_sat()" function from "streamMetabolizer"
  df$mbar <- df$pressure_mmHg * 1.33322 # SHOULD CALCULATE MBAR FROM PA OF GLDAS!!!
  df$DO.sat <- calc_DO_sat(temp.water = df$Temp_C, pressure.air = df$mbar,
                           salinity.water = 0, model = "garcia-benson")
  # renaming columns to match "streamMetabolizer" inputs and selecting only those needed
  df <- df %>% 
    dplyr::rename(DO.obs = DO_mgL, depth = depth, temp.water = Temp_C, light = SW,
                  discharge = discharge_m3_s) %>% 
    dplyr::select(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge)
  
  return(df)
}

test <- old_prep(old_sfkeel, -123.775930)

ggplot(data = test, aes(x = solar.time, y = light)) +
  scale_x_datetime(limits = lims) +
  geom_point(color = "red") +
  ggtitle("NLDAS only (no streamLight")

#### TEST EXAMPLE FROM PACKAGE
dat <- data_metab(num_days='3', res='15', day_start=4, day_end=28)
bayes_name <- mm_name(type='bayes', pool_K600='none', err_obs_iid=TRUE, err_proc_iid=TRUE)
bayes_name
bayes_specs <- specs(bayes_name)
bayes_specs

bayes_specs <- revise(bayes_specs, burnin_steps=100, saved_steps=200, n_cores=1, GPP_daily_mu=3, GPP_daily_sigma=2)
mm <- metab(bayes_specs, data=dat)
mm
predict_metab(mm)
plot_metab_preds(mm)
get_params(mm)
predict_DO(mm) %>% head()
plot_DO_preds(mm)
mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=3)


### trial with sfk eel mir 2023 data 

# subset after running above code
subset2023 <- inputs_prepped$sfkeel_mir_2023 %>% 
  dplyr::filter(solar.time >= "2023-07-15 00:00:00" & solar.time <= "2023-07-23 00:00:00")

# visualize inputs
visualize_inputs(subset2023)

# specs
bayes_name_sfkeel_mir_2023 <- mm_name(type='bayes', pool_K600="binned",
                                      err_obs_iid=TRUE, err_proc_iid = TRUE,
                                      ode_method = "trapezoid", deficit_src='DO_mod', engine='stan')
bayes_specs_sfkeel_mir_2023 <- specs(bayes_name_sfkeel_mir_2023, burnin_steps = 1000, saved_steps = 5000,
                                     thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)# trying to go for more iterations?

# trial
sfkeel_mir_2023 <- metab(bayes_specs_sfkeel_mir_2023, data = subset2023)
sfkeel_mir_2023_fit <- get_fit(sfkeel_mir_2023)

# check warnings
sfkeel_mir_2023_fit[["warnings"]]

# inspect MCMC chain
rstan::traceplot(get_mcmc(sfkeel_mir_2023), pars='GPP_daily', nrow=3)
rstan::traceplot(get_mcmc(sfkeel_mir_2023), pars='ER_daily', nrow=3)
rstan::traceplot(get_mcmc(sfkeel_mir_2023), pars='K600_daily', nrow=3)


# diagnostic plots
plot_binning(sfkeel_mir_2023_fit, sfkeel_mir_2023, 
             title = "South Fork Eel @ Miranda, 2023")
plot_metab_preds(predict_metab(sfkeel_mir_2023))
plot_DO_preds(predict_DO(sfkeel_mir_2023))
plot_ER_K600(sfkeel_mir_2023_fit, title = "South Fork Eel @ Miranda, 2023")
plot_K600(sfkeel_mir_2023_fit, title = "South Fork Eel @ Miranda, 2023")

# save files
write_files(sfkeel_mir_2023_fit, sfkeel_mir_2023, "/sfkeel_mir_2023/week_test/20240710/",
            "sfkeel_mir_2023")

# remove rds
rm(sfkeel_mir_2023)
sfkeel_mir_2023 <- readRDS("sfkeel_mir_2023/week_test/20240710/sfkeel_mir_2023metab_obj.rds")

### trial with russian data
russubset <- inputs_prepped$russian_2022 %>% 
  dplyr::filter(solar.time >= "2022-07-15 00:00:00" & solar.time <= "2022-07-23 00:00:00")

# visualize inputs
visualize_inputs(russubset)

# specs
bayes_name_russian_2022 <- mm_name(type='bayes', pool_K600="binned",
                                      err_obs_iid=TRUE, err_proc_iid = TRUE,
                                      ode_method = "trapezoid", deficit_src='DO_mod', engine='stan')
bayes_specs_russian_2022 <- specs(bayes_name_russian_2022, burnin_steps = 1000, saved_steps = 5000,
                                     thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)# trying to go for more iterations?

# trial
russian_2022 <- metab(bayes_specs_russian_2022, data = russubset)
russian_2022_fit <- get_fit(russian_2022)

# check warnings
russian_2022_fit[["warnings"]]

# diagnostic plots
plot_binning(russian_2022_fit, russian_2022, 
             title = "Russian River, 2022")
plot_metab_preds(predict_metab(russian_2022))
plot_DO_preds(predict_DO(russian_2022))
plot_ER_K600(russian_2022_fit, title = "Russian River, 2022")
plot_K600(russian_2022_fit, title = "Russian River, 2022")

# save files
write_files(russian_2022_fit, russian_2022, "/russian_2022/week_test/20240710/",
            "russian_2022")

# cor test
cor.test(russian_2022_fit$daily$K600_daily_mean, russian_2022_fit$daily$ER_daily_mean)
# less correlated but still correlated

### test run of MLE version for south fork eel 2022
bayes_name_sfkeel_mir_2022_MLE <- mm_name(type='mle', pool_K600="binned",
                                   err_obs_iid=TRUE, err_proc_iid = TRUE,
                                   ode_method = "trapezoid", deficit_src='DO_mod', engine='stan') # CHANGE ENGINE
bayes_specs_sfkeel_mir_2022_MLE<- specs(bayes_name_russian_2022, burnin_steps = 1000, saved_steps = 5000,
                                  thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)# trying to go for more iterations?

sfkeel_mir_2022_fit[["warnings"]]

# MLE for sfkeel mir 2023
# subset after running above code
subset2023 <- inputs_prepped$sfkeel_mir_2023 %>% 
  dplyr::filter(solar.time >= "2023-07-15 00:00:00" & solar.time <= "2023-07-23 00:00:00")

# visualize inputs
visualize_inputs(subset2023)

# specs
name_MLE_sfkeel_2023 <- mm_name(type='mle', ode_method = "trapezoid", deficit_src='DO_mod', engine='nlm')
specs_MLE_sfkeel_2023 <- specs(name_MLE_sfkeel_2023) 
?streamMetabolizer::specs

sfkeel_2023_mle <- metab(specs_MLE_sfkeel_2023, data = subset2023)
sfkeel_2023_mle_fit <- get_fit(sfkeel_2023_mle)

view(sfkeel_2023_mle_fit$K600.daily)


plot_DO_preds(predict_DO(sfkeel_2023_mle))

## night for sfkeel mir 2023
name_night_sfkeel_2023 <- mm_name(type = "night", ode_method = "euler")
specs_night_sfkeel_2023 <- specs(name_night_sfkeel_2023) 

sfkeel_2023_night <- metab(specs_night_sfkeel_2023, data = subset2023)
sfkeel_2023_night_fit <- get_fit(sfkeel_2023_night)


plot_DO_preds(predict_DO(sfkeel_2023_night))

### standish hickey full run
visualize_inputs(inputs_prepped$sfkeel_sth_2023)

# setting bayesian model specifications
bayes_name_sfkeel_sth_2023 <- mm_name(type='bayes', pool_K600="binned",
                                      err_obs_iid=TRUE, err_proc_iid = TRUE,
                                      ode_method = "trapezoid", deficit_src='DO_mod', engine='stan')
bayes_specs_sfkeel_sth_2023 <- specs(bayes_name_sfkeel_sth_2023, burnin_steps = 1000, saved_steps = 5000,
                                     thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10) # trying to go for more iterations?

# changing to fit range of log(Q) for site
bayes_specs_sfkeel_sth_2023$K600_lnQ_nodes_centers <- c(-1.5, -1, -.5, 0, .5, 1, 1.5)
bayes_specs_sfkeel_sth_2023$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5 

# trial
sfkeel_sth_2023 <- metab(bayes_specs_sfkeel_sth_2023, data = inputs_prepped$sfkeel_sth_2023)
sfkeel_sth_2023_fit <- get_fit(sfkeel_sth_2023)

# check warnings
sfkeel_sth_2023_fit[["warnings"]]

# inspect MCMC chain
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='GPP_daily', nrow=3)
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='ER_daily', nrow=3)
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='K600_daily', nrow=3)

# check rhat for convergence
sfkeel_sth_2023_fit$overall %>%
  dplyr::select(ends_with('Rhat'))

# save files
write_files(sfkeel_sth_2023_fit, sfkeel_sth_2023, "/sfkeel_sth_2023/20240716/", 
            "sfkeel_sth_2023")

# visualizing outputs
plot_binning(sfkeel_sth_2023_fit, sfkeel_sth_2023, 
             title = "South Fork Eel @ Miranda, 2022")
plot_metab_preds(predict_metab(sfkeel_sth_2023))
plot_DO_preds(predict_DO(sfkeel_sth_2023, date_start = "2023-08-01", date_end = "2023-08-04"))
plot_ER_K600(sfkeel_sth_2023_fit, title = "South Fork Eel @ Standish Hickey, 2023")
plot_K600(sfkeel_sth_2023_fit, title = "South Fork Eel @ Standish Hickey, 2023")

# K600 and ER correlation test
cor.test(sfkeel_sth_2023_fit$daily$K600_daily_mean, sfkeel_sth_2023_fit$daily$ER_daily_mean)

# remove large .rmd before starting next model (to avoid R crashing)
rm(sfkeel_mir_2022)
