#### modeling metabolism and functions to assess model outputs
### Jordan Zabrecky
## last edited 07.04.2024

# This code models metabolism using the "streamMetabolizer" package and also
# provides functions for visualizing inputs & outputs, and saving outputs

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
           light = convert_SW_to_PAR(SW_W_m_2),
           discharge = discharge_m3_s) %>% 
    dplyr::select(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge)
  return(new_df)
}

# apply function to dataframes
inputs_prepped <- lapply(inputs_list, function(x) metab_prep(x))

#### (3) Functions to visualize outputs ####

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
    write.csv(data_fit[[i]], filename, row.names = FALSE)
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

#### (4) Running streamMetabolizer for all sites and visualizing outputs ####

# set working directory
setwd("data/metab_model_outputs")

# two model names for now...
bayesian_mm <- mm_name(type = "bayes", pool_K600 = "binned", err_obs_iid = TRUE, 
                             err_proc_iid = TRUE, ode_method = "trapezoid", deficit_src = "DO_mod",
                             engine = "stan")
gpp_proc_error_mm <- mm_name(type = "bayes", pool_K600 = "binned", err_proc_GPP = TRUE, 
                            err_proc_iid = TRUE, ode_method = "trapezoid", deficit_src = "DO_mod",
                            engine = "stan")

## south fork eel @ miranda 2022

# to be done!

## russian 2022

visualize_inputs(inputs_prepped$russian_2022)

# test 1 -- iid process error

# setting model specs
rus_specs <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                   thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
rus_specs$K600_lnQ_nodes_centers <- c(-.5, 0, .5, 1, 1.5, 2, 2.5)
rus_specs$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5 

# running model
russian_2022_1 <- metab(rus_specs, data = inputs_prepped$russian_2022)

# get fit and save files
russian_2022_1_fit <- get_fit(russian_2022_1)
write_files(russian_2022_1_fit, russian_2022_1, "/russian_2022/20240828/",
            "russian_2022_iid_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(russian_2022_1) # dip on august 1st to investigate; otherwise looks good!
plot_DO_preds(russian_2022_1, date_start = "2022-06-24", date_end = "2022-07-03")
plot_DO_preds(russian_2022_1, date_start = "2022-07-03", date_end = "2022-07-13")
plot_DO_preds(russian_2022_1, date_start = "2022-07-13", date_end = "2022-07-23")
plot_DO_preds(russian_2022_1, date_start = "2022-07-23", date_end = "2022-08-02")
plot_DO_preds(russian_2022_1, date_start = "2022-08-02", date_end = "2022-08-12")
plot_DO_preds(russian_2022_1, date_start = "2022-08-12", date_end = "2022-08-22") # bad period here- investigate biofouling?
plot_DO_preds(russian_2022_1, date_start = "2022-08-22", date_end = "2022-09-02") # bad period here as well- investigate biofouling?
plot_binning(russian_2022_1_fit, russian_2022_1, "Russian 2022 (iid_proc_error)")
plot_ER_K600(russian_2022_1_fit, "Russian 2022 (iid_proc_error)")
plot_K600(russian_2022_1_fit, "Russian 2022 (iid_proc_error)")

# convergence assessment
russian_2022_1_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
russian_2022_1_fit[["warnings"]]
rstan::traceplot(get_mcmc(russian_2022_1), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(russian_2022_1), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(russian_2022_1), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(russian_2022_1)

# test 2 -- GPP proc error

# setting model specs
rus_specs_2 <- specs(gpp_proc_error_mm, burnin_steps = 5000, saved_steps = 5000,
                   thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
rus_specs_2$K600_lnQ_nodes_centers <- c(-.5, -.25, 0, .25, .5, .75, 1)
rus_specs_2$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
russian_2022_2 <- metab(rus_specs_2, data = inputs_prepped$russian_2022)

# get fit and save files
russian_2022_2_fit <- get_fit(russian_2022_2)
write_files(russian_2022_2_fit, russian_2022_2, "/russian_2022/20240830/",
            "russian_2022_gpp_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(russian_2022_2)
ggplot(russian_2022_2_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-06")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-20")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-02")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-17")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(russian_2022_2, date_start = "2022-06-24", date_end = "2022-07-03")
plot_DO_preds(russian_2022_2, date_start = "2022-07-03", date_end = "2022-07-13")
plot_DO_preds(russian_2022_2, date_start = "2022-07-13", date_end = "2022-07-23")
plot_DO_preds(russian_2022_2, date_start = "2022-07-23", date_end = "2022-08-02")
plot_DO_preds(russian_2022_2, date_start = "2022-08-02", date_end = "2022-08-12")
plot_DO_preds(russian_2022_2, date_start = "2022-08-12", date_end = "2022-08-22")
plot_DO_preds(russian_2022_2, date_start = "2022-08-22", date_end = "2022-09-02")
plot_binning(russian_2022_2_fit, russian_2022_2, "Russian 2022 (GPP_proc_error)")
plot_ER_K600(russian_2022_2_fit, "Russian 2022 (GPP_proc_error)")
plot_K600(russian_2022_2_fit, "Russian 2022 (GPP_proc_error)")

# convergence assessment
russian_2022_2_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
russian_2022_2_fit[["warnings"]]
rstan::traceplot(get_mcmc(russian_2022_2), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(russian_2022_2), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(russian_2022_2), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(russian_2022_2)

## south fork eel @ miranda 2022

visualize_inputs(inputs_prepped$sfkeel_mir_2022)

# test 1 -- iid process error

sfkeel_mir_specs_1 <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_specs_1$K600_lnQ_nodes_centers <- c(-1.3, -.8, -.3, .2, .7, 1.2, 1.8)
sfkeel_mir_specs_1$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2022_1 <- metab(sfkeel_mir_specs_1, data = inputs_prepped$sfkeel_mir_2022)

# get fit and save files
sfkeel_mir_2022_1_fit <- get_fit(sfkeel_mir_2022_1)
write_files(sfkeel_mir_2022_1_fit, sfkeel_mir_2022_1, "/sfkeel_mir_2022/20240830/",
            "sfkeel_mir_2022_iid_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2022_1)
ggplot(sfkeel_mir_2022_1_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-23")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-09-06")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-06-29", date_end = "2022-07-08")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-07-08", date_end = "2022-07-18")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-07-18", date_end = "2022-07-28")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-07-28", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-08-06", date_end = "2022-08-16")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-08-16", date_end = "2022-08-26")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-08-26", date_end = "2022-09-03")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-09-03", date_end = "2022-09-10")
plot_DO_preds(sfkeel_mir_2022_1, date_start = "2022-09-10", date_end = "2022-09-17")
plot_binning(sfkeel_mir_2022_1_fit, sfkeel_mir_2022_1, "SFE @ Miranda 2022 (IID_proc_error)")
plot_ER_K600(sfkeel_mir_2022_1_fit, "SFE @ Miranda 2022 (IID_proc_error)")
plot_K600(sfkeel_mir_2022_1_fit, "SFE @ Miranda 2022 (IID_proc_error)")

# convergence assessment
sfkeel_mir_2022_1_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2022_1_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2022_1), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_1), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_1), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_mir_2022_1)

# test 2 -- GPP process error

sfkeel_mir_specs_2 <- specs(gpp_proc_error_mm, burnin_steps = 5000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_specs_2$K600_lnQ_nodes_centers <- c(-1.3, -.8, -.3, .2, .7, 1.2, 1.8)
sfkeel_mir_specs_2$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2022_2 <- metab(sfkeel_mir_specs_2, data = inputs_prepped$sfkeel_mir_2022)

# get fit and save files
sfkeel_mir_2022_2_fit <- get_fit(sfkeel_mir_2022_2)
write_files(sfkeel_mir_2022_2_fit, sfkeel_mir_2022_2, "/sfkeel_mir_2022/20240831/",
            "sfkeel_mir_2022_gpp_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2022_2) # weird high date
ggplot(sfkeel_mir_2022_2_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-23")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-09-06")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-06-29", date_end = "2022-07-08")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-07-08", date_end = "2022-07-18")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-07-18", date_end = "2022-07-28")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-07-28", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-08-06", date_end = "2022-08-16")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-08-16", date_end = "2022-08-26")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-08-26", date_end = "2022-09-03")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-09-03", date_end = "2022-09-10")
plot_DO_preds(sfkeel_mir_2022_2, date_start = "2022-09-10", date_end = "2022-09-17")
plot_binning(sfkeel_mir_2022_2_fit, sfkeel_mir_2022_2, "SFE @ Miranda 2022 (GPP_proc_error)")
plot_ER_K600(sfkeel_mir_2022_2_fit, "SFE @ Miranda 2022 (GPP_proc_error)")
plot_K600(sfkeel_mir_2022_2_fit, "SFE @ Miranda 2022 (GPP_proc_error)")

# convergence assessment
sfkeel_mir_2022_2_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2022_2_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2022_2), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_2), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_2), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_mir_2022_2)

## salmon 2022
# hold off till obtaining external data due to biofouling

## south fork eel @ miranda 2023

# test 1 -- iid process error
sfkeel_mir_specs_1 <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_specs_1$K600_lnQ_nodes_centers <- c(-1.3, -.8, -.3, .2, .7, 1.2, 1.8)
sfkeel_mir_specs_1$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2023_1 <- metab(sfkeel_mir_specs_1, data = inputs_prepped$sfkeel_mir_2023)

# get fit and save files
sfkeel_mir_2023_1_fit <- get_fit(sfkeel_mir_2023_1)
write_files(sfkeel_mir_2023_1_fit, sfkeel_mir_2023_1, "/sfkeel_mir_2023/20240901/",
            "sfkeel_mir_2023_iid_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2023_1) # investigate GPP dips
ggplot(sfkeel_mir_2023_1_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2023-06-25")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-03")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-17")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-31")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-07")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-22")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-04")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-18")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-06-18", date_end = "2023-06-28")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-06-28", date_end = "2023-07-07")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-07-07", date_end = "2023-07-17")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-07-17", date_end = "2023-07-27")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-07-27", date_end = "2023-08-05")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-08-05", date_end = "2023-08-15")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-08-15", date_end = "2023-08-25")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-08-25", date_end = "2023-09-03")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-09-03", date_end = "2023-09-13")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-09-13", date_end = "2023-09-23")
plot_DO_preds(sfkeel_mir_2023_1, date_start = "2023-09-23", date_end = "2023-09-27")
plot_binning(sfkeel_mir_2023_1_fit, sfkeel_mir_2023_1, "SFE @ Miranda 2023 (IID_proc_error)")
plot_ER_K600(sfkeel_mir_2023_1_fit, "SFE @ Miranda 2023 (IID_proc_error)")
plot_K600(sfkeel_mir_2023_1_fit, "SFE @ Miranda 2023 (IID_proc_error)")

# convergence assessment
sfkeel_mir_2023_1_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2023_1_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2023_1), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2023_1), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2023_1), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_mir_2023_1)

# test 2 -- GPP process error

sfkeel_mir_specs_2 <- specs(gpp_proc_error_mm, burnin_steps = 5000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_specs_2$K600_lnQ_nodes_centers <- c(-1.3, -.8, -.3, .2, .7, 1.2, 1.8)
sfkeel_mir_specs_2$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2023_2 <- metab(sfkeel_mir_specs_2, data = inputs_prepped$sfkeel_mir_2023)

# get fit and save files
sfkeel_mir_2023_2_fit <- get_fit(sfkeel_mir_2023_2)
write_files(sfkeel_mir_2023_2_fit, sfkeel_mir_2023_2, "/sfkeel_mir_2023/20240903/",
            "sfkeel_mir_2023_gpp_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2023_2) # investigate GPP dips
ggplot(sfkeel_mir_2023_2_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2023-06-25")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-03")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-17")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-31")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-07")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-22")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-04")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-18")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-06-18", date_end = "2023-06-28")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-06-28", date_end = "2023-07-07")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-07-07", date_end = "2023-07-17")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-07-17", date_end = "2023-07-27")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-07-27", date_end = "2023-08-05")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-08-05", date_end = "2023-08-15")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-08-15", date_end = "2023-08-25")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-08-25", date_end = "2023-09-03")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-09-03", date_end = "2023-09-13")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-09-13", date_end = "2023-09-23")
plot_DO_preds(sfkeel_mir_2023_2, date_start = "2023-09-23", date_end = "2023-09-27")
plot_binning(sfkeel_mir_2023_2_fit, sfkeel_mir_2023_2, "SFE @ Miranda 2023 (GPP_proc_error)")
plot_ER_K600(sfkeel_mir_2023_2_fit, "SFE @ Miranda 2023 (GPP_proc_error)")
plot_K600(sfkeel_mir_2023_2_fit, "SFE @ Miranda 2023 (GPP_proc_error)")

# convergence assessment
sfkeel_mir_2023_2_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2023_2_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2023_2), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2023_2), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2023_2), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_mir_2023_2)

## south fork eel @ standish hickey 2023

# test 1 -- iid process error
sfkeel_sth_specs_1 <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_sth_specs_1$K600_lnQ_nodes_centers <- c(-1.2, -.8, -.4, 0, .4, .8, 1.2)
sfkeel_sth_specs_1$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_sth_2023_1 <- metab(sfkeel_sth_specs_1, data = inputs_prepped$sfkeel_sth_2023)

# get fit and save files
sfkeel_sth_2023_1_fit <- get_fit(sfkeel_sth_2023_1)
write_files(sfkeel_sth_2023_1_fit, sfkeel_sth_2023_1, "/sfkeel_sth_2023/20240904/",
            "sfkeel_sth_2023_iid_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_sth_2023_1) # investigate GPP dips
ggplot(sfkeel_sth_2023_1_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2023-07-03")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-11")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-17")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-31")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-07")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-22")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-04")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-18")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-06-24", date_end = "2023-07-01")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-07-01", date_end = "2023-07-09")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-07-09", date_end = "2023-07-17")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-07-17", date_end = "2023-07-31")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-07-31", date_end = "2023-08-08")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-08-08", date_end = "2023-08-14")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-08-14", date_end = "2023-08-22")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-08-22", date_end = "2023-08-30")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-08-30", date_end = "2023-09-08")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-09-08", date_end = "2023-09-16")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-09-16", date_end = "2023-09-21")
plot_DO_preds(sfkeel_sth_2023_1, date_start = "2023-09-21", date_end = "2023-09-27")
plot_binning(sfkeel_sth_2023_1_fit, sfkeel_sth_2023_1, "SFE @ Standish Hickey 2023 (IID_proc_error)")
plot_ER_K600(sfkeel_sth_2023_1_fit, "SFE @ Standish Hickey 2023 (IID_proc_error)")
plot_K600(sfkeel_sth_2023_1_fit, "SFE @ Standish Hickey 2023 (IID_proc_error)")

# convergence assessment
sfkeel_sth_2023_1_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_sth_2023_1_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_sth_2023_1), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_sth_2023_1), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_sth_2023_1), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_sth_2023_1)

# test 1 -- gpp process error
sfkeel_sth_specs_2 <- specs(gpp_proc_error_mm, burnin_steps = 5000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_sth_specs_2$K600_lnQ_nodes_centers <- c(-1.2, -.8, -.4, 0, .4, .8, 1.2)
sfkeel_sth_specs_2$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_sth_2023_2 <- metab(sfkeel_sth_specs_2, data = inputs_prepped$sfkeel_sth_2023)

# get fit and save files
sfkeel_sth_2023_2_fit <- get_fit(sfkeel_sth_2023_2)
write_files(sfkeel_sth_2023_2_fit, sfkeel_sth_2023_2, "/sfkeel_sth_2023/20240905/",
            "sfkeel_sth_2023_gpp_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_sth_2023_2) # okay pattern a bit more in line here; but GPP estimate is CRAZY high
ggplot(sfkeel_sth_2023_2_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2023-07-03")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-11")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-17")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-31")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-07")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-22")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-04")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-18")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-09-24")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-06-24", date_end = "2023-07-01")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-07-01", date_end = "2023-07-09")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-07-09", date_end = "2023-07-17")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-07-17", date_end = "2023-07-31")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-07-31", date_end = "2023-08-08")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-08-08", date_end = "2023-08-14")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-08-14", date_end = "2023-08-22")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-08-22", date_end = "2023-08-30")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-08-30", date_end = "2023-09-08")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-09-08", date_end = "2023-09-16")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-09-16", date_end = "2023-09-21")
plot_DO_preds(sfkeel_sth_2023_2, date_start = "2023-09-21", date_end = "2023-09-27")
plot_binning(sfkeel_sth_2023_2_fit, sfkeel_sth_2023_2, "SFE @ Standish Hickey 2023 (GPP_proc_error)")
plot_ER_K600(sfkeel_sth_2023_2_fit, "SFE @ Standish Hickey 2023 (GPP_proc_error)")
plot_K600(sfkeel_sth_2023_2_fit, "SFE @ Standish Hickey 2023 (GPP_proc_error)")

# convergence assessment
sfkeel_sth_2023_2_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_sth_2023_2_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_sth_2023_2), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_sth_2023_2), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_sth_2023_2), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_sth_2023_2)

### (second) final initial test- trying sfkeel-mir 2022 but with 10000 warmup and 5000 iter after

sfkeel_mir_specs_3 <- specs(bayesian_mm, burnin_steps = 10000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_specs_3$K600_lnQ_nodes_centers <- c(-1.3, -.8, -.3, .2, .7, 1.2, 1.8) # this is for range of 2023 so can tighten...
sfkeel_mir_specs_3$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2022_3 <- metab(sfkeel_mir_specs_3, data = inputs_prepped$sfkeel_mir_2022)

# get fit and save files
sfkeel_mir_2022_3_fit <- get_fit(sfkeel_mir_2022_3)
write_files(sfkeel_mir_2022_3_fit, sfkeel_mir_2022_3, "/sfkeel_mir_2022/20240906/",
            "sfkeel_mir_2022_iid_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2022_3) # weird high date
ggplot(sfkeel_mir_2022_3_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-23")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-09-06")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-06-29", date_end = "2022-07-08")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-07-08", date_end = "2022-07-18")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-07-18", date_end = "2022-07-28")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-07-28", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-08-06", date_end = "2022-08-16")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-08-16", date_end = "2022-08-26")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-08-26", date_end = "2022-09-03")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-09-03", date_end = "2022-09-10")
plot_DO_preds(sfkeel_mir_2022_3, date_start = "2022-09-10", date_end = "2022-09-17")
plot_binning(sfkeel_mir_2022_3_fit, sfkeel_mir_2022_3, "SFE @ Miranda 2022 (IID_proc_error)")
plot_ER_K600(sfkeel_mir_2022_3_fit, "SFE @ Miranda 2022 (IID_proc_error)")
plot_K600(sfkeel_mir_2022_3_fit, "SFE @ Miranda 2022 (IID_proc_error)")

# convergence assessment
sfkeel_mir_2022_3_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2022_3_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2022_3), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_3), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_3), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_mir_2022_3)

### trial 4 with 15000 warmup

sfkeel_mir_specs_4 <- specs(bayesian_mm, burnin_steps = 15000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_specs_4$K600_lnQ_nodes_centers <- c(-1.3, -.8, -.3, .2, .7, 1.2, 1.8) # this is for range of 2023 so can tighten...
sfkeel_mir_specs_4$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2022_4 <- metab(sfkeel_mir_specs_4, data = inputs_prepped$sfkeel_mir_2022)

# get fit and save files
sfkeel_mir_2022_4_fit <- get_fit(sfkeel_mir_2022_4)
write_files(sfkeel_mir_2022_4_fit, sfkeel_mir_2022_4, "/sfkeel_mir_2022/20240909/",
            "sfkeel_mir_2022_iid_proc_error")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2022_4) # weird high date
ggplot(sfkeel_mir_2022_4_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-14")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-28")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-08-23")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-09-06")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-06-29", date_end = "2022-07-08")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-07-08", date_end = "2022-07-18")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-07-18", date_end = "2022-07-28")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-07-28", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-08-06", date_end = "2022-08-16")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-08-16", date_end = "2022-08-26")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-08-26", date_end = "2022-09-03")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-09-03", date_end = "2022-09-10")
plot_DO_preds(sfkeel_mir_2022_4, date_start = "2022-09-10", date_end = "2022-09-17")
plot_binning(sfkeel_mir_2022_4_fit, sfkeel_mir_2022_4, "SFE @ Miranda 2022 (IID_proc_error)")
plot_ER_K600(sfkeel_mir_2022_4_fit, "SFE @ Miranda 2022 (IID_proc_error)")
plot_K600(sfkeel_mir_2022_4_fit, "SFE @ Miranda 2022 (IID_proc_error)")

# convergence assessment
sfkeel_mir_2022_4_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2022_4_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2022_4), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_4), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022_4), pars='K600_daily', nrow=10)

## salmon 2023
# hold off till obtaining external data due to biofouling