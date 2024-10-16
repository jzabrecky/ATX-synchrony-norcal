#### modeling metabolism and functions to assess model outputs
### Jordan Zabrecky
## last edited 09.25.2024

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

# goodness of fit metric calculations
calc_gof_metrics <- function(metab, subfolder, SiteID) {
  data <- get_data(metab)
  data <- na.omit(data) # do not calculate when data missing
  
  #RMSE
  rmse <- sqrt(sum((data$DO.obs-data$DO.mod)^2)/length(data$DO.mod))
  
  #NRMSE
  nrmse <- rmse/(max(data$DO.obs)-min(data$DO.obs))
  
  # save metrics and print them to console
  metrics <- as.data.frame(cbind(rmse, nrmse))
  write.csv(metrics, paste(".", subfolder, SiteID, "_metrics.csv", sep=""),
            row.names = FALSE)
  return(metrics)
}

#### (4) Running streamMetabolizer for all sites and visualizing outputs ####

# set working directory
setwd("data/metab_model_outputs")

# our base model name
bayesian_mm <- mm_name(type = "bayes", pool_K600 = "binned", err_obs_iid = TRUE, 
                       err_proc_iid = TRUE, ode_method = "trapezoid", deficit_src = "DO_mod",
                       engine = "stan")

## russian river 2022

# visualize inputs
visualize_inputs(inputs_prepped$russian_2022)
visualize_inputs(inputs_prepped$russian_2022_USGS)

# model specs
russian_2022_specs <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                               thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
russian_2022_specs$K600_lnQ_nodes_centers <- c(-0.25, 0, 0.25, 0.5, 0.75, 1.0, 1.25)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
russian_2022_specs$K600_lnQ_nodediffs_sdlog <- 0.25 / 2

# running model
russian_2022 <- metab(russian_2022_specs, data = inputs_prepped$russian_2022)

# get fit and save files
sfkeel_sth_2023_fit <- get_fit(sfkeel_sth_2023)
write_files(sfkeel_sth_2023_fit, sfkeel_sth_2023, "/sfkeel_sth_2023/",
            "sfkeel_sth_2023")

# model diagnostics stuff


#### OLD BELOW- will delete

## south fork eel @ standish hickey 2023

# visualize inputs
visualize_inputs(inputs_prepped$sfkeel_sth_2023)

## this model has tightened binning and is being run for more iterations than the previous
sfkeel_sth_2023_specs <- specs(bayesian_mm, burnin_steps = 8000, saved_steps = 4000,
                               thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_sth_2023_specs$K600_lnQ_nodes_centers <- c(-0.25, 0, 0.25, 0.5, 0.75, 1.0, 1.25)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
sfkeel_sth_2023_specs$K600_lnQ_nodediffs_sdlog <- 0.25 / 2

# running model
sfkeel_sth_2023 <- metab(sfkeel_sth_2023_specs, data = inputs_prepped$sfkeel_sth_2023)

# get fit and save files
sfkeel_sth_2023_fit <- get_fit(sfkeel_sth_2023)
write_files(sfkeel_sth_2023_fit, sfkeel_sth_2023, "/sfkeel_sth_2023/",
            "sfkeel_sth_2023")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_sth_2023) # investigate GPP dips
ggplot(sfkeel_sth_2023_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-06-24", date_end = "2023-07-01")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-01", date_end = "2023-07-09")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-09", date_end = "2023-07-17")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-17", date_end = "2023-07-31")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-31", date_end = "2023-08-08")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-08", date_end = "2023-08-14")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-14", date_end = "2023-08-22")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-22", date_end = "2023-08-30")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-30", date_end = "2023-09-08")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-09-08", date_end = "2023-09-16")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-09-16", date_end = "2023-09-21")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-09-21", date_end = "2023-09-27")
plot_binning(sfkeel_sth_2023_fit, sfkeel_sth_2023, "SFE @ Standish Hickey 2023")
plot_ER_K600(sfkeel_sth_2023_fit, "SFE @ Standish Hickey 2023")
cor.test(sfkeel_sth_2023_fit$daily$ER_mean, sfkeel_sth_2023_fit$daily$K600_daily_mean) # not correlated -.237, p = 0.057
plot_K600(sfkeel_sth_2023_fit, "SFE @ Standish Hickey 2023")

# convergence assessment
sfkeel_sth_2023_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_sth_2023_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='K600_daily', nrow=10)

# goodness of fit metrics
calc_gof_metrics(sfkeel_sth_2023, "/sfkeel_sth_2023/", "sfkeel_sth_2023")
