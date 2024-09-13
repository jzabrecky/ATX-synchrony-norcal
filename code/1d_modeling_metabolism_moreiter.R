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

# our base model name
bayesian_mm <- mm_name(type = "bayes", pool_K600 = "binned", err_obs_iid = TRUE, 
                       err_proc_iid = TRUE, ode_method = "trapezoid", deficit_src = "DO_mod",
                       engine = "stan")

## south fork eel @ miranda 2022 (09-11-2024)
visualize_inputs(inputs_prepped$sfkeel_mir_2022)

## this model has tightened binning and is being run for more iterations than the previous
sfkeel_mir_2022_specs <- specs(bayesian_mm, burnin_steps = 20000, saved_steps = 5000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_2022_specs$K600_lnQ_nodes_centers <- c(-1.2, -0.75, -0.3, 0.15, 0.6, 1.05, 1.5)
sfkeel_mir_2022_specs$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2022 <- metab(sfkeel_mir_2022_specs, data = inputs_prepped$sfkeel_mir_2022)

# get fit and save files
sfkeel_mir_2022_fit <- get_fit(sfkeel_mir_2022)
write_files(sfkeel_mir_2022_fit, sfkeel_mir_2022, "/sfkeel_mir_2022/20240911/",
            "sfkeel_mir_2022")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2022)
ggplot(sfkeel_mir_2022_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-06-29", date_end = "2022-07-08")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-08", date_end = "2022-07-18")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-18", date_end = "2022-07-28")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-28", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-06", date_end = "2022-08-16")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-16", date_end = "2022-08-26")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-26", date_end = "2022-09-03")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-09-03", date_end = "2022-09-10")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-09-10", date_end = "2022-09-17")
plot_binning(sfkeel_mir_2022_fit, sfkeel_mir_2022, "SFE @ Miranda 2022 (20,000 warmup)")
plot_ER_K600(sfkeel_mir_2022_fit, "SFE @ Miranda 2022 (20,000 warmup)")
cor.test(sfkeel_mir_2022_fit$daily$ER_mean, sfkeel_mir_2022_fit$daily$K600_daily_mean) # correlated; 0.6131324
plot_K600(sfkeel_mir_2022_fit, "SFE @ Miranda 2022 (20,000 warmup)")

# convergence assessment
sfkeel_mir_2022_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2022_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_mir_2022)

### test 2 w/ sfkeel-mir_2022
# changing binning back to the original (a bit wider), but keeping the iterations

## this model has tightened binning and is being run for more iterations than the previous
sfkeel_mir_2022_specs_2 <- specs(bayesian_mm, burnin_steps = 20000, saved_steps = 5000,
                               thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site
sfkeel_mir_2022_specs_2$K600_lnQ_nodes_centers <- c(-1.3, -.8, -.3, .2, .7, 1.2, 1.8)
sfkeel_mir_2022_specs_2$K600_lnQ_nodediffs_sdlog <- 0.5 / 2  # need to change for centers .5

# running model
sfkeel_mir_2022_2 <- metab(sfkeel_mir_2022_specs_2, data = inputs_prepped$sfkeel_mir_2022)

# get fit and save files
sfkeel_mir_2022_fit <- get_fit(sfkeel_mir_2022)
write_files(sfkeel_mir_2022_fit, sfkeel_mir_2022, "/sfkeel_mir_2022/20240911/",
            "sfkeel_mir_2022")

# model evaluation- DO preds, ER, GPP, K600 estimates
plot_metab_preds(sfkeel_mir_2022)
ggplot(sfkeel_mir_2022_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-06-29", date_end = "2022-07-08")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-08", date_end = "2022-07-18")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-18", date_end = "2022-07-28")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-28", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-06", date_end = "2022-08-16")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-16", date_end = "2022-08-26")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-26", date_end = "2022-09-03")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-09-03", date_end = "2022-09-10")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-09-10", date_end = "2022-09-17")
plot_binning(sfkeel_mir_2022_fit, sfkeel_mir_2022, "SFE @ Miranda 2022 (20,000 warmup)")
plot_ER_K600(sfkeel_mir_2022_fit, "SFE @ Miranda 2022 (20,000 warmup)")
cor.test(sfkeel_mir_2022_fit$daily$ER_mean, sfkeel_mir_2022_fit$daily$K600_daily_mean) # correlated; 0.6131324
plot_K600(sfkeel_mir_2022_fit, "SFE @ Miranda 2022 (20,000 warmup)")

# convergence assessment
sfkeel_mir_2022_fit$overall %>% # get r-hat
  dplyr::select(ends_with('Rhat'))
sfkeel_mir_2022_fit[["warnings"]]
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='GPP_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='K600_daily', nrow=10)

# remove RDS object before running next model!
rm(sfkeel_mir_2022)