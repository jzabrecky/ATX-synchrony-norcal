#### modeling metabolism and functions to assess model outputs
### Jordan Zabrecky
## last edited 02.24.2025

# This code models metabolism using the "streamMetabolizer" package and also
# provides functions for visualizing inputs & outputs, and saving outputs

#### (1) Loading packages and reading in data #### 

## Loading libraries
# if "rstan" an "StanHeaders" is not downloaded, see documentation here for downloading:
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

# if "streamMetabolizer" is not downloaded, see documentation here for downloading:
# https://github.com/DOI-USGS/streamMetabolizer

lapply(c("rstan", "StanHeaders", "streamMetabolizer", "tidyverse", "plyr"), 
       require, character.only = T)

## Reading in model input data (from starting working directory)
model_inputs <- ldply(list.files(path = "./data/metab_model_inputs/", pattern = "modelinputs"), function(filename) {
  d <- read.csv(paste("data/metab_model_inputs/", filename, sep = ""))
  return(d)
})

# convert solar.time to POSIXct class
model_inputs$solar.time <- as.POSIXct(model_inputs$solar.time, format = "%Y-%m-%d %H:%M:%S",
                                      tz = "UTC")

# separating into a list based on site/year
inputs_prepped <- split(model_inputs, model_inputs$site_year)

# function to remove site year column
remove_siteyear <- function(df) {
  new_df <- df %>% 
    dplyr::select(!site_year)
  return(new_df)
}

# apply function to list of inputs
inputs_prepped <- lapply(inputs_prepped, function(x) remove_siteyear(x))

# loading k600 prior estimates
k600_priors <- read.csv("./data/metab_model_inputs/k600_estimates.csv")

#### (2) Functions to visualize outputs and calculate gof and bins ####

# function to visualize all inputs on separate plots
visualize_inputs_full <- function(df){
  
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

# function to visualize temperature, solar, and DO on same plot to check for
# timing that makes sense
visualize_inputs_zoomed <- function(df, starttime, endtime) {
  
  # allow for string as parameter and convert string to datetime object
  starttime <- as_datetime(starttime)
  endtime <- as_datetime(endtime)
  
  # make plot
  plot <- ggplot(df, aes(x = solar.time)) +
    geom_line(aes(y = temp.water, color = "temperature C")) +
    geom_line(aes(y = DO.obs * 3, color = "DO mg L^-1")) +
    geom_line(aes(y = light / 70, color = "PAR umol m^-2 s^-1")) + 
    scale_x_datetime(limits = c(starttime, endtime)) +
    scale_color_manual("Metabolism Inputs", 
                       values = c("green", "red", "blue")) + 
    labs(y = "scaled values", x = "solar time") +
    theme_bw()
  
  return(plot)
}

# function to easily save output data after model run
write_files <- function(data_fit, data_metab, subfolder, SiteID){
  for (i in seq_along(data_fit)) {
    filename = paste(".", subfolder, SiteID, names(data_fit)[i], ".csv", sep="")
    write.csv(data_fit[[i]], filename, row.names = FALSE)
  }
  write.csv(unlist(get_specs(data_metab)), paste(".", subfolder, SiteID, "_specs.csv", sep=""),
            row.names = FALSE)
  write.csv(get_data_daily(data_metab), paste(".", subfolder, SiteID, "_daily_Qbins.csv", sep=""),
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
  rmse <- sqrt(sum((data$DO.mod-data$DO.obs)^2)/length(data$DO.mod))
  
  #NRMSE
  nrmse <- rmse/(max(data$DO.obs)-min(data$DO.obs))
  
  # save metrics and print them to console
  metrics <- as.data.frame(cbind(rmse, nrmse))
  write.csv(metrics, paste(".", subfolder, SiteID, "_metrics.csv", sep=""),
            row.names = FALSE)
  return(metrics)
}

# function from Alice Carter to set bins based on discharge range (edited)
set_Q_nodes <- function(specs_poolBinned, discharge, k600_prior, 
                        log_k600_sd, min_bins) {
  
  # get range of discharges
  Qrange = quantile(log(discharge),
                    probs = c(0.1, 0.9), na.rm = T)
  
  # separate into minimum # of bins
  n = min_bins
  
  # this code will create more bins if needed based on discharge range
  delta = (Qrange[2]-Qrange[1])/n
  while(delta > 1){
    n = n + 1
    delta <- (Qrange[2]-Qrange[1])/n
  }
  
  # set number of nodes
  nodes <- seq(Qrange[1], Qrange[2], length.out = n)
  specs_poolBinned$K600_lnQ_nodes_centers <- nodes
  # from github: adjust to be 1/5 of distance between nodes
  specs_poolBinned$K600_lnQ_nodediffs_sdlog <- (nodes[2] - nodes[1]) / 5
  # set k600 prior mean for log-normal (so log k600 prior estimate)
  specs_poolBinned$K600_lnQ_nodes_meanlog <- rep(round(log(k600_prior), 2), n)
  specs_poolBinned$K600_lnQ_nodes_sdlog <- rep(log_k600_sd, n)
  return(specs_poolBinned)
}

# function to plot binning after using function above
plot_Q_bins <- function(discharge, specs_poolBinned) {
  plot(density(log(discharge), na.rm = T))
  abline(v = specs_poolBinned$K600_lnQ_nodes_centers)
}

#### (3) Running streamMetabolizer for all sites and visualizing outputs ####

# set working directory
setwd("data/metab_model_outputs")

# our base model name (note: trapezoid ode method is default)
bayesian_mm <- mm_name(type = "bayes", pool_K600 = "binned", 
                       err_obs_iid = TRUE, err_proc_iid = TRUE,
                       ode_method = "trapezoid", deficit_src = 'DO_mod', 
                       engine = 'stan')

#### RUSSIAN RIVER- our miniDOT data ####

# visualize inputs
visualize_inputs_full(inputs_prepped$russian_2022)
visualize_inputs_zoomed(inputs_prepped$russian_2022, "2022-07-01 00:00:00", "2022-07-03 00:00:00")

# set model specs
specs_russian <- specs(bayesian_mm, burnin_steps = 2000, saved_steps = 1000)

# change binning (using USGS data since it has longer range)
specs_russian <- set_Q_nodes(specs_russian, inputs_prepped$russian_2022_USGS$discharge, 
                             # using default sd; doesn't change k600 & best convergence
                             k600_priors[1,2], log_k600_sd = 0.5, min_bins = 3)
plot_Q_bins(inputs_prepped$russian_2022_USGS$discharge, specs_russian)

# running model
russian_metab <- metab(specs_russian, data = inputs_prepped$russian_2022)

# get fit and save files
russian_fit <- get_fit(russian_metab)
write_files(russian_fit, russian_metab, "/russian/", "russian")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(russian_metab)

# plot GPP estimates w/ sensor cleaning dates-- of course no relationship with our cleaning here!
ggplot(russian_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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

# look at DO predictions vs. data on a 7-day interval to look closely
# having a little bit of issues on the early morning (night to light) / trough
# but otherwise not awful
plot_DO_preds(russian_metab, date_start = "2022-06-24", date_end = "2022-07-01")
plot_DO_preds(russian_metab, date_start = "2022-07-01", date_end = "2022-07-08")
plot_DO_preds(russian_metab, date_start = "2022-07-08", date_end = "2022-07-15")
plot_DO_preds(russian_metab, date_start = "2022-07-15", date_end = "2022-07-22")
plot_DO_preds(russian_metab, date_start = "2022-07-22", date_end = "2022-07-29")
plot_DO_preds(russian_metab, date_start = "2022-07-29", date_end = "2022-08-05") # gets worse in aug
plot_DO_preds(russian_metab, date_start = "2022-08-05", date_end = "2022-08-12")
plot_DO_preds(russian_metab, date_start = "2022-08-12", date_end = "2022-08-19") # not a good week; gets good again after cleaning on 8/17
plot_DO_preds(russian_metab, date_start = "2022-08-19", date_end = "2022-08-26")
plot_DO_preds(russian_metab, date_start = "2022-08-26", date_end = "2022-09-03") # again bad before cleaning- biofouling issue

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(russian_fit, russian_metab, "Russian 2022")
plot_ER_K600(russian_fit, "Russian 2022")
cor.test(russian_fit$daily$ER_mean, russian_fit$daily$K600_daily_mean) # correlated -.596; p << 0.008
plot_K600(russian_fit, "Russian 2022")

# convergence assessments
rstan::traceplot(get_mcmc(russian_metab), pars='GPP_daily', nrow=10) # good
rstan::traceplot(get_mcmc(russian_metab), pars='ER_daily', nrow=10) # good
rstan::traceplot(get_mcmc(russian_metab), pars='K600_daily', nrow=10) # good
mean(na.omit(russian_fit$daily$GPP_Rhat)) # good
mean(na.omit(russian_fit$daily$ER_daily_Rhat)) # good
mean(na.omit(russian_fit$daily$K600_daily_Rhat)) # good

# goodness of fit metrics
calc_gof_metrics(russian_metab, "/russian/", "russian") # RMSE 0.319, nRMSE 0.0737

# remove large model object before starting next run
rm(russian_metab, russian_fit)

#### RUSSIAN RIVER- USGS data ####

# set model specs- USGS data is needing more chains perhaps because of 15-m interval?
specs_russian_USGS <- specs(bayesian_mm, burnin_steps = 4000, saved_steps = 2000)

# change binning
specs_russian_USGS <- set_Q_nodes(specs_russian_USGS, inputs_prepped$russian_2022_USGS$discharge, 
                                  # using default sd; doesn't change k600 & best convergence
                                  k600_priors[1,2], log_k600_sd = 0.5, min_bins = 3)
plot_Q_bins(inputs_prepped$russian_2022_USGS$discharge, specs_russian_USGS)

# running model (using specs set above)
russian_USGS_metab <- metab(specs_russian_USGS, data = inputs_prepped$russian_2022_USGS)

# get fit and save files
russian_USGS_fit <- get_fit(russian_USGS_metab)
write_files(russian_USGS_fit, russian_USGS_metab, "/russian_USGS/", "russian_USGS")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(russian_USGS_metab)

# plot GPP estimates w/ sensor cleaning dates-- of course no relationship with our cleaning here!
ggplot(russian_USGS_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(russian_USGS_metab, date_start = "2022-06-24", date_end = "2022-07-01") # looks pretty good!
plot_DO_preds(russian_USGS_metab, date_start = "2022-07-01", date_end = "2022-07-08")
plot_DO_preds(russian_USGS_metab, date_start = "2022-07-08", date_end = "2022-07-15")
plot_DO_preds(russian_USGS_metab, date_start = "2022-07-15", date_end = "2022-07-22")
plot_DO_preds(russian_USGS_metab, date_start = "2022-07-22", date_end = "2022-07-29")
plot_DO_preds(russian_USGS_metab, date_start = "2022-07-29", date_end = "2022-08-05") # one day that is off
plot_DO_preds(russian_USGS_metab, date_start = "2022-08-05", date_end = "2022-08-12")
plot_DO_preds(russian_USGS_metab, date_start = "2022-08-12", date_end = "2022-08-19")
plot_DO_preds(russian_USGS_metab, date_start = "2022-08-19", date_end = "2022-08-26")
plot_DO_preds(russian_USGS_metab, date_start = "2022-08-26", date_end = "2022-09-03")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(russian_USGS_fit, russian_USGS_metab, "Russian USGS 2022")
plot_ER_K600(russian_USGS_fit, "Russian USGS 2022")
cor.test(russian_USGS_fit$daily$ER_mean, russian_USGS_fit$daily$K600_daily_mean) # -0.057; not correlated p > 0.5768
plot_K600(russian_USGS_fit, "Russian USGS 2022")

# convergence assessments
rstan::traceplot(get_mcmc(russian_USGS_metab), pars='GPP_daily', nrow=10) # great
rstan::traceplot(get_mcmc(russian_USGS_metab), pars='ER_daily', nrow=10) # great
rstan::traceplot(get_mcmc(russian_USGS_metab), pars='K600_daily', nrow=10) # decent
mean(na.omit(russian_USGS_fit$daily$GPP_Rhat)) # good
mean(na.omit(russian_USGS_fit$daily$ER_daily_Rhat)) # good
mean(na.omit(russian_USGS_fit$daily$K600_daily_Rhat)) # good

# goodness of fit metrics
calc_gof_metrics(russian_USGS_metab, "/russian_USGS/", "russian_USGS") # RMSE 0.344, nRMSE 0.0514

# remove large model object before starting next run
rm(russian_USGS_metab, russian_USGS_fit)

#### SALMON RIVER- our miniDOT data ####

# model years together
inputs_prepped$salmon <- rbind(inputs_prepped$salmon_2022, inputs_prepped$salmon_2023)

# visualize inputs
visualize_inputs_full(inputs_prepped$salmon)
visualize_inputs_full(inputs_prepped$salmon_2022)
visualize_inputs_full(inputs_prepped$salmon_2023)
visualize_inputs_zoomed(inputs_prepped$salmon, "2022-07-01 00:00:00", "2022-07-03 00:00:00")
visualize_inputs_zoomed(inputs_prepped$salmon, "2023-07-01 00:00:00", "2023-07-03 00:00:00")

# set model specs
specs_salmon <- specs(bayesian_mm, burnin_steps = 2000, saved_steps = 1000)

# change binning
specs_salmon <- set_Q_nodes(specs_salmon, inputs_prepped$salmon$discharge, 
                            # using default sd; doesn't change k600 & best convergence
                            k600_priors[2,2], log_k600_sd = 0.5, min_bins = 4)
plot_Q_bins(inputs_prepped$salmon$discharge, specs_salmon)

# running model
salmon_metab <- metab(specs_salmon, data = inputs_prepped$salmon)

# get fit and save files
salmon_fit <- get_fit(salmon_metab)
write_files(salmon_fit, salmon_metab, "/salmon/", "salmon")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(salmon_metab)

# plot GPP estimates w/ sensor cleaning dates;
# removed the worst of the biofouling prior to long time period already I guess
ggplot(salmon_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  scale_x_date(limits = as.Date(c("2022-06-15", "2022-09-30"))) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-26")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
ggplot(salmon_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  scale_x_date(limits = as.Date(c("2023-06-15", "2023-09-30"))) +
  geom_vline(xintercept = as_date(c("2023-07-13")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-27")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(salmon_metab, date_start = "2022-06-27", date_end = "2022-07-03")
plot_DO_preds(salmon_metab, date_start = "2022-07-03", date_end = "2022-07-10")
plot_DO_preds(salmon_metab, date_start = "2022-07-10", date_end = "2022-07-17") # interesting flat bottoms
plot_DO_preds(salmon_metab, date_start = "2022-07-17", date_end = "2022-07-24")
plot_DO_preds(salmon_metab, date_start = "2022-07-24", date_end = "2022-07-31")
plot_DO_preds(salmon_metab, date_start = "2022-07-31", date_end = "2022-08-06")
plot_DO_preds(salmon_metab, date_start = "2022-08-06", date_end = "2022-08-13")
plot_DO_preds(salmon_metab, date_start = "2022-08-13", date_end = "2022-08-20")
plot_DO_preds(salmon_metab, date_start = "2022-08-20", date_end = "2022-08-27")
plot_DO_preds(salmon_metab, date_start = "2022-08-27", date_end = "2022-09-04")
plot_DO_preds(salmon_metab, date_start = "2022-09-04", date_end = "2022-09-11")
plot_DO_preds(salmon_metab, date_start = "2022-09-11", date_end = "2022-09-18")
plot_DO_preds(salmon_metab, date_start = "2022-09-18", date_end = "2022-09-22") 
plot_DO_preds(salmon_metab, date_start = "2023-06-28", date_end = "2023-07-05") # connected but weird flat bottom
plot_DO_preds(salmon_metab, date_start = "2023-07-05", date_end = "2023-07-12")
plot_DO_preds(salmon_metab, date_start = "2023-07-12", date_end = "2023-07-19")
plot_DO_preds(salmon_metab, date_start = "2023-07-19", date_end = "2023-07-26")
plot_DO_preds(salmon_metab, date_start = "2023-07-26", date_end = "2023-07-31")
plot_DO_preds(salmon_metab, date_start = "2023-07-31", date_end = "2023-08-07")
plot_DO_preds(salmon_metab, date_start = "2023-08-07", date_end = "2023-08-15")
plot_DO_preds(salmon_metab, date_start = "2023-08-15", date_end = "2023-08-22")
plot_DO_preds(salmon_metab, date_start = "2023-08-22", date_end = "2023-08-29")
plot_DO_preds(salmon_metab, date_start = "2023-08-29", date_end = "2023-09-06")
plot_DO_preds(salmon_metab, date_start = "2023-09-06", date_end = "2023-09-13")
plot_DO_preds(salmon_metab, date_start = "2023-09-13", date_end = "2023-09-20")
plot_DO_preds(salmon_metab, date_start = "2023-09-20", date_end = "2023-09-28")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(salmon_fit, salmon_metab, "Salmon 2022-2023")
plot_ER_K600(salmon_fit, "Salmon 2022-2023")
cor.test(salmon_fit$daily$ER_mean, salmon_fit$daily$K600_daily_mean) # 0.259; correlated p << 0.009
plot_K600(salmon_fit, "Salmon 2022-2023")

# convergence assessments
rstan::traceplot(get_mcmc(salmon_metab), pars='GPP_daily', nrow=10) # great
rstan::traceplot(get_mcmc(salmon_metab), pars='ER_daily', nrow=10) # great
rstan::traceplot(get_mcmc(salmon_metab), pars='K600_daily', nrow=10) # great
mean(na.omit(salmon_fit$daily$GPP_Rhat)) # great
mean(na.omit(salmon_fit$daily$ER_daily_Rhat)) # great
mean(na.omit(salmon_fit$daily$K600_daily_Rhat)) # great

# goodness of fit metrics
calc_gof_metrics(salmon_metab, "/salmon/", "salmon") # RMSE 0.350, nRMSE 0.0357

# remove large model object before starting next run
rm(salmon_metab, salmon_fit)

#### SALMON RIVER- Karuk data ####

# model years together
inputs_prepped$salmon_karuk <- rbind(inputs_prepped$salmon_2022_karuk, inputs_prepped$salmon_2023_karuk)

# visualize inputs
visualize_inputs_full(inputs_prepped$salmon_karuk)
visualize_inputs_full(inputs_prepped$salmon_2022_karuk)
visualize_inputs_full(inputs_prepped$salmon_2023_karuk)
visualize_inputs_zoomed(inputs_prepped$salmon_karuk, "2022-07-01 00:00:00", "2022-07-03 00:00:00")
visualize_inputs_zoomed(inputs_prepped$salmon_karuk, "2023-08-14 00:00:00", "2023-08-16 00:00:00")

# set model specs (need more with this data to converge)
specs_salmon_karuk <- specs(bayesian_mm, burnin_steps = 6000, saved_steps = 3000, 
                            # large amount of chains so doing some thinning
                            thin_steps = 20)

# change binning
specs_salmon_karuk <- set_Q_nodes(specs_salmon, inputs_prepped$salmon_karuk$discharge, 
                                  # using default sd; doesn't change k600 & best convergence
                                  k600_priors[2,2], log_k600_sd = 0.5, min_bins = 4)
plot_Q_bins(inputs_prepped$salmon_karuk$discharge, specs_salmon_karuk)

# running model
salmon_karuk_metab <- metab(specs_salmon_karuk, data = inputs_prepped$salmon_karuk)

# get fit and save files
salmon_karuk_fit <- get_fit(salmon_karuk_metab)
write_files(salmon_karuk_fit, salmon_karuk_metab, "/salmon_karuk/", "salmon_karuk")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(salmon_karuk_metab)

# plot GPP estimates w/ sensor cleaning dates;
# removed the worst of the biofouling prior to long time period already I guess
ggplot(salmon_karuk_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  scale_x_date(limits = as.Date(c("2022-06-15", "2022-09-30"))) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-26")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()
ggplot(salmon_karuk_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  scale_x_date(limits = as.Date(c("2023-06-15", "2023-09-30"))) +
  geom_vline(xintercept = as_date(c("2023-07-13")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-27")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(salmon_karuk_metab, date_start = "2022-06-27", date_end = "2022-07-03") # all look great sans one weird day below
plot_DO_preds(salmon_karuk_metab, date_start = "2022-07-03", date_end = "2022-07-10")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-07-10", date_end = "2022-07-17")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-07-17", date_end = "2022-07-24")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-07-24", date_end = "2022-07-31")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-07-31", date_end = "2022-08-06")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-08-06", date_end = "2022-08-13")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-08-13", date_end = "2022-08-20")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-08-20", date_end = "2022-08-27")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-08-27", date_end = "2022-09-04")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-09-04", date_end = "2022-09-11")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-09-11", date_end = "2022-09-18")
plot_DO_preds(salmon_karuk_metab, date_start = "2022-09-18", date_end = "2022-09-22") 
plot_DO_preds(salmon_karuk_metab, date_start = "2023-06-28", date_end = "2023-07-05") # connected but weird flat bottom
plot_DO_preds(salmon_karuk_metab, date_start = "2023-07-05", date_end = "2023-07-12")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-07-12", date_end = "2023-07-19")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-07-19", date_end = "2023-07-26")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-07-26", date_end = "2023-07-31")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-07-31", date_end = "2023-08-07")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-08-07", date_end = "2023-08-15")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-08-15", date_end = "2023-08-22") # one weird day
plot_DO_preds(salmon_karuk_metab, date_start = "2023-08-22", date_end = "2023-08-29")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-08-29", date_end = "2023-09-06")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-09-06", date_end = "2023-09-13")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-09-13", date_end = "2023-09-20")
plot_DO_preds(salmon_karuk_metab, date_start = "2023-09-20", date_end = "2023-09-28")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(salmon_karuk_fit, salmon_karuk_metab, "Salmon (Karuk Data) 2022-2023")
plot_ER_K600(salmon_karuk_fit,  "Salmon (Karuk Data) 2022-2023")
cor.test(salmon_karuk_fit$daily$ER_mean, salmon_karuk_fit$daily$K600_daily_mean) # -0.589; correlated p << 0.002
plot_K600(salmon_karuk_fit,  "Salmon (Karuk Data) 2022-2023")

# convergence assessments
rstan::traceplot(get_mcmc(salmon_karuk_metab), pars='GPP_daily', nrow=10) # decent
rstan::traceplot(get_mcmc(salmon_karuk_metab), pars='ER_daily', nrow=10) # decent
rstan::traceplot(get_mcmc(salmon_karuk_metab), pars='K600_daily', nrow=10) # decent
mean(na.omit(salmon_karuk_fit$daily$GPP_Rhat)) # decent
mean(na.omit(salmon_karuk_fit$daily$ER_daily_Rhat)) # decent
mean(na.omit(salmon_karuk_fit$daily$K600_daily_Rhat)) # decent

# goodness of fit metrics
calc_gof_metrics(salmon_karuk_metab, "/salmon_karuk/", "salmon_karuk") # RMSE 0.384, nRMSE 0.0132

# remove large model object before starting next run
rm(salmon_karuk_metab, salmon_karuk_fit)

#### SOUTH FORK EEL RIVER @ MIRANDA ####

# model years together
inputs_prepped$sfkeel_mir <- rbind(inputs_prepped$sfkeel_mir_2022, 
                                   inputs_prepped$sfkeel_mir_2023)

# visualize inputs
visualize_inputs_full(inputs_prepped$sfkeel_mir)
visualize_inputs_full(inputs_prepped$sfkeel_mir_2022)
visualize_inputs_full(inputs_prepped$sfkeel_mir_2023)
visualize_inputs_zoomed(inputs_prepped$sfkeel_mir_2022, "2022-08-01 00:00:00", "2022-08-03 00:00:00")
visualize_inputs_zoomed(inputs_prepped$sfkeel_mir_2023, "2023-08-15 00:00:00", "2023-08-17 00:00:00")

# set model specs
specs_sfkeel_mir <- specs(bayesian_mm, burnin_steps = 2000, saved_steps = 1000)

# change binning
specs_sfkeel_mir <- set_Q_nodes(specs_sfkeel_mir, inputs_prepped$sfkeel_mir$discharge, 
                                k600_priors[3,2], log_k600_sd = 0.5, min_bins = 4)
plot_Q_bins(inputs_prepped$sfkeel_mir$discharge, specs_sfkeel_mir)

# running model
sfkeel_mir_metab <- metab(specs_sfkeel_mir, data = inputs_prepped$sfkeel_mir)

# get fit and save files
sfkeel_mir_fit <- get_fit(sfkeel_mir_metab)
write_files(sfkeel_mir_fit, sfkeel_mir_metab, "/sfkeel_mir/", "sfkeel_mir")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(sfkeel_mir_metab) # seems so much higher without depth applied!

# plot GPP estimates w/ sensor cleaning dates -- no obvious biofouling
ggplot(sfkeel_mir_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  scale_x_date(limits = as.Date(c("2022-06-15", "2022-09-30"))) +
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
ggplot(sfkeel_mir_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  scale_x_date(limits = as.Date(c("2023-06-15", "2023-09-30"))) +
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

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-06-28", date_end = "2022-07-05")
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-07-05", date_end = "2022-07-12") # some disconnections but mostly good
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-07-12", date_end = "2022-07-19") # missing data from sensor weirdness week
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-07-19", date_end = "2022-07-30") # again missing data from sensor weirdness
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-07-30", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-08-06", date_end = "2022-08-13")
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-08-13", date_end = "2022-08-20")
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-08-20", date_end = "2022-08-27")
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-08-27", date_end = "2022-09-04")
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-09-04", date_end = "2022-09-11")
plot_DO_preds(sfkeel_mir_metab, date_start = "2022-09-11", date_end = "2022-09-18")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-06-18", date_end = "2023-06-25") # look good!
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-06-25", date_end = "2023-07-01")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-07-01", date_end = "2023-07-08")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-07-08", date_end = "2023-07-15")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-07-15", date_end = "2023-07-22")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-07-22", date_end = "2023-07-29")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-07-29", date_end = "2023-08-05") # gets worse again here
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-08-05", date_end = "2023-08-12")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-08-12", date_end = "2023-08-19")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-08-19", date_end = "2023-08-26")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-08-26", date_end = "2023-09-03")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-09-03", date_end = "2023-09-10")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-09-10", date_end = "2023-09-17")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-09-17", date_end = "2023-09-24")
plot_DO_preds(sfkeel_mir_metab, date_start = "2023-09-24", date_end = "2023-09-30")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(sfkeel_mir_fit, sfkeel_mir_metab, "South Fork Eel @ Miranda 2022-2023") # points all within bins
plot_ER_K600(sfkeel_mir_fit, "South Fork Eel @ Miranda 2022-2023")
cor.test(sfkeel_mir_fit$daily$ER_mean, sfkeel_mir_fit$daily$K600_daily_mean) # correlated 0.304; p << 0.007
plot_K600(sfkeel_mir_fit, "South Fork Eel @ Miranda 2022-2023")

# convergence assessment
rstan::traceplot(get_mcmc(sfkeel_mir_metab), pars='GPP_daily', nrow=10) # great
rstan::traceplot(get_mcmc(sfkeel_mir_metab), pars='ER_daily', nrow=10) # great
rstan::traceplot(get_mcmc(sfkeel_mir_metab), pars='K600_daily', nrow=10) # looks good
mean(na.omit(sfkeel_mir_fit$daily$GPP_Rhat)) # great
mean(na.omit(sfkeel_mir_fit$daily$ER_daily_Rhat)) # great
mean(na.omit(sfkeel_mir_fit$daily$K600_daily_Rhat)) # great

# goodness of fit metrics
calc_gof_metrics(sfkeel_mir_metab, "/sfkeel_mir/", "sfkeel_mir") # rmse 0.272, nrsme 0.0420

# remove large model object before starting next run
rm(sfkeel_mir_metab, sfkeel_mir_fit)

#### SOUTH FORK EEL RIVER @ STANDISH HICKEY ####

# visualize inputs
visualize_inputs_full(inputs_prepped$sfkeel_sth)
visualize_inputs_zoomed(inputs_prepped$sfkeel_sth, "2023-08-15 00:00:00", "2023-08-17 00:00:00")

# set model specs- needs more time to converge than south fork eel @ miranda
specs_sfkeel_sth <- specs(bayesian_mm, burnin_steps = 4000, saved_steps = 2000)

# change binning
specs_sfkeel_sth <- set_Q_nodes(specs_sfkeel_sth, inputs_prepped$sfkeel_sth$discharge, 
                                k600_priors[4,2], log_k600_sd = 0.5, min_bins = 2)
plot_Q_bins(inputs_prepped$sfkeel_sth$discharge, specs_sfkeel_sth)
# looks weird but that's because of time periods removed!

# running model
sfkeel_sth_metab <- metab(specs_sfkeel_sth, data = inputs_prepped$sfkeel_sth_2023)

# get fit and save files
sfkeel_sth_fit <- get_fit(sfkeel_sth_metab)
write_files(sfkeel_sth_fit, sfkeel_sth_metab, "/sfkeel_sth/",
            "sfkeel_sth")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(sfkeel_sth_metab) # seems so much higher without depth applied!-- need to figure out why there is only one day for early july..

# plot GPP estimates w/ sensor cleaning dates --
# increases in july after cleaning but want to keep at least two days after cleaning
# all other biofouling that would have been obvious was removed
ggplot(sfkeel_sth_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-06-24", date_end = "2023-07-01")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-07-01", date_end = "2023-07-08")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-07-08", date_end = "2023-07-15")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-07-23", date_end = "2023-07-30") # skipping time because forgot to turn sensor on; looks good!
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-07-30", date_end = "2023-08-06")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-08-06", date_end = "2023-08-13")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-08-13", date_end = "2023-08-20")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-08-20", date_end = "2023-08-27")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-08-27", date_end = "2023-09-03")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-09-03", date_end = "2023-09-10")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-09-10", date_end = "2023-09-17")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-09-17", date_end = "2023-09-24")
plot_DO_preds(sfkeel_sth_metab, date_start = "2023-09-24", date_end = "2023-09-30")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(sfkeel_sth_fit, sfkeel_sth_metab, "South fork eel @ standish hickey 2023") # points all within bins
plot_ER_K600(sfkeel_sth_fit, "South fork eel @ standish hickey 2023")
cor.test(sfkeel_sth_fit$daily$ER_mean, sfkeel_sth_fit$daily$K600_daily_mean) # not correlated .140; p = 0.2452
plot_K600(sfkeel_sth_fit, "South fork eel @ standish hickey 2023")

# convergence assessment
rstan::traceplot(get_mcmc(sfkeel_sth_metab), pars='GPP_daily', nrow=10) # good
rstan::traceplot(get_mcmc(sfkeel_sth_metab), pars='ER_daily', nrow=10) # good
rstan::traceplot(get_mcmc(sfkeel_sth_metab), pars='K600_daily', nrow=10) # decent
mean(na.omit(sfkeel_sth_fit$daily$GPP_Rhat)) # great
mean(na.omit(sfkeel_sth_fit$daily$ER_daily_Rhat)) # great
mean(na.omit(sfkeel_sth_fit$daily$K600_daily_Rhat)) # great

# goodness of fit metrics
calc_gof_metrics(sfkeel_sth_metab, "/sfkeel_sth/", "sfkeel_sth") # rmse 0.201, nrsme 0.0324

# remove large model object though this is the last run :)
rm(sfkeel_sth_metab, sfkeel_sth_fit)
