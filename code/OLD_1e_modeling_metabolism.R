#### modeling metabolism and functions to assess model outputs
### Jordan Zabrecky
## last edited 02.04.2025

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

# apply function to list of 
inputs_prepped <- lapply(inputs_prepped, function(x) remove_siteyear(x))

#### (3) Functions to visualize outputs ####

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

#### (4) Running streamMetabolizer for all sites and visualizing outputs ####

# set working directory
setwd("data/metab_model_outputs")

# our base model name
bayesian_mm <- mm_name(type = "bayes", pool_K600 = "binned", err_obs_iid = TRUE, 
                       err_proc_iid = TRUE, ode_method = "trapezoid", deficit_src = "DO_mod",
                       engine = "stan")

## russian river 2022 -- sensor likely has biofouling issue but running it to compare with USGS DO data

# visualize inputs
visualize_inputs_full(inputs_prepped$russian_2022)
visualize_inputs_zoomed(inputs_prepped$russian_2022, "2022-07-01 00:00:00", "2022-07-03 00:00:00")

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
russian_2022_fit <- get_fit(russian_2022)
write_files(russian_2022_fit, russian_2022, "/russian_2022/", "russian_2022")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(russian_2022)

# plot GPP estimates w/ sensor cleaning dates-- potential biofouling issues here
ggplot(russian_2022_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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
# but otherwise not bad
plot_DO_preds(russian_2022, date_start = "2022-06-24", date_end = "2022-07-01")
plot_DO_preds(russian_2022, date_start = "2022-07-01", date_end = "2022-07-08")
plot_DO_preds(russian_2022, date_start = "2022-07-08", date_end = "2022-07-15")
plot_DO_preds(russian_2022, date_start = "2022-07-15", date_end = "2022-07-22")
plot_DO_preds(russian_2022, date_start = "2022-07-22", date_end = "2022-07-29")
plot_DO_preds(russian_2022, date_start = "2022-07-29", date_end = "2022-08-05")
plot_DO_preds(russian_2022, date_start = "2022-08-05", date_end = "2022-08-12")
plot_DO_preds(russian_2022, date_start = "2022-08-12", date_end = "2022-08-19") # not a good week; gets good again after cleaning on 8/17
plot_DO_preds(russian_2022, date_start = "2022-08-19", date_end = "2022-08-26")
plot_DO_preds(russian_2022, date_start = "2022-08-26", date_end = "2022-09-03") # again bad before cleaning- biofouling issue

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(russian_2022_fit, russian_2022, "Russian 2022") # points all within bins
plot_ER_K600(russian_2022_fit, "Russian 2022")
cor.test(russian_2022_fit$daily$ER_mean, russian_2022_fit$daily$K600_daily_mean) # correlated -.609; p << 0.003
plot_K600(russian_2022_fit, "Russian 2022")

# convergence assessment
rstan::traceplot(get_mcmc(russian_2022), pars='GPP_daily', nrow=10) # looks good!
rstan::traceplot(get_mcmc(russian_2022), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(russian_2022), pars='K600_daily', nrow=10)

# goodness of fit metrics
calc_gof_metrics(russian_2022, "/russian_2022/", "russian_2022") # RMSE 0.31, nRMSE 0.072

# remove large model object before starting next run
rm(russian_2022, russian_2022_fit)

## russian river 2022 (USGS DO data ver.)

# visualize inputs
visualize_inputs_full(inputs_prepped$russian_2022_USGS)
visualize_inputs_zoomed(inputs_prepped$russian_2022_USGS, "2022-07-01 00:00:00", "2022-07-03 00:00:00")

# using same specs and binning

# get fit and save files
russian_2022_USGS_fit <- get_fit(russian_2022_USGS)
write_files(russian_2022_USGS_fit, russian_2022_USGS, "/russian_2022_USGS/", "russian_2022_USGS")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(russian_2022_USGS)
russian_2022_USGS <- readRDS("./russian_2022_USGS/russian_2022_USGSmetab_obj.rds")

# plot GPP estimates w/ sensor cleaning dates-- of course no relationship with our cleaning here!
ggplot(russian_2022_USGS_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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
# but otherwise not bad
plot_DO_preds(russian_2022_USGS, date_start = "2022-06-24", date_end = "2022-07-01") # some weirdness here w/ measured DO
plot_DO_preds(russian_2022_USGS, date_start = "2022-07-01", date_end = "2022-07-08") # also weirdness here w/ measured DO
plot_DO_preds(russian_2022_USGS, date_start = "2022-07-08", date_end = "2022-07-15")
plot_DO_preds(russian_2022_USGS, date_start = "2022-07-15", date_end = "2022-07-22") # looks great!
plot_DO_preds(russian_2022_USGS, date_start = "2022-07-22", date_end = "2022-07-29")
plot_DO_preds(russian_2022_USGS, date_start = "2022-07-29", date_end = "2022-08-05")
plot_DO_preds(russian_2022_USGS, date_start = "2022-08-05", date_end = "2022-08-12")
plot_DO_preds(russian_2022_USGS, date_start = "2022-08-12", date_end = "2022-08-19")
plot_DO_preds(russian_2022_USGS, date_start = "2022-08-19", date_end = "2022-08-26")
plot_DO_preds(russian_2022_USGS, date_start = "2022-08-26", date_end = "2022-09-03")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(russian_2022_USGS_fit, russian_2022_USGS, "Russian 2022 USGS") # points all within bins
plot_ER_K600(russian_2022_USGS_fit, "Russian 2022 USGS")
cor.test(russian_2022_USGS_fit$daily$ER_mean, russian_2022_USGS_fit$daily$K600_daily_mean) # correlated -.393; p << 0.008
plot_K600(russian_2022_USGS_fit, "Russian 2022 USGS")

# convergence assessment
rstan::traceplot(get_mcmc(russian_2022_USGS), pars='GPP_daily', nrow=10) # looks good!
rstan::traceplot(get_mcmc(russian_2022_USGS), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(russian_2022_USGS), pars='K600_daily', nrow=10) # look decent

# goodness of fit metrics
calc_gof_metrics(russian_2022_USGS, "/russian_2022_USGS/", "russian_2022_USGS") # RMSE 0.32, nRMSE 0.048

# remove large model object before starting next run
rm(russian_2022_USGS, russian_2022_USGS_fit)

## salmon river 2022-- sensor also likely has biofouling issue; redo with karuk tribe data below

# visualize inputs
visualize_inputs_full(inputs_prepped$salmon_2022)
visualize_inputs_zoomed(inputs_prepped$salmon_2022, "2022-08-10 00:00:00", "2022-08-12 00:00:00")

# model specs
salmon_2022_specs <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                           thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site max 2.97, min is 1.49
salmon_2022_specs$K600_lnQ_nodes_centers <- c(1.50, 1.75, 2.0, 2.25, 2.50, 2.75, 3.0)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
salmon_2022_specs$K600_lnQ_nodediffs_sdlog <- 0.5 / 2

# running model
salmon_2022 <- metab(salmon_2022_specs, data = inputs_prepped$salmon_2022)

# get fit and save files
salmon_2022_fit <- get_fit(salmon_2022)
write_files(salmon_2022_fit, salmon_2022, "/salmon_2022/",
            "salmon_2022")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(salmon_2022) # ER estimation issues; some 95% into positive; likely due to high Q?
# also GPP drop late August by fire to the south; overestimates drop as 95% interval into negative

# plot GPP estimates w/ sensor cleaning dates
ggplot(salmon_2022_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-26")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(salmon_2022, date_start = "2022-06-27", date_end = "2022-07-03")
plot_DO_preds(salmon_2022, date_start = "2022-07-03", date_end = "2022-07-10")
plot_DO_preds(salmon_2022, date_start = "2022-07-10", date_end = "2022-07-17") # interesting flat bottoms
plot_DO_preds(salmon_2022, date_start = "2022-07-17", date_end = "2022-07-24")
plot_DO_preds(salmon_2022, date_start = "2022-07-24", date_end = "2022-07-31")
plot_DO_preds(salmon_2022, date_start = "2022-07-31", date_end = "2022-08-06")
plot_DO_preds(salmon_2022, date_start = "2022-08-06", date_end = "2022-08-13")
plot_DO_preds(salmon_2022, date_start = "2022-08-13", date_end = "2022-08-20")
plot_DO_preds(salmon_2022, date_start = "2022-08-20", date_end = "2022-08-27")
plot_DO_preds(salmon_2022, date_start = "2022-08-27", date_end = "2022-09-04") # not great
plot_DO_preds(salmon_2022, date_start = "2022-09-04", date_end = "2022-09-11") # weird breakage; flat bottoms again
plot_DO_preds(salmon_2022, date_start = "2022-09-11", date_end = "2022-09-18")
plot_DO_preds(salmon_2022, date_start = "2022-09-18", date_end = "2022-09-22") 

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(salmon_2022_fit, salmon_2022, "Salmon 2022") # points all within bins
plot_ER_K600(salmon_2022_fit, "Salmon 2022")
cor.test(salmon_2022_fit$daily$ER_mean, salmon_2022_fit$daily$K600_daily_mean) # correlated .479; p << 0.001
plot_K600(salmon_2022_fit, "Salmon 2022")

# convergence assessment
rstan::traceplot(get_mcmc(salmon_2022), pars='GPP_daily', nrow=10) # looks great
rstan::traceplot(get_mcmc(salmon_2022), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(salmon_2022), pars='K600_daily', nrow=10) # look decent

# goodness of fit metrics
calc_gof_metrics(salmon_2022, "/salmon_2022/", "salmon_2022") # rmse 0.51, nrsme 0.052

# remove large model object before starting next run
rm(salmon_2022, salmon_2022_fit)

## salmon river 2022 (Karuk Tribe data)

# visualize inputs
visualize_inputs_full(inputs_prepped$salmon_2022_karuk)
visualize_inputs_zoomed(inputs_prepped$salmon_2022_karuk, "2022-07-04 00:00:00", "2022-07-06 00:00:00")

# using same specs and binning

# running model
salmon_2022_karuk <- metab(salmon_2022_specs, data = inputs_prepped$salmon_2022_karuk)

# get fit and save files
salmon_2022_karuk_fit <- get_fit(salmon_2022_karuk)
write_files(salmon_2022_karuk_fit, salmon_2022_karuk, "/salmon_2022_karuk/",
            "salmon_2022_karuk")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(salmon_2022_karuk) # GPP still increasing across the summer but not as drastically

# plot GPP estimates w/ sensor cleaning dates-- of course no relation with our cleaning here!
ggplot(salmon_2022_karuk_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2022-07-12")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2022-07-26")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(salmon_2022_karuk, date_start = "2022-06-27", date_end = "2022-07-03") # looks great
plot_DO_preds(salmon_2022_karuk, date_start = "2022-07-03", date_end = "2022-07-10")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-07-10", date_end = "2022-07-17")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-07-17", date_end = "2022-07-24")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-07-24", date_end = "2022-07-31")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-07-31", date_end = "2022-08-06")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-08-06", date_end = "2022-08-13")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-08-13", date_end = "2022-08-20")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-08-20", date_end = "2022-08-27")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-08-27", date_end = "2022-09-04")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-09-04", date_end = "2022-09-11")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-09-11", date_end = "2022-09-18")
plot_DO_preds(salmon_2022_karuk, date_start = "2022-09-18", date_end = "2022-09-22") 

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(salmon_2022_karuk_fit, salmon_2022_karuk, "Salmon 2022 Karuk") # points all within bins
plot_ER_K600(salmon_2022_karuk_fit, "Salmon 2022 Karuk")
cor.test(salmon_2022_karuk_fit$daily$ER_mean, salmon_2022_karuk_fit$daily$K600_daily_mean) # correlated -0.332; p << 0.002
plot_K600(salmon_2022_karuk_fit, "Salmon 2022 Karuk")

# convergence assessment
rstan::traceplot(get_mcmc(salmon_2022_karuk), pars='GPP_daily', nrow=10) # looks great now
rstan::traceplot(get_mcmc(salmon_2022_karuk), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(salmon_2022_karuk), pars='K600_daily', nrow=10) # looks great

# goodness of fit metrics
calc_gof_metrics(salmon_2022_karuk, "/salmon_2022_karuk/", "salmon_2022_karuk") # rmse 0.04, nrsme 0.015

# remove large model object before starting next run
rm(salmon_2022_karuk, salmon_2022_karuk_fit)

## salmon river 2023-- sensor likely had biofouling issues, redo w/ Karuk tribe data below

# visualize inputs
visualize_inputs_full(inputs_prepped$salmon_2023)
visualize_inputs_zoomed(inputs_prepped$salmon_2023, "2023-08-10 00:00:00", "2023-08-12 00:00:00")

# model specs
salmon_2023_specs <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                           thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site max 2.97, min is 1.49
salmon_2023_specs$K600_lnQ_nodes_centers <- c(1.6, 1.9, 2.2, 2.5, 2.8, 3.1, 3.4)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
salmon_2023_specs$K600_lnQ_nodediffs_sdlog <- 0.3 / 2

# running model
salmon_2023 <- metab(salmon_2023_specs, data = inputs_prepped$salmon_2023)

# get fit and save files
salmon_2023_fit <- get_fit(salmon_2023)
write_files(salmon_2023_fit, salmon_2023, "/salmon_2023/",
            "salmon_2023")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(salmon_2023) # maybe one day of ER interval above 0?

# plot GPP estimates w/ sensor cleaning dates-- no obvious biofouling
ggplot(salmon_2023_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2023-07-13")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-27")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(salmon_2023, date_start = "2023-06-28", date_end = "2023-07-05") # connected but weird flat bottom
plot_DO_preds(salmon_2023, date_start = "2023-07-05", date_end = "2023-07-12") # some disconnections but in realm of DO observe
plot_DO_preds(salmon_2023, date_start = "2023-07-12", date_end = "2023-07-19")
plot_DO_preds(salmon_2023, date_start = "2023-07-19", date_end = "2023-07-26")
plot_DO_preds(salmon_2023, date_start = "2023-07-26", date_end = "2023-07-31")
plot_DO_preds(salmon_2023, date_start = "2023-07-31", date_end = "2023-08-07")
plot_DO_preds(salmon_2023, date_start = "2023-08-07", date_end = "2023-08-15")
plot_DO_preds(salmon_2023, date_start = "2023-08-15", date_end = "2023-08-22") # one day of weirdness here!
plot_DO_preds(salmon_2023, date_start = "2023-08-22", date_end = "2023-08-29")
plot_DO_preds(salmon_2023, date_start = "2023-08-29", date_end = "2023-09-06")
plot_DO_preds(salmon_2023, date_start = "2023-09-06", date_end = "2023-09-13")
plot_DO_preds(salmon_2023, date_start = "2023-09-13", date_end = "2023-09-20")
plot_DO_preds(salmon_2023, date_start = "2023-09-20", date_end = "2023-09-28")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(salmon_2023_fit, salmon_2023, "Salmon 2023") # points all within bins
plot_ER_K600(salmon_2023_fit, "Salmon 2023")
cor.test(salmon_2023_fit$daily$ER_mean, salmon_2023_fit$daily$K600_daily_mean) # correlated -.569; p << 0.001
plot_K600(salmon_2023_fit, "Salmon 2023")

# convergence assessment
rstan::traceplot(get_mcmc(salmon_2023), pars='GPP_daily', nrow=10) # looks great
rstan::traceplot(get_mcmc(salmon_2023), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(salmon_2023), pars='K600_daily', nrow=10) # looks good

# goodness of fit metrics
calc_gof_metrics(salmon_2023, "/salmon_2023/", "salmon_2023") # rmse 0.55, nrsme 0.016

# remove large model object before starting next run
rm(salmon_2023, salmon_2023_fit)

## salmon river 2023 (Karuk Tribe data)

# visualize inputs
visualize_inputs_full(inputs_prepped$salmon_2023_karuk)
visualize_inputs_zoomed(inputs_prepped$salmon_2023_karuk, "2023-08-10 00:00:00", "2023-08-12 00:00:00")

# running model
salmon_2023_karuk <- metab(salmon_2023_specs, data = inputs_prepped$salmon_2023_karuk)

# get fit and save files
salmon_2023_karuk_fit <- get_fit(salmon_2023_karuk)
write_files(salmon_2023_karuk_fit, salmon_2023_karuk, "/salmon_2023_karuk/",
            "salmon_2023_karuk")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(salmon_2023_karuk) # maybe one day of ER interval above 0?

# plot GPP estimates w/ sensor cleaning dates-- no obvious biofouling
ggplot(salmon_2023_karuk_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
  geom_point(color = "darkgreen", size = 3) +
  geom_line(color = "darkgreen") +
  geom_vline(xintercept = as_date(c("2023-07-13")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-07-27")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  geom_vline(xintercept = as_date(c("2023-08-10")), 
             color = "darkgray", linetype = 2, size = 1.5) +
  theme_bw()

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(salmon_2023_karuk, date_start = "2023-06-28", date_end = "2023-07-05") # connected but weird flat bottom
plot_DO_preds(salmon_2023_karuk, date_start = "2023-07-05", date_end = "2023-07-12") # some disconnections but in realm of DO observe
plot_DO_preds(salmon_2023_karuk, date_start = "2023-07-12", date_end = "2023-07-19")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-07-19", date_end = "2023-07-26")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-07-26", date_end = "2023-07-31")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-07-31", date_end = "2023-08-07")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-08-07", date_end = "2023-08-15")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-08-15", date_end = "2023-08-22") # really weird here
plot_DO_preds(salmon_2023_karuk, date_start = "2023-08-22", date_end = "2023-08-29")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-08-29", date_end = "2023-09-06") # again weirdness
plot_DO_preds(salmon_2023_karuk, date_start = "2023-09-06", date_end = "2023-09-13")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-09-13", date_end = "2023-09-20")
plot_DO_preds(salmon_2023_karuk, date_start = "2023-09-20", date_end = "2023-09-28")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(salmon_2023_karuk_fit, salmon_2023_karuk, "Salmon 2023") # points all within bins
plot_ER_K600(salmon_2023_karuk_fit, "Salmon 2023 (Karuk)")
cor.test(salmon_2023_karuk_fit$daily$ER_mean, salmon_2023_karuk_fit$daily$K600_daily_mean) # correlated -.879; p << 0.001
plot_K600(salmon_2023_karuk_fit, "Salmon 2023")

# convergence assessment
rstan::traceplot(get_mcmc(salmon_2023_karuk), pars='GPP_daily', nrow=10) # terrible
rstan::traceplot(get_mcmc(salmon_2023_karuk), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(salmon_2023_karuk), pars='K600_daily', nrow=10) # terrible

# goodness of fit metrics
calc_gof_metrics(salmon_2023_karuk, "/salmon_2023_karuk/", "salmon_2023_karuk") # rmse 0.079, nrsme 0.030

# remove large model object before starting next run
rm(salmon_2023_karuk, salmon_2023_karuk_fit)

## south fork eel @ miranda 2022

# visualize inputs
visualize_inputs_full(inputs_prepped$sfkeel_mir_2022)
visualize_inputs_zoomed(inputs_prepped$sfkeel_mir_2022, "2022-08-10 00:00:00", "2022-08-12 00:00:00")

# model specs
sfkeel_mir_2022_specs <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                           thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site max 2.97, min is 1.49
sfkeel_mir_2022_specs$K600_lnQ_nodes_centers <- c(-1.2, -0.75, -0.30, 0.15, 0.60, 1.05, 1.5)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
sfkeel_mir_2022_specs$K600_lnQ_nodediffs_sdlog <- 0.45 / 2

# running model
sfkeel_mir_2022 <- metab(sfkeel_mir_2022_specs, data = inputs_prepped$sfkeel_mir_2022)

# get fit and save files
sfkeel_mir_2022_fit <- get_fit(sfkeel_mir_2022)
write_files(sfkeel_mir_2022_fit, sfkeel_mir_2022, "/sfkeel_mir_2022/",
            "sfkeel_mir_2022")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(sfkeel_mir_2022) # seems so much higher without depth applied!

# plot GPP estimates w/ sensor cleaning dates -- no obvious biofouling
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

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-06-28", date_end = "2022-07-05")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-05", date_end = "2022-07-12") # some disconnections but mostly good
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-12", date_end = "2022-07-19") # missing data from sensor weirdness week
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-19", date_end = "2022-07-30") # again missing data from sensor weirdness
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-07-30", date_end = "2022-08-06")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-06", date_end = "2022-08-13")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-13", date_end = "2022-08-20")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-20", date_end = "2022-08-27")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-08-27", date_end = "2022-09-04")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-09-04", date_end = "2022-09-11")
plot_DO_preds(sfkeel_mir_2022, date_start = "2022-09-11", date_end = "2022-09-18")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(sfkeel_mir_2022_fit, sfkeel_mir_2022, "South fork eel @ miranda 2022") # points all within bins
plot_ER_K600(sfkeel_mir_2022_fit, "South fork eel @ miranda 2022")
cor.test(sfkeel_mir_2022_fit$daily$ER_mean, sfkeel_mir_2022_fit$daily$K600_daily_mean) # correlated 0.867; p << 0.002
plot_K600(sfkeel_mir_2022_fit, "South fork eel @ miranda 2022")

# convergence assessment
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='GPP_daily', nrow=10) # looks great
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2022), pars='K600_daily', nrow=10) # looks good

# goodness of fit metrics
calc_gof_metrics(sfkeel_mir_2022, "/sfkeel_mir_2022/", "sfkeel_mir_2022") # rmse 0.28, nrsme 0.044

# remove large model object before starting next run
rm(sfkeel_mir_2022, sfkeel_mir_2022_fit)

## south fork eel @ miranda 2023

# visualize inputs
visualize_inputs_full(inputs_prepped$sfkeel_mir_2023)
visualize_inputs_zoomed(inputs_prepped$sfkeel_mir_2023, "2023-09-10 00:00:00", "2023-09-12 00:00:00")

# model specs
sfkeel_mir_2023_specs <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                               thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site max 2.97, min is 1.49
sfkeel_mir_2023_specs$K600_lnQ_nodes_centers <- c(-1.4, -0.9, -0.4, 0.1, 0.6, 1.1, 1.6)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
sfkeel_mir_2023_specs$K600_lnQ_nodediffs_sdlog <- 0.5 / 2

# running model
sfkeel_mir_2023 <- metab(sfkeel_mir_2023_specs, data = inputs_prepped$sfkeel_mir_2023)

# get fit and save files
sfkeel_mir_2023_fit <- get_fit(sfkeel_mir_2023)
write_files(sfkeel_mir_2023_fit, sfkeel_mir_2023, "/sfkeel_mir_2023/",
            "sfkeel_mir_2023")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(sfkeel_mir_2023) # seems so much higher without depth applied!

# plot GPP estimates w/ sensor cleaning dates -- no obvious biofouling
ggplot(sfkeel_mir_2023_fit$daily, aes(x = date, y = GPP_mean)) + # plot with sensor cleaning dates
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

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-06-18", date_end = "2023-06-25") # look good!
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-06-25", date_end = "2023-07-01")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-07-01", date_end = "2023-07-08")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-07-08", date_end = "2023-07-15")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-07-15", date_end = "2023-07-22")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-07-22", date_end = "2023-07-29")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-07-29", date_end = "2023-08-05")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-08-05", date_end = "2023-08-12")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-08-12", date_end = "2023-08-19")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-08-19", date_end = "2023-08-26")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-08-26", date_end = "2023-09-03")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-09-03", date_end = "2023-09-10")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-09-10", date_end = "2023-09-17")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-09-17", date_end = "2023-09-24")
plot_DO_preds(sfkeel_mir_2023, date_start = "2023-09-24", date_end = "2023-09-30")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(sfkeel_mir_2023_fit, sfkeel_mir_2023, "South fork eel @ miranda 2023") # points all within bins
plot_ER_K600(sfkeel_mir_2023_fit, "South fork eel @ miranda 2023")
cor.test(sfkeel_mir_2023_fit$daily$ER_mean, sfkeel_mir_2023_fit$daily$K600_daily_mean) # not correlated -.119; p = 0.2372
plot_K600(sfkeel_mir_2023_fit, "South fork eel @ miranda 2023")

# convergence assessment
rstan::traceplot(get_mcmc(sfkeel_mir_2023), pars='GPP_daily', nrow=10) # looks good
rstan::traceplot(get_mcmc(sfkeel_mir_2023), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_mir_2023), pars='K600_daily', nrow=10) # looks good

# goodness of fit metrics
calc_gof_metrics(sfkeel_mir_2023, "/sfkeel_mir_2023/", "sfkeel_mir_2023") # rmse 0.25, nrsme 0.048

# remove large model object before starting next run
rm(sfkeel_mir_2023, sfkeel_mir_2023_fit)

## south fork eel @ standish hickey 2023

# visualize inputs
visualize_inputs_full(inputs_prepped$sfkeel_sth_2023)
visualize_inputs_zoomed(inputs_prepped$sfkeel_sth_2023, "2023-09-10 00:00:00", "2023-09-12 00:00:00")

# model specs
sfkeel_sth_2023_specs <- specs(bayesian_mm, burnin_steps = 5000, saved_steps = 5000,
                               thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site max 2.97, min is 1.49
sfkeel_sth_2023_specs$K600_lnQ_nodes_centers <- c(-0.9, -0.6, -0.3, 0.0, 0.3, 0.6, 0.9)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
sfkeel_sth_2023_specs$K600_lnQ_nodediffs_sdlog <- 0.3 / 2

# running model
sfkeel_sth_2023 <- metab(sfkeel_sth_2023_specs, data = inputs_prepped$sfkeel_sth_2023)

# get fit and save files
sfkeel_sth_2023_fit <- get_fit(sfkeel_sth_2023)
write_files(sfkeel_sth_2023_fit, sfkeel_sth_2023, "/sfkeel_sth_2023/",
            "sfkeel_sth_2023")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(sfkeel_sth_2023) # seems so much higher without depth applied!-- need to figure out why there is only one day for early july..

# plot GPP estimates w/ sensor cleaning dates --
# increases in july after cleaning but want to keep at least two days after cleaning
# all other biofouling that would have been obvious was removed
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

# look at DO predictions vs. data on a 7-day interval to look closely
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-06-24", date_end = "2023-07-01") # look fine
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-01", date_end = "2023-07-08")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-08", date_end = "2023-07-15")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-23", date_end = "2023-07-30") # skipping time because forgot to turn sensor on; looks good!
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-07-30", date_end = "2023-08-06")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-06", date_end = "2023-08-13")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-13", date_end = "2023-08-20")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-20", date_end = "2023-08-27")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-08-27", date_end = "2023-09-03")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-09-03", date_end = "2023-09-10")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-09-10", date_end = "2023-09-17")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-09-17", date_end = "2023-09-24")
plot_DO_preds(sfkeel_sth_2023, date_start = "2023-09-24", date_end = "2023-09-30")

# plot binning, ER vs. K600, correlation test for ER and K600, and K600
plot_binning(sfkeel_sth_2023_fit, sfkeel_sth_2023, "South fork eel @ standish hickey 2023") # points all within bins
plot_ER_K600(sfkeel_sth_2023_fit, "South fork eel @ standish hickey 2023")
cor.test(sfkeel_sth_2023_fit$daily$ER_mean, sfkeel_sth_2023_fit$daily$K600_daily_mean) # not correlated .139; p = 0.2469
plot_K600(sfkeel_sth_2023_fit, "South fork eel @ standish hickey 2023")

# convergence assessment
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='GPP_daily', nrow=10) # looks decent
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='ER_daily', nrow=10)
rstan::traceplot(get_mcmc(sfkeel_sth_2023), pars='K600_daily', nrow=10) # not horrible

# goodness of fit metrics
calc_gof_metrics(sfkeel_sth_2023, "/sfkeel_sth_2023/", "sfkeel_sth_2023") # rmse 0.20, nrsme 0.032

# remove large model object though this is the last run :)
rm(sfkeel_sth_2023, sfkeel_sth_2023_fit)
