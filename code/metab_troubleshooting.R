## metabolism troubleshooting

lapply(c("rstan", "StanHeaders", "streamMetabolizer", "tidyverse", "plyr"), 
       require, character.only = T)

## Reading in model input data (from starting working directory)
model_inputs <- ldply(list.files(path = "./data/metab_model_inputs/", pattern = "modelinputs"), function(filename) {
  d <- read.csv(paste("data/metab_model_inputs/", filename, sep = ""))
  return(d)
})

# convert solar.time to POSIXct class
model_inputs$solar.time <- as.POSIXct(model_inputs$solar.time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# separating into a list based on site/year
inputs_prepped <- split(model_inputs, model_inputs$site_year)

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

# subsetting salmon 2022 karuk
subset <- inputs_prepped$salmon_2022_karuk %>% 
  dplyr::filter(solar.time >= "2022-07-25 00:00:00" & solar.time <= "2022-08-07 00:00:00")

# visualize inputs
visualize_inputs_full(inputs_prepped$salmon_2022)
visualize_inputs_zoomed(inputs_prepped$salmon_2022, "2022-08-10 00:00:00", "2022-08-12 00:00:00")


# from laurel:
## no prior for ER or GPP
## come up with prior K600- using equation from Raymond's paper
## changing bin's based on Alice's code, 1-2 bins per order of magnitude
## change 3-4 bins w/ Alice's code
## 2,000, 1,000 chains for models
## look at SEM for russian and south fork eel river to ballpark a slope

bayesian_mm <- mm_name(type = "bayes", pool_K600 = "binned", 
                       err_obs_iid = TRUE, err_proc_iid = TRUE,
                       ode_method = "trapezoid", deficit_src = 'DO_mod', 
                       engine = 'stan')

# model specs
salmon_2022_specs <- specs(bayesian_mm, burnin_steps = 1000, saved_steps = 1000,
                           thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)
salmon_karuk_specs <- specs(bayesian_mm, burnin_steps = 1000, saved_steps = 1000,
                            thin_steps = 1, GPP_daily_mu = 10, ER_daily_mu = -10)

# changing range of log(Q) to better match site max 2.97, min is 1.49
salmon_2022_specs$K600_lnQ_nodes_centers <- c(1.50, 1.75, 2.0, 2.25, 2.50, 2.75, 3.0)
# guidelines from github: use 0.5 for centers 1 apart and 0.5 / 5 for centers 0.2 apart
# so sd half of distance each center is apart
salmon_2022_specs$K600_lnQ_nodediffs_sdlog <- 0.5 / 2

salmon_karuk_specs$K600_lnQ_nodes_centers <- c(0.75, 1.25, 1.75, 2.25, 2.75, 3.25, 3.75)
salmon_karuk_specs$K600_lnQ_nodediffs_sdlog <- 0.5 / 2

# running model
salmon_subset <- metab(salmon_2022_specs, data = subset)
salmon_karuk_data <- rbind(inputs_prepped$salmon_2022_karuk, inputs_prepped$salmon_2023_karuk)
salmon_karuk <- metab(salmon_karuk_specs, data = salmon_karuk_data %>% select(!site_year))

# get fit and save files
salmon_2022_fit <- get_fit(salmon_2022)
write_files(salmon_2022_fit, salmon_2022, "/salmon_2022/",
            "salmon_2022")

# plot metab estimates (note: these are w/o correct depths and will change)
plot_metab_preds(salmon_2022) # ER estimation issues; some 95% into positive; likely due to high Q?
# also GPP drop late August by fire to the south; overestimates drop as 95% interval into negative
