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

# setting bayesian model specifications
bayes_name_new <- mm_name(type='bayes', pool_K600="binned",
                          err_obs_iid=TRUE, err_proc_iid = TRUE,
                          ode_method = "trapezoid", deficit_src='DO_mod', engine='stan')
bayes_specs_new <- specs(bayes_name_new) # may need to adjust in the future

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

# visualize_inputs
visualize_inputs(inputs_prepped$sfkeel_mir_2022)

# run model
sfkeel_mir_2022 <- metab(bayes_specs_new, data = inputs_prepped$sfkeel_mir_2022)
sfkeel_mir_2022_fit <- get_fit(sfkeel_mir_2022)

# check warnings
sfkeel_mir_2022_fit[["warnings"]]

# check rhat for convergence
sfkeel_mir_2022_fit$overall %>%
  dplyr::select(ends_with('Rhat'))

# save files
write_files(sfkeel_mir_2022_fit, sfkeel_mir_2022, "/sfkeel_mir_2022/20240708/", 
            "sfkeel_mir_2022")

# visualizing outputs
plot_binning(sfkeel_mir_2022_fit, sfkeel_mir_2022, 
             title = "South Fork Eel @ Miranda, 2022")
plot_metab_preds(predict_metab(sfkeel_mir_2022))
plot_DO_preds(predict_DO(sfkeel_mir_2022))
plot_ER_K600(sfkeel_mir_2022_fit, title = "South Fork Eel @ Miranda, 2022")
plot_K600(sfkeel_mir_2022_fit, title = "South Fork Eel @ Miranda, 2022")

# K600 and ER correlation test
cor.test(sfkeel_mir_2022_fit$daily$K600_daily_mean, sfkeel_mir_2022_fit$daily$ER_daily_mean)

# remove large .rmd before starting next model (to avoid R crashing)
rm(sfkeel_mir_2022)