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

## Reading in model input data
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
    select(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge)
  return(new_df)
}

# apply function to dataframes
inputs_prepped <- lapply(inputs_list, function(x) metab_prep(x))

#### (3) Final visual check of inputs and then modelling metabolism ####

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
writefiles <- function(data_fit, data_metab, subfolder, SiteID){
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

# set working directory
setwd("data/metab_model_outputs")

## south fork eel @ miranda 2022
visualize_inputs(inputs_prepped$sfkeel_mir_2022)
sfkeel_mir_2022 <- metab(bayes_specs_new, data = inputs_prepped$sfkeel_mir_2022)