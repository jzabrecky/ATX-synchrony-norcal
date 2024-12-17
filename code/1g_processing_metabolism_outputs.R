#### processing metabolism outputs
### Jordan Zabrecky
## last edited 12.14.2024

# This code processes metabolism outputs from the "streamMetabolizer" package
# from script "1g_processing_metabolism_outputs.csv" and saves a csv

#### (1) Loading packages and reading in data #### 

# loading packages
lapply(c("tidyverse", "plyr", "lubridate", "dataRetrieval"), require, character.only = T)

# function to read in data across subfolders of certain name
read_metab_data <- function(path, pattern) {
  
  # get list of subfolders in folder path
  folders <- list.files(path)
  
  # initialize empty data frame
  final <- data.frame()
  
  # iterate through each subfolder and add cvs's to dataframe
  for(i in 1:length(folders)) {
    temp <- ldply(list.files(paste(path, folders[i], sep = ""), pattern = pattern), function(filename) {
      d <- read.csv(paste(path, folders[i], "/", filename, sep = ""), header = TRUE) 
      d$site_year <- folders[i] # add name of site and year
      return(d)
    })
    final <- rbind(final, temp)
  }
  
  return(final)
}

# read in daily metab data
daily_metab <- read_metab_data(path = "./data/metab_model_outputs/", pattern = "daily.csv")

# convert date column to date object
daily_metab$date <- ymd(daily_metab$date)

# split into a list based on site_year
daily_metab_list <- split(daily_metab, daily_metab$site_year)

# read in metrics data
metrics <- read_metab_data(path = "./data/metab_model_outputs/", pattern = "metrics.csv")
# briefly look at these- add to supplemental table

##  get daily discharge from USGS for each site (needed to figure out the depth per day!)

# site and parameter info
USGS_gages <- c("11463000", "11522500", "11476500", "11475800") # our gages
site_names <- c("russian", "salmon", "sfkeel_mir", "sfkeel_sth") # site names in order of above
param <- "00060" # discharge param for USGS

# use dataRetrival to get daily discharge
USGS_daily_discharge <- lapply(USGS_gages, function(x) 
  readNWISdv(x, param, "2022-06-23","2023-09-30"))
names(USGS_daily_discharge) <- site_names # adding site names to list

# function to clean USGS discharge data frame list
clean_discharge <- function(df) {
  df <- df %>% 
    # convert time zone as data is in UTC
    mutate(date = ymd(Date),
           discharge_m3_s = as.double(X_00060_00003) / 35.31) %>% # rename parameter to discharge
    select(date, discharge_m3_s)
}

# apply function to data frame
USGS_daily_discharge <- lapply(USGS_daily_discharge, function(x) clean_discharge(x))

#### (2) Making depth-discharge relationships ####

#function to get channel geomorphology data from USGS
geomorph_data <- function(site_num) {
  df <- readNWISmeas(siteNumbers = site_num, expanded = T) %>%
    dplyr::filter(measured_rating_diff == "Good")%>%
    dplyr::select("chan_discharge", "chan_width","chan_area", "chan_velocity", "measurement_dt", "site_no")%>%
    mutate(mean_z = chan_area/chan_width) %>% 
    # convert from ft to meters
    mutate(discharge_m3_s = chan_discharge / 35.31,
           depth_m = mean_z * 0.3048)
  return(df)
}

# function to plot and visualize depth-discharge relationship
Q_depth_plot <- function(x, y , model) {
  ggplot() +
    geom_point(aes(x = x, y = y), size = 3, color = "skyblue") + 
    geom_abline(slope = model$coefficients[[2]], intercept = model$coefficients[[1]], 
                linewidth =1.5, color="darkblue", linetype = "dotted") +
    xlab("Discharge (cms)")+
    ylab("Depth (m)")+
    theme_bw()
}

## south fork eel @ miranda

# read in data from sophie's geomorph model
depths_sfkeel <- read.csv("./data/modeled_depths/sfkeel_modeled_depths.csv")

# roughly a log-log relationship so will just use that
miranda_depth_model <- lm(log(median_depth_mir) ~ log(discharge_cms), data = depths_sfkeel)

# visualize relationship
Q_depth_plot(x = log(depths_sfkeel$discharge_cms), y = log(depths_sfkeel$median_depth_mir), 
             model = miranda_depth_model)

## south fork eel @ standish hickey

# roughly a log-log relationship so will just use that
sth_depth_model <- lm(log(median_depth_stan) ~ log(discharge_cms), data = depths_sfkeel)

# visualize relationship
Q_depth_plot(x = log(depths_sfkeel$discharge_cms), y = log(depths_sfkeel$median_depth_stan), 
             model = sth_depth_model)

## salmon

# get salmon river kayak depths
kayak_salmon <- read.csv("./data/EDI_data_package/kayak_depth_width.csv") %>% 
  dplyr::filter(site == "SAL" & measurement_type == "depth") %>%
  dplyr::mutate(Date = ymd(field_date)) %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarize(mean_depth_m = mean(meters))

# join in daily discharge data
kayak_salmon <- left_join(kayak_salmon, readNWISdv("11522500", "00060", 
                                                   kayak_salmon$Date[1], 
                                                   kayak_salmon$Date[2]))

# linear log-depth ~ log-discharge model
salmon_depth_model <- lm(log(depth_m) ~ log(discharge_m3_s), data = geomorph_sal)

# visualize relationship
Q_depth_plot(x = log(geomorph_sal$discharge_m3_s), y = log(geomorph_sal$depth_m), 
             model = salmon_depth_model)

## russian

# get russian river geomorphology
geomorph_rus <- geomorph_data("11463000")

# linear log-depth ~ log-discharge model
russian_depth_model <- lm(log(depth_m) ~ log(discharge_m3_s), data = geomorph_sal)

# visualize relationship
Q_depth_plot(x = log(geomorph_rus$discharge_m3_s), y = log(geomorph_rus$depth_m), 
             model = russian_depth_model)

## apply depths to discharge dataframe
USGS_daily_discharge$russian$depth_m <- exp(russian_depth_model$coefficients[[1]] + 
                                              (log(USGS_daily_discharge$russian$discharge_m3_s) 
                                               * russian_depth_model$coefficients[[2]]))
USGS_daily_discharge$salmon$depth_m <- exp(salmon_depth_model$coefficients[[1]] +
                                             (log(USGS_daily_discharge$salmon$discharge_m3_s)
                                              * salmon_depth_model$coefficients[[2]]))
USGS_daily_discharge$sfkeel_mir$depth_m <- exp(miranda_depth_model$coefficients[[1]] +
                                                 (log(USGS_daily_discharge$sfkeel_mir$discharge_m3_s)
                                                  * miranda_depth_model$coefficients[[2]]))
USGS_daily_discharge$sfkeel_sth$depth_m <- exp(sth_depth_model$coefficients[[1]] +
                                                 (log(USGS_daily_discharge$sfkeel_sth$discharge_m3_s)
                                                  * sth_depth_model$coefficients[[2]]))

#### (3) Processing metabolism data ####

# joining daily discharge with metabolism data
# (doing it by individual because there is a lot of site matching regardless)
daily_metab_list$russian_2022 <- left_join(daily_metab_list$russian_2022, 
                                           USGS_daily_discharge$russian, by = "date")
daily_metab_list$russian_2022_USGS <- left_join(daily_metab_list$russian_2022_USGS, 
                                                USGS_daily_discharge$russian, by = "date")
daily_metab_list$salmon_2022 <- left_join(daily_metab_list$salmon_2022,
                                          USGS_daily_discharge$salmon, by = "date")
daily_metab_list$salmon_2023 <- left_join(daily_metab_list$salmon_2023,
                                          USGS_daily_discharge$salmon, by = "date")
daily_metab_list$sfkeel_mir_2022 <- left_join(daily_metab_list$sfkeel_mir_2022,
                                              USGS_daily_discharge$sfkeel_mir, by = "date")
daily_metab_list$sfkeel_mir_2023 <- left_join(daily_metab_list$sfkeel_mir_2023,
                                              USGS_daily_discharge$sfkeel_mir, by = "date")
daily_metab_list$sfkeel_sth_2023 <- left_join(daily_metab_list$sfkeel_sth_2023,
                                              USGS_daily_discharge$sfkeel_mir, by = "date")

# function to finish processing metabolism dataframes
metab_processing <- function(df) {
  new_df <- df %>%
    mutate(GPP.mean = GPP_mean * depth_m,
           GPP.2.5.pct = GPP_2.5pct * depth_m,
           GPP.97.5.pct = GPP_97.5pct * depth_m,
           ER.mean = ER_mean * depth_m,
           ER.2.5.pct = ER_2.5pct * depth_m,
           ER.97.5.pct = ER_97.5pct * depth_m) %>% 
    filter(GPP.2.5.pct > 0 & ER.2.5.pct < 0) %>%
    mutate(NEP.mean = GPP.mean + ER.mean,
           NEP.2.5.pct = GPP.2.5.pct + ER.2.5.pct,
           NEP.97.5.pct = GPP.97.5.pct + ER.97.5.pct) %>% 
    dplyr::select(site_year, date, GPP.mean, GPP.2.5.pct, GPP.97.5.pct, ER.mean,
                  ER.2.5.pct, ER.97.5.pct, NEP.mean, NEP.2.5.pct, NEP.97.5.pct)
  
  return(new_df)
}
    
# apply function to list of metabolism outputs
daily_metab_final <- lapply(daily_metab_list, function(x) metab_processing(x))

# NEED TO LOOK AT SALMON DEPTHS- VERY DIFFERENT FROM MODEL and 2022 and 2023 very different
# ALSO COMPARE RUSSIAN; south fork eels look like where I would expect them to be;
# look at how joanna's model differs from what I did (she used an exponential)

# ALSO REALIZING I USED GEOMORPH INSTEAD OF KAYAK DEPTHS WHOOPS
# ISSUES w/ SALMON RIVER KAYAK DATA-- IF NOT DOING THIS REMOVE FROM EDI DATA

# if using geomorph data investigate WHY it is so different from original estimates