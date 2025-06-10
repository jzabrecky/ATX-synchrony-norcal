#### processing metabolism outputs
### Jordan Zabrecky
## last edited 06.09.2025

# This code processes metabolism outputs from the "streamMetabolizer" package
# from script "1e_processing_metabolism_outputs.csv" and saves a csv

#### (1) Loading packages and reading in data #### 

# loading packages
lapply(c("tidyverse", "plyr", "lubridate"), require, character.only = T)

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
      d$site <- folders[i] # add name of site
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
daily_metab_list <- split(daily_metab, daily_metab$site)

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
    dplyr::select(date, discharge_m3_s)
}

# apply function to data frame
USGS_daily_discharge <- lapply(USGS_daily_discharge, function(x) clean_discharge(x))

# RUN ONCE: save these in case USGS data goes offline
#for(i in 1:length(site_names)) {
#  df <- USGS_daily_discharge[[i]] %>% 
#    mutate(site = site_names[[i]]) %>% 
#    relocate(site, .before = date)
#  
#  write.csv(df, 
#            paste("./data/USGS/", site_names[i], "_discharge_daily.csv", sep = ""),
#            row.names = FALSE)
#}

#### (2) Making depth-discharge relationships ####

# function to get channel geomorphology data from USGS
geomorph_data <- function(site_num) {
  df <- readNWISmeas(siteNumbers = site_num, expanded = T) %>%
    dplyr::filter(measured_rating_diff == "Good")%>%
    dplyr::select("chan_discharge", "chan_width","chan_area", "chan_velocity", "measurement_dt", "site_no")%>%
    mutate(mean_z = chan_area/chan_width) %>% 
    # convert from ft to meters
    mutate(discharge_m3_s = chan_discharge / 35.31,
           depth_m = mean_z * 0.3048,
           velocity_m_s = chan_velocity * 0.3048)
  return(df)
}

# function to get and edit discharge data from USGS
discharge_data <- function(site_num, start_date, end_date) {
  df <- readNWISdv(site_num, "00060", start_date, end_date) %>% 
    mutate(field_date = ymd(Date),
           discharge_m3_s = X_00060_00003 / 35.31) %>% 
    dplyr::select(field_date, discharge_m3_s)
  return(df)
}

# function to plot and visualize log depth-log discharge relationship model
Q_depth_model_plot <- function(x, y , model) {
  ggplot() +
    geom_point(aes(x = x, y = y), size = 3, color = "skyblue") + 
    geom_abline(slope = model$coefficients[[2]], intercept = model$coefficients[[1]], 
                linewidth =1.5, color="darkblue", linetype = "dotted") +
    xlab("Log(Discharge (cm^3/s))")+
    ylab("Log(Depth (m))")+
    theme_bw()
}

# function to plot and visualize final data w/ kayak depths
Q_depth_final_plot <- function(USGS_discharge_data, kayak_data, xlim, ylim) {
  ggplot() +
    geom_point(aes(x = USGS_discharge_data$discharge_m3_s, 
                   y = USGS_discharge_data$depth_m), size = 2, color = "skyblue") +
    geom_point(aes(x = kayak_data$discharge_m3_s, y = kayak_data$mean_depth_m),
               size = 3, color = "darkblue") +
    xlab("Discharge (cm^3/s)") +
    ylab("Depth (m)") +
    xlim(xlim) +
    ylim(ylim) +
    theme_bw()
}

## south fork eel @ miranda

# get south fork eel @ miranda river kayak depth
kayak_miranda <- read.csv("./data/EDI_data_package/kayak_depth_width.csv") %>% 
  dplyr::filter(site == "SFE-M" & measurement_type == "depth") %>%
  dplyr::filter(transect != 18) %>% # removed transects 18 as first date was half depth of later two dates
  dplyr::filter(transect != 5 & transect != 12) %>%  # removed transects 5 & 12 as first date was ~0.3 m lower than second date
  dplyr::mutate(field_date = ymd(field_date)) %>% 
  dplyr::group_by(field_date) %>% 
  dplyr::summarize(mean_depth_m = mean(meters))

# linear log-depth ~ log-discharge model (Leopold and Maddock (1953))
kayak_miranda <- left_join(kayak_miranda, discharge_data("11476500", kayak_miranda$field_date[1],
                                                         kayak_miranda$field_date[3]),
                           by = "field_date")

# linear log-depth ~ log-discharge model (Leopold and Maddock (1953))
miranda_depth_model <- lm(log(mean_depth_m) ~ log(discharge_m3_s), data = kayak_miranda)

# visualize log-log relationship with model
Q_depth_model_plot(x = log(kayak_miranda$discharge_m3_s), y = log(kayak_miranda$mean_depth_m), 
                   model = miranda_depth_model)

## south fork eel @ standish hickey

# get south fork eel @ standish hickey river kayak depth
kayak_standish <- read.csv("./data/EDI_data_package/kayak_depth_width.csv") %>% 
  dplyr::filter(site == "SFE-SH" & measurement_type == "depth") %>%
  dplyr::mutate(field_date = ymd(field_date)) %>% 
  dplyr::group_by(field_date) %>% 
  dplyr::summarize(mean_depth_m = mean(meters))

# linear log-depth ~ log-discharge model (Leopold and Maddock (1953))
kayak_standish <- left_join(kayak_standish, discharge_data("11475800", kayak_standish$field_date[1],
                                                           kayak_standish$field_date[3]),
                           by = "field_date")

# linear log-depth ~ log-discharge model (Leopold and Maddock (1953))
standish_depth_model <- lm(log(mean_depth_m) ~ log(discharge_m3_s), data = kayak_standish)

# visualize relationship
Q_depth_model_plot(x = log(kayak_standish$discharge_m3_s), y = log(kayak_standish$mean_depth_m), 
                   model = standish_depth_model)

## salmon

# get russian river geomorphology
geomorph_sal <- geomorph_data("11522500")

# get salmon river kayak depth
kayak_salmon <- read.csv("./data/EDI_data_package/kayak_depth_width.csv") %>% 
  dplyr::filter(site == "SAL" & measurement_type == "depth") %>%
  dplyr::mutate(field_date = ymd(field_date)) %>% 
  dplyr::group_by(field_date) %>% 
  dplyr::summarize(mean_depth_m = mean(meters))

# join in daily discharge data
kayak_salmon <- left_join(kayak_salmon, discharge_data("11522500", kayak_salmon$field_date[1], 
                                                       kayak_salmon$field_date[1]),
                          by = "field_date")

# linear log-depth ~ log-discharge model (Leopold and Maddock (1953))
salmon_depth_model <- lm(log(depth_m) ~ log(discharge_m3_s), data = geomorph_sal)

# calculate offset
salmon_offset <- kayak_salmon$mean_depth_m[1] - 
  exp(salmon_depth_model$coefficients[[1]] +
        (log(kayak_salmon$discharge_m3_s[1])
         * salmon_depth_model$coefficients[[2]]))

# visualize relationship (geomorph data only will check w/ kayak offset later
Q_depth_model_plot(x = log(geomorph_sal$discharge_m3_s), y = log(geomorph_sal$depth_m), 
                   model = salmon_depth_model)

## russian

# get russian river geomorphology
geomorph_rus <- geomorph_data("11463000")

# get salmon river kayak depth
kayak_russian <- read.csv("./data/EDI_data_package/kayak_depth_width.csv") %>% 
  dplyr::filter(site == "RUS" & measurement_type == "depth") %>%
  dplyr::mutate(field_date = ymd(field_date)) %>% 
  dplyr::group_by(field_date) %>% 
  dplyr::summarize(mean_depth_m = mean(meters))

# join in daily discharge data
kayak_russian <- left_join(kayak_russian, discharge_data("11463000", kayak_russian$field_date[1], 
                                                       kayak_russian$field_date[1]),
                          by = "field_date")

# linear log-depth ~ log-discharge model (Leopold and Maddock (1953))
russian_depth_model <- lm(log(depth_m) ~ log(discharge_m3_s), data = geomorph_rus)

# calculate offset
russian_offset <- kayak_russian$mean_depth_m[1] - 
  exp(russian_depth_model$coefficients[[1]] +
        (log(kayak_russian$discharge_m3_s[1])
         * russian_depth_model$coefficients[[2]]))

# visualize relationship
Q_depth_model_plot(x = log(geomorph_rus$discharge_m3_s), y = log(geomorph_rus$depth_m), 
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
USGS_daily_discharge$sfkeel_sth$depth_m <- exp(standish_depth_model$coefficients[[1]] +
                                                 (log(USGS_daily_discharge$sfkeel_sth$discharge_m3_s)
                                                  * standish_depth_model$coefficients[[2]]))

# apply offset to salmon & russian
USGS_daily_discharge$salmon$depth_m <- USGS_daily_discharge$salmon$depth_m + salmon_offset
USGS_daily_discharge$russian$depth_m <- USGS_daily_discharge$russian$depth_m + russian_offset

# check offset plots
Q_depth_final_plot(USGS_daily_discharge$russian, kayak_russian, c(0,5), c(0,3))
Q_depth_final_plot(USGS_daily_discharge$salmon, kayak_salmon, c(0, 24), c(0,3))
Q_depth_final_plot(USGS_daily_discharge$sfkeel_mir, kayak_miranda, c(0,3), c(0,2))
Q_depth_final_plot(USGS_daily_discharge$sfkeel_sth, kayak_standish, c(0,3), c(0,2))

#### (3) Processing metabolism data ####

# joining daily discharge with metabolism data
# (doing it by individual because there is a lot of site matching regardless)
daily_metab_list$russian <- left_join(daily_metab_list$russian, 
                                      USGS_daily_discharge$russian, by = "date")
daily_metab_list$russian_USGS <- left_join(daily_metab_list$russian_USGS, 
                                           USGS_daily_discharge$russian, by = "date")
daily_metab_list$salmon <- left_join(daily_metab_list$salmon,
                                     USGS_daily_discharge$salmon, by = "date")
daily_metab_list$salmon_karuk <- left_join(daily_metab_list$salmon_karuk,
                                                USGS_daily_discharge$salmon, by = "date")
daily_metab_list$sfkeel_mir <- left_join(daily_metab_list$sfkeel_mir,
                                         USGS_daily_discharge$sfkeel_mir, by = "date")
daily_metab_list$sfkeel_sth <- left_join(daily_metab_list$sfkeel_sth,
                                         USGS_daily_discharge$sfkeel_sth, by = "date")

# calculate mean r-hat values for each parameter
rhats <- data.frame()
for(i in 1:length(daily_metab_list)) {
  new <- daily_metab_list[[i]] %>% 
    summarize(site = site[1],
              mean_GPP_rhat = mean(GPP_Rhat, na.rm = TRUE),
              mean_ERR_rhat = mean(ER_Rhat, na.rm = TRUE),
              mean_K600_rhat = mean(K600_daily_Rhat, na.rm = TRUE))
  rhats <- rbind(rhats, new)
}

# join with metrics
metrics <- left_join(metrics, rhats, by = "site") %>% 
  relocate(site, .before = rmse)

# note for applying depths after running metabolism model
# thus, as we are not dividing GPP by anything, our GPP will be in units g O2 m^-3 d^-1
# so to get it in g O2 m^-2 d^-1 we will multiply by depth (m/m^-3 = 1/m^-2)

# function to finish processing metabolism dataframes
metab_processing <- function(df) {
  new_df <- df %>%
    # multiply GPP and ER by depth
    dplyr::mutate(GPP.mean = GPP_mean * depth_m,
           GPP.2.5.pct = GPP_2.5pct * depth_m,
           GPP.97.5.pct = GPP_97.5pct * depth_m,
           ER.mean = ER_mean * depth_m,
           ER.2.5.pct = ER_2.5pct * depth_m,
           ER.97.5.pct = ER_97.5pct * depth_m) %>% 
    # filter out unrealistic GPP and ER values
    dplyr::filter(GPP.mean > 0 & ER.mean < 0) %>%
    # calculate NEP
    dplyr::mutate(NEP.mean = GPP.mean + ER.mean,
           NEP.2.5.pct = GPP.2.5.pct + ER.2.5.pct,
           NEP.97.5.pct = GPP.97.5.pct + ER.97.5.pct) %>% 
    # add site_year tag
    dplyr::mutate(year = year(date),
                  site_year = paste(site, "_", year, sep = "")) %>% 
    # select only columns we care about
    dplyr::select(site_year, date, GPP.mean, GPP.2.5.pct, GPP.97.5.pct, GPP_Rhat,
                  ER.mean, ER.2.5.pct, ER.97.5.pct, ER_Rhat, NEP.mean, NEP.2.5.pct, 
                  NEP.97.5.pct, K600_daily_mean, K600_daily_Rhat, discharge_m3_s) %>% 
  return(new_df)
}

# function to double-check that r-hats are below 1.05 for all estimated parameters
# (just checking max value rather than any)
check_rhat <- function(df) {
  GPP_any <- any(df$GPP_Rhat > 1.05, na.rm = TRUE)
  ER_any <- any(df$ER_Rhat > 1.05, na.rm = TRUE)
  K600_any <- any(df$K600 > 1.05, na.rm = TRUE)
  print(c(GPP_any, ER_any, K600_any))
}
    
# apply functions to list of metabolism outputs
lapply(daily_metab_list, function(x) check_rhat(x)) # no r-hats above 1.05!
daily_metab_final <- lapply(daily_metab_list, function(x) metab_processing(x))

# plot original versus processed GPP
plot_GPP <- function(original_data, processed_data) {
  ggplot() +
    geom_point(aes(x = original_data$date, y = original_data$GPP_mean), 
               color = "gray", size = 2.5, alpha = 1)  +
    geom_point(aes(x = processed_data$date, y = processed_data$GPP.mean), 
               color = "#456C2B", size = 2.5, alpha = 1)  +
    labs(title = as.character(original_data$site_year[1])) +
    theme_bw()
}

# apply to list of dataframes
for(i in 1:length(daily_metab_list)) {
  print(plot_GPP(daily_metab_list[[i]], daily_metab_final[[i]]))
}

# join dataframes in list together to re-separate out by site_year
all_metab_final <- data.frame()
for(i in 1:length(daily_metab_list)) {
  all_metab_final <- rbind(all_metab_final, daily_metab_final[[i]])
}
daily_metab_final_site_year <- split(all_metab_final, all_metab_final$site_year)

# save processed metabolism estimates as csv's
lapply(names(daily_metab_final_site_year), function(x) write.csv(daily_metab_final_site_year[[x]], 
                                                                 file = paste("./data/metab_model_outputs_processed/", 
                                                                              x, "_metab", ".csv", sep = ""), 
                                                                 row.names = FALSE))

# save metrics
write.csv(metrics, "./data/metab_model_outputs_processed/metrics.csv", row.names = FALSE)
