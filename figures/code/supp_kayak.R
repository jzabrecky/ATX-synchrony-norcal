### old code for kayak depths

## supplemental figure comparing those with Sophie's modeled depth estimates!!1

# will rewrite header here better later

### ALSO NEED TO READ IN FROM EDI DATA PACKAGE :)

#### (1) Loading libraries and data ####

# loading packages
lapply(c("tidyverse", "lubridate", "dataRetrieval"), require, character.only = T)

# loading kayak data
kayak_sfkeel <- read.csv("./data/field_and_lab/raw_data/sfkeel_kayak_measurements.csv")

# converting date as string to date object
kayak_sfkeel$Date <- mdy(kayak_sfkeel$Date)

# read in data from sophie's geomorph model
depths_sfkeel <- read.csv("./data/modeled_depths/sfkeel_modeled_depths.csv")

# function to clean discharge downloads
edit_Q_depth_df <- function(data) {
  new <- data %>% 
    mutate(discharge_m3_s = X_00060_00003 / 35.31) %>% 
    select(Date, depth_m, discharge_m3_s)
  return(new)
}

#### (2) Processing kayak depths ####

## south fork eel @ miranda

# calculating average depth per kayak run
depth_Q_sfkeel_mir <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Miranda", Meas_Type == "Depth") %>%
  filter(Transect != 18) %>% # removed transects 18 as first date was half depth of later two dates
  filter(Transect != 5 & Transect != 12) %>%  # removed transects 5 & 12 as first date was ~0.3 m lower than second date
  # this may be either because our GPS was slightly off or differences when taking depths across transects between two different kayakers
  dplyr::group_by(Date) %>% 
  dplyr::summarize(depth_m = mean(Depth_cm_final) / 100)

# getting daily discharge data and adding it to depth-discharge data frame
depth_Q_sfkeel_mir <- left_join(depth_Q_sfkeel_mir, 
                                readNWISdv("11476500", "00060", depth_Q_sfkeel_mir$Date[1], depth_Q_sfkeel_mir$Date[3]))
depth_Q_sfkeel_mir <- edit_Q_depth_df(depth_Q_sfkeel_mir)

## south fork eel @ standish hickey

# calculating average depth per kayak run
depth_Q_sfkeel_sth <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Standish", Meas_Type == "Depth") %>% 
  dplyr::group_by(Date) %>%  # not seeing any weird transect issues here!
  dplyr::summarize(depth_m = mean(Depth_cm_final) / 100)
# also makes sense that this site is as deep despite lower discharge-- more pools

# getting daily discharge data and adding it to depth-discharge data frame
depth_Q_sfkeel_sth <- left_join(depth_Q_sfkeel_sth, 
                                readNWISdv("11475800", "00060", depth_Q_sfkeel_sth$Date[1], depth_Q_sfkeel_sth$Date[3]))
depth_Q_sfkeel_sth <- edit_Q_depth_df(depth_Q_sfkeel_sth)

#### (3) Making plots ####

## WILL LIKELY NEED TO CHANGE AESTHETICS

miranda <- ggplot() +
  geom_point(data = depth_Q_sfkeel_mir, aes(x = discharge_m3_s, y = depth_m),
             color = "darkblue", size = 3) +
  geom_point(data = depths_sfkeel, aes(x = discharge_cms, y = median_depth_mir),
             color = "skyblue", size = 1.5) +
  xlim(0.28, 4.8) + # min & max discharge for metabolism model input
  labs(x = "Discharge (cms)", y = "Depth (m)", title = "South Fork Eel at Miranda") +
  theme_bw()
miranda

standish <- ggplot() +
  geom_point(data = depth_Q_sfkeel_sth, aes(x = discharge_m3_s, y = depth_m),
             color = "darkblue", size = 3) +
  geom_point(data = depths_sfkeel, aes(x = discharge_cms, y = median_depth_stan),
             color = "skyblue", size = 1.5) +
  xlim(0.4, 2.40) + # max discharge for metabolism model input
  labs(x = "Discharge (cms)", y = "Depth (m)", title = "South Fork Eel at Standish Hickey") +
  theme_bw()
standish
