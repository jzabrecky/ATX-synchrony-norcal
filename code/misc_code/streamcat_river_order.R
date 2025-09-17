#### Using StreamCat to get river order for study rivers
### Jordan Zabrecky
## 8.7.2025

# This code uses StreamCat to get the river orders of our study rivers
# (aka the Russian River, Salmon River, South Fork Eel River)

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "StreamCatTools", "sf"), 
       require, character.only = T)

# read in location data
locations <- read.csv("./data/EDI_data_package/site_reach_locations.csv") %>% 
  #filter(site_reach_ID == "SFE-M-1S" | site_reach_ID == "SFE-SH-1S" | 
  #         site_reach_ID == "RUS-1S" | site_reach_ID == "SAL-1S") %>% 
  # convert lat/long to spatial point feature
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4269) %>% 
  sf::st_transform(crs = 5070)

# break up data
locations_list <-
  locations %>% 
  rowwise() %>%
  group_split()
  
#### (2) Match reaches to NHD comid ####

cl <- parallel::makeCluster(10)
comids <- 
  parLapply(cl,
            locations_list,
            StreamCatTools::sc_get_comid) %>% 
  unlist()
parallel::stopCluster(cl)

# okay now I am lost; I think it is not in streamCat and is actually in NHD

