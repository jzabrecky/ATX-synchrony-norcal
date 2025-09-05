#### code from Joanna to calculate river orders
### edited slightly to just read in reach locations already in data folder
## 09.05.2025

## probably don't need all of these but here they are

library(tidyverse)
library(readxl)
library(readr)
library(tigris)
library(StreamCatTools)
library(parallel)
lapply(c("sf", "dplyr", "nhdplusTools", "dataRetrieval", "tidyr"), require, character.only=T)


CAriv <- 
  read.csv("./data/EDI_data_package/site_reach_locations.csv") %>% 
  dplyr::select(site_reach_ID, latitude, longitude) %>% 
  sf::st_as_sf(coords = c('longitude', 'latitude'), crs = 4269) %>% 
  sf::st_transform(crs = 5070)

# break up data
CAriv_list <-
  CAriv %>% 
  rowwise() %>%
  group_split()

# create cluster and run query in parallel
cl <- parallel::makeCluster(10)
comids <- 
  parLapply(cl,
            CAriv_list,
            StreamCatTools::sc_get_comid) %>% 
  unlist()
parallel::stopCluster(cl)

# update CAriv object with comids
CAriv <- CAriv %>% 
  mutate(comid = as.numeric(comids))

# Get stream order data from NHDPlus VAA (Value Added Attributes)
# Note: This may take a few minutes as it downloads the VAA table
vaa <- get_vaa(c("comid","streamorde"))

# Join stream order data with our locations
result <- CAriv %>%
  left_join(vaa, by = "comid") %>%
  rename(Strahler_Order = streamorde)

## all 5th order streams
