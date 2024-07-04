#### functions associated with downloading and processing GLDAS data
### edited from "StreamLight" Package by Jordan Zabrecky
## last edited 06.19.2024

# This data modified the "extract_height" function from the "StreamLight" 
# package to allow for use of a locally downloaded .asc file from 
# Simard et al. (2011) from https://webmap.ornl.gov/ogc/dataset.jsp?ds_id=10023
# and, additionally, only returns tree height rather than full table

# new parameter: 
#'@param simard_loc location of downloaded Simard et al. (2011) .asc file of global tree heights

extract_height <- function(Site_ID, Lat, Lon, site_crs, simard_loc){
  #Import the Simard et al. (2011) dataset 
  simard2011 <- raster::raster(paste0(simard_loc))
  
  # #Import the Simard et al. (2011) dataset if it is not already loaded
  #   if(!exists("simard2011")){data("simard2011", package = "StreamLightUtils")}
  
  #Defining the min and max values (by default these are not associated with the raster)
  map_ranges <- raster::setMinMax(simard2011)
  
  #Create a simple features object from the site location
  site_location <- data.frame(Lat, Lon)
  
  #Check if the data is in WGS84, if not reproject to WGS84
  if(site_crs == 4326){
    xy_sf <- sf::st_as_sf(site_location, coords = c('Lon', 'Lat'), crs = site_crs)
  } else{
    xy_native <- sf::st_as_sf(site_location, coords = c('Lon', 'Lat'), crs = site_crs)
    xy_sf <- sf::st_transform(xy_native, crs = 4326)
  } #End if else statement    
  
  #Extracting the canopy height at our site
  suppressWarnings(TH <- raster::extract(map_ranges, xy_sf))
  
  #Bind together the final information
  bound <- setNames(data.frame(Site_ID, Lat, Lon, TH), c("Site_ID", "Lat", "Lon", "TH"))
  
  #return(bound)
  
  # only interested in returning the tree height
  return(bound$TH)
} #End extract_height function
