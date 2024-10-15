library(dataRetrieval)
library(tidyverse)

# salmon code from Joanna that works
geomorph_sal <- readNWISmeas(siteNumbers="11522500",expanded = T) %>%
  dplyr::filter(measured_rating_diff == "Good")%>%
  dplyr::select("chan_discharge", "chan_width","chan_area", "chan_velocity", "measurement_dt", "site_no")%>%
  mutate(year = year(as.Date(measurement_dt)))%>%
  mutate(mean_z = chan_area/chan_width)

# input russian data
geomorph_rus <- readNWISmeas(siteNumbers="11463000",expanded = T) %>%
  dplyr::filter(measured_rating_diff == "Good")%>%
  dplyr::select("chan_discharge", "chan_width","chan_area", "chan_velocity", "measurement_dt", "site_no")%>%
  mutate(year = year(as.Date(measurement_dt)))%>%
  mutate(mean_z = chan_area/chan_width)

# rest of joanna code
#convert to m
SAL_DQ <- geomorph[,c("measurement_dt","chan_discharge","mean_z")]
SAL_DQ$depth_m <- SAL_DQ$mean_z*0.3048
SAL_DQ$discharge <- SAL_DQ$chan_discharge/35.314666212661

#visualize
ggplot(SAL_DQ, aes(discharge, depth_m))+
  geom_point()+
  #scale_x_log10()+
  #scale_y_log10()+
  theme_bw()


## use log-log as well here
## Extract relationship information (power law relationship works better here than linear)
# SAL
summary(lm(log(SAL_DQ$depth_m) ~ log(SAL_DQ$discharge))) # Depth (m) = exp(0.32207*log(Q) + -1.03866)
