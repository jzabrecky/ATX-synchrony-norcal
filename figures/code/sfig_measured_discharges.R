#### USGS gage discharge vs. our discharge measurements
### Jordan Zabrecky
## last edited: 07.21.2025

# This figure shows that our discharge measurements are in rough accordance 
# with nearby USGS gage discharge measurements

#### (1) Loading libraries, our discharge data, and USGS discharge data ####

# Loading libraries
lapply(c("tidyverse", "lubridate", "dataRetrieval", "plyr"), require, 
       character.only = T)

# Loading our discharge data measured with SonTek Flowmeter 2 and calculated
# using the velocity-area method (NOTE: calculations done externally)
discharge <- read.csv("./data/EDI_data_package/discharge_measurements.csv")

# Loading USGS discharge data
USGS_discharge <- ldply(list.files(path = "./data/USGS/", pattern = "_continuous.csv"), function(filename) {
  d <- read.csv(paste("data/USGS/", filename, sep = ""))
  d$site = filename %>% stringr::str_remove("_discharge_continuous.csv")
  d$date_time = ymd_hms(d$date_time, tz = "America/Los_Angeles")
  return(d)
})

# split into list
USGS_discharge_list <- split(USGS_discharge, USGS_discharge$site)

#### (2) Cleaning dataframes ####

# create date_time column for our measured discharge data
discharge$date_time <- ymd_hms(discharge$date_time, tz = "America/Los_Angeles") # converting to POSIXct

# take second measurement (the better one) for SFE-SH on 8/24/2023
discharge <- discharge[-19,]

# trimming USGS_discharge dataset to fit field season we measured discharge in
USGS_discharge_list$russian <- USGS_discharge_list$russian %>% 
  filter(date_time >= "2022-06-24 00:00:00" & date_time <= "2022-09-16 00:00:00") %>% 
  mutate(site = "RUS")
USGS_discharge_list$sfkeel_mir <- USGS_discharge_list$sfkeel_mir %>% 
  filter(date_time >= "2022-06-29 00:00:00" & date_time <= "2022-09-18 00:00:00") %>% 
  mutate(site = "SFE-M")
USGS_discharge_list$sfkeel_sth <- USGS_discharge_list$sfkeel_sth %>% 
  dplyr::filter(date_time >= "2023-06-24 00:00:00" & date_time <= "2023-09-28 00:00:00") %>% 
  mutate(site = "SFE-SH")

# combining all into one data frame
discharge_all <- rbind(USGS_discharge_list$russian, USGS_discharge_list$sfkeel_mir, 
                       USGS_discharge_list$sfkeel_sth) %>% 
  mutate(minute = minute(date_time)) %>% 
  # only keep 30's and 00's so it doesn't take as long to plot and matches up with our measured
  filter(minute == 0 | minute == 30) %>% 
  select(!minute)
  
# left joining in our discharge measurements
discharge_all <- left_join(discharge_all, discharge, by = c("date_time", "site"))

#### (3) Making figure ####

# use factoring to control order of sites on plot
discharge_all$site_f <- factor(discharge_all$site, levels = c("SFE-M", "SFE-SH", "RUS"))

# make figure
figure <- ggplot(data = discharge_all, aes(x = date_time, y = discharge_m3_s.x)) +
  geom_area(fill = "#d9ecff") +
  geom_point(aes(y = discharge_m3_s.y, fill = site), size = 4, alpha = 0.8, stroke = 1, shape = 21) +
  facet_wrap(~site_f, ncol = 1, scales = "free", 
             labeller = as_labeller(c(`RUS` = "Russian River (RUS)", 
                                      `SFE-M` = "South Fork Eel River Lower (SFE-Lower)",
                                      `SFE-SH` = "South Fork Eel River Upper (SFE-Upper)"))) +
  scale_fill_manual(values = c("#bdb000", "#416f16", "#bfe079")) +
  labs(x = NULL, y = expression("Discharge (m"^3~"s"^-1*")")) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"))
figure

ggsave("./figures/sfig_measured_discharge_notfinal.tiff", dpi = 600, 
       width=12, height=12, unit="cm")
