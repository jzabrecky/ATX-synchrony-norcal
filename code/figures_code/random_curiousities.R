### hello :)

library("plyr")
library("tidyverse")
library("lubridate")
library("dataRetrieval")

#### looking at green algae cover vs. microcoleus & then vs. anabaena ####
percover <- read.csv("./data/field_and_lab/percover_byreach.csv")
percover$field_date <- ymd(percover$field_date)
percover$year <- year(percover$field_date)


# adding site_reach_year column
percover <- percover %>% 
  mutate(site_reach_year = case_when((site_reach == "SFE-M-1S" & year == 2022) ~ "SFE-M-1S_2022",
                               (site_reach == "SFE-M-3" & year == 2022) ~ "SFE-M-3_2022",
                               (site_reach == "SFE-M-4" & year == 2022) ~ "SFE-M-4_2022",
                               (site_reach == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site_reach == "RUS-1S") ~ "RUS-1S_2022",
                               (site_reach == "RUS-2") ~ "RUS-2_2022",
                               (site_reach == "RUS-3") ~ "RUS-3_2022",
                               (site_reach == "SAL-1S" & year == 2022) ~ "SAL-1S_2022",
                               (site_reach == "SAL-2" & year == 2022) ~ "SAL-2_2022",
                               (site_reach == "SAL-3" & year == 2022) ~ "SAL-3_2022",
                               (site_reach == "SFE-M-1S" & year == 2023) ~ "SFE-M-1S_2023",
                               (site_reach == "SFE-M-2" & year == 2023) ~ "SFE-M-2_2023",
                               (site_reach == "SFE-M-3" & year == 2023) ~ "SFE-M-3_2023",
                               (site_reach == "SFE-M-4" & year == 2023) ~ "SFE-M-4_2023",
                               (site_reach == "SAL-1S" & year == 2023) ~ "SAL-1S_2023",
                               (site_reach == "SAL-2" & year == 2023) ~ "SAL-2_2023",
                               (site_reach == "SAL-3" & year == 2023) ~ "SAL-3_2023",
                               (site_reach == "SFE-SH-1S") ~ "SFE-SH-1S_2023"))

# GA & microcoleus
percover_1 <- pivot_longer(percover, cols = c(5,6),values_to = "percent", names_to = "type")

#### temperature and microcoleus
micro_GA <- ggplot(data = percover_1, aes(x = field_date, y = percent, fill = type, 
                                             color = type, linetype = type, shape = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_fill_manual(values = c("darkgreen","#62a7f8")) +
  geom_area(aes(fill = type, group = type),
            alpha = 0.3, position = 'identity') +http://127.0.0.1:9553/graphics/plot_zoom_png?width=1883&height=783
  scale_color_manual(values = c("darkgreen","#62a7f8")) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  facet_wrap(~site_reach_year, scales ="free") +
  theme_bw()
micro_GA

# GA & anabaena/cylindrospermum
percover_2 <- pivot_longer(percover, cols = c(5,7),values_to = "percent", names_to = "type")

ana_cyl_GA <- ggplot(data = percover_2, aes(x = field_date, y = percent, fill = type, 
                                          color = type, linetype = type, shape = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_fill_manual(values = c("#bdb000", "darkgreen")) +
  geom_area(aes(fill = type, group = type),
            alpha = 0.3, position = 'identity') +
  scale_color_manual(values = c("#bdb000", "darkgreen")) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  facet_wrap(~site_reach_year, scales ="free") +
  theme_bw()

ana_cyl_GA

#### temperature and microcoleus
percover_site <- read.csv("./data/field_and_lab/percover_bysite.csv")
miniDOT_data <- ldply(list.files(path = "./data/miniDOT/", pattern = "_miniDOT.csv"), function(filename) {
  d <- read.csv(paste("data/miniDOT/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_miniDOT.csv")
  d$site = d$site_year %>% str_sub(end=-6)
  return(d)
})
percover_site$date_time <- ymd_hms(paste(percover_site$field_date, "08:00:00", sep = " "))
percover_site$field_date <- ymd(percover_site$field_date)
miniDOT_data$date_time <- ymd_hms(miniDOT_data$date_time)

percover_site <- percover_site %>% 
  mutate(year = year(field_date)) %>%
  filter(site != "SFE-M_excl_site2" | year != 2023) %>% # get rid of exclusion of site 2 in 2023
  mutate(site = case_when(site == "SFE-M_all_sites" ~ "SFE-M",
                          site == "RUS" ~ "RUS",
                          site == "SFE-M_excl_site2" ~ "SFE-M",
                          site == "SAL" ~ "SAL",
                          site == "SFE-SH" ~ "SFE-SH"))

# adding site_year info to accrual dataframe
percover_site <- percover_site %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

percover_temp <- ggplot(data = percover_site, aes(x = date_time, y = microcoleus)) +
  geom_point(size = 4, color = "#62a7f8") +
  facet_wrap(~site_year, scales ="free") +
  geom_line(data = miniDOT_data, aes(x = date_time, y = Temp_C)) +
  theme_bw()
percover_temp

#### sophie's depths vs mine
sophie <- ldply(list.files(path = "./data/sophie_depth/", pattern = "_depth.csv"), function(filename) {
  d <- read.csv(paste("./data/sophie_depth/", filename, sep = ""))
  d$site = filename %>% stringr::str_remove("_depth.csv")
  d$site<- d$site %>% stringr::str_remove("sophie_")
  return(d)
})


## kayak stuff- a lot pulled from 1d code
# reading in data (what we have now...)
kayak_sfkeel <- read.csv("./data/depth_measurements/sfkeel_kayak_measurements.csv")
# converting date as string to date object
kayak_sfkeel$Date <- mdy(kayak_sfkeel$Date)

# function to clean discharge downloads
edit_Q_depth_df <- function(data) {
  new <- data %>% 
    mutate(discharge_m3_s = X_00060_00003 / 35.31) %>% 
    select(Date, depth_m, discharge_m3_s)
  return(new)
}

# calculating average depth per kayak run
depth_Q_sfkeel_mir <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Miranda", Meas_Type == "Depth") %>%
  filter(Transect != 18) %>% # removed transects 18 as first date was half depth of later two dates
  filter(Transect != 5 & Transect != 12) %>%  # removed transects 5 & 12 as first date was ~0.3 m lower than second date
  # this may be either because our GPS was slightly off or differences when taking depths across transects between two different kayakers
  dplyr::group_by(Date) %>% 
  dplyr::summarize(depth_m = mean(Depth_cm_final) / 100)

# getting daily discharge data and adding it to depth-discharge data frame
# mean daily discharge in cfs
param <- "00060"
depth_Q_sfkeel_mir <- left_join(depth_Q_sfkeel_mir, 
                                readNWISdv("11476500", param, depth_Q_sfkeel_mir$Date[1], depth_Q_sfkeel_mir$Date[3]))
depth_Q_sfkeel_mir <- edit_Q_depth_df(depth_Q_sfkeel_mir)

# calculating average depth per kayak run
depth_Q_sfkeel_sth <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Standish", Meas_Type == "Depth") %>% 
  dplyr::group_by(Date) %>%  # not seeing any weird transect issues here!
  dplyr::summarize(depth_m = mean(Depth_cm_final) / 100)
# also makes sense that this site is as deep despite lower discharge-- more pools

# getting daily discharge data and adding it to depth-discharge data frame
depth_Q_sfkeel_sth <- left_join(depth_Q_sfkeel_sth, 
                                readNWISdv("11475800", param, depth_Q_sfkeel_sth$Date[1], depth_Q_sfkeel_sth$Date[3]))
depth_Q_sfkeel_sth <- edit_Q_depth_df(depth_Q_sfkeel_sth)

Q_depth <- ggplot(data = sophie, aes(x = discharge_cms, y = depth_m)) +
  geom_point(size = 3, color = "darkblue") +
  xlab("Discharge (cms)")+
  ylab("Depth (m)") +
  ggtitle("Sophie Depth Estimates (unaltered)") +
  facet_wrap(~site) +
  theme_bw()
Q_depth

Q_depth_log_log <- ggplot(data = sophie, aes(x = log(discharge_cms), y = log(depth_m))) + 
  geom_point(size = 3, color = "darkblue") +
  xlab("Log of Discharge (cms)")+
  ylab("Log of Depth (m)") +
  ggtitle("Sophie Depth Estimates (unaltered)") +
  facet_wrap(~site) +
  theme_bw()
Q_depth_log_log

# make offset of sophie
sophie_offset <- split(sophie, sophie$site)
view(sophie_offset$sfkeel_mir)

# remove points beyond our lowest kayak discharge
sophie_offset$sfkeel_mir <- sophie_offset$sfkeel_mir[14:nrow(sophie_offset$sfkeel_mir),]
sophie_offset$sfkeel_sth <- sophie_offset$sfkeel_sth[11:nrow(sophie_offset$sfkeel_sth),]

# calculate difference (the 14 and 11 are now the first rows)
mir_dif <- log(depth_Q_sfkeel_mir$depth_m[3]) - sophie_offset$sfkeel_mir$depth_m[1]
sth_dif <- depth_Q_sfkeel_sth$depth_m[3] - sophie_offset$sfkeel_sth$depth_m[1]

# add offset
sophie_offset$sfkeel_mir$depth_m_with_offset <- sophie_offset$sfkeel_mir$depth_m + mir_dif
sophie_offset$sfkeel_sth$depth_m_with_offset <- sophie_offset$sfkeel_sth$depth_m + sth_dif

### NEED TO HAVE OFFSET LOGGED SOMEHOW

# make plot
Q_depth <- ggplot(data = sophie_offset$sfkeel_mir, aes(x = discharge_cms, y = depth_m_with_offset)) +
  geom_point(size = 3, color = "darkblue") +
  geom_point(data = depth_Q_sfkeel_mir, aes(x = discharge_m3_s, y = depth_m, color ="red", size = 3)) +
  xlab("Discharge (cms)")+
  ylab("Depth (m)") +
  ggtitle("Miranda (Sophie w/ offset in blue; kayak in Red") +
  theme_bw()
Q_depth

# make plot
Q_depth <- ggplot(data = sophie_offset$sfkeel_sth, aes(x = log(discharge_cms), y = log(depth_m_with_offset))) +
  geom_point(size = 3, color = "darkblue") +
  geom_point(data = depth_Q_sfkeel_sth, aes(x = log(discharge_m3_s), y = log(depth_m), color ="red", size = 3)) +
  xlab("Discharge (cms)")+
  ylab("Depth (m)") +
  ggtitle("Standish Hickey (Sophie w/ offset in blue; kayak in Red") +
  theme_bw()
Q_depth
