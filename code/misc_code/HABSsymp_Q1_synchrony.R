#### Large synchrony figure of metabolism, accrual, and anatoxins (for HABs poster)
### Jordan Zabrecky
## last edited 09.24.2024

# Preliminary version of synchrony figure for US Harmful Algae Symposium

# FOR DATA COMPILING, A SIMILAR THING MAY OCCUR FOR STEP 4; IN SUCH CASE,
# THIS DATA WILL BE MOVED TO THERE AND THE FINAL CSV FOR THAT WILL BE IMPORTED HERE 

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "cowplot"), 
       require, character.only = T)

# loading in metabolism data
metabolism <- ldply(list.files(path = "./data/prelim_metab_estimates/", pattern = "daily_est.csv"), function(filename) {
  d <- read.csv(paste("./data/prelim_metab_estimates/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_daily_est.csv")
  d$site = d$site_year %>% str_sub(end=-6)
  return(d)
})

# using discharge already downloaded in our model input csv
# IN THE FUTURE WILL JUST GET DAILY ESTIMATE AND LEFT JOIN :)
discharge <- ldply(list.files(path = "./data/metab_model_inputs/", pattern = "_modelinputs.csv"), function(filename) {
  d <- read.csv(paste("./data/metab_model_inputs/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_modelinputs.csv") 
  d <- d %>% 
    select(site_year, date_time, discharge_m3_s) %>% # selecting columns we care about
  return(d)
})

# loading in benthic cyanobacteria data
accrual <- read.csv("./data/field_and_lab/percover_bysite.csv")
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")

#### (2) Data processing before making figures ####

# applying lubridate
metabolism$date <- ymd(metabolism$date)
discharge$date_time <- ymd_hms(discharge$date_time)
accrual$field_date <- ymd(accrual$field_date)
anatoxins$field_date <- ymd(anatoxins$field_date)

## adjusting metabolism dataset

# cutting down metabolism data (may adjust depending on how csvs are saved from final modeling :) )
metabolism <- metabolism %>% 
  select(site_year, date, GPP_mean, GPP_sd, GPP_2.5pct, GPP_97.5pct, ER_mean, ER_sd, ER_2.5pct, ER_97.5pct) %>% 
  mutate(NEP_mean = GPP_mean + ER_mean, # adding because ER is negative
         NEP_2.5pct = GPP_2.5pct + ER_2.5pct,
         NEP_97.5pct = GPP_97.5pct + ER_97.5pct,
         # need to calculate a date_time so it can be plotted with continuous discharge
         date_time = as.POSIXct(paste(date, "06:00:00", sep = " "))) # will just set as 6am on that day's estimate

# split into a list by site year
metabolism_list <- split(metabolism, metabolism$site_year)

## adjusting discharge dataset

# need to supplement discharge for salmon 2022 and russian 2022 because sensors were taken out before last day of sampling
rus_dis <- readNWISuv("11463000", "00060", "2022-09-01", "2022-09-17")
sal_dis <- readNWISuv("11522500", "00060", "2022-09-22", "2022-09-24")
sth_dis <- readNWISuv("11475800", "00060", "2023-06-19", "2023-06-24")

# joining together temporarily
xtra_dis <- rbind(rus_dis, sal_dis)
xtra_dis <- rbind(xtra_dis, sth_dis)

# creating date_time column in PST
xtra_dis$date_time <- ymd_hms(xtra_dis$dateTime) # converting to POSIXct
xtra_dis$date_time <- with_tz(xtra_dis$date_time, tz = "America/Los_Angeles") # change from UTC to PST

# add site_year info and select columns we care about
xtra_dis <- xtra_dis %>% 
  mutate(site_year = case_when(site_no == "11463000" ~ "russian_2022",
                               site_no == "11522500" ~ "salmon_2022",
                               site_no == "11475800" ~ "sfkeel_sth_2023"),
         discharge_m3_s = X_00060_00000 / 35.31) %>% 
  select(site_year, date_time, discharge_m3_s)

# binding to discharge dataframe
discharge <- rbind(discharge, xtra_dis)

# cutting down discharge data as 5-minute intervals take forever to plot
discharge <- discharge %>%
  mutate(minute = minute(date_time),
         hour = hour(date_time)) %>% 
  # will ideally use actual downloaded daily discharge in future, but for purposes of poster will use
  # the discharge at 6 pm
  filter(minute == 0 & hour == 6)

# split into a list by site
discharge_list <- split(discharge, discharge$site_year)

## adjusting accrual dataset

# removing 2023 that exludes SFE-M-2 and making site names continuous
accrual <- accrual %>% 
  mutate(year = year(field_date)) %>%
  filter(site != "SFE-M_excl_site2" | year != 2023) %>% # get rid of exclusion of site 2 in 2023
  mutate(site = case_when(site == "SFE-M_all_sites" ~ "SFE-M",
                          site == "RUS" ~ "RUS",
                          site == "SFE-M_excl_site2" ~ "SFE-M",
                          site == "SAL" ~ "SAL",
                          site == "SFE-SH" ~ "SFE-SH"))

# adding site_year info to accrual dataframe
accrual <- accrual %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

# pivot to make data frame longer (group & percent)
accrual_long <- pivot_longer(accrual, cols = c(3:7), names_to = "group", values_to = "percent")

# we only care about microcoleus and anabaena & cylindrospermum
accrual_long <- accrual_long %>% 
  filter(group == "microcoleus" | group == "anabaena_cylindrospermum")

# split into a list by site
accrual_list <- split(accrual_long, accrual_long$site_year)

## adjusting anatoxins dataset

# also adding site_year information to anatoxins dataframe
anatoxins <- anatoxins %>% 
  mutate(year = year(field_date)) %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

# sample collected on 9/8/2022 was a retake for a sample on 9/6/2022
# as a substitute tech took a very watery sample of just the ends of the mats at SFE-M-1S
anatoxins$field_date[16] <- anatoxins$field_date[15]

# calculate average and max per day at each site
atx_summarized <- anatoxins %>% 
  dplyr::rename(group = sample_type) %>% 
  dplyr::group_by(site_year, field_date, site, group) %>% 
  dplyr::summarize(mean_ATX_all_ug_chla_g = mean(ATX_all_ug_chla_ug),
                   mean_ATX_all_ug_afdm_g = mean(ATX_all_ug_afdm_g),
                   max_ATX_all_ug_chla_g = max(ATX_all_ug_chla_ug), # will probably not use max
                   max_ATX_all_ug_afdm_g = max(ATX_all_ug_afdm_g))

# split into a list by site
anatoxins_list <- split(atx_summarized, atx_summarized$site_year)

#### (3) Making Metabolism & Discharge Figures ####

# making them all separately so I can easily modify the scales on each separately

## south fork eel river @ miranda 2022

sfkmir22_GPP_dis <- ggplot(data = metabolism_list$sfkeel_mir_2022, aes(x = date_time)) +
  geom_area(data = discharge_list$sfkeel_mir_2022, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#BFDDFD") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 2.5, alpha = 1) +
  labs(y = NULL, x = NULL) +
  scale_x_datetime(limits = as_datetime(c("2022-06-29 00:00:00", "2022-09-20 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
         panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
         text = element_text(size = 30), axis.text.x = element_blank(), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
         axis.ticks.length=unit(.25, "cm"))
sfkmir22_GPP_dis

## south fork eel river @ miranda 2023

sfkmir23_GPP_dis <- ggplot(data = metabolism_list$sfkeel_mir_2023, aes(x = date_time)) +
  geom_area(data = discharge_list$sfkeel_mir_2023, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#BFDDFD") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 2.5, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2023-06-18 00:00:00", "2023-09-27 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.text.x = element_blank(), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
        axis.ticks.length=unit(.25, "cm"))
sfkmir23_GPP_dis

## south fork eel river @ standish hickey 2023

# RIBBON NOT SHOWING FOR SINGLE POINTS HERE?
sfksth23_GPP_dis <- ggplot(data = metabolism_list$sfkeel_sth_2023, aes(x = date_time)) +
  geom_area(data = discharge_list$sfkeel_sth_2023, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#BFDDFD") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 2.5, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2023-06-20 00:00:00", "2023-09-27 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.text.x = element_blank(), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
        axis.ticks.length=unit(.25, "cm"))
sfksth23_GPP_dis

## russian river 2022

# NEEDS TO BE REDONE W/ USGS DATA
rus22_GPP_dis <- ggplot(data = metabolism_list$russian_2022, aes(x = date_time)) +
  geom_area(data = discharge_list$russian_2022, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#BFDDFD") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 2.5, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2022-06-24 00:00:00", "2022-09-16 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.text.x = element_blank(), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
        axis.ticks.length=unit(.25, "cm"))
rus22_GPP_dis

## salmon river 2022

# NEEDS TO BE REDONE WITH KARUK DATA
# removed values where ribbon is below zero for now
metabolism_list$salmon_2022[c(34:35),c(3:13)] <- NA
sal22_GPP_dis <- ggplot(data = metabolism_list$salmon_2022, aes(x = date_time)) +
  geom_area(data = discharge_list$salmon_2022, aes(y = discharge_m3_s * 0.63, x = date_time), fill = "#BFDDFD") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 2.5, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2022-06-26 00:00:00", "2022-09-23 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 0.63)) +
  coord_cartesian(ylim = c(0, 19)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.text.x = element_blank(), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
        axis.ticks.length=unit(.25, "cm"))
sal22_GPP_dis

## salmon river 2023

# NEEDS TO BE REDONE WITH KARUK DATA
sal23_GPP_dis <- ggplot(data = metabolism_list$salmon_2023, aes(x = date_time)) +
  geom_area(data = discharge_list$salmon_2023, aes(y = discharge_m3_s * 0.63, x = date_time), fill = "#BFDDFD") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 2.5, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2023-06-25 00:00:00", "2023-09-25 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 0.63)) +
  coord_cartesian(ylim = c(0, 19)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.text.x = element_blank(), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
        axis.ticks.length=unit(.25, "cm"))
sal23_GPP_dis

#### (3) Making Accrual & Anatoxins Figures ####

## south fork eel river @ miranda 2022

sfkmir22_acc_atx <- ggplot(data = accrual_list$sfkeel_mir_2022, aes(x = field_date, y = percent)) +
  geom_bar(data = anatoxins_list$sfkeel_mir_2022, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_afdm_g, fill = group), width = 6, color = "black") +
  geom_line(data = accrual_list$sfkeel_mir_2022, aes(y = 110 - (percent * 4), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = accrual_list$sfkeel_mir_2022, aes(y = 110 - (percent * 4), color = group,shape = group),
             size = 5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 18),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2022-06-29", "2022-09-20"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
sfkmir22_acc_atx

## south fork eel river @ miranda 2023

sfkmir23_acc_atx <- ggplot(data = accrual_list$sfkeel_mir_2023, aes(x = field_date, y = percent)) +
  geom_bar(data = anatoxins_list$sfkeel_mir_2023, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_afdm_g, fill = group), width = 5.5, color = "black") +
  geom_line(data = accrual_list$sfkeel_mir_2023, aes(y = 110 - (percent * 4), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = accrual_list$sfkeel_mir_2023, aes(y = 110 - (percent * 4), color = group,shape = group),
             size = 5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 18),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
sfkmir23_acc_atx

## south fork eel river @ standish hickey 2023

# need to add in 0's for TAC or TM data when there is none so bar stays 1/2 length
# 8/22, 8/28, 9/4, 9/12 need TM and 8/7 needs TM
atx_mod_sfksth23 <- rbind(anatoxins_list$sfkeel_sth_2023, # duplicate those dates on new df
              anatoxins_list$sfkeel_sth_2023[c(7, 11, 12, 13, 14),])
atx_mod_sfksth23[19:23,5:8] <- 0 # make ATX values for those all 0
atx_mod_sfksth23[19,4] <- "TM" # flip labels for those rows
atx_mod_sfksth23[20:23,4] <- "TAC"

sfksth23_acc_atx <- ggplot(data = accrual_list$sfkeel_sth_2023, aes(x = field_date, y = percent)) +
  geom_bar(data = atx_mod_sfksth23, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_afdm_g, fill = group), width = 5.5, color = "black") +
  geom_line(data = accrual_list$sfkeel_sth_2023, aes(y = 110 - (percent * 4), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = accrual_list$sfkeel_sth_2023, aes(y = 110 - (percent * 4), color = group,shape = group),
             size = 5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 18),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2023-06-20", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
sfksth23_acc_atx

## russian river 2022

# remove microcoleus entirely from accrual so we don't get a line for y = 0
acc_mod_rus22 <- accrual_list$russian_2022 %>% 
  filter(group == "anabaena_cylindrospermum")

# still want to keep atx bars 1/2 length
atx_mod_rus22 <- rbind(anatoxins_list$russian_2022, # duplicate those dates on new df
                       anatoxins_list$russian_2022[c(2:5),])
atx_mod_rus22[6:9,5:8] <- 0 # make ATX values for those all 0
atx_mod_rus22[6:9,4] <- "TM" # flip labels for those rows

rus22_acc_atx <- ggplot(data = acc_mod_rus22, aes(x = field_date, y = percent)) +
  geom_bar(data = atx_mod_rus22, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_afdm_g, fill = group), width = 6, color = "black") +
  geom_line(data = acc_mod_rus22, aes(y = 110 - (percent * 4), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = acc_mod_rus22, aes(y = 110 - (percent * 4), color = group,shape = group),
             size = 5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 18),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2022-06-24", "2022-09-16"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
rus22_acc_atx

## salmon river 2022

# remove A&C entirely from accrual so we don't get a line for y = 0
acc_mod_sal22 <- accrual_list$salmon_2022 %>% 
  filter(group == "microcoleus")

# add segment column to avoid line being drawn across plot when we weren't taking data
acc_mod_sal22$segment <- 1
acc_mod_sal22$segment[4] <- 2

# dataframe for rectangle bounds
wildfire1 <- data.frame(xmin = as.Date("2022-07-28"),
                        xmax = as.Date("2022-09-20"),
                        ymin = 0, 
                        ymax = 110)

# STILL NEEDS WILDFIRE RECTANGLE
sal22_acc_atx <- ggplot(data = acc_mod_sal22) +
  geom_bar(data = anatoxins_list$salmon_2022, position = "dodge", stat = "identity", 
           aes(x = field_date, y = mean_ATX_all_ug_afdm_g, fill = group), width = 6, color = "black") +
  geom_rect(data = wildfire1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ededed") +
  geom_line(data = acc_mod_sal22, aes(x = field_date, y = 110 - (percent * 4), group = segment, 
                                      color = group, linetype = group), linewidth = 2) +
  geom_point(data = acc_mod_sal22, aes(x = field_date, y = 110 - (percent * 4), color = group,
                                       shape = group),size = 5) +
  scale_color_manual("Group", values = c("#2871c7"),
                     labels = c("Microcoleus")) +
  scale_linetype_manual("Group", values = c("dashed"),
                        labels = c("Microcoleus")) +
  scale_shape_manual("Group", values = c(18),
                     labels = c("Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2022-06-26", "2022-09-23"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
sal22_acc_atx

## salmon river 2023

# remove A&C entirely from accrual so we don't get a line for y = 0
acc_mod_sal23 <- accrual_list$salmon_2023 %>% 
  filter(group == "microcoleus")

# dataframe for rectangle bounds
wildfire2 <- data.frame(xmin = as.Date("2023-08-12"),
                        xmax = as.Date("2023-09-24"),
                        ymin = 0, 
                        ymax = 110)

# needs rectangle for fire?
sal23_acc_atx <- ggplot(data = acc_mod_sal23) +
  geom_bar(data = anatoxins_list$salmon_2023, position = "dodge", stat = "identity", 
           aes(x = field_date, y = mean_ATX_all_ug_afdm_g, fill = group), width = 6, color = "black") +
  geom_rect(data = wildfire2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ededed") +
  geom_line(data = acc_mod_sal23, aes(x = field_date, y = 110 - (percent * 4), color = group, 
                                      linetype = group), linewidth = 2) +
  geom_point(data = acc_mod_sal23, aes(x = field_date, y = 110 - (percent * 4), color = group,
                                       shape = group),size = 5) +
  scale_color_manual("Group", values = c("#2871c7"),
                     labels = c("Microcoleus")) +
  scale_linetype_manual("Group", values = c("dashed"),
                        labels = c("Microcoleus")) +
  scale_shape_manual("Group", values = c(18),
                     labels = c("Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  scale_x_date(limits = as.Date(c("2023-06-25", "2023-09-25"))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
sal23_acc_atx

#### (4) Joining of figures ####

# use cowplot!!
sfkeel_mir_22 <- plot_grid(sfkmir22_GPP_dis, sfkmir22_acc_atx, nrow = 2, align = "hv")
sfkeel_mir_23 <- plot_grid(sfkmir23_GPP_dis, sfkmir23_acc_atx, nrow = 2, align = "hv")
sfkeel_sth_23 <- plot_grid(sfksth23_GPP_dis, sfksth23_acc_atx, nrow = 2, align = "hv")
russian_22 <- plot_grid(rus22_GPP_dis, rus22_acc_atx, nrow = 2, align = "hv")
salmon_22 <- plot_grid(sal22_GPP_dis, sal22_acc_atx, nrow = 2, align = "hv")
salmon_23 <- plot_grid(sal23_GPP_dis, sal23_acc_atx, nrow = 2, align = "hv")

left_side <- plot_grid(sfkeel_mir_22, sfkeel_mir_23, sfkeel_sth_23, nrow = 3, align = "hv")
right_side <- plot_grid(russian_22, salmon_22, salmon_23, nrow = 3, align = "hv")

both <- plot_grid(left_side, right_side, ncol = 2) # this will need some work

# good export size is 10.5 x 5.5 (maybe a bit taller- will test in a min)

#### (5) Miscellaneous to get legend for all items ####
metabolism_legend_data <- metabolism_list$sfkeel_mir_2023 %>% 
  mutate(class = "GPP")

# literally just cutting the scraps from viewing pdf in inkscape for now,
# but curious (to look into for future) if there is just a way to make a legend alone
sfkmir23_GPP_dis <- ggplot(data = metabolism_legend_data, aes(x = date_time)) +
  geom_area(data = discharge_list$sfkeel_mir_2023, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#BFDDFD") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct, fill = class), alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B" , size = 2.5, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2023-06-18 00:00:00", "2023-09-27 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  scale_fill_manual("GPP", values = c("#BFDDFD")) +
  #scale_color_manual("GPP", values = c("#456C2B")) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.text.x = element_blank(), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
        axis.ticks.length=unit(.25, "cm"))
sfkmir23_GPP_dis

sfkmir23_acc_atx <- ggplot(data = accrual_list$sfkeel_mir_2023, aes(x = field_date, y = percent)) +
  geom_bar(data = anatoxins_list$sfkeel_mir_2023, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_afdm_g, fill = group), color = "black", width = 6) +
  geom_line(data = accrual_list$sfkeel_mir_2023, aes(y = 110 - (percent * 4), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = accrual_list$sfkeel_mir_2023, aes(y = 110 - (percent * 4), color = group,shape = group),
             size = 5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 18),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group2", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"))
sfkmir23_acc_atx

test <- ggplot(data = wildfire2) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "lightgray")
