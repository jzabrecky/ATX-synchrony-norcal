#### Large synchrony figure
### Jordan Zabrecky
## last edited 03.14.2025

## IN PROGRESS :)

# Large synchrony figure of gross primary productivity (GPP), % cover, and
# anatoxin concentrations for each river and year

# will likely separate GPP out

# NEEDS TO BE UPDATED WITH SALMON RIVER WHEN YOU FIGURE OUT THOSE ISSUES

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "cowplot"), 
       require, character.only = T)

## metabolism

# loading in metabolism data
metabolism <- ldply(list.files(path = "./data/metab_model_outputs_processed/", 
                               pattern = "_metab.csv"), function(filename) {
  d <- read.csv(paste("./data/metab_model_outputs_processed/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_metab.csv") # get site & year info
  d$site = d$site_year %>% str_sub(end=-6) # get site only info
  d$date = ymd(d$date) # convert date to date object
  return(d)
})

# split into a list based on site & year
metabolism_list <- split(metabolism, metabolism$site_year)

## discharge

# getting daily discharge data
USGS_gages <- c("11463000", "11522500", "11476500", "11475800") # USGS site numbers
param <- "00060" # mean daily discharge in cfs
discharge <- lapply(USGS_gages, function(x) readNWISdv(x, param, "2022-06-15","2023-10-01"))
site_names <- c("RUS", "SAL", "SFE_M", "SFE_SH") # site names
names(discharge) <- site_names # adding site names to discharge list

# function to clean discharge data frame
clean_discharge <- function(df) {
  new_df <- df %>% 
    mutate(discharge_m3_s = X_00060_00003 / 35.31) %>% 
    select(Date, discharge_m3_s)
  return(new_df)
}

# apply function to list of discharge dataframes
discharge <- lapply(discharge, function(x) clean_discharge(x))

## benthic cyanobacteria cover & anatoxins

# loading in data & mutating
# literally cannot figure out this data transformation atm....
cover <- read.csv("./data/field_and_lab/percover_bysite.csv") %>% 
  # calculate upper and lower sd (but lower bounds cannot go below 0)
  mutate(field_date = ymd(field_date),
         year = year(field_date),
         site_year = case_when(site == "RUS" ~ "russian_2022", # create site_year codes
                               site == "SFE-SH" ~ "sfkeel_sth_2022",
                               (site == "SFE-M_excl_site2" & year == 2022) ~ "sfkeel_mir_2022",
                               site == "SFE-M_all_sites" ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023")) %>% 
  filter(!is.na(site_year)) # will use the SFE-M for 2023 that uses all sites
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date),
         site = case_when(grepl("RUS", site_reach) ~ "RUS", # create site codes
                          grepl("SAL", site_reach) ~ "SAL",
                          grepl("SFE-M", site_reach) ~ "SFE-M",
                          grepl("SFE-SH", site_reach) ~ "SFE-SH"),
         site_year = case_when(site == "RUS" ~ "russian_2022", # create site_year codes
                               site == "SFE-SH" ~ "sfkeel_sth_2022",
                               (site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023"))

# separating cover into two dataframes and adding group label
cover_micro <- cover %>% 
  select(field_date, site_year, microcoleus_mean, microcoleus_sd) %>% 
  mutate(group = "Microcoleus") %>% 
  dplyr::rename(mean = microcoleus_mean,
         sd = microcoleus_sd)
cover_anacyl <- cover %>% 
  select(field_date, site_year, anabaena_cylindrospermum_mean, anabaena_cylindrospermum_sd) %>% 
  mutate(group = "Anabaena/Cylindrospermum") %>% 
  dplyr::rename(mean = anabaena_cylindrospermum_mean,
         sd = anabaena_cylindrospermum_sd)

# joining together dataframes and calculating max and min with mean and sd
cover_final <- rbind(cover_micro, cover_anacyl) %>% 
  mutate(max = mean + sd,
         min = case_when(mean - sd > 0 ~ mean - sd,
                         is.na(sd) ~ NA,
                          TRUE ~ 0))

# split dataframe into list of dataframes
cover_list <- split(cover_final, cover_final$site_year)

# treating 9-8-22 sample as if its 9-6-22
anatoxins$field_date[43] <- ymd("2022-09-06")

# summarize anatoxins by site
atx_summarized <- anatoxins %>% 
  dplyr::rename(group = sample_type) %>% 
  dplyr::group_by(site_year, field_date, group) %>% 
  dplyr::reframe(mean_ATX_all_ug_orgmat_g = mean(ATX_all_ug_orgmat_g),
                   sd_ATX_all_ug_orgmat_g = sd(ATX_all_ug_orgmat_g),
                 ATXa_ug_g_mean = mean(ATXa_ug_g),
                 dhATXa_ug_g_mean = mean(dhATXa_ug_g))# to potentially look at ratio

# split into list of dataframes
anatoxins_list <- split(atx_summarized, atx_summarized$site_year)

#### (2) Making metabolism & discharge figures ####

## set theme for all plots
theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                  panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 30), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm")))

## south fork eel @ miranda 2022

# add segment column to avoid ribbon being drawn across plot when we weren't taking data
metabolism_list$sfkeel_mir_2022$segment <- 1
metabolism_list$sfkeel_mir_2022$segment[14:nrow(metabolism_list$sfkeel_mir_2022)] <- 2

# figure
sfkmir22_GPP_dis <- ggplot(data = metabolism_list$sfkeel_mir_2022, aes(x = date)) +
  geom_area(data = discharge$SFE_M, aes(y = discharge_m3_s * 2, x = Date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct, group = segment),
              fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-29", "2022-09-20"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2)) +
  coord_cartesian(ylim = c(0, 9))
sfkmir22_GPP_dis

## south fork eel @ miranda 2023

# figure
sfkmir23_GPP_dis <- ggplot(data = metabolism_list$sfkeel_mir_2023, aes(x = date)) +
  geom_area(data = discharge$SFE_M, aes(y = discharge_m3_s * 1.4, x = Date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct), fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 1.4)) +
  coord_cartesian(ylim = c(0, 7)) +
  labs(y = NULL, x = NULL)
sfkmir23_GPP_dis

## south fork eel @ standish hickey 2023

# error bar for single points
error_bar <- metabolism_list$sfkeel_sth_2023[4:5,]
metabolism_list$sfkeel_sth_2023[4:5,4:5] <- NA

# add segment column to avoid ribbon being drawn across plot when we weren't taking data
metabolism_list$sfkeel_sth_2023$segment <- 1
metabolism_list$sfkeel_sth_2023$segment[4:5] <- 2
metabolism_list$sfkeel_sth_2023$segment[6:8] <- 3
metabolism_list$sfkeel_sth_2023$segment[9:nrow(metabolism_list$sfkeel_sth_2023)] <- 4

# figure
sfksth23_GPP_dis <- ggplot(data = metabolism_list$sfkeel_sth_2023, aes(x = date)) +
  geom_area(data = discharge$SFE_SH, aes(y = discharge_m3_s * 5, x = Date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct, group = segment),
              fill = "#9ced66", alpha = 0.8) +
  geom_linerange(data = error_bar, aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct),
                  alpha = 0.8, color = "#9ced66", linewidth = 1.5) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2023-06-20", "2023-09-27"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 5)) +
  coord_cartesian(ylim = c(0, 14)) +
  labs(y = NULL, x = NULL)
sfksth23_GPP_dis

## russian river 2022

# figure
rus22_GPP_dis <- ggplot(data = metabolism_list$russian_USGS_2022, aes(x = date)) +
  geom_area(data = discharge$RUS, aes(y = discharge_m3_s * 2.5, x = Date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct), fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2022-06-24 00:00:00", "2022-09-16 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 14)) +
  labs(y = NULL, x = NULL)
rus22_GPP_dis

## salmon river 2022

# figure- NEED TO SEE IF I CAN IMPROVE THIS DATA
sal22_GPP_dis <- ggplot(data = metabolism_list$salmon_karuk_2022, aes(x = date)) +
  geom_area(data = discharge$SAL, aes(y = discharge_m3_s * 0.5, x = Date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct), fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2022-06-26", "2022-09-23"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 0.5)) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(y = NULL, x = NULL)
sal22_GPP_dis

## salmon river 2023

# figure- again need to figure out problematic GPP here
sal23_GPP_dis <- ggplot(data = metabolism_list$salmon_karuk_2023, aes(x = date)) +
  geom_area(data = discharge$SAL, aes(y = discharge_m3_s * 0.3, x = Date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct), fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2023-06-25", "2023-09-25"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 0.3)) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(y = NULL, x = NULL)
sal23_GPP_dis

# need to get legend-- will have to clip from plots above
# and edit in 

#### (3) Making cover & anatoxin figures ####

## south fork eel @ miranda

# figure
sfkmir22_acc_atx <- ggplot(data = cover_list$sfkeel_mir_2022, aes(x = field_date)) +
  geom_bar(data = anatoxins_list$sfkeel_mir_2022, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_orgmat_g, fill = group), width = 6, color = "black") +
  geom_line(data = cover_list$sfkeel_mir_2022, aes(y = 110 - (mean * 4), color = group, linetype = group),
            linewidth = 2) +
  geom_errorbar(data = cover_list$sfkeel_mir_2022, aes(ymin = 110 - ((min) * 4),
                                                       ymax = 110 - ((max) * 4),
                                                       color = group), 
                size = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = cover_list$sfkeel_mir_2022, aes(y = 110 - (mean * 4), color = group ,shape = group),
             size = 5.5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2022-06-29", "2022-09-20"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  theme(legend.position = "none") # will move over legend via illustrator
sfkmir22_acc_atx

## south fork eel @ miranda 2023

# figure
sfkmir23_acc_atx <- ggplot(data = cover_list$sfkeel_mir_2023, aes(x = field_date)) +
  geom_bar(data = anatoxins_list$sfkeel_mir_2023, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_orgmat_g, fill = group), width = 5.5, color = "black") +
  geom_line(data = cover_list$sfkeel_mir_2023, aes(y = 110 - (mean * 4), color = group, linetype = group),
            linewidth = 2) +
  geom_errorbar(data = cover_list$sfkeel_mir_2023, aes(ymin = 110 - ((min) * 4),
                                                        ymax = 110 - ((max) * 4),
                                                        color = group), 
                 size = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = cover_list$sfkeel_mir_2023, aes(y = 110 - (mean * 4), color = group,shape = group),
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

# need to add in 0's for TAC or TM data when there is none so bar stays 1/2 length
# 8/22, 8/28, 9/4, 9/12 need TM and 8/7 needs TM
# no error bar here as it is one reach
atx_mod_sfksth23 <- rbind(anatoxins_list$sfkeel_sth_2022, # duplicate those dates on new df
                          anatoxins_list$sfkeel_sth_2022[c(1, 2, 7, 12, 13, 14),])
atx_mod_sfksth23[19:24,4] <- 0 # make ATX values for those all 0
atx_mod_sfksth23[c(19:20, 22:24),3] <- "TAC" # flip labels for those rows
atx_mod_sfksth23[21,3] <- "TM"

sfksth23_acc_atx <- ggplot(data = cover_list$sfkeel_sth_2022, aes(x = field_date)) +
  geom_bar(data = atx_mod_sfksth23, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_orgmat_g, fill = group), width = 5.5, color = "black") +
  geom_line(data = cover_list$sfkeel_sth_2022, aes(y = 40 - (mean * 3), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = cover_list$sfkeel_sth_2022, aes(y = 40 - (mean * 3), color = group,shape = group),
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
  ylim(0, 40) +
  scale_x_date(limits = as.Date(c("2023-06-20", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 40)/3) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
sfksth23_acc_atx

# remove microcoleus entirely from accrual so we don't get a line for y = 0
acc_mod_rus22 <- cover_list$russian_2022 %>% 
  filter(group == "Anabaena/Cylindrospermum")

# still want to keep atx bars 1/2 length
atx_mod_rus22 <- rbind(anatoxins_list$russian_2022, # duplicate those dates on new df
                       anatoxins_list$russian_2022)
atx_mod_rus22[6:10,4:5] <- 0 # make ATX values for those all 0
atx_mod_rus22[6:10,3] <- "TM" # flip labels for those rows

rus22_acc_atx <- ggplot(data = acc_mod_rus22, aes(x = field_date, y = mean)) +
  geom_bar(data = atx_mod_rus22, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_all_ug_orgmat_g, fill = group), width = 6, color = "black") +
  geom_line(data = acc_mod_rus22, aes(y = 10 - (mean * 1.25), color = group, linetype = group),
            linewidth = 2) +
  geom_errorbar(data = acc_mod_rus22, aes(ymin = 10 - ((min) * 1.5),
                                                       ymax = 10 - ((max) * 1.25),
                                                       color = group), 
                size = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = acc_mod_rus22, aes(y = 10 - (mean * 1.25), color = group,shape = group),
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
  ylim(0, 20) +
  scale_x_date(limits = as.Date(c("2022-06-24", "2022-09-16"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 10)/1.25) * -1)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
rus22_acc_atx

# remove A&C entirely from accrual so we don't get a line for y = 0
acc_mod_sal22 <- cover_list$salmon_2022 %>% 
  filter(group == "Microcoleus")

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
           aes(x = field_date, y = mean_ATX_all_ug_orgmat_g, fill = group), width = 6, color = "black") +
  geom_rect(data = wildfire1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ededed") +
  geom_line(data = acc_mod_sal22, aes(x = field_date, y = 110 - (mean * 4), group = segment, 
                                      color = group, linetype = group), linewidth = 2) +
  geom_errorbar(data = acc_mod_sal22, aes(x = field_date,
                                          ymin = 110 - ((min) * 4),
                                          ymax = 110 - ((max) * 4),
                                          color = group), 
                size = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = acc_mod_sal22, aes(x = field_date, y = 110 - (mean * 4), color = group,
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
acc_mod_sal23 <- cover_list$salmon_2023 %>% 
  filter(group == "Microcoleus")

# dataframe for rectangle bounds
wildfire2 <- data.frame(xmin = as.Date("2023-08-12"),
                        xmax = as.Date("2023-09-24"),
                        ymin = 0, 
                        ymax = 110)

# needs rectangle for fire?
sal23_acc_atx <- ggplot(data = acc_mod_sal23) +
  geom_bar(data = anatoxins_list$salmon_2023, position = "dodge", stat = "identity", 
           aes(x = field_date, y = mean_ATX_all_ug_orgmat_g, fill = group), width = 6, color = "black") +
  geom_rect(data = wildfire2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#ededed") +
  geom_line(data = acc_mod_sal23, aes(x = field_date, y = 110 - (mean * 6), color = group, 
                                      linetype = group), linewidth = 2) +
  geom_errorbar(data = acc_mod_sal23, aes(x = field_date,
                                          ymin = 110 - ((min) * 6),
                                          ymax = 110 - ((max) * 6),
                                          color = group), 
                size = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = acc_mod_sal23, aes(x = field_date, y = 110 - (mean * 6), color = group,
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
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/6) * -1)) +
  scale_x_date(limits = as.Date(c("2023-06-25", "2023-09-25"))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), plot.margin = unit(c(.3,0,0,0), "cm")) +
  theme(legend.position = "none") # will move over legend via illustrator
sal23_acc_atx










## joining of figures

# use cowplot!!
sfkeel_mir_22 <- plot_grid(sfkmir22_GPP_dis, sfkmir22_acc_atx, nrow = 2, align = "hv") +
sfkeel_mir_23 <- plot_grid(sfkmir23_GPP_dis, sfkmir23_acc_atx, nrow = 2, align = "hv")
sfkeel_sth_23 <- plot_grid(sfksth23_GPP_dis, sfksth23_acc_atx, nrow = 2, align = "hv")
russian_22 <- plot_grid(rus22_GPP_dis, rus22_acc_atx, nrow = 2, align = "hv")
salmon_22 <- plot_grid(sal22_GPP_dis, sal22_acc_atx, nrow = 2, align = "hv")
salmon_23 <- plot_grid(sal23_GPP_dis, sal23_acc_atx, nrow = 2, align = "hv")










### RANDOM-------------------------------------------------------------
# hysteresis loops

# anatoxins per taxa (TM and TAC have separate rows)
anatoxins_taxa <- split(atx_summarized, atx_summarized$group)
anatoxins_taxa$TAC <- anatoxins_taxa$TAC %>% 
  dplyr::rename(TAC_mean_ATX = mean_ATX_all_ug_orgmat_g,
         TAC_sd_ATX = sd_ATX_all_ug_orgmat_g,
         TAC_ATXa = ATXa_ug_g_mean,
         TAC_dhATXa = dhATXa_ug_g_mean) %>% 
  select(!group)
anatoxins_taxa$TM <- anatoxins_taxa$TM %>% 
  dplyr::rename(TM_mean_ATX = mean_ATX_all_ug_orgmat_g,
         TM_sd_ATX = sd_ATX_all_ug_orgmat_g,
         TM_ATXa = ATXa_ug_g_mean,
         TM_dhATXa = dhATXa_ug_g_mean) %>% 
  select(!group)

# join atx and cover data
hysteresis <- left_join(cover, anatoxins_taxa$TAC, by = c("field_date", "site_year"))
hysteresis <- left_join(hysteresis, anatoxins_taxa$TM, by = c("field_date", "site_year"))

# fill NA
hysteresis$TAC_mean_ATX <- replace_na(hysteresis$TAC_mean_ATX, 0)
hysteresis$TM_mean_ATX <- replace_na(hysteresis$TM_mean_ATX, 0)

# change site tag
hysteresis$site[which(hysteresis$site == "SFE-M_all_sites")] <- "SFE-M"
hysteresis$site[which(hysteresis$site == "SFE-M_excl_site2")] <- "SFE-M"

# all on one plot
hysteresis_plot_TM <- ggplot(data = hysteresis, aes(x = microcoleus_mean, y = TM_mean_ATX)) +
  geom_point(aes(color = site_year, shape = site_year), size = 3) +
  ggtitle("Microcoleus") #+
  #scale_y_log10()
hysteresis_plot_TM

hysteresis_plot_TAC <- ggplot(data = hysteresis, aes(x = anabaena_cylindrospermum_mean, y = TAC_mean_ATX)) +
  geom_point(aes(color = site_year, shape = site_year), size = 3) +
  ggtitle("Anabaena") +
  scale_y_log10()
hysteresis_plot_TAC

hysteresis <- hysteresis %>% 
  mutate(TM_atx_dhatx = TM_ATXa / TM_dhATXa,
         TAC_atx_dhatx = TAC_ATXa / TAC_dhATXa)

# all on one plot
hysteresis_plot_TM <- ggplot(data = hysteresis, aes(x = microcoleus_mean, y = TM_atx_dhatx)) +
  geom_point(aes(color = site_year, shape = site_year), size = 3) +
  ggtitle("Microcoleus")
hysteresis_plot_TM

hysteresis_plot_TAC <- ggplot(data = hysteresis, aes(x = anabaena_cylindrospermum_mean, y = TAC_atx_dhatx)) +
  geom_point(aes(color = site_year, shape = site_year), size = 3) +
  ggtitle("Anabaena")
hysteresis_plot_TAC

### plot of GPP vs. cover & anatoxins

metabolism_list$sfkeel_mir_2023 <- metabolism_list$sfkeel_mir_2023 %>% 
  dplyr::rename(field_date = date)

sfkeel_mir_23_cover <- left_join(cover_list$sfkeel_mir_2023, metabolism_list$sfkeel_mir_2023, by = "field_date")
sfkeel_mir_23_atx <- left_join(anatoxins_list$sfkeel_mir_2023, metabolism_list$sfkeel_mir_2023, by = "field_date")

sfkeel_mir_23_cover_m <- sfkeel_mir_23_cover %>% 
  filter(group == "Microcoleus")

# note that this is not the mean of four days like in modeling
ggplot(data = sfkeel_mir_23_cover, aes(x = GPP.mean, y = mean)) +
  geom_point() +
  facet_wrap(~group)
