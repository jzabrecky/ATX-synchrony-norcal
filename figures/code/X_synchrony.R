#### Figure X. Large synchrony figure of metabolism, accrual, and anatoxins
### Jordan Zabrecky
## last edited 09.24.2024

# This figure shows the gorss primary productivity, discharge, benthic cyanobacteria
# accrual, and corresponding anatoxin concentrations for each sensor placement with
# its affilitated upstream reaches incoporated

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval"), require, character.only = T)

# loading in metabolism data
metabolism <- ldply(list.files(path = "./data/prelim_metab_estimates/", pattern = "daily_est.csv"), function(filename) {
  d <- read.csv(paste("./data/prelim_metab_estimates/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_daily_est.csv")
  d$site = d$site_year %>% str_sub(end=-6)
  return(d)
})

# using discharge already downloaded in our model input csv
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

# split into a list by site
metabolism_list <- split(metabolism, metabolism$site_year)

## adjusting discharge dataset

# need to supplement discharge for salmon 2022 and russian 2022 because sensors were taken out before last day of sampling
rus_dis <- readNWISuv("11463000", "00060", "2022-09-01", "2022-09-17")
sal_dis <- readNWISuv("11522500", "00060", "2022-09-22", "2022-09-24")

# joining together temporarily
xtra_dis <- rbind(rus_dis, sal_dis)

# creating date_time column in PST
xtra_dis$date_time <- ymd_hms(xtra_dis$dateTime) # converting to POSIXct
xtra_dis$date_time <- with_tz(xtra_dis$date_time, tz = "America/Los_Angeles") # change from UTC to PST

# add site_year info and select columns we care about
xtra_dis <- xtra_dis %>% 
  mutate(site_year = case_when(site_no == "11463000" ~ "russian_2022",
                               site_no == "11522500" ~ "salmon_2022"),
         discharge_m3_s = X_00060_00000 / 35.31) %>% 
  select(site_year, date_time, discharge_m3_s)

# binding to discharge dataframe
discharge <- rbind(discharge, xtra_dis)

# cutting down discharge data as 5-minute intervals take forever to plot
discharge <- discharge %>%
  mutate(minute = minute(date_time)) %>% 
  filter(minute == 0 | minute == 15 | minute == 30 | minute == 45)

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

# split into a list by site
accrual_list <- split(accrual, accrual$site_year)

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

# calculate average and max per day at each site
atx_summarized <- anatoxins %>% 
  dplyr::group_by(site_year, field_date, site, sample_type) %>% 
  dplyr::summarize(mean_ATX_all_ug_chla_g = mean(ATX_all_ug_chla_g),
                   mean_ATX_all_ug_afdm_g = mean(ATX_all_ug_afdm_g),
                   max_ATX_all_ug_chla_g = max(ATX_all_ug_chla_g), # will probably not use max
                   max_ATX_all_ug_afdm_g = max(ATX_all_ug_afdm_g))

# split into a list by site
anatoxins_list <- split(atx_summarized, atx_summarized$site_year)

#### (3) Making Metabolism & Discharge Figures ####

# making them all separately so I can easily modify the scales on each separately

sfkmir22_GPP_dis <- ggplot(data = metabolism_list$sfkeel_mir_2022, aes(x = date_time)) +
  geom_area(data = discharge_list$sfkeel_mir_2022, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#a2cae8") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 3, alpha = 1) +
  labs(y = NULL, x = NULL) +
  scale_x_datetime(limits = as_datetime(c("2022-06-29 00:00:00", "2022-09-17 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.1), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 16))
sfkmir22_GPP_dis

# this one has proper second axis, but may adjust the range depending on model rerun TBD
# waiting to do others until that is complete
sfkmir23_GPP_dis <- ggplot(data = metabolism_list$sfkeel_mir_2023, aes(x = date_time)) +
  geom_area(data = discharge_list$sfkeel_mir_2023, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#a2cae8") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 3, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2023-06-18 00:00:00", "2023-09-25 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.1), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 16))
sfkmir23_GPP_dis

### NEED TO SEE MAX GPP OF NEW RUN
sfksth23_GPP_dis <- ggplot(data = metabolism_list$sfkeel_sth_2023, aes(x = date_time)) +
  geom_area(data = discharge_list$sfkeel_sth_2023, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#a2cae8") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 3, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2023-06-20 00:00:00", "2023-09-25 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.1), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 16))
sfksth23_GPP_dis

# NEEDS TO BE REDONE W/ USGS DATA
rus22_GPP_dis <- ggplot(data = metabolism_list$russian_2022, aes(x = date_time)) +
  geom_area(data = discharge_list$russian_2022, aes(y = discharge_m3_s * 2.5, x = date_time), fill = "#a2cae8") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 3, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2022-06-24 00:00:00", "2022-09-16 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 12)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.1), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 16))
rus22_GPP_dis

# NEEDS TO BE REDONE WITH KARUK DATA
# removed values where ribbon is below zero for now
metabolism_list$salmon_2022[c(34:35),c(3:13)] <- NA
sal22_GPP_dis <- ggplot(data = metabolism_list$salmon_2022, aes(x = date_time)) +
  geom_area(data = discharge_list$salmon_2022, aes(y = discharge_m3_s * 0.63, x = date_time), fill = "#a2cae8") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 3, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2022-06-26 00:00:00", "2022-09-23 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 0.63)) +
  coord_cartesian(ylim = c(0, 19)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.1), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 16)) # MAYBE WANT TO ADJUST TEXT SIZE
sal22_GPP_dis

# NEEDS TO BE REDONE WITH KARUK DATA
sal23_GPP_dis <- ggplot(data = metabolism_list$salmon_2023, aes(x = date_time)) +
  geom_area(data = discharge_list$salmon_2023, aes(y = discharge_m3_s * 0.63, x = date_time), fill = "#a2cae8") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 3, alpha = 1) +
  scale_x_datetime(limits = as_datetime(c("2023-06-25 00:00:00", "2023-09-25 00:00:00"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 0.63)) +
  coord_cartesian(ylim = c(0, 19)) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.1), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 16))
sal23_GPP_dis

# put all together into an object

#### (3) Making Accrual & Anatoxins Figures ####

# thoughts: try plot of bar graph upside down and then subtract from some number for percent cover 
# may have to half each so they don't overlap
# eg the full y scale length is 200 or 150 but anatoxins only goes up to 150 and then the remaining 50 is occupied by line
# will plot afdm version

# 101 max atx for afdm
# max 20 for percent cover

# start with upside down bar plot with max to 120
test <- ggplot(data = anatoxins_list$sfkeel_mir_2023, aes(x = field_date, y = mean_ATX_all_ug_afdm_g,
                                                          fill = sample_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  scale_y_reverse() +
  ylim(125, 0)
test
# need to add accrual subtracted by whatever I decide
# also need to decide on how much overlap between samples
# may involve flipping things around
# also fix color

# looking at kelly code
# Sample data
df <- data.frame(
  x = 1:10,
  y = rnorm(10, 50, 10),
  z = rnorm(10, 0.8, 0.1)
)
# Plot with secondary axis on the top (inverted second y-axis)
ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  scale_y_continuous(name = "Interruptions/day") +
  scale_x_continuous(name = "Time",
                     sec.axis = sec_axis(~.*5, name = "Productivity % of best",
                                         labels = function(b) { paste0(round(b * 100, 0), "%") })) +
  theme(axis.title.x.top = element_text(color = "blue"))

test_accrual <- accrual_list$sfkeel_mir_2023 %>% 
  select(field_date, microcoleus, anabaena)

test_accrual_long <- pivot_longer(test_accrual, cols = c(2:3), values_to = "percent", names_to = "sample_type")

test_atx <- atx_summarized %>% 
  filter(site_year == "sfkeel_mir_2023") %>% 
  dplyr::select(!c(max_ATX_all_ug_chla_g, max_ATX_all_ug_afdm_g)) %>% 
  mutate(sample_type = case_when(sample_type == "TM" ~ "microcoleus",
                                 sample_type == "TAC" ~ "anabaena")) # WILL NEED TO CHANGE NAME HERE!

# doing a single plot test using south fork eel miranda 2023
test <- ggplot(data = test_accrual_long, aes(x = field_date, y = percent, fill = sample_type, 
                                             color = sample_type, linetype = sample_type, shape = sample_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_fill_manual(values = c("#bdb000","#62a7f8")) +
  geom_area(aes(fill = sample_type, group = sample_type),
            alpha = 0.3, position = 'identity') +
  scale_color_manual(values = c("#bdb000","#62a7f8")) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  labs(y = "Percent Cover", x = NULL) +
  geom_bar(data = test_atx, aes(x = field_date, y = mean_ATX_all_ug_afdm_g, fill = sample_type), position = "dodge", 
           stat = "identity") +
  scale_y_reverse() +
  theme_bw()
test  

# can get upside down bar plot working :)
test_sep <- ggplot(data = test_atx, aes(x = field_date, y = mean_ATX_all_ug_afdm_g, fill = sample_type)) +
  geom_bar(position="dodge", stat="identity") +
  scale_y_reverse() +
  test_sep

barplot(height = mean_ATX_all_ug_afdm_g)
