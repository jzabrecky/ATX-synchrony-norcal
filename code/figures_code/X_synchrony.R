#### Figure X. Large synchrony figure of metabolism, accrual, and anatoxins
### Jordan Zabrecky
## last edited 09.19.2024

# This figure shows the metabolism modelled for all sites
# and the <AVERAGED OR MAX> percent cover and anatoxin concentrations blah blah

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

# loading in metabolism data as one data-frame for now
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
  return(d)
})

# loading in benthic cyanobacteria data
accrual <- read.csv("./data/field_and_lab/percover_bysite.csv")
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")

# applying lubridate
metabolism$date <- ymd(metabolism$date)
discharge$date_time <- ymd_hms(discharge$date_time)
accrual$field_date <- ymd(accrual$field_date)
anatoxins$field_date <- ymd(anatoxins$field_date)

#### (2) Processing data to make figures ####

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

# also adding this information to anatoxins dataframe
anatoxins <- anatoxins %>% 
  mutate(year = year(field_date)) %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

# cutting down metabolism data (this code may get moved to modeling latent biomass code)
metabolism <- metabolism %>% 
  select(site_year, date, GPP_mean, GPP_sd, GPP_2.5pct, GPP_97.5pct, ER_mean, ER_sd, ER_2.5pct, ER_97.5pct) %>% 
  mutate(NEP_mean = GPP_mean + ER_mean, # adding because ER is negative
         NEP_2.5pct = GPP_2.5pct + ER_2.5pct,
         NEP_97.5pct = GPP_97.5pct + ER_97.5pct,
         date_time = as.POSIXct(date))
# probably have to convert date to date time in some way
# also have times with positive ER for the Salmon which need to get filtered out for NEP figure

# cutting down discharge data, 5-minute intervals take forever to plot
discharge <- discharge %>%
  select(site_year, date_time, discharge_m3_s) %>% 
  mutate(minute = minute(date_time)) %>% 
  filter(minute == 0 | minute == 30)

# process anatoxin data by calculating average per day
atx_summarized <- anatoxins %>% 
  dplyr::group_by(site_year, field_date, site, sample_type) %>% 
  dplyr::summarize(mean_ATX_all_ug_chla_g = mean(ATX_all_ug_chla_g),
            mean_ATX_all_ug_afdm_g = mean(ATX_all_ug_afdm_g),
            max_ATX_all_ug_chla_g = max(ATX_all_ug_chla_g),
            max_ATX_all_ug_afdm_g = max(ATX_all_ug_afdm_g))

# include list just incase
accrual_list <- split(accrual, accrual$site_year)

#### (3) Making Figures ####

## HOW DOES IT MAKE SENSE TO STRUCTURE THIS? AS INDIVIDUAL OR FACET WRAP

## metabolism figure w/ discharge
metab_dis_plot <- ggplot(data = metabolism, aes(x = date_time)) +
  geom_area(data = discharge, aes(y = discharge_m3_s, x = date_time), fill = "#a2cae8") +
  geom_ribbon(aes(ymin = GPP_2.5pct, ymax = GPP_97.5pct), fill = "#BEFB96", alpha = 0.8) +
  geom_point(aes(y = GPP_mean), color = "#456C2B", size = 3, alpha = 1) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  facet_wrap(~site_year, scales ="free")
metab_dis_plot

# need to have individual scale per discharge of each and need to create 2nd axis

# test discharge separately....
dis_plot <- ggplot(data = discharge, aes(x = date_time, y = discharge_m3_s)) +
  geom_area(fill = "lightblue") +
  facet_wrap(~site_year, scales = "free")
dis_plot

# other to-do:
# -make not ugly
# -fix date issue for russian 2022
# -names
# -order of panels
# -also connecting with the second plot

# briefly, removing periods in data where ER is positive
NEP_metabolism <- metabolism %>% 
  filter(ER_mean <= 0) # remove where ER is positive

## NEP figure w/ discharge
NEP_dis_plot <- ggplot(data = NEP_metabolism, aes(x = date)) +
  geom_ribbon(aes(ymin = NEP_2.5pct, ymax = NEP_97.5pct), fill = "lightgray", alpha = 0.8) +
  geom_point(aes(y = NEP_mean), color = "darkgray", size = 3, alpha = 1) +
  geom_hline(yintercept = 0, lwd = 1.5, color = "black", linetype = 2) +
  labs(y = NULL, x = NULL) +
  theme_bw() +
  facet_wrap(~site_year, scales ="free") +
NEP_dis_plot

## Accrual & anatoxins plot
test <- 