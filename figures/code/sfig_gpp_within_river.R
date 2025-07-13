#### Primary figure for taxa-specific cover & anatoxins and GPP on each river
### Jordan Zabrecky
## last edited: 06.28.2025

# This script creates a supplemental figure showing the difference
# of magnitudes of GPP within the river

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot", "gridExtra", "grid"), 
       require, character.only = T)

## gpp data
gpp <- rbind(read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2023_metab.csv"),
             read.csv("./data/metab_model_outputs_processed/sfkeel_sth_2023_metab.csv")) %>% 
  mutate(date = ymd(date))

## discharge data
disc <- rbind(read.csv("./data/USGS/sfkeel_mir_discharge_daily.csv"),
              read.csv("./data/USGS/sfkeel_sth_discharge_daily.csv")) %>% 
  mutate(date = ymd(date),
         site_year = case_when(site == "sfkeel_mir" ~ "sfkeel_mir_2023",
                               TRUE ~ "sfkeel_sth_2023"))

#### (2) Making Figure ####

# add segment column to avoid ribbon being drawn across plot when we weren't taking data
gpp$segment <- 1
gpp$segment[104:105] <- 2
gpp$segment[106:108] <- 3
gpp$segment[109:nrow(gpp)] <- 4

# gpp plots -- may need to separate out to put together panels
gpp_plots <- ggplot(data = gpp, aes(x = date)) +
  geom_area(data = disc, aes(y = discharge_m3_s * 2.5), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct, group = segment),
              fill = "#9ced66", alpha = 0.8) +
  geom_linerange(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct),
                 alpha = 0.8, color = "#9ced66", linewidth = 1.5) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2023-06-20", "2023-09-27"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  facet_wrap(~site_year, ncol = 1) +
  coord_cartesian(ylim = c(0, 13.6)) +
  labs(y = NULL, x = NULL)
gpp_plots