#### Using streamPULSE data to contextualize our GPP estimates
### Jordan Zabrecky
## 10.30.2025

## This script contextualizes our GPP values to streamPULSE data

#### (1) Loading libraries & data ####

# load libraries
library(tidyverse)

# read in our data
gpp_rus <- read.csv("./data/metab_model_outputs_processed/russian_USGS_2022_metab.csv")
gpp_sal <- read.csv("./data/metab_model_outputs_processed/salmon_karuk_2022_metab.csv")
gpp_sfk <- read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2022_metab.csv")
my_gpp <- rbind(gpp_rus, gpp_sal, gpp_sfk)

# read in Appling et al. 2018 data
appling <- read_tsv("./data/other_gpp/daily_predictions.tsv", col_names = TRUE)
site_data <- read_tsv("./data/other_gpp/site_data.tsv", col_names = TRUE)

# subset of sites used in Savoy et al. 2019
savoy <- read.csv('./data/other_gpp/savoy2019_sites.csv')

#### (2) Filter & Process data ####

# join in site_data, add in DOY, and filter for summer only & Savoy et al. 2019 sites
appling <- left_join(appling, site_data, by = c("site_name")) %>% 
  mutate(year = year(ymd(date)),
         DOY = yday(ymd(date))) %>% 
  # get days of year from June 22nd to September 24th
  filter(DOY > 173 & DOY < 267) %>% 
  filter(nwis_id %in% savoy$USGS_ID)

# what is their mean & max?
mean(appling$GPP) # 3.7
max(appling$GPP) # 58.5 which seems crazy

# add day of year to my gpp
my_gpp <- my_gpp %>% 
  mutate(DOY = yday(ymd(date)))

#### (3) Plot data ####

# plot
gpp_plot <- ggplot(data = appling, aes(x = DOY, y = GPP)) +
  geom_line(aes(group = interaction(nwis_id, year)), color = "gray", alpha = 0.7) +
  theme(legend.position = "none") +
  geom_line(data = my_gpp, aes(x = DOY, y = GPP.mean, color = site_year)) +
  ylim(0,25)
gpp_plot
