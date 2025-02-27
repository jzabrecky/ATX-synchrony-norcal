#### GPP figure
### Jordan Zabrecky
## last edited 02.27.2025

# figure for Q2


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

# get day of year
metabolism$doy <- yday(metabolism$date)

# don't care about our miniDOT for russian & salmon bc using external data
metabolism <- metabolism %>% 
  filter(site_year != "russian_2022" & site_year != "salmon_2022" & site_year != "salmon_2023")

## attempt at figure
fig <- ggplot(metabolism, aes(x = doy, y = GPP.mean)) +
  geom_point(aes(fill = site_year, shape = site, color = site_year), size = 3, alpha = 0.5) +
  scale_fill_manual(values = c("#bdb000", "#62a7f8", "#009186", "#416f16", 
                                "#809509","#a8ff82")) +
  scale_color_manual(values = c("#bdb000", "#62a7f8", "#009186", "#416f16", 
                               "#809509","#a8ff82")) +
  scale_shape_manual(values = c(24, 21, 22, 23)) +
  facet_wrap(~site)
fig
