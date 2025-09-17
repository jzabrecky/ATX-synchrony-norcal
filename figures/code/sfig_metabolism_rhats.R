#### Supplemental figure for r-hats for metabolism models
### Jordan Zabrecky
## last edited: 07.21.2025

## This script produces a figure displaying all r-hats for each metabolism
## parameter (GPP, ER, K600) and demonstrates that they are all below 1.05

#### (1) Loading data and and libraries ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), 
       require, character.only = T)

# load in metabolism data
metab <- ldply(list.files(path = "./data/metab_model_outputs_processed/", pattern = "_metab.csv"), function(filename) {
  d <- read.csv(paste("data/metab_model_outputs_processed/", filename, sep = ""))
  d$site = d$site_year %>% str_sub(end=-6)
  d$field_date = ymd(d$date)
  d$year = year(d$field_date)
  return(d)
})

# remove sites & years we don't need
sites <- c("sfkeel_mir_2022", "sfkeel_mir_2023", "russian_USGS_2022", "salmon_karuk_2022",
           "sfkeel_sth_2023")
metab <- metab %>% 
  filter(site_year %in% sites)

# lastly, reorganize to plot
metab_final <- metab %>% 
  select(field_date, site_year, site, GPP_Rhat, ER_Rhat, K600_daily_Rhat) %>% 
  pivot_longer(cols = c("GPP_Rhat", "ER_Rhat", "K600_daily_Rhat"),
                        names_to = "parameter", values_to = "r_hat") %>% 
  mutate(site_year)

#### (2) Making Figure ####

# add in factors to control ordering in ggplot
metab_final$parameter_f <- factor(metab_final$parameter, 
                                  levels=c("GPP_Rhat", "ER_Rhat", "K600_daily_Rhat"))
metab_final$site_year_f <- factor(metab_final$site_year, 
                                  levels = c("sfkeel_mir_2022", "sfkeel_mir_2023", 
                                             "sfkeel_sth_2023", "russian_USGS_2022", "salmon_karuk_2022"))

# figure
figure <- ggplot(data = metab_final, aes(x = field_date, y = r_hat)) +
  geom_point(alpha = 0.5) +
  facet_grid(rows = vars(parameter_f), 
             cols = vars(site_year_f), scales = "free_x",
             labeller =  as_labeller(c(`sfkeel_mir_2022` = "SFE-Lower 2022", 
                                       `sfkeel_mir_2023`= "SFE-Lower 2023",
                                       `sfkeel_sth_2023` = "SFE-Upper 2023",
                                       `russian_USGS_2022` = "RUS 2022",
                                       `salmon_karuk_2022` = "SAL 2022",
                                       `GPP_Rhat` = "GPP Daily R-hats",
                                       `ER_Rhat` = "ER Daily R-hats",
                                       `K600_daily_Rhat` = "*K~600~* Daily R-hats"))) +
  ylim(1.00, 1.06) +
  geom_hline(yintercept = 1.05, linetype = "dashed", color = "red", linewidth = 1.02)+
  theme_bw() +
  theme(strip.background = element_blank()) +
  labs(x = NULL, y = "R-hat") + 
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 10), axis.ticks.length=unit(.25, "cm")) +
  theme(strip.text = ggtext::element_markdown()) # to K600 fancy
figure

ggsave("./figures/sfig_metabolism_rhats.tiff", dpi = 600, 
       width=18, height=12, unit="cm")
