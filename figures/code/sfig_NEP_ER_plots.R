#### Supplemental figure showing NEP estimates
### Jordan Zabrecky
## last edited: 04.23.2026

# This code makes a supplemental figure showing ER and NEP estimates 

#### (1) Loading libraries ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), 
       require, character.only = T)

# loading metabolism data
metab <- ldply(c("sfkeel_mir_2022", "sfkeel_mir_2023", "salmon_karuk_2022", "sfkeel_sth_2023", "russian_USGS_2022"), 
      function(x) {
  df = read.csv(paste("./data/metab_model_outputs_processed/", x, "_metab.csv", sep = "")) %>% 
    mutate(date = ymd(date))
  return(df)
})

#### (2) Making Figures ####

# set theme for all plots
theme_set(theme_bw() + theme(legend.position = "bottom", 
                             panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                             panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.text = element_text(size = 10), title = element_text(size = 10),
                             strip.background = element_blank(), strip.text = element_text(size = 10)))

# factor site year for desired order
metab <- metab %>% 
  mutate(site_year_factor = factor(site_year, levels = c("sfkeel_mir_2022", "sfkeel_mir_2023",
                                                         "russian_USGS_2022", "sfkeel_sth_2023",
                                                         "salmon_karuk_2022")))

# add segments for skeel_mir_2022 & sfkeel_sth_2023
metab <- metab %>% 
  mutate(segment = case_when(site_year == "sfkeel_mir_2022" & date > as.Date("2022-07-13") ~ 2,
                             site_year == "sfkeel_sth_2023" & date > as.Date("2023-06-30") &
                               date < as.Date("2023-07-05") ~ 2,
                             site_year == "sfkeel_sth_2023" & date > as.Date("2023-07-05") &
                               date < as.Date("2023-07-14") ~ 3,
                             site_year == "sfkeel_sth_2023" & date > as.Date("2023-07-14") &
                               date < as.Date("2023-07-30") ~ 4, 
                             site_year == "sfkeel_sth_2023" & date > as.Date("2023-07-30") ~ 5,
                             TRUE ~ 1))

# make plot
figure <- ggplot(data = metab, aes(x = date)) +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct, group = segment),
              fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 1.2, alpha = 1) +
  geom_ribbon(aes(ymin = ER.2.5.pct, ymax = ER.97.5.pct, group = segment),
              fill = "#E8BCBA", alpha = 0.8) +
  geom_point(aes(y = ER.mean), color = "#B3423D", size = 1.2, alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(x = date, ymin = NEP.2.5.pct, ymax = NEP.97.5.pct, group = segment),
              fill = "#CCCCCC", alpha = 0.8) +
  geom_point(aes(x = date, y = NEP.mean), 
             color = "#787878", size = 1.2, alpha = 1)  +
  facet_wrap(~site_year_factor, scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL)
figure  

# save
ggsave("./figures/sfig_nep_er_gpp.tiff", dpi = 600, 
       width=17.5, height=18, unit="cm") 

#### (3) Investigating range of values

# calculate ranges
ranges <- metab %>% 
  dplyr::group_by(site_year) %>% 
  dplyr::summarize(max_GPP = max(GPP.mean),
                   min_GPP = min(GPP.mean),
                   max_NEP = max(NEP.mean),
                   min_NEP = min(NEP.mean),
                   GPP_range = max_GPP - min_GPP,
                   NEP_range = max_NEP - min_NEP) %>% 
  ungroup() %>% 
  mutate(GPP_greater = eval(GPP_range > NEP_range))
view(ranges)
# not true for Russian USGS 2022 nor south fork Eel Miranda 2023
# but for SFE-Miranda 2023 we seem to have a big NEP outlier after rains
# otherwise min NEP is ~ -2.7
eval(ranges$GPP_range[which(ranges$site_year == "sfkeel_mir_2023")] >
       ranges$max_NEP[which(ranges$site_year == "sfkeel_mir_2023")] - -2.7)
# removing that outlier, GPP range is greater!
