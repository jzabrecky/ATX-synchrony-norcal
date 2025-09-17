#### Kayak Measured depths on Log Discharge-Log Depth plots
### Jordan Zabrecky
## last edited: 07.21.2025

# This figure shows our depth-discharge relationships for each river
# and plots our kayak measurements over it

#### (1) Loading libraries and data ####

# use work from script that builds depth discharge relationships
source("code/1f_processing_metabolism_outputs.R")

# adding site_name column for discharge
site_names <- c("russian", "salmon", "sfkeel_mir", "sfkeel_sth")
for(i in 1:length(USGS_daily_discharge)) {
  USGS_daily_discharge[[i]]$site <- site_names[i]
}

# filtering out dates for discharge
USGS_daily_discharge$sfkeel_mir <- USGS_daily_discharge$sfkeel_mir %>% 
  dplyr::filter(date >= "2023-06-15" & date <= "2023-09-27")
USGS_daily_discharge$sfkeel_sth <- USGS_daily_discharge$sfkeel_sth %>% 
  dplyr::filter(date >= "2023-06-15" & date <= "2023-09-27")
USGS_daily_discharge$russian <- USGS_daily_discharge$russian %>% 
  dplyr::filter(discharge_m3_s < 6) %>% # have a kayak measurement at discharge higher
  dplyr::filter(date >= "2023-01-01") # than the given dates
# may debate leaving at range of sampling discharge
USGS_daily_discharge$salmon <- USGS_daily_discharge$salmon %>% 
  dplyr::filter(date >= "2023-06-15" & date <= "2023-09-27")

# combining discharge to one data frame to use facet wrap in ggplot
discharge_all <- rbind(USGS_daily_discharge$russian, USGS_daily_discharge$salmon,
                       USGS_daily_discharge$sfkeel_mir, USGS_daily_discharge$sfkeel_sth) %>% 
  dplyr::rename(field_date = date)

# add site identifier to kayak dataframes
kayak_miranda$site <- "sfkeel_mir"
kayak_standish$site <- "sfkeel_sth"
kayak_russian$site <- "russian"
kayak_salmon$site <- "salmon"

# join kayak data into one dataframe
kayak <- rbind(kayak_miranda, kayak_standish, kayak_russian, kayak_salmon)

#### (2) Making figure ####

# use factoring to control order of sites in ggplot
kayak$site_f <- factor(kayak$site, levels = c("sfkeel_mir", "sfkeel_sth",
                                              "russian", "salmon"))
discharge_all$site_f <- factor(discharge_all$site, levels = c("sfkeel_mir",
                                                              "sfkeel_sth",
                                                              "russian",
                                                              "salmon"))

# make figure
figure <- ggplot() +
  geom_point(data = discharge_all,
             aes(x = discharge_m3_s, y = depth_m), color = "skyblue", size = 3) +
  geom_point(data = kayak,
             aes(x = discharge_m3_s, y = mean_depth_m, fill = site), 
             color = "black", size = 4, alpha = 0.9, stroke = 1.2, shape = 21) +
  facet_wrap(~site_f, scales = "free",
             labeller = as_labeller(c(`russian` = "Russian River (RUS)", 
                                      `salmon`= "Salmon River (SAL)",
                                      `sfkeel_mir` = "South Fork Eel River Lower (SFE-Lower)",
                                      `sfkeel_sth` = "South Fork Eel River Upper (SFE-Upper)"))) +
  scale_fill_manual(values = c("#bdb000", "#62a7f8", "#416f16", "#a8ff82")) +
  #scale_color_manual(values = c("#7d7400", "darkblue", "#32590d", "#71d446")) +
  #scale_shape_manual(values = c(24, 21, 22, 23)) +
  labs(x = expression("Discharge (m"^3~"s"^-1*")"), y = "Depth (m)") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
        text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"))
figure

# save figure
ggsave("./figures/sfig_kayak_depths_notfinal.tiff", dpi = 600, 
       width=18, height=12, unit="cm")
