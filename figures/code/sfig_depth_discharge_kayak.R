#### Kayak Measured depths on Log Discharge-Log Depth plots
### Jordan Zabrecky
## last edited: 02.04.2025

# This figure shows our depth-discharge relationships for each river
# and plots our kayak measurements over it

#### (1) Loading libraries and data ####

# use work from script that builds depth discharge relationships
source("code/1g_processing_metabolism_outputs.R")

# adding site_name column for discharge
site_names <- c("russian", "salmon", "sfkeel_mir", "sfkeel_sth")
for(i in 1:length(USGS_daily_discharge)) {
  USGS_daily_discharge[[i]]$site <- site_names[i]
}

# filtering out dates for discharge
USGS_daily_discharge$sfkeel_mir <- USGS_daily_discharge$sfkeel_mir %>% 
  filter(date >= "2023-06-15" & date <= "2023-09-27")
USGS_daily_discharge$sfkeel_sth <- USGS_daily_discharge$sfkeel_sth %>% 
  filter(date >= "2023-06-15" & date <= "2023-09-27")
USGS_daily_discharge$russian <- USGS_daily_discharge$russian %>% 
  filter(discharge_m3_s < 6) %>% # have a kayak measurement at discharge higher
  filter(date >= "2023-01-01") # than the given dates
# may debate leaving at range of sampling discharge
USGS_daily_discharge$salmon <- USGS_daily_discharge$salmon %>% 
  filter(date >= "2023-06-15" & date <= "2023-09-27")

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

figure <- ggplot() +
  geom_point(data = discharge_all,
             aes(x = discharge_m3_s, y = depth_m), color = "skyblue", size = 3) +
  geom_point(data = kayak,
             aes(x = discharge_m3_s, y = mean_depth_m, fill = site,
                 shape = site), color = "black", size = 4, alpha = 0.9, stroke = 1.2) +
  facet_wrap(~site, scales = "free",
             labeller = as_labeller(c(`russian` = "Russian River (RUS)", 
                                      `salmon`= "Salmon River (SAL)",
                                      `sfkeel_mir` = "South Fork Eel River at Miranda (SFE-M)",
                                      `sfkeel_sth` = "South Fork Eel River at Standish Hickey (SFE-SH)"))) +
  scale_fill_manual(values = c("#bdb000", "blue", "#416f16", "#a8ff82")) +
  #scale_color_manual(values = c("#7d7400", "darkblue", "#32590d", "#71d446")) +
  scale_shape_manual(values = c(24, 21, 22, 23)) +
  labs(x = expression("Discharge (m"^3~"s"^-1*")"), y = "Depth (m)") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1.2),
        text = element_text(size = 20), axis.ticks.length=unit(.25, "cm"))
figure