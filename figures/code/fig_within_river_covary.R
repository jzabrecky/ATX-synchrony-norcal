#### Primary figure to explore relationship between cover and ATX
### Jordan Zabrecky
## last edited: 08.12.2025

# This script creates a supplementary figure to explore the relationships
# between taxa-specific cover and ATX

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot"), 
       require, character.only = T)

# loading in cover, atx, and median GPP four days prior to field date
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum,
         TM_ATX_all_ug_g, TAC_ATX_all_ug_g, GPP_median_tofourdaysprior)

# fill in ATX NAs with zero
data$TM_ATX_all_ug_g <- replace_na(data$TM_ATX_all_ug_g, 0)
data$TAC_ATX_all_ug_g <- replace_na(data$TAC_ATX_all_ug_g, 0)

# calculate mean behavior 
mean_data <- data %>% 
  dplyr::group_by(field_date) %>% 
  dplyr::summarize(mean_micro_cover = mean(microcoleus),
                   mean_anacyl_cover = mean(anabaena_cylindrospermum),
                   mean_TM_ATX = mean(TM_ATX_all_ug_g),
                   mean_TAC_ATX = mean(TAC_ATX_all_ug_g))

# set universal theme for plots
theme_set(theme_bw() + 
            theme(legend.position = "bottom", 
                   panel.grid.minor = element_blank(),
                   panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1.5),
                   text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                   strip.background = element_blank(), plot.title = element_text(size=10)))

#### (2) Cover & ATX figures ####

# need to make manual dataframes so that two sites are not
# linked together in geom_segment
data$end_micro_cover <- c(data$microcoleus[-1], NA)
data$end_anacyl_cover <- c(data$anabaena_cylindrospermum[-1], NA)
data$end_TM_atx <-  c(data$TM_ATX_all_ug_g[-1], NA)
data$end_TAC_atx <- c(data$TAC_ATX_all_ug_g[-1], NA)

# do the same for mean data
mean_data$end_micro_cover <- c(mean_data$mean_micro_cover[-1], NA)
mean_data$end_anacyl_cover <- c(mean_data$mean_anacyl_cover[-1], NA)
mean_data$end_TM_atx <-  c(mean_data$mean_TM_ATX[-1], NA)
mean_data$end_TAC_atx <- c(mean_data$mean_TAC_ATX[-1], NA)

# add in NAs to cut the segment on last field days
data[which(data$field_date == ymd("2023-09-24")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx", "end_GPP")] <- NA
mean_data[which(mean_data$field_date == ymd("2023-09-24")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx", "end_GPP")] <- NA

## note: for these covary plots, the main figure will be the one with
## the mean behavior and each reach in a lighter color and the one with reach 
## reach in its own highlight will go to supplement

## anabaena/cylindrospermum

# all reaches
ana_cov_atx_reach <- ggplot(data = data, aes(anabaena_cylindrospermum, TAC_ATX_all_ug_g)) +
  geom_point(aes(color = site_reach), size = 2.5) +
  geom_segment(data = data, linewidth = 1, alpha = 0.8,
               aes(xend = end_anacyl_cover,
                   yend = end_TAC_atx,
                   color = site_reach),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) + 
  scale_color_manual("Reach:", values = c("#302d00", "#6b6404", "#8f8504", 
                                          "#c9be22", "#e8de48"),
                     labels = c("SFE-Lower-1S", "SFE-Lower-2", "SFE-Lower-3", 
                                "SFE-Lower-4", "SFE-Upper-1S"))+ 
  labs(y = NULL, x = NULL, title = "Anabaena/Cylindrospermum") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "right")
ana_cov_atx_reach
# have one odd reach out at the moment

# all reaches- mean behavior- option 1
ana_cov_atx_mean <- ggplot(data = data, aes(anabaena_cylindrospermum, TAC_ATX_all_ug_g)) +
  geom_point(size = 2, color = "#fff77a") +
  geom_segment(data = data, linewidth = 0.8, alpha = 0.8,
               aes(xend = end_anacyl_cover,
                   yend = end_TAC_atx),
               color = "#fff77a",
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  geom_point(data = mean_data, aes(x = mean_anacyl_cover, y = mean_TAC_ATX), 
             color = "#8f8504", size = 2.5) +
  geom_segment(data = mean_data, linewidth = 1, color = "#8f8504",
               aes(x = mean_anacyl_cover,
                   y = mean_TAC_ATX,
                   xend = end_anacyl_cover,
                   yend = end_TAC_atx),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  labs(y = NULL, x = NULL, title = "Anabaena/Cylindrospermum") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
ana_cov_atx_mean

## microcoleus

# all reaches
micro_cov_atx_reach <- ggplot(data = data, aes(microcoleus, TM_ATX_all_ug_g)) +
  geom_point(aes(color = site_reach), size = 2.5) +
  geom_segment(data = data,
               linewidth = 1, alpha = 0.8,
               aes(xend = end_micro_cover,
                   yend = end_TM_atx,
                   color = site_reach),
               arrow = arrow(length = unit(3, "mm"))) +
  scale_color_manual("Reach:", values = c("#0c223b", "#1e426b", "#2871c7", 
                                          "#689bd9", "#a6ceff"),
                     labels = c("SFE-Lower-1S", "SFE-Lower-2", "SFE-Lower-3", 
                                "SFE-Lower-4", "SFE-Upper-1S")) + 
  labs(y = NULL, x = NULL, title = "Microcoleus") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "right")
micro_cov_atx_reach

# all reaches- mean behavior- option 1
micro_cov_atx_mean <- ggplot(data = data, aes(microcoleus, TM_ATX_all_ug_g)) +
  geom_point(size = 2, color = "#bad9ff") +
  geom_segment(data = data, linewidth = 0.8, alpha = 0.8,
               aes(xend = end_micro_cover,
                   yend = end_TM_atx),
               color = "#bad9ff",
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  geom_point(data = mean_data, aes(x = mean_micro_cover, y = mean_TM_ATX), 
             color = "#2871c7", size = 2.5) +
  geom_segment(data = mean_data, linewidth = 1, color = "#2871c7",
               aes(x = mean_micro_cover,
                   y = mean_TM_ATX,
                   xend = end_micro_cover,
                   yend = end_TM_atx),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  labs(y = NULL, x = NULL, title = "Microcoleus") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
micro_cov_atx_mean

#### (3) Putting figures together & saving ####

# main figure
main <- plot_grid(micro_cov_atx_mean, ana_cov_atx_mean, ncol = 1,
                  align = "h")
main

ggsave("./figures/fig_cover_ATX_covary_notfinal.tiff", dpi = 600, 
       width=7.5, height=12, unit="cm") 

# supplemental figure
sup <- plot_grid(micro_cov_atx_reach, ana_cov_atx_reach, ncol = 1,
                 align = "v")
sup

ggsave("./figures/sfig_cover_ATX_covary_notfinal.tiff", dpi = 600, 
       width=12, height=14, unit="cm")
