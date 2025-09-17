#### Supplemental figure to explore cover & ATX with biweekly 2022 data
### Jordan Zabrecky
## last edited: 09.16.2025

# This script creates a supplementary figure to explore the relationships
# between taxa-specific cover and ATX for biweekly 2022 data

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot", "gridExtra", "grid",
         "ggtext"), 
       require, character.only = T)

# loading in cover and anatoxins
data <- read.csv("./data/field_and_lab/allrivers22_combined.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum,
         TM_ATX_all_ug_g, TAC_ATX_all_ug_g)

# fill in ATX NAs with zero
data$TM_ATX_all_ug_g <- replace_na(data$TM_ATX_all_ug_g, 0)
data$TAC_ATX_all_ug_g <- replace_na(data$TAC_ATX_all_ug_g, 0)

# set universal theme for plots
# set universal theme for all plots
theme_set(theme_bw() + 
            theme(panel.grid.minor = element_blank(),
                  panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1),
                  text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                  strip.background = element_blank(),
                  plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  plot.title = element_markdown(size = 10, hjust = 0.5)))

# change 7/6 Russian sampling to 7/6 to match the other two 
# (was supposed to sample the first day but field day was taking too long so came back)
data$field_date[which(data$field_date == as.Date("2022-07-07"))] <- as.Date("2022-07-06")

# calculate mean behavior 
mean_data <- data %>% 
  dplyr::group_by(field_date, site) %>% 
  dplyr::summarize(mean_micro_cover = mean(microcoleus),
                   mean_anacyl_cover = mean(anabaena_cylindrospermum),
                   mean_TM_ATX = mean(TM_ATX_all_ug_g),
                   mean_TAC_ATX = mean(TAC_ATX_all_ug_g))
# order by site 
mean_data <- mean_data[order(mean_data$site),]

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
data[which(data$field_date == ymd("2022-09-15")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx")] <- NA
data[which(data$field_date == ymd("2022-09-17")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx")] <- NA
data[which(data$field_date == ymd("2022-09-22")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx")] <- NA
mean_data[which(mean_data$field_date == ymd("2022-09-15")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx")] <- NA
mean_data[which(mean_data$field_date == ymd("2022-09-17")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx")] <- NA
mean_data[which(mean_data$field_date == ymd("2022-09-22")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx")] <- NA

## anabaena/cylindrospermum

# south fork eel river
sfkeel_ana <- ggplot(data = data %>% filter(site == "SFE-M"), 
                     aes(anabaena_cylindrospermum, TAC_ATX_all_ug_g)) +
  geom_point(size = 2, color = "#fff77a") +
  geom_segment(data = data %>% filter(site == "SFE-M"), linewidth = 0.8, alpha = 0.8,
               aes(xend = end_anacyl_cover, yend = end_TAC_atx), color = "#fff77a",
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  geom_point(data = mean_data %>% filter(site == "SFE-M"), 
             aes(x = mean_anacyl_cover, y = mean_TAC_ATX), 
             color = "#8f8504", size = 2.5) +
  geom_segment(data = mean_data %>% filter(site == "SFE-M"), linewidth = 1, 
               color = "#8f8504",
               aes(x = mean_anacyl_cover,
                   y = mean_TAC_ATX,
                   xend = end_anacyl_cover,
                   yend = end_TAC_atx),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  labs(y = NULL, x = NULL, title = "Anabaena/Cylindrospermum") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
sfkeel_ana # why is color weird


rus_ana <- ggplot(data = data %>% filter(site == "RUS"),
                           aes(anabaena_cylindrospermum, TAC_ATX_all_ug_g)) +
  geom_point(size = 2, color = "#fff77a") +
  geom_segment(data = data %>% filter(site == "RUS"), linewidth = 0.8, alpha = 0.8,
               aes(xend = end_anacyl_cover, yend = end_TAC_atx), color = "#fff77a",
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  geom_point(data = mean_data %>% filter(site == "RUS"), 
             aes(x = mean_anacyl_cover, y = mean_TAC_ATX), 
             color = "#8f8504", size = 2.5) +
  geom_segment(data = mean_data %>% filter(site == "RUS"), linewidth = 1, 
               color = "#8f8504",
               aes(x = mean_anacyl_cover,
                   y = mean_TAC_ATX,
                   xend = end_anacyl_cover,
                   yend = end_TAC_atx),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  labs(y = NULL, x = NULL, title = "Anabaena/Cylindrospermum") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
rus_ana

## microcoleus

# south fork eel river
sfkeel_micro <- ggplot(data = data %>% filter(site == "SFE-M"), 
                       aes(microcoleus, TM_ATX_all_ug_g)) + 
     geom_point(size = 2, color = "#bad9ff") +
     geom_segment(data = data %>% filter(site == "SFE-M"), linewidth = 0.8, alpha = 0.8,
                  aes(xend = end_micro_cover,
                      yend = end_TM_atx),
                  color = "#bad9ff",
                  arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
     geom_point(data = mean_data %>% filter(site == "SFE-M"), aes(x = mean_micro_cover, y = mean_TM_ATX), 
                color = "#2871c7", size = 2.5) +
     geom_segment(data = mean_data %>% filter(site == "SFE-M"), linewidth = 1, color = "#2871c7",
                  aes(x = mean_micro_cover,
                      y = mean_TM_ATX,
                      xend = end_micro_cover,
                      yend = end_TM_atx),
                  arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
     labs(y = NULL, x = NULL, title = "Microcoleus") +
     scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
     theme(legend.position = "top")
sfkeel_micro

# salmon
sal_micro <- ggplot(data = data %>% filter(site == "SAL"), 
                    aes(microcoleus, TM_ATX_all_ug_g)) + 
  geom_point(size = 2, color = "#bad9ff") +
  geom_segment(data = data %>% filter(site == "SAL"), linewidth = 0.8, alpha = 0.8,
               aes(xend = end_micro_cover,
                   yend = end_TM_atx),
               color = "#bad9ff",
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  geom_point(data = mean_data %>% filter(site == "SAL"), aes(x = mean_micro_cover, y = mean_TM_ATX), 
             color = "#2871c7", size = 2.5) +
  geom_segment(data = mean_data %>% filter(site == "SAL"), linewidth = 1, color = "#2871c7",
               aes(x = mean_micro_cover,
                   y = mean_TM_ATX,
                   xend = end_micro_cover,
                   yend = end_TM_atx),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) +
  labs(y = NULL, x = NULL, title = "Microcoleus") +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
sal_micro

#### (3) Putting together plots and saving ####

# plot all
all <- plot_grid(sfkeel_micro, sfkeel_ana, scale = 0.92) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
all 
# dropping russian and salmon

# save figure
ggsave("./figures/sfig_cover_ATX_covary_2022_notfinal.tiff", dpi = 600, 
       width=16, height=9, unit="cm")
