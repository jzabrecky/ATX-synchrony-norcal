#### Primary figure for taxa-specific cover & anatoxins and GPP on each river
### Jordan Zabrecky
## last edited: 08.06.2025

# This script creates a supplemental figure showing the difference
# of magnitudes of GPP within the river

# figure out why GPP ribbon is weird..?>?>

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot"), 
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

# to calculate maximum for site for lines
cover <- read.csv("./data/field_and_lab/percover_bysite.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date)) %>% 
  filter(year == 2023)
cover_list <- split(cover, cover$site)
atx <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  select(field_date, site_reach, site, TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g) %>% 
  mutate(field_date = ymd(field_date),
         TM_ATX_all_ug_orgmat_g = case_when(is.na(TM_ATX_all_ug_orgmat_g) ~ 0,
                                            TRUE ~ TM_ATX_all_ug_orgmat_g),
         TAC_ATX_all_ug_orgmat_g = case_when(is.na(TAC_ATX_all_ug_orgmat_g) ~ 0,
                                             TRUE ~ TAC_ATX_all_ug_orgmat_g)) %>% 
  dplyr::group_by(site, field_date) %>% 
  dplyr::summarize(mean_TM_atx = mean(TM_ATX_all_ug_orgmat_g),
                   mean_TAC_atx = mean(TAC_ATX_all_ug_orgmat_g))
atx_list <- split(atx, atx$site)

#### (2) Making Figures ####


# set theme for all plots
theme_set(theme_bw() + theme(legend.position = "bottom", 
                             panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                             panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1.5),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.text = element_text(size = 10), title = element_text(size = 10)))

# add segment column to avoid ribbon being drawn across plot when we weren't taking data
gpp$segment <- 1
gpp$segment[104:105] <- 2
gpp$segment[106:108] <- 3
gpp$segment[109:nrow(gpp)] <- 4

# dates for taxa-specific cover and anatoxin
mir_ac <- cover_list$`SFE-M_all_sites`$field_date[which(cover_list$`SFE-M_all_sites`$anabaena_cylindrospermum_mean 
                                               == max(cover_list$`SFE-M_all_sites`$anabaena_cylindrospermum_mean))]
mir_m <- cover_list$`SFE-M_all_sites`$field_date[which(cover_list$`SFE-M_all_sites`$microcoleus_mean 
                                              == max(cover_list$`SFE-M_all_sites`$microcoleus_mean))]
mir_ac_atx <- atx_list$`SFE-M`$field_date[which(atx_list$`SFE-M`$mean_TAC_atx == 
                                                   max(atx_list$`SFE-M`$mean_TAC_atx))]
mir_m_atx <- atx_list$`SFE-M`$field_date[which(atx_list$`SFE-M`$mean_TM_atx == 
                                                  max(atx_list$`SFE-M`$mean_TM_atx))]

# upper/miranda GPP
mir_gpp <- ggplot(data = gpp %>% filter(site_year == "sfkeel_mir_2023"), aes(x = date)) +
  geom_area(data = disc %>% filter(site_year == "sfkeel_mir_2023"), 
            aes(y = discharge_m3_s * 2), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct),
              fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 1.2, alpha = 1) +
  geom_vline(xintercept = mir_ac, color = "#8f8504", linewidth = 2, alpha = 1) +
  geom_vline(xintercept = mir_m, color = "#2871c7", linewidth = 2, alpha = 1) +
  geom_vline(xintercept = mir_ac_atx, color = "#d1c960", linewidth = 2, alpha = 1,
             linetype = "dashed") +
  geom_vline(xintercept = mir_m_atx, color = "#5a88bf", linewidth = 2, alpha = 1,
             linetype = "dashed") +
  scale_x_date(limits = as.Date(c("2023-06-20", "2023-09-27"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2)) +
  coord_cartesian(ylim = c(0, 10)) +
  labs(y = NULL, x = NULL, title = "South Fork Eel Upper")
mir_gpp

## standish hickey/lower

# dates for taxa-specific cover and anatoxin
sth_ac <- cover_list$`SFE-SH`$field_date[which(cover_list$`SFE-SH`$anabaena_cylindrospermum_mean 
                                               == max(cover_list$`SFE-SH`$anabaena_cylindrospermum_mean))]
sth_m <- cover_list$`SFE-SH`$field_date[which(cover_list$`SFE-SH`$microcoleus_mean 
                                              == max(cover_list$`SFE-SH`$microcoleus_mean))]
sth_ac_atx <- atx_list$`SFE-SH`$field_date[which(atx_list$`SFE-SH`$mean_TAC_atx == 
                                                   max(atx_list$`SFE-SH`$mean_TAC_atx))]
sth_m_atx <- atx_list$`SFE-SH`$field_date[which(atx_list$`SFE-SH`$mean_TM_atx == 
                                                  max(atx_list$`SFE-SH`$mean_TM_atx))]

# lower/standish hickey GPP
sth_gpp <- ggplot(data = gpp %>% filter(site_year == "sfkeel_sth_2023"), aes(x = date)) +
  geom_area(data = disc %>% filter(site_year == "sfkeel_sth_2023"), 
            aes(y = discharge_m3_s * 2.5), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct, group = segment),
              fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 1.2, alpha = 1) +
  geom_vline(xintercept = sth_ac, color = "#8f8504", linewidth = 2, alpha = 1) +
  geom_vline(xintercept = sth_m, color = "#2871c7", linewidth = 2, alpha = 1) +
  geom_vline(xintercept = sth_ac_atx, color = "#d1c960", linewidth = 2, alpha = 1,
             linetype = "dashed") +
  geom_vline(xintercept = sth_m_atx, color = "#5a88bf", linewidth = 2, alpha = 1,
             linetype = "dashed") +
  scale_x_date(limits = as.Date(c("2023-06-20", "2023-09-27"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 13.6)) +
  labs(y = NULL, x = NULL, title = "South Fork Eel Lower")
sth_gpp

#### (3) Putting plots together & saving ####

# putting two plots together
all <- plot_grid(mir_gpp, sth_gpp, align = "v", ncol = 1)
all

# saving
ggsave("./figures/sfig_gpp_sfkeel_2023_notfinal.tiff", dpi = 600, 
       width=12, height=10, unit="cm") 
