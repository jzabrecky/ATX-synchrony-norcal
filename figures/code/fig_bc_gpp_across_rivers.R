#### Primary figure for taxa-specific cover & anatoxins and GPP on each river
### Jordan Zabrecky
## last edited: 06.10.2025

# This script creates a primary figure for Q1 focused on the relationships
# between benthic cyanobacteria dynamics and GPP across rivers

# IN PROGRESS- still trying to determine final symbology for presence

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot", "gridExtra", "grid"), 
       require, character.only = T)

## loading in data

## percent cover data
cover <- read.csv("./data/field_and_lab/percover_bysite.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date),
         microcoleus_present = 
           case_when(proportion_micro_transects > 0 ~ "yes",
                     TRUE ~ "no"),
         ana_cyl_present = 
           case_when(proportion_ana_cyl_transects > 0 ~ "yes",
                     TRUE ~ "no")) %>% 
  filter(year == 2022) %>% 
  select(field_date, site, microcoleus_mean, microcoleus_sd, microcoleus_present,
         anabaena_cylindrospermum_mean, anabaena_cylindrospermum_sd, ana_cyl_present) %>% 
  mutate(site = case_when(site == "SFE-M_excl_site2" ~ "SFE-M",
                          TRUE ~ site))

# separating instead of mutating longer twice...
anacyl <- cover %>% 
  select(field_date, site, anabaena_cylindrospermum_mean, anabaena_cylindrospermum_sd,
         ana_cyl_present) %>% 
  dplyr::rename(mean = anabaena_cylindrospermum_mean,
                sd = anabaena_cylindrospermum_sd,
                present = ana_cyl_present) %>% 
  mutate(taxa = "anabaena_cylindrospermum")
microcoleus <- cover %>% 
  select(field_date, site, microcoleus_mean, microcoleus_sd, microcoleus_present) %>% 
  dplyr::rename(mean = microcoleus_mean,
                sd = microcoleus_sd,
                present = microcoleus_present) %>% 
  mutate(taxa = "microcoleus")

# joining back together cover data and calculating +/- 1 sd
cover <- rbind(anacyl, microcoleus) %>% 
  mutate(max = mean + sd,
         min = case_when(mean - sd > 0 ~ mean - sd,
                         is.na(sd) ~ NA,
                         TRUE ~ 0),
         # column to indicate it is present and is captured by quadrat survey
         quadrat = case_when(mean > 0 ~ "yes",
                             TRUE ~ "no"))

## anatoxin data
atx <- read.csv("./data/field_and_lab/allrivers22_combined.csv") %>% 
  select(field_date, site_reach, site, TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_orgmat_g,
                anabaena_cylindrospermum = TAC_ATX_all_ug_orgmat_g) %>% 
  pivot_longer(cols = c(4,5), names_to = "taxa", values_to = "ATX_ug_orgmat_g")

# treat 07-07 sample as 07-06 because that is the date that the other reaches were sampled
atx$field_date[which(atx$field_date == ymd("2022-07-07"))] <- ("2022-07-06")
atx$field_date <- ymd(atx$field_date) # aware that this is redundant but lubridate was having issues w/ me doing it in prior line (?)

# need to calculate average atx per day
atx <- atx %>% 
  # replace NAs with 0, using 0 for absence
  mutate(ATX_ug_orgmat_g = replace_na(ATX_ug_orgmat_g, 0)) %>% 
  dplyr::group_by(field_date, site, taxa) %>% 
  dplyr::summarize(mean_ATX_ug_orgmat_g = mean(ATX_ug_orgmat_g))

## gpp data

# full time series for each site
gpp <- rbind(read.csv("./data/metab_model_outputs_processed/sfkeel_mir_2022_metab.csv"),
             read.csv("./data/metab_model_outputs_processed/russian_USGS_2022_metab.csv"),
             read.csv("./data/metab_model_outputs_processed/salmon_karuk_2022_metab.csv")) %>% 
  mutate(date = ymd(date))

## discharge data
disc <- rbind(read.csv("./data/USGS/sfkeel_mir_discharge_daily.csv"),
              read.csv("./data/USGS/russian_discharge_daily.csv"),
              read.csv("./data/USGS/salmon_discharge_daily.csv")) %>% 
  mutate(date = ymd(date))

## split all into lists
atx_list <- split(atx, atx$site)
cover_list <- split(cover, cover$site)
gpp_list <- split(gpp, gpp$site_year)
disc_list <- split(disc, disc$site)

#### (2) Making benthic cyanobacterial dynamics figures ####

# set theme for all plots
theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), strip.background = element_blank(),
                  panel.grid.major = element_blank(), panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 20), strip.text = element_text(size = 15),
                  plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm"), 
                  plot.title = element_text(size = 15, face = "bold", hjust = 0.5)))

## South Fork Eel River

# plot
bc_sfkeel <- ggplot(data = cover_list$`SFE-M`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M`, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g, fill = taxa), width = 7, color = "black") +
  geom_line(data = cover_list$`SFE-M`, aes(y = 110 - (mean * 4), color = taxa, linetype = taxa),
            linewidth = 1.5) +
  geom_errorbar(data = cover_list$`SFE-M`, aes(ymin = 110 - ((min) * 4),
                                               ymax = 110 - ((max) * 4),
                                               color = taxa), 
                linewidth = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = cover_list$`SFE-M`, aes(y = 110 - (mean * 4), color = taxa ,shape = taxa),
             size = 4) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  ggtitle("South Fork Eel River") +
  theme(legend.position = "none") # will move over legend via illustrator
bc_sfkeel

# ver with different symbology
bc_sfkeel2 <- ggplot(data = cover_list$`SFE-M`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M`, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g, fill = taxa), width = 7, color = "black") +
  geom_line(data = cover_list$`SFE-M`, aes(y = 110 - (mean * 4), color = taxa, linetype = taxa),
            linewidth = 1.5) +
  geom_errorbar(data = cover_list$`SFE-M`, aes(ymin = 110 - ((min) * 4),
                                                       ymax = 110 - ((max) * 4),
                                                       color = taxa), 
                linewidth = 1.25, alpha = 0.7, width = 2, position = position_dodge(width = 1.5)) +
  geom_point(data = cover_list$`SFE-M`, aes(y = 110 - (mean * 4), color = taxa, 
                                            shape = interaction(present, quadrat)),
             size = 3.5, stroke = 2, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 19)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1)) +
  ggtitle("South Fork Eel River") #+
  theme(legend.position = "none") # will move over legend via illustrator
bc_sfkeel2

## Russian River

# remove microcoleus so line not present 
# don't do this if using second option???
cover_list$RUS <- cover_list$RUS %>% 
  filter(taxa == "anabaena_cylindrospermum")

# plot
bc_russian <- ggplot(data = cover_list$RUS, aes(x = field_date)) +
  geom_bar(data = atx_list$RUS, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g, fill = taxa), width = 6, color = "black") +
  geom_line(data = cover_list$RUS, aes(y = 10 - (mean * 1.25), color = taxa, linetype = taxa),
            linewidth = 2) +
  geom_errorbar(data = cover_list$RUS, aes(ymin = 10 - ((min) * 1.25),
                                           ymax = 10 - ((max) * 1.25),
                                           color = taxa), 
                size = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = cover_list$RUS, aes(y = 10 - (mean * 1.25), color = taxa, shape = taxa),
             size = 5) +
  scale_color_manual("Group", values = c("#8f8504"),
                     labels = c("Anabaena & Cylindrospermum")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 18),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 10)/1.25) * -1)) +
  ggtitle("Russian River") +
  theme(legend.position = "none") # will move over legend via illustrator
bc_russian

# version w/ different symbology
bc_russian2 <- ggplot(data = cover_list$RUS, aes(x = field_date)) +
  geom_bar(data = atx_list$RUS, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g, fill = taxa), width = 6, color = "black") +
  geom_line(data = cover_list$RUS, aes(y = 10 - (mean * 1.25), color = taxa, linetype = taxa),
            linewidth = 2) +
  geom_errorbar(data = cover_list$RUS, aes(ymin = 10 - ((min) * 1.25),
                                          ymax = 10 - ((max) * 1.25),
                                          color = taxa), 
                linewidth = 1.25, alpha = 0.7, width = 2, position = position_dodge(width = 1.5)) +
  geom_point(data = cover_list$RUS, aes(y = 10 - (mean * 1.25), color = taxa, 
                                        shape = interaction(present, quadrat)),
             size = 3.5, stroke = 2, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 19)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 10)/1.25) * -1)) +
  ggtitle("Russian River") #+
  theme(legend.position = "none") # will move over legend via illustrator
bc_russian2

## Salmon River

# remove anabaena so line not present
# only for first version, don't do this for second
# though consider that anabaena is like .44% present at the end of the season
cover_list$SAL <- cover_list$SAL %>% 
  filter(taxa == "microcoleus")

# create gray box for wildfire (when sampling was not taking place!) 
wildfire <- data.frame(xmin = as.Date("2022-07-30"),
                       xmax = as.Date("2022-09-18"),
                       ymin = 0, 
                       ymax = 5)

# if removed anabaena-
# add segment column to avoid line being drawn across plot when we weren't taking data
cover_list$SAL$segment <- 1
cover_list$SAL$segment[4] <- 2

# plot
bc_salmon <- ggplot(data = cover_list$SAL) +
  geom_bar(data = atx_list$SAL, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g, x = field_date, fill = taxa), width = 7, color = "black") +
  geom_rect(data = wildfire, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "#ededed") +
  geom_line(data = cover_list$SAL, aes(y = 5 - (mean * .15), 
                                       x = field_date, color = taxa, 
                                       linetype = taxa, group = segment), linewidth = 1.5) +
  geom_errorbar(data = cover_list$SAL, aes(ymin = 5 - ((min) * .15),
                                           ymax = 5 - ((max) * .15),
                                           x = field_date,
                                           color = taxa), 
                linewidth = 1.25, alpha = 0.7, width = 2) +
  geom_point(data = cover_list$SAL, aes(y = 5 - (mean * .15), 
                                        x = field_date, color = taxa, 
                                        shape = taxa),
             size = 4) +
  scale_color_manual("Group", values = c("#2871c7"),
                     labels = c("Microcoleus")) +
  scale_linetype_manual("Group", values = c("dashed"),
                        labels = c("Microcoleus")) +
  scale_shape_manual("Group", values = c(18),
                     labels = c("Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(limits = c(5,0), sec.axis = sec_axis(~ ((. - 5)/.15) * -1)) +
  ggtitle("Salmon River") +
  theme(legend.position = "none")
bc_salmon

# add segment column to avoid line being drawn across plot when we weren't taking data
# this set up is specifically for not removing anabaena
cover_list$SAL$segment <- 1
cover_list$SAL$segment[c(4)] <- 2
cover_list$SAL$segment[c(5:7)] <- 3
cover_list$SAL$segment[c(8)] <- 4

# plot
bc_salmon2 <- ggplot(data = cover_list$SAL) +
  geom_bar(data = atx_list$SAL, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g, x = field_date, fill = taxa), width = 7, color = "black") +
  geom_rect(data = wildfire, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "#ededed") +
  geom_line(data = cover_list$SAL, aes(y = 5 - (mean * .15), 
                                       x = field_date, color = taxa, 
                                       linetype = taxa, group = segment), linewidth = 1.5) +
  geom_errorbar(data = cover_list$SAL, aes(ymin = 5 - ((min) * .15),
                                           ymax = 5 - ((max) * .15),
                                           x = field_date,
                                           color = taxa), 
                linewidth = 1.25, alpha = 0.7, width = 2, position = position_dodge(width = 1.5)) +
  geom_point(data = cover_list$SAL, aes(y = 5 - (mean * .15), 
                                        x = field_date, color = taxa, 
                                        shape = interaction(present, quadrat)),
             size = 3.5, stroke = 2, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504", "#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present/Quadrat", values = c(4, 19)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(limits = c(5,0), sec.axis = sec_axis(~ ((. - 5)/.15) * -1)) +
  ggtitle("Salmon River") #+
  theme(legend.position = "none")
bc_salmon2

#### (3) Making gpp/discharge figures ####

## South Fork Eel River

# add segment column to avoid ribbon being drawn across plot when we weren't taking data
gpp_list$sfkeel_mir_2022$segment <- 1
gpp_list$sfkeel_mir_2022$segment[14:nrow(gpp_list$sfkeel_mir_2022)] <- 2

# plot
gpp_sfkeel <- ggplot(data = gpp_list$sfkeel_mir_2022, aes(x = date)) +
  geom_area(data = disc_list$sfkeel_mir, aes(y = discharge_m3_s * 2, x = date), 
            fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct, group = segment),
              fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2)) +
  ggtitle("South Fork Eel River") +
  coord_cartesian(ylim = c(0, 9))
gpp_sfkeel

## Russian River

# plot
gpp_russian <- ggplot(data = gpp_list$russian_USGS_2022, aes(x = date)) +
  geom_area(data = disc_list$russian, aes(y = discharge_m3_s * 2.5, x = date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct), fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 2.5)) +
  coord_cartesian(ylim = c(0, 13)) +
  ggtitle("Russian River") +
  labs(y = NULL, x = NULL)
gpp_russian

## Salmon River

# plot
gpp_salmon <- ggplot(data = gpp_list$salmon_karuk_2022, aes(x = date)) +
  geom_area(data = disc_list$salmon, aes(y = discharge_m3_s * 0.45, x = date), fill = "#d9ecff") +
  geom_ribbon(aes(ymin = GPP.2.5.pct, ymax = GPP.97.5.pct), fill = "#9ced66", alpha = 0.8) +
  geom_point(aes(y = GPP.mean), color = "#397014", size = 2.5, alpha = 1) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 0.45)) +
  coord_cartesian(ylim = c(0, 11)) +
  ggtitle("Salmon River") +
  labs(y = NULL, x = NULL)
gpp_salmon

#### (4) Putting figures together

# putting together
all <- plot_grid(bc_sfkeel, gpp_sfkeel, bc_russian, 
                         gpp_russian, bc_salmon, gpp_salmon, 
                         ncol = 2, align = "hv", scale = 0.98)

final <- grid.arrange(all, left = textGrob("\u03bcg ATX per g OM", 
                                           gp=gpar(fontsize=14), rot=90),
                      right = textGrob("percent cover (%)", 
                                       gp=gpar(fontsize=14), rot = 270))
# STILL IN PROGRESS

bc <- plot_grid(bc_sfkeel, bc_russian, bc_salmon, align = "hv", ncol = 1)

# will edit further in adobe