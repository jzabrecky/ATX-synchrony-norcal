#### Figure showing taxa-specific cover & anatoxins and relationships per reach
### Jordan Zabrecky
## last edited 05.05.2025

# This code creates figures that show taxa-specific cover and mat 
# anatoxin concentrations for each reach and year and also creates
# smaller plots of the relationship between standardized cover
# and anatoxin concentrations for each taxa (per reach and year)

# IN PROGRESS :)

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "cowplot"), 
       require, character.only = T)

# loading raw data
cover <- read.csv("./data/field_and_lab/percover_byreach.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date)) %>% # just doing sfkeel 2023 for now...
  filter(site == "SFE-M" | site == "SFE-SH") %>% 
  filter(year == 2023) %>% 
  select(field_date, site_reach, microcoleus, anabaena_cylindrospermum)
cover_longer <- pivot_longer(cover, c(3,4), names_to = "group", values_to = "mean")
atx <- read.csv("./data/field_and_lab/cyano_atx.csv") %>% 
  mutate(field_date = ymd(field_date),
       year = year(field_date)) %>% # just doing sfkeel 2023 for now...
  filter(site_reach == "SFE-M-1S" | site_reach == "SFE-M-2" | 
           site_reach == "SFE-M-3" | site_reach == "SFE-M-4" |
           site_reach == "SFE-SH-1S") %>% 
  filter(year == 2023)

# separating into lists per reach
cover_list <- split(cover_longer, cover_longer$site_reach)
atx_list <- split(atx, atx$site_reach)

# loading data standardized per reach
# standardized_perreach
data_stnd <- read.csv("./data/field_and_lab/synchrony_stnd_per_reach.csv")

#### (2) STARTING ONLY WITH SOUTH FORK EEL 2023 FOR POSTER ####

# theme set for all
theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                  panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 30), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm")))

## (a) SFE-M-1S
sfe_m_1s <- ggplot(data = cover_list$`SFE-M-1S`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M-1S`, position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_orgmat_g, fill = sample_type), width = 5.5, color = "black") +
  geom_line(data = cover_list$`SFE-M-1S`, aes(y = 250 - (mean * 8), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = cover_list$`SFE-M-1S`, aes(y = 250 - (mean * 8), color = group ,shape = group),
             size = 5.5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 250)/8) * -1)) +
  theme(legend.position = "none") # will move over legend via illustrator
sfe_m_1s

## (b) SFE-M-2
# need to add in 0's for TAC or TM data when there is none so bar stays 1/2 length
atx_list$`SFE-M-2`<- rbind(atx_list$`SFE-M-2`, # duplicate those dates on new df
                           atx_list$`SFE-M-2`[c(12, 15),])
atx_list$`SFE-M-2`[20:21,12] <- 0 # make ATX values for those all 0
atx_list$`SFE-M-2`[20:21,3] <- "TAC" # flip labels for those rows

sfe_m_2 <- ggplot(data = cover_list$`SFE-M-2`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M-2`, position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_orgmat_g, fill = sample_type), width = 5.5, color = "black") +
  geom_line(data = cover_list$`SFE-M-2`, aes(y = 250 - (mean * 8), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = cover_list$`SFE-M-2`, aes(y = 250 - (mean * 8), color = group ,shape = group),
             size = 5.5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 250) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 250)/8) * -1)) +
  theme(legend.position = "none") # will move over legend via illustrator
sfe_m_2
# WILL COME BACK TO FIX THE SCALE :)

## (c) SFE-M-3
# need to add in 0's for TAC or TM data when there is none so bar stays 1/2 length
atx_list$`SFE-M-3`<- rbind(atx_list$`SFE-M-3`, # duplicate those dates on new df
                           atx_list$`SFE-M-3`[c(16, 17, 20),])
atx_list$`SFE-M-3`[21:23,12] <- 0 # make ATX values for those all 0
atx_list$`SFE-M-3`[21:23,3] <- "TAC" # flip labels for those rows

sfe_m_3 <- ggplot(data = cover_list$`SFE-M-3`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M-3`, position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_orgmat_g, fill = sample_type), width = 5.5, color = "black") +
  geom_line(data = cover_list$`SFE-M-3`, aes(y = 250 - (mean * 8), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = cover_list$`SFE-M-3`, aes(y = 250 - (mean * 8), color = group ,shape = group),
             size = 5.5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 250)/8) * -1)) +
  theme(legend.position = "none") # will move over legend via illustrator
sfe_m_3

## (d) SFE-M-4
# need to add in 0's for TAC or TM data when there is none so bar stays 1/2 length
atx_list$`SFE-M-4`<- rbind(atx_list$`SFE-M-4`, # duplicate those dates on new df
                           atx_list$`SFE-M-4`[c(1, 2, 3, 4, 7, 10, 11, 12, 13),])
atx_list$`SFE-M-4`[14:22,12] <- 0 # make ATX values for those all 0
atx_list$`SFE-M-4`[14:18,3] <- "TM" # flip labels for those rows
atx_list$`SFE-M-4`[19:22,3] <- "TAC" # flip labels for those rows

sfe_m_4 <- ggplot(data = cover_list$`SFE-M-4`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M-4`, position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_orgmat_g, fill = sample_type), width = 5.5, color = "black") +
  geom_line(data = cover_list$`SFE-M-4`, aes(y = 250 - (mean * 8), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = cover_list$`SFE-M-4`, aes(y = 250 - (mean * 8), color = group ,shape = group),
             size = 5.5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 250) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 250)/8) * -1)) +
  theme(legend.position = "none") # will move over legend via illustrator
sfe_m_4

# need to add in 0's for TAC or TM data when there is none so bar stays 1/2 length
atx_list$`SFE-SH-1S`<- rbind(atx_list$`SFE-SH-1S`, # duplicate those dates on new df
                           atx_list$`SFE-SH-1S`[c(1, 2, 7, 12, 13, 14),])
atx_list$`SFE-SH-1S`[19:24,12] <- 0 # make ATX values for those all 0
atx_list$`SFE-SH-1S`[c(19, 20, 22, 23, 24),3] <- "TAC" # flip labels for those rows
atx_list$`SFE-SH-1S`[21,3] <- "TM" # flip labels for those rows

## (e) SFE-SH-1S
sfe_sh_1s <- ggplot(data = cover_list$`SFE-SH-1S`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-SH-1S`, position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_orgmat_g, fill = sample_type), width = 5.5, color = "black") +
  geom_line(data = cover_list$`SFE-SH-1S`, aes(y = 250 - (mean * 8), color = group, linetype = group),
            linewidth = 2) +
  geom_point(data = cover_list$`SFE-SH-1S`, aes(y = 250 - (mean * 8), color = group ,shape = group),
             size = 5.5) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  ylim(0, 250) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 250)/8) * -1)) +
  theme(legend.position = "none") # will move over legend via illustrator
sfe_sh_1s

#### relationships between cover & atx

# make it so you can facet wrap the two
data_stnd_longer1 <- pivot_longer(data_stnd, c(5,6), names_to = "taxa", values_to = "cover")
data_stnd_longer2 <- data_stnd %>% 
  select(!c(microcoleus, anabaena_cylindrospermum)) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_g, # FORGOT ORG MAT!?!?!
         anabaena_cylindrospermum = TAC_ATX_all_ug_g) %>% 
  select(!GPP_median_fourdaysprior)
data_stnd_longer2 <- pivot_longer(data_stnd_longer2, c(5,6), names_to = "taxa", values_to = "atx")
data_ready <- left_join(data_stnd_longer1, data_stnd_longer2, by = c("X", "field_date", "site_reach", "site", "taxa"))

# split into list
data_stnd_list <- split(data_ready, data_ready$site_reach)

# add grouping
for(i in 1:length(data_stnd_list)) {
  data_stnd_list[[i]]$group <- seq(1:nrow(data_stnd_list[[i]]))
}

covary_M_1s  <- ggplot(data = data_stnd_list$`SFE-M-1S`, aes(x = cover, y = atx)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), group = group, arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  
  theme(legend.position = "none")
covary_M_1s

covary_M_2  <- ggplot(data = data_stnd_list$`SFE-M-2`, aes(x = cover, y = atx)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  
  theme(legend.position = "none")
covary_M_2

covary_M_3  <- ggplot(data = data_stnd_list$`SFE-M-3`, aes(x = cover, y = atx)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  
  theme(legend.position = "none")
covary_M_3

covary_M_4  <- ggplot(data = data_stnd_list$`SFE-M-4`, aes(x = cover, y = atx)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  
  theme(legend.position = "none")
covary_M_4

covary_SH_1s  <- ggplot(data = data_stnd_list$`SFE-SH-1S`, aes(x = cover, y = atx)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  
  theme(legend.position = "none")
covary_SH_1s

