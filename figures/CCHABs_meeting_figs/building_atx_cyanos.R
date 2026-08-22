#### Script to progressively build cover & ATX plots for presentation purposes
### Jordan Zabrecky
## last edited: 07.22.2026

# see title

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot"), 
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
  filter(!site %in% c("RUS", "SAL", "SFE-M_excl_site2")) %>% 
  select(field_date, site, microcoleus_mean, microcoleus_sd, microcoleus_present,
         anabaena_cylindrospermum_mean, anabaena_cylindrospermum_sd, ana_cyl_present)

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
                             TRUE ~ "no")) %>% 
  mutate(year = year(field_date))

## anatoxin data
atx <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  select(field_date, site_reach, site, TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_orgmat_g,
                anabaena_cylindrospermum = TAC_ATX_all_ug_orgmat_g) %>% 
  pivot_longer(cols = c(4,5), names_to = "taxa", values_to = "ATX_ug_orgmat_g")

# need to calculate average atx per day
atx <- atx %>% 
  # replace NAs with 0 for calculations purposes
  mutate(ATX_ug_orgmat_g = replace_na(ATX_ug_orgmat_g, 0)) %>% 
  dplyr::group_by(field_date, site, taxa) %>% 
  dplyr::summarize(mean_ATX_ug_orgmat_g = mean(ATX_ug_orgmat_g)) %>% 
  mutate(year = year(field_date))

# To-do:
# LOAD 2024 DATA

# set theme for all plots
theme_set(theme_bw() + theme(legend.position = "bottom", 
                             panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                             panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 15), axis.ticks.length=unit(.25, "cm")))

#### (2) Building Up Figure - 2022 ####

cover_sfkeel23 <- cover %>% 
  filter(site == "SFE-M_all_sites" & year == 2023) %>% 
  mutate(field_date = ymd(field_date))
atx_sfkeel23 <- atx %>% 
  filter(site == "SFE-M" & year == 2023) %>% 
  mutate(field_date = ymd(field_date))

# only Microcoleus cover
bc_sfkeel_23_microcoveronly <- ggplot(data = cover_sfkeel23 %>% filter(taxa == "microcoleus"), aes(x = field_date)) +
  geom_bar(data = atx_sfkeel23 %>% filter(site == "RUS"), position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g , fill = taxa, color = taxa), width = 5) +
  geom_line(data = cover_sfkeel23 %>% filter(taxa == "microcoleus"), aes(y = 260 - (mean * 8), color = taxa, linetype = taxa),
            linewidth = 1.25) +
  geom_errorbar(data = cover_sfkeel23 %>% filter(taxa == "microcoleus"), aes(ymin = 260 - ((min) * 8),
                                           ymax = 260 - ((max) * 8),
                                           color = taxa), 
                linewidth = 1.25, alpha = 0.5, width = 6, position = position_dodge(width = 1.5)) +
  geom_point(data = cover_sfkeel23 %>% filter(taxa == "microcoleus"), aes(y = 260 - (mean * 8), color = taxa, 
                                        shape = interaction(present, quadrat)),
             size = 2, stroke = 1.5, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16, 16)) +
  scale_fill_manual("Group", values = c("#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(limits = c(0, 265), sec.axis = sec_axis(~ ((. - 260)/8) * -1, 
                                      #name = "Cover (%)")) +
  ))+
  theme(legend.position = "none")
bc_sfkeel_23_microcoveronly

#  cover only
bc_sfkeel_23_coveronly <- ggplot(data = cover_sfkeel23, aes(x = field_date)) +
  #geom_bar(data = atx_sfkeel23, position = "dodge", stat = "identity", 
  #         aes(y = mean_ATX_ug_orgmat_g, fill = taxa, color = taxa), width = 5) +
  geom_line(data = cover_sfkeel23, aes(y = 260 - (mean * 8), color = taxa, linetype = taxa),
            linewidth = 1.25) +
  geom_errorbar(data = cover_sfkeel23, aes(ymin = 260 - ((min) * 8),
                                           ymax = 260 - ((max) * 8),
                                           color = taxa), 
                linewidth = 1.25, alpha = 0.5, width = 6, position = position_dodge(width = 1.5)) +
  geom_point(data = cover_sfkeel23, aes(y = 260 - (mean * 8), color = taxa, 
                                        shape = interaction(present, quadrat)),
             size = 2, stroke = 1.5, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16, 16)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(limits = c(0, 265), sec.axis = sec_axis(~ ((. - 260)/8) * -1, 
                                                          #name = "Cover (%)")) +
  ))+
  theme(legend.position = "none")
bc_sfkeel_23_coveronly

# everything
bc_sfkeel_23 <- ggplot(data = cover_sfkeel23, aes(x = field_date)) +
  geom_bar(data = atx_sfkeel23, position = "dodge", stat = "identity", 
           aes(y = mean_ATX_ug_orgmat_g, fill = taxa, color = taxa), width = 5) +
  geom_line(data = cover_sfkeel23, aes(y = 260 - (mean * 8), color = taxa, linetype = taxa),
            linewidth = 1.25) +
  geom_errorbar(data = cover_sfkeel23, aes(ymin = 260 - ((min) * 8),
                                               ymax = 260 - ((max) * 8),
                                               color = taxa), 
                linewidth = 1.25, alpha = 0.5, width = 6, position = position_dodge(width = 1.5)) +
  geom_point(data = cover_sfkeel23, aes(y = 260 - (mean * 8), color = taxa, 
                                            shape = interaction(present, quadrat)),
             size = 2, stroke = 1.5, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16, 16)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(limits = c(0, 265), sec.axis = sec_axis(~ ((. - 260)/8) * -1, 
                                      #name = "Cover (%)")) +
  ))+
  theme(legend.position = "none")
bc_sfkeel_23

plot <- plot_grid(bc_sfkeel_23_microcoveronly, bc_sfkeel_23_coveronly, bc_sfkeel_23, ncol = 1,
                  align = "hv")
plot
