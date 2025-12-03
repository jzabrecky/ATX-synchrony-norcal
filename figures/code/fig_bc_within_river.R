#### Primary figure for taxa-specific cover & anatoxins and GPP on each river
### Jordan Zabrecky
## last edited: 10.25.2025

# This script creates a primary figure for Q2 focused on the relationships
# between benthic cyanobacteria dynamics within the same river. Additionally,
# at the end of the script a question- "How does peak magnitude differ at each 
# reach?" is answered

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot"), 
       require, character.only = T)

# loading in data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  select(field_date, site_reach, microcoleus, anabaena_cylindrospermum, 
         TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g, 
         proportion_micro_transects, proportion_ana_cyl_transects)

# edit data for plot purposes (pivoting longer)
cover_longer <- data %>% 
  select(!c(TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g,
            proportion_micro_transects, proportion_ana_cyl_transects)) %>% 
  pivot_longer(cols = c("microcoleus", "anabaena_cylindrospermum"),
               names_to = "taxa", values_to = "cover")
atx_longer <- data %>% 
  select(!c(microcoleus, anabaena_cylindrospermum,
            proportion_micro_transects, proportion_ana_cyl_transects)) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_orgmat_g,
         anabaena_cylindrospermum = TAC_ATX_all_ug_orgmat_g) %>%
  pivot_longer(cols = c("microcoleus", "anabaena_cylindrospermum"),
             names_to = "taxa", values_to = "ATX_all_ug_orgmat_g")
presence_longer <- data %>% 
  select(!c(TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g)) %>% 
  # turn presence into binary if nearby transect of in quadrat
  mutate(microcoleus = case_when(proportion_micro_transects > 0 ~ "yes",
                                 TRUE ~ "no"),
         anabaena_cylindrospermum = case_when(proportion_ana_cyl_transects > 0 ~ "yes",
                                              TRUE ~ "no")) %>% 
  pivot_longer(cols = c("microcoleus", "anabaena_cylindrospermum"),
               names_to = "taxa", values_to = "present") %>%  
  select(field_date, site_reach, taxa, present)

# put all data together
data_longer <- left_join(cover_longer, atx_longer, by = c("taxa", "field_date", "site_reach"))
data_longer <- left_join(data_longer, presence_longer, by = c("taxa", "field_date", "site_reach")) %>% 
  # add in if it is present in quadrat survey design
  mutate(quadrat = case_when(cover > 0 ~ "yes",
                             TRUE ~ "no"))

# replace anatoxin NAs with 0's
data_longer$ATX_all_ug_orgmat_g <- replace_na(data_longer$ATX_all_ug_orgmat_g)

#### (2) Making figure ####

# set theme for all plots
theme_set(theme_bw() + theme(legend.position = "bottom", 
                             panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                             panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.2, "cm"),
                             strip.background = element_blank()))

# benthic cyanobacteria plot
sfe_all <- ggplot(data = data_longer, aes(x = field_date)) +
  geom_bar(position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_orgmat_g, fill = taxa, color = taxa), 
           width = 5) +
  geom_line(aes(y = 260 - (cover * 8), color = taxa, linetype = taxa),
            linewidth = 0.8) +
  geom_point(aes(y = 260 - (cover * 8), color = taxa, shape = present),
             size = 1.5, stroke = 1, position = position_dodge(width = 1.8)) +
  scale_color_manual("Taxa", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Taxa", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16)) +
  scale_fill_manual("Taxa", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~site_reach, ncol = 1, 
             labeller =  as_labeller(c(`SFE-M-1S` = "SFE-Lower-1S", 
                                       `SFE-M-2`= "SFE-Lower-2",
                                       `SFE-M-3` = "SFE-Lower-3",
                                       `SFE-M-4` = "SFE-Lower-4",
                                       `SFE-SH-1S` = "SFE-Upper-1S"))) +
  labs(#y = expression(paste("Anatoxins (", mu, "g g"^-1, "OM)")), 
       y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 260)/8) * -1, 
                                      #name = "Cover (%)")) +
                                          ))+
  theme(strip.text = element_text(size = 8, face = "bold")) +
  theme(legend.position = "none") # will move over legend via illustrator
sfe_all 

# save figure
ggsave("./figures/fig_bc_withinrivers_notfinal.tiff", dpi = 600, 
       width=9, height=14.25, unit="cm") 

#### (3) Misc. Questions ####

# how does peak magnitude vary per reach??
summary <- data %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(max_m = max(microcoleus),
                   max_ac = max(anabaena_cylindrospermum),
                   max_m_atx = max(TM_ATX_all_ug_orgmat_g, na.rm = TRUE),
                   max_ac_atx = max(TAC_ATX_all_ug_orgmat_g, na.rm = TRUE))
max(summary$max_m) - min(summary$max_m) # 15.7% difference
max(summary$max_ac) - min(summary$max_ac) # 21.81% difference
