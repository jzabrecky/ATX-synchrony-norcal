#### Taxa-specific cover and anatoxin relationships per reach all rivers (2022 data)
### Jordan Zabrecky
## last edited: 07.25.2025

# This script plots a supplemental figure taxon-specific cover and anatoxin concentrations
# for each reach sampled on each river in 2022 (rather than grouping all reaches together)

#### (1) Loading in libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "dataRetrieval", "cowplot", 
         "gridExtra", "grid"), 
       require, character.only = T)

# loading in data
data <- read.csv("./data/field_and_lab/allrivers22_combined.csv") %>% 
  mutate(field_date = ymd(field_date))

# pivot longer (cover)
cover <- data %>% 
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum,) %>% 
  pivot_longer(cols = c(4,5), names_to = "taxa", values_to = "percent_cover")

# get presence/absence
presence <- data %>% 
  select(field_date, site_reach, site, proportion_micro_transects, 
         proportion_ana_cyl_transects) %>% 
  mutate(microcoleus_present = 
           case_when(proportion_micro_transects > 0 ~ "yes",
                     TRUE ~ "no"),
         ana_cyl_present = 
           case_when(proportion_ana_cyl_transects > 0 ~ "yes",
                     TRUE ~ "no")) %>% 
  dplyr::rename(microcoleus = microcoleus_present,
         anabaena_cylindrospermum = ana_cyl_present) %>% 
  select(!c("proportion_micro_transects", "proportion_ana_cyl_transects")) %>% 
  pivot_longer(cols = c(4,5), names_to = "taxa", values_to = "presence")

# join in presence data with cover
cover <- left_join(cover, presence, by = c("field_date", "site_reach", "site", "taxa"))

# pivot longer (anatoxins)
anatoxins <- data %>% 
  select(field_date, site_reach, site, TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_orgmat_g,
                anabaena_cylindrospermum = TAC_ATX_all_ug_orgmat_g) %>% 
  pivot_longer(cols = c(4,5), names_to = "taxa", values_to = "ATX_ug_orgmat_g")

# split based on site_reach
cover_list <- split(cover, cover$site)
atx_list <- split(anatoxins, anatoxins$site)

#### (2) Making plots ####

# set theme for all plots
theme_set(theme_bw() + theme(legend.position = "bottom", 
                             panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                             panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             strip.background = element_blank()))

## (a) Russian River

## russian river reaches

# make figure
rus_figs <- ggplot(data = cover_list$RUS, aes(x = field_date)) +
  geom_bar(data = atx_list$RUS, position = "dodge", stat = "identity", 
           aes(y = ATX_ug_orgmat_g, fill = taxa, color = taxa), width = 7.5) +
  geom_line(data = cover_list$RUS, aes(y = 10 - (percent_cover * 1.25), color = taxa, linetype = taxa),
            linewidth = 1.25) +
  geom_point(data = cover_list$RUS, aes(y = 10 - (percent_cover * 1.25), color = taxa, 
                                        shape = interaction(presence)),
             size = 2, stroke = 1.5, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 10)/1.25) * -1)) +
  theme(legend.position = "none") + # will move over legend via illustrator
  facet_grid(~site_reach,
             labeller =  as_labeller(c(`RUS-1S` = "Reach 1 (RUS-1)", 
                                       `RUS-2`= "Reach 2 (RUS-2)",
                                       `RUS-3` = "Reach 3 (RUS-3)"))) + 
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26")))
rus_figs

## salmon river reaches

# create gray box for wildfire (when sampling was not taking place!) 
wildfire <- data.frame(xmin = as.Date("2022-07-30"),
                       xmax = as.Date("2022-09-18"),
                       ymin = 0, 
                       ymax = 5)

# make figure
# (having issues getting segment grouping to remove lines around box
# so will just remove lines later)
sal_figs <- ggplot(data = cover_list$SAL) +
  geom_bar(data = atx_list$SAL, position = "dodge", stat = "identity", 
           aes(x = field_date, y = ATX_ug_orgmat_g, fill = taxa, color = taxa), width = 7.5) +
  geom_line(data = cover_list$SAL, aes(x = field_date, 
                                       y = 5 - (percent_cover * .15), color = taxa,
                                       linetype = taxa),
          linewidth = 1.25) +
  geom_point(data = cover_list$SAL, aes(x = field_date, 
                                        y = 5 - (percent_cover * .15), color = taxa, 
                                        shape = interaction(presence)),
             size = 2, stroke = 1.5, position = position_dodge(width = 1.5)) +
  geom_rect(data = wildfire, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "#ededed") +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(limits = c(5,0), sec.axis = sec_axis(~ ((. - 5)/.15) * -1)) +
  theme(legend.position = "none") + # will move over legend via illustrator
  facet_grid(~site_reach,
             labeller =  as_labeller(c(`SAL-1S` = "Reach 1 (SAL-1)", 
                                       `SAL-2`= "Reach 2 (SAL-2)",
                                       `SAL-3` = "Reach 3 (SAL-3)")))
sal_figs 

## south fork eel reaches

# make figures
eel_figs <- ggplot(data = cover_list$`SFE-M`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M`, position = "dodge", stat = "identity", 
           aes(y = ATX_ug_orgmat_g, fill = taxa, color = taxa), width = 7.5) +
  geom_line(data = cover_list$`SFE-M`, aes(y = 250 - (percent_cover * 7), color = taxa, linetype = taxa),
            linewidth = 1.25) +
  geom_point(data = cover_list$`SFE-M`, aes(y = 250 - (percent_cover * 7), color = taxa, 
                                        shape = interaction(presence)),
             size = 2, stroke = 1.5, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(limits = c(250,0), sec.axis = sec_axis(~ ((. - 250)/7) * -1)) +
  theme(legend.position = "none") + # will move over legend via illustrator
  facet_grid(~site_reach,
             labeller =  as_labeller(c(`SFE-M-1S` = "Reach 1 (SFE-M-1)", 
                                       `SFE-M-3`= "Reach 2 (SFE-M-3)",
                                       `SFE-M-4` = "Reach 3 (SFE-M-4)"))) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26")))
eel_figs

# note: tried to do log bar graphs using pseudo_log_trans() to deal with values <1
# (which end up as <0 after being logged), but having issues reversing it

#### (3) Putting plots together into one big plot & saving ####

# using cowplot to put together plots
all <- plot_grid(eel_figs, rus_figs, sal_figs, nrow = 3, align='vh',
                 scale = 0.96) +
  theme(plot.background = element_rect(fill = "white", color = "white")) # white background
all # will edit further in inkscape

ggsave("./figures/sfig_bc_acrossrivers_allreaches_notfinal.tiff", dpi = 600, 
       width=18, height=18, unit="cm") 
