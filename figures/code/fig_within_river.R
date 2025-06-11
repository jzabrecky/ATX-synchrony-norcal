#### Primary figure for taxa-specific cover & anatoxins and GPP on each river
### Jordan Zabrecky
## last edited: 06.10.2025

# This script creates a primary figure for Q2 focused on the relationships
# between benthic cyanobacteria dynamics and GPP within the same river

# NOTE: NEED TO DECIDE IF NON DETECTS ARE ZERO OR NA :)

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot", "gridExtra", "grid"), 
       require, character.only = T)

# loading in data
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  select(field_date, site_reach, microcoleus, anabaena_cylindrospermum, 
         TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g)

# edit data for plot purposes (pivoting longer)
cover_longer <- data %>% 
  select(!c(TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g)) %>% 
  pivot_longer(cols = c("microcoleus", "anabaena_cylindrospermum"),
               names_to = "taxa", values_to = "cover")
atx_longer <- data %>% 
  select(!c(microcoleus, anabaena_cylindrospermum)) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_orgmat_g,
         anabaena_cylindrospermum = TAC_ATX_all_ug_orgmat_g) %>%
  pivot_longer(cols = c("microcoleus", "anabaena_cylindrospermum"),
             names_to = "taxa", values_to = "ATX_all_ug_orgmat_g")
data_longer <- left_join(cover_longer, atx_longer, by = c("taxa", "field_date", "site_reach"))

#### (2) Making figure ####

# set theme for all plots
theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), strip.background = element_blank(),
                  panel.grid.major = element_blank(), panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 20), strip.text = element_text(size = 15),
                  plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm"), 
                  plot.title = element_text(size = 15, face = "bold", hjust = 0.5)))

# plot
sfe_all <- ggplot(data = data_longer, aes(x = field_date)) +
  geom_bar(position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_orgmat_g, fill = taxa), width = 5.5, color = "black") +
  geom_line(aes(y = 250 - (cover * 8), color = taxa, linetype = taxa),
            linewidth = 1.5) +
  geom_point(aes(y = 250 - (cover * 8), color = taxa ,shape = taxa),size = 3) +
  scale_color_manual("Taxa", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Taxa", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Taxa", values = c(16, 15),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Taxa", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~site_reach, ncol = 1) +
  labs(y = NULL, x = NULL) +
  ylim(0, 110) +
  scale_x_date(limits = as.Date(c("2023-06-18", "2023-09-27"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 250)/8) * -1)) +
  theme(legend.position = "none") # will move over legend via illustrator

# make a little smaller in grid before adding y-axis labels 
temp <- plot_grid(sfe_all, scale = 0.95)
# may also want to add in a, b, c, d

# add y-axis labels
final <- grid.arrange(temp, left = textGrob("\u03bcg ATX per g OM", 
                                           gp=gpar(fontsize=13), rot=90),
                      right = textGrob("percent cover (%)", 
                                       gp=gpar(fontsize=13), rot = 270))
