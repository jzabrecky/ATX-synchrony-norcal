#### Taxa-specific cover and anatoxin relationships per reach all rivers (2022 data)
### Jordan Zabrecky
## last edited: 06.02.2025

# This script plots a supplemental figure taxa-specific cover and anatoxin concentrations
# for each reach sampled on each river in 2022

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
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum) %>% 
  pivot_longer(cols = c(4,5), names_to = "taxa", values_to = "percent_cover")

# pivot longer (anatoxins)
anatoxins <- data %>% 
  select(field_date, site_reach, site, TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_orgmat_g,
                anabaena_cylindrospermum = TAC_ATX_all_ug_orgmat_g) %>% 
  pivot_longer(cols = c(4,5), names_to = "taxa", values_to = "ATX_ug_orgmat_g")

# fill anatoxin NAs with zero
anatoxins$ATX_ug_orgmat_g <- na.fill(anatoxins$ATX_ug_orgmat_g, 0)

# split based on site_reach
cover_list <- split(cover, cover$site)
atx_list <- split(anatoxins, anatoxins$site)

#### (2) Making plots ####

# set theme for all plots
theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), strip.background = element_blank(),
                  panel.grid.major = element_blank(), panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 20), strip.text = element_text(size = 15),
                  plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm"), 
                  plot.title = element_text(size = 15, face = "bold", hjust = 0.5)))

## (a) Russian River

## russian river reaches

# remove microcoleus so line not present
cover_list$RUS <- cover_list$RUS %>% 
  filter(taxa == "anabaena_cylindrospermum")

# make figure
rus_figs <- ggplot(data = cover_list$RUS, aes(x = field_date)) +
  geom_bar(data = atx_list$RUS, position = "dodge", stat = "identity", 
           aes(y = ATX_ug_orgmat_g, fill = taxa), width = 7, color = "black") +
  geom_line(data = cover_list$RUS, aes(y = 10 - (percent_cover * 1.25), color = taxa, 
                                        linetype = taxa), linewidth = 1.5) +
  geom_point(data = cover_list$RUS, aes(y = 10 - (percent_cover * 1.25), color = taxa, 
                                         shape = taxa),
             size = 4) +
  scale_color_manual("Group", values = c("#8f8504"),
                     labels = c("Anabaena & Cylindrospermum")) +
  scale_linetype_manual("Group", values = c("dotted"),
                        labels = c("Anabaena & Cylindrospermum")) +
  scale_shape_manual("Group", values = c(16),
                     labels = c("Anabaena & Cylindrospermum")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  facet_grid(~site_reach,
             labeller =  as_labeller(c(`RUS-1S` = "Reach 1 (RUS-1)", 
                                       `RUS-2`= "Reach 2 (RUS-2)",
                                       `RUS-3` = "Reach 3 (RUS-3)"))) + 
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(limits = c(10,0), sec.axis = sec_axis(~ ((. - 10)/1.25) * -1)) +
  theme(legend.position = "none") + 
  ggtitle("B. Russian River")
rus_figs

## salmon river reaches

# create gray box for wildfire (when sampling was not taking place!) 
wildfire <- data.frame(xmin = as.Date("2022-07-30"),
                       xmax = as.Date("2022-09-18"),
                       ymin = 0, 
                       ymax = 5)

# remove anabaena from cover so we don't get a line at 0
cover_list$SAL <- cover_list$SAL %>% 
  filter(taxa == "microcoleus")

# add segment column to avoid line being drawn across plot when we weren't taking data
cover_list$SAL$segment <- 1 # pre-fire
cover_list$SAL$segment[c(4, 7, 11)] <- 2 # post-fire; last sampling

# make figure
sal_figs <- ggplot(data = cover_list$SAL) +
  geom_bar(data = atx_list$SAL, position = "dodge", stat = "identity", 
           aes(y = ATX_ug_orgmat_g, x = field_date, fill = taxa), width = 7, color = "black") +
  geom_rect(data = wildfire, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "#ededed") +
  geom_line(data = cover_list$SAL, aes(y = 5 - (percent_cover * .15), 
                                          x = field_date, color = taxa, 
                                          linetype = taxa, group = segment), linewidth = 1.5) +
  geom_point(data = cover_list$SAL, aes(y = 5 - (percent_cover * .15), 
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
  facet_grid(~site_reach,
             labeller =  as_labeller(c(`SAL-1S` = "Reach 1 (SAL-1)", 
                                       `SAL-2`= "Reach 2 (SAL-2)",
                                       `SAL-3` = "Reach 3 (SAL-3)"))) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(limits = c(5,0), sec.axis = sec_axis(~ ((. - 5)/.15) * -1)) +
  theme(legend.position = "none") +
  ggtitle("C. Salmon River")
sal_figs

## south fork eel reaches

# make figures
eel_figs <- ggplot(data = cover_list$`SFE-M`, aes(x = field_date)) +
  geom_bar(data = atx_list$`SFE-M`, position = "dodge", stat = "identity", 
           aes(y = ATX_ug_orgmat_g, fill = taxa), width = 7, color = "black") +
  geom_line(data = cover_list$`SFE-M`, aes(y = 250 - (percent_cover * 7), color = taxa, 
                                          linetype = taxa), linewidth = 1.5) +
  geom_point(data = cover_list$`SFE-M`, aes(y = 250 - (percent_cover * 7), color = taxa, 
                                           shape = taxa),
             size = 4) +
  scale_color_manual("Group", values = c("#8f8504", "#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Group", values = c(16, 18),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  facet_grid(~site_reach,
             labeller =  as_labeller(c(`SFE-M-1S` = "Reach 1 (SFE-M-1)", 
                                       `SFE-M-3`= "Reach 2 (SFE-M-3)",
                                       `SFE-M-4` = "Reach 3 (SFE-M-4)"))) +
  scale_x_date(limits = as.Date(c("2022-06-20", "2022-09-26"))) +
  scale_y_reverse(limits = c(250,0), sec.axis = sec_axis(~ ((. - 250)/7) * -1)) +
  theme(legend.position = "none") + 
  ggtitle("A. South Fork Eel River")
eel_figs

# note: tried to do log bar graphs using pseudo_log_trans() to deal with values <1
# (which end up as <0 after being logged), but having issues reversing it

#### (3) Putting plots together into one big plot

# using cowplot to put together plots
all <- plot_grid(eel_figs, rus_figs, sal_figs, nrow = 3, align='vh')

# using gridExtra to add axes labels
final <- grid.arrange(all, left = textGrob("\u03bcg ATX per g OM", 
                                           gp=gpar(fontsize=14), rot=90),
                      right = textGrob("percent cover (%)", 
                                       gp=gpar(fontsize=14), rot = 270))
