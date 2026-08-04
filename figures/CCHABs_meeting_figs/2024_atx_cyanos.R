#### Making plots similar to those made for 2022 and 2023 using 2024 data
### Jordan Zabrecky
## 08.03.2026

# see title

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot"), 
       require, character.only = T)

# percent cover data
cover <- read.csv("./figures/CCHABs_meeting_figs/2024_data/cover_averaged_24.csv") %>% 
  mutate(Date = ymd(Date)) %>% 
  mutate(min = case_when(min < 0 ~ 0,
                         TRUE ~ min))

# quadrat/presence data
present <- read.csv("./figures/CCHABs_meeting_figs/2024_data/presence_quadrat_24.csv") %>% 
  mutate(Date = ymd(Date))

# join
cover <- left_join(cover, present, by = c("taxa", "Date"))

# weird and annoying that there is an x for that one day
cover$presence[4] <- "y"
cover$quadrat[4] <- "y"

# ATX data
atx <- read.csv("./figures/CCHABs_meeting_figs/2024_data/atx_averaged_24.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  filter(!sample_type == 0) %>% 
  mutate(sample_type = case_when(sample_type == "TM" ~ "microcoleus", 
                                 sample_type == "TAC" ~ "ana_cyl"))

#### (2) Plotting ####

# theme
theme_set(theme_bw() + theme(legend.position = "bottom", 
                             panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                             panel.border = element_rect(linewidth = 1.5), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 15), axis.ticks.length=unit(.25, "cm")))

# plot
ggplot(data = cover, aes(x = Date)) +
  geom_bar(data = atx, position = "dodge", stat = "identity", 
           aes(y = ATX_all_ug_OM_g, fill = sample_type, color = sample_type, x = field_date), width = 5) +
  geom_line(data = cover, aes(y = 110 - (mean * 4), color = taxa, linetype = taxa, x = Date),
            linewidth = 1.25) +
  geom_errorbar(data = cover, aes(ymin = 110 - ((min) * 4),
                                  ymax = 110 - ((max) * 4),
                                  color = taxa), 
                linewidth = 1.25, alpha = 0.5, width = 6, position = position_dodge(width = 1.5)) +
  geom_point(data = cover, aes(y = 110 - (mean * 4), color = taxa, 
                               shape = interaction(presence, quadrat)),
             size = 2, stroke = 1.5, position = position_dodge(width = 1.5)) +
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_linetype_manual("Group", values = c("dotted", "dashed"),
                        labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  scale_shape_manual("Present / Quadrat", values = c(4, 16, 16)) +
  scale_fill_manual("Group", values = c("#d1c960","#5a88bf"),
                    labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  labs(y = NULL, x = NULL) +
  scale_x_date(limits = as.Date(c("2024-06-18", "2024-10-12"))) +
  scale_y_reverse(sec.axis = sec_axis(~ ((. - 110)/4) * -1, 
                                                          #name = "Cover (%)")) +
  )) + 
  theme(legend.position = "none")

ggsave("./figures/CCHABs_meeting_figs/bc_dynamics_2024.tiff", dpi = 600, 
       width=10, height=4.5, unit="cm") 

