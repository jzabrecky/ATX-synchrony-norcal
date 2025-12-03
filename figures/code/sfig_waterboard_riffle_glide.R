#### Supplemental figure comparing individual M. mats from riffle versus pool
### Jordan Zabrecky
## last edited: 07.29.2025

# This script creates a supplementary figure to show the difference 
# between anatoxin concentrations in individual mat samples in pools
# versus riffles using data from the California Water Board (with permission!)

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "cowplot"), 
       require, character.only = T)

# loading data
data <- read.csv("./data/field_and_lab/state_monitoring_mats.csv") %>% 
  mutate(date = mdy(date))

#### (2) Making plot ####

# set universal theme
theme_set(theme_bw() + 
            theme(legend.position = "top",
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                  text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                  axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
                  legend.text = element_text(size=10)))

# no scientific notion!
options(scipen=10000)

# plot
fig <- ggplot(data = data, aes(x = date, y = ATXa_ug_L)) +
  geom_point(aes(fill = habitat), shape = 21, size = 4, alpha = 0.8, stroke = 0.8) +
  scale_y_continuous(trans='log10') +
  scale_fill_manual(NULL, 
                   labels = c("epiphyte in pool", "cobble in riffle"),
                   values = c("#ebdf38", "#62a7f8")) +
  labs(x = NULL, y = NULL) + 
  theme(legend.position = "bottom")
fig  

# pair with photo of epiphyte in pool habitat in Inkscape
ggsave("./figures/sfig_atx_epiphyte_cobble_notfinal.tiff", dpi = 600, 
       width=9, height=9, unit="cm")
