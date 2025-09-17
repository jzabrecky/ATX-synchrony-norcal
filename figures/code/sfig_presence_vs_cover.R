#### Figure to show difference between presence/absence data and cover
### Jordan Zabrecky
## last edited: 08.08.2025

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "cowplot"), 
       require, character.only = T)

# loading data
cover <- read.csv("./data/field_and_lab/percover_byreach.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date)) %>% 
  # change site_reach names to new %>% 
  mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S",
                                TRUE ~ site_reach))

#### (2) Making figures ####

# set universal theme
theme_set(theme_bw() + theme(legend.position = "bottom",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.text.y = element_text(size = 10), axis.text.x = element_text(size = 10),
                             legend.text = element_text(size = 10)))

# color palette
palette <- c("#1E426B","#377B76", "#57C785", "#A2D366", "#E8DE48")

## Microcoleus

# salmon 2022
sal22 <- ggplot(data = cover %>% filter(site == "SAL" & year == 2022),
                aes(x = proportion_micro_transects,
                    y = microcoleus)) +
  geom_point(aes(fill = site_reach, color = site_reach, shape = site_reach), size = 3) +
  scale_fill_manual(NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_color_manual(NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_shape_manual(NULL, values = c(21, 23, 25)) +
  coord_flip() # decided it looked better like this; easier to do than changing x & y in aes()
sal22

# south fork eel 2022
sfe22 <- ggplot(data = cover %>% filter(site == "SFE-M" & year == 2022),
                aes(x = proportion_micro_transects,
                    y = microcoleus)) +
  geom_point(aes(color = site_reach, fill = site_reach, shape = site_reach), size = 3) +
  scale_color_manual(NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_fill_manual(NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_shape_manual(NULL, values = c(21, 23, 25)) +
  coord_flip() # decided it looked better like this; easier to do than changing x & y in aes()
sfe22

# south fork eel 2023
sfe23 <- ggplot(data = cover %>% filter(site == "SFE-M" | site == "SFE-SH" & year == 2023),
                aes(x = proportion_micro_transects,
                    y = microcoleus)) +
  geom_point(aes(color = site_reach, fill = site_reach, shape = site_reach), size = 3) +
  scale_color_manual(NULL, values = palette) +
  scale_fill_manual(NULL, values = palette) +
  scale_shape_manual(NULL, values = c(21, 22, 23, 24, 25)) +
  coord_flip() # decided it looked better like this; easier to do than changing x & y in aes()
sfe23


## Anabaena/Cylindrospermum

# russian
rus22 <- ggplot(data = cover %>% filter(site == "RUS"), aes(x = proportion_ana_cyl_transects,
                                                            y = anabaena_cylindrospermum)) +
  geom_point(aes(color = site_reach, fill = site_reach, shape = site_reach), size = 3) +
  scale_color_manual(NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_fill_manual(NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_shape_manual(NULL, values = c(21, 23, 25)) +
  coord_flip() # decided it looked better like this; easier to do than changing x & y in aes()
rus22

# south fork eel 2022
sfe22_a <- ggplot(data = cover %>% filter(site == "SFE-M" & year == 2022),
                aes(x = proportion_ana_cyl_transects,
                    y = anabaena_cylindrospermum)) +
  geom_point(aes(color = site_reach, fill = site_reach, shape = site_reach), size = 3) +
  scale_color_manual(NULL, NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_fill_manual(NULL, NULL, values = c(palette[1], palette[3], palette[5])) +
  scale_shape_manual(NULL, NULL, values = c(21, 23, 25)) +
  coord_flip() # decided it looked better like this; easier to do than changing x & y in aes()
sfe22_a + coord_flip()

# south fork eel 2023
sfe23_a <- ggplot(data = cover %>% filter(site == "SFE-M" | site == "SFE-SH" & year == 2023),
                aes(x = proportion_ana_cyl_transects,
                    y = anabaena_cylindrospermum)) +
  geom_point(aes(color = site_reach, fill = site_reach, shape = site_reach), size = 3) +
  scale_color_manual(NULL, NULL, values = palette) +
  scale_fill_manual(NULL, NULL, values = palette) +
  scale_shape_manual(NULL, NULL, values = c(21, 22, 23, 24, 25)) +
  coord_flip() # decided it looked better like this; easier to do than changing x & y in aes()
sfe23_a

#### (3) Putting Plots together ####

# microcoleus on left and anabaena on right; 2023 highest
all <- plot_grid(sfe23, sfe23_a, sfe22, sfe22_a, sal22, rus22, ncol = 2, align = "v")  +
  theme(plot.background = element_rect(fill = "white", color = "white"))

all

# saving plot
ggsave("./figures/sfig_presence_vs_cover_notfinal.tiff", dpi = 600, 
       width=18, height=20, unit="cm")

# separate for cleaner legend
legend <- plot_grid(sfe23, sfe22, rus22, sal22, ncol = 1)
legend

# save
ggsave("./figures/sfig_presence_vs_cover_legend.tiff", dpi = 600, 
       width=17.75, height=20, unit="cm")
