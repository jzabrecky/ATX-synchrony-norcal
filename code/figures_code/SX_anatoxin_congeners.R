#### Figure SX. Anatoxin and congener related figures
### Jordan Zabrecky
## last edited: 08.30.2024

# This figure shows the proportions of anatoxin congeners blah blah
# also have in here for now- ATX / afdm vs ATX / chla for samples

#### (1) Loading libraries and anatoxins data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "calecopal"), require, character.only = T)

# loading anatoxins data
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")
riffle_exp <- read.csv("./data/field_and_lab/riffle_exp_atx.csv")

# use lubridate
anatoxins$field_date <- ymd(anatoxins$field_date)

#### (2) Percent of congeners per site Figure ####

# summarize total percent of congeners by site
anatoxins_site <- anatoxins %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(ATXa_ug_g_percent = sum(ATXa_ug_g) / sum(ATX_all_ug_g),
                   dhATXa_ug_g_percent = sum(dhATXa_ug_g) / sum(ATX_all_ug_g),
                   HTXa_ug_g_percent = sum(HTXa_ug_g) / sum(ATX_all_ug_g))

# pivot longer
anatoxins_site_long <- anatoxins_site %>% 
  pivot_longer(!site_reach, names_to = "congener", values_to = "percent")

# remove sites with no anatoxins
anatoxins_site_long <- anatoxins_site_long %>% 
  filter(site_reach != "SAL-2" & site_reach != "SAL-3")

# custom ordering of site_reach
anatoxins_site_long$site_reach <- factor(anatoxins_site_long$site_reach , 
                                         levels=c("SFE-M-1S", "SFE-M-2", "SFE-M-3", "SFE-M-4", 
                                                  "SFE-SH-1S", "SAL-1S", "RUS-1S", "RUS-2", "RUS-3") )

# making figure
anatoxins_congener_stacked_bar <- ggplot(anatoxins_site_long, aes(fill = congener, 
                                                                  y = percent, x = site_reach)) + 
  scale_fill_manual("Congeners", labels = c("Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                    values = c("#416f16", "#b4e685", "#62a7f8")) +
  geom_bar(position="fill", stat="identity") +
  theme_bw() +
  labs(x = "Site-Reach", y = "Percent of Total Detected Anatoxins") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
anatoxins_congener_stacked_bar

#### (3) Congeners through time Figure ####

# mostly trial code for now :)
test <- anatoxins %>% 
  filter(site_reach == "SFE-M-3")
test_long <- pivot_longer(test, c(6:9), names_to = "congener", values_to = "ug_g_dryweight") %>% 
  filter(sample_type == "TM") %>% 
  filter(field_date >= as.Date("2023-01-01")) %>% 
  select(field_date, site_reach, congener, ug_g_dryweight)

# would probably want to do an if the site does not include HTX do not include at all????

anatoxins_through_time <- ggplot(test_long, aes(x = field_date, y = ug_g_dryweight,
                                                color = congener, fill = congener, shape = congener)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_point(size = 2.5) +
  scale_color_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = cal_palette("kelp2", n = 4, type = "continuous")) +
  scale_fill_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = cal_palette("kelp2", n = 4, type = "continuous")) +
  scale_shape_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = c(23, 24, 22, 21)) +
  labs(x = NULL, y =  "Micrograms toxin / grams dry weight", title = "SFE-M-3 Microcoleus") +
  theme_bw()
anatoxins_through_time

# murgh need to figure out colors
