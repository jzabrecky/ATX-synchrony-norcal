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
  labs(x = "Site-Reach", y = "Proportion of Total Detected Anatoxins") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
anatoxins_congener_stacked_bar

#### (3) Congeners through time Figure ####

# add year information
anatoxins <- anatoxins %>% 
  mutate(year = year(field_date))

# pivoting long and subsampling for sample type
anatoxins_long_TM <- pivot_longer(anatoxins, c(6:9), names_to = "congener", values_to = "ug_g_dryweight") %>% 
  filter(sample_type == "TM") %>% 
  select(field_date, year, site_reach, congener, ug_g_dryweight) %>% 
  filter(site_reach != "SAL-1S" & site_reach != "SAL-2" & site_reach != "SAL-3")

anatoxins_long_TAC <- pivot_longer(anatoxins, c(6:9), names_to = "congener", values_to = "ug_g_dryweight") %>% 
  filter(sample_type == "TAC") %>% 
  select(field_date, year, site_reach, congener, ug_g_dryweight) %>% 
  filter(site_reach != "SAL-1S" & site_reach != "SAL-2" & site_reach != "SAL-3")

# facet-wrap for TM samples
anatoxins_through_time_TM <- ggplot(anatoxins_long_TM, aes(x = field_date, y = ug_g_dryweight,
                                                color = congener, fill = congener, shape = congener)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_point(size = 2.5) +
  scale_color_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = cal_palette("kelp2", n = 4, type = "continuous")) +
  scale_fill_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = cal_palette("kelp2", n = 4, type = "continuous")) +
  scale_shape_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = c(23, 24, 22, 21)) +
  labs(x = NULL, y =  "Micrograms toxin / grams dry weight", title = "Microcoleus samples") +
  theme_bw() +
  facet_wrap(~site_reach+year, scales ="free")
anatoxins_through_time_TM

# facet-wrap for TAC samples
anatoxins_through_time_TAC <- ggplot(anatoxins_long_TAC, aes(x = field_date, y = ug_g_dryweight,
                                                           color = congener, fill = congener, shape = congener)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_point(size = 2.5) +
  scale_color_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = cal_palette("kelp2", n = 4, type = "continuous")) +
  scale_fill_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                    values = cal_palette("kelp2", n = 4, type = "continuous")) +
  scale_shape_manual("Congeners", labels = c("Total anatoxins", "Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                     values = c(23, 24, 22, 21)) +
  labs(x = NULL, y =  "Micrograms toxin / grams dry weight", title = "Anabaena + Cylindrospermum samples") +
  theme_bw() +
  facet_wrap(~site_reach+year, scales ="free")
anatoxins_through_time_TAC

#### Figure: Compare chl-a and afdm ####

# add sample name information
anatoxins$sample_name <- paste(anatoxins$site_reach, anatoxins$field_date, anatoxins$sample_type, sep = " ")

# scale chlorophyll-a
anatoxins <- anatoxins %>% 
  mutate(ATX_all_ug_chla_g_scaled = ATX_all_ug_chla_g * 14)

# separate out TAC and TM
anatoxins_long <- pivot_longer(anatoxins, cols = c(13, 16), values_to = "values", names_to = "type") %>% 
  select(sample_name, sample_type, type, values) %>% 
  filter(values > 0)

# double bar graph
dbl_bar_graph <- ggplot(anatoxins_long, aes(x = values, y = sample_name, fill = type)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_continuous("ug ATX per g AFDM", sec.axis = sec_axis(~.x/15, name="ug ATX per g Chl-a")) +
  scale_fill_manual(NULL, labels = c("Anatoxins per AFDM", "Anatoxins per chl-a"),
                    values = c("#fc5d78", "#09dede")) +
  labs(x = "ug Anatoxins / g AFDM", y = "Sample", title = "ATX per chl-a vs. AFDM") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5)) +
  facet_wrap(~sample_type, scales ="free")
dbl_bar_graph
