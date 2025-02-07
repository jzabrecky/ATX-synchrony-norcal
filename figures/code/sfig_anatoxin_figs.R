#### Supplemental figures related to anatoxin concentrations
### Jordan Zabrecky
## last edited: 02.05.2025

# These figures show (1) the proportion of anatoxin congeners at each reach,
# (2) the difference between anatoxins normalized per organic matter vs. chl-a,
# (3) the difference in mat concentrations on our single day riffle experiment

#### (1) Loading libraries and anatoxins data ####

# loading libraries
lapply(c("tidyverse", "lubridate"), require, character.only = T)

# loading anatoxins data
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")
riffle_exp <- read.csv("./data/field_and_lab/riffle_exp_atx.csv")

# use lubridate
anatoxins$field_date <- ymd(anatoxins$field_date)
anatoxins$year <- year(anatoxins$field_date)

#### (2) Percent of congeners per site Figure ####

# summarize total percent of congeners by site
anatoxins_site <- anatoxins %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(ATXa_ug_g_percent = sum(ATXa_ug_g) / sum(ATX_all_ug_g),
                   dhATXa_ug_g_percent = sum(dhATXa_ug_g) / sum(ATX_all_ug_g),
                   HTXa_ug_g_percent = sum(HTXa_ug_g) / sum(ATX_all_ug_g))

# looking to see if there is difference per year real quick...
anatoxin_site_year <- anatoxins %>% 
  dplyr::group_by(site_reach, year) %>% 
  dplyr::summarize(ATXa_ug_g_percent = sum(ATXa_ug_g) / sum(ATX_all_ug_g),
                   dhATXa_ug_g_percent = sum(dhATXa_ug_g) / sum(ATX_all_ug_g),
                   HTXa_ug_g_percent = sum(HTXa_ug_g) / sum(ATX_all_ug_g))
# nope, no greater than 0.1 or 10% and usually closer

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
congener_fig <- ggplot(anatoxins_site_long, aes(fill = congener, y = percent, x = site_reach)) + 
  scale_fill_manual("Congeners:", labels = c("Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                    values = c("#416f16", "#62a7f8", "#ebdf38")) +
  geom_bar(position="fill", stat="identity") +
  theme_bw() +
  labs(x = "Reach", y = "Proportion of Total Detected Anatoxins") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1.2),
        text = element_text(size = 20), axis.ticks.length=unit(.25, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
congener_fig

#### (2) Normalizing by OM vs. chl-a figure ####

### WOULD LIKE TO TRY TO ORDER THIS OLDEST TO NEWEST SAMPLE

# add sample name information
anatoxins$sample_name <- paste(anatoxins$site_reach, anatoxins$field_date, sep = " ")

# scale normalization to chl-a to match that of 
anatoxins_edit <- anatoxins %>% 
  mutate(ATX_all_ug_chla_mg = ATX_all_ug_chla_ug * 1000,
         ATX_all_ug_chla_mg_scaled = ATX_all_ug_chla_mg * 12)

# separate out TAC and TM
anatoxins_edit_long <- pivot_longer(anatoxins_edit, cols = c(16, 12),
                                    values_to = "values",
                                    names_to = "normalized_type") %>% 
  select(sample_name, sample_type, normalized_type, values) 

# making figure-- joanna wants log scale
normalize_fig <- ggplot(anatoxins_edit_long, aes(x = values, y = sample_name, 
                                            fill = normalized_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_continuous(expression(paste(mu, "g ATX per g OM"), sep = ""), 
                     sec.axis = sec_axis(~.x/12, 
                                         name = expression(paste(mu, "g ATX per mg chl-a"), sep = ""))) +
  scale_fill_manual(NULL, labels = c("ATX normalized to OM", "ATX normalized to chl-a"),
                    values = c("#ebdf38", "#62a7f8")) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5)) +
  facet_wrap(~sample_type, scales = "free_y",
             labeller =  as_labeller(c(`TM` = "Microcoleus", 
                                       `TAC`= "Anabaena/Cylindrospermum"))) +
  # facet wrap edits
  theme(strip.text = element_text(face = "italic"), strip.background = element_blank())
normalize_fig
# also need to add theme stuff; and adjust scale
# still want to change order from oldest to newest sample
# and move legend bar

#### (3) Riffle experiment ####

# pivot dataframe longer and keep columns we care about
riffle_exp_long <- pivot_longer(riffle_exp, cols = c(4:5), names_to = "congener",
                                values_to = "toxin_ug_dryweight_g") %>% 
  select(site_reach, congener, toxin_ug_dryweight_g)

# color palette
grad_palette <- c("#63a8f8", "#009eba", "#009186", "#00834c", "#416f16",
                  "#597e12", "#809509", "#9ea302", "#bdb000")

# making figure
riffle_fig <- ggplot(data = riffle_exp_long, aes(x = site_reach, y = toxin_ug_dryweight_g)) +
  geom_point(aes(color = site_reach), size = 6, alpha = 0.9, stroke = 1) +
  facet_wrap(~congener, nrow = 2, scales = "free_y",
             labeller =  as_labeller(c(`ATXa_ug_g` = "Anatoxin-a (ATX-a)", 
                                       `dhATXa_ug_g`= "Dihydro-anatoxin-a (dHATX-a)"))) +
  labs(x = NULL, y = expression(paste(mu, "g toxin g"^-1, "dry weight"))) +
  scale_y_continuous(expand=expansion(mult = 0.4)) +
  scale_color_manual(values = grad_palette) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1.2),
        text = element_text(size = 20), axis.ticks.length=unit(.25, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
riffle_fig
# need to decide on point size stuff with other figures at some point before finalizing