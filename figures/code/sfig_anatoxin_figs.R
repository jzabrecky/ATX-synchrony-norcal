#### Supplemental figures related to anatoxin concentrations
### Jordan Zabrecky
## last edited: 07.01.2025

# These figures show (1) the proportion of anatoxin congeners for each sample,
# (2) the difference between anatoxins normalized per organic matter vs. chl-a,
# (3) the difference in mat concentrations on our single day riffle experiment

#### (1) Loading libraries and anatoxins data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "ggtext", "scales"), 
       require, character.only = T)

# loading anatoxins data
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")
riffle_exp <- read.csv("./data/field_and_lab/riffle_exp_atx.csv")

# use lubridate
anatoxins$field_date <- ymd(anatoxins$field_date)
anatoxins$year <- year(anatoxins$field_date)

# create character column that includes site, reach, and field date information
anatoxins <- anatoxins %>%
  # need to mutate to get new site reach name
  mutate(new_site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                    site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                    site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                    site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                    site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S",
                                    TRUE ~ site_reach)) %>% 
  mutate(site_reach_date = paste(new_site_reach, field_date, sep = " "))

# set universal theme for all plots
theme_set(theme_bw() + theme(legend.position = "top",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1.2),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 10)))

#### (2) Proportion of congeners ####

# pivot anatoxins data to be longer (will include triplicates)
anatoxins_long <- anatoxins %>% 
  pivot_longer(c(ATXa_ug_g:HTXa_ug_g), names_to = "congener",
               values_to = "ug_g") %>% 
  select(field_date, year, site_reach, site_reach_date, sample_type, congener, ug_g) %>% 
  # only care about samples with detections
  filter(ug_g > 0)

# figure for microcoleus samples
congener_fig <- ggplot(anatoxins_long, aes(fill = congener, y = site_reach_date, x = ug_g)) + 
  scale_fill_manual("Congeners:", labels = c("Anatoxin-a (ATX-a)", "Dihydro-anatoxin-a (dHATX-a)", "Homoanatoxin-a (HTX-a)"),
                    values = c("#416f16", "#62a7f8", "#ebdf38")) +
  geom_bar(position="fill", stat="identity") +
  facet_wrap(~sample_type, scales = "free", ncol = 2,
             labeller =  as_labeller(c(`TM` = "*Microcoleus* samples", 
                                       `TAC`= "*Anabaena/Cylindrospermum* samples"))) +
  labs(x = "Proportion of Total Detected Anatoxins", y = "Reach & Date") +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(strip.text = ggtext::element_markdown()) # to get only genus name italicized
congener_fig

#### (2) Normalizing by OM vs. chl-a figure ####

# scale normalization to chl-a to match that of 
anatoxins_edit <- anatoxins %>% 
  mutate(ATX_all_ug_chla_mg = ATX_all_ug_chla_ug * 1000,
         ATX_all_ug_chla_mg_scaled = ATX_all_ug_chla_mg * 12)

# separate out TAC and TM
anatoxins_edit_long <- pivot_longer(anatoxins_edit, cols = c(16, 12),
                                    values_to = "values",
                                    names_to = "normalized_type") %>% 
  select(site_reach_date, sample_type, normalized_type, values) %>% 
  # only care about samples with detections
  filter(values > 0) 

# figure
normalize_fig <- ggplot(anatoxins_edit_long, aes(x = values, y = site_reach_date, 
                                            fill = normalized_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_continuous(expression(paste(mu, "g anatoxins per g OM"), sep = ""), 
                     trans = pseudo_log_trans(base = 10),
                     breaks = c(0, 5, 10, 25, 50, 100, 200),
                     sec.axis = sec_axis(~.x/12, 
                                         name = expression(paste(mu, "g anatoxins per mg chl-a"), sep = ""),
                                         breaks = c(0, 1, 2.5, 5, 10, 20))) +
  scale_fill_manual(NULL, labels = c("anatoxins normalized to chl-a", "anatoxins normalized to OM"),
                    values = c("#ebdf38", "#62a7f8")) +
  facet_wrap(~sample_type, scales = "free_y",
             labeller =  as_labeller(c(`TM` = "*Microcoleus* samples", 
                                       `TAC`= "*Anabaena/Cylindrospermum* samples"))) +
  labs(y = "Reach & Date of Sample") +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(strip.text = element_blank()) + # to get only genus name italicized
  # old with label.. ggtext::element_markdown()
  #theme(strip.placement = "outside") + # move facet wrap title above y-values
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), # rotate numbers
        axis.text.y = element_text(size = 9)) # make sample reach & date smaller to fit
normalize_fig

# save figure
ggsave("./figures/sfig_atx_normalized_notfinal.tiff", dpi = 600, 
       width=18, height=22, unit="cm")

#### (3) Riffle experiment ####

# color palette
grad_palette <- c("#080f2b", "#002b50", "#004967", "#00677a", "#008788", "#00aa8a", 
                  "#00cc77", "#7fe95e", "#daff47")

# making figure
riffle_fig <- ggplot(data = riffle_exp, aes(x = ATXa_ug_g, y = dhATXa_ug_g)) +
  geom_point(aes(fill = site_reach), shape = 21, size = 6, alpha = 0.9, stroke = 1) +
  labs(y = expression(paste(mu, "g dhATX-a g"^-1, "dry weight")), 
       x = expression(paste(mu, "g ATX-a g"^-1, "dry weight"))) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  scale_fill_manual(values = grad_palette) +
  theme(legend.title=element_blank(), axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12), legend.position = "right")
riffle_fig
# pair with photo in editing program
# also use scale from blue to yellow because I think when it goes back it overlaps some colors
