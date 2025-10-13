#### Supplemental figure showing relative abundance of M and A/C in target samples
### Jordan Zabrecky
## last edited: 10.13.2025

## This is a supplemental figure that shows the relative abundance of Microcoleus
## and Anabaena/Cylindrospermum in targeted samples respectively (excluding non-algal portion)

## Microscopy data was processed from EDI package in the ATX-community-norcal project
## https://github.com/jzabrecky/ATX-community-norcal/blob/main/code/pre_EDI_data_gathering/pre_EDI_1a_putting_together_microscopy.R
## exclusion of nonalgal portion was calculated here:
## https://github.com/jzabrecky/ATX-community-norcal/blob/main/code/1b_processing_microscopy.R

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "plyr"), 
       require, character.only = T)

# reading in microscopy data
microscopy <- ldply(list.files(path = "./data/field_and_lab/", pattern = "algalonly"), function(filename) {
  d <- read.csv(paste("data/field_and_lab/", filename, sep = "")) %>% 
    # need to mutate to get new site reach name
    mutate(new_site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                      site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                      site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                      site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                      site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S",
                                      TRUE ~ site_reach)) %>% 
    # column for "target" taxon depending on taxon
    #mutate(target_taxon = case_when(sample_type == "TM" ~ microcoleus,
    #                                sample_type == "TAC" ~ anabaena_and_cylindrospermum)) %>% 
    # site reach and date in one column
    mutate(site_reach_date = paste(new_site_reach, field_date, sep = " ")) %>% 
    relocate(site_reach_date, .before = site_reach) %>% 
    relocate(new_site_reach, .before = site_reach)
  return(d)
})

# pivot longer
microscopy_longer <- microscopy %>% 
  pivot_longer(7:ncol(microscopy), names_to = "taxa", values_to = "percent")
  
#### (2) Making Figure ####

# set universal theme for all plots
theme_set(theme_bw() + theme(legend.position = "top",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 10)))

# pivot longer and trim down categories
microscopy_longer <- microscopy %>% 
  pivot_longer(7:ncol(microscopy), names_to = "taxa", values_to = "percent") %>% 
  mutate(taxa = case_when(taxa == "microcoleus" ~ "microcoleus",
                          taxa == "anabaena_and_cylindrospermum" ~ 
                            "anabaena_and_cylindrospermum",
                          taxa == "non_e_diatoms" ~ "diatoms",
                          taxa == "e_diatoms" ~ "diatoms",
                          taxa == "green_algae" ~ "green_algae",
                          TRUE ~ "other_cyanobacteria")) %>% 
  mutate(taxa_f = factor(taxa, levels = c("other_cyanobacteria", "diatoms", "green_algae",
                                          "anabaena_and_cylindrospermum", "microcoleus")))

# color palette
palette <- c("#1E426B","#377B76", "#57C785", "#A2D366", "#E8DE48")

# figure for microcoleus samples
microscopy_fig <- ggplot(microscopy_longer,
                              aes(fill = taxa_f, y = site_reach_date, x = percent)) + 
  geom_bar(stat="identity") +
  facet_wrap(~sample_type, scales = "free", ncol = 2,
             labeller =  as_labeller(c(`TM` = "*Microcoleus* samples", 
                                       `TAC`= "*Anabaena/Cylindrospermum* samples"))) +
  scale_fill_manual(labels = c("Other Cyanobacteria", "Diatoms", "Green Algae", 
                                  "*Anabaena* and *Cylindrospermum*", "*Microcoleus*"), 
                    values = palette) +
  labs(x = "Percent of Sample Algal Composition", y = "Reach & Date") +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(strip.text = ggtext::element_markdown(),
        legend.position = "none") # to get only genus name italicized
microscopy_fig

# save figure
ggsave("./figures/sfig_microscopy_composition_not_final.tiff", dpi = 600, 
       width=18, height=21, unit="cm")

microscopy_legend <- ggplot(microscopy_longer,
                         aes(fill = taxa_f, y = site_reach_date, x = percent)) + 
  geom_bar(stat="identity") +
  facet_wrap(~sample_type, scales = "free", ncol = 2,
             labeller =  as_labeller(c(`TM` = "*Microcoleus* samples", 
                                       `TAC`= "*Anabaena/Cylindrospermum* samples"))) +
  scale_fill_manual(labels = c("Other Cyanobacteria", "Diatoms", "Green Algae", 
                               "*Anabaena* and *Cylindrospermum*", "*Microcoleus*"), 
                    values = palette) +
  labs(x = "Percent of Sample Algal Composition", y = "Reach & Date") +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(strip.text = ggtext::element_markdown(),
        legend.text = ggtext::element_markdown(),
        legend.position = "right") # to get only genus name italicized
microscopy_legend

# save figure
ggsave("./figures/sfig_microscopy_composition_legend.tiff", dpi = 600, 
       width=18, height=22, unit="cm")


#### (3) What is mean & range of each sample ? ####

## Microcoleus?
min((microscopy %>% filter(sample_type == "TM"))$microcoleus) # 30.38462
max((microscopy %>% filter(sample_type == "TM"))$microcoleus) # 94.84536
mean((microscopy %>% filter(sample_type == "TM"))$microcoleus) # 75.00533

## Anabaena/Cylindrospermum
min((microscopy %>% filter(sample_type == "TAC"))$anabaena_and_cylindrospermum) # 13.21429
max((microscopy %>% filter(sample_type == "TAC"))$anabaena_and_cylindrospermum) # 75.74219
mean((microscopy %>% filter(sample_type == "TAC"))$anabaena_and_cylindrospermum) # 35.90746

## curious about the amount of green algae in these samples
min((microscopy %>% filter(sample_type == "TAC"))$green_algae) # 2.734375
max((microscopy %>% filter(sample_type == "TAC"))$green_algae) # 70.67138
mean((microscopy %>% filter(sample_type == "TAC"))$green_algae) # 24.98525
