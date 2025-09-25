#### Supplemental figures showing parameter estimates from predictive models
### Jordan Zabrecky
## last edited: 9.24.25

## This code makes figures to show our parameter estimates for all models (supplemental)
## and is used when pulling in data to make figures 6 & 7

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse","cowplot", "plyr", "ggtext", "ggallin"), 
       require, character.only = T)

# loading data 
param_est <- read.csv("./data/predictive_models/parameter_est_allmodels.csv") %>% 
  mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S")) %>% 
  # factor model column to get desired order
  mutate(model_f = factor(model, levels = c("physical", "physical_w_cover",
                                            "chemical", "chemical_w_cover", 
                                            "biological", "biological_w_cover",
                                            "physicochemical", "physicochemical_w_cover",
                                            "ecohydrological", "ecohydrological_w_cover",
                                            "biochemical", "biochemical_w_cover", 
                                            "all", "all_w_cover"))) %>%
  # do same for parameters (ggplot will go from bottom to top, so reversed)
  mutate(parameters_f = factor(parameters, levels = c("Cover", "GPP", "Conductivity",
                                                      "OPhos", "DIN", "Discharge", 
                                                      "Temperature"))) %>% 
  mutate(model_f = case_when(grepl("w_cover", model_f) ~ paste(gsub("_w_cover", " with cover", model_f)),
                           TRUE ~ model_f))

# split into list based on what is being predicted 
param_est_list <- split(param_est, param_est$predicting) 

#### (2) Making supplemental figure ####

# set universal theme
theme_set(theme_bw() + theme(legend.position = "top",
                             panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.title.y = ggtext::element_markdown(size = 10), 
                             axis.text.x = element_text(size = 10),
                             axis.text.y = element_text(size = 10),
                             plot.title = ggtext::element_markdown(size = 10, hjust = 0.5),
                             strip.text = element_text(face="bold", size=10)))

# palette
palette <- c("#1E426B","#377B76", "#57C785", "#A2D366", "#E8DE48")

# (doing separate plots for each as they will each have their own breaks)

## microcoleus cover
ggplot(data = param_est_list[[4]], aes(y = parameters_f)) +
  geom_hline(yintercept = (1:5)+0.5, linetype = "dashed", color = "gray") +
  geom_point(aes(x = mean, color = site_reach), position = position_dodge(width = 1)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = site_reach),
                position = position_dodge(width = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = palette) +
  scale_x_continuous(trans = pseudolog10_trans,
                     breaks = c(-15, -5, -1, 0, 1)) +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(legend.position = "none") +
  labs(x = "posterior estimates", y = "", title = "Posterior Estimates of Models Predicting *Microcoleus* Cover") +
  facet_wrap(~model_f)

# saving plot
ggsave(paste("./figures/sfig_paramest_m_cover_notfinal.tiff", sep = ""), 
       dpi = 600, width = 18, height = 14, unit = "cm")

## anabaena/cylindrospermum cover
ggplot(data = param_est_list[[2]], aes(y = parameters_f)) +
  geom_hline(yintercept = (1:5)+0.5, linetype = "dashed", color = "gray") +
  geom_point(aes(x = mean, color = site_reach), position = position_dodge(width = 1)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = site_reach),
                position = position_dodge(width = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = palette) +
  scale_x_continuous(trans = pseudolog10_trans,
                     breaks = c(-5, -1, 0, 1, 5)) +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(legend.position = "none") +
  labs(x = "posterior estimates", y = "", title = "Posterior Estimates of Models Predicting *Anabaena/Cylindrospermum* Cover") +
  facet_wrap(~model_f)

# save plot    
ggsave(paste("./figures/sfig_paramest_ac_cover_notfinal.tiff", sep = ""), 
       dpi = 600, width = 18, height = 14, unit = "cm")

## microcoleus ATX
ggplot(data = param_est_list[[3]], aes(y = parameters_f)) +
  geom_hline(yintercept = (1:6)+0.5, linetype = "dashed", color = "gray") +
  geom_point(aes(x = mean, color = site_reach), position = position_dodge(width = 1)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = site_reach),
                position = position_dodge(width = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = palette) +
  scale_x_continuous(trans = pseudolog10_trans,
                     breaks = c(-50, -10, 0, 10,50)) +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(legend.position = "none") +
  labs(x = "posterior estimates", y = "", title = "Posterior Estimates of Models Predicting *Microcoleus* Mat Anatoxins") +
  facet_wrap(~model_f)

# save plot    
ggsave(paste("./figures/sfig_paramest_m_atx_notfinal.tiff", sep = ""), 
       dpi = 600, width = 18, height = 21, unit = "cm")

## Anabaena/Cylindrospermum ATX
ggplot(data = param_est_list[[1]], aes(y = parameters_f)) +
  geom_hline(yintercept = (1:6)+0.5, linetype = "dashed", color = "gray") +
  geom_point(aes(x = mean, color = site_reach), position = position_dodge(width = 1)) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper, color = site_reach),
                position = position_dodge(width = 1)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = palette) +
  scale_x_continuous(trans = pseudolog10_trans,
                     breaks = c(-50, -10, 0, 10, 50)) +
  theme(strip.background = element_blank()) + # get rid of gray background for facet title
  theme(legend.position = "none") +
  labs(x = "posterior estimates", y = "", title = "Posterior Estimates of Models Predicting *Anabaena/Cylindrospermum* Mat Anatoxins") +
  facet_wrap(~model_f)

# save plot    
ggsave(paste("./figures/sfig_paramest_ac_atx_notfinal.tiff", sep = ""), 
       dpi = 600, width = 18, height = 21, units = "cm")
