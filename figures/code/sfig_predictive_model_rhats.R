#### Supplemental figure to show convergence of all covariate r-hats
### Jordan Zabrecky
## last edited: 09.19.2025

## This supplemental figure shows that the r-hat for all covariates in all
## predictive models for every omitted reach is below 1.05

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "ggtext"), 
       require, character.only = T)

# read in all rhats 
rhats <- read.csv("./data/predictive_models/rhats_allmodels.csv") %>% 
  pivot_longer(cols = c(2:6), names_to = "site_reach", values_to = "rhat") %>% 
  mutate(site_reach = case_when(site_reach == "SFE.M.1S" ~ "SFE-Lower-1S",
                                site_reach == "SFE.M.2" ~ "SFE-Lower-2",
                                site_reach == "SFE.M.3" ~ "SFE-Lower-3",
                                site_reach == "SFE.M.4" ~ "SFE-Lower-4",
                                site_reach == "SFE.SH.1S" ~ "SFE-Upper-1S"),
         predicting = case_when(predicting == "AC_atx" ~ "*Anabaena/Cylindrospermum* Mat Anatoxin Models",
                                predicting == "AC_cover" ~ "*Anabaena/Cylindrospermum* Cover Models",
                                predicting == "M_atx" ~ "*Microcoleus* Mat Anatoxin Models",
                                predicting == "M_cover" ~ "*Microcoleus* Cover Models")) %>% 
  # turning parameters & predicting into a factor; not including lp_ for parameters
  mutate(parameters_f = factor(parameters, levels = c("b0", "Temperature", "Discharge", 
                                                      "DIN", "OPhos", "Conductivity", "GPP",
                                                      "sigma")),
         predicting_f = factor(predicting, levels = c("*Microcoleus* Cover Models", 
                                                      "*Anabaena/Cylindrospermum* Cover Models",
                                                      "*Microcoleus* Mat Anatoxin Models",
                                                      "*Anabaena/Cylindrospermum* Mat Anatoxin Models"))) %>% 
  na.omit()

#### (2) Making figure ####

# set universal theme
theme_set(theme_bw() + theme(legend.position = "top",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.title.y = ggtext::element_markdown(size = 10), 
                             axis.text.x = element_text(size = 10),
                             axis.text.y = element_text(size = 10),
                             plot.title = ggtext::element_markdown(size = 10, hjust = 0.5),
                             strip.text = element_text(face="bold", size=10),
                             strip.background = element_blank()))

# just going to use viridis as color palette
# making figure
figure <- ggplot(data = rhats, aes(x = parameters_f, y = rhat)) +
  facet_grid(site_reach~predicting_f) +
  geom_hline(yintercept = 1.05, linetype = "dashed", color = "red", linewidth = 1.02) +
  geom_jitter(aes(color = parameters_f), alpha = 0.5) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  theme(legend.position = "none") +
  theme(strip.text = ggtext::element_markdown()) +
  labs(x = "Parameters", y = "R-hats")
figure

# save figure
ggsave(paste("./figures/sfig_prediction_rhats_notfinal.tiff"), dpi = 600,
       width = 17.6, height = 18, unit = "cm")
