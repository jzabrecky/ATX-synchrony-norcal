#### Supplemental figures showing normalized covariates/predictors
### Jordan Zabrecky
## last edited: 08.06.2025

# This (large figure) shows all predictors standardized for Q3

#### (1) Loading libraries and data ####

# Luckily, we already did most of this work on a supplemental script looking
# at covariates...
source("./code/supplemental_code/S3a_response_covar_exploration.R")

#### (2) Putting together plots and saving ####

# ordering according to model groupings
all <- plot_grid(temperature, discharge, NULL, DIN, phosphate, conductivity,
          GPP, NULL, NULL, micro_cover, anacyl_cover, NULL, ncol = 3) +
  theme(plot.background = element_rect(fill = "white", color = "white"))

# save
ggsave("./figures/sfig_standardized_predictors_notfinal.tiff", dpi = 600, 
       width=16.5, height=20, unit="cm")

# save legend
legend + theme(legend.text = element_text(size = 10))
ggsave("./figures/sfig_standardized_predictors_legend.tiff", dpi = 600,
       width = 8, height = 5)
