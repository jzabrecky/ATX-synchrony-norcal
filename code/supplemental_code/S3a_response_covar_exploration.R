#### predictive modeling covariate exploration
### Jordan Zabrecky
## last edited: 07.02.2025

# This code explores response variables covariates that may be used in 
# predictive models

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse"), require, character.only = T)

# loading in normalized/standardized data
norm_data <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date))

# loading in raw data
raw_data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

#### (2) Plotting response variables ####

# palette for plots
palette <- c("#080f2b","#00677a", "#00aa8a", "#00cc77", "#daff47")

# set theme
theme_set(theme_bw() + theme(panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1.2),
                             text = element_text(size = 15), axis.ticks.length=unit(.25, "cm"),
                             axis.text.y = element_text(size = 10)))

# code template (not easily making it into a function at the present)
# need to fill in y & grouping & ylab & title
ggplot(data = norm_data, aes(x = field_date, y = y)) +
  geom_point(aes(color = grouping, shape = grouping, fill = grouping), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = grouping), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab(ylab_title) +
  ggtitle(title) +
  theme(legend.position = element_blank())

# plotting function

# normalized Microcoleus cover (quadrat metric)
norm_Micro <- ggplot(data = norm_data, aes(x = field_date, y = resp_M_cover_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Normalized to % Maximum at Reach") +
  ggtitle("Microcoleus Cover") +
  theme(legend.position = "none")
norm_Micro

# normalized Anabaena/Cylindrospermum cover (quadrat metric)
norm_AnaCyl <- ggplot(data = norm_data, aes(x = field_date, y = resp_AC_cover_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Normalized to % Maximum at Reach") +
  ggtitle("Anabaena/Cylindrospermum Cover") +
  theme(legend.position = "none")
norm_AnaCyl
# may want to consider throwing out site 2 since mostly Anabaena
# accumulated on the banks at this site and the one time it appeared in
# quadrat was Cylindrospermum in upstream transect (shallow pool)

# normalized Microcoleus presence (proportion of transects)
norm_Micro_pres <- ggplot(data = norm_data, aes(x = field_date, y = resp_M_pres_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Normalized to % Maximum at Reach") +
  ggtitle("Microcoleus Presence (Proportion of Transects Present)") +
  theme(legend.position = "none")
norm_Micro_pres

# normalized Anabaena/Cylindrospermum presence (proportion of transects)
norm_AnaCyl_pres <- ggplot(data = norm_data, aes(x = field_date, y = resp_AC_pres_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Normalized to % Maximum at Reach") +
  ggtitle("Anabaena/Cylindrospermum Presence (Proportion of Transects Present)") +
  theme(legend.position = "none")
norm_AnaCyl_pres

# normalized Microcoleus anatoxin concentrations
norm_Micro_atx <- ggplot(data = norm_data, aes(x = field_date, y = resp_M_atx_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Normalized to % Maximum at Reach") +
  ggtitle("Microcoleus ATX") +
  theme(legend.position = "none")
norm_Micro_atx
# want to consider if we want to have a statement where no anatoxins
# are predicted if there is no presence 
# (i.e. proportion of transects present == 0)

# normalized Anabaena/Cylindrospermum anatoxin concentrations
norm_AnaCyl_atx <- ggplot(data = norm_data, aes(x = field_date, y = resp_AC_atx_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Normalized to % Maximum at Reach") +
  ggtitle("Anabaena/Cylindrospermum ATX") +
  theme(legend.position = "none")
norm_AnaCyl_atx

#### (3) Plotting covariates ####

## Physical covariates

# temperature
temperature <- ggplot(data = norm_data, aes(x = field_date, y = temp_C)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Temperature") +
  theme(legend.position = "none")
temperature

# discharge
discharge <- ggplot(data = norm_data, aes(x = field_date, y = discharge_m3_s)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Discharge") +
  theme(legend.position = "none")
discharge

## Water Quality covariates

# phosphate
phosphate <- ggplot(data = norm_data, aes(x = field_date, y = oPhos_ug_P_L)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Orthophosphate") +
  theme(legend.position = "none")
phosphate

# ammonium
ammonium <- ggplot(data = norm_data, aes(x = field_date, y = ammonium_mg_N_L)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Ammonium") +
  theme(legend.position = "none")
ammonium

# nitrate
nitrate <- ggplot(data = norm_data, aes(x = field_date, y = nitrate_mg_N_L)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Nitrate") +
  theme(legend.position = "none")
nitrate
# outlier for SFE-M-3 at end of July

# dissolved inorganic nitrogen (DIN; amm + nitrate)
DIN <- ggplot(data = norm_data, aes(x = field_date, y = DIN_mg_N_L)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Dissolved Inorganic Nitrogen (DIN)") +
  theme(legend.position = "none")
DIN

# conductivity
conductivity <- ggplot(data = norm_data, aes(x = field_date, y = cond_uS_cm)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Conductivity") +
  theme(legend.position = "none")
conductivity

# total dissolved carbon (TDC)
TDC <- ggplot(data = norm_data, aes(x = field_date, y = TDC_mg_L)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Total Dissolved Carbon (TDC)") +
  theme(legend.position = "none")
TDC
# very variable? likely will not use this data

# dissolved organic carbon (DOC)
DOC <- ggplot(data = norm_data, aes(x = field_date, y = TDC_mg_L)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Dissolved Organic Carbon (DOC)") +
  theme(legend.position = "none")
DOC
# decrease at end of season; likely will not use this data though

## Biological covariates

# GPP
TDC <- ggplot(data = norm_data, aes(x = field_date, y = TDC_mg_L)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Total Dissolved Carbon (TDC)") +
  theme(legend.position = "none")
TDC
# very variable? likely will not use this data

# dissolved organic carbon (DOC)
GPP <- ggplot(data = norm_data, aes(x = field_date, y = GPP_median_tofourdaysprior)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Gross Primary Productivity (GPP)") +
  theme(legend.position = "none")
GPP

# Microcoleus cover
micro_cover <- ggplot(data = norm_data, aes(x = field_date, y = microcoleus)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Microcoleus Cover") +
  theme(legend.position = "none")
micro_cover

# Anabaena/Cylindrospermum cover
anacyl_cover <- ggplot(data = norm_data, aes(x = field_date, y = anabaena_cylindrospermum)) +
  geom_point(aes(color = site_reach, shape = site_reach, fill = site_reach), 
             size = 4.5, alpha = 0.8) +
  geom_line(aes(color = site_reach), alpha = 0.8, linewidth = 1.25) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) +
  scale_color_manual(values = palette) + 
  xlab(NULL) +
  ylab("Standardized per Reach") +
  ggtitle("Anabaena/Cylindrospermum Cover") +
  theme(legend.position = "none")
anacyl_cover

#### (4) Checking for covariance ####

# covariance matrix
cov(norm_data[,4:23] %>% na.omit())

## notes:
# none appear to be greater than 0.7 except nitrate w/ DIN 
# this is to be expected; we are just using DIN in place of nitrate