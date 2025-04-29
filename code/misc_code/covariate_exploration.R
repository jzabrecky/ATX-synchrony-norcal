#### predictive modeling covariate exploration
### Jordan Zabrecky
## last edited: 04.25.2025

# This code explores response variables covariates that may be used in 
# predictive models

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse"), require, character.only = T)

# loading in normalized data
norm_data <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date))

# loading in raw data
raw_data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>%
  mutate(field_date = ymd(field_date))

#### (2) Plotting response variables ####

# set theme for all plots
theme_set(theme_bw())

# normalized Microcoleus cover (quadrat metric)
ggplot(data = norm_data, aes(x = field_date, y = resp_M_cover_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach), size = 3) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Microcoleus Cover (normalized per max of reach)")

# normalized Anabaena/Cylindrospermum cover (quadrat metric)
ggplot(data = norm_data, aes(x = field_date, y = resp_AC_cover_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach), size = 3) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Anabaena/Cylindrospermum Cover (normalized per max of reach)")
# may want to consider throwing out site 2 since mostly Anabaena
# accumulated on the banks at this site and the one time it appeared in
# quadrat was Cylindrospermum in upstream transect (shallow pool)

# normalized Microcoleus presence (proportion of transects)
ggplot(data = norm_data, aes(x = field_date, y = resp_M_pres_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach), size = 3) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Proportion of Transects with Microcoleus Present (normalized per max of reach)")

# normalized Anabaena/Cylindrospermum presence (proportion of transects)
ggplot(data = norm_data, aes(x = field_date, y = resp_AC_pres_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach), size = 3) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Proportion of Transects with Anabaena/Cylindrospermum Present (normalized per max of reach)")

# normalized Microcoleus anatoxin concentrations
ggplot(data = norm_data, aes(x = field_date, y = resp_M_atx_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Microcoleus anatoxin concentrations (normalized per max of reach)")
# want to consider if we want to have a statement where no anatoxins
# are predicted if there is no presence 
# (i.e. proportion of transects present == 0)

# normalized Anabaena/Cylindrospermum anatoxin concentrations
ggplot(data = norm_data, aes(x = field_date, y = resp_AC_atx_norm)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Anabaena/Cylindrospermum anatoxin concentrations (normalized per max of reach)")

#### (3) Plotting covariates ####

## Physical covariates

# temperature
ggplot(data = norm_data, aes(x = field_date, y = temp_C)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Water temperature (standardized per reach)")

# discharge
ggplot(data = norm_data, aes(x = field_date, y = discharge_m3_s)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Discharge (standardized per site)")

## Water Quality covariates

# phosphate
ggplot(data = norm_data, aes(x = field_date, y = oPhos_ug_P_L)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Phosphate (standardized per reach)")

# ammonium
ggplot(data = norm_data, aes(x = field_date, y = ammonium_mg_N_L)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Ammonium (standardized per reach)")

# nitrate
ggplot(data = norm_data, aes(x = field_date, y = nitrate_mg_N_L)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Nitrate (standardized per reach)")
# outlier for SFE-M-3 at end of July

# dissolved inorganic nitrogen (DIN; amm + nitrate)
ggplot(data = norm_data, aes(x = field_date, y = DIN_mg_N_L)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("DIN (standardized per reach)")

# conductivity
ggplot(data = norm_data, aes(x = field_date, y = cond_uS_cm)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Conductivity (standardized per reach)")

# total dissolved carbon (TDC)
ggplot(data = norm_data, aes(x = field_date, y = TDC_mg_L)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("TDC (standardized per reach)")
# very variable? likely will not use this data

# dissolved organic carbon (DOC)
ggplot(data = norm_data, aes(x = field_date, y = DOC_mg_L)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("DOC (standardized per reach)")
# decrease at end of season; likely will not use this data though

## Biological covariates

# GPP
ggplot(data = norm_data, aes(x = field_date, y = GPP_median_fourdaysprior)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("GPP (standardized per site)")

# Microcoleus cover
ggplot(data = norm_data, aes(x = field_date, y = resp_M_cover_stnd)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Microcoleus (standardized per reach)")

# Anabaena/Cylindrospermum cover
ggplot(data = norm_data, aes(x = field_date, y = resp_AC_cover_stnd)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Anabaena/Cylindrospermum (standardized per reach)")

## miscellaneous

# median depth sampled
ggplot(data = norm_data, aes(x = field_date, y = median_depth_cm_sampled)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Median Depth Sampled")

# nitrogen fixer cover
ggplot(data = norm_data, aes(x = field_date, y = other_nfixers)) +
  geom_point(aes(color = site_reach, shape = site_reach)) +
  geom_line(aes(color = site_reach)) +
  ggtitle("Median Depth Sampled")
