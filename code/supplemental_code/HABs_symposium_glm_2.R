#### script for glms for HABs symposium poster
### Jordan Zabrecky
## last edited: 10.09.2024

# This script makes a generalized linear model (glm) for each site
# and plots the effect size for each variable for poster for 12th
# U.S. Symposium on Harmful Algae

# This script analyzes reaches as indiviuals rather than averaged across

#### Loading libraries and data ####

# loading libraries
lapply(c("lubridate", "plyr", "tidyverse", "lme4", "dataRetrieval", "sjPlot"), require, character.only = T)

##  use dataRetrieval to get daily discharge (rather than continuous from our miniDOT inputs!)
# USGS site numbers
USGS_gages <- c("11463000", "11522500", "11476500", "11475800")

# mean daily discharge in cfs
param <- "00060"

# use "DataRetrieval" to download data (rather than continuous on our miniDOTs!)
discharge <- lapply(USGS_gages, function(x) readNWISdv(x, param, "2022-06-15","2023-10-01"))

# loading smaller datasets
nutrients <- read.csv("./data/field_and_lab/water_chemistry.csv")
occurence <- read.csv("./data/field_and_lab/percover_byreach.csv")
anatoxins <- read.csv("./data/field_and_lab/cyano_atx.csv")

# convert dates from character to date object
nutrients$field_date <- ymd(nutrients$field_date)
occurence$field_date <- ymd(occurence$field_date)
anatoxins$field_date <- ymd(anatoxins$field_date)

#### (2) Joining all into a single dataframe ####

# function to clean discharge data
clean_discharge <- function(df) {
  new_df <- df %>% 
    dplyr::rename(field_date = Date) %>% 
    mutate(discharge_cms = X_00060_00003 / 35.31) %>% 
    mutate(site = case_when(site_no == 11463000 ~ "RUS",
                            site_no == 11522500 ~ "SAL" ,
                            site_no == 11476500 ~ "SFE-M",
                            site_no == 11475800 ~ "SFE-SH")) %>% 
    dplyr::select(field_date, discharge_cms, site)
  return(new_df)
}

# applying function to dataframe list
clean_discharge <- lapply(discharge, function(x) clean_discharge(x))

# make empty dataframe for all discharge
discharge_all <- data.frame()

# make discharge into one dataframe
for(i in 1:length(discharge)) {
  discharge_all <- bind_rows(discharge_all, clean_discharge[[i]])
}

# add site_year column to occurence df
occurence_final <- occurence %>% 
  mutate(year = year(field_date)) %>%
  mutate(micro_ana_cyl = microcoleus + anabaena_cylindrospermum) %>% 
  mutate(site_year = case_when((site == "SFE-M" & year == 2022) ~ "sfkeel_mir_2022",
                               (site == "SFE-M" & year == 2023) ~ "sfkeel_mir_2023",
                               (site == "SAL" & year == 2022) ~ "salmon_2022",
                               (site == "SAL" & year == 2023) ~ "salmon_2023",
                               (site == "RUS") ~ "russian_2022",
                               (site == "SFE-SH") ~ "sfkeel_sth_2023"))

# average TM and TA of same site_reach on same field date
anatoxins_final <- anatoxins %>% 
  dplyr::group_by(site_reach, field_date) %>% 
  dplyr::summarize(ATX_all_ug_afdm_g_average = mean(ATX_all_ug_afdm_g))

# left join in nutrients data
all <- left_join(occurence_final, nutrients, by = c("field_date", "site_reach", "site", "reach"))
all <- left_join(all, discharge_all, by = c("field_date", "site"))
all <- left_join(all, anatoxins_final, by = c("field_date", "site_reach"))

# remove micronutrient columns, DO_mgL, and pH
final <- all[,c(-19, -21, -26, -30:-36)] %>% 
  select(field_date, site_reach, site_year, site, micro_ana_cyl, ATX_all_ug_afdm_g_average,
         discharge_cms, temp_C, cond_uS_cm, oPhos_ug_P_L, nitrate_mg_N_L, ammonium_mg_N_L)

# split by different
final_site_year <- split(final, final$site_year)
final_site <- split(final, final$site)

#### (2) Standardizing covariates and taking log anatoxin & cover ####

# look at distribution of all anatoxin data
hist(log(final$ATX_all_ug_afdm_g_average)) # roughly normal so log-normal!
# recognize this may vary per site
hist(log(final$micro_ana_cyl)) # close enough

# function to scale dataframes
scale_df <- function(df) {
  new_df <- df %>% 
    na.omit() %>% 
    mutate(ATX_corrected = ATX_all_ug_afdm_g_average + 0.001,
           cover_corrected = micro_ana_cyl + 0.001,
           log_cover = (micro_ana_cyl),
           log_ATX = log(ATX_corrected),
           temperature = scale(temp_C),
           conductivity = scale(cond_uS_cm),
           phosphate = scale(oPhos_ug_P_L),
           nitrate = scale(nitrate_mg_N_L),
           ammonium = scale(ammonium_mg_N_L),
           discharge = scale(discharge_cms))
  return(new_df)
}

# applying function
sfkeel_mir_scaled <- scale_df(final_site$`SFE-M`)
sfkeel_mir_2022_scaled <- scale_df(final_site_year$sfkeel_mir_2022)
sfkeel_mir_2023_scaled <- scale_df(final_site_year$sfkeel_mir_2023)
sfkeel_sth_2023_scaled <- scale_df(final_site$`SFE-SH`)
russian_scaled <- scale_df(final_site_year$russian_2022)
salmon_scaled <- scale_df(final_site$SAL)
salmon_2022_scaled <- scale_df(final_site_year$salmon_2022)
salmon_2023_scaled <- scale_df(final_site_year$salmon_2023)

#### (3) GLMs with ATX as reponse ####
# need to decide on colors! and range when all are finished

### South Fork Eel @ Miranda both years
model1_ln <- glm(log_ATX ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
              data = sfkeel_mir_scaled)

plot_model(model1_ln, dot.size = 5, line.size = 1.5, color = c("#8f8504", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 2, alpha = 0.3) +
  ylim(-5, 5)

### South Fork Eel @ Miranda 2022
model2_ln <- glm(log_ATX ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = sfkeel_mir_2022_scaled)

plot_model(model2_ln, dot.size = 5, line.size = 1.5, color = c("#8f8504", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL) +
  geom_vline(xintercept = 0, linetype = "dashed")

### South Fork Eel @ Miranda 2023
model3_ln <- glm(log_ATX ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = sfkeel_mir_2023_scaled)

plot_model(model3_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

### South Fork Eel @ Standish 2023
model4_ln <- glm(log_ATX ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = sfkeel_sth_2023_scaled)

plot_model(model4_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

### Russian 2022
model5_ln <- glm(log_ATX ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = russian_scaled)

plot_model(model5_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

##### GLMs with cover as response ####

### South Fork Eel @ Miranda both years
model6_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = sfkeel_mir_scaled)

plot_model(model6_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

### South Fork Eel @ Miranda 2022
model7_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = sfkeel_mir_2022_scaled)

plot_model(model6_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

### South Fork Eel @ Miranda 2023
model7_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = sfkeel_mir_2023_scaled)

plot_model(model7_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

#### South Fork Eel @ Standish 2023
model8_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = sfkeel_sth_2023_scaled)

plot_model(model8_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

#### Russian River 2022
model9_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = russian_scaled)

plot_model(model9_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

#### Salmon River both years
model10_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = salmon_scaled)

plot_model(model10_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)

#### Salmon River 2022
model11_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                 data = salmon_2022_scaled)

plot_model(model11_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)
# not enough data for confidence intervals

#### Salmon River 2023
model12_ln <- glm(log_cover ~ nitrate + ammonium + phosphate + temperature + conductivity + discharge,
                  data = salmon_2023_scaled)

plot_model(model12_ln, dot.size = 5, line.size = 1.5, color = c("#456C2B", "#37578c")) + theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
        text = element_text(size = 30), axis.ticks.length=unit(.25, "cm"), axis.title.x = element_blank()) + 
  labs(title = NULL, xlab = NULL)
