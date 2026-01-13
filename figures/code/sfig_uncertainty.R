#### Supplemental figures to represent uncertainty in predictions
### Jordan Zabrecky
## last edited: 01.13.2026

# insert description here

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), 
       require, character.only = T)

## loading data

# observed/true values
observed <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  # change site_reach to new names for publication
  mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S")) %>% 
  select(field_date, site_reach, resp_M_cover_norm, resp_AC_cover_norm, resp_M_atx_norm,
         resp_AC_atx_norm)

# get last observed value as our predictive models omit that date
# and instead use the future column to designate the next sampling value
last_day_observed <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  # change site_reach to new names for publication
  mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S")) %>% 
  select(field_date, site_reach, future_M_cover_norm, future_AC_cover_norm, future_M_atx_norm,
         future_AC_atx_norm) %>% 
  filter(field_date == ymd("2023-09-18")) %>% 
  mutate(field_date = ymd("2023-09-24")) %>% 
  dplyr::rename(resp_M_cover_norm = future_M_cover_norm,
                resp_AC_cover_norm = future_AC_cover_norm,
                resp_M_atx_norm = future_M_atx_norm,
                resp_AC_atx_norm = future_AC_atx_norm)

# join in final observations
observed <- rbind(observed, last_day_observed)

# function to load files with argument for folder as a string and / at the end
loadpredfiles <- function(folder) {
  filepath = paste("./data/predictive_models/", folder, sep = "")
  predictions = ldply(list.files(path = filepath, pattern = "predictions"), 
                      function(filename) {
                        d <- read.csv(paste(filepath, filename, sep = ""))
                        # add column for what we are predicting
                        d$predicting <- str_remove(filename, "predictions_")
                        d$predicting <- str_remove(d$predicting, ".csv") # remove .csv
                        # some final manipulating
                        d <- d %>% 
                          # convert date from string to date object
                          mutate(field_date = ymd(field_date)) %>% 
                          # change site_reach to new names for publication
                          mutate(site_reach = case_when(site_reach == "SFE-M-1S" ~ "SFE-Lower-1S",
                                                        site_reach == "SFE-M-2" ~ "SFE-Lower-2",
                                                        site_reach == "SFE-M-3" ~ "SFE-Lower-3",
                                                        site_reach == "SFE-M-4" ~ "SFE-Lower-4",
                                                        site_reach == "SFE-SH-1S" ~ "SFE-Upper-1S")) %>% 
                          # add column (T/F) if cover is included as covariate
                          mutate(cover_covariate = case_when(grepl("w_cover", model) ~ TRUE,
                                                             TRUE ~ FALSE),
                                 # make a final column that tells both what we are predicting and if cover
                                 # is a covariate
                                 predicting_w_cover = case_when(cover_covariate == TRUE ~ paste(predicting, "_w_cover", sep = ""),
                                                                TRUE ~ predicting)) %>% 
                          # factor model to get order we want when using facet grid
                          mutate(model_f = factor(model, levels = c("physical", "physical_w_cover",
                                                                    "chemical", "chemical_w_cover", 
                                                                    "biological", "biological_w_cover",
                                                                    "physicochemical", "physicochemical_w_cover",
                                                                    "ecohydrological", "ecohydrological_w_cover",
                                                                    "biochemical", "biochemical_w_cover", 
                                                                    "all", "all_w_cover"))) %>% 
                          # lastly, remove null models because we don't care about that here
                          filter(model != "null")
                        
                        return(d)
                      })
  
  # split into list based on what we are predicting
  return(split(predictions, predictions$predicting))
}

# predictions (original- w/ param & process uncertainty but no initial condition)
predictions <- loadpredfiles("")
predictions_proc_un <- loadpredfiles("process_uncertainty/")
predictions_IC_un <- loadpredfiles("initialcondition_uncertainty/")


# maybe overlay standard deviation of predictions rather than NRMSE

# thoughts: same prediction figures but with different colors for different uncertainties???

# will require loading predictions for each

#### (2) Making supplemental figures ####

## (a) setting plot themes

# set universal theme
theme_set(theme_bw() + theme(legend.position = "top",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.title.y = ggtext::element_markdown(size = 10), 
                             axis.text.x = element_text(size = 10),
                             axis.text.y = element_text(size = 10),
                             plot.title = ggtext::element_markdown(size = 10, hjust = 0.5),
                             strip.text = element_text(face="bold", size=10)))

# color palette
palette <- c("#E8DE48", "#57C785", "#1E426B")

# title labels
titles <- c("*Anabaena/Cylindrospermum* Anatoxin Concentration Predictions",
            "*Anabaena/Cylindrospermum* Cover Predictions",
            "*Microcoleus* Anatoxin Concentration Predictions",
            "*Microcoleus* Cover Predictions")
ylabels <- c("*Anabaena/Cylindrospermum* anatoxin concentrations (normalized to maximum of reach)",
             "*Anabaena/Cylindrospermum* cover (normalized to maximum of reach)",
             "*Microcoleus* anatoxin concentrations (normalized to maximum of reach)",
             "*Microcoleus* cover (normalized to maximum of reach)")

## (b) cover predictions

# cover predictions (indexes 2 and 4)
cover_indices <- c(2,4)
  

# need to incorporate process, parameter, then IC
ggplot(data = predictions[[4]], aes(x = field_date)) +
  geom_ribbon(data = predictions_IC_un[[4/2]], aes(ymin = ci_lower, ymax = ci_upper), 
              fill = palette[3], alpha = 0.2) +
  geom_point(data = predictions_IC_un[[4/2]], aes(y = mean), color = palette[1], size = 1,
             alpha = 0.2) +
  geom_ribbon(data = predictions[[4]], aes(ymin = ci_lower, ymax = ci_upper), 
              fill = palette[1], alpha = 0.2) +
  geom_point(data = predictions[[4]], aes(y = mean), color = palette[2], size = 1,
             alpha = 0.2) +
  #geom_ribbon(data = predictions_proc_un[[4]], aes(ymin = ci_lower, ymax = ci_upper), 
  #            fill = palette[3], alpha = 0.2) +
  #geom_point(data = predictions_proc_un[[4]], aes(y = mean), color = palette[3], size = 1,
  #           alpha = 0.2) +
  #geom_point(data = observed, aes(x = field_date, y = value)) deal with this later, will probably put in list
  facet_grid(model_f~site_reach)
