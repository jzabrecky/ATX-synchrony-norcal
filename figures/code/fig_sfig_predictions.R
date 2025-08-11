#### Main figure and supplemental figures showing our predictions
### Jordan Zabrecky
## last edited: 8.8.25

## This code makes figures showing our predictions vs. observed values for
## (1) all models for a supplemental figure and (2) showing only the best models
## for predicting cover, predicting anatoxins, and then predicting anatoxins
## using cover as a coariate (for a total of 6)

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "cowplot", "plyr", "ggtext"), 
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

# pivot longer 
observed <- pivot_longer(observed, cols = c("resp_M_cover_norm":"resp_AC_atx_norm"), names_to = "predicting",
               values_to = "observed")

# split into what we are predicting
observed_list <- split(observed, observed$predicting)
# manually adding observed to follow alphabetical order when split by models w/ cover
observed_list_w_cover <- list()
observed_list_w_cover[[1]] <- observed_list$resp_AC_atx_norm
observed_list_w_cover[[2]] <- observed_list$resp_AC_atx_norm
observed_list_w_cover[[3]] <- observed_list$resp_AC_cover_norm
observed_list_w_cover[[4]] <- observed_list$resp_M_atx_norm
observed_list_w_cover[[5]] <- observed_list$resp_M_atx_norm
observed_list_w_cover[[6]] <- observed_list$resp_M_cover_norm
  
# predictions
predictions <- ldply(list.files(path = "./data/predictive_models/", pattern = "predictions"), 
      function(filename) {
        d <- read.csv(paste("./data/predictive_models/", filename, sep = ""))
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

# split into list by what we are predicting
predictions_list <- split(predictions, predictions$predicting)
predictions_list_coversplit <- split(predictions, predictions$predicting_w_cover) # separates atx w/ and w/o cover covariate

# for each list split into what model/covariates are predicting

# nRMSEs
nRMSEs <- ldply(list.files(path = "./data/predictive_models/", pattern = "nrmse"), 
                function(filename) {
                  d <- read.csv(paste("./data/predictive_models/", filename, sep = ""))
                  # add column for what we are predicting
                  d$predicting <- str_remove(filename, "nrmse_")
                  d$predicting <- str_remove(d$predicting, ".csv") # remove .csv
                  # some final manipulating
                  d <- d %>% 
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
                                                          TRUE ~ predicting))  %>% 
                    # create a label column using mean (but rounding to two decimals!) for plots
                    mutate(label = paste("nRMSE=", round(mean, 2))) %>% 
                    # factor model to match what was done with predictions above
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


# split into list by what we are predicting
nRMSE_list <- split(nRMSEs, nRMSEs$predicting)
nRMSE_list_splitcover <- split(nRMSEs, nRMSEs$predicting_w_cover) # separates atx w/ and w/o cover covariate

#### (2) Making supplemental figures ####

## (a) setting plot themes

# set universal theme
theme_set(theme_bw() + theme(legend.position = "top",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1.2),
                             text = element_text(size = 20), axis.ticks.length=unit(.25, "cm"),
                             axis.title.y = ggtext::element_markdown(size = 20), 
                             axis.text.x = element_text(size = 20),
                             axis.text.y = element_text(size = 20),
                             plot.title = ggtext::element_markdown(size = 20, hjust = 0.5),
                             strip.text = element_text(face="bold", size=20)))
# note: size text is large here but seems to be saving to a size way smaller than 10
# double-checked that it is about 10 when uploading the export on a Word doc

# color palette
palette <- c("#E8DE48", "#B4D65E", "#8BCF6F", "#57C785", "#47A27E", "#387E77", "#1E426B")
palette_w_cover <- c("#E8DE48", "#F0E985", "#B4D65E", "#CDE494", "#8BCF6F", "#B2DF9F", 
                     "#57C785", "#8FDAAE", "#47A27E", "#84C1A9", "#387E77", 
                     "#7AA9A4", "#1E426B", "#69819C")

# title labels
titles <- c("*Anabaena/Cylindrospermum* Anatoxin Concentration Predictions",
            "*Anabaena/Cylindrospermum* Cover Predictions",
            "*Microcoleus* Anatoxin Concentration Predictions",
            "*Microcoleus* Cover Predictions")
ylabels <- c("*Anabaena/Cylindrospermum* anatoxin concentration normalized to maximum of reach",
             "*Anabaena/Cylindrospermum* cover normalized to maximum of reach",
             "*Microcoleus* anatoxin concentration normalized to maximum of reach",
             "*Microcoleus* cover normalized to maximum of reach")
nRMSE_locations <- c(as.Date("2023-07-10"), as.Date("2023-09-01"), as.Date("2023-07-10"),
                     as.Date("2023-07-10"))

## (b) cover predictions

# cover predictions (indexes 2 and 4)
cover_indices <- c(2,4)

for(i in cover_indices) {
    ggplot(data = predictions_list[[i]], aes(x = field_date)) +
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = model_f), 
                    alpha = 0.5) +
        geom_point(aes(y = mean, color = model_f), size = 5) +
        geom_point(data = observed_list[[i]], aes(x = field_date, y = observed), 
                   color = "#262626", size = 4, shape = 18) +
        #geom_point(data = observed, aes(x = field_date, y = value)) deal with this later, will probably put in list
        facet_grid(model_f~site_reach) +
        labs(x = NULL, y = ylabels[i], title = titles[i]) +
        theme(legend.position = "none") +
        theme(strip.background = element_blank()) + # get rid of gray background for facet title
        geom_text(
          data = nRMSE_list[[i]],
          # note size seems to be different for below than above, hence the low size
          # will double-check in inkscape that it is correct
          mapping = aes(x = nRMSE_locations[i], y = 95, label = label), size= 5) +
        scale_color_manual(values = palette) +
        scale_fill_manual(values = palette) +
        # labels for reach & model labels
        scale_y_continuous(sec.axis = sec_axis(~ . , name = "Model", breaks = NULL, labels = NULL)) +
        scale_x_date(sec.axis = sec_axis(~ . , name = "Predicted Reach", breaks = NULL, labels = NULL))
  
  # save plot    
  ggsave(paste("./figures/sfig_predicting_", names(predictions_list)[i], "predictions_notfinal.tiff", sep = ""), 
                   dpi = 600, width = 18, height = 18)
} 

## (c) anatoxins

# anatoxin prediction indexes (1 and 3)
atx_indices <- c(1,3)

for(i in atx_indices) {
  ggplot(data = predictions_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = model_f), 
                alpha = 0.5) +
    geom_point(aes(y = mean, color = model_f), size = 5) +
    geom_point(data = observed_list[[i]], aes(x = field_date, y = observed), 
               color = "#262626", size = 4, shape = 18) +
    #geom_point(data = observed, aes(x = field_date, y = value)) deal with this later, will probably put in list
    facet_grid(model_f~site_reach) +
    labs(x = NULL, y = ylabels[i], title = titles[i]) +
    theme(legend.position = "none") +
    theme(strip.background = element_blank()) + # get rid of gray background for facet title
    geom_text(
      data = nRMSE_list[[i]],
      # note size seems to be different for below than above, hence the low size
      # will double-check in inkscape that it is correct
      mapping = aes(x = nRMSE_locations[i], y = 95, label = label), size= 5) +
    scale_color_manual(values = palette_w_cover) +
    scale_fill_manual(values = palette_w_cover) +
    # labels for reach & model labels
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Model", breaks = NULL, labels = NULL)) +
    scale_x_date(sec.axis = sec_axis(~ . , name = "Predicted Reach", breaks = NULL, labels = NULL))
  
  # save plot    
  ggsave(paste("./figures/sfig_predicting_", names(predictions_list)[i], "_notfinal.tiff", sep = ""), 
         dpi = 600, width = 17.5, height = 22.5)
} 

#### (3) Making main figure ####

# which reach performs best?
average_per_reach <- nRMSEs %>% 
  dplyr::group_by(site_reach) %>% 
  dplyr::summarize(mean_of_all = mean(mean))
best_reach <- average_per_reach$site_reach[which.min(average_per_reach$mean_of_all)]
print(best_reach) # SFE-Lower-1S !

# filter for nRMSEs only for that reach
nRMSE_best_reach_only <- lapply(nRMSE_list_splitcover, function(x) x <- x %>% 
                                    filter(site_reach == best_reach))

# empty list for model and nRMSE attributes
best_models <- data.frame(predicting = names(nRMSE_best_reach_only),
                          model = rep(NA, 6),
                          nRMSE = rep(NA, 6))

# get indices of best models for each 
for(i in 1:length(nRMSE_best_reach_only)) {
  index_of_best = which.min(nRMSE_best_reach_only[[i]]$mean)
  best_models$model[i] <- nRMSE_best_reach_only[[i]]$model[index_of_best]
  best_models$nRMSE[i] <- nRMSE_best_reach_only[[i]]$label[index_of_best]
}

# empty list for plots
best_plots <- list()

# labels/titles in order of plots here
titles_w_cover <- c("*Anabaena/Cylindrospermum* Anatoxin Concentration Predictions",
                    "*Anabaena/Cylindrospermum* Anatoxin Concentration Predictions",
            "*Anabaena/Cylindrospermum* Cover Predictions",
            "*Microcoleus* Anatoxin Concentration Predictions",
            "*Microcoleus* Anatoxin Concentration Predictions",
            "*Microcoleus* Cover Predictions")
ylabels_w_cover <- c("*Anabaena/Cylindrospermum* anatoxin concentration normalized to maximum of reach",
                     "*Anabaena/Cylindrospermum* anatoxin concentration normalized to maximum of reach",
                      "*Anabaena/Cylindrospermum* cover normalized to maximum of reach",
                      "*Microcoleus* anatoxin concentration normalized to maximum of reach",
                     "*Microcoleus* anatoxin concentration normalized to maximum of reach",
                      "*Microcoleus* cover normalized to maximum of reach")
nRMSE_locations_w_cover <- c(as.Date("2023-07-10"), as.Date("2023-07-10"), as.Date("2023-09-01"), as.Date("2023-07-10"),
                     as.Date("2023-07-10"), as.Date("2023-07-10"))

# make plots; may need to adjust indexes for 

for(i in 1:nrow(best_models)) {
  palette_index <- as.integer(nRMSE_list$AC_atx$model_f
    [which(best_models$model[i] == nRMSE_list$AC_atx$model_f)[1]])
  
  best_plots[[i]] <- ggplot(data = predictions_list_coversplit[[i]] %>% filter(site_reach == best_reach) %>% 
                         filter(model == best_models$model[i]), aes(x = field_date)) +
                       geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = model_f), 
                                   alpha = 0.5) +
                         geom_point(aes(y = mean, color = model_f), size = 5) +
                         geom_point(data = observed_list_w_cover[[i]] %>% filter(site_reach == best_reach),
                                    aes(x = field_date, y = observed), 
                                    color = "#262626", size = 4, shape = 18) +
                        labs(x = NULL, y = ylabels_w_cover[i], title = paste(titles_w_cover[i], "<br>", best_models$model[i], sep ="")) +
                        theme(legend.position = "none") +
                        theme(strip.background = element_blank()) + # get rid of gray background for facet title
                        geom_text(
                          data = best_models[i,],
                          # note size seems to be different for below than above, hence the low size
                          # will double-check in inkscape that it is correct
                          mapping = aes(x = nRMSE_locations_w_cover[i], y = 95, label = nRMSE), size= 5) +
                        scale_color_manual(values = palette_w_cover[palette_index]) +
                        scale_fill_manual(values = palette_w_cover[palette_index])
  
  # print out plots to see them!
  print(best_plots[[i]])
}

# adding names to list of plots to make putting together final plot easier
names(best_plots) <- names(predictions_list_coversplit)

# putting plots together in one
all <- plot_grid(best_plots$M_cover, best_plots$AC_cover,
                 best_plots$M_atx, best_plots$AC_atx,
                 best_plots$M_atx_w_cover, best_plots$AC_atx_w_cover,
                 ncol = 2)
all

# save plot    
ggsave(paste("./figures/fig_best_predictions_notfinal.tiff", sep = ""), 
       dpi = 600, width = 8.5, height = 10)
