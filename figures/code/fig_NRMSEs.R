#### Figure to show NRMSEs for all predictive models
### Jordan Zabrecky
## last edited: 08.13.2025

## This figure shows the NRMSEs for all predicted models for all reaches
## and compares to the null model (as indicated by a line) for that reach

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "cowplot", "plyr", "ggtext"), 
       require, character.only = T)

# loading data
NRMSEs <- ldply(list.files(path = "./data/predictive_models/", pattern = "nrmse"), 
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
                    # site reach separation for models with cover and without
                    mutate(site_reach_cover = case_when(grepl("w_cover", model) ~ paste(site_reach, "_w_cover", sep = ""),
                                                        TRUE ~ site_reach)) %>% 
                    # model base (excluding with cover argument) 
                    mutate(model_base = case_when(grepl("_w_cover", model) ~ str_remove(model, "_w_cover"),
                                                  TRUE ~ model)) %>% 
                    # add column (T/F) if cover is included as covariate
                    mutate(cover_covariate = case_when(grepl("w_cover", model) ~ TRUE,
                                                       TRUE ~ FALSE),
                           # make a final column that tells both what we are predicting and if cover
                           # is a covariate
                           predicting_w_cover = case_when(cover_covariate == TRUE ~ paste(predicting, "_w_cover", sep = ""),
                                                          TRUE ~ predicting)) %>% 
                    # factor model to match what was done with predictions above
                    mutate(model_base_f = factor(model_base, levels = c("physical", "chemical", 
                                                                        "biological", "physicochemical", 
                                                                        "ecohydrological", "biochemical",  
                                                                        "all"))) %>% 
                    # factor predicting to have M before AC in facet wrap
                    mutate(predicting_f = factor(predicting, levels = c("M_cover", "AC_cover", "M_atx", "AC_atx")))
                  return(d)
                })


# separate out nulls
NRMSE_nonull <- NRMSEs %>% filter(model!= "null")
NRMSE_nulls <- NRMSEs %>% filter(model == "null")

#### (2) Making plots ####

# set universal theme
theme_set(theme_bw() + theme(legend.position = "top",
                             panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1.2),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.title.y = ggtext::element_markdown(size = 10), 
                             axis.text.x = element_text(size = 10),
                             axis.text.y = element_text(size = 10),
                             plot.title = ggtext::element_markdown(size = 10, hjust = 0.5),
                             strip.text = element_text(face="bold", size=10)))

# create dataframe of segments for null data ()
null_list <- split(NRMSE_nulls, NRMSE_nulls$predicting)
null_data <- list()
for(i in 1:length(null_list)) {
  # for cover models
  if(i == 2 | i == 4) {
  null_data[[i]] <- data.frame(site_reach = null_list[[i]]$site_reach,
                                x_start = c(0.7, 1.7, 2.7, 3.7, 4.7),
                               x_end = c(1.3, 2.3, 3.3, 4.3, 5.3),
                               y_start = null_list[[i]]$mean,
                               y_end = null_list[[i]]$mean,
                               predicting_f = null_list[[i]]$predicting_f)
  } else if(i == 1 | i ==3) { # longer x to include both w/ and w/o cover models
      null_data[[i]] <- data.frame(site_reach = null_list[[i]]$site_reach,
                                x_start = c(0.7, 2.7, 4.7, 6.7, 8.7),
                               x_end = c(2.3, 4.3, 6.3, 8.3, 10.3),
                               y_start = null_list[[i]]$mean,
                               y_end = null_list[[i]]$mean,
                               predicting_f = null_list[[i]]$predicting_f)
  }
  
}

# color palette
palette_outline <- c("#a8a231", "#839c46", "#608f4d", "#3f8f60", "#34755b", "#2a5e59", "#152f4d")
palette_fill <- c("#F0E985", "#CDE494", "#B2DF9F", "#8FDAAE", "#84C1A9", "#7AA9A4", "#69819C")

## cover plots

# separate out data for cover predictions
cover_data <- NRMSE_nonull %>% filter(predicting == "M_cover" | predicting == "AC_cover")
null_cover_data <- rbind(null_data[[2]], null_data[[4]])

# facet wrap plot for cover NRMSEs
cover_plot <- ggplot(data = cover_data, aes(x = site_reach)) +
  geom_vline(xintercept = c(1:5)+0.5, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model_base_f), 
                position = position_dodge(width=0.5)) +
  geom_point(aes(y = mean, color = model_base_f, fill = model_base_f, shape = model_base_f), 
             position = position_dodge(width=0.5), size = 3) +
  geom_segment(data = null_cover_data, aes(x = x_start, xend = x_end,
                                          y = y_start, yend = y_end),
               linewidth = 0.8, alpha = 0.75) +
  scale_color_manual(values = palette_outline) +
  scale_fill_manual(values = palette_fill) +
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23, 21)) +
  labs(x = NULL, y = "NRMSE") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap(~predicting_f, ncol = 1) +
  theme(strip.background = element_blank())
cover_plot

## anatoxin plots

# separate out data for anatoxin predictions
atx_data <- NRMSE_nonull %>% filter(predicting == "M_atx" | predicting == "AC_atx")
null_atx_data <- rbind(null_data[[1]], null_data[[3]])

# facet wrap plot for atx NRMSEs
atx_plot <- ggplot(data = atx_data, aes(x = site_reach_cover))  +
  geom_vline(xintercept = c(2,4,6,8)+0.5, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model_base_f), 
                position = position_dodge(width=0.5)) +
  geom_point(aes(y = mean, color = model_base_f, fill = model_base_f, shape = model_base_f), 
             position = position_dodge(width=0.5), size = 3) +
  geom_segment(data = null_atx_data, aes(x = x_start, xend = x_end,
                                          y = y_start, yend = y_end),
               linewidth = 0.8, alpha = 0.75) +
  scale_color_manual(values = palette_outline) +
  scale_fill_manual(values = palette_fill) +
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23, 21)) +
  scale_x_continuous(breaks = (1.5, 2.5, 3.5, 4.5, 5.5))
  labs(x = NULL, y = "NRMSE") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap(~predicting_f, ncol = 1) +
  theme(strip.background = element_blank())
atx_plot

#### (3) Putting together plots & saving ####

# putting all together
all <- plot_grid(cover_plot, atx_plot, nrow = 1, rel_widths = c(1.25,2), scale = 0.98) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
all

# save plot
ggsave(paste("./figures/fig_NRMSEs_notfinal.tiff", sep = ""), 
       dpi = 600, width = 18, height = 13, unit = "cm")

# deciding to do a longer version of cover
just_cover <- plot_grid(cover_plot, scale = 0.98) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
just_cover

# save plot
ggsave(paste("./figures/fig_NRMSEs_just_cover.tiff", sep = ""), 
       dpi = 600, width = 7, height = 14, unit = "cm")

# save legend
legend <- ggplot(data = cover_data, aes(x = site_reach)) +
  geom_vline(xintercept = c(1:5)+0.5, linetype = "dashed", color = "gray") +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model_base_f), 
                position = position_dodge(width=0.5)) +
  geom_point(aes(y = mean, color = model_base_f, fill = model_base_f, shape = model_base_f), 
             position = position_dodge(width=0.5), size = 3) +
  geom_segment(data = null_cover_data, aes(x = x_start, xend = x_end,
                                           y = y_start, yend = y_end),
               linewidth = 0.8, alpha = 0.75) +
  scale_color_manual(values = palette_outline) +
  scale_fill_manual(values = palette_fill) +
  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23, 21)) +
  labs(x = NULL, y = "NRMSE") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap(~predicting_f, ncol = 1) +
  theme(strip.background = element_blank())
legend

# save plot
ggsave(paste("./figures/fig_NRMSEs_legend.tiff", sep = ""), 
       dpi = 600, width = 18, height = 12, unit = "cm")

#### (4) Miscellaneous Questions ####

## how do mean NRMSE of taxa-specific cover predictions and anatoxin predictions compare?
taxa_specific_NRMSE <- NRMSE_nonull %>% 
  dplyr::group_by(predicting) %>% 
  dplyr::summarize(mean_NRMSE = mean(mean),
                   mean_lower = mean(ci_lower),
                   mean_upper = mean(ci_upper))

# add in reach grouping
taxa_specific_NRMSE <- NRMSE_nonull %>% 
  dplyr::group_by(predicting, site_reach) %>% 
  dplyr::summarize(mean_NRMSE = mean(mean),
                   mean_lower = mean(ci_lower),
                   mean_upper = mean(ci_upper))

## how many models outperformed the null for each group?

# join in null model means
NRMSEs_withnulls <- left_join(NRMSE_nonull, NRMSE_nulls %>% select(site_reach, predicting, mean), 
                              by = c("site_reach", "predicting"))

# calculate # of models outperforming null
outperforming <- NRMSEs_withnulls %>% 
  dplyr::group_by(predicting) %>% 
  dplyr::summarize(total = length(model),
                   outperforming_null = sum(mean.x < mean.y))

# group by reach as well
outperforming_by_reach <- NRMSEs_withnulls %>% 
  dplyr::group_by(predicting, site_reach) %>% 
  dplyr::summarize(total = length(model),
                   outperforming_null = sum(mean.x < mean.y))

## what is the difference in NRMSE for atx models with and without cover?
taxa_specific_NRMSE <- NRMSE_nonull %>% 
  dplyr::group_by(predicting, cover_covariate) %>% 
  dplyr::summarize(mean_NRMSE = mean(mean),
                   mean_lower = mean(ci_lower),
                   mean_upper = mean(ci_upper))

## what were the best models for anatoxins prediction
best_atx_models <- NRMSE_nonull %>% 
  filter(predicting == "M_atx" | predicting == "AC_atx") %>% 
  dplyr::group_by(model, predicting) %>% 
  dplyr::summarize(mean_NRMSE = mean(mean))
best_atx_models_split <- split(best_atx_models, best_atx_models$predicting)
view(best_atx_models_split$AC_atx)
view(best_atx_models_split$M_atx)
