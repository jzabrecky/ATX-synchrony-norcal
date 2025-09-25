#### Figure to show NRMSEs for all predictive models
### Jordan Zabrecky
## last edited: 09.22.2025

## Main figure that shows the NRMSEs comparisons between M vs. AC cover and anatoxins
## models and M & AC anatoxin models not including cover as a covariate and including 
## cover as a covariate with each model in the background versus the null model for that reach
## plus the old NRMSE figure (which will not be used?)

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
                    # make a column to indicate if model was omitted or not
                    mutate(omitted = case_when(is.na(mean) ~ TRUE,
                                               TRUE ~ FALSE)) %>% 
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
                                                                        "all")),
                           model_f = factor(model, levels = c("physical", "physical_w_cover",
                                                              "chemical", "chemical_w_cover", 
                                                              "biological", "biological_w_cover",
                                                              "physicochemical", "physicochemical_w_cover",
                                                              "ecohydrological", "ecohydrological_w_cover",
                                                              "biochemical", "biochemical_w_cover", 
                                                              "all", "all_w_cover"))) %>% 
                    # factor predicting to have M before AC in facet wrap
                    mutate(predicting_f = factor(predicting, levels = c("M_cover", "AC_cover", "M_atx", "AC_atx")))
                  return(d)
                })

# fill in dummy variables for missing data from omitted models
omitted_models <- data.frame(site_reach = rep("SFE-Lower-1S", 4),
                             model = c("all", "ecohydrological", "biological", "biochemical"),
                             mean = rep(0.5, 4),
                             ci_lower = rep(0.5, 4),
                             ci_upper = rep(0.5, 4),
                             predicting = rep("AC_cover", 4),
                             omitted = rep(TRUE, 4),
                             site_reach_cover = rep("SFE-Lower-1S_w_cover"),
                             model_base = c("all", "ecohydrological", "biological", "biochemical"),
                             cover_covariate = rep(FALSE, 4),
                             predicting_w_cover = rep("AC_cover", 4),
                             model_base_f = c("all", "ecohydrological", "biological", "biochemical"),
                             model_f = c("all", "ecohydrological", "biological", "biochemical"),
                             predicting_f = rep("AC_cover", 4))

# bind together with NRMSEs
NRMSEs <- rbind(NRMSEs, omitted_models)

# separate out nulls
NRMSE_nonull <- NRMSEs %>% filter(model!= "null")
NRMSE_nulls <- NRMSEs %>% filter(model == "null")

#### (2) Creating main figure ####

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

# palettes
palette <- c("#E8DE48", "#B4D65E", "#8BCF6F", "#57C785", "#47A27E", "#387E77", "#1E426B")
#palette_w_cover <- c("#E8DE48", "#F0E985", "#B4D65E", "#CDE494", "#8BCF6F", "#B2DF9F", 
#                     "#57C785", "#8FDAAE", "#47A27E", "#84C1A9", "#387E77", 
#                     "#7AA9A4", "#1E426B", "#69819C") where cover is slightly lighter in color
palette_w_cover <- c("#E8DE48", "#E8DE48", "#B4D65E", "#B4D65E", "#8BCF6F", "#8BCF6F", 
                     "#57C785",  "#57C785", "#47A27E", "#47A27E", "#387E77", "#387E77", 
                     "#1E426B", "#1E426B")
# for AC cover which has omitted models

# create dataframe of segments for null data ()
null_list <- split(NRMSE_nulls, NRMSE_nulls$predicting)
names(null_list) <- unique(NRMSE_nulls$predicting)
null_data <- list()
# for cover comparison
null_data$'cover' <- data.frame(site_reach = c(null_list$M_cover$site_reach, 
                                            null_list$AC_cover$site_reach),
                             x_start = c(0.55, 0.73, 0.91, 1.09, 1.27,
                                         1.55, 1.73, 1.91, 2.09, 2.27),
                             x_end = c(0.73, 0.91, 1.09, 1.27, 1.45,
                                       1.73, 1.91, 2.09, 2.27, 2.45),
                             y_start = c(null_list$M_cover$mean, 
                                         null_list$AC_cover$mean),
                             y_end = c(null_list$M_cover$mean, 
                                       null_list$AC_cover$mean),
                             predicting_f = c(null_list$M_cover$predicting_f,
                                              null_list$AC_cover$predicting_f))
# for atx comparison
null_data$'atx' <- data.frame(site_reach = c(null_list$M_atx$site_reach, 
                                               null_list$AC_atx$site_reach),
                                x_start = c(0.525, 0.715, 0.905, 1.095, 1.285,
                                            1.525, 1.715, 1.905, 2.095, 2.285),
                                x_end = c(0.715, 0.905, 1.095, 1.285, 1.475,
                                          1.715, 1.905, 2.095, 2.285, 2.475),
                                y_start = c(null_list$M_atx$mean, 
                                            null_list$AC_atx$mean),
                                y_end = c(null_list$M_atx$mean, 
                                          null_list$AC_atx$mean),
                                predicting_f = c(null_list$M_atx$predicting_f,
                                                 null_list$AC_atx$predicting_f))
# for M atx comparison
null_data$'M_atx' <- data.frame(site_reach = c(null_list$M_atx$site_reach, 
                                             null_list$M_atx$site_reach),
                                x_start = c(0.55, 0.73, 0.91, 1.09, 1.27,
                                            1.55, 1.73, 1.91, 2.09, 2.27),
                                x_end = c(0.73, 0.91, 1.09, 1.27, 1.45,
                                          1.73, 1.91, 2.09, 2.27, 2.45),
                              y_start = c(null_list$M_atx$mean, 
                                          null_list$M_atx$mean),
                              y_end = c(null_list$M_atx$mean, 
                                        null_list$M_atx$mean),
                              predicting_f = c(null_list$M_atx$predicting_f,
                                               null_list$M_atx$predicting_f))
# for AC atx comparison
null_data$'AC_atx' <- data.frame(site_reach = c(null_list$AC_atx$site_reach, 
                                               null_list$AC_atx$site_reach),
                                 x_start = c(0.55, 0.73, 0.91, 1.09, 1.27,
                                             1.55, 1.73, 1.91, 2.09, 2.27),
                                 x_end = c(0.73, 0.91, 1.09, 1.27, 1.45,
                                           1.73, 1.91, 2.09, 2.27, 2.45),
                                y_start = c(null_list$AC_atx$mean, 
                                            null_list$AC_atx$mean),
                                y_end = c(null_list$AC_atx$mean, 
                                          null_list$AC_atx$mean),
                                predicting_f = c(null_list$AC_atx$predicting_f,
                                                 null_list$AC_atx$predicting_f))

# summarizing based on taxa and then based on if cover is included
mean_NRMSE_taxa <- NRMSE_nonull %>%
  dplyr::group_by(predicting_f) %>% 
  dplyr::summarize(mean_mean = mean(mean),
                   mean_ci_lower = mean(ci_lower),
                   mean_ci_upper = mean(ci_upper))
mean_NRMSE_cover <- NRMSE_nonull %>% 
  dplyr::group_by(predicting_f, cover_covariate) %>% 
  dplyr::summarize(mean_mean = mean(mean),
                   mean_ci_lower = mean(ci_lower),
                   mean_ci_upper = mean(ci_upper))
mean_nulls_taxa <- NRMSE_nulls %>% 
  group_by(predicting_f) %>% 
  dplyr::summarize(mean_mean = mean(mean))
mean_nulls_cover <- NRMSE_nulls %>% 
  group_by(predicting_f, cover_covariate) %>% 
  dplyr::summarize(mean_mean = mean(mean))

##  making figure

# cover comparison AC vs M. (note special alpha for omitted)
cover_comparison <- ggplot() +
  geom_errorbar(data = NRMSE_nonull %>% filter(predicting_f == "M_cover" |
                                                 predicting_f == "AC_cover"), 
                aes(x = predicting_f, ymin = ci_lower, ymax = ci_upper, 
                    y = mean, shape = site_reach, color = model_f, alpha = omitted), 
                position = position_dodge(width=0.9), linewidth = 0.2, width = 0.5) +
  geom_point(data = NRMSE_nonull %>% filter(predicting_f == "M_cover" |
                                              predicting_f == "AC_cover"), 
             aes(x = predicting_f, y = mean,
                 shape = site_reach, color = model_f, fill = model_f, alpha = omitted), 
             position = position_dodge(width=0.9), size = 0.8) +
  geom_errorbar(data = mean_NRMSE_taxa %>% filter(predicting_f == "M_cover" |
                                                 predicting_f == "AC_cover"), 
                aes(ymin = mean_ci_lower, ymax = mean_ci_upper, 
                    x = predicting_f), color = "#2e2e2e",  linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSE_taxa %>% filter(predicting_f == "M_cover" |
                                                 predicting_f == "AC_cover"),
             aes(x = predicting_f, y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette) + 
  scale_alpha_manual(values = c(1, 0)) +
  geom_segment(data = null_data$cover, aes(x = x_start, xend = x_end,
                                         y = y_start, yend = y_end),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
cover_comparison

# atx comparison AC vs. M
atx_comparison <- ggplot() +
  geom_errorbar(data = NRMSE_nonull %>% filter(predicting_f == "M_atx" |
                                                 predicting_f == "AC_atx"), 
                aes(x = predicting_f, ymin = ci_lower, ymax = ci_upper, 
                    y = mean, shape = site_reach, color = model_f), 
                position = position_dodge(width=0.95), linewidth = 0.2, width = 0.5) +
  geom_point(data = NRMSE_nonull %>% filter(predicting_f == "M_atx" |
                                              predicting_f == "AC_atx"), 
             aes(x = predicting_f, y = mean, shape = site_reach,
                 color = model_f, fill = model_f), 
             position = position_dodge(width=0.95), size = 0.8) +
  geom_errorbar(data = mean_NRMSE_taxa %>% filter(predicting_f == "M_atx" |
                                                    predicting_f == "AC_atx"), 
                aes(ymin = mean_ci_lower, ymax = mean_ci_upper, 
                    x = predicting_f), color = "#2e2e2e", linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSE_taxa %>% filter(predicting_f == "M_atx" |
                                                 predicting_f == "AC_atx"),
             aes(x = predicting_f, y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette_w_cover) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette_w_cover) + 
  geom_segment(data = null_data$atx, aes(x = x_start, xend = x_end,
                                           y = y_start, yend = y_end),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
atx_comparison
  
# M atx comparison w & w/o cover
M_atx_comparison <- ggplot() +
  geom_errorbar(data = NRMSE_nonull %>% filter(predicting_f == "M_atx"), 
                aes(x = interaction(predicting_f, cover_covariate), ymin = ci_lower, ymax = ci_upper, 
                    y = mean, shape = site_reach, color = model_f), 
                position = position_dodge(width=0.9), linewidth = 0.2, width = 0.5) +
  geom_point(data = NRMSE_nonull %>% filter(predicting_f == "M_atx"), 
             aes(interaction(predicting_f, cover_covariate), y = mean,
                 shape = site_reach, color = model_f, fill = model_f), 
             position = position_dodge(width=0.9), size = 0.8) +
  geom_errorbar(data = mean_NRMSE_cover %>% filter(predicting_f == "M_atx"), 
                aes(ymin = mean_ci_lower, ymax = mean_ci_upper, 
                    interaction(predicting_f, cover_covariate)), color = "#2e2e2e", linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSE_cover %>% filter(predicting_f == "M_atx"),
             aes(x = interaction(predicting_f, cover_covariate), y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette_w_cover) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette_w_cover) + 
  geom_segment(data = null_data$M_atx, aes(x = x_start, xend = x_end,
                                            y = y_start, yend = y_end),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
M_atx_comparison

# AC atx comparison w & w/o cover
AC_atx_comparison <- ggplot() +
  geom_errorbar(data = NRMSE_nonull %>% filter(predicting_f == "AC_atx"), 
                aes(x = interaction(predicting_f, cover_covariate), ymin = ci_lower, ymax = ci_upper, 
                    y = mean, shape = site_reach, color = model_f), 
                position = position_dodge(width=0.9), linewidth = 0.2, width = 0.5) +
  geom_point(data = NRMSE_nonull %>% filter(predicting_f == "AC_atx"), 
             aes(interaction(predicting_f, cover_covariate), y = mean, shape = site_reach,
                 color = model_f, fill = model_f), 
             position = position_dodge(width=0.9), size = 0.8) +
  geom_errorbar(data = mean_NRMSE_cover %>% filter(predicting_f == "AC_atx"), 
                aes(ymin = mean_ci_lower, ymax = mean_ci_upper, 
                    interaction(predicting_f, cover_covariate)), color = "#2e2e2e", linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSE_cover %>% filter(predicting_f == "AC_atx"),
             aes(x = interaction(predicting_f, cover_covariate), y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette_w_cover) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette_w_cover) + 
  geom_segment(data = null_data$AC_atx, aes(x = x_start, xend = x_end,
                                           y = y_start, yend = y_end),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
AC_atx_comparison

# version for legend
legend <- ggplot() +
  geom_errorbar(data = NRMSE_nonull %>% filter(predicting_f == "AC_atx"), 
                aes(x = interaction(predicting_f, cover_covariate), ymin = ci_lower, ymax = ci_upper, 
                    y = mean, shape = site_reach, color = model_f), 
                position = position_dodge(width=0.7), linewidth = 0.2, width = 0.5) +
  geom_point(data = NRMSE_nonull %>% filter(predicting_f == "AC_atx"), 
             aes(interaction(predicting_f, cover_covariate), y = mean, shape = site_reach,
                 color = model_f, fill = model_f), 
             position = position_dodge(width=0.7), size = 0.8) +
  geom_errorbar(data = mean_NRMSE_cover %>% filter(predicting_f == "AC_atx"), 
                aes(ymin = mean_ci_lower, ymax = mean_ci_upper, 
                    interaction(predicting_f, cover_covariate)), color = "#2e2e2e", linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSE_cover %>% filter(predicting_f == "AC_atx"),
             aes(x = interaction(predicting_f, cover_covariate), y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette_w_cover) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  scale_fill_manual(values = palette_w_cover) + 
  geom_segment(data = null_data$AC_atx, aes(x = x_start, xend = x_end,
                                            y = y_start, yend = y_end),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "right")
legend

ggsave(paste("./figures/fig_NRMSEs_legend.tiff"), dpi = 600,
       width = 9, height = 13, unit = "cm")

#### (3) Putting together plots & saving ####

main <- plot_grid(cover_comparison, NA, M_atx_comparison, AC_atx_comparison,
                  align = "hv", ncol = 2) +
  theme(plot.background = element_rect(fill = "white", color = "white")) # white background
main
# dropping atx as it's redundant

# save to fit dimensions of one column
ggsave("./figures/fig_NRMSEs_notfinal.tiff", 
       dpi = 600, width = 18, height = 11, unit = "cm")
# want to try to change linewidth!!

#### (4) Old Plots ####

# color palette
palette_outline <- c("#a8a231", "#839c46", "#608f4d", "#3f8f60", "#34755b", "#2a5e59", "#152f4d")
palette_fill <- c("#F0E985", "#CDE494", "#B2DF9F", "#8FDAAE", "#84C1A9", "#7AA9A4", "#69819C")


# create dataframe of segments for null data () with new spacing intervals
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
  labs(x = NULL, y = "NRMSE") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap(~predicting_f, ncol = 1) +
  theme(strip.background = element_blank())
atx_plot

## putting together old plots & saving

# putting all together
all <- plot_grid(cover_plot, atx_plot, nrow = 1, rel_widths = c(1.25,2), scale = 0.98) +
  theme(plot.background = element_rect(fill = "white", color = "white"))
all

# save plot
#ggsave(paste("./figures/sfig_NRMSEs_notfinal.tiff", sep = ""), 
#       dpi = 600, width = 18, height = 13, unit = "cm")

# save legend
#legend <- ggplot(data = cover_data, aes(x = site_reach)) +
#  geom_vline(xintercept = c(1:5)+0.5, linetype = "dashed", color = "gray") +
#  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model_base_f), 
#                position = position_dodge(width=0.5)) +
#  geom_point(aes(y = mean, color = model_base_f, fill = model_base_f, shape = model_base_f), 
#             position = position_dodge(width=0.5), size = 3) +
#  geom_segment(data = null_cover_data, aes(x = x_start, xend = x_end,
#                                           y = y_start, yend = y_end),
#               linewidth = 0.8, alpha = 0.75) +
#  scale_color_manual(values = palette_outline) +
#  scale_fill_manual(values = palette_fill) +
#  scale_shape_manual(values = c(21, 22, 23, 21, 22, 23, 21)) +
#  labs(x = NULL, y = "NRMSE") +
#  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
#  facet_wrap(~predicting_f, ncol = 1) +
#  theme(strip.background = element_blank())
#legend

# save plot
#ggsave(paste("./figures/sfig_NRMSEs_legend.tiff", sep = ""), 
#       dpi = 600, width = 18, height = 12, unit = "cm")

#### (5) Miscellaneous Questions ####

## how do mean NRMSE of taxa-specific cover predictions and anatoxin predictions compare?
taxa_specific_NRMSE <- NRMSE_nonull %>% 
  dplyr::group_by(predicting) %>% 
  dplyr::summarize(mean_NRMSE = mean(mean),
                   mean_lower = mean(ci_lower),
                   mean_upper = mean(ci_upper))

# add in reach grouping
taxa_specific_NRMSE_reach <- NRMSE_nonull %>% 
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
taxa_specific_NRMSE_w_cover <- NRMSE_nonull %>% 
  dplyr::group_by(predicting, cover_covariate) %>% 
  dplyr::summarize(mean_NRMSE = mean(mean),
                   mean_lower = mean(ci_lower),
                   mean_upper = mean(ci_upper))

## what were the best models for prediction of cover and atx
best_models <- NRMSE_nonull %>% 
  dplyr::group_by(model, predicting) %>% 
  dplyr::summarize(mean_NRMSE = mean(mean))
best_models_split <- split(best_models, best_models$predicting)
view(best_models_split$AC_atx)
view(best_models_split$AC_cover)
view(best_models_split$M_atx)
