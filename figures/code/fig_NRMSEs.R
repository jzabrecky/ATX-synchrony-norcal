#### Figure to show NRMSEs for all predictive models
### Jordan Zabrecky
## last edited: 10.25.2025

## Main figure that shows the NRMSEs comparisons between M vs. AC cover and anatoxins
## models and M & AC anatoxin models not including cover as a covariate and including 
## cover as a covariate with each model in the background versus the null model
## At the end misc. questions are also answered 

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "cowplot", "plyr", "ggtext", "ggsignif"), 
       require, character.only = T)

NRMSEs <- ldply(list.files(path = "./data/predictive_models/", pattern = "NRMSE"), 
                function(filename) {
                  d <- read.csv(paste("./data/predictive_models/", filename, sep = "")) %>% 
                    # add in what we are predicting from file name
                    mutate(predicting = str_remove(filename, "NRMSE_" )) %>% 
                    mutate(predicting = str_remove(predicting, ".csv")) %>% 
                    # remove null models
                    filter(model != "null") %>% 
                    # remove NMRSEs where site_reach predicted are separated out
                    filter(predicting %in% c("AC_atx", "AC_cover", "M_atx", "M_cover")) %>% 
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
                    # factor for predicting order
                    mutate(predicting_f = factor(predicting, levels = c("M_cover", "AC_cover",
                                                                        "M_atx", "AC_atx")),
                           predicting_w_cover_f = factor(predicting_w_cover,
                                                         levels = c("M_cover", "AC_cover", 
                                                                    "M_atx", "M_atx_w_cover",
                                                                    "AC_atx", "AC_atx_w_cover"))) %>% 
                    # get base model (i.e., without cover)
                    # factor for model order
                    mutate(model_f = factor(model, levels = c("physical", "chemical", "biological",
                                                              "physicochemical", ""))) %>% 
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
                                                              "all", "all_w_cover")))
                  
                  # add in factor stuff later
})

# calculate mean of mean and ci's (without cover models separated out)
mean_NRMSEs <- NRMSEs %>% 
  dplyr::group_by(predicting_f) %>% 
  dplyr::summarize(mean_mean = mean(mean),
                   mean_lower_ci = mean(lower_ci),
                   mean_upper_ci = mean(upper_ci))

# calculate mean of mean and ci's (with cover models separated out)
mean_NRMSEs_w_cover <- NRMSEs %>% 
  dplyr::group_by(predicting_w_cover_f) %>% 
  dplyr::summarize(mean_mean = mean(mean),
                   mean_lower_ci = mean(lower_ci),
                   mean_upper_ci = mean(upper_ci))

# get null models only
null_models <- ldply(list.files(path = "./data/predictive_models/", pattern = "NRMSE"), 
                     function(filename) {
                       d <- read.csv(paste("./data/predictive_models/", filename, sep = "")) %>% 
                         # add in what we are predicting from file name
                         mutate(predicting = str_remove(filename, "NRMSE_" )) %>% 
                         mutate(predicting = str_remove(predicting, ".csv")) %>% 
                         # remove null models
                         filter(model == "null")
})

#### (2) Creating Figure ####

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
palette_w_cover <- c("#E8DE48", "#E8DE48", "#B4D65E", "#B4D65E", "#8BCF6F", "#8BCF6F", 
                     "#57C785",  "#57C785", "#47A27E", "#47A27E", "#387E77", "#387E77", 
                     "#1E426B", "#1E426B")

## (a) cover comparison figure
cover_comparison <- ggplot() +
  geom_errorbar(data = NRMSEs %>% filter(predicting_f == "M_cover" | predicting_f == "AC_cover"),
             aes(x = predicting_f, ymin = lower_ci, ymax = upper_ci, color = model_f),
             position = position_dodge(width=0.9), linewidth = 0.8) +
  geom_point(data = NRMSEs %>% filter(predicting_f == "M_cover" | predicting_f == "AC_cover"),
              aes(x = predicting_f, y = mean, 
                  color = model_f, fill = model_f),
             position = position_dodge(width=0.9), size = 1) +
  geom_errorbar(data = mean_NRMSEs %>% filter(predicting_f == "M_cover" |
                                                    predicting_f == "AC_cover"), 
                aes(ymin = mean_lower_ci, ymax = mean_upper_ci, 
                    x = predicting_f), color = "#2e2e2e",  linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSEs %>% filter(predicting_f == "M_cover" |
                                                 predicting_f == "AC_cover"),
             aes(x = predicting_f, y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) + 
  geom_segment(aes(x = c(0.5, 1.5), xend = c(1.5, 2.5),
                   y = c(null_models$mean[which(null_models$predicting == "M_cover")],
                         null_models$mean[which(null_models$predicting == "AC_cover")]),
                   yend = c(null_models$mean[which(null_models$predicting == "M_cover")],
                            null_models$mean[which(null_models$predicting == "AC_cover")])),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
cover_comparison

## (b) atx comparison figure
atx_comparison <- ggplot() +
  geom_errorbar(data = NRMSEs %>% filter(predicting_f == "M_atx" | predicting_f == "AC_atx"),
                aes(x = predicting_f, ymin = lower_ci, ymax = upper_ci, color = model_f),
                position = position_dodge(width=0.9), linewidth = 0.8) +
  geom_point(data = NRMSEs %>% filter(predicting_f == "M_atx" | predicting_f == "AC_atx"),
             aes(x = predicting_f, y = mean, 
                 color = model_f, fill = model_f),
             position = position_dodge(width=0.9), size = 1) +
  geom_errorbar(data = mean_NRMSEs %>% filter(predicting_f == "M_atx" |
                                                predicting_f == "AC_atx"), 
                aes(ymin = mean_lower_ci, ymax = mean_upper_ci, 
                    x = predicting_f), color = "#2e2e2e",  linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSEs %>% filter(predicting_f == "M_atx" |
                                             predicting_f == "AC_atx"),
             aes(x = predicting_f, y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette_w_cover) +
  scale_fill_manual(values = palette_w_cover) + 
  geom_segment(aes(x = c(0.5, 1.5), xend = c(1.5, 2.5),
                   y = c(null_models$mean[which(null_models$predicting == "M_cover")],
                         null_models$mean[which(null_models$predicting == "AC_cover")]),
                   yend = c(null_models$mean[which(null_models$predicting == "M_cover")],
                            null_models$mean[which(null_models$predicting == "AC_cover")])),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
atx_comparison

## (c) w/ and w/o cover comparison for Microcoleus
M_atx_comparison <- ggplot() +
  geom_errorbar(data = NRMSEs %>% filter(predicting_f == "M_atx"),
                aes(x = predicting_w_cover_f, ymin = lower_ci, ymax = upper_ci, color = model_base_f),
                position = position_dodge(width=0.9), linewidth = 0.8) +
  geom_point(data = NRMSEs %>% filter(predicting_f == "M_atx"),
             aes(x = predicting_w_cover_f, y = mean, 
                 color = model_base_f, fill = model_base_f),
             position = position_dodge(width=0.9), size = 1) +
  geom_errorbar(data = mean_NRMSEs_w_cover %>% filter(predicting_w_cover_f == "M_atx" | 
                                                        predicting_w_cover_f == "M_atx_w_cover"), 
                aes(ymin = mean_lower_ci, ymax = mean_upper_ci, 
                    x = predicting_w_cover_f), color = "#2e2e2e",  linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSEs_w_cover %>% filter(predicting_w_cover_f == "M_atx" | 
                                                     predicting_w_cover_f == "M_atx_w_cover"),
             aes(x = predicting_w_cover_f, y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) + 
  geom_segment(aes(x = c(0.5, 1.5), xend = c(1.5, 2.5),
                   y = c(null_models$mean[which(null_models$predicting == "M_atx")],
                         null_models$mean[which(null_models$predicting == "M_atx")]),
                   yend = c(null_models$mean[which(null_models$predicting == "M_atx")],
                            null_models$mean[which(null_models$predicting == "M_atx")])),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
M_atx_comparison

## (d) w/ and w/o cover comparison for Microcoleus
AC_atx_comparison <- ggplot() +
  geom_errorbar(data = NRMSEs %>% filter(predicting_f == "AC_atx"),
                aes(x = predicting_w_cover_f, ymin = lower_ci, ymax = upper_ci, color = model_base_f),
                position = position_dodge(width=0.9), linewidth = 0.8) +
  geom_point(data = NRMSEs %>% filter(predicting_f == "AC_atx"),
             aes(x = predicting_w_cover_f, y = mean, 
                 color = model_base_f, fill = model_base_f),
             position = position_dodge(width=0.9), size = 1) +
  geom_errorbar(data = mean_NRMSEs_w_cover %>% filter(predicting_w_cover_f == "AC_atx" | 
                                                        predicting_w_cover_f == "AC_atx_w_cover"), 
                aes(ymin = mean_lower_ci, ymax = mean_upper_ci, 
                    x = predicting_w_cover_f), color = "#2e2e2e",  linewidth = .8,
                width = 0.2, alpha = 0.7)  +
  geom_point(data = mean_NRMSEs_w_cover %>% filter(predicting_w_cover_f == "AC_atx" | 
                                                     predicting_w_cover_f == "AC_atx_w_cover"),
             aes(x = predicting_w_cover_f, y = mean_mean), color = "#2e2e2e", fill = "white",
             size = 1.5, stroke = 1.1, alpha = 0.7, shape = 16) +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) + 
  ylim(0.1, 0.8) +
  geom_segment(aes(x = c(0.5, 1.5), xend = c(1.5, 2.5),
                   y = c(null_models$mean[which(null_models$predicting == "AC_atx")],
                         null_models$mean[which(null_models$predicting == "AC_atx")]),
                   yend = c(null_models$mean[which(null_models$predicting == "AC_atx")],
                            null_models$mean[which(null_models$predicting == "AC_atx")])),
               linewidth = 0.4, linetype = "11", color = "#2e2e2e") +
  theme(legend.position = "none")
AC_atx_comparison

#### (3) Putting together figure & saving ####

main <- plot_grid(cover_comparison, NA, M_atx_comparison, AC_atx_comparison,
                  align = "hv", ncol = 2) +
  theme(plot.background = element_rect(fill = "white", color = "white")) # white background
main
# dropping atx as it's redundant

# save to fit dimensions of one column
ggsave("./figures/fig_NRMSEs_notfinal.tiff", 
       dpi = 600, width = 18, height = 12, unit = "cm")
# want to try to change linewidth!!

#### (4) Miscellaneous Questions ####

## What is the mean NRMSE for each thing we are predicting?
view(mean_NRMSEs)
view(mean_NRMSEs_w_cover)

## Which/how many models outperform the null?
NRMSEs_null <- left_join(NRMSEs, null_models, by = c("predicting")) %>% 
  mutate(outperform = case_when(mean.x < mean.y ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::group_by(predicting) %>% 
  dplyr::summarize(outperforming = sum(outperform),
                   total = length(outperform))
NRMSEs_null <- left_join(NRMSEs, null_models, by = c("predicting")) %>% 
  mutate(outperform = case_when(mean.x < mean.y ~ 1,
                                TRUE ~ 0)) %>% 
  dplyr::group_by(predicting_w_cover) %>% 
  dplyr::summarize(outperforming = sum(outperform),
                   total = length(outperform))

## What models performed best for each?
performance <- split(NRMSEs, NRMSEs$predicting)
view(performance$AC_cover)
view(performance$M_cover)
view(performance$M_atx)
view(performance$AC_atx)

## What are the range of NRMSEs
performance_w_c <- split(NRMSEs, NRMSEs$predicting_w_cover)
max(performance_w_c$M_cover$mean) - min(performance_w_c$M_cover$mean) # 0.13
max(performance_w_c$AC_cover$mean) - min(performance_w_c$AC_cover$mean) # 0.22

max(performance_w_c$M_atx$mean) - min(performance_w_c$M_atx$mean) # 0.08
max(performance_w_c$M_atx_w_cover$mean) - min(performance_w_c$M_atx_w_cover$mean) # 0.07
max(performance_w_c$AC_atx$mean) - min(performance_w_c$AC_atx$mean) # 0.05
max(performance_w_c$AC_atx_w_cover$mean) - min(performance_w_c$AC_atx_w_cover$mean) # 0.05
max(performance$M_atx$mean) - min(performance$M_atx$mean) # 0.08
