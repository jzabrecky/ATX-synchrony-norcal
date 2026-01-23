#### Supplemental figures to represent uncertainty in predictions
### Jordan Zabrecky
## last edited: 01.22.2026

# insert description here

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), 
       require, character.only = T)

## loading data

# predictive uncertainty
pred_unc <- read.csv("./data/predictive_models/predictive_uncertainty.csv") %>% 
  # factor predicting for desired order in facet_wrap
  mutate(predicting_f = factor(predicting, levels = c("M_cover", "AC_cover", 
                                                      "M_atx", "AC_atx"))) %>% 
  # factor models for desired order on x-axis
  mutate(model_f = factor(model, levels = rev(c("physical", "physical_w_cover",
                                            "chemical", "chemical_w_cover", 
                                            "biological", "biological_w_cover",
                                            "physicochemical", "physicochemical_w_cover",
                                            "ecohydrological", "ecohydrological_w_cover",
                                            "biochemical", "biochemical_w_cover", 
                                            "all", "all_w_cover")),
                          labels = rev(c("physical", "1-*(with cover)*",
                                         "chemical", "2-*(with cover)*", "biological",
                                         "3-*(with cover)*",
                                         "physicochemical", "4-*(with cover)*",
                                         "ecohydrological", "5-*(with cover)*",
                                         "biochemical", "6-*(with cover)*", 
                                         "all", "7-*(with cover)*")))) %>% 
  # factor uncertainty for desired order
  mutate(uncertainty_f = factor(uncertainty, levels = c("process_only", "param_and_process",
                                                        "param_process_and_initialcon")))

# summarize for line point
pred_unc_summary <- pred_unc %>% 
  dplyr::group_by(predicting_f, uncertainty_f, model_f) %>% 
  dplyr::summarize(mean_sd = mean(standard_dev),
                   sd_sd = sd(standard_dev),
                   mean_nrmse = mean(nrmse),
                   sd_nrmse = sd(nrmse)) %>% 
  ungroup()

#### (2) Making Figure ####

# set universal plot theme
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
palette <- c("#416f16", "#62a7f8", "#ebdf38")

# making figure
sd_plot <- ggplot(data = pred_unc_summary, aes(y = model_f)) +
  geom_point(aes(x = mean_sd, color = uncertainty_f),
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = mean_sd - sd_sd, xmax = mean_sd + sd_sd, color = uncertainty_f), 
                position = position_dodge(width = 0.9)) +
  scale_color_manual(labels = c("Process Uncertainty Only",
                                "Parameter and Process Uncertainty",
                                "Parameter, Process, and Initial Condition Uncertainty"),
                       values = palette) +
  labs(y = "Model", x = "Predictive Uncertainty") +
  facet_wrap(~predicting_f, 
             labeller = as_labeller(c(`M_cover` = "Models Predicting *Microcoleus* Cover ", 
                            `AC_cover`= "Models Predicting *Anabaena/Cylindrospermum* Cover",
                            `M_atx` = "Models Predicting *Microcoleus* Anatoxins",
                            `AC_atx` = "Models Predicting *Anabaena/Cylindrospermum* Anatoxins")),
             ncol = 1, scales = "free_y") +
  theme(strip.background = element_blank(), legend.position = "none",
        axis.text.y = element_markdown()) # will add in legend separately
sd_plot

# save figure
ggsave("./figures/fig_uncertainty)notfinal.tiff", dpi = 600, 
       width=8.5, height=22, unit="cm")
# will add in legend separately

# legend
sd_legend <- ggplot(data = pred_unc_summary, aes(y = model_f)) +
  geom_point(aes(x = mean_sd, color = uncertainty_f),
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = mean_sd - sd_sd, xmax = mean_sd + sd_sd, color = uncertainty_f), 
                position = position_dodge(width = 0.9)) +
  scale_color_manual(labels = c("Process Uncertainty Only",
                                "Parameter and Process Uncertainty",
                                "Parameter, Process, and Initial Condition Uncertainty"),
                     values = palette) +
  labs(y = "Model", x = "Predictive Uncertainty") +
  facet_wrap(~predicting_f, 
             labeller = as_labeller(c(`M_cover` = "Models Predicting *Microcoleus* Cover ", 
                                      `AC_cover`= "Models Predicting *Anabaena/Cylindrospermum* Cover",
                                      `M_atx` = "Models Predicting *Microcoleus* Anatoxins",
                                      `AC_atx` = "Models Predicting *Anabaena/Cylindrospermum* Anatoxins")),
             ncol = 1, scales = "free_y") +
  theme(strip.background = element_blank(), legend.position = "right",
        axis.text.y = element_markdown()) # will add in legend separately
sd_legend

ggsave("./figures/fig_uncertainty_legend.tiff", dpi = 600, 
       width=19, height=18, unit="cm")

# look at same figure but for NRMSE
nrmse_plot  <- ggplot(data = pred_unc_summary, aes(y = model_f)) +
  geom_point(aes(x = mean_nrmse, color = uncertainty_f),
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(xmin = mean_nrmse - sd_nrmse, xmax = mean_nrmse + sd_nrmse, color = uncertainty_f), 
                position = position_dodge(width = 0.9)) +
  scale_color_manual(labels = c("Process Uncertainty Only",
                                "Parameter and Process Uncertainty",
                                "Parameter, Process, and Initial Condition Uncertainty"),
                     values = palette) +
  labs(y = "Model", x = "Predictive Uncertainty") +
  facet_wrap(~predicting_f, 
             labeller = as_labeller(c(`M_cover` = "Models Predicting *Microcoleus* Cover ", 
                                      `AC_cover`= "Models Predicting *Anabaena/Cylindrospermum* Cover",
                                      `M_atx` = "Models Predicting *Microcoleus* Anatoxins",
                                      `AC_atx` = "Models Predicting *Anabaena/Cylindrospermum* Anatoxins")),
             ncol = 1, scales = "free_y") +
  theme(strip.background = element_blank(), legend.position = "none") # will add in legend separately
nrmse_plot

# just for reference, individual predictive uncertainty points
sd_plot_ind_sitereach <- ggplot(data = pred_unc, aes(y = model_f)) +
  geom_point(aes(x = standard_dev, color = uncertainty_f, shape = site_reach),
             position = position_dodge(width = 0.9)) +
  scale_color_manual(labels = c("Process Uncertainty Only",
                                "Parameter and Process Uncertainty",
                                "Parameter, Process, and Initial Condition Uncertainty"),
                     values = palette) +
  labs(y = "Model", x = "Predictive Uncertainty") +
  facet_wrap(~predicting_f, 
             labeller = as_labeller(c(`M_cover` = "Models Predicting *Microcoleus* Cover ", 
                                      `AC_cover`= "Models Predicting *Anabaena/Cylindrospermum* Cover",
                                      `M_atx` = "Models Predicting *Microcoleus* Anatoxins",
                                      `AC_atx` = "Models Predicting *Anabaena/Cylindrospermum* Anatoxins")),
             ncol = 1, scales = "free_y") +
  theme(strip.background = element_blank(), legend.position = "none") # will add in legend separately
sd_plot_ind_sitereach

#### (3) Questions ####

## How much more uncertainty (% of process only predictive uncertainty) does each
## additional source of uncertainty add?
added_uncertainty <- pred_unc_summary %>% 
  select(uncertainty_f, predicting_f, model_f, mean_sd) %>% 
  pivot_wider(values_from = mean_sd, names_from = uncertainty_f) %>% 
  mutate(parameter_added_uncertainty = param_and_process - process_only,
         initialcondition_added_uncertainty = param_process_and_initialcon - param_and_process) %>% 
  mutate(perct_inc_param = (parameter_added_uncertainty / process_only) * 100,
         perct_inc_ic = (initialcondition_added_uncertainty / param_and_process) * 100)

added_uncertainty_summary <- added_uncertainty %>% 
  group_by(predicting_f) %>% 
  dplyr::summarize(mean_perct_inc_param = mean(perct_inc_param),
                   mean_perct_inc_ic = mean(perct_inc_ic)) %>% 
  dplyr::ungroup()
mean(added_uncertainty_summary$mean_perct_inc_param) # 0.96%
mean(added_uncertainty_summary$mean_perct_inc_ic, na.rm = TRUE) # 0.12%

## What was the average predictive uncertainty for each predicted category?
taxa_pred_unc <- pred_unc_summary %>% 
  group_by(predicting_f, uncertainty_f)
