## predictions figure

library("tidyverse")

preds_M <- read.csv("./data/predictive_models/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
preds_AC <- read.csv("./data/predictive_models/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
atx_M <- read.csv("./data/predictive_models/predictions_M_atx.csv") %>% 
  mutate(field_date = ymd(field_date))
atx_AC <- read.csv("./data/predictive_models/predictions_AC_cover.csv")
observed <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date))
preds_M_normal_dist <- read.csv("./data/predictive_models/normal_test/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
preds_M_no_autoreg <- read.csv("./data/predictive_models/no_autoregressive/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
atx_M_old <- read.csv("./data/predictive_models/old_predictions_nrmse_for_EFI_conf/predictions_M_atx.csv") %>% 
  mutate(field_date = ymd(field_date))
atx_M_nomatrix_fixed_reg <- read.csv("./data/predictive_models/no_matrix_test/predictions_M_atx.csv") %>% 
  mutate(field_date = ymd(field_date))
atx_norm_autoreg <- read.csv("./data/predictive_models/M_atx_normaltest_w_autoregressive/predictions_M_atx.csv") %>% 
  mutate(field_date = ymd(field_date))

# plot colors
palette <- c("#523939", "#528b87", "#416f16", "#62a7f8", "#8b9609", "gray", "#bdb000", "#90ac7c")
ribbon_palette <- c("#c9a3a3", "#aed4d1", "#c7e3ac", "#c7e1ff", "#dee3a1", "gray", "#ebe7b0", "#d6edc5")

# microcoleus cover predictions
preds_M_list <- split(preds_M, preds_M$model) # split into list

for(i in 1:length(preds_M_list)) {
  plot <- ggplot(data = preds_M_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# normal attempt
preds_M_normal_dist_list <- split(preds_M_normal_dist, 
                                  preds_M_normal_dist$model) # split into list

for(i in 1:length(preds_M_normal_dist_list)) {
  plot <- ggplot(data = preds_M_normal_dist_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_normal_dist_list[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    coord_cartesian(ylim = c(0,100)) +
    theme_bw()
  print(plot)
}

# no autoregressive term attempt
preds_M_no_autoreg_list <- split(preds_M_no_autoreg, 
                                  preds_M_no_autoreg$model) # split into list

for(i in 1:length(preds_M_no_autoreg_list)) {
  plot <- ggplot(data = preds_M_no_autoreg_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_no_autoreg_list[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    coord_cartesian(ylim = c(0,100)) +
    theme_bw()
  print(plot)
}

# anabaena/cylindrospermum cover predictions

preds_AC_list <- split(preds_AC, preds_AC$model) # split into list

for(i in 1:length(preds_AC_list)) {
  plot <- ggplot(data = preds_AC_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black") +
    labs(title = paste(preds_AC_list[[i]]$model[1], "-- Anabaena Cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# microcoleus anatoxins predictions

preds_M_atx_list <- split(atx_M, atx_M$model) # split into list

for(i in 1:length(preds_M_atx_list)) {
  plot <- ggplot(data = preds_M_atx_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_atx_norm), color = "black") +
    labs(title = paste(preds_M_atx_list[[i]]$model[1], "-- Microcoleus ATX"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# how about old atx??? note this does not have matrix change, autoreg change, and normal change
preds_M_atx_old_list <- split(atx_M_old, atx_M_old$model) # split into list

for(i in 1:length(preds_M_atx_old_list)) {
  plot <- ggplot(data = preds_M_atx_old_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_atx_norm), color = "black") +
    labs(title = paste(preds_M_atx_old_list[[i]]$model[1], "-- Microcoleus ATX"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# try with just the normal
pred_M_atx_norm_autoreg_list <- split(atx_norm_autoreg, atx_norm_autoreg$model)

for(i in 1:length(pred_M_atx_norm_autoreg_list)) {
  plot <- ggplot(data = pred_M_atx_norm_autoreg_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_atx_norm), color = "black") +
    labs(title = paste(pred_M_atx_norm_autoreg_list[[i]]$model[1], "-- Anabaena ATX"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# single test w/o matrix change
pred_M_atx_no_matrix <- split(atx_M_nomatrix_fixed_reg, atx_M_nomatrix_fixed_reg$model)

for(i in 1:length(pred_M_atx_no_matrix)) {
  plot <- ggplot(data = pred_M_atx_no_matrix[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_atx_norm), color = "black") +
    labs(title = paste(pred_M_atx_no_matrix[[i]]$model[1], "-- Anabaena ATX"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# anabaena anatoxins predictions
preds_AC_atx_list <- split(atx_AC, atx_AC$model) # split into list

for(i in 1:length(preds_AC_atx_list)) {
  plot <- ggplot(data = preds_AC_atx_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_atx_norm), color = "black") +
    labs(title = paste(preds_AC_atx_list[[i]]$model[1], "-- Anabaena ATX"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# response distribtuions
hist(observed$resp_M_cover_norm)
hist(observed$resp_AC_cover_norm)
hist(observed$resp_M_atx_norm)
hist(observed$resp_AC_atx_norm)
