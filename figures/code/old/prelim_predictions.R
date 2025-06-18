## predictions figure

library("tidyverse")

preds_M <- read.csv("./data/predictive_models/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
preds_AC <- read.csv("./data/predictive_models/predictions_AC_cover.csv")
atx_M <- read.csv("./data/predictive_models/predictions_M_atx.csv")
atx_AC <- read.csv("./data/predictive_models/predictions_AC_cover.csv")
observed <- read.csv("./data/predictive_models/inputs.csv") %>% 
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
