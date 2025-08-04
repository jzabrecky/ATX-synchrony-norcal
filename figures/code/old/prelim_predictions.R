## predictions figure

library("tidyverse")
library("lubridate")




# plot colors
palette <- c("#523939", "#528b87", "#416f16", "#62a7f8", "#8b9609", "gray", "#bdb000", "#90ac7c")
ribbon_palette <- c("#c9a3a3", "#aed4d1", "#c7e3ac", "#c7e1ff", "#dee3a1", "gray", "#ebe7b0", "#d6edc5")

observed <- read.csv("./data/predictive_models/inputs.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_final <- read.csv("./data/predictive_models/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_0.05_test <- read.csv("./data/predictive_models/compare_zero_sub_values/0.05/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_log <- read.csv("./data/predictive_models/log_ver/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_oldlinear <- read.csv("./data/predictive_models/july_stuff/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_oddreachout <- read.csv("./data/predictive_models/predictions_M_cover_oddreachout.csv") %>% 
  mutate(field_date = ymd(field_date))
AC_cover_final <- read.csv("./data/predictive_models/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
AC_cover_log <- read.csv("./data/predictive_models/log_ver/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_atx_final <- read.csv("./data/predictive_models/predictions_M_atx.csv") %>% 
  mutate(field_date = ymd(field_date))
AC_atx_final <- read.csv("./data/predictive_models/predictions_AC_atx.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_logtest_0.6 <- read.csv("./data/predictive_models/log_tests/starting_value_0.6/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_logtest_0.05 <- read.csv("./data/predictive_models/log_tests/starting_value_0.05/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_logtest_0.01 <- read.csv("./data/predictive_models/log_tests/starting_value_0.01/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
M_cover_logtest_0.005 <- read.csv("./data/predictive_models/log_tests/starting_value_0.005/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))


AC_cover_logtest_0.6 <- read.csv("./data/predictive_models/log_tests/starting_value_0.6/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
AC_cover_logtest_0.05 <- read.csv("./data/predictive_models/log_tests/starting_value_0.05/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
AC_cover_logtest_0.01 <- read.csv("./data/predictive_models/log_tests/starting_value_0.01/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
AC_cover_logtest_0.005 <- read.csv("./data/predictive_models/log_tests/starting_value_0.005/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))

# microcoleus cover predictions (final model?)
preds_M_list <- split(M_cover_final, M_cover_final$model) # split into list

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

# log test; microcoleus cover predictions
preds_M_list_log <- split(M_cover_log, M_cover_log$model) # split into list

for(i in 1:length(preds_M_list_log)) {
  plot <- ggplot(data = preds_M_list_log[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_log[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}


# log test; anabaena/cylindrospermum cover predictions
preds_AC_cover_list <- split(AC_cover_log, AC_cover_log$model) # split into list

for(i in 1:length(preds_AC_cover_list)) {
  plot <- ggplot(data = preds_AC_cover_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_AC_cover_list[[i]]$model[1], "-- Anabaena/Cylindrospermum cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# old linear predictions
preds_M_list_linear <- split(M_cover_oldlinear, M_cover_oldlinear$model)

for(i in 1:length(preds_M_list_linear)) {
  plot <- ggplot(data = preds_M_list_linear[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_linear[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_M_list_oddreachout <- split(M_cover_oddreachout, M_cover_oddreachout$model) # split into list

for(i in 1:length(preds_M_list_oddreachout)) {
  plot <- ggplot(data = preds_M_list_oddreachout[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_oddreachout[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_M_list_0.05 <- split(M_cover_0.05_test, M_cover_0.05_test$model) # split into list

for(i in 1:length(preds_M_list_0.05)) {
  plot <- ggplot(data = preds_M_list_0.05[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_0.05[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# anabaena/cylindrospermum cover predictions (final model?)
preds_AC_list <- split(AC_cover_final, AC_cover_final$model) # split into list

for(i in 1:length(preds_AC_list)) {
  plot <- ggplot(data = preds_AC_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_AC_list[[i]]$model[1], "-- Anabaena/Cyl cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# microcoleus cover predictions (final model?)
preds_M_list <- split(M_cover_final, M_cover_final$model) # split into list

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

# microcoleus cover predictions (final model?)
preds_M_list <- split(M_cover_final, M_cover_final$model) # split into list

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

atx_palette <- c("#523939", "#523939", "#528b87",  "#528b87", "#416f16", "#416f16", 
                 "#62a7f8", "#62a7f8", "#8b9609", "#8b9609", "gray", "#bdb000",  "#bdb000",
                 "#90ac7c", "#90ac7c")
atx_ribbon_palette <- c("#c9a3a3", "#c9a3a3", "#aed4d1", "#aed4d1", "#c7e3ac", "#c7e3ac",
                        "#c7e1ff", "#c7e1ff", "#dee3a1", "#dee3a1", "gray", "#ebe7b0", "#ebe7b0", 
                        "#d6edc5", "#d6edc5")

# microcoleus anatoxins predictions (final model?)
preds_M_list_atx <- split(M_atx_final, M_atx_final$model) # split into list

for(i in 1:length(preds_M_list_atx)) {
  plot <- ggplot(data = preds_M_list_atx[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = atx_ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = atx_palette[i]) +
    geom_point(data = observed, aes(y = resp_M_atx_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_atx[[i]]$model[1], "-- Microcoleus atx"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_AC_list_atx <- split(AC_atx_final, AC_atx_final$model) # split into list

for(i in 1:length(preds_AC_list_atx)) {
  plot <- ggplot(data = preds_AC_list_atx[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = atx_ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = atx_palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_atx_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_AC_list_atx[[i]]$model[1], "-- Microcoleus atx"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}


### log starting value tests

# log test; microcoleus cover predictions
preds_M_list_log_0.6 <- split(M_cover_logtest_0.6, M_cover_logtest_0.6$model) # split into list

for(i in 1:length(preds_M_list_log_0.6)) {
  plot <- ggplot(data = preds_M_list_log_0.6[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_log_0.6[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_M_list_log_0.05 <- split(M_cover_logtest_0.05, M_cover_logtest_0.6$model) # split into list

for(i in 1:length(preds_M_list_log_0.05)) {
  plot <- ggplot(data = preds_M_list_log_0.05[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_log_0.05[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_M_list_log_0.01 <- split(M_cover_logtest_0.01, M_cover_logtest_0.01$model) # split into list

for(i in 1:length(preds_M_list_log_0.01)) {
  plot <- ggplot(data = preds_M_list_log_0.01[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_log_0.01[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_M_list_log_0.005 <- split(M_cover_logtest_0.005, M_cover_logtest_0.005$model) # split into list

for(i in 1:length(preds_M_list_log_0.005)) {
  plot <- ggplot(data = preds_M_list_log_0.005[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_log_0.005[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# log test; microcoleus cover predictions
preds_AC_list_log_0.6 <- split(AC_cover_logtest_0.6, AC_cover_logtest_0.6$model) # split into list

for(i in 1:length(preds_AC_list_log_0.6)) {
  plot <- ggplot(data = preds_AC_list_log_0.6[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_log_0.6[[i]]$model[1], "-- Anabaena cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_AC_list_log_0.05 <- split(AC_cover_logtest_0.05, AC_cover_logtest_0.6$model) # split into list

for(i in 1:length(preds_AC_list_log_0.05)) {
  plot <- ggplot(data = preds_AC_list_log_0.05[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_AC_list_log_0.05[[i]]$model[1], "-- Anabaena cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

preds_AC_list_log_0.01 <- split(AC_cover_logtest_0.01, AC_cover_logtest_0.01$model) # split into list

for(i in 1:length(preds_AC_list_log_0.01)) {
  plot <- ggplot(data = preds_AC_list_log_0.01[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_AC_list_log_0.01[[i]]$model[1], "-- Anabaena cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}


preds_AC_list_log_0.005 <- split(AC_cover_logtest_0.005, AC_cover_logtest_0.005$model) # split into list

for(i in 1:length(preds_AC_list_log_0.005)) {
  plot <- ggplot(data = preds_AC_list_log_0.005[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_AC_list_log_0.005[[i]]$model[1], "-- Anabaena cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}









##### older stuff below


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
preds_M_logged_0.1 <- read.csv("./data/predictive_models/add_0.1_withtruncation/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
preds_AC_logged_0.1 <- read.csv("./data/predictive_models/add_0.1_withtruncation/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
option2_M <- read.csv("./data/predictive_models/option2/predictions_M_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
option2_AC <- read.csv("./data/predictive_models/option2/predictions_AC_cover.csv") %>% 
  mutate(field_date = ymd(field_date))
debug_trial <- read.csv("./data/predictive_models/option2/debug_trial/predictions_M_cover.csv") %>% 
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

# debug trial
debug_M_list <- split(debug_trial, debug_trial$model)

for(i in 1:length(debug_M_list)) {
  plot <- ggplot(data = debug_M_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(debug_M_list[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# microcoleus cover predictions (option 2)
preds_M_list_opt2 <- split(option2_M, option2_M$model) # split into list

for(i in 1:length(preds_M_list_opt2)) {
  plot <- ggplot(data = preds_M_list_opt2[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    labs(title = paste(preds_M_list_opt2[[i]]$model[1], "-- Microcoleus cover"), y = "% of max at reach") +
    facet_wrap(~site_reach) +
    theme_bw()
  print(plot)
}

# logged (changing as i go for latest predictions)
preds_M_logged_list <- split(preds_M_logged_0.1, preds_M_logged_0.1$model) # split into list

for(i in 1:length(preds_M_logged_list)) {
  plot <- ggplot(data = preds_M_logged_list[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_M_cover_norm), color = "black",
               shape = 18) +
    ylim(0,100) +
    labs(title = paste(preds_M_logged_list[[i]]$model[1], "-- Microcoleus cover (added 0.1 to 0"), y = "% of max at reach") +
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

# logged anabaena/cylindrospermum cover predictions version
preds_AC_list_logged <- split(preds_AC_logged_0.1, preds_AC_logged_0.1$model) # split into list

for(i in 1:length(preds_AC_list_logged)) {
  plot <- ggplot(data = preds_AC_list_logged[[i]], aes(x = field_date)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, group = 1), fill = ribbon_palette[i], alpha = 0.8) +
    geom_point(aes(y = mean), size = 3, color = palette[i]) +
    geom_point(data = observed, aes(y = resp_AC_cover_norm), color = "black") +
    labs(title = paste(preds_AC_list_logged[[i]]$model[1], "-- Anabaena Cover with 0.1 added to 0's"), y = "% of max at reach") +
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

# logged anabaena cover version

# response distribtuions
hist(observed$resp_M_cover_norm)
hist(observed$resp_AC_cover_norm)
hist(observed$resp_M_atx_norm)
hist(observed$resp_AC_atx_norm)
