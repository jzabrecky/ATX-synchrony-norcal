#### read in data & libraries

library(tidyverse)
library(ggsignif)

data <- read.csv("./data/predictive_models/inputs.csv")

cover_nrmse_M <- read.csv("./data/predictive_models/20250311_nrmse_M_cover_truncated.csv")
cover_nrmse_A <- read.csv("./data/predictive_models/20250311_nrmse_A_cover_truncated.csv")

cover_nrmse_M_long <- pivot_longer(cover_nrmse_M, cols = c(2:6), names_to = "site_reach",
                                 values_to = "nrmse")

cover_nrmse_M_long_null <- cover_nrmse_M_long %>% 
  filter(model == "null")

cover_nrmse_A_long <- pivot_longer(cover_nrmse_A, cols = c(2:6), names_to = "site_reach",
                                   values_to = "nrmse")

cover_nrmse_A_long_null <- cover_nrmse_A_long %>% 
  filter(model == "null")
# duplicate it
#cover_nrmse_M_long_null <- rbind(cover_nrmse_M_long_null, cover_nrmse_M_long_null)

cover_nrmse_M_long <- cover_nrmse_M_long %>% 
  filter(model != "null")

cover_nrmse_A_long <- cover_nrmse_A_long %>% 
  filter(model != "null")

theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                  panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 22), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm")))

nulls <- cover_nrmse_M_long_null$nrmse
dodge <- position_dodge(width=0.5)  
plot <- ggplot(data = cover_nrmse_M_long, aes(x = site_reach, y = nrmse, fill = model,
                                              shape = model)) +
  geom_point(size = 6, alpha = 0.9, position = dodge) +
  geom_signif(y_position = c(nulls), xmin = c(0.6,1.6,2.6,3.6,4.6), 
              xmax = c(1.4,2.4,3.4,4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  coord_cartesian(ylim = c(0.18, 0.47)) +
  scale_x_discrete(labels= c("SFE-M-1S", "SFE-M-2", "SFE-M-3", "SFE-M-4", "SFE-SH-1S")) +
  scale_fill_manual(values = c("#bdb000", "#62a7f8", "#416f16"),
                    limits = c("physical", "chemical", "biological")) +
  scale_shape_manual(values = c(24, 22, 23),
                       limits = c("physical", "chemical", "biological")) +
  theme(axis.text.x = element_text(angle = 15, vjust = .8, size = 18),
        plot.title = element_text(size = 20)) +
  labs(title = "Microcoleus Cover Prediction nRMSE", x = NULL, y = "nRMSE")
plot

nulls <- cover_nrmse_A_long_null$nrmse
plot2 <- ggplot(data = cover_nrmse_A_long, aes(x = site_reach, y = nrmse, fill = model,
                                              shape = model)) +
  geom_point(size = 6, alpha = 0.9, position = dodge) +
  geom_signif(y_position = c(nulls), xmin = c(0.6,1.6,2.6,3.6,4.6), 
              xmax = c(1.4,2.4,3.4,4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  coord_cartesian(ylim = c(0.18, 0.47)) +
  scale_x_discrete(labels= c("SFE-M-1S", "SFE-M-2", "SFE-M-3", "SFE-M-4", "SFE-SH-1S")) +
  scale_fill_manual(values = c("#bdb000", "#62a7f8", "#416f16"),
                    limits = c("physical", "chemical", "biological")) +
  scale_shape_manual(values = c(24, 22, 23),
                     limits = c("physical", "chemical", "biological")) +
  theme(axis.text.x = element_text(angle = 15, vjust = .8, size = 18),
        plot.title = element_text(size = 16)) +
  labs(title = "Anabaena/Cylindrospermum Cover Prediction nRMSE", x = NULL, y = "nRMSE")
plot2

# REMOVE LEGEND
library(cowplot)
plot_grid(plot, plot2, nrow = 2, labels = c("A", "B"))
        