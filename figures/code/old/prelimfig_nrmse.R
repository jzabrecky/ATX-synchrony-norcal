#### read in data & libraries

library(tidyverse)
library(ggsignif)

# SOMETHING UP WITH CONFIDENCE INTERVALS ATM :)

cover_nrmse_M <- read.csv("./data/predictive_models/nrmse_M_cover.csv")
cover_nrmse_AC <- read.csv("./data/predictive_models/nrmse_AC_cover.csv")
atx_nRMSE_M <- read.csv("./data/predictive_models/nrmse_M_atx.csv")
atx_nRMSE_AC <- read.csv("./data/predictive_models/nrmse_AC_atx.csv")
atx_nRMSE_M_w_cover <- read.csv("./data/predictive_models/nrmse_M_atx_w_cover.csv")
atx_nRMSE_AC_w_cover <- read.csv("./data/predictive_models/nrmse_M_atx_w_cover.csv")

cover_M_null <- cover_nrmse_M %>% 
  filter(model == "null")
cover_nrmse_M <- cover_nrmse_M %>% 
  filter(model != "null")

cover_AC_null <- cover_nrmse_AC %>% 
  filter(model == "null")
cover_nrmse_AC <- cover_nrmse_AC %>% 
  filter(model != "null")

atx_nRMSE_M_null <- atx_nRMSE_M %>% 
  filter(model == "null")
atx_nRMSE_M <- atx_nRMSE_M %>% 
  filter(model != "null")

atx_nRMSE_AC_null <- atx_nRMSE_AC %>% 
  filter(model == "null")
atx_nRMSE_AC <- atx_nRMSE_AC %>% 
  filter(model != "null")


atx_nRMSE_M_w_cover <- atx_nRMSE_M_w_cover %>% 
  filter(model != "null")
atx_nRMSE_AC_w_cover <- atx_nRMSE_AC_w_cover %>% 
  filter(model != "null")


cover_plot <- ggplot(data = cover_nrmse_M, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = model, shape = model), size = 7, alpha = 0.8, position = position_dodge(width=0.5), 
             color = "black") +
  geom_signif(y_position = c(cover_M_null$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model), position = position_dodge(width=0.5)) +
  scale_fill_manual(values = c("#523939", "#528b87", "#416f16", "#62a7f8", "#8b9609", "#bdb000", "#90ac7c"),
                     labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                                "biochemical (DIN + ophos + cond + GPP)",
                                "biological (GPP)",
                                "chemical (DIN + ophos + cond)",
                                "ecohydrological (temp + dis + GPP)",
                                "physical (temp + dis)",
                                "physicochemical (temp + dis + DIN + ophos + cond)")) +
  scale_shape_manual(values = c(21, 22, 22, 23, 22, 23, 23),
                     labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                                "biochemical (DIN + ophos + cond + GPP)",
                                "biological (GPP)",
                                "chemical (DIN + ophos + cond)",
                                "ecohydrological (temp + dis + GPP)",
                                "physical (temp + dis)",
                                "physicochemical (temp + dis + DIN + ophos + cond)")) +
  labs(title = "nRMSE for Microcoleus cover predictions") +
  theme_bw()
cover_plot

ac_cover_plot <- ggplot(data = cover_nrmse_AC, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = model, shape = model), size = 7, alpha = 0.8, position = position_dodge(width=0.5), 
             color = "black") +
  geom_signif(y_position = c(cover_AC_null$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  scale_fill_manual(values = c("#523939", "#528b87", "#416f16", "#62a7f8", "#8b9609", "#bdb000", "#90ac7c"),
                    labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                               "biochemical (DIN + ophos + cond + GPP)",
                               "biological (GPP)",
                               "chemical (DIN + ophos + cond)",
                               "ecohydrological (temp + dis + GPP)",
                               "physical (temp + dis)",
                               "physicochemical (temp + dis + DIN + ophos + cond)")) +
  scale_shape_manual(values = c(21, 22, 22, 23, 22, 23, 23),
                     labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                                "biochemical (DIN + ophos + cond + GPP)",
                                "biological (GPP)",
                                "chemical (DIN + ophos + cond)",
                                "ecohydrological (temp + dis + GPP)",
                                "physical (temp + dis)",
                                "physicochemical (temp + dis + DIN + ophos + cond)")) +
  labs(title = "nRMSE for Anabaena/Cylindrospermum cover predictions") +
  theme_bw()
ac_cover_plot

atx_m_plot <- ggplot(data = atx_nRMSE_M, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = model, shape = model), size = 7, alpha = 0.8, position = position_dodge(width=0.5), 
             color = "black") +
  geom_signif(y_position = c(atx_nRMSE_M_null$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model), position = position_dodge(width=0.5)) +
  scale_fill_manual(values = c("#523939", "#528b87", "#416f16", "#62a7f8", "#8b9609", "#bdb000", "#90ac7c"),
                    labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                               "biochemical (DIN + ophos + cond + GPP)",
                               "biological (GPP)",
                               "chemical (DIN + ophos + cond)",
                               "ecohydrological (temp + dis + GPP)",
                               "physical (temp + dis)",
                               "physicochemical (temp + dis + DIN + ophos + cond)")) +
  scale_shape_manual(values = c(21, 22, 22, 23, 22, 23, 23),
                     labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                                "biochemical (DIN + ophos + cond + GPP)",
                                "biological (GPP)",
                                "chemical (DIN + ophos + cond)",
                                "ecohydrological (temp + dis + GPP)",
                                "physical (temp + dis)",
                                "physicochemical (temp + dis + DIN + ophos + cond)")) +
  labs(title = "nRMSE for Microcoleus cover predictions") +
  theme_bw()
atx_m_plot

atx_m_plot_w_cover <- ggplot(data = atx_nRMSE_M_w_cover, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = model, shape = model), size = 7, alpha = 0.8, position = position_dodge(width=0.5), 
             color = "black") +
  geom_signif(y_position = c(atx_nRMSE_M_null$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model), position = position_dodge(width=0.5)) +
  scale_fill_manual(values = c("#523939", "#528b87", "#416f16", "#62a7f8", "#8b9609", "#bdb000", "#90ac7c", "lightgray"),
                    labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                               "biochemical (DIN + ophos + cond + GPP)",
                               "biological (GPP)",
                               "chemical (DIN + ophos + cond)",
                               "ecohydrological (temp + dis + GPP)",
                               "physical (temp + dis)",
                               "physicochemical (temp + dis + DIN + ophos + cond)",
                               "cover only")) +
  scale_shape_manual(values = c(21, 22, 22, 23, 22, 23, 23, 22),
                     labels = c("all (temp + dis + DIN + ophos + cond + GPP)", 
                                "biochemical (DIN + ophos + cond + GPP)",
                                "biological (GPP)",
                                "chemical (DIN + ophos + cond)",
                                "ecohydrological (temp + dis + GPP)",
                                "physical (temp + dis)",
                                "physicochemical (temp + dis + DIN + ophos + cond)",
                                "cover only")) +
  labs(title = "nRMSE for Microcoleus cover predictions") +
  theme_bw()
atx_m_plot_w_cover



# old code below
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
        

# for EFI POSTER

#nRSMSE for cover

# only want those w/ GPP
# also change color scale