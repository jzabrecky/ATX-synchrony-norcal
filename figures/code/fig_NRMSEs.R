#### Main figure to show posterior NRMSEs
### Jordan Zabrecky
## 10.27.2025

## This figure creates a main figure showing the posterior NRMSEs for 
## model predicting (a) Microcoleus vs. Anabaena/Cylindrospermum cover,
## (b) Microcoleus anatoxins with and without cover as a covariate,
## (c) Anabaena/Cylindrospermum anatoxins with and without cover as a covariate

## note: requires being on a computer where model RDS objects are stored!

#### (1) Loading libraries & data ####

# load packages & data from processing script
source("./code/3g_NRMSE_summaries.R")

# load additional library
library("cowplot")

# load in null NRMSE information
null_models <- ldply(list.files(path = "./data/predictive_models/", pattern = "NRMSE"), 
                     function(filename) {
                       d <- read.csv(paste("./data/predictive_models/", filename, sep = "")) %>% 
                         # add in what we are predicting from file name
                         mutate(predicting = str_remove(filename, "NRMSE_" )) %>% 
                         mutate(predicting = str_remove(predicting, ".csv")) %>% 
                         # add in factored column
                         mutate(predicting_f = factor(predicting, levels = c("M_cover", "AC_cover", "M_atx", "AC_atx"))) %>% 
                         # only null models
                         filter(model == "null")
                     })

#### (2) Making figures ####

# set universal theme
theme_set(theme_bw() + theme(panel.grid.minor = element_blank(),
                             panel.border = element_rect(linewidth = 1.2), axis.ticks = element_line(linewidth = 1),
                             text = element_text(size = 10), axis.ticks.length=unit(.25, "cm"),
                             axis.title.y = ggtext::element_markdown(size = 10), 
                             axis.text.x = element_text(size = 10),
                             axis.text.y = element_text(size = 10),
                             plot.title = ggtext::element_markdown(size = 10, hjust = 0.5),
                             strip.text = element_text(face="bold", size=10), strip.background = element_blank(),
                             legend.position = "bottom"))

palette <- c("#E8DE48", "#B4D65E", "#8BCF6F", "#57C785", "#47A27E", "#387E77", "#1E426B")
m_palette <- c("#0D3665", "#2F63A0", "#5697E3", "#6180bb", "#72B0F9", "#A9CAF0", "#D9E7F5")
m_palette <- c("#D9E7F5", "#A9CAF0", "#72B0F9", "#6180bb", "#5697E3", "#2F63A0", "#0D3665")
ac_palette <- c("#716B12", "#98901E", "#BDB329", "#EBDF38", "#EEE675", "#EFEA9C", "#F1EFC9")
ac_palette <- c("#F1EFC9", "#EFEA9C", "#EEE675", "#EBDF38", "#BDB329", "#98901E", "#716B12")

## (a) M vs. AC cover model

# add in factor for desired order
test_a_data <- test_a_data %>% 
  mutate(predicting_f = factor(predicting, levels = c("M_cover", "AC_cover")),
         model_f = factor(model, levels = c("physical", "chemical", "biological", "physicochemical",
                          "ecohydrological", "biochemical", "all")))

# plot option 1
cover_NRMSEs <- ggplot(data = test_a_data, aes(x = x, fill = interaction(model_f, predicting_f))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = palette) +
  scale_fill_manual(values = c(m_palette, ac_palette)) +
  facet_wrap(~predicting_f, ncol = 1) +
  geom_vline(data = null_models %>% filter(predicting %in% c("M_cover", "AC_cover")),
             aes(xintercept = mean), linetype = "dashed", color = "#2e2e2e") +
  xlim(0.04, 0.89) +
  labs(x = NULL, y = NULL) #+
  #theme(legend.position = "none")
cover_NRMSEs

# plot option 2
cover_NRMSEs2 <- ggplot(data = test_a_data, aes(x = x, fill = interaction(model_f, predicting_f))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = palette) +
  scale_fill_manual(values = c(palette, palette)) +
  facet_wrap(~predicting_f, ncol = 1) +
  geom_vline(data = null_models %>% filter(predicting %in% c("M_cover", "AC_cover")),
             aes(xintercept = mean), linetype = "dashed", color = "#2e2e2e") +
  xlim(0.04, 0.89) +
  labs(x = NULL, y = NULL) #+
#theme(legend.position = "none")
cover_NRMSEs2

## (b) M atx model w vs. w/o cover

# add in factor
NRMSE_list$M_atx <- NRMSE_list$M_atx %>% 
  # get base model & factor
  mutate(model_base = str_remove(model, "_w_cover"),
         model_base_f = factor(model_base, levels = c("physical", "chemical", "biological", "physicochemical",
                                            "ecohydrological", "biochemical", "all")))

# plot option 1
atx_m_NRMSEs <- ggplot(data = NRMSE_list$M_atx, aes(x = x, fill = model_base_f)) +
                  geom_density(alpha = 0.6) +
                  scale_fill_manual(values = c(m_palette, m_palette)) +
                  geom_vline(xintercept = null_models$mean[which(null_models$predicting == "M_atx")],
                             linetype = "dashed", color = "#2e2e2e") +
                  xlim(0.06, 0.67) +
                  facet_wrap(~w_cover, ncol = 1) +
                  labs(x = NULL, y = NULL)
atx_m_NRMSEs

# plot option 2 
atx_m_NRMSEs2 <- ggplot(data = NRMSE_list$M_atx, aes(x = x, fill = model_base_f)) +
                  geom_density(alpha = 0.6) +
                  scale_fill_manual(values = c(palette, palette)) +
                  geom_vline(xintercept = null_models$mean[which(null_models$predicting == "M_atx")],
                             linetype = "dashed", color = "#2e2e2e") +
                  xlim(0.06, 0.67) +
                  facet_wrap(~w_cover, ncol = 1) +
                  labs(x = NULL, y = NULL)
atx_m_NRMSEs2

## (c) AC atx model w vs. w/o cover

# add in factor
NRMSE_list$AC_atx <- NRMSE_list$AC_atx %>% 
  # get base model & factor
  mutate(model_base = str_remove(model, "_w_cover"),
         model_base_f = factor(model_base, levels = c("physical", "chemical", "biological", "physicochemical",
                                                      "ecohydrological", "biochemical", "all")))

# plot option 1
atx_ac_NRMSEs <-  ggplot(data = NRMSE_list$AC_atx, aes(x = x, fill = model_base_f)) +
                    geom_density(alpha = 0.6) +
                    scale_fill_manual(values = c(ac_palette, ac_palette)) +
                    geom_vline(xintercept = null_models$mean[which(null_models$predicting == "AC_atx")],
                               linetype = "dashed", color = "#2e2e2e") +
                    xlim(0.06, 0.67) +
                    facet_wrap(~w_cover, ncol = 1) +
                    labs(x = NULL, y = NULL)
atx_ac_NRMSEs

# plot option 2
atx_ac_NRMSEs2 <-  ggplot(data = NRMSE_list$AC_atx, aes(x = x, fill = model_base_f)) +
                    geom_density(alpha = 0.6) +
                    scale_fill_manual(values = c(palette, palette)) +
                    geom_vline(xintercept = null_models$mean[which(null_models$predicting == "AC_atx")],
                               linetype = "dashed", color = "#2e2e2e") +
                    xlim(0.06, 0.67) +
                    facet_wrap(~w_cover, ncol = 1) +
                    labs(x = NULL, y = NULL)
atx_ac_NRMSEs2

#### (3) Putting Figures Together ####

# color option 1
opt1 <- plot_grid(cover_NRMSEs, atx_m_NRMSEs, atx_ac_NRMSEs, align = "h", nrow = 1)
opt1

# color option 2
opt2 <- plot_grid(cover_NRMSEs2, atx_m_NRMSEs2, atx_ac_NRMSEs2, align = "h", nrow = 1)
opt2

