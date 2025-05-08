library(tidyverse)
library(ggsignif)

# SOMETHING UP WITH CONFIDENCE INTERVALS ATM :)

cover_nrmse_M <- read.csv("./data/predictive_models/nrmse_M_cover.csv")
cover_nrmse_AC <- read.csv("./data/predictive_models/nrmse_AC_cover.csv")
atx_nRMSE_M <- read.csv("./data/predictive_models/nrmse_M_atx.csv")
atx_nRMSE_AC <- read.csv("./data/predictive_models/nrmse_AC_atx.csv")
atx_nRMSE_M_w_cover <- read.csv("./data/predictive_models/nrmse_M_atx_w_cover.csv")
atx_nRMSE_AC_w_cover <- read.csv("./data/predictive_models/nrmse_M_atx_w_cover.csv")

cover_null_M <- cover_nrmse_M %>% 
  filter(model == "null")
cover_null_AC <- cover_nrmse_AC %>% 
  filter(model == "null")
atx_null_M <- atx_nRMSE_M %>% 
  filter(model == "null")
atx_null_AC <- atx_nRMSE_AC %>% 
  filter(model == "null")

cover_M <- cover_nrmse_M %>% 
  filter(model != "null" & model != "biological" & model != "physicochemical" & model != "all")
cover_AC <- cover_nrmse_AC %>% 
  filter(model != "null" & model != "biological" & model != "physicochemical" & model != "all")


theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                  panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 30), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm")))

# adding class to get order
cover_M <- cover_M %>% 
  mutate(class = case_when(model == "physical" ~ "a",
                           model == "ecohydrological" ~ "b",
                           model == "chemical" ~ "c",
                           model == "biochemical" ~ "d"),
         group = case_when((class == "a" | class == "b") ~ "X",
                           (class == "c" | class == "d") ~ "Y"))
cover_AC <- cover_AC %>% 
  mutate(class = case_when(model == "physical" ~ "a",
                           model == "ecohydrological" ~ "b",
                           model == "chemical" ~ "c",
                           model == "biochemical" ~ "d"))

M_cover_plot <- ggplot(data = cover_M, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = class, shape = class, color = class), size = 7, 
             alpha = 0.8,
             position = position_dodge(width=0.6), stroke = 2) +
  geom_signif(y_position = c(cover_null_M$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  scale_fill_manual(values = c("white", "#8f8504", "white", "#657ef7")) +
  scale_color_manual(values = c("#8f8504", "#8f8504", "#657ef7", "#657ef7")) +
  scale_shape_manual(values = c(21, 21, 22, 22)) +
  #facet_wrap(~group) +
  coord_cartesian(ylim = c(0.2, 0.4)) +
  theme(legend.position = "none", axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_blank())
M_cover_plot


AC_cover_plot <- ggplot(data = cover_AC, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = class, shape = class, color = class), size = 7, 
             alpha = 0.8,
             position = position_dodge(width=0.6), stroke = 2) +
  geom_signif(y_position = c(cover_null_AC$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  scale_fill_manual(values = c("white", "#8f8504", "white", "#657ef7")) +
  scale_color_manual(values = c("#8f8504", "#8f8504", "#657ef7", "#657ef7")) +
  scale_shape_manual(values = c(21, 21, 22, 22)) +
  coord_cartesian(ylim = c(0.2, 0.4)) +
  theme(legend.position = "none", axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())
AC_cover_plot

# joining ATX dataframes
atx_M <- rbind(atx_nRMSE_M, atx_nRMSE_M_w_cover) %>% 
  filter(model == "chemical" | model == "biochemical" | model == "ecohydrological" |
           model == "physical" | model == "w_cover" | model == "biological_w_cover")
atx_AC <- rbind(atx_nRMSE_AC, atx_nRMSE_AC_w_cover) %>% 
  filter(model == "chemical" | model == "biochemical" | model == "ecohydrological" |
           model == "physical" | model == "w_cover" | model == "biological_w_cover")
atx_M <- atx_M %>% 
  mutate(class = case_when(model == "physical" ~ "a",
                           model == "ecohydrological" ~ "b",
                           model == "chemical" ~ "c",
                           model == "biochemical" ~ "d",
                           model == "w_cover" ~ "e",
                           model == "biological_w_cover" ~ "f"))
atx_AC <- atx_AC %>% 
  mutate(class = case_when(model == "physical" ~ "a",
                           model == "ecohydrological" ~ "b",
                           model == "chemical" ~ "c",
                           model == "biochemical" ~ "d",
                           model == "w_cover" ~ "e",
                           model == "biological_w_cover" ~ "f"))

M_atx_plot <- ggplot(data = atx_M, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = class, shape = class, color = class), size = 7, 
             alpha = 0.8,
             position = position_dodge(width=0.7), stroke = 2) +
  geom_signif(y_position = c(atx_null_M$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  scale_fill_manual(values = c("white", "#8f8504", "white", "#657ef7", 
                               "white", "#416f16")) +
  scale_color_manual(values = c("#8f8504", "#8f8504", "#657ef7", "#657ef7",
                                "#416f16", "#416f16")) +
  scale_shape_manual(values = c(21, 21, 22, 22, 23, 23)) +
  coord_cartesian(ylim = c(0.2, 0.4)) +
  theme(legend.position = "none", axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())
M_atx_plot


AC_atx_plot <- ggplot(data = atx_AC, aes(x = site_reach, y = mean)) +
  geom_point(aes(fill = class, shape = class, color = class), size = 7, 
             alpha = 0.8,
             position = position_dodge(width=0.7), stroke = 2) +
  geom_signif(y_position = c(atx_null_AC$mean), xmin = c(0.6, 1.6, 2.6, 3.6, 4.6), 
              xmax = c(1.4, 2.4, 3.4, 4.4, 5.4), annotation = c("", "", "", "" ,""),
              tip_length = 0, size = 0.6) +
  scale_fill_manual(values = c("white", "#8f8504", "white", "#657ef7", 
                               "white", "#416f16")) +
  scale_color_manual(values = c("#8f8504", "#8f8504", "#657ef7", "#657ef7",
                                "#416f16", "#416f16")) +
  scale_shape_manual(values = c(21, 21, 22, 22, 23, 23)) +
  coord_cartesian(ylim = c(0.2, 0.4)) +
  theme(legend.position = "top", axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank())
AC_atx_plot

# thoughts:
# how does GPP improve our predictions
# water chemistry w/ and w/o
# physical w/ and w/o
# all?

# test....
test <- ggplot(data = cover_AC, aes(x = model, y = mean)) +
  geom_point(aes(fill = site_reach, shape = site_reach, color = site_reach), size = 7, 
             alpha = 0.8,
             position = position_dodge(width=0.6), stroke = 2)
test
