#### Supplemental figure to explore cover, ATX, and GPP relationships
### Jordan Zabrecky
## last edited: 06.21.2025

# This script creates a supplementary figure to explore the relationships
# between taxa-specific cover and ATX and also GPP

# maybe will separate out anabaena/microcoleus data and do facet wrap for each river!

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot", "gridExtra", "grid",
         "ggtext"), 
       require, character.only = T)

# loading in cover, atx, and median GPP four days prior to field date
data <- read.csv("./data/field_and_lab/allrivers22_combined.csv") %>% 
  mutate(field_date = ymd(field_date)) %>% 
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum,
         TM_ATX_all_ug_g, TAC_ATX_all_ug_g, GPP_median_fourdaysprior)

# fill in ATX NAs with zero
data$TM_ATX_all_ug_g <- replace_na(data$TM_ATX_all_ug_g, 0)
data$TAC_ATX_all_ug_g <- replace_na(data$TAC_ATX_all_ug_g, 0)

# set universal theme for plots
theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), strip.background = element_blank(),
                  panel.grid.major = element_blank(), panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 15), strip.text = element_text(size = 15),
                  plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm"), 
                  plot.title = element_markdown(hjust = 0.5)))

#### (2) Cover & ATX figures ####

# need to make manual dataframes so that two sites are not
# linked together in geom_segment
data$end_micro_cover <- c(data$microcoleus[-1], NA)
data$end_anacyl_cover <- c(data$anabaena_cylindrospermum[-1], NA)
data$end_TM_atx <-  c(data$TM_ATX_all_ug_g[-1], NA)
data$end_TAC_atx <- c(data$TAC_ATX_all_ug_g[-1], NA)
data$end_GPP <- c(data$GPP_median_fourdaysprior[-1], NA)

# add in NAs to cut the segment on last field days
data[which(data$field_date == ymd("2022-09-15")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx", "end_GPP")] <- NA
data[which(data$field_date == ymd("2022-09-17")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx", "end_GPP")] <- NA
data[which(data$field_date == ymd("2022-09-22")), 
     c("end_micro_cover", "end_anacyl_cover", "end_TM_atx", "end_TAC_atx", "end_GPP")] <- NA

## anabaena/cylindrospermum

# south fork eel river
sfkeel_ana <- ggplot(data = data[which(data$site == "SFE-M"),], 
                     aes(anabaena_cylindrospermum, TAC_ATX_all_ug_g)) +
                geom_point(aes(color = site_reach), size = 4.5) +
                geom_segment(data = data[which(data$site == "SFE-M"),],
                             linewidth = 1.1, alpha = 0.8,
                             aes(xend = end_anacyl_cover,
                                 yend = end_TAC_atx,
                                 color = site_reach),
                             arrow = arrow(length = unit(3, "mm"))) +
                scale_color_manual("Reach:", values = c("#5e5801", "#8f8504", "#e0d42f")) + 
                labs(y = expression(paste(mu, "g ATX per g OM"), sep = ""), 
                     x = "% cover") +
                scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
                theme(legend.position = "top")
sfkeel_ana

# russian
rus_ana <- ggplot(data = data[which(data$site == "RUS"),], 
                   aes(anabaena_cylindrospermum, TAC_ATX_all_ug_g)) +
              geom_point(aes(color = site_reach), size = 4.5) +
              geom_segment(data = data[which(data$site == "RUS"),],
                           linewidth = 1.1, alpha = 0.8,
                           aes(xend = end_anacyl_cover,
                               yend = end_TAC_atx,
                               color = site_reach),
                arrow = arrow(type = "open", length = unit(0.15, "inches"))) + 
              scale_color_manual("Reach:", values = c("#5e5801", "#8f8504", "#e0d42f")) + 
              labs(y = expression(paste(mu, "g ATX per g OM"), sep = ""), 
                   x = "% cover") +
              scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
              theme(legend.position = "top")

## microcoleus

# south fork eel river
sfkeel_micro <- ggplot(data = data[which(data$site == "SFE-M"),], 
                       aes(microcoleus, TM_ATX_all_ug_g)) +
                  geom_point(aes(color = site_reach), size = 4.5) +
                  geom_segment(data = data[which(data$site == "SFE-M"),],
                               linewidth = 1.1, alpha = 0.8,
                               aes(xend = end_micro_cover,
                                   yend = end_TM_atx,
                                   color = site_reach),
                               arrow = arrow(length = unit(3, "mm"))) +
                  scale_color_manual("Reach:", values = c("#142d4a", "#2871c7", "#7eb8fc")) + 
                  labs(y = expression(paste(mu, "g ATX per g OM"), sep = ""), 
                       x = "% cover") +
                  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
                  theme(legend.position = "top")
sfkeel_micro

# salmon
sal_micro <- ggplot(data = data[which(data$site == "SAL"),], 
                   aes(microcoleus, TM_ATX_all_ug_g)) +
              geom_point(aes(color = site_reach), size = 4.5) +
              geom_segment(data = data[which(data$site == "SAL"),],
                           linewidth = 1.1, alpha = 0.8,
                           aes(xend = end_micro_cover,
                               yend = end_TM_atx,
                               color = site_reach),
                           arrow = arrow(type = "open", length = unit(0.15, "inches"))) + 
              scale_color_manual("Reach:", values = c("#142d4a", "#2871c7", "#7eb8fc")) + 
              labs(y = expression(paste(mu, "g ATX per g OM"), sep = ""), 
                   x = "% cover") +
              scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
              theme(legend.position = "top")
sal_micro

#### (3) Cover & GPP figures ####

## anabaena

# south fork eel river
sfkeel_ana_gpp <- ggplot(data = data[which(data$site == "SFE-M"),], 
                     aes(GPP_median_fourdaysprior, anabaena_cylindrospermum)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "SFE-M"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_anacyl_cover,
                   color = site_reach),
               arrow = arrow(length = unit(3, "mm"))) +
  scale_color_manual("Reach:", values = c("#5e5801", "#8f8504", "#e0d42f")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       y = "% cover") +
  theme(legend.position = "top")
sfkeel_ana_gpp

# russian river
rus_ana_gpp <- ggplot(data = data[which(data$site == "RUS"),], 
                         aes(GPP_median_fourdaysprior, anabaena_cylindrospermum)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "RUS"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_anacyl_cover,
                   color = site_reach),
               arrow = arrow(length = unit(3, "mm"))) +
  scale_color_manual("Reach:", values = c("#5e5801", "#8f8504", "#e0d42f")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       y = "% cover") +
  theme(legend.position = "top")
rus_ana_gpp

## microcoleus

# south fork eel river
sfkeel_micro_gpp <- ggplot(data = data[which(data$site == "SFE-M"),], 
                       aes(GPP_median_fourdaysprior, microcoleus)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "SFE-M"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_micro_cover,
                   color = site_reach),
               arrow = arrow(length = unit(3, "mm"))) +
  scale_color_manual("Reach:", values = c("#142d4a", "#2871c7", "#7eb8fc")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       y = "% cover") +
  theme(legend.position = "top")
sfkeel_micro_gpp

# salmon
salmon_micro_gpp <- ggplot(data = data[which(data$site == "SAL"),], 
                    aes(GPP_median_fourdaysprior, microcoleus)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "SAL"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_micro_cover,
                   color = site_reach),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) + 
  scale_color_manual("Reach:", values = c("#142d4a", "#2871c7", "#7eb8fc")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       y = "% cover") +
  theme(legend.position = "top")
salmon_micro_gpp

#### (4) ATX & GPP figures ####

## anabaena

# south fork eel river
sfkeel_TAC_atx_gpp <- ggplot(data = data[which(data$site == "SFE-M"),], 
                         aes(GPP_median_fourdaysprior, TAC_ATX_all_ug_g)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "SFE-M"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_TAC_atx,
                   color = site_reach),
               arrow = arrow(length = unit(3, "mm"))) +
  scale_color_manual("Reach:", values = c("#5e5801", "#8f8504", "#e0d42f")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       expression(paste(mu, "g ATX per g OM"), sep = "")) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
sfkeel_TAC_atx_gpp

# russian river
rus_TAC_atx_gpp <- ggplot(data = data[which(data$site == "RUS"),], 
                      aes(GPP_median_fourdaysprior, TAC_ATX_all_ug_g)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "RUS"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_TAC_atx,
                   color = site_reach),
               arrow = arrow(length = unit(3, "mm"))) +
  scale_color_manual("Reach:", values = c("#5e5801", "#8f8504", "#e0d42f")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       y = expression(paste(mu, "g ATX per g OM"), sep = "")) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
rus_TAC_atx_gpp

## microcoleus

# south fork eel river
sfkeel_TM_atx_gpp <- ggplot(data = data[which(data$site == "SFE-M"),], 
                           aes(GPP_median_fourdaysprior, TM_ATX_all_ug_g)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "SFE-M"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_TM_atx,
                   color = site_reach),
               arrow = arrow(length = unit(3, "mm"))) +
  scale_color_manual("Reach:", values = c("#142d4a", "#2871c7", "#7eb8fc")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       y = expression(paste(mu, "g ATX per g OM"), sep = "")) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
sfkeel_TM_atx_gpp

# salmon
salmon_TM_atx_gpp <- ggplot(data = data[which(data$site == "SAL"),], 
                           aes(GPP_median_fourdaysprior, TM_ATX_all_ug_g)) +
  geom_point(aes(color = site_reach), size = 4.5) +
  geom_segment(data = data[which(data$site == "SAL"),],
               linewidth = 1.1, alpha = 0.8,
               aes(xend = end_GPP,
                   yend = end_TM_atx,
                   color = site_reach),
               arrow = arrow(type = "open", length = unit(0.15, "inches"))) + 
  scale_color_manual("Reach:", values = c("#142d4a", "#2871c7", "#7eb8fc")) + 
  labs(x = expression(paste("g O"[2], " m"^-2, " d"^-1)), 
       y = expression(paste(mu, "g ATX per g OM"), sep = "")) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
  theme(legend.position = "top")
salmon_TM_atx_gpp

#### (5) Joining together figures for final ####

# tbd; may make individual dataframes and facet wrap

# making rows for each taxa
anacyl_row <- plot_grid(sfkeel_ana, rus_ana, 
          ncol = 2, align = "hv", scale = 0.95)
micro_row <- plot_grid(sfkeel_micro, sal_micro,
                       ncol = 2, align = "hv")

# adding title taxa label to row
anacyl_w_title <- grid.arrange(anacyl_row, top = textGrob("Anabaena/Cylindrospermum", 
                                        gp = gpar(fontsize = 20, fontface = "italic")))
micro_w_title <- grid.arrange(micro_row, top = textGrob("Microcoleus", 
                                                          gp = gpar(fontsize = 20, fontface = "italic")))

# final
final <- plot_grid(anacyl_w_title, micro_w_title, align = "hv", nrow = 2)
# didn't really work...

finalfinal <- plot_grid(sfkeel_ana, rus_ana, sfkeel_micro, sal_micro, 
                       ncol = 2, labels = c("A", "B", "C", "D"))
# maybe still need to do alignment work