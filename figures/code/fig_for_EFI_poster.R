# figure to show relationship of 
# cover + atx
# cover + GPP
# atx + GPP

# averaging all plots together
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum, 
         TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g, GPP_median_fourdaysprior) %>% 
  mutate(field_date = ymd(field_date))

data$TM_ATX_all_ug_orgmat_g <- replace_na(data$TM_ATX_all_ug_orgmat_g , 0)
data$TAC_ATX_all_ug_orgmat_g <- replace_na(data$TAC_ATX_all_ug_orgmat_g , 0)

data_site <- data
data_site[,c(4:8)] <- apply(data_site[,c(4:8)], MARGIN = 2, function(x) ave(x, data$site_reach, FUN = scale))

# plot
data_site_longer1 <- pivot_longer(data_site, col = c(4,5), values_to = "cover", names_to = "taxa") %>% 
  select(!c(TM_ATX_all_ug_orgmat_g, TAC_ATX_all_ug_orgmat_g))
data_site_longer2 <- data_site %>% 
  select(!(c(microcoleus, anabaena_cylindrospermum, GPP_median_fourdaysprior))) %>% 
  dplyr::rename(microcoleus = TM_ATX_all_ug_orgmat_g,
         anabaena_cylindrospermum = TAC_ATX_all_ug_orgmat_g)
data_site_longer2 <- pivot_longer(data_site_longer2, col = c(4,5), names_to = "taxa", values_to = "atx")

data_site_longer <- left_join(data_site_longer1, data_site_longer2, by = c("field_date", "site", "site_reach", "taxa"))

# replace field_date for 7.11.2023 as 7.10 for STH
data_site_longer[which(data_site_longer$field_date == ymd("2023-07-11")),]$field_date <- ymd("2023-07-10")

# summarize data
data_summary <- data_site_longer %>% 
  dplyr::group_by(field_date, taxa) %>% 
  dplyr::summarize(mean_cover = mean(cover),
                   mean_atx = mean(atx),
                   mean_GPP = mean(GPP_median_fourdaysprior))

theme_set(theme_bw() +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                  panel.border = element_rect(linewidth = 3), axis.ticks = element_line(linewidth = 2.8),
                  text = element_text(size = 30), plot.margin = unit(c(.5, 0, 0, 0), "cm"),
                  axis.ticks.length=unit(.25, "cm")))

# fig!
atx_cover <- ggplot(data = data_summary, aes(x = mean_cover, y = mean_atx)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none", strip.background = element_blank(), 
        strip.text.x = element_blank())
atx_cover

cover_GPP <- ggplot(data = data_summary, aes(x = mean_GPP, y = mean_cover)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none", strip.background = element_blank(), 
        strip.text.x = element_blank())
cover_GPP

cover_GPP <- ggplot(data = data_summary, aes(x = mean_GPP, y = mean_atx)) +
  geom_point(aes(color = taxa), shape = 16, size = 4) +
  geom_path(aes(color = taxa), arrow = arrow(length = unit(0.55, "cm"))) + 
  scale_color_manual("Group", values = c("#8f8504","#2871c7"),
                     labels = c("Anabaena & Cylindrospermum", "Microcoleus")) +
  facet_wrap(~taxa) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none", strip.background = element_blank(), 
        strip.text.x = element_blank())
cover_GPP

