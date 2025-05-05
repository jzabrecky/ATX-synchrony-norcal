## just doing his with 

library(tidyverse)

data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum,
         TM_ATX_all_ug_g, TAC_ATX_all_ug_g, GPP_median_fourdaysprior)

data$TM_ATX_all_ug_g <- replace_na(data$TM_ATX_all_ug_g , 0)
data$TAC_ATX_all_ug_g <- replace_na(data$TAC_ATX_all_ug_g , 0)

data <- data %>% 
  na.omit()

# standardize by site
data_site <- data
data_site[,c(4:8)] <- apply(data[,c(4:8)], MARGIN = 2, function(x) ave(x, data$site, FUN = scale))

# synchrony at site level
site_synchrony <- data_site %>% 
  group_by(site) %>% 
  summarize(cor_M = cor(microcoleus, TM_ATX_all_ug_g),
            cor_AC = cor(anabaena_cylindrospermum, TAC_ATX_all_ug_g))

# standardize by site_reach
data_reach <- data
data_reach[,c(4:8)] <- apply(data[,c(4:8)], MARGIN = 2, function(x) ave(x, data$site_reach, FUN = scale))

reach_synchrony <- data_site %>% 
  group_by(site_reach) %>% 
  summarize(cor_M = cor(microcoleus, TM_ATX_all_ug_g),
            cor_AC = cor(anabaena_cylindrospermum, TAC_ATX_all_ug_g),
            cor_M_cov_GPP = cor(microcoleus, GPP_median_fourdaysprior),
            cor_A_cov_GPP = cor(anabaena_cylindrospermum, GPP_median_fourdaysprior))
# getting more negative than I thought-- maybe need real z-score

cover_atx <- ggplot(data = data, aes(x = microcoleus, y = TM_ATX_all_ug_g)) +
  geom_point() +
  facet_wrap(~site)
cover_atx

cover_atx_AC <- ggplot(data = data_site, aes(x = anabaena_cylindrospermum, y = TAC_ATX_all_ug_g)) +
  geom_point() +
  coord_cartesian(xlim = c(-4,4), ylim = c(-4, 4)) +
  facet_wrap(~site)
cover_atx_AC

# maybe would still want to average across all reaches for site level?
# yeah I think that is what we want to do
