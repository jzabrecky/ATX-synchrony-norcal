#### this script is a work in progress but standardizes 
### cover, anatoxins, and GPP (at reach and site level)
### to evaluate synchrony of those processes

# only have sfk eel 2023 data currently
data <- read.csv("./data/field_and_lab/sfkeel23_combined.csv") %>% 
  select(field_date, site_reach, site, microcoleus, anabaena_cylindrospermum,
         TM_ATX_all_ug_g, TAC_ATX_all_ug_g, GPP_median_fourdaysprior)

data$TM_ATX_all_ug_g <- replace_na(data$TM_ATX_all_ug_g , 0)
data$TAC_ATX_all_ug_g <- replace_na(data$TAC_ATX_all_ug_g , 0)

data_reach <- data
data_reach[,c(4:8)] <- apply(data[,c(4:8)], MARGIN = 2, function(x) ave(x, data$site_reach, FUN = scale))

# save csv
write.csv(data_reach, "./data/field_and_lab/synchrony_stnd_per_reach.csv")
