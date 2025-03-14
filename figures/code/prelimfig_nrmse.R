#### read in data & libraries

library(tidyverse)

cover_nrmse <- read.csv("./data/predictive_models/20250311_nrmse_M_cover.csv")
atx_nrmse <- read.csv("")

cover_nrmse_long <- pivot_longer(cover_nrmse, cols = c(2:6), names_to = "site_reach",
                                 values_to = "nrmse") %>% 
  filter(model != "null")

plot <- ggplot(data = cover_nrmse_long, aes(x = site_reach, y = nrmse, color = model,
                                            shape = model)) +
  geom_point(size = 3) +
  theme_bw()
plot
