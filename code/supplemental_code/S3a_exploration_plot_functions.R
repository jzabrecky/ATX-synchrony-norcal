#### functions for plotting for data exploration scripts
### Jordan Zabrecky
## 01.12.2025

# This supporting code has two functions to plot predictor and response values
# both over time and correlation (separated out by site-reach)

#### All functions ####

# plots for time series
# five panel with each site reach separated out
time_plot <- function(data, predictor, response, title) {
  ggplot(data = data, aes(x = field_date)) +
    geom_point(y = predictor, color = "gray") +
    geom_point(y = response, color = "purple") +
    labs(title = title) +
    facet_wrap(~site_reach) +
    theme_bw()
}

# plots for correlation
corr_plot <- function(data, predictor, response, title) {
  
  # plot with all site reaches
  all <- ggplot(data = data, aes(x = predictor, y = response, color = site_reach, 
                                 shape = site_reach)) +
    geom_point() +
    labs(title = title) +
    theme_bw()
  print(all)
  
  # plots with site reaches separated out
  separate <- ggplot(data = data, aes(x = predictor, y = response, color = site_reach, 
                                      shape = site_reach)) +
    geom_point() +
    labs(title = title) +
    theme_bw() +
    facet_wrap(~site_reach)
  print(separate)
}
