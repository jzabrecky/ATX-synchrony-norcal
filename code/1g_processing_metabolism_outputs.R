#### adding depth to metabolism outputs and saving an abbreviated csv 
# that is not ignored by gitignore

#### OLD DEPTH CODE BELOW :)

#### (5) Incorporating depth-discharge relationship ####

# reading in data (what we have now...)
kayak_sfkeel <- read.csv("../depth_measurements/sfkeel_kayak_measurements.csv")

# converting date as string to date object
kayak_sfkeel$Date <- mdy(kayak_sfkeel$Date)

# function to plot and visualize depth-discharge relationship
Q_depth_plot <- function(x, y , model) {
  ggplot() +
    geom_point(aes(x = x, y = y), size = 3, color = "darkblue") + 
    geom_abline(slope = model$coefficients[[2]], intercept = model$coefficients[[1]], 
                linewidth =1.5, color="skyblue", linetype = "dotted") +
    xlab("Discharge (cms)")+
    ylab("Depth (m)")+
    theme_bw()
}

# function to clean discharge downloads
edit_Q_depth_df <- function(data) {
  new <- data %>% 
    mutate(discharge_m3_s = X_00060_00003 / 35.31) %>% 
    select(Date, depth_m, discharge_m3_s)
  return(new)
}

## russian 

# using past relationship from transect of discharge measurement for now...
discharge$russian$depth_m <- (0.08601 * discharge$russian$discharge_m3_s) + 0.31433

## salmon

# using past relationship modeled with USGS channel cross-section data
discharge$salmon$depth_m <- exp((0.32207 * log(discharge$salmon$discharge_m3_s)) - 1.03866)

## south fork eel @ miranda

# calculating average depth per kayak run
depth_Q_sfkeel_mir <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Miranda", Meas_Type == "Depth") %>%
  filter(Transect != 18) %>% # removed transects 18 as first date was half depth of later two dates
  filter(Transect != 5 & Transect != 12) %>%  # removed transects 5 & 12 as first date was ~0.3 m lower than second date
  # this may be either because our GPS was slightly off or differences when taking depths across transects between two different kayakers
  group_by(Date) %>% 
  summarize(depth_m = mean(Depth_cm_final) / 100)

# getting daily discharge data and adding it to depth-discharge data frame
depth_Q_sfkeel_mir <- left_join(depth_Q_sfkeel_mir, 
                                readNWISdv("11476500", param, depth_Q_sfkeel_mir$Date[1], depth_Q_sfkeel_mir$Date[3]))
depth_Q_sfkeel_mir <- edit_Q_depth_df(depth_Q_sfkeel_mir)

# model relationship between depth and discharge 
# log(depth) ~ log(discharge) shows most linear relationship
# likely underestimates all but summer depths, but we are not modelling metabolism then
# frequentist linear regression model
sfkeel_mir_lm <- lm(log(depth_m) ~ log(discharge_m3_s), data = depth_Q_sfkeel_mir)
# having issues with brm divergent transitions so will stick with above

# plot relationship
Q_depth_plot(log(depth_Q_sfkeel_mir$discharge_m3_s), log(depth_Q_sfkeel_mir$depth_m), sfkeel_mir_lm)

# use model to fill in depth on discharge plot
discharge$sfkeel_mir$depth_m <- exp(sfkeel_mir_lm$coefficients[[1]] + (log(discharge$sfkeel_mir[[2]]) * sfkeel_mir_lm$coefficients[[2]]))

## south fork eel @ standish hickey

# calculating average depth per kayak run
depth_Q_sfkeel_sth <- kayak_sfkeel %>% 
  filter(Site == "SfkEel_Standish", Meas_Type == "Depth") %>% 
  group_by(Date) %>%  # not seeing any weird transect issues here!
  summarize(depth_m = mean(Depth_cm_final) / 100)
# also makes sense that this site is as deep despite lower discharge-- more pools

# getting daily discharge data and adding it to depth-discharge data frame
depth_Q_sfkeel_sth <- left_join(depth_Q_sfkeel_sth, 
                                readNWISdv("11475800", param, depth_Q_sfkeel_sth$Date[1], depth_Q_sfkeel_sth$Date[3]))
depth_Q_sfkeel_sth <- edit_Q_depth_df(depth_Q_sfkeel_sth)

# model relationship between depth and discharge
# log(depth) ~ discharge shows most linear relationship
# though not as good of a fit overall as miranda
# however, due to Leopold and Maddock (1953) hydraulic theory, we'll stick with log-log
sfkeel_sth_lm <- lm(log(depth_m) ~ log(discharge_m3_s), data = depth_Q_sfkeel_sth)

# plot relationship
Q_depth_plot(log(depth_Q_sfkeel_sth$discharge_m3_s), log(depth_Q_sfkeel_sth$depth_m), sfkeel_sth_lm)

# use model to fill in depth on discharge plot NEED TO FIX THIS
discharge$sfkeel_sth$depth_m <- exp(sfkeel_sth_lm$coefficients[[1]] + (log(discharge$sfkeel_sth[[2]]) * sfkeel_sth_lm$coefficients[[2]]))
