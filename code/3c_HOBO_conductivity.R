#### cleaning and assembling HOBO U-24 sensor conductivity & temperature data
### Jordan Zabrecky
## DATE

# This code reads in csv's of conductivity data from HOBO-U24 sensors saved 
# from the HOBOware software and removes any outliers or periods where 
# the sensor was pulled out of water and saves it to a new csv

#### (1) Loading libraries and HOBO data ####

## steal from old scripts on desktop
## maybe use dyplots??




### OLD CODE-- clearly added this in Excel manually lol

# installing dygraphs to remove specific outliers
install.packages("dygraphs")

# libraries
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(dygraphs)
library(xts)

# load the data
HOBOsal <- read.csv("HOBO_Salmon_cleaned.csv")
HOBOrus <- read.csv("HOBO_Russian_cleaned.csv")
HOBOeel <- read.csv("HOBO_SfkEel_cleaned.csv")

HOBOroving <- read.csv("HOBO_roving_cleaned.csv")

# converting to POSIXT
makePosixct <- function(data) {
  posixct <- as.POSIXct(c(data$date_time), format="%m/%d/%Y %H:%M", tz = "UTC")
  data <- mutate(data, date_time = posixct)
}

HOBOsal <- makePosixct(HOBOsal)
HOBOrus <- makePosixct(HOBOrus)
HOBOeel <- makePosixct(HOBOeel)
HOBOroving <- makePosixct(HOBOroving)

#### Cleaning High Values ####

# Filtering out obvious high outliers that can easily be removed
# Then subsquently writing csv with cleaned values for conductivity (WHICH I WILL REMOVE)
HOBOsal <- filter(HOBOsal, low_range_con < 180)
write_csv(HOBOsal, "H://2022-2023 CA Anatoxins/HOBO_Salmon_cleaned_conductivity.csv")

HOBOeel <- filter(HOBOeel, low_range_con <500)
HOBOeel <- filter(HOBOeel, low_range_con >240)
write_csv(HOBOeel, "H://2022-2023 CA Anatoxins/HOBO_SfkEel_cleaned_conductivity.csv")

# Using dygraph to remove "harder-to-get" outliers; adjusting time constraints as necessary
vis_data <- function(x){
  
  x <- subset(x, x$date_time < "2022-09-23 00:00:00" & x$date_time > "2022-09-20 00:00:00")
  
  # Then you can create the xts format, and thus use dygraph
  dat <- xts(x = x$low_range_con, order.by = x$date_time)
  
  # Make the chart
  p <- dygraph(dat)
  p
  
}

# Removing harder-to-get outliers for Salmon River
vis_data(sal_adj)
sal_adj <- HOBOsal[-which(HOBOsal$date_time >= "2022-07-04 14:45:00" & HOBOsal$date_time <= "2022-07-04 14:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-07-07 04:15:00" & sal_adj$date_time <= "2022-07-07 04:15:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-07-08 01:30:00" & sal_adj$date_time <= "2022-07-08 01:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-07-08 10:00:00" & sal_adj$date_time <= "2022-07-08 10:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-07-10 02:45:00" & sal_adj$date_time <= "2022-07-10 02:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-07-10 09:30:00" & sal_adj$date_time <= "2022-07-10 09:30:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-07-16 02:15:00" & sal_adj$date_time <= "2022-07-16 03:30:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-08-04 02:45:00" & sal_adj$date_time <= "2022-08-04 03:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-08-11 09:45:00" & sal_adj$date_time <= "2022-08-11 12:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-08-12 08:15:00" & sal_adj$date_time <= "2022-08-12 10:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-08-13 02:00:00" & sal_adj$date_time <= "2022-08-13 03:30:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-08-14 02:15:00" & sal_adj$date_time <= "2022-08-14 03:30:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-08-15 07:00:00" & sal_adj$date_time <= "2022-08-15 09:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-08-20 09:15:00" & sal_adj$date_time <= "2022-08-20 12:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time == "2022-08-30 17:00:00" & sal_adj$date_time <= "2022-08-30 17:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-04 11:00:00" & sal_adj$date_time <= "2022-09-04 11:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-13 00:15:00" & sal_adj$date_time <= "2022-09-13 05:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-13 07:45:00" & sal_adj$date_time <= "2022-09-13 11:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-13 22:15:00" & sal_adj$date_time <= "2022-09-14 00:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-14 05:45:00" & sal_adj$date_time <= "2022-09-14 08:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-17 12:30:00" & sal_adj$date_time <= "2022-09-17 13:45:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-18 16:30:00" & sal_adj$date_time <= "2022-09-18 18:00:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-19 04:45:00" & sal_adj$date_time <= "2022-09-19 09:30:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-19 21:30:00" & sal_adj$date_time <= "2022-09-19 22:30:00"),]
sal_adj <- sal_adj[-which(sal_adj$date_time >= "2022-09-21 06:30:00" & sal_adj$date_time <= "2022-09-21 08:30:00"),]

# Removing harder-to-get outliers for South Fork Eel River
vis_data(eel_adj)
eel_adj <- HOBOeel[-which(HOBOeel$date_time >= "2022-08-28 07:45:00" & HOBOeel$date_time <= "2022-08-28 08:45:00"),]
eel_adj <- eel_adj[-which(eel_adj$date_time >= "2022-09-01 14:00:00" & eel_adj$date_time <= "2022-09-01 15:45:00"),]
eel_adj <- eel_adj[-which(eel_adj$date_time >= "2022-09-02 01:30:00" & eel_adj$date_time <= "2022-09-02 02:45:00"),]
eel_adj <- eel_adj[-which(eel_adj$date_time >= "2022-09-12 15:15:00" & eel_adj$date_time <= "2022-09-12 15:45:00"),]
eel_adj <- eel_adj[-which(eel_adj$date_time >= "2022-09-16 17:15:00" & eel_adj$date_time <= "2022-09-16 18:45:00"),]

# Removing harder-to-get outliers for Russian River
vis_data(rus_adj)
rus_adj <- HOBOrus[-which(HOBOrus$date_time >= "2022-06-27 07:15:00" & HOBOrus$date_time <= "2022-06-27 12:15:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-07-05 10:45:00" & rus_adj$date_time <= "2022-07-06 03:30:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-07-16 22:00:00" & rus_adj$date_time <= "2022-07-16 22:00:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-07-23 03:15:00" & rus_adj$date_time <= "2022-07-23 06:00:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-03 05:30:00" & rus_adj$date_time <= "2022-08-03 06:00:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-11 07:15:00" & rus_adj$date_time <= "2022-08-11 08:45:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-15 09:45:00" & rus_adj$date_time <= "2022-08-15 12:00:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-25 00:15:00" & rus_adj$date_time <= "2022-08-25 03:30:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-25 07:15:00" & rus_adj$date_time <= "2022-08-25 08:45:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-26 11:00:00" & rus_adj$date_time <= "2022-08-26 13:15:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-29 08:45:00" & rus_adj$date_time <= "2022-08-29 08:45:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-29 10:45:00" & rus_adj$date_time <= "2022-08-29 13:45:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-08-31 22:15:00" & rus_adj$date_time <= "2022-08-31 23:15:00"),]
rus_adj <- rus_adj[-which(rus_adj$date_time >= "2022-09-01 02:45:00" & rus_adj$date_time <= "2022-09-01 02:45:00"),]

# Creating new CSVs with cleaned data


summary(HOBOeel)
summary(HOBOrus)
summary(HOBOsal)

#### Making salinity plot ####

salfig <- ggplot(sal_adj, aes(x = date_time)) +
  geom_line(aes(y = low_range_con),  color = "lightsalmon3") +
  labs(title = "Salmon River", y = "μS/cm", x = NULL) +
  scale_x_datetime(limits = as.POSIXct(c("2022-06-24 17:00:00 UTC", "2022-09-21 17:45:00 UTC"))) +
  coord_cartesian(ylim = c(90, 180)) +
  theme_bw()
salfig

eelfig <- ggplot(eel_adj, aes(x = date_time)) +
  geom_line(aes(y = low_range_con),  color = "seagreen3") +
  labs(title = "South Fork Eel River", y = "μS/cm", x = NULL) +
  scale_x_datetime(limits = as.POSIXct(c("2022-06-24 17:00:00 UTC", "2022-09-21 17:45:00 UTC"))) +
  coord_cartesian(ylim = c(245, 285)) +
  theme_bw()
eelfig

rusfig <- ggplot(rus_adj, aes(x = date_time)) +
  geom_line(aes(y = low_range_con),  color = "darkgoldenrod2") +
  labs(title = "Russian River", y = "μS/cm", x = NULL) +
  scale_x_datetime(limits = as.POSIXct(c("2022-06-24 17:00:00 UTC", "2022-09-21 17:45:00 UTC"))) +
  coord_cartesian(ylim = c(245, 325)) +
  theme_bw()
rusfig

combined <- grid.arrange(eelfig, salfig, rusfig, nrow = 3, top =textGrob("Clean Data", gp=gpar(fontsize=15,font=1)))

#### Making salinity plot with old outliers to compare to newly created one

salfig3 <- ggplot(HOBOsal, aes(x = date_time)) +
  geom_line(aes(y = low_range_con),  color = "lightsalmon3") +
  labs(title = "Salmon River", y = NULL, x = NULL) +
  scale_x_datetime(limits = as.POSIXct(c("2022-06-24 17:00:00 UTC", "2022-09-21 17:45:00 UTC"))) +
  coord_cartesian(ylim = c(90, 180)) +
  theme_bw()
salfig3

eelfig3 <- ggplot(HOBOeel, aes(x = date_time)) +
  geom_line(aes(y = low_range_con),  color = "seagreen3") +
  labs(title = "South Fork Eel River", y = NULL, x = NULL) +
  scale_x_datetime(limits = as.POSIXct(c("2022-06-24 17:00:00 UTC", "2022-09-21 17:45:00 UTC"))) +
  coord_cartesian(ylim = c(245, 285)) +
  theme_bw()
eelfig3

rusfig3 <- ggplot(HOBOrus, aes(x = date_time)) +
  geom_line(aes(y = low_range_con),  color = "darkgoldenrod2") +
  labs(title = "Russian River", y = NULL, x = NULL) +
  scale_x_datetime(limits = as.POSIXct(c("2022-06-24 17:00:00 UTC", "2022-09-21 17:45:00 UTC"))) +
  coord_cartesian(ylim = c(245, 325)) +
  theme_bw()
rusfig3

combined3 <- grid.arrange(eelfig3, salfig3, rusfig3, nrow = 3, top =textGrob("Raw Data", gp=gpar(fontsize=15,font=1)))

# Make graphic with 2 columns and 3 rows for comparison
clean_vs <- grid.arrange(combined, combined3, ncol = 2)

#### Looking at roving sensors and adding those in ####

HOBOroving_r <- filter(HOBOroving, site == "RUS")
HOBOroving_e <- filter(HOBOroving, site == "EEL")
HOBOroving_s <- filter(HOBOroving, site == "SAL")

salfig2 <- salfig + geom_point(data = HOBOroving_s, aes(x = date_time, y = low_range_con), alpha = 0.4, color = "red")
salfig2

eelfig2 <- eelfig + geom_point(data = HOBOroving_e, aes(x = date_time, y = low_range_con), alpha = 0.4, color = "green")
eelfig2

rusfig2 <- rusfig + geom_point(data = HOBOroving_r, aes(x = date_time, y = low_range_con), alpha = 0.4, color = "brown")
rusfig2

combined2 <- grid.arrange(eelfig2, salfig2, rusfig2, ncol = 3)