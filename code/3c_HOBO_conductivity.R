#### cleaning and assembling HOBO U-24 sensor conductivity & temperature data
### Jordan Zabrecky
## 10.02.2024

# This code reads in csv's of conductivity data from HOBO-U24 sensors saved 
# from the HOBOware software and removes any outliers or periods where 
# the sensor was pulled out of water and saves it to a new csv

#### (1) Loading libraries and HOBO data ####

# load libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

## load HOBO data

# function to read in HOBO csv data
read_HOBO_csvs <- function(path) {
  # list out subfolders in folder
  files <- list.files(path)
  
  # initialize empty dataframe
  final <- data.frame()
  
  # iterate through each subfolder and add cvs's to dataframe
  for(i in 1:length(files)) {
    temp <- ldply(list.files(paste(path, files[i], sep = ""), pattern = ".csv"), function(filename) {
              d <- read.csv(paste(path, files[i], "/", filename, sep = ""), header = FALSE) 
              d <- d[-1,] # remove first row
              d <- d[-(which(d[,1] == "#")),] # remove rows with column headers
              d <- d[,-1] # remove first column (which is just row names)
              d <- d[,1:4] # keep only columns with data
              d <- d[-(which(d[,2] == "")),] # remove rows that are "" or empty
              colnames(d) <- c("date_time", "low_range_cond_uS_cm", "full_range_cond_uS_cm", "temp_C")
              d$site <- files[i] %>% stringr::str_sub(start = 10, end = nchar(files[i])) # add name of site
              return(d)
            })
    final <- rbind(final, temp)
  }
  return(final)
}

# reading in HOBO data by year
HOBO_2022 <- read_HOBO_csvs("./data/HOBO/2022_raw_data/")
HOBO_2023 <- read_HOBO_csvs("./data/HOBO/2023_raw_data/")

# our time in the original csv was GMT -7, so our timezone is PST as we want
HOBO_2022$date_time <- mdy_hms(HOBO_2022$date_time, tz = "America/Los_Angeles")
HOBO_2023$date_time <- mdy_hms(HOBO_2023$date_time, tz = "America/Los_Angeles")

# lastly, create a column with site_year information
HOBO_2022 <- HOBO_2022 %>% 
  mutate(site_year = case_when(site == "Miranda" ~ "sfkeel_mir_2022",
                               site == "Russian" ~ "russian_2022",
                               site == "Salmon" ~ "salmon_2022"))
HOBO_2023 <- HOBO_2023 %>% 
  mutate(site_year = case_when(site == "Miranda" ~ "sfkeel_mir_2023",
                               site == "Standish" ~ "sfkeel_sth_2023",
                               site == "Salmon" ~ "salmon_2023"))

# convert temperature and conductivity classes to numeric
HOBO_2022[,2:4] <- sapply(HOBO_2022[,2:4], as.numeric)
HOBO_2023[,2:4] <- sapply(HOBO_2023[,2:4], as.numeric)

# splitting data into lists for cleaning
HOBO_2022_list <- split(HOBO_2022, HOBO_2022$site_year)
HOBO_2023_list <- split(HOBO_2023, HOBO_2023$site_year)

#### (2) Remove maintenance periods and outliers ####

# using dygraphs package in script "1d_visualizing_HOBO_data_cleaning.R"
# while performing these steps to identify outliers and confirm maintenance times

## (a) Removing maintenance periods

# russian 2022
russian_2022_cleaning <- HOBO_2022_list$russian_2022 %>% 
  filter(date_time >= "2022-06-24 17:15:00")  # initial launch time
# no retrieval time because sensor was stolen :(
# all maintenance times removed below (and some weirdness right before pick-up)
russian_2022_cleaning <- russian_2022_cleaning %>% 
  filter(date_time <= "2022-07-06 09:30:00" | date_time >= "2022-07-06 10:30:00") %>% 
  filter(date_time <= "2022-07-20 06:45:00" | date_time >= "2022-07-20 09:55:00") %>% 
  filter(date_time <= "2022-08-02 08:15:00" | date_time >= "2022-08-02 10:10:00") %>% 
  # readjusted brick so maintenance time longer than normal here
  filter(date_time <= "2022-08-17 07:50:00" | date_time >= "2022-08-17 10:15:00") %>% 
  # last maintenance time with no end date because sensor was stolen :(
  filter(date_time <= "2022-09-01 7:40:00")

# south fork eel @ miranda 2022
sfkeel_mir_2022_cleaning <- HOBO_2022_list$sfkeel_mir_2022 %>% 
  filter(date_time >= "2022-06-29 10:40:00") %>%  # initial launch time
  filter(date_time <= "2022-09-17 09:03:00") # retrieval time
# all maintenance times removed below
sfkeel_mir_2022_cleaning <- sfkeel_mir_2022_cleaning %>%
  filter(date_time <= "2022-07-14 09:10:00" | date_time >= "2022-07-14 09:20:00") %>% 
  filter(date_time <= "2022-07-28 09:20:00" | date_time >= "2022-07-28 10:10:00") %>% 
  filter(date_time <= "2022-08-10 08:55:00" | date_time >= "2022-08-10 09:25:00") %>% 
  filter(date_time <= "2022-08-23 09:30:00" | date_time >= "2022-08-23 11:25:00") %>% 
  filter(date_time <= "2022-09-06 09:15:00" | date_time >= "2022-09-06 10:25:00")

## (b) removing outliers

# using new object name to preserve temperature data (that doesn't have outliers)

# russian 2022
russian_2022_cleaning_cond <- russian_2022_cleaning %>% 
  filter(date_time <= "2022-06-27 14:10:00" | date_time >= "2022-06-27 17:50:00") %>% # <4 hours
  filter(date_time <= "2022-07-06 05:50:00" | date_time >= "2022-07-06 06:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 06:50:00" | date_time >= "2022-07-06 07:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 07:40:00" | date_time >= "2022-07-06 08:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-06 08:50:00" | date_time >= "2022-07-06 09:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-13 16:40:00" | date_time >= "2022-07-13 17:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-17 04:50:00" | date_time >= "2022-07-17 05:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-23 10:20:00" | date_time >= "2022-07-23 10:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-23 11:20:00" | date_time >= "2022-07-23 11:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-29 08:20:00" | date_time >= "2022-07-29 08:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-29 08:50:00" | date_time >= "2022-07-29 09:10:00") %>% # <1 hour
  filter(date_time <= "2022-08-03 12:40:00" | date_time >= "2022-08-03 12:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-04 10:10:00" | date_time >= "2022-08-04 10:20:00") %>% # <1 hour
  filter(date_time <= "2022-08-11 14:20:00" | date_time >= "2022-08-11 15:20:00") %>% # <1 hour
  filter(date_time <= "2022-08-13 12:10:00" | date_time >= "2022-08-13 12:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-25 07:40:00" | date_time >= "2022-08-25 09:10:00") %>% # <2 hours
  filter(date_time <= "2022-08-25 09:20:00" | date_time >= "2022-08-25 09:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-25 14:20:00" | date_time >= "2022-08-25 14:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-26 18:15:00" | date_time >= "2022-08-26 19:20:00") %>% # <2 hours
  filter(date_time <= "2022-08-29 15:40:00" | date_time >= "2022-08-29 15:50:00") %>% # <1 hour
  filter(date_time <= "2022-08-29 18:20:00" | date_time >= "2022-08-29 18:40:00") %>% # <1 hour
  filter(date_time <= "2022-08-29 19:40:00" | date_time >= "2022-08-29 19:50:00") # <1 hour

# south fork eel @ miranda 2022
sfkeel_mir_2022_cleaning_cond <- sfkeel_mir_2022_cleaning %>%
  filter(date_time <= "2022-07-02 05:20:00" | date_time >= "2022-07-02 05:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-03 15:40:00" | date_time >= "2022-07-03 16:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-04 09:40:00" | date_time >= "2022-07-04 09:50:00") %>% # <1 hour
  filter(date_time <= "2022-07-04 17:10:00" | date_time >= "2022-07-04 18:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-04 18:50:00" | date_time >= "2022-07-04 19:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-04 20:10:00" | date_time >= "2022-07-04 21:10:00") %>% # <2 hours
  filter(date_time <= "2022-07-05 03:20:00" | date_time >= "2022-07-05 03:40:00") %>% # <1 hour
  filter(date_time <= "2022-07-05 08:10:00" | date_time >= "2022-07-05 08:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-09 08:50:00" | date_time >= "2022-07-09 09:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-10 20:40:00" | date_time >= "2022-07-10 21:20:00") %>% # <1 hour
  filter(date_time <= "2022-07-16 22:50:00" | date_time >= "2022-07-17 12:50:00") %>% # <4 hours
  filter(date_time <= "2022-07-17 18:20:00" | date_time >= "2022-07-17 19:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-19 07:10:00" | date_time >= "2022-07-19 08:10:00") %>% # <2 hours
  filter(date_time <= "2022-07-19 17:10:00" | date_time >= "2022-07-19 18:50:00") %>% # <2 hours
  filter(date_time <= "2022-07-21 15:50:00" | date_time >= "2022-07-21 16:10:00") %>% # <1 hour
  filter(date_time <= "2022-07-22 05:20:00" | date_time >= "2022-07-22 09:40:00") %>% # <5 hours
  filter(date_time <= "2022-08-26 02:10:00" | date_time >= "2022-08-26 02:20:00") %>% # <1 hour
  filter(date_time <= "2022-09-15 15:40:00" | date_time >= "2022-09-15 15:50:00") # <1 hour
# linear interpolation??? MAYBE?

#### (3) recombining and saving dataframe?


# reading in list
HOBO_2022_list <- lapply(file_list, function(x) read_HOBO_csv(paste(path, x, sep = "")))

HOBO_2022 <- reduce(intersect, HOBO_2022_list)

# 2023


miniDOT_data <- ldply(list.files(path = "./data/miniDOT/", pattern = "_miniDOT.csv"), function(filename) {
  d <- read.csv(paste("data/miniDOT/", filename, sep = ""))
  d$site_year = filename %>% stringr::str_remove("_miniDOT.csv")
  d$site = d$site_year %>% str_sub(end=-6)
  return(d)
})

create_df <- function(source_path) {
  list.files(path = here(source_path),
             pattern = ".csv",
             full.names = T) %>% 
    map_df(~read_csv)
}


create_df("./data/HOBO/2022_raw_data/20775521_Miranda/")

## 03-2023 (post 2022 field season & pre 2023 field season)
path <- "./data/miniDOT/intercalibrations/202303/"
file_list <- list.files(path)
HOBO_data_22 <- ldply(list.files(path = "./data/HOBO/2022_raw_data/"), function(filename) {
  d <- read.csv(paste("./data/HOBO/2022_raw_data", filename, sep = ""))
  d$site = d$site_year %>% str_sub(end=-6)
  return(d)
})

create_df <- function(source_path) {
  list.files(path = here(source_path),
             pattern = "*.txt",
             full.names = T) %>% 
    map_df(~read_csv(., skip = 3, col_names = header_list))
}

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