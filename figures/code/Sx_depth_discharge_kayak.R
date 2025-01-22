#### Kayak Measured depths on Log Discharge-Log Depth plots
### Jordan Zabrecky
## last edited: 01.21.2025

# This figure shows our depth-discharge relationships for each river
# and plots our kayak measurements over it

#### (1) Loading libraries and data ####

# use work from script that builds depth discharge relationships
source("code/1g_processing_metabolism_outputs.R")

# adding site_name column for discharge
site_names <- c("russian", "salmon", "sfkeel_mir", "sfkeel_sth")
for(i in 1:length(USGS_daily_discharge)) {
  USGS_daily_discharge[[i]]$site <- site_names[i]
}

# combining discharge to one data frame to use facet wrap in ggplot
discharge_all <- rbind(USGS_daily_discharge$russian, USGS_daily_discharge$salmon,
                       USGS_daily_discharge$sfkeel_mir, USGS_daily_discharge$sfkeel_sth)

#### (2) 