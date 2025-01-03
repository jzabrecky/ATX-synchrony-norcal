#### processing chlorophyll-a and pheophytin concentrations for freeze-dried cyanobacteria mass
### Jordan Zabrecky (modified from Joanna Blaszczak)
## last edited 01.02.24

# This code calculates chlorophyll-a and pheophytin concentrations for freeze-dried
# cyanobacteria mass from RFUs obtained from the Blaszczak Lab's Trilogy Fluorometer 
# using the 2024-2025 calibration

# link to calculation power point: https://docs.google.com/presentation/d/1qoOF3yhsAzrzcR9l_J0bSWqni8gjoPfq/edit#slide=id.p3

#### (1) Loading libraries & data ####

# import packages
lapply(c("plyr","dplyr", "tidyverse"), require, character.only=T)

# set working directory to lead in data
setwd("./data/field_and_lab/raw_data/cyano_chla")

# RFU data
rawRFU <- ldply(list.files(pattern = "_RFUdata.csv"), function(filename) {
  d <- read.csv(filename)
  d$file <- filename
  d$analysis_date <- sapply(strsplit(d$file,"_"), `[`, 1)
  return(d)
})

# metadata
metadata <- ldply(list.files(pattern = "_metadata.csv"), function(filename) {
  d <- read.csv(filename)
  d$file <- filename
  d$analysis_date <- sapply(strsplit(d$file,"_"), `[`, 1)
  return(d)
})

# convert extraction volume from mL to L as Trilogy equation output is in chla ug/L
rawRFU <- rawRFU %>% 
  mutate(Extraction_vol_L = Extraction_vol / 1000) %>% # mL to L
  select(!Extraction_vol) %>% 
  dplyr::rename(Extraction_vol = Extraction_vol_L) # giving this column the old name to keep code the same

# check data frame formatting
sapply(rawRFU, class)
sapply(metadata, class)

# split into list of dataframes
rawRFU_list <- split(rawRFU, rawRFU$analysis_date)
metadata_list <- split(metadata, metadata$analysis_date)

# merge all by analysis date and Rack_ID
merged_list <- list()
for(i in 1:length(names(rawRFU_list))){
  merged_list[[i]] <- left_join(rawRFU_list[[i]][,c("Fluorometer_ID","RFU","Units","Unacidified_acidified","Rack_ID","Extraction_vol")],
                                metadata_list[[i]][,c("Rack_ID", "site_reach", "field_date", "type", "triplicate", "mass_mg", "dilution")],
                                by = "Rack_ID", multiple = "all")
}

# add analysis date as name for each dataframe in merged list
names(merged_list) <- names(rawRFU_list)

# check an individual dataframe from analysis date
View(merged_list$'20240817')

#### (2) Define the standard curves for low and high ####

# RFU cutoff between low and high standard curve = 797672.88
# calibration curve for 2024-2025: https://docs.google.com/spreadsheets/d/1qO1-9TI8dwxcrY6I3o4K0haUapuAcIKJRSivkrpgS8o/edit#gid=400761777
low_Fm <- 1.71; high_Fm <- 1.75
low_Fa <- 42865.05; high_Fa <- 215013.47
low_Fb <- 71173.95; high_Fb <- 372314.34
low_std_C1 <- 50; high_std_C1 <- 250
low_blank <- 3235.6; high_blank <- 6604.78

# ** previous investigations show that controlling for differences in cuvette
# blanks at the beginning of each run have <0.6 % change for samples with the 
# biggest differences in cuvette blanks so we will not be doing the work to make 
# those very minor adjustments :) **

#### (3) Calculating chlorophyll-a and pheophytin concentrations ####

# function to calculate chlorophyll-a and pheophytin
chla_pheo_calc <- function(ex){
  
  # define volume of the solvent and mass of sample
  vol_solvent <- ex$Extraction_vol[1]
  mass_mg <- ex$mass_mg[1]
  
  # next, determine which curve type to use
  ex$curve_type <- ifelse(ex$RFU <= 797672.88,
                          yes = "low", no = "high")
  
  # determine if acidified or unacidified RFU below detection limit 
  # (lowest standard from lowest cal curve)
  ex$RFU_bdl_sep <- (ex$RFU < 71173.95)
  # for reference, highest standard is 8262338.50 RFU so over detection
  # limit shouldn't be an issue
  
  # define parameters for low and high curves
  Int_B_low <- low_std_C1*((ex[which(ex$Unacidified_acidified == "U"),]$RFU - low_blank)/(low_Fb - low_blank))
  Int_A_low <- low_std_C1*((ex[which(ex$Unacidified_acidified == "A"),]$RFU - low_blank)/(low_Fb - low_blank))
  Int_B_high <- high_std_C1*((ex[which(ex$Unacidified_acidified == "U"),]$RFU - high_blank)/(high_Fb - high_blank))
  Int_A_high <- high_std_C1*((ex[which(ex$Unacidified_acidified == "A"),]$RFU - high_blank)/(high_Fb - high_blank))
  
  # apply interpolation based on whether its a high or low curve
  ex$Chla_ug_mg_raw <- ifelse(ex$curve_type == "low",
                              yes = (low_Fm/(low_Fm - 1))*(Int_B_low - Int_A_low)*(vol_solvent/mass_mg), # (L/ug ratio, get answer in chl-a/ug sample)
                              no = (high_Fm/(high_Fm - 1))*(Int_B_high - Int_A_high)*(vol_solvent/mass_mg))
  
  ex$Pheo_ug_mg_raw <- ifelse(ex$curve_type == "low",
                              yes = (low_Fm/(low_Fm - 1))*((low_Fm*Int_A_low) - Int_B_low)*(vol_solvent/mass_mg),
                              no = (high_Fm/(high_Fm - 1))*((high_Fm*Int_A_high) - Int_B_high)*(vol_solvent/mass_mg))
  
  # account for dilution factor
  ex$Chla_ug_mg <- ex$Chla_ug_mg_raw / ex$dilution
  ex$Pheo_ug_mg <- ex$Pheo_ug_mg_raw / ex$dilution
  
  # return true if either acidified or unacidified RFU is below detection limit
  ex$RFU_bdl <- ifelse(ex$RFU_bdl_sep[1] == TRUE | ex$RFU_bdl_sep[2] == TRUE, TRUE, FALSE)
  
  # preserve dilution information to potentially adjust for future chlorophyll-a runs
  output <- as.data.frame(cbind(ex$Chla_ug_mg[1], ex$Pheo_ug_mg[1], ex$dilution[1], ex$RFU_bdl[1]))
  
  return(output)
}

# function to apply the above function across list of dataframes
chla_pheo_processing <- function(dat){
  # split data by Rack_ID
  df_rack_split <- split(dat, dat$Rack_ID)
  # apply chlorophyll-a and pheophytin calculation function
  chla_Pheo_df <- ldply(lapply(df_rack_split, function(x) chla_pheo_calc(x)), data.frame)
  # column names for output
  colnames(chla_Pheo_df) <- c("Rack_ID", "Chla_ug_mg","Pheo_ug_mg", "dilution", "RFU_bdl")
  
  return(chla_Pheo_df)
}

# applying function to list of merged data frames
processed_data <- ldply(lapply(merged_list, function(x) chla_pheo_processing(x)))

# adding in name of first column
colnames(processed_data)[1] <- "analysis_date"

# merge again with metadata
processed_data$Rack_ID <- as.character(processed_data$Rack_ID) # convert to character to match
metadata$Rack_ID <- as.character(metadata$Rack_ID)
chla_pheo_calculations <- left_join(na.omit(processed_data),
                                    metadata[,c("Rack_ID","site_reach","field_date", "type", "triplicate", "mass_mg", "analysis_date")],
                                    by = c("analysis_date","Rack_ID"))

#### (4) Final processing of chlorophyll-a and pheophytin calculations ####

# remove values below detection limit and with negative pheophytin values
# and get ready for final csv
chla_pheo_final <- chla_pheo_calculations %>% 
  filter(RFU_bdl == 0) %>% # RFU below detection limit
  filter(Pheo_ug_mg > 0) %>%  # negative pheophytin values
  mutate(field_date = mdy(field_date)) %>% # convert to yyyy-mm-dd
  dplyr::rename(sample_type = type) %>% 
  select(field_date, site_reach, sample_type, triplicate, Chla_ug_mg, Pheo_ug_mg)

# we have a couple of samples that got accidentally got ran twice 
# so will just take the first one
which(chla_pheo_final$sample_type == "TM" & 
        chla_pheo_final$field_date == mdy("9/12/2023") &
        chla_pheo_final$site_reach == "SFE-M-1S")
chla_pheo_final <- chla_pheo_final[-202,]
which(chla_pheo_final$sample_type == "TM" & 
        chla_pheo_final$field_date == mdy("9/12/2023") &
        chla_pheo_final$site_reach == "SFE-SH-1S")
chla_pheo_final <- chla_pheo_final[-222,]

# save final csv
write.csv(chla_pheo_final, "../../../EDI_data_package/target_sample_chlorophyll.csv", row.names = FALSE)
