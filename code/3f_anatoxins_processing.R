#### processing anatoxin concentrations from SUNY-ESF
### Jordan Zabrecky
## last edited 9.13.24

# This code processes the CSV provided by SUNY-ESF and matches the sample IDs to
# our sample metadata. Then, anatoxins (total and individual congener groups) are
# recalculated as anatoxins per approximate organic matter per sample and 
# anatoxins per approximate chlorophyll-a per samples 
# (i.e. via two different biomass proxies)

#### (1) Loading libraries & data ####

# import packages
lapply(c("plyr","dplyr", "tidyverse"), require, character.only=T)

# reading in csvs from various places
anatoxins_full <- read.csv("./data/field_and_lab/raw_data/SUNY_ESF_LCMS_MS.csv")
metadata <- read.csv("./data/field_and_lab/raw_data/SUNY_ESF_metadata.csv")
per_OM <- read.csv("./data/field_and_lab/cyano_percent_OM.csv")
chla <- read.csv("./data/field_and_lab/cyano_chla.csv")

#### (2) Processing anatoxins csv ####

## (a) cleaning ESF csv
# lots of cleaning to be done with SUNY ESF data sheet...
view(anatoxins_full)

# columns 2, 26, 27, 31, 32, 33, 36, 38, 40, 42 are important
anatoxins_reduced <- anatoxins_full[c(2, 26, 27, 34, 37, 39, 41, 43)]
# (I already know all of our samples had none of the 
# three cylindrospermopsin derivatives tested, so not including them here)

# rename these columns to be more computer-legible
labels <- c("ESF_ID", "MCY_ug_g", "MCY_det_limit_full", "ATX_det_limit_full", 
            "ATXa_ug_g", "HTXa_ug_g", "dhATXa_ug_g", "dhHTXa_ug_g")

# adding labels as column names
colnames(anatoxins_reduced) <- labels

# convert columns that are character class to numeric 
# (this will replace the "-" with NA)
anatoxins_reduced[5:8] <- sapply(anatoxins_reduced[5:8], as.numeric)

# parse detection limit column and convert to numeric
anatoxins_reduced <- anatoxins_reduced %>%
  mutate(MCY_det_limit = as.numeric(str_sub(anatoxins_reduced$MCY_det_limit_full,3, 6)),
         ATX_det_limit = as.numeric(str_sub(anatoxins_reduced$ATX_det_limit_full,3, 8))) %>% 
  select(-MCY_det_limit_full, -ATX_det_limit_full)

## (b) processing data values
# values below detection limit were included in our report, but we will not be
# reporting those values, so let's replace them with 0
anatoxins_processed <- anatoxins_reduced %>% 
  mutate(
    MCY_ug_g = case_when(MCY_ug_g < MCY_det_limit ~ 0,
                          TRUE ~ MCY_ug_g),
    ATXa_ug_g = case_when(ATXa_ug_g < ATX_det_limit ~ 0,
                            TRUE ~ ATXa_ug_g),
    HTXa_ug_g = case_when(HTXa_ug_g < ATX_det_limit ~ 0,
                            TRUE ~ HTXa_ug_g),
    dhATXa_ug_g = case_when(dhATXa_ug_g < ATX_det_limit ~ 0,
                     TRUE ~ dhATXa_ug_g),
    dhHTXa_ug_g = case_when(dhHTXa_ug_g < ATX_det_limit ~ 0,
                            TRUE ~ dhHTXa_ug_g)
  )

# quick look- we have no dhHTX-a present in our samples
# of our reported HTX-a, only 4 were actually detectable
# of our reported ATX-a, 11 were NOT detectable
# of our reported dhATX-a, 23 were NOT detectable
# only 10 samples had detectable microcystin
view(anatoxins_processed)

# fill NA values with 0 
anatoxins_processed <- replace(anatoxins_processed, is.na(anatoxins_processed), 0)

# calculate total anatoxins
anatoxins_processed <- anatoxins_processed %>% 
  mutate(ATX_all_ug_g = ATXa_ug_g + HTXa_ug_g + dhATXa_ug_g + dhHTXa_ug_g)

# match ESF_ID with metadata
combined <- left_join(metadata, anatoxins_processed, by = "ESF_ID")

# check blanks to make sure they had no anatoxin detections
combined$ATX_all_ug_g[which(combined$sample_type == "BLANK")]

# blanks are zero- can remove them!
combined <- combined[-which(combined$sample_type == "BLANK"),]

# check gloetrichia samples and microcoleus from the Poudre
combined$ATX_all_ug_g[which(combined$sample_type == "Gloetrichia")]
combined$ATX_all_ug_g[which(combined$site_reach == "Poudre")]

# no anatoxins detected in those- can remove them as well
combined <- combined[-which(combined$sample_type == "Gloetrichia"),]
combined <- combined[-which(combined$site_reach == "Poudre"),]

# also- looked at our one sample extracted in formic acid-
# like the other TM from that day extracted in methanol it has no detectable ATX!

## (c) analyze triplicates
# filter out triplicates and calculate summary statistics
triplicates <- combined %>% 
  filter(triplicate == "y") %>% 
  dplyr::group_by(field_date, site_reach, sample_type) %>% 
  dplyr::summarize(mean_atxa = mean(ATXa_ug_g),
                   mean_htxa = mean(HTXa_ug_g),
                   mean_dhatxa = mean(dhATXa_ug_g),
                   mean_atx = mean(ATX_all_ug_g),
                   sd_atx = sd(ATX_all_ug_g), # only will calculate for sum
                   rsd_atx = sd_atx * 100 / mean_atx) %>% 
  ungroup() %>% 
  distinct()

# look at triplicate results
view(triplicates) # two highest triplicates were HIGH
# 173% RSD- SAL-1S TM and SFE-M-2 TAC; very high but sort of expected?
triplicates$rsd_atx <- replace(triplicates$rsd_atx, is.nan(triplicates$rsd_atx), NA)
mean(triplicates$rsd_atx, na.rm = TRUE) # average is 36.33%
median(triplicates$rsd_atx, na.rm = TRUE) # median is 20.46%

#### RESUME HERE~!!!!!
# take dataset and select for columns we care about before merging
triplicates_final <- triplicates %>% 
  dplyr::rename(Chla_ug_mg = mean_chla,
                Pheo_ug_mg = mean_pheo) %>% 
  select(field_date, site_reach, type, Chla_ug_mg, Pheo_ug_mg)