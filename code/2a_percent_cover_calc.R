#### calculating percent cover from reach surveys for each reach & site
### Jordan Zabrecky
## last edited: 01.15.2025

# This code calculates averages using % cover data for each study reach
# and additionally each river site (designated by nearby sensors & USGS gage)
# It also tallies the number and calculates the percent of transects 
# where Anabaena or Microcoleus was present

#### (1) Loading libraries and reach data ####

# loading libraries
lapply(c("tidyverse", "lubridate"), require, character.only = T)

# loading raw % cover data
percover <- read.csv("./data/EDI_data_package/benthic_surveys.csv")

# change "y" to 1's and "n" to 0's
percover[percover == "y"] <- 1
percover[percover == "n"] <- 0

# convert field_date to date type and binary columns adjusted above to numeric
percover$field_date <- ymd(percover$field_date)
percover$Micro_pres <- as.numeric(percover$Micro_pres)
percover$Ana_Cyl_pres <- as.numeric(percover$Ana_Cyl_pres)
percover$riffle_rapid <- as.numeric(percover$riffle_rapid)

#### (3) Calculating percent cover for each reach on each sampling day ####

## function to calculate % cover averages for each reach on each sampling day
average_per_reach <- function(data) {
  data %>% 
    dplyr::group_by(site, field_date, reach) %>% 
    dplyr::mutate(
      green_algae = mean(green_algae),
      microcoleus = mean(Microcoleus),
      anabaena_cylindrospermum = mean(Anabaena_Cylindrospermum),
      bare_biofilm = mean(bare_biofilm),
      other_nfixers = mean(other_N_fixers),
      micro_transects = sum(Micro_pres),
      ana_cyl_transects = sum(Ana_Cyl_pres),
      riffle_rapid_transects = sum(riffle_rapid, na.rm = TRUE),
      total_transects = length(transect),
      proportion_micro_transects = micro_transects / total_transects,
      proportion_ana_cyl_transects = ana_cyl_transects / total_transects,
      proportion_riffle_rapid_transects = riffle_rapid_transects / total_transects,
      average_depth_cm_sampled = mean(depth_cm, na.rm = TRUE),
      median_depth_cm_sampled = median(depth_cm, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    select(field_date, site_reach, site, reach, green_algae, microcoleus,
           anabaena_cylindrospermum, bare_biofilm, other_nfixers, micro_transects, ana_cyl_transects, 
           total_transects, proportion_micro_transects, proportion_ana_cyl_transects,
           proportion_riffle_rapid_transects, average_depth_cm_sampled, median_depth_cm_sampled) %>% 
    distinct()
}

# applying function to data
percover_reach <- average_per_reach(percover)

# saving new csv
write.csv(percover_reach, "data/field_and_lab/percover_byreach.csv", row.names = FALSE)

#### (4) Calculating % cover averages for each site* on each sampling day ####
## * site = sensor grouping / nearest USGS discharge station

## function to calculate % cover averages for each site on each sampling day
## applies to percent cover already averaged for each reach because
## we care about the standard deviation across reaches (not transects)
# (also includes standard deviation across reaches)
average_per_site <- function(data) {
  data %>%
    dplyr::group_by(site, field_date) %>% 
    dplyr::mutate(
      green_algae_mean = mean(green_algae),
      green_algae_sd = sd(green_algae),
      microcoleus_mean = mean(microcoleus),
      microcoleus_sd = sd(microcoleus),
      anabaena_cylindrospermum_mean = mean(anabaena_cylindrospermum),
      anabaena_cylindrospermum_sd = sd(anabaena_cylindrospermum),
      bare_biofilm_mean = mean(bare_biofilm),
      bare_biofilm_sd = sd(bare_biofilm),
      other_nfixers_mean = mean(other_nfixers),
      other_nfixers_sd = sd(other_nfixers) # removed all the other stuff for now...
    ) %>% 
    ungroup() %>% 
    select(field_date, site, green_algae_mean, green_algae_sd, microcoleus_mean, 
           microcoleus_sd, anabaena_cylindrospermum_mean, anabaena_cylindrospermum_sd, 
           bare_biofilm_mean, bare_biofilm_sd, other_nfixers_mean, other_nfixers_sd) %>% 
    distinct() 
}

# Want to manually merge 7.6.2022 RUS-4S & RUS-3UP and 7.7.2022 RUS-2UP to be on the same day 
# as they are only a day apart and were ideally done on the same day
# things do not always go according to plan!
percover_reach$field_date[14] <- mdy("07/06/2022")

# applying function to data
percover_site <- average_per_site(percover_reach)

# sampling at SFE-M in 2023 included site 2 whereas SFE-M in 2022 did not
# so want to make a disclaimer
percover_site$site[12:18] <- "SFE-M_excl_site2"
percover_site$site[33:47] <- "SFE-M_all_sites"

# to be able to compare 2022 and 2023, let's recalculate for just those original sites in 2023
SFE_M_excl_site2 <- percover_reach %>% 
  filter(site == "SFE-M",
         reach != "2") %>% 
  filter(field_date >= "2023-01-01 00:00:00")

# applying function to just this data
SFE_M_excl_site2_calc <- average_per_site(SFE_M_excl_site2)

# adding note/modified site
SFE_M_excl_site2_calc$site <- "SFE-M_excl_site2"

# binding above dataframe to full dataframe
percover_site_final <- rbind(percover_site, SFE_M_excl_site2_calc)

# saving new csv
write.csv(percover_site_final, "data/field_and_lab/percover_bysite.csv", row.names = FALSE)
