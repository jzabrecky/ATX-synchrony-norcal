#### Processing 2024 data to make figure similar to that of 2023&2022 from chapter 1
### Jordan Zabrecky
## last edited: 08.03.2026

# see title

#### (1) Loading libraries & data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr", "cowplot"), 
       require, character.only = T)

## loading in data

# data
cover_24 <- read.csv("./figures/CCHABs_meeting_figs/2024_data/SFE_atx_percover.csv") %>% 
  mutate(Date = mdy(Date))
atx_24 <- read.csv("./figures/CCHABs_meeting_figs/2024_data/cyano_atx_24.csv") %>% 
  mutate(field_date = mdy(field_date)) %>% 
  filter(site == "SFE-M")

#### (2) cover data ####

# merge based on reach and date
cover_reach <- cover_24 %>% 
  dplyr::group_by(Site, Date) %>% 
  dplyr::summarize(ana_cyl = mean(A),
                   microcoleus = mean(M)) %>% 
  ungroup()

# mean based across all for the river
cover_river <- cover_reach %>% 
  pivot_longer(cols = c("ana_cyl", "microcoleus"), values_to = "cover", names_to = "taxa") %>% 
  dplyr::group_by(Date, taxa) %>% 
  dplyr::summarize(mean = mean(cover),
                   sd = sd(cover),
                   min = mean - sd,
                   max = mean + sd) %>% 
  ungroup()

# save
write.csv(cover_river, "./figures/CCHABs_meeting_figs/2024_data/cover_averaged_24.csv",
          row.names = FALSE)

#### (3) Presence/Absence ####

presence_reach <- cover_24 %>% 
  mutate(M.present = case_when(M.present == "yes" ~ 1,
                               TRUE ~ 0),
         A.present = case_when(A.present == "yes" ~ 1,
                               TRUE ~ 0)) %>% 
  dplyr::group_by(Site, Date) %>% 
  dplyr::summarize(ana_cyl = sum(M.present),
                   microcoleus = sum(A.present)) %>% 
  ungroup()

presence_river <- presence_reach %>% 
  pivot_longer(cols = c("ana_cyl", "microcoleus"), values_to = "presence", names_to = "taxa") %>% 
  dplyr::group_by(Date, taxa) %>% 
  dplyr::summarize(presence = sum(presence)) %>% 
  ungroup() %>% 
  mutate(presence = case_when(presence > 0 ~ "y",
                              TRUE ~ "n"))

quadrat_reach <- cover_24 %>% 
  dplyr::group_by(Site, Date) %>% 
  dplyr::summarize(ana_cyl = sum(M),
                   microcoleus = sum(A)) %>% 
  ungroup()

quadrat_river <- presence_reach %>% 
  pivot_longer(cols = c("ana_cyl", "microcoleus"), values_to = "quadrat", names_to = "taxa") %>% 
  dplyr::group_by(Date, taxa) %>% 
  dplyr::summarize(quadrat = sum(quadrat)) %>% 
  ungroup() %>% 
  mutate(quadrat = case_when(quadrat > 0 ~ "y",
                              TRUE ~ "n"))

# join together
final <- left_join(presence_river, quadrat_river, by = c("Date", "taxa"))

# write csv
write.csv(final, "./figures/CCHABs_meeting_figs/2024_data/presence_quadrat_24.csv",
          row.names = FALSE)

#### (4) ATX data ####


# merge based on reach and date (in case of duplicates)
atx_reach <- atx_24 %>% 
  dplyr::group_by(field_date, reach, sample_type) %>% 
  dplyr::summarize(ATX_all_ug_g = mean(ATX_all_ug_g)) %>% 
  ungroup()

# need to normalize based on AFDM
OM_raw <- read.csv("./figures/CCHABs_meeting_figs/2024_data/afdm_24.csv")
# note that weirdly three values are >100%
OM_24 <- read.csv("./figures/CCHABs_meeting_figs/2024_data/afdm_24.csv") %>% 
  mutate(reach = str_split_i(Sample.ID, "-", 2),
         field_date = mdy(Field.Date)) %>% 
  mutate(reach = case_when(reach == "4S" ~ "1S",
                           reach == "BUG" ~ "2",
                           reach == "3UP" ~ "3",
                           reach == "2UP" ~ "4")) %>% 
  mutate(sample_type = case_when(Cyano == "Microcoleus" ~ "TM",
                                 Cyano == "Anabaena" ~ "TAC")) %>% 
  select(reach, field_date, sample_type, X.OM) 

# join in with ATX data
together <- left_join(atx_reach %>% 
                      select(field_date, reach, sample_type, ATX_all_ug_g),
                    OM_24, by = c("sample_type", "field_date", "reach")) %>% 
  mutate(percent_OM = as.numeric(str_split_i(X.OM, "%", 1)) / 100) %>% 
  mutate(percent_OM = case_when(percent_OM > 1 ~ 1,
                                TRUE ~ percent_OM)) %>% 
  mutate(ATX_all_ug_OM_g = ATX_all_ug_g / percent_OM)

# merge for river
river <- together %>% 
  dplyr::group_by(field_date, sample_type) %>% 
  dplyr::summarize(ATX_all_ug_OM_g = mean(ATX_all_ug_OM_g)) %>% 
  ungroup()

# save
write.csv(river, "./figures/CCHABs_meeting_figs/2024_data/atx_averaged_24.csv",
          row.names = FALSE)
