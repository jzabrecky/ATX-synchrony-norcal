### looking at stuff for results section
### will write this up nicer later

library(tidyverse)
library(plyr)

cover <- read.csv("./data/field_and_lab/percover_byreach.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date))
atx <- read.csv("./data/field_and_lab/cyano_atx.csv") %>% 
  mutate(field_date = ymd(field_date),
         year = year(field_date))

atx_tm <- atx %>% 
  filter(sample_type == "TM")
atx_tac <- atx %>% 
  filter(sample_type == "TAC")

summary_cover <- cover %>% 
  group_by(site_reach, year) %>% 
  summarize(max_micro = max(microcoleus),
            max_anacyl = max(anabaena_cylindrospermum))

summary_cover <- left_join()
# maybe make a for loop
# Peak cover dates

summary_tm_atx <- atx_tm %>% 
  group_by(site_reach, year) %>% 
  summarize(max_tm_atx = max(ATX_all_ug_orgmat_g))

summary_tac_atx <- atx_tac %>% 
  group_by(site_reach, year) %>% 
  summarize(max_tac_atx = max(ATX_all_ug_orgmat_g))


#Peak anatoxin dates:
#8-10 or 8-23 for all 2022 TM samples
#8-22 or 9-04 or 9-18 for all 2023 TM samples

#7-28 or 8-10 for all 2022 TAC samples
#8-14 for all 2023 TAC samples
# can literally match up summary tables to get this better

# congeners- looking at specific sample
congeners <- atx %>% 
  mutate(proportion_atxa = ATXa_ug_g / ATX_all_ug_g,
         proportion_dhatxa = dhATXa_ug_g / ATX_all_ug_g,
         proportion_htxa = HTXa_ug_g / ATX_all_ug_g) 

congeners_sfkeel <- congeners %>% 
  filter(site_reach == "SFE-M-1S" | site_reach == "SFE-M-2" | site_reach == "SFE-M-3"
         | site_reach == "SFE-M-4" | site_reach == "SFE-SH-1S")

congeners_russian <- congeners %>% 
  filter(site_reach == "RUS-1S" | site_reach == "RUS-2" | site_reach == "RUS-3")


# looking at GPP
metab <- ldply(list.files(path = "./data/metab_model_outputs_processed/", pattern = "_metab.csv"), function(filename) {
  d <- read.csv(paste("data/metab_model_outputs_processed/", filename, sep = ""))
  d$site = d$site_year %>% str_sub(end=-6)
  d$field_date = ymd(d$date)
  d$year = year(d$field_date)
  return(d)
})

metab_summary <- metab %>% 
  dplyr::group_by(site_year) %>% 
  dplyr::summarize(max_GPP = max(GPP.mean))

# 7-29 for south fork eel miranda 2022
# 8-16 for south fork eel miranda 2023
# 7-26 for south fork eel standish hickey 2023
# 9-03 for russian 2022
# 7-03 for salmon 2023
# 9-10 for salmon 2022