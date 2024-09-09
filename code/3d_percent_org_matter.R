#### calculating percent organic matter for TM and TAC samples
### Jordan Zabrecky
## DATE

# This code calculates percent organic matter for TM (targeted-Microcoleus) and
# TAC (targeted-Anabaena and Cylindrospermum) using tin weight, dry weight,
# and combusted weight obtained day to week after sampling at UC-B Angelo Reserve

#### (1) Loading libraries and data ####

# loading libraries
lapply(c("tidyverse", "lubridate", "plyr"), require, character.only = T)

# reading in weights measured at UC-B Angelo Reserve Lab
data <- ldply(list.files(path = "./data/field_and_lab/raw_data/", 
                         pattern = "angelo"), function(filename) {
                           d <- read.csv(paste("data/field_and_lab/raw_data/", filename, sep = ""))
                           d$field_date <- mdy(d$field_date)
                           return(d)
                         })

#### (2) Calculating percent organic matter ####

# note: had two samples (6/29/2022 SFE-M-3 and SFE-M-4 that molded before we 
# had a dessicate container while we were waiting for oven use
# will likely just use SFE-M-1S weight for these <- TALK TO JOANNA

# calculate dry weight (both organic and inorganic matter) without tin weight
data$dry_sample_g <- data$dry_sample_tin_g - data$tin_g

# calculate AFDM (ash-free dry mass or organic matter)
data$afdm_g <- data$dry_sample_tin_g - data$combusted_sample_tin_g

# calculate percent organic matter (percent of matter that is organic/combusted)
data$per_org_matter <- data$afdm_g / data$dry_sample_g

# dealing with triplicate
# filter for triplicate; summarize by date
# remove tripliate and put summary into final data-frame
# order by date; (and then site reach if needed)
# need to fix this; guy next to me is making me feel uncomfortable
triplicates <- data %>% 
  filter(triplicate == "y") %>% 
  group_by(site_reach, field_date) %>% 
  mutate(mean = mean(per_org_matter),
         sd = sd(per_org_matter)) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(field_date, site_reach, site, reach, sample_type, triplicate, mean, sd)
# # LOOK at percent cover code